{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : MCP.Server.Auth
Description : MCP-compliant OAuth 2.1 authentication with pluggable credential backends
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC

This module provides MCP-compliant OAuth 2.1 authentication with PKCE support,
and re-exports the credential authentication backend types.

== Re-exports

* "Servant.OAuth2.IDP.Auth.Backend" - AuthBackend typeclass for pluggable credential validation
* "Servant.OAuth2.IDP.Auth.Demo" - Demo implementation with hardcoded credentials
-}
module MCP.Server.Auth (
    -- * AuthBackend typeclass (re-exported from Servant.OAuth2.IDP.Auth.Backend)
    module Servant.OAuth2.IDP.Auth.Backend,

    -- * OAuth Configuration
    OAuthProvider (..),
    OAuthGrantType (..),
    MCPOAuthConfig (..),
    defaultDemoMCPOAuthConfig,

    -- * Token Validation
    TokenInfo (..),
    extractBearerToken,

    -- * PKCE Support
    PKCEChallenge (..),
    generateCodeVerifier,
    generateCodeChallenge,
    validateCodeVerifier,

    -- * Metadata Discovery (re-exported from Servant.OAuth2.IDP.Metadata)
    module Servant.OAuth2.IDP.Metadata,

    -- * Protected Resource Metadata (RFC 9728) (re-exported)
    ProtectedResourceAuth,
    ProtectedResourceAuthConfig (..),
) where

-- Re-exports
import Servant.OAuth2.IDP.Auth.Backend
import Servant.OAuth2.IDP.Metadata

import Crypto.Hash (hashWith)
import Crypto.Hash.Algorithms (SHA256 (..))
import Crypto.Random (getRandomBytes)
import Data.Aeson (FromJSON)
import Data.Aeson qualified as Aeson
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.ByteString.Base64.URL qualified as B64URL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.Generics (Generic)

import Servant.OAuth2.IDP.Types (
    CodeChallenge,
    CodeVerifier,
    OAuthGrantType (..),
    unCodeChallenge,
    unCodeVerifier,
 )

-- | OAuth provider configuration (MCP-compliant)
data OAuthProvider = OAuthProvider
    { providerName :: Text
    , clientId :: Text
    , clientSecret :: Maybe Text -- Optional for public clients
    , authorizationEndpoint :: Text
    , tokenEndpoint :: Text
    , userInfoEndpoint :: Maybe Text
    , scopes :: [Text]
    , grantTypes :: [OAuthGrantType]
    , requiresPKCE :: Bool -- MCP requires PKCE for all clients
    , metadataEndpoint :: Maybe Text -- For OAuth metadata discovery
    }
    deriving (Show, Eq, Generic)

-- | MCP-specific OAuth configuration (demo-specific fields)
data MCPOAuthConfig = MCPOAuthConfig
    { autoApproveAuth :: Bool
    -- ^ Demo mode auto-approval flag (bypasses interactive login)
    , oauthProviders :: [OAuthProvider]
    -- ^ External OAuth identity providers for federated login
    , demoUserIdTemplate :: Maybe Text
    -- ^ Template for generating demo user IDs (e.g., "user-{id}"). Nothing means no demo mode.
    , demoEmailDomain :: Text
    -- ^ Domain suffix for demo user emails (e.g., "example.com")
    , demoUserName :: Text
    -- ^ Display name for demo users
    , publicClientSecret :: Maybe Text
    -- ^ Secret returned for public clients (typically empty or Nothing)
    , authorizationSuccessTemplate :: Text
    -- ^ HTML template for authorization success page
    }
    deriving (Show, Eq, Generic)

-- | Default demo configuration for MCPOAuthConfig
defaultDemoMCPOAuthConfig :: MCPOAuthConfig
defaultDemoMCPOAuthConfig =
    MCPOAuthConfig
        { autoApproveAuth = False -- Interactive login by default
        , oauthProviders = []
        , demoUserIdTemplate = Just "user-{id}"
        , demoEmailDomain = "example.com"
        , demoUserName = "Demo User"
        , publicClientSecret = Nothing
        , authorizationSuccessTemplate = "<html><body><h1>Authorization Successful</h1></body></html>"
        }

-- | PKCE challenge data
data PKCEChallenge = PKCEChallenge
    { codeVerifier :: Text
    , codeChallenge :: Text
    , challengeMethod :: Text -- Always "S256" for MCP
    }
    deriving (Show, Generic)

-- | Token introspection response
data TokenInfo = TokenInfo
    { active :: Bool
    , scope :: Maybe Text
    , clientId :: Maybe Text
    , username :: Maybe Text
    , tokenType :: Maybe Text
    , exp :: Maybe Integer -- Expiration time (Unix timestamp)
    , iat :: Maybe Integer -- Issued at time (Unix timestamp)
    , nbf :: Maybe Integer -- Not before time (Unix timestamp)
    , sub :: Maybe Text -- Subject
    , aud :: Maybe [Text] -- Audience
    , iss :: Maybe Text -- Issuer
    }
    deriving (Show, Generic)

instance FromJSON TokenInfo where
    parseJSON = Aeson.withObject "TokenInfo" $ \v ->
        TokenInfo
            <$> v Aeson..: "active"
            <*> v Aeson..:? "scope"
            <*> v Aeson..:? "client_id"
            <*> v Aeson..:? "username"
            <*> v Aeson..:? "token_type"
            <*> v Aeson..:? "exp"
            <*> v Aeson..:? "iat"
            <*> v Aeson..:? "nbf"
            <*> v Aeson..:? "sub"
            <*> v Aeson..:? "aud"
            <*> v Aeson..:? "iss"

-- | Extract Bearer token from Authorization header
extractBearerToken :: Text -> Maybe Text
extractBearerToken authHeader =
    case T.words authHeader of
        ["Bearer", token] -> Just token
        _ -> Nothing

-- | Generate a cryptographically secure code verifier for PKCE
generateCodeVerifier :: IO Text
generateCodeVerifier = do
    bytes <- getRandomBytes 32 -- 32 bytes = 256 bits entropy
    pure $ TE.decodeUtf8 $ B64URL.encodeUnpadded bytes -- 43 chars

-- | Generate code challenge from verifier using SHA256 (S256 method)
generateCodeChallenge :: Text -> Text
generateCodeChallenge verifier =
    let verifierBytes = TE.encodeUtf8 verifier
        challengeHash = hashWith SHA256 verifierBytes
        challengeBytes = convert challengeHash :: ByteString
     in TE.decodeUtf8 $ B64URL.encodeUnpadded challengeBytes

-- | Validate PKCE code verifier against challenge
validateCodeVerifier :: CodeVerifier -> CodeChallenge -> Bool
validateCodeVerifier verifier challenge = generateCodeChallenge (unCodeVerifier verifier) == unCodeChallenge challenge

-- | Type-level tag for MCP protected resource authentication
data ProtectedResourceAuth

-- | Configuration for ProtectedResourceAuth
newtype ProtectedResourceAuthConfig = ProtectedResourceAuthConfig
    { resourceMetadataUrl :: Text
    {- ^ URL to the protected resource metadata endpoint
    e.g., "https://mcp.example.com/.well-known/oauth-protected-resource"
    -}
    }
    deriving (Show, Generic)
