{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module      : Servant.OAuth2.IDP.API
Description : OAuth 2.1 API type definitions
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC

This module provides the OAuth 2.1 API type definitions for use in
Servant route composition. The API types are separated from the handler
implementations to enable modular API composition.

= API Structure

The OAuth API is composed of several sub-APIs:

* 'ProtectedResourceAPI': RFC 9728 protected resource metadata
* 'OAuthAPI': Complete OAuth 2.1 server API (metadata, registration, authorization, token)

= Content Types

This module also provides the 'HTML' content type for serving HTML pages
in OAuth flows (login pages, error pages).
-}
module Servant.OAuth2.IDP.API (
    -- * OAuth API Types
    OAuthAPI,
    ProtectedResourceAPI,
    LoginAPI,

    -- * Content Types
    HTML,

    -- * Request/Response Types
    LoginForm (..),
    ClientRegistrationRequest (..),
    ClientRegistrationResponse (..),
    TokenRequest (..),
    TokenResponse (..),
) where

import Data.Aeson qualified as Aeson
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant (
    FormUrlEncoded,
    Get,
    Header,
    Headers,
    JSON,
    NoContent,
    Post,
    QueryParam,
    QueryParam',
    ReqBody,
    Required,
    StdMethod (POST),
    Verb,
    (:<|>),
    (:>),
 )
import Servant.HTML.Lucid (HTML)
import Web.FormUrlEncoded (FromForm (..), parseUnique)

import Servant.OAuth2.IDP.Auth.Backend (PlaintextPassword, Username, mkPlaintextPassword, mkUsername)
import Servant.OAuth2.IDP.Handlers.HTML (LoginPage)
import Servant.OAuth2.IDP.Metadata (OAuthMetadata, ProtectedResourceMetadata)
import Servant.OAuth2.IDP.Types (
    AccessToken,
    AuthCodeId,
    ClientAuthMethod,
    ClientId,
    ClientName,
    ClientSecret,
    CodeChallenge,
    CodeChallengeMethod,
    CodeVerifier,
    GrantType (..),
    LoginAction (..),
    OAuthState,
    RedirectTarget,
    RedirectUri,
    RefreshToken,
    RefreshTokenId,
    ResourceIndicator,
    ResponseType,
    Scopes,
    SessionCookie,
    SessionId,
    TokenType,
    TokenValidity,
    mkSessionId,
 )
import Web.HttpApiData (parseUrlPiece)

-- -----------------------------------------------------------------------------
-- API Types
-- -----------------------------------------------------------------------------

{- | Protected Resource Metadata API (RFC 9728).

Provides metadata about OAuth-protected resources including:

* Resource identifier
* List of authorization servers
* Supported scopes
* Supported bearer token methods

This endpoint is served at @/.well-known/oauth-protected-resource@
per RFC 9728 specification.

= Response Example

@
{
  "resource": "https://api.example.com",
  "authorization_servers": ["https://api.example.com"],
  "scopes_supported": ["mcp:read", "mcp:write"],
  "bearer_methods_supported": ["header"]
}
@
-}
type ProtectedResourceAPI =
    ".well-known" :> "oauth-protected-resource" :> Get '[JSON] ProtectedResourceMetadata

{- | Login API endpoint.

Handles the interactive login flow for OAuth authorization:

1. User submits credentials via form
2. Server validates credentials
3. On success: redirects with authorization code
4. On failure: returns error page

The endpoint returns an HTTP 302 redirect with:

* @Location@ header: redirect URI with code or error
* @Set-Cookie@ header: session cookie (cleared after use)

This endpoint is typically reached after the user is shown a login page
by the authorization endpoint.
-}
type LoginAPI =
    "login"
        :> Header "Cookie" Text
        :> ReqBody '[FormUrlEncoded] LoginForm
        :> Verb 'POST 302 '[HTML] (Headers '[Header "Location" RedirectTarget, Header "Set-Cookie" SessionCookie] NoContent)

{- | Complete OAuth 2.1 API.

Provides all OAuth endpoints required by the OAuth specification:

* @/.well-known/oauth-protected-resource@: Protected resource metadata (RFC 9728)
* @/.well-known/oauth-authorization-server@: Authorization server metadata (RFC 8414)
* @/register@: Dynamic client registration (RFC 7591)
* @/authorize@: Authorization endpoint (RFC 6749)
* @/login@: Login form submission endpoint (custom)
* @/token@: Token exchange endpoint (RFC 6749)

All endpoints follow their respective RFCs.

= API Composition

The API can be composed with other Servant APIs:

@
type FullAPI = OAuthAPI :<|> McpAPI

server :: ServerT FullAPI m
server = oauthServer :<|> mcpServer
@

= Metadata Discovery

Clients should start by querying the metadata endpoints:

1. @GET /.well-known/oauth-authorization-server@ - Server capabilities
2. @GET /.well-known/oauth-protected-resource@ - Resource metadata

This provides automatic configuration of OAuth clients per RFC 8414.
-}
type OAuthAPI =
    ProtectedResourceAPI
        :<|> ".well-known" :> "oauth-authorization-server" :> Get '[JSON] OAuthMetadata
        :<|> "register"
            :> ReqBody '[JSON] ClientRegistrationRequest
            :> Verb 'POST 201 '[JSON] ClientRegistrationResponse
        :<|> "authorize"
            :> QueryParam' '[Required] "response_type" ResponseType
            :> QueryParam' '[Required] "client_id" ClientId
            :> QueryParam' '[Required] "redirect_uri" RedirectUri
            :> QueryParam' '[Required] "code_challenge" CodeChallenge
            :> QueryParam' '[Required] "code_challenge_method" CodeChallengeMethod
            :> QueryParam "scope" Scopes
            :> QueryParam "state" OAuthState
            :> QueryParam "resource" ResourceIndicator
            :> Get '[HTML] (Headers '[Header "Set-Cookie" SessionCookie] LoginPage)
        :<|> LoginAPI
        :<|> "token"
            :> ReqBody '[FormUrlEncoded] TokenRequest
            :> Post '[JSON] TokenResponse

-- -----------------------------------------------------------------------------
-- Request/Response Types
-- -----------------------------------------------------------------------------

{- | Login form data.

Contains user credentials submitted via the login page, plus the session ID
to correlate with the pending authorization request.
-}
data LoginForm = LoginForm
    { formUsername :: Username
    , formPassword :: PlaintextPassword
    , formSessionId :: SessionId
    , formAction :: LoginAction
    }
    deriving (Generic, Show)

instance FromForm LoginForm where
    fromForm form = do
        userText <- parseUnique "username" form
        passText <- parseUnique "password" form
        sessText <- parseUnique "session_id" form
        action <- parseUnique "action" form

        username <- case mkUsername userText of
            Just u -> Right u
            Nothing -> Left "Invalid username"
        let password = mkPlaintextPassword passText
        sessionId <- case mkSessionId sessText of
            Just s -> Right s
            Nothing -> Left "Invalid session_id (must be UUID)"

        pure $ LoginForm username password sessionId action

{- | Client registration request.

Submitted by OAuth clients to register dynamically per RFC 7591.
-}
data ClientRegistrationRequest = ClientRegistrationRequest
    { client_name :: ClientName
    , redirect_uris :: NonEmpty RedirectUri
    , grant_types :: NonEmpty GrantType
    , response_types :: NonEmpty ResponseType
    , token_endpoint_auth_method :: ClientAuthMethod
    }
    deriving (Show, Generic, Aeson.FromJSON)

{- | Client registration response.

Returned after successful client registration. Contains client credentials
and registered metadata.
-}
data ClientRegistrationResponse = ClientRegistrationResponse
    { client_id :: ClientId
    , client_secret :: ClientSecret -- Empty string for public clients
    , client_name :: ClientName
    , redirect_uris :: NonEmpty RedirectUri
    , grant_types :: NonEmpty GrantType
    , response_types :: NonEmpty ResponseType
    , token_endpoint_auth_method :: ClientAuthMethod
    }
    deriving (Show, Generic, Aeson.ToJSON)

{- | Token endpoint response.

Returned from the token endpoint after successful token exchange.
Contains access token, optional refresh token, and metadata.
-}
data TokenResponse = TokenResponse
    { access_token :: AccessToken
    , token_type :: TokenType
    , expires_in :: Maybe TokenValidity
    , refresh_token :: Maybe RefreshToken
    , scope :: Maybe Scopes
    }
    deriving (Show, Generic)

instance Aeson.ToJSON TokenResponse where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions{Aeson.omitNothingFields = True}

{- | Token endpoint request.

Sum type capturing the two supported grant types with their specific parameters.

= Grant Types

* 'AuthorizationCodeGrant': Exchange authorization code for tokens (RFC 6749 Section 4.1.3)
* 'RefreshTokenGrant': Exchange refresh token for new access token (RFC 6749 Section 6)

= Usage

@
-- In handlers:
case tokenRequest of
    AuthorizationCodeGrant code verifier mResource -> ...
    RefreshTokenGrant refreshToken mResource -> ...
@
-}
data TokenRequest
    = -- | Authorization code grant with PKCE verification
      AuthorizationCodeGrant
        { reqAuthCode :: AuthCodeId
        , reqCodeVerifier :: CodeVerifier
        , reqResource :: Maybe ResourceIndicator
        }
    | -- | Refresh token grant
      RefreshTokenGrant
        { reqRefreshToken :: RefreshTokenId
        , reqResource :: Maybe ResourceIndicator
        }
    deriving (Show, Generic)

{- | Parse token request from form-encoded data.

Parses the @grant_type@ field first, then dispatches to appropriate parser:

* @authorization_code@: Requires @code@ and @code_verifier@
* @refresh_token@: Requires @refresh_token@
* Other grant types: Returns error

The @resource@ parameter (RFC 8707) is optional for both grant types.
-}
instance FromForm TokenRequest where
    fromForm form = do
        -- Parse grant_type first to determine which variant to construct
        grantTypeText <- parseUnique "grant_type" form
        case parseUrlPiece grantTypeText of
            Left err -> Left $ "Invalid grant_type: " <> err
            Right grantType -> case grantType of
                GrantAuthorizationCode -> do
                    -- Parse authorization code grant parameters
                    codeText <- parseUnique "code" form
                    code <- case parseUrlPiece codeText of
                        Left err -> Left $ "Invalid code: " <> err
                        Right c -> Right c

                    verifierText <- parseUnique "code_verifier" form
                    verifier <- case parseUrlPiece verifierText of
                        Left err -> Left $ "Invalid code_verifier: " <> err
                        Right v -> Right v

                    -- Optional resource parameter
                    let mResource = case parseUnique "resource" form of
                            Left _ -> Nothing
                            Right r -> case parseUrlPiece r of
                                Left _ -> Nothing
                                Right ri -> Just ri

                    pure $ AuthorizationCodeGrant code verifier mResource
                GrantRefreshToken -> do
                    -- Parse refresh token grant parameters
                    tokenText <- parseUnique "refresh_token" form
                    refreshToken <- case parseUrlPiece tokenText of
                        Left err -> Left $ "Invalid refresh_token: " <> err
                        Right t -> Right t

                    -- Optional resource parameter
                    let mResource = case parseUnique "resource" form of
                            Left _ -> Nothing
                            Right r -> case parseUrlPiece r of
                                Left _ -> Nothing
                                Right ri -> Just ri

                    pure $ RefreshTokenGrant refreshToken mResource
                GrantClientCredentials ->
                    Left "Unsupported grant_type: client_credentials"
