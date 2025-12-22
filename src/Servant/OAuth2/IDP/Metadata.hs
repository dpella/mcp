{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Servant.OAuth2.IDP.Metadata
Description : OAuth metadata types per RFC 8414 and RFC 9728
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC

This module provides OAuth metadata types for discovery endpoints:
- RFC 8414: OAuth Authorization Server Metadata
- RFC 9728: OAuth Protected Resource Metadata

Both types use custom JSON instances with snake_case field names per the RFCs.
-}
module Servant.OAuth2.IDP.Metadata (
    -- * OAuth Authorization Server Metadata (RFC 8414)
    OAuthMetadata (..),
    mkOAuthMetadata,

    -- ** Field Accessors
    oauthIssuer,
    oauthAuthorizationEndpoint,
    oauthTokenEndpoint,
    oauthRegistrationEndpoint,
    oauthUserInfoEndpoint,
    oauthJwksUri,
    oauthScopesSupported,
    oauthResponseTypesSupported,
    oauthGrantTypesSupported,
    oauthTokenEndpointAuthMethodsSupported,
    oauthCodeChallengeMethodsSupported,

    -- * OAuth Protected Resource Metadata (RFC 9728)
    ProtectedResourceMetadata,
    mkProtectedResourceMetadata,
    mkProtectedResourceMetadataForDemo,

    -- ** Field Accessors
    prResource,
    prAuthorizationServers,
    prScopesSupported,
    prBearerMethodsSupported,
    prResourceName,
    prResourceDocumentation,

    -- * Bearer Token Methods (RFC 6750)
    BearerMethod (..),
) where

import Control.Monad (guard)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, withText, (.:), (.:?), (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.Types (Pair, Parser)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Network.URI (URI (..), isAbsoluteURI, parseURI)
import Servant.OAuth2.IDP.Types (
    ClientAuthMethod,
    CodeChallengeMethod,
    GrantType,
    ResponseType,
    Scope,
 )

-- -----------------------------------------------------------------------------
-- Bearer Token Methods (RFC 6750)
-- -----------------------------------------------------------------------------

{- | Bearer token presentation methods per RFC 6750 Section 2.

OAuth 2.0 Bearer tokens can be presented to resource servers using
different HTTP methods. This type represents the supported methods.
-}
data BearerMethod
    = -- | Authorization header (RFC 6750 Section 2.1)
      BearerHeader
    | -- | Form-encoded body parameter (RFC 6750 Section 2.2)
      BearerBody
    | -- | URI query parameter (RFC 6750 Section 2.3, NOT RECOMMENDED)
      BearerUri
    deriving (Eq, Show, Generic)

-- | ToJSON instance mapping to RFC 6750 string values
instance ToJSON BearerMethod where
    toJSON BearerHeader = toJSON ("header" :: Text)
    toJSON BearerBody = toJSON ("body" :: Text)
    toJSON BearerUri = toJSON ("query" :: Text)

-- | FromJSON instance parsing RFC 6750 string values
instance FromJSON BearerMethod where
    parseJSON = withText "BearerMethod" $ \case
        "header" -> pure BearerHeader
        "body" -> pure BearerBody
        "query" -> pure BearerUri
        other -> fail $ "Invalid bearer method: " <> T.unpack other

-- -----------------------------------------------------------------------------
-- JSON Helper Functions
-- -----------------------------------------------------------------------------

-- | Convert URI to Text for JSON serialization
uriToText :: URI -> Text
uriToText = T.pack . show

-- | Parse Text as URI for JSON deserialization
textToURI :: Text -> Parser URI
textToURI t = case parseURI (T.unpack t) of
    Just uri -> pure uri
    Nothing -> fail $ "Invalid URI: " <> T.unpack t

-- | Helper to conditionally include optional JSON fields
optional :: (ToJSON a) => Text -> Maybe a -> [Pair]
optional _ Nothing = []
optional key (Just val) = [Key.fromText key .= val]

-- | Parse a list into NonEmpty, failing if empty
parseNonEmptyList :: [a] -> Parser (NonEmpty a)
parseNonEmptyList [] = fail "Expected non-empty list"
parseNonEmptyList (x : xs) = pure (x :| xs)

-- -----------------------------------------------------------------------------
-- URI Validation Helper
-- -----------------------------------------------------------------------------

-- | Check if a Text value is an absolute HTTPS URI
isAbsoluteHttpsUri :: Text -> Bool
isAbsoluteHttpsUri uri =
    case parseURI (T.unpack uri) of
        Just u -> uriScheme u == "https:" && isAbsoluteURI (T.unpack uri)
        Nothing -> False

-- -----------------------------------------------------------------------------
-- OAuth Authorization Server Metadata (RFC 8414)
-- -----------------------------------------------------------------------------

{- | OAuth Authorization Server Metadata per RFC 8414.

This type represents the discovery metadata returned by the
@.well-known/oauth-authorization-server@ endpoint.

Required fields:
- issuer: Authorization server issuer identifier (MUST be absolute URI with https)
- authorization_endpoint: URL of the authorization endpoint
- token_endpoint: URL of the token endpoint
- response_types_supported: List of supported OAuth 2.0 response_type values

Optional fields provide additional server capabilities and endpoints.
-}
data OAuthMetadata = OAuthMetadata
    { issuer :: Text
    -- ^ Authorization server issuer identifier (MUST be absolute URI with https)
    , authorizationEndpoint :: Text
    -- ^ URL of the authorization endpoint
    , tokenEndpoint :: Text
    -- ^ URL of the token endpoint
    , registrationEndpoint :: Maybe Text
    -- ^ URL of the client registration endpoint (RFC 7591)
    , userInfoEndpoint :: Maybe Text
    -- ^ URL of the UserInfo endpoint (OpenID Connect)
    , jwksUri :: Maybe Text
    -- ^ URL of the JSON Web Key Set document
    , scopesSupported :: Maybe [Scope]
    -- ^ List of OAuth 2.0 scope values supported
    , responseTypesSupported :: [ResponseType]
    -- ^ List of OAuth 2.0 response_type values supported
    , grantTypesSupported :: Maybe [GrantType]
    -- ^ List of OAuth 2.0 grant_type values supported
    , tokenEndpointAuthMethodsSupported :: Maybe [ClientAuthMethod]
    -- ^ List of client authentication methods supported at token endpoint
    , codeChallengeMethodsSupported :: Maybe [CodeChallengeMethod]
    -- ^ List of PKCE code challenge methods supported
    }
    deriving (Eq, Show, Generic)

-- | Field accessors for OAuthMetadata
oauthIssuer :: OAuthMetadata -> Text
oauthIssuer = issuer

oauthAuthorizationEndpoint :: OAuthMetadata -> Text
oauthAuthorizationEndpoint = authorizationEndpoint

oauthTokenEndpoint :: OAuthMetadata -> Text
oauthTokenEndpoint = tokenEndpoint

oauthRegistrationEndpoint :: OAuthMetadata -> Maybe Text
oauthRegistrationEndpoint = registrationEndpoint

oauthUserInfoEndpoint :: OAuthMetadata -> Maybe Text
oauthUserInfoEndpoint = userInfoEndpoint

oauthJwksUri :: OAuthMetadata -> Maybe Text
oauthJwksUri = jwksUri

oauthScopesSupported :: OAuthMetadata -> Maybe [Scope]
oauthScopesSupported = scopesSupported

oauthResponseTypesSupported :: OAuthMetadata -> [ResponseType]
oauthResponseTypesSupported = responseTypesSupported

oauthGrantTypesSupported :: OAuthMetadata -> Maybe [GrantType]
oauthGrantTypesSupported = grantTypesSupported

oauthTokenEndpointAuthMethodsSupported :: OAuthMetadata -> Maybe [ClientAuthMethod]
oauthTokenEndpointAuthMethodsSupported = tokenEndpointAuthMethodsSupported

oauthCodeChallengeMethodsSupported :: OAuthMetadata -> Maybe [CodeChallengeMethod]
oauthCodeChallengeMethodsSupported = codeChallengeMethodsSupported

-- | Custom ToJSON instance with snake_case field names per RFC 8414
instance ToJSON OAuthMetadata where
    toJSON OAuthMetadata{..} =
        object $
            [ "issuer" .= issuer
            , "authorization_endpoint" .= authorizationEndpoint
            , "token_endpoint" .= tokenEndpoint
            , "response_types_supported" .= responseTypesSupported
            ]
                ++ optionalFields
      where
        optionalFields =
            concat
                [ maybe [] (\v -> ["registration_endpoint" .= v]) registrationEndpoint
                , maybe [] (\v -> ["userinfo_endpoint" .= v]) userInfoEndpoint
                , maybe [] (\v -> ["jwks_uri" .= v]) jwksUri
                , maybe [] (\v -> ["scopes_supported" .= v]) scopesSupported
                , maybe [] (\v -> ["grant_types_supported" .= v]) grantTypesSupported
                , maybe [] (\v -> ["token_endpoint_auth_methods_supported" .= v]) tokenEndpointAuthMethodsSupported
                , maybe [] (\v -> ["code_challenge_methods_supported" .= v]) codeChallengeMethodsSupported
                ]

-- | Custom FromJSON instance with snake_case field names per RFC 8414
instance FromJSON OAuthMetadata where
    parseJSON = withObject "OAuthMetadata" $ \v ->
        OAuthMetadata
            <$> v .: "issuer"
            <*> v .: "authorization_endpoint"
            <*> v .: "token_endpoint"
            <*> v .:? "registration_endpoint"
            <*> v .:? "userinfo_endpoint"
            <*> v .:? "jwks_uri"
            <*> v .:? "scopes_supported"
            <*> v .: "response_types_supported"
            <*> v .:? "grant_types_supported"
            <*> v .:? "token_endpoint_auth_methods_supported"
            <*> v .:? "code_challenge_methods_supported"

{- | Smart constructor for OAuthMetadata.

Validates that all URI fields are absolute HTTPS URIs per RFC 8414.
Returns Nothing if any URI validation fails.
-}
mkOAuthMetadata ::
    Text ->
    Text ->
    Text ->
    Maybe Text ->
    Maybe Text ->
    Maybe Text ->
    Maybe [Scope] ->
    [ResponseType] ->
    Maybe [GrantType] ->
    Maybe [ClientAuthMethod] ->
    Maybe [CodeChallengeMethod] ->
    Maybe OAuthMetadata
mkOAuthMetadata
    iss
    authzEndpoint
    tokEndpoint
    regEndpoint
    userInfoEp
    jwksU
    scopesSupp
    responseTypesSupp
    grantTypesSupp
    tokenAuthMethodsSupp
    challengeMethodsSupp = do
        -- Validate required URI fields
        guard (isAbsoluteHttpsUri iss)
        guard (isAbsoluteHttpsUri authzEndpoint)
        guard (isAbsoluteHttpsUri tokEndpoint)
        -- Validate optional URI fields (Nothing is valid, Just uri must be HTTPS)
        case regEndpoint of
            Nothing -> pure ()
            Just uri -> guard (isAbsoluteHttpsUri uri)
        case userInfoEp of
            Nothing -> pure ()
            Just uri -> guard (isAbsoluteHttpsUri uri)
        case jwksU of
            Nothing -> pure ()
            Just uri -> guard (isAbsoluteHttpsUri uri)
        -- All validations passed, construct the value
        pure $
            OAuthMetadata
                { issuer = iss
                , authorizationEndpoint = authzEndpoint
                , tokenEndpoint = tokEndpoint
                , registrationEndpoint = regEndpoint
                , userInfoEndpoint = userInfoEp
                , jwksUri = jwksU
                , scopesSupported = scopesSupp
                , responseTypesSupported = responseTypesSupp
                , grantTypesSupported = grantTypesSupp
                , tokenEndpointAuthMethodsSupported = tokenAuthMethodsSupp
                , codeChallengeMethodsSupported = challengeMethodsSupp
                }

-- -----------------------------------------------------------------------------
-- OAuth Protected Resource Metadata (RFC 9728)
-- -----------------------------------------------------------------------------

{- | OAuth Protected Resource Metadata per RFC 9728.

This type represents the discovery metadata returned by the
@.well-known/oauth-protected-resource@ endpoint.

Required fields:
- resource: Protected resource identifier (MUST be absolute URI with https)
- authorization_servers: List of authorization server issuer identifiers

Optional fields provide additional resource server information.
-}
data ProtectedResourceMetadata = ProtectedResourceMetadata
    { prResource :: URI
    -- ^ Protected resource identifier (MUST be absolute URI with https)
    , prAuthorizationServers :: NonEmpty URI
    -- ^ List of authorization server issuer identifiers (≥1 required per RFC)
    , prScopesSupported :: Maybe [Scope]
    -- ^ Scope values the resource server understands
    , prBearerMethodsSupported :: Maybe (NonEmpty BearerMethod)
    -- ^ Token presentation methods (default: ["header"])
    , prResourceName :: Maybe Text
    -- ^ Human-readable name for display
    , prResourceDocumentation :: Maybe Text
    -- ^ URL of developer documentation
    }
    deriving (Eq, Show, Generic)

-- | Custom ToJSON instance with snake_case field names per RFC 9728
instance ToJSON ProtectedResourceMetadata where
    toJSON prm =
        object $
            [ "resource" .= uriToText (prResource prm)
            , "authorization_servers" .= fmap uriToText (toList $ prAuthorizationServers prm)
            ]
                ++ optional "scopes_supported" (prScopesSupported prm)
                ++ optional "bearer_methods_supported" (toList <$> prBearerMethodsSupported prm)
                ++ optional "resource_name" (prResourceName prm)
                ++ optional "resource_documentation" (prResourceDocumentation prm)

-- | Custom FromJSON instance with snake_case field names per RFC 9728
instance FromJSON ProtectedResourceMetadata where
    parseJSON = withObject "ProtectedResourceMetadata" $ \o -> do
        resource <- o .: "resource" >>= textToURI
        authServers <- o .: "authorization_servers" >>= parseNonEmptyURIs
        scopesSupp <- o .:? "scopes_supported"
        bearerMethodsSupp <- o .:? "bearer_methods_supported" >>= traverse parseNonEmptyList
        resourceName <- o .:? "resource_name"
        resourceDoc <- o .:? "resource_documentation"
        pure $
            ProtectedResourceMetadata
                { prResource = resource
                , prAuthorizationServers = authServers
                , prScopesSupported = scopesSupp
                , prBearerMethodsSupported = bearerMethodsSupp
                , prResourceName = resourceName
                , prResourceDocumentation = resourceDoc
                }
      where
        parseNonEmptyURIs :: [Text] -> Parser (NonEmpty URI)
        parseNonEmptyURIs [] = fail "authorization_servers must contain at least one URI"
        parseNonEmptyURIs (x : xs) = do
            first <- textToURI x
            rest <- mapM textToURI xs
            pure (first :| rest)

{- | Smart constructor for ProtectedResourceMetadata.

Validates that resource URI and optional documentation URI are absolute HTTPS URIs per RFC 9728.
Returns Nothing if any URI validation fails.
-}
mkProtectedResourceMetadata ::
    URI ->
    NonEmpty URI ->
    Maybe [Scope] ->
    Maybe (NonEmpty BearerMethod) ->
    Maybe Text ->
    Maybe Text ->
    Maybe ProtectedResourceMetadata
mkProtectedResourceMetadata resource authServers scopesSupp bearerMethodsSupp resourceName resourceDoc = do
    -- Validate resource URI is HTTPS
    guard $ uriScheme resource == "https:"
    guard $ isAbsoluteURI $ show resource

    -- Validate documentation URI is HTTPS if provided
    case resourceDoc of
        Just doc -> guard $ isAbsoluteHttpsUri doc
        Nothing -> pure ()

    -- All validations passed, construct the metadata
    pure $
        ProtectedResourceMetadata
            { prResource = resource
            , prAuthorizationServers = authServers
            , prScopesSupported = scopesSupp
            , prBearerMethodsSupported = bearerMethodsSupp
            , prResourceName = resourceName
            , prResourceDocumentation = resourceDoc
            }

{- | Smart constructor for ProtectedResourceMetadata for demo/development use.

WARNING: This function is for DEMO and DEVELOPMENT use only. It allows HTTP URLs
for local testing. Production deployments MUST use 'mkProtectedResourceMetadata'
which enforces HTTPS per RFC 9728.

Unlike 'mkProtectedResourceMetadata', this function:
- Accepts both HTTP and HTTPS URIs (for localhost testing)
- Still validates that URIs are well-formed absolute URIs
- Should NOT be used in production environments

Returns Nothing if URI format validation fails.
-}
mkProtectedResourceMetadataForDemo ::
    URI ->
    NonEmpty URI ->
    Maybe [Scope] ->
    Maybe (NonEmpty BearerMethod) ->
    Maybe Text ->
    Maybe Text ->
    Maybe ProtectedResourceMetadata
mkProtectedResourceMetadataForDemo resource authServers scopesSupp bearerMethodsSupp resourceName resourceDoc = do
    -- Validate resource URI is absolute (HTTP or HTTPS allowed for demo)
    guard $ isAbsoluteURI $ show resource

    -- Validate documentation URI is absolute if provided (HTTP or HTTPS allowed for demo)
    case resourceDoc of
        Just doc -> guard $ isAbsoluteURI $ T.unpack doc
        Nothing -> pure ()

    -- All validations passed, construct the metadata
    pure $
        ProtectedResourceMetadata
            { prResource = resource
            , prAuthorizationServers = authServers
            , prScopesSupported = scopesSupp
            , prBearerMethodsSupported = bearerMethodsSupp
            , prResourceName = resourceName
            , prResourceDocumentation = resourceDoc
            }
