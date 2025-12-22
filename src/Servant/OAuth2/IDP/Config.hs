{-# LANGUAGE DeriveGeneric #-}

{- |
Module      : Servant.OAuth2.IDP.Config
Description : Protocol-agnostic OAuth configuration
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC

This module provides OAuthEnv, a record type for protocol-agnostic OAuth
configuration.
-}
module Servant.OAuth2.IDP.Config (
    OAuthEnv (..),
) where

import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime)
import GHC.Generics (Generic)
import Network.URI (URI)

import Servant.OAuth2.IDP.Metadata (ProtectedResourceMetadata)
import Servant.OAuth2.IDP.Types (
    ClientAuthMethod,
    CodeChallengeMethod,
    OAuthGrantType,
    ResponseType,
    Scope,
 )

-- | Protocol-agnostic OAuth configuration
data OAuthEnv = OAuthEnv
    { oauthRequireHTTPS :: Bool
    -- ^ Security flag: require HTTPS for redirect URIs (except localhost)
    , oauthBaseUrl :: URI
    -- ^ Base URL for OAuth endpoints (e.g., "https://api.example.com")
    , oauthAuthCodeExpiry :: NominalDiffTime
    -- ^ Authorization code expiry duration
    , oauthAccessTokenExpiry :: NominalDiffTime
    -- ^ Access token expiry duration
    , oauthLoginSessionExpiry :: NominalDiffTime
    -- ^ Login session expiry duration
    , oauthAuthCodePrefix :: Text
    -- ^ Prefix for generated authorization codes
    , oauthRefreshTokenPrefix :: Text
    -- ^ Prefix for generated refresh tokens
    , oauthClientIdPrefix :: Text
    -- ^ Prefix for generated client IDs
    , oauthSupportedScopes :: [Scope]
    -- ^ Supported OAuth scopes (can be empty - no required scopes)
    , oauthSupportedResponseTypes :: NonEmpty ResponseType
    -- ^ Supported response types (RFC requires at least one)
    , oauthSupportedGrantTypes :: NonEmpty OAuthGrantType
    -- ^ Supported grant types (RFC requires at least one)
    , oauthSupportedAuthMethods :: NonEmpty ClientAuthMethod
    -- ^ Supported token endpoint authentication methods (RFC requires at least one)
    , oauthSupportedCodeChallengeMethods :: NonEmpty CodeChallengeMethod
    -- ^ Supported PKCE code challenge methods (RFC requires at least one)
    , resourceServerBaseUrl :: URI
    -- ^ Base URL for the resource server (e.g., "https://resource.example.com")
    , resourceServerMetadata :: ProtectedResourceMetadata
    -- ^ Protected resource metadata (RFC 9728) for resource server discovery
    , oauthServerName :: Text
    -- ^ Server name for branding (e.g., HTML titles like 'Sign In - {serverName}')
    , oauthScopeDescriptions :: Map Scope Text
    -- ^ Human-readable descriptions for OAuth scopes (for consent pages)
    }
    deriving (Generic)
