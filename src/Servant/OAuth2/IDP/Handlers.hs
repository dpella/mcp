{- |
Module      : Servant.OAuth2.IDP.Handlers
Description : Polymorphic OAuth 2.1 handler implementations
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC

This module re-exports polymorphic OAuth 2.1 handler implementations using
the OAuthStateStore and AuthBackend typeclasses for pluggable backends.

All handlers are polymorphic over the monad @m@ with typeclass constraints,
allowing them to work with different storage and auth backends:

* In-memory (demo/testing)
* PostgreSQL (production)
* Redis (caching)
* LDAP/Active Directory (authentication)

= Usage

@
-- Use handlers directly in your server implementation
oauthServer :: ServerT OAuthAPI m
oauthServer =
    handleProtectedResourceMetadata
        :<|> handleMetadata
        :<|> handleRegister
        :<|> handleAuthorize
        :<|> handleLogin
        :<|> handleToken
@

= Module Organization

This module has been refactored into focused submodules:

* "Servant.OAuth2.IDP.Handlers.HTML" - HTML rendering for login and error pages
* "Servant.OAuth2.IDP.Handlers.Helpers" - Helper functions (session extraction, token generation)
* "Servant.OAuth2.IDP.Handlers.Metadata" - OAuth metadata discovery endpoints
* "Servant.OAuth2.IDP.Handlers.Registration" - Dynamic client registration
* "Servant.OAuth2.IDP.Handlers.Authorization" - Authorization endpoint (login page display)
* "Servant.OAuth2.IDP.Handlers.Login" - Login form submission (credential validation)
* "Servant.OAuth2.IDP.Handlers.Token" - Token endpoint (authorization_code and refresh_token grants)
-}
module Servant.OAuth2.IDP.Handlers (
    -- * Metadata Handlers
    handleMetadata,
    handleProtectedResourceMetadata,
    defaultProtectedResourceMetadata,

    -- * Registration Handler
    handleRegister,

    -- * Authorization Handler
    handleAuthorize,

    -- * Login Handler
    handleLogin,

    -- * Token Handlers
    handleToken,
    handleAuthCodeGrant,
    handleRefreshTokenGrant,

    -- * HTML Rendering Functions
    scopeToDescription,
    formatScopeDescriptions,

    -- * Helper Functions
    extractSessionFromCookie,
    generateAuthCode,
    generateJWTAccessToken,
    generateRefreshTokenWithConfig,
) where

import Servant.OAuth2.IDP.Handlers.Authorization (handleAuthorize)
import Servant.OAuth2.IDP.Handlers.HTML (
    formatScopeDescriptions,
    scopeToDescription,
 )
import Servant.OAuth2.IDP.Handlers.Helpers (
    extractSessionFromCookie,
    generateAuthCode,
    generateJWTAccessToken,
    generateRefreshTokenWithConfig,
 )
import Servant.OAuth2.IDP.Handlers.Login (handleLogin)
import Servant.OAuth2.IDP.Handlers.Metadata (
    defaultProtectedResourceMetadata,
    handleMetadata,
    handleProtectedResourceMetadata,
 )
import Servant.OAuth2.IDP.Handlers.Registration (handleRegister)
import Servant.OAuth2.IDP.Handlers.Token (
    handleAuthCodeGrant,
    handleRefreshTokenGrant,
    handleToken,
 )
