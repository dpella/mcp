{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module      : Servant.OAuth2.IDP.Server
Description : Polymorphic OAuth 2.1 server using typeclass architecture
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC

This module provides a polymorphic OAuth 2.1 server implementation using
the OAuthStateStore and AuthBackend typeclasses for pluggable backends.

= Architecture

The server is polymorphic over the monad @m@, requiring:

* 'OAuthStateStore m': Storage for OAuth state (codes, tokens, clients)
* 'AuthBackend m': Credential validation backend
* 'MonadIO m': Ability to perform IO operations

This allows the server to work with different storage and auth backends:

* In-memory (demo/testing)
* PostgreSQL (production)
* Redis (caching)
* LDAP/Active Directory (authentication)

= Usage

@
-- Create server with specific monad
server :: Server OAuthAPI
server = hoistServer (Proxy :: Proxy OAuthAPI) (runAppM appEnv) oauthServer
@

= Module Structure

Handler implementations are in Servant.OAuth2.IDP.Handlers module.
This module re-exports the handlers and provides the main oauthServer entry point.
-}
module Servant.OAuth2.IDP.Server (
    -- * API Definition (re-exported from API module)
    OAuthAPI,
    ProtectedResourceAPI,
    LoginAPI,
    HTML,

    -- * Request/Response Types (re-exported from API module)
    ClientRegistrationRequest,
    ClientRegistrationResponse,
    LoginForm,
    TokenResponse,

    -- * Constraint Alias
    OAuthConstraints,

    -- * Server Implementation
    oauthServer,

    -- * Polymorphic Handlers
    handleMetadata,
    handleProtectedResourceMetadata,
    handleRegister,
    handleAuthorize,
    handleLogin,
    handleToken,
    handleAuthCodeGrant,
    handleRefreshTokenGrant,
) where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Data.Generics.Product (HasType)
import Data.Generics.Sum.Typed (AsType)
import Servant (
    ServerT,
    (:<|>) (..),
 )
import Servant.Auth.Server (JWTSettings, ToJWT)

import Plow.Logging (IOTracer)
import Servant.OAuth2.IDP.API (
    ClientRegistrationRequest,
    ClientRegistrationResponse,
    HTML,
    LoginAPI,
    LoginForm,
    OAuthAPI,
    ProtectedResourceAPI,
    TokenResponse,
 )
import Servant.OAuth2.IDP.Auth.Backend (AuthBackend (..))
import Servant.OAuth2.IDP.Config (OAuthEnv)
import Servant.OAuth2.IDP.Errors (AuthorizationError, LoginFlowError, ValidationError)
import Servant.OAuth2.IDP.Handlers (
    handleAuthCodeGrant,
    handleAuthorize,
    handleLogin,
    handleMetadata,
    handleProtectedResourceMetadata,
    handleRefreshTokenGrant,
    handleRegister,
    handleToken,
 )
import Servant.OAuth2.IDP.Store (OAuthStateStore (..))
import Servant.OAuth2.IDP.Trace (OAuthTrace)

-- -----------------------------------------------------------------------------
-- Constraint Alias
-- -----------------------------------------------------------------------------

{- | Constraint alias for OAuth server operations.

Captures all the typeclass constraints needed for OAuth handlers:

* 'OAuthStateStore m': Storage for OAuth state
* 'AuthBackend m': Credential validation
* 'MonadIO m': Ability to perform IO operations

Handlers can use this alias to reduce boilerplate:

@
handleSomeRoute :: (OAuthConstraints m) => ... -> m Response
@
-}
type OAuthConstraints m =
    ( OAuthStateStore m
    , AuthBackend m
    , MonadIO m
    )

-- -----------------------------------------------------------------------------
-- Server Implementation
-- -----------------------------------------------------------------------------

{- | OAuth server implementation (polymorphic over monad).

This is the main entry point for the OAuth server. It provides handlers
for all OAuth endpoints, polymorphic over the monad @m@.

All handlers have been ported from HTTP.hs to use the typeclass-based
architecture (OAuthStateStore, AuthBackend, MonadTime).

== Usage

To use this server with a ReaderT (ExceptT m):

@
-- Create AppEnv combining all dependencies
let appEnv = AppEnv
      { envOAuth = oauthTVarEnv
      , envAuth = demoCredEnv
      , envConfig = config
      , envTracer = tracer
      , envJWT = jwtSettings
      }

-- Convert to Servant Handler using hoistServerWithContext
let ctx = cookieSettings :. jwtSettings :. EmptyContext
application = serveWithContext
  (Proxy :: Proxy OAuthAPI)
  ctx
  (hoistServerWithContext
    (Proxy :: Proxy OAuthAPI)
    (Proxy :: '[CookieSettings, JWTSettings])
    (runAppM appEnv)
    oauthServer)
@

Or with a custom monad implementing the required typeclasses:

@
customHandler :: CustomMonad (Server OAuthAPI)
customHandler = do
  env <- ask
  pure $ hoistServer (Proxy :: Proxy OAuthAPI) (runCustomMonad env) oauthServer
@
-}
oauthServer ::
    forall m env e.
    ( OAuthStateStore m
    , AuthBackend m
    , AuthBackendUser m ~ OAuthUser m
    , ToJWT (OAuthUser m)
    , MonadIO m
    , MonadReader env m
    , MonadError e m
    , AsType ValidationError e
    , AsType AuthorizationError e
    , AsType LoginFlowError e
    , HasType OAuthEnv env
    , HasType (IOTracer OAuthTrace) env
    , HasType JWTSettings env
    ) =>
    ServerT OAuthAPI m
oauthServer =
    handleProtectedResourceMetadata
        :<|> handleMetadata
        :<|> handleRegister
        :<|> handleAuthorize
        :<|> handleLogin
        :<|> handleToken
