{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : MCP.Server.HTTP.AppEnv
Description : Composite environment and error types for OAuth HTTP server
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC

This module provides composite environment and error types that unify
OAuth state storage, credential authentication, server configuration,
and tracing into a single environment for the HTTP server handlers.

= Design

The composite types enable use of @generic-lens@ and @HasType@/@AsType@
for accessing components without boilerplate. This allows handlers to
use constraints like:

@
handleSomeRoute
  :: (MonadReader env m, HasType OAuthTVarEnv env, MonadIO m)
  => m Response
@

Instead of passing the entire @AppEnv@ or writing custom has-field typeclasses.

= Usage

@
import MCP.Server.HTTP.AppEnv
import Control.Monad.Reader
import GHC.Generics (Generic)
import Data.Generics.Product (HasType)

-- Access a component via HasType
getOAuthEnv :: (MonadReader env m, HasType OAuthTVarEnv env) => m OAuthTVarEnv
getOAuthEnv = view (typed @OAuthTVarEnv)

-- Handle errors with toServerError
handleAppError :: AppError -> Handler a
handleAppError err = throwError (toServerError err)
@
-}
module MCP.Server.HTTP.AppEnv (
    -- * HTTP Server Configuration
    HTTPServerConfig (..),

    -- * Composite Environment
    AppEnv (..),

    -- * Composite Error
    AppError (..),

    -- * Application Monad
    AppM (..),
    runAppM,

    -- * Error Translation
    toServerError,
) where

import Control.Concurrent.STM (TVar, atomically, readTVar, writeTVar)
import Control.Concurrent.STM.TVar (readTVarIO)
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT (..), asks, runReaderT)
import Control.Monad.Time (MonadTime (..))
import Crypto.JOSE (JWK)
import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as TL
import Data.Time.Clock (UTCTime, addUTCTime)
import GHC.Generics (Generic)
import Lucid (renderText, toHtml)
import Network.Wai.Handler.Warp (Port)
import Servant (Handler, ServerError, err400, err401, err500, errBody)
import Servant.Auth.Server (JWTSettings)

import MCP.Server (ServerState)
import MCP.Server.Auth (OAuthConfig, ProtectedResourceMetadata)
import MCP.Trace.HTTP (HTTPTrace (HTTPOAuthBoundary))
import MCP.Types (Implementation, ServerCapabilities)
import Plow.Logging (IOTracer)
import Servant.OAuth2.IDP.Auth.Backend (AuthBackend (..))
import Servant.OAuth2.IDP.Auth.Demo (AuthUser, DemoAuthError (..), DemoCredentialEnv)
import Servant.OAuth2.IDP.Boundary (domainErrorToServerError)
import Servant.OAuth2.IDP.LoginFlowError (LoginFlowError)
import Servant.OAuth2.IDP.Store (OAuthStateStore (..))
import Servant.OAuth2.IDP.Store.InMemory (
    ExpiryConfig (..),
    OAuthState (..),
    OAuthStoreError (..),
    OAuthTVarEnv (..),
 )
import Servant.OAuth2.IDP.Types (
    AuthCodeId (..),
    AuthorizationCode (..),
    AuthorizationError (..),
    PendingAuthorization (..),
    SessionId (..),
    ValidationError (..),
    authorizationErrorToResponse,
    validationErrorToResponse,
 )

-- -----------------------------------------------------------------------------
-- HTTP Server Configuration
-- -----------------------------------------------------------------------------

{- | Configuration for running an MCP HTTP server

This type was moved from MCP.Server.HTTP to avoid circular imports.
-}
data HTTPServerConfig = HTTPServerConfig
    { httpPort :: Port
    , httpBaseUrl :: Text -- Base URL for OAuth endpoints (e.g., "https://api.example.com")
    , httpServerInfo :: Implementation
    , httpCapabilities :: ServerCapabilities
    , httpEnableLogging :: Bool
    , httpOAuthConfig :: Maybe OAuthConfig
    , httpJWK :: Maybe JWK -- JWT signing key
    , httpProtocolVersion :: Text -- MCP protocol version
    , httpProtectedResourceMetadata :: Maybe ProtectedResourceMetadata
    {- ^ Custom protected resource metadata.
    When Nothing, auto-generated from httpBaseUrl.
    -}
    }
    deriving (Generic)

-- -----------------------------------------------------------------------------
-- Composite Environment
-- -----------------------------------------------------------------------------

{- | Composite environment for the HTTP OAuth server.

Combines all dependencies needed by HTTP handlers:

* 'envOAuth': OAuth state storage (authorization codes, tokens, clients)
* 'envAuth': Credential authentication backend
* 'envConfig': HTTP server configuration (port, base URL, OAuth settings)
* 'envTracer': Structured tracing for HTTP and OAuth events
* 'envJWT': JWT settings for token signing and validation

Derives 'Generic' to enable @generic-lens@ access via 'HasType'.
-}
data AppEnv = AppEnv
    { envOAuth :: OAuthTVarEnv
    -- ^ OAuth state storage environment
    , envAuth :: DemoCredentialEnv
    -- ^ Credential authentication environment
    , envConfig :: HTTPServerConfig
    -- ^ HTTP server configuration
    , envTracer :: IOTracer HTTPTrace
    -- ^ Tracer for HTTP and OAuth events
    , envJWT :: JWTSettings
    -- ^ JWT settings for token signing and validation
    , envServerState :: TVar ServerState
    -- ^ MCP server state (for HTTP OAuth entry point)
    , envTimeProvider :: Maybe (TVar UTCTime)
    -- ^ Optional time provider for testing (Nothing = use IO time)
    }
    deriving (Generic)

-- -----------------------------------------------------------------------------
-- Application Monad
-- -----------------------------------------------------------------------------

{- | Application monad for HTTP handlers.

Combines ReaderT for environment access with ExceptT for error handling,
layered over Servant's Handler monad.

This monad provides:

* 'MonadReader AppEnv': Access to OAuth state, auth backend, config, and tracer
* 'MonadError AppError': Composable error handling via AppError sum type
* 'MonadIO': Lift IO operations
* Automatic conversion to Handler via natural transformation

== Usage

@
myHandler :: AppM ResponseType
myHandler = do
  -- Access environment components
  oauthEnv <- asks envOAuth

  -- Perform operations that may fail
  result <- lookupAuthCode codeId
  case result of
    Nothing -> throwError (ValidationErr "Invalid code")
    Just code -> ...
@
-}
newtype AppM a = AppM
    { unAppM :: ReaderT AppEnv (ExceptT AppError Handler) a
    }
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadReader AppEnv
        , MonadError AppError
        )

{- | Run an AppM computation in the Handler monad.

Executes the ReaderT and ExceptT layers, translating AppError to ServerError.

Uses 'domainErrorToServerError' from OAuth.Boundary for centralized error
translation with secure logging policy (associated types are logged but
return generic errors, fixed types are safe to expose).

@
handler :: AppEnv -> AppM a -> Handler a
handler env action = runAppM env action
@
-}
runAppM :: AppEnv -> AppM a -> Handler a
runAppM env action = do
    result <- runExceptT $ runReaderT (unAppM action) env
    case result of
        Left err -> do
            -- Use domainErrorToServerError for centralized translation
            -- Type annotation helps resolve ambiguous associated types
            mServerErr <- domainErrorToServerError @Handler @AppM (envTracer env) HTTPOAuthBoundary err
            case mServerErr of
                Just serverErr -> throwError serverErr
                Nothing -> throwError err500{errBody = "Internal server error"}
        Right a -> pure a

-- -----------------------------------------------------------------------------
-- Composite Error
-- -----------------------------------------------------------------------------

{- | Composite error type for the HTTP OAuth server.

Unifies all possible error types from different components:

* 'OAuthStoreErr': Storage backend errors (unavailable, internal errors)
* 'AuthBackendErr': Authentication failures (invalid credentials, user not found)
* 'ValidationErr': Semantic validation errors (redirect URI mismatch, unsupported response type)
* 'AuthorizationErr': OAuth authorization errors per RFC 6749 (invalid grant, access denied, etc.)
* 'LoginFlowErr': Login flow errors (cookies disabled, session expired, etc.)

Derives 'Generic' to enable @generic-lens@ projection via 'AsType'.
-}
data AppError
    = -- | Error from OAuth state storage
      OAuthStoreErr OAuthStoreError
    | -- | Error from credential authentication
      AuthBackendErr DemoAuthError
    | -- | Semantic validation error
      ValidationErr ValidationError
    | -- | OAuth authorization error
      AuthorizationErr AuthorizationError
    | -- | Login flow error
      LoginFlowErr LoginFlowError
    deriving (Generic)

-- -----------------------------------------------------------------------------
-- Error Translation
-- -----------------------------------------------------------------------------

{- | Translate application errors to Servant ServerError.

Maps domain-specific errors to appropriate HTTP status codes:

* 'OAuthStoreErr': Storage errors map to 500 Internal Server Error
* 'AuthBackendErr': Authentication errors map to 401 Unauthorized
* 'ValidationErr': Semantic validation errors map to 400 Bad Request
* 'AuthorizationErr': OAuth authorization errors map to appropriate status per RFC 6749

This function is used at the Servant boundary to convert domain errors
into HTTP responses.

== Examples

@
toServerError (OAuthStoreErr (StoreUnavailable "Database connection failed"))
-- err500 { errBody = "Storage error: Database connection failed" }

toServerError (AuthBackendErr InvalidCredentials)
-- err401 { errBody = "Authentication failed: Invalid credentials" }

toServerError (ValidationErr (ClientNotRegistered clientId))
-- err400 { errBody = "client_id not registered: ..." }

toServerError (AuthorizationErr (InvalidGrant "Code expired"))
-- err400 { errBody = JSON OAuth error response }
@
-}
toServerError :: AppError -> ServerError
toServerError (OAuthStoreErr storeErr) =
    case storeErr of
        StoreUnavailable msg ->
            err500{errBody = "Storage error: " <> toLBS msg}
        StoreInternalError msg ->
            err500{errBody = "Internal storage error: " <> toLBS msg}
toServerError (AuthBackendErr authErr) =
    case authErr of
        InvalidCredentials ->
            err401{errBody = "Authentication failed: Invalid credentials"}
        UserNotFound _ ->
            -- Don't leak user existence to unauthorized clients
            err401{errBody = "Authentication failed: Invalid credentials"}
toServerError (ValidationErr validationErr) =
    let (_status, message) = validationErrorToResponse validationErr
     in err400{errBody = toLBS message}
toServerError (AuthorizationErr authzErr) =
    let (_status, oauthResp) = authorizationErrorToResponse authzErr
     in err400{errBody = encode oauthResp}
toServerError (LoginFlowErr loginErr) =
    -- Render LoginFlowError as HTML using ToHtml instance
    err400{errBody = LBS.fromStrict . TE.encodeUtf8 . TL.toStrict . renderText . toHtml $ loginErr}

-- -----------------------------------------------------------------------------
-- Utilities
-- -----------------------------------------------------------------------------

-- | Convert Text to lazy ByteString for errBody
toLBS :: Text -> LBS.ByteString
toLBS = LBS.fromStrict . TE.encodeUtf8

-- -----------------------------------------------------------------------------
-- Typeclass Instances for AppM
-- -----------------------------------------------------------------------------

{- Note: AsType instances for ValidationError and AuthorizationError are
automatically derived by generic-lens from the Generic instance on AppError.
No manual instances needed.
-}

{- | MonadTime instance for AppM.

Delegates to IO's MonadTime instance to get real system time, unless
a test time provider (TVar UTCTime) is present in the environment.
-}
instance MonadTime AppM where
    currentTime = do
        mTimeProvider <- asks envTimeProvider
        case mTimeProvider of
            Just tvar -> liftIO $ readTVarIO tvar
            Nothing -> liftIO currentTime
    monotonicTime = liftIO monotonicTime

{- | OAuthStateStore instance for AppM.

Delegates operations to the in-memory TVar-based implementation via
the OAuthTVarEnv in the AppEnv. Uses ReaderT transformation to access
the OAuth environment component.

= IMPORTANT: Intentional Code Duplication

This instance duplicates expiry logic from "Servant.OAuth2.IDP.Store.InMemory"
in 'lookupAuthCode', 'consumeAuthCode', and 'lookupPendingAuth'. This is
INTENTIONAL and NECESSARY for correct test behavior.

== The MonadTime Context Issue

AppM implements 'MonadTime' via the @envTimeProvider@ TVar (see line 341-346),
which allows tests to control time by advancing the TVar. This is essential
for testing time-dependent OAuth flows (code expiry, token expiry, etc.).

However, if we delegated expiry-checking methods to InMemory via:

@
lookupAuthCode codeId = do
  oauthEnv <- asks envOAuth
  liftIO $ runReaderT (lookupAuthCode codeId) oauthEnv
@

The @runReaderT ... oauthEnv@ creates a @ReaderT OAuthTVarEnv IO@ context
where 'MonadTime' resolves to IO's instance (real system time), NOT AppM's
test time provider. This would break test time control completely.

== Why Not Alternatives?

1. Pass time explicitly: Would require changing OAuthStateStore API signatures
2. Remove MonadTime: Too radical, loses composability
3. Custom ReaderT wrapper: Complex, obscures the actual issue

== Consequence

The duplicated methods must be kept in sync manually with InMemory.hs.
When InMemory's expiry logic changes, this instance must be updated.

See: ADR-005 (specs\/004-oauth-auth-typeclasses\/adr\/ADR-005-time-provider-limitation.md)
See: Bead mcp-2z6.9
-}
instance OAuthStateStore AppM where
    type OAuthStateError AppM = OAuthStoreError
    type OAuthStateEnv AppM = OAuthTVarEnv
    type OAuthUser AppM = AuthUser

    storeAuthCode code = do
        oauthEnv <- asks envOAuth
        liftIO $ runReaderT (storeAuthCode code) oauthEnv

    lookupAuthCode codeId = do
        oauthEnv <- asks envOAuth
        -- lookupAuthCode uses MonadTime - run in AppM context
        now <- currentTime
        liftIO $ atomically $ do
            state <- readTVar (oauthStateVar oauthEnv)
            let key = unAuthCodeId codeId
            case Map.lookup key (authCodes state) of
                Nothing -> pure Nothing
                Just authCode ->
                    if now >= authExpiry authCode
                        then pure Nothing
                        else pure (Just authCode)

    deleteAuthCode codeId = do
        oauthEnv <- asks envOAuth
        liftIO $ runReaderT (deleteAuthCode codeId) oauthEnv

    consumeAuthCode codeId = do
        oauthEnv <- asks envOAuth
        -- consumeAuthCode uses MonadTime - run in AppM context
        now <- currentTime
        liftIO $ atomically $ do
            state <- readTVar (oauthStateVar oauthEnv)
            let key = unAuthCodeId codeId
            case Map.lookup key (authCodes state) of
                Nothing -> pure Nothing
                Just code
                    -- Check if expired
                    | now >= authExpiry code -> pure Nothing
                    | otherwise -> do
                        -- Delete the code atomically within the same transaction
                        let newState = state{authCodes = Map.delete key (authCodes state)}
                        writeTVar (oauthStateVar oauthEnv) newState
                        pure (Just code)

    storeAccessToken tokenId user = do
        oauthEnv <- asks envOAuth
        liftIO $ runReaderT (storeAccessToken tokenId user) oauthEnv

    lookupAccessToken tokenId = do
        oauthEnv <- asks envOAuth
        liftIO $ runReaderT (lookupAccessToken tokenId) oauthEnv

    storeRefreshToken tokenId pair = do
        oauthEnv <- asks envOAuth
        liftIO $ runReaderT (storeRefreshToken tokenId pair) oauthEnv

    lookupRefreshToken tokenId = do
        oauthEnv <- asks envOAuth
        liftIO $ runReaderT (lookupRefreshToken tokenId) oauthEnv

    updateRefreshToken tokenId pair = do
        oauthEnv <- asks envOAuth
        liftIO $ runReaderT (updateRefreshToken tokenId pair) oauthEnv

    storeClient clientId info = do
        oauthEnv <- asks envOAuth
        liftIO $ runReaderT (storeClient clientId info) oauthEnv

    lookupClient clientId = do
        oauthEnv <- asks envOAuth
        liftIO $ runReaderT (lookupClient clientId) oauthEnv

    storePendingAuth sessionId auth = do
        oauthEnv <- asks envOAuth
        liftIO $ runReaderT (storePendingAuth sessionId auth) oauthEnv

    lookupPendingAuth sessionId = do
        oauthEnv <- asks envOAuth
        -- lookupPendingAuth uses MonadTime - run in AppM context
        now <- currentTime
        let config = oauthExpiryConfig oauthEnv
        liftIO $ atomically $ do
            state <- readTVar (oauthStateVar oauthEnv)
            let key = unSessionId sessionId
            case Map.lookup key (pendingAuthorizations state) of
                Nothing -> pure Nothing
                Just auth ->
                    let expiryTime = addUTCTime (loginSessionExpiry config) (pendingCreatedAt auth)
                     in if now >= expiryTime
                            then pure Nothing
                            else pure (Just auth)

    deletePendingAuth sessionId = do
        oauthEnv <- asks envOAuth
        liftIO $ runReaderT (deletePendingAuth sessionId) oauthEnv

{- | AuthBackend instance for AppM.

Delegates credential validation to the demo credential implementation via
the DemoCredentialEnv in the AppEnv. Uses ReaderT transformation to access
the auth environment component.
-}
instance AuthBackend AppM where
    type AuthBackendError AppM = DemoAuthError
    type AuthBackendEnv AppM = DemoCredentialEnv
    type AuthBackendUser AppM = AuthUser

    validateCredentials username password = do
        authEnv <- asks envAuth
        liftIO $ runReaderT (validateCredentials username password) authEnv
