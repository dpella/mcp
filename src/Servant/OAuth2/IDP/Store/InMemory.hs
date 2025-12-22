{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Servant.OAuth2.IDP.Store.InMemory
Description : TVar-based in-memory implementation of OAuthStateStore
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com

This module provides an in-memory TVar-based implementation of the 'OAuthStateStore'
typeclass suitable for development, testing, and single-instance deployments.

= Usage

@
import Servant.OAuth2.IDP.Store.InMemory
import Control.Monad.Reader (runReaderT)
import Control.Monad.IO.Class (MonadIO)

-- Create environment with default expiry settings
env <- newOAuthTVarEnv defaultExpiryConfig

-- Run OAuth operations
result <- runReaderT myOAuthOperation env
@

= Expiry Handling

The in-memory store automatically filters expired entries during lookup operations:

* 'lookupAuthCode': Returns 'Nothing' if the authorization code has expired
* 'lookupPendingAuth': Returns 'Nothing' if the pending authorization session has expired

Expiry times are configured via 'ExpiryConfig':

* 'authCodeExpiry': How long authorization codes remain valid (default: 10 minutes)
* 'loginSessionExpiry': How long pending authorization sessions remain valid (default: 10 minutes)

= Thread Safety

All operations use STM transactions via 'TVar', ensuring atomicity and consistency
across concurrent access.
-}
module Servant.OAuth2.IDP.Store.InMemory (
    -- * Environment
    OAuthTVarEnv (..),
    ExpiryConfig (..),
    OAuthState (..),

    -- * Error type
    OAuthStoreError (..),

    -- * Initialization
    newOAuthTVarEnv,
    defaultExpiryConfig,
    emptyOAuthState,
) where

import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, writeTVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.Time (MonadTime (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime, addUTCTime)
import Servant.OAuth2.IDP.Auth.Demo (AuthUser)
import Servant.OAuth2.IDP.Store (OAuthStateStore (..))
import Servant.OAuth2.IDP.Types (
    AccessTokenId,
    AuthCodeId,
    AuthorizationCode (..),
    ClientId,
    ClientInfo,
    PendingAuthorization (..),
    RefreshTokenId,
    SessionId,
 )

-- -----------------------------------------------------------------------------
-- Environment and Configuration
-- -----------------------------------------------------------------------------

-- | Environment for TVar-based OAuth state storage
data OAuthTVarEnv = OAuthTVarEnv
    { oauthStateVar :: TVar OAuthState
    -- ^ Mutable reference to OAuth state
    , oauthExpiryConfig :: ExpiryConfig
    -- ^ Configuration for expiry times
    }

-- | Configuration for expiry times
data ExpiryConfig = ExpiryConfig
    { authCodeExpiry :: NominalDiffTime
    -- ^ How long authorization codes remain valid (default: 600 seconds / 10 minutes)
    , loginSessionExpiry :: NominalDiffTime
    -- ^ How long pending authorization sessions remain valid (default: 600 seconds / 10 minutes)
    }

-- | Default expiry configuration (10 minutes for both auth codes and login sessions)
defaultExpiryConfig :: ExpiryConfig
defaultExpiryConfig =
    ExpiryConfig
        { authCodeExpiry = 600 -- 10 minutes
        , loginSessionExpiry = 600 -- 10 minutes
        }

-- | OAuth server state stored in memory using maps
data OAuthState = OAuthState
    { authCodes :: Map AuthCodeId (AuthorizationCode AuthUser)
    -- ^ Authorization codes keyed by AuthCodeId (stores full user)
    , accessTokens :: Map AccessTokenId AuthUser
    -- ^ Access tokens keyed by AccessTokenId
    , refreshTokens :: Map RefreshTokenId (ClientId, AuthUser)
    -- ^ Refresh tokens keyed by RefreshTokenId
    , registeredClients :: Map ClientId ClientInfo
    -- ^ Registered clients keyed by ClientId
    , pendingAuthorizations :: Map SessionId PendingAuthorization
    -- ^ Pending authorizations keyed by SessionId
    }

-- | Create empty OAuth state
emptyOAuthState :: OAuthState
emptyOAuthState =
    OAuthState
        { authCodes = Map.empty
        , accessTokens = Map.empty
        , refreshTokens = Map.empty
        , registeredClients = Map.empty
        , pendingAuthorizations = Map.empty
        }

-- | Create new OAuth environment with the given expiry configuration
newOAuthTVarEnv :: (MonadIO m) => ExpiryConfig -> m OAuthTVarEnv
newOAuthTVarEnv config = do
    stateVar <- liftIO $ newTVarIO emptyOAuthState
    pure
        OAuthTVarEnv
            { oauthStateVar = stateVar
            , oauthExpiryConfig = config
            }

-- -----------------------------------------------------------------------------
-- Error Type
-- -----------------------------------------------------------------------------

-- | Errors that can occur during in-memory OAuth operations
data OAuthStoreError
    = -- | The storage backend is unavailable
      StoreUnavailable Text
    | -- | An internal error occurred
      StoreInternalError Text
    deriving (Eq, Show)

-- -----------------------------------------------------------------------------
-- OAuthStateStore Instance
-- -----------------------------------------------------------------------------

instance (MonadIO m, MonadTime m) => OAuthStateStore (ReaderT OAuthTVarEnv m) where
    type OAuthStateError (ReaderT OAuthTVarEnv m) = OAuthStoreError
    type OAuthStateEnv (ReaderT OAuthTVarEnv m) = OAuthTVarEnv
    type OAuthUser (ReaderT OAuthTVarEnv m) = AuthUser

    -- Authorization Code Operations

    storeAuthCode code = do
        env <- ask
        liftIO . atomically $ do
            state <- readTVar (oauthStateVar env)
            let key = authCodeId code
            let newState = state{authCodes = Map.insert key code (authCodes state)}
            writeTVar (oauthStateVar env) newState

    lookupAuthCode codeId = do
        env <- ask
        now <- currentTime
        liftIO . atomically $ do
            state <- readTVar (oauthStateVar env)
            case Map.lookup codeId (authCodes state) of
                Nothing -> pure Nothing
                Just code
                    -- Check if expired
                    | now >= authExpiry code -> pure Nothing
                    | otherwise -> pure (Just code)

    deleteAuthCode codeId = do
        env <- ask
        liftIO . atomically $ do
            state <- readTVar (oauthStateVar env)
            let newState = state{authCodes = Map.delete codeId (authCodes state)}
            writeTVar (oauthStateVar env) newState

    consumeAuthCode codeId = do
        env <- ask
        now <- currentTime
        liftIO . atomically $ do
            state <- readTVar (oauthStateVar env)
            case Map.lookup codeId (authCodes state) of
                Nothing -> pure Nothing
                Just code
                    -- Check if expired
                    | now >= authExpiry code -> pure Nothing
                    | otherwise -> do
                        -- Delete the code atomically within the same transaction
                        let newState = state{authCodes = Map.delete codeId (authCodes state)}
                        writeTVar (oauthStateVar env) newState
                        pure (Just code)

    -- Access Token Operations

    storeAccessToken tokenId user = do
        env <- ask
        liftIO . atomically $ do
            state <- readTVar (oauthStateVar env)
            let newState = state{accessTokens = Map.insert tokenId user (accessTokens state)}
            writeTVar (oauthStateVar env) newState

    lookupAccessToken tokenId = do
        env <- ask
        liftIO . atomically $ do
            state <- readTVar (oauthStateVar env)
            pure $ Map.lookup tokenId (accessTokens state)

    -- Refresh Token Operations

    storeRefreshToken tokenId pair = do
        env <- ask
        liftIO . atomically $ do
            state <- readTVar (oauthStateVar env)
            let newState = state{refreshTokens = Map.insert tokenId pair (refreshTokens state)}
            writeTVar (oauthStateVar env) newState

    lookupRefreshToken tokenId = do
        env <- ask
        liftIO . atomically $ do
            state <- readTVar (oauthStateVar env)
            pure $ Map.lookup tokenId (refreshTokens state)

    updateRefreshToken tokenId pair = do
        env <- ask
        liftIO . atomically $ do
            state <- readTVar (oauthStateVar env)
            let newState = state{refreshTokens = Map.insert tokenId pair (refreshTokens state)}
            writeTVar (oauthStateVar env) newState

    -- Client Registration Operations

    storeClient clientId info = do
        env <- ask
        liftIO . atomically $ do
            state <- readTVar (oauthStateVar env)
            let newState = state{registeredClients = Map.insert clientId info (registeredClients state)}
            writeTVar (oauthStateVar env) newState

    lookupClient clientId = do
        env <- ask
        liftIO . atomically $ do
            state <- readTVar (oauthStateVar env)
            pure $ Map.lookup clientId (registeredClients state)

    -- Pending Authorization Operations

    storePendingAuth sessionId auth = do
        env <- ask
        liftIO . atomically $ do
            state <- readTVar (oauthStateVar env)
            let newState = state{pendingAuthorizations = Map.insert sessionId auth (pendingAuthorizations state)}
            writeTVar (oauthStateVar env) newState

    lookupPendingAuth sessionId = do
        env <- ask
        now <- currentTime
        let config = oauthExpiryConfig env
        liftIO . atomically $ do
            state <- readTVar (oauthStateVar env)
            case Map.lookup sessionId (pendingAuthorizations state) of
                Nothing -> pure Nothing
                Just auth ->
                    -- Check if session has expired
                    let expiryTime = addUTCTime (loginSessionExpiry config) (pendingCreatedAt auth)
                     in if now >= expiryTime
                            then pure Nothing
                            else pure (Just auth)

    deletePendingAuth sessionId = do
        env <- ask
        liftIO . atomically $ do
            state <- readTVar (oauthStateVar env)
            let newState = state{pendingAuthorizations = Map.delete sessionId (pendingAuthorizations state)}
            writeTVar (oauthStateVar env) newState
