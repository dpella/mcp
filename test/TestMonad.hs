{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : TestMonad
Description : Deterministic test monad for OAuth typeclass testing
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC

This module provides a 'TestM' monad for deterministic testing of OAuth operations.
It allows controlling time, managing in-memory OAuth state, and validating credentials
in a pure, predictable way.

= Usage

@
import TestMonad
import Data.Time (UTCTime)
import qualified Data.Map.Strict as Map

testExample :: IO ()
testExample = do
  let env = mkTestEnv (read "2025-01-01 00:00:00 UTC") Map.empty
  result <- runTestM env $ do
    -- Set up test credentials
    addTestCredential (Username "alice") (mkPlaintextPassword "secret")

    -- Validate credentials
    isValid <- validateCredentials (Username "alice") (mkPlaintextPassword "secret")

    -- Advance time
    advanceTime 3600  -- 1 hour

    -- Check expiry
    now <- currentTime
    pure (isValid, now)

  print result
@

= Design

The 'TestM' monad is a 'ReaderT' over 'IO' that carries:

* 'TestEnv': Environment containing mock time and OAuth state
* In-memory storage: Using 'IORef' for mutable state
* Deterministic time: Settable and advanceable via helper functions

This design satisfies the typeclass instances:

* 'MonadTime': Returns mock time from environment
* 'OAuthStateStore': Reads/writes in-memory state
* 'AuthBackend': Validates against in-memory credential store
-}
module TestMonad (
    -- * Test Monad
    TestM (..),
    runTestM,

    -- * Test Environment
    TestEnv (..),
    mkTestEnv,

    -- * Helper Functions
    setTime,
    advanceTime,
    addTestCredential,
    getOAuthState,

    -- * Re-exports for convenience
    module Servant.OAuth2.IDP.Store,
    module Servant.OAuth2.IDP.Auth.Backend,
    module MCP.Server.Time,
) where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Data.ByteArray (ScrubbedBytes)
import Data.ByteArray qualified as BA
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text.Encoding qualified as TE
import Data.Time.Clock (UTCTime, addUTCTime)
import MCP.Server.Time
import Servant.OAuth2.IDP.Auth.Backend
import Servant.OAuth2.IDP.Auth.Demo (AuthUser (..))
import Servant.OAuth2.IDP.Store
import Servant.OAuth2.IDP.Types hiding (OAuthState)

-- -----------------------------------------------------------------------------
-- Test Environment
-- -----------------------------------------------------------------------------

{- | Test environment containing mock time and OAuth state.

This environment is passed to the 'TestM' monad via 'ReaderT'.
-}
data TestEnv = TestEnv
    { testTime :: IORef UTCTime
    -- ^ Current mock time (mutable)
    , testOAuthState :: IORef OAuthState
    -- ^ OAuth state (auth codes, tokens, clients, sessions)
    , testCredentials :: IORef CredentialStore
    -- ^ Credential store for authentication
    }

{- | OAuth state for in-memory storage.

Contains all OAuth-related data structures.
-}
data OAuthState = OAuthState
    { oauthAuthCodes :: Map AuthCodeId (AuthorizationCode AuthUser)
    , oauthAccessTokens :: Map AccessTokenId AuthUser
    , oauthRefreshTokens :: Map RefreshTokenId (ClientId, AuthUser)
    , oauthClients :: Map ClientId ClientInfo
    , oauthPendingAuths :: Map SessionId PendingAuthorization
    }

-- | Create an empty OAuth state.
emptyOAuthState :: OAuthState
emptyOAuthState =
    OAuthState
        { oauthAuthCodes = Map.empty
        , oauthAccessTokens = Map.empty
        , oauthRefreshTokens = Map.empty
        , oauthClients = Map.empty
        , oauthPendingAuths = Map.empty
        }

{- | Create a test environment with initial time and credentials.

@
let env = mkTestEnv (read "2025-01-01 00:00:00 UTC") Map.empty
@
-}
mkTestEnv :: UTCTime -> Map Username HashedPassword -> IO TestEnv
mkTestEnv initialTime initialCreds = do
    timeRef <- newIORef initialTime
    stateRef <- newIORef emptyOAuthState
    -- Create a test salt from a fixed string (for deterministic testing)
    let saltBytes = BA.convert (TE.encodeUtf8 "test-salt-for-deterministic-testing") :: ScrubbedBytes
        salt = Salt saltBytes
    credsRef <- newIORef (CredentialStore initialCreds salt)
    pure
        TestEnv
            { testTime = timeRef
            , testOAuthState = stateRef
            , testCredentials = credsRef
            }

-- -----------------------------------------------------------------------------
-- Test Monad
-- -----------------------------------------------------------------------------

{- | Test monad with controllable time and in-memory storage.

This is a 'ReaderT' over 'IO' that carries the 'TestEnv'.
It has instances for:

* 'MonadTime': Returns mock time
* 'OAuthStateStore': In-memory OAuth state
* 'AuthBackend': In-memory credential validation
-}
newtype TestM a = TestM {unTestM :: ReaderT TestEnv IO a}
    deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader TestEnv)

{- | Run a test computation with the given environment.

@
env <- mkTestEnv (read "2025-01-01 00:00:00 UTC") Map.empty
result <- runTestM env myTestComputation
@
-}
runTestM :: TestEnv -> TestM a -> IO a
runTestM env (TestM action) = runReaderT action env

-- -----------------------------------------------------------------------------
-- MonadTime Instance
-- -----------------------------------------------------------------------------

instance MonadTime TestM where
    currentTime = TestM $ do
        env <- ask
        liftIO $ readIORef (testTime env)

    monotonicTime = TestM $ do
        -- For testing, we can just return a fixed value or derive from currentTime
        -- Since we don't need high-precision timing in tests, return 0
        pure 0.0

-- -----------------------------------------------------------------------------
-- OAuthStateStore Instance
-- -----------------------------------------------------------------------------

instance OAuthStateStore TestM where
    type OAuthStateError TestM = ()
    type OAuthStateEnv TestM = TestEnv
    type OAuthUser TestM = AuthUser

    -- Authorization Code Operations
    storeAuthCode code = TestM $ do
        env <- ask
        liftIO $ modifyIORef' (testOAuthState env) $ \s ->
            s{oauthAuthCodes = Map.insert (authCodeId code) code (oauthAuthCodes s)}

    lookupAuthCode codeId = TestM $ do
        env <- ask
        now <- liftIO $ readIORef (testTime env)
        state <- liftIO $ readIORef (testOAuthState env)
        pure $ case Map.lookup codeId (oauthAuthCodes state) of
            Nothing -> Nothing
            Just code
                | authExpiry code < now -> Nothing -- Expired
                | otherwise -> Just code

    deleteAuthCode codeId = TestM $ do
        env <- ask
        liftIO $ modifyIORef' (testOAuthState env) $ \s ->
            s{oauthAuthCodes = Map.delete codeId (oauthAuthCodes s)}

    consumeAuthCode codeId = TestM $ do
        env <- ask
        now <- liftIO $ readIORef (testTime env)
        -- Use atomicModifyIORef' for atomicity
        liftIO $ atomicModifyIORef' (testOAuthState env) $ \s ->
            case Map.lookup codeId (oauthAuthCodes s) of
                Nothing -> (s, Nothing)
                Just code
                    | authExpiry code < now -> (s, Nothing) -- Expired
                    | otherwise ->
                        -- Delete and return the code atomically
                        let newState = s{oauthAuthCodes = Map.delete codeId (oauthAuthCodes s)}
                         in (newState, Just code)

    -- Access Token Operations
    storeAccessToken tokenId user = TestM $ do
        env <- ask
        liftIO $ modifyIORef' (testOAuthState env) $ \s ->
            s{oauthAccessTokens = Map.insert tokenId user (oauthAccessTokens s)}

    lookupAccessToken tokenId = TestM $ do
        env <- ask
        state <- liftIO $ readIORef (testOAuthState env)
        pure $ Map.lookup tokenId (oauthAccessTokens state)

    -- Refresh Token Operations
    storeRefreshToken tokenId clientUser = TestM $ do
        env <- ask
        liftIO $ modifyIORef' (testOAuthState env) $ \s ->
            s{oauthRefreshTokens = Map.insert tokenId clientUser (oauthRefreshTokens s)}

    lookupRefreshToken tokenId = TestM $ do
        env <- ask
        state <- liftIO $ readIORef (testOAuthState env)
        pure $ Map.lookup tokenId (oauthRefreshTokens state)

    updateRefreshToken tokenId clientUser = TestM $ do
        env <- ask
        liftIO $ modifyIORef' (testOAuthState env) $ \s ->
            s{oauthRefreshTokens = Map.insert tokenId clientUser (oauthRefreshTokens s)}

    -- Client Registration Operations
    storeClient clientId info = TestM $ do
        env <- ask
        liftIO $ modifyIORef' (testOAuthState env) $ \s ->
            s{oauthClients = Map.insert clientId info (oauthClients s)}

    lookupClient clientId = TestM $ do
        env <- ask
        state <- liftIO $ readIORef (testOAuthState env)
        pure $ Map.lookup clientId (oauthClients state)

    -- Pending Authorization Operations
    storePendingAuth sessionId auth = TestM $ do
        env <- ask
        liftIO $ modifyIORef' (testOAuthState env) $ \s ->
            s{oauthPendingAuths = Map.insert sessionId auth (oauthPendingAuths s)}

    lookupPendingAuth sessionId = TestM $ do
        env <- ask
        now <- liftIO $ readIORef (testTime env)
        state <- liftIO $ readIORef (testOAuthState env)
        -- Check if session expired (600 seconds = 10 minutes, matching InMemory default)
        -- The law tests set createdAt to 2019-12-31, and currentTime is 2020-01-01,
        -- so expired sessions will correctly return Nothing.
        let sessionExpirySeconds = 600 :: Integer -- 10 minutes (matches loginSessionExpiry default)
        pure $ case Map.lookup sessionId (oauthPendingAuths state) of
            Nothing -> Nothing
            Just auth
                | addUTCTime (fromIntegral sessionExpirySeconds) (pendingCreatedAt auth) < now ->
                    Nothing -- Expired
                | otherwise -> Just auth

    deletePendingAuth sessionId = TestM $ do
        env <- ask
        liftIO $ modifyIORef' (testOAuthState env) $ \s ->
            s{oauthPendingAuths = Map.delete sessionId (oauthPendingAuths s)}

-- -----------------------------------------------------------------------------
-- AuthBackend Instance
-- -----------------------------------------------------------------------------

instance AuthBackend TestM where
    type AuthBackendError TestM = ()
    type AuthBackendEnv TestM = TestEnv
    type AuthBackendUser TestM = AuthUser

    validateCredentials username password = TestM $ do
        env <- ask
        store <- liftIO $ readIORef (testCredentials env)
        case Map.lookup username (storeCredentials store) of
            Nothing -> pure Nothing -- User not found
            Just storedHash -> do
                let candidateHash = mkHashedPassword (storeSalt store) password
                if storedHash == candidateHash -- Constant-time via ScrubbedBytes Eq
                    then do
                        let userId = UserId (unUsername username)
                        let authUser =
                                AuthUser
                                    { userUserId = userId
                                    , userUserEmail = Just (unUsername username <> "@test.local")
                                    , userUserName = Just (unUsername username)
                                    }
                        pure $ Just authUser
                    else pure Nothing

-- -----------------------------------------------------------------------------
-- Helper Functions
-- -----------------------------------------------------------------------------

{- | Set the mock time to a specific value.

@
setTime (read "2025-12-31 23:59:59 UTC")
@
-}
setTime :: UTCTime -> TestM ()
setTime newTime = TestM $ do
    env <- ask
    liftIO $ writeIORef (testTime env) newTime

{- | Advance the mock time by a number of seconds.

@
advanceTime 3600  -- Advance by 1 hour
@
-}
advanceTime :: Integer -> TestM ()
advanceTime seconds = TestM $ do
    env <- ask
    liftIO $ modifyIORef' (testTime env) $ \t ->
        addUTCTime (fromIntegral seconds) t

{- | Add a test credential to the store.

@
addTestCredential (Username "alice") (mkPlaintextPassword "secret")
@
-}
addTestCredential :: Username -> PlaintextPassword -> TestM ()
addTestCredential username password = TestM $ do
    env <- ask
    liftIO $ modifyIORef' (testCredentials env) $ \store ->
        let hash = mkHashedPassword (storeSalt store) password
         in store{storeCredentials = Map.insert username hash (storeCredentials store)}

{- | Get the current OAuth state (for testing/inspection).

@
state <- getOAuthState
let numClients = Map.size (oauthClients state)
@
-}
getOAuthState :: TestM OAuthState
getOAuthState = TestM $ do
    env <- ask
    liftIO $ readIORef (testOAuthState env)
