{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Laws.OAuthStateStoreSpec
Description : Polymorphic typeclass law tests for OAuthStateStore
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC

This module provides polymorphic property-based tests for the 'OAuthStateStore'
typeclass laws. These tests are implementation-agnostic and can be used with
any 'OAuthStateStore' instance (in-memory, PostgreSQL, Redis, etc.).

== Usage

@
import Laws.OAuthStateStoreSpec (oauthStateStoreLaws)
import MyApp.Storage (AppM, runAppM)

spec :: Spec
spec = do
    describe "AppM OAuthStateStore instance" $ do
        oauthStateStoreLaws runAppM
@

The 'oauthStateStoreLaws' function requires:
1. An 'OAuthStateStore' instance for your monad
2. A 'MonadTime' instance for your monad
3. A runner function: @forall a. m a -> IO a@

== Tested Laws

* **Round-trip**: Store and lookup returns the stored value (when not expired)
* **Delete**: Lookup after delete returns Nothing
* **Idempotence**: Store twice is same as store once
* **Overwrite**: Second store with same key replaces first value
* **Expiry**: Lookup returns Nothing for expired codes/sessions

These laws ensure that any 'OAuthStateStore' implementation behaves correctly
and consistently, regardless of the underlying storage backend.
-}
module Laws.OAuthStateStoreSpec (
    oauthStateStoreLaws,
) where

import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), addUTCTime)
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary, ioProperty, (===))

-- OAuth domain types and typeclass (also imports orphan Arbitrary instances)
import Servant.OAuth2.IDP.Store (OAuthStateStore (..))
import Servant.OAuth2.IDP.Types (
    AccessTokenId,
    AuthorizationCode (..),
    ClientId,
    ClientInfo,
    PendingAuthorization (..),
    RefreshTokenId,
    SessionId,
 )

{- | Polymorphic spec: tests the OAuthStateStore INTERFACE, not implementation.

This spec is polymorphic over the monad 'm', allowing it to test any
'OAuthStateStore' instance. The caller provides a runner function to
execute 'm' actions in 'IO'.

Example usage:

@
-- Test in-memory implementation
spec :: Spec
spec = oauthStateStoreLaws runInMemory

-- Test PostgreSQL implementation
spec :: Spec
spec = oauthStateStoreLaws runPostgres
@

The runner function typically:
1. Sets up necessary environment (connections, state, etc.)
2. Runs the 'm' action
3. Cleans up resources

For simple in-memory implementations, it might just unwrap a 'ReaderT':

@
runInMemory :: InMemoryM a -> IO a
runInMemory action = do
    state <- newTVarIO emptyState
    runReaderT action state
@
-}
oauthStateStoreLaws ::
    forall m.
    ( OAuthStateStore m
    , Arbitrary (OAuthUser m)
    , Eq (OAuthUser m)
    , Show (OAuthUser m)
    ) =>
    -- | Runner function to execute 'm' in 'IO'
    (forall a. m a -> IO a) ->
    Spec
oauthStateStoreLaws runM = describe "OAuthStateStore laws" $ do
    describe "AuthorizationCode" $ do
        prop "round-trip: lookup after store returns the value (non-expired)" $
            \(code :: AuthorizationCode (OAuthUser m)) -> ioProperty $ do
                -- Ensure code is not expired by setting expiry far in future
                let validCode = code{authExpiry = addUTCTime 86400 (authExpiry code)}
                result <- runM $ do
                    storeAuthCode validCode
                    lookupAuthCode (authCodeId validCode)
                pure $ result === Just validCode

        prop "delete: lookup after delete returns Nothing" $
            \(code :: AuthorizationCode (OAuthUser m)) -> ioProperty $ do
                result <- runM $ do
                    storeAuthCode code
                    deleteAuthCode (authCodeId code)
                    lookupAuthCode (authCodeId code)
                pure $ result === Nothing

        prop "idempotence: store twice is same as store once" $
            \(code :: AuthorizationCode (OAuthUser m)) -> ioProperty $ do
                let validCode = code{authExpiry = addUTCTime 86400 (authExpiry code)}
                result <- runM $ do
                    storeAuthCode validCode
                    storeAuthCode validCode
                    lookupAuthCode (authCodeId validCode)
                pure $ result === Just validCode

        prop "overwrite: second store with same key replaces first" $
            \(code1 :: AuthorizationCode (OAuthUser m)) (code2 :: AuthorizationCode (OAuthUser m)) -> ioProperty $ do
                let code2' =
                        code2
                            { authCodeId = authCodeId code1
                            , authExpiry = addUTCTime 86400 (authExpiry code2)
                            }
                result <- runM $ do
                    storeAuthCode code1
                    storeAuthCode code2'
                    lookupAuthCode (authCodeId code1)
                pure $ result === Just code2'

        prop "expiry: lookup returns Nothing for expired codes" $
            \(code :: AuthorizationCode (OAuthUser m)) -> ioProperty $ do
                -- Make code expired by setting expiry to a fixed past time (year 2019)
                -- This ensures expiry < currentTime (2020-01-01) for all test runs
                let pastTime = UTCTime (fromGregorian 2019 12 31) 0
                let expiredCode = code{authExpiry = pastTime}
                result <- runM $ do
                    storeAuthCode expiredCode
                    lookupAuthCode (authCodeId expiredCode)
                pure $ result === Nothing

    describe "ClientInfo" $ do
        prop "round-trip: lookup after store returns the value" $
            \(clientId :: ClientId) (info :: ClientInfo) -> ioProperty $ do
                result <- runM $ do
                    storeClient clientId info
                    lookupClient clientId
                pure $ result === Just info

        prop "idempotence: store twice is same as store once" $
            \(clientId :: ClientId) (info :: ClientInfo) -> ioProperty $ do
                result <- runM $ do
                    storeClient clientId info
                    storeClient clientId info
                    lookupClient clientId
                pure $ result === Just info

        prop "overwrite: second store with same key replaces first" $
            \(clientId :: ClientId) (info1 :: ClientInfo) (info2 :: ClientInfo) -> ioProperty $ do
                result <- runM $ do
                    storeClient clientId info1
                    storeClient clientId info2
                    lookupClient clientId
                pure $ result === Just info2

    describe "PendingAuthorization" $ do
        prop "round-trip: lookup after store returns the value (non-expired)" $
            \(sessionId :: SessionId) (pending :: PendingAuthorization) -> ioProperty $ do
                -- Ensure session is not expired by setting creation time in recent past
                let validPending = pending{pendingCreatedAt = addUTCTime (-60) (pendingCreatedAt pending)}
                result <- runM $ do
                    storePendingAuth sessionId validPending
                    lookupPendingAuth sessionId
                pure $ result === Just validPending

        prop "delete: lookup after delete returns Nothing" $
            \(sessionId :: SessionId) (pending :: PendingAuthorization) -> ioProperty $ do
                result <- runM $ do
                    storePendingAuth sessionId pending
                    deletePendingAuth sessionId
                    lookupPendingAuth sessionId
                pure $ result === Nothing

        prop "idempotence: store twice is same as store once" $
            \(sessionId :: SessionId) (pending :: PendingAuthorization) -> ioProperty $ do
                let validPending = pending{pendingCreatedAt = addUTCTime (-60) (pendingCreatedAt pending)}
                result <- runM $ do
                    storePendingAuth sessionId validPending
                    storePendingAuth sessionId validPending
                    lookupPendingAuth sessionId
                pure $ result === Just validPending

        prop "expiry: lookup returns Nothing for expired sessions" $
            \(sessionId :: SessionId) (pending :: PendingAuthorization) -> ioProperty $ do
                -- Make session expired by setting creation time to a fixed past time (year 2019)
                -- This ensures createdAt + loginSessionExpiry < currentTime (2020-01-01) for all test runs
                let pastTime = UTCTime (fromGregorian 2019 12 31) 0
                let expiredPending = pending{pendingCreatedAt = pastTime}
                result <- runM $ do
                    storePendingAuth sessionId expiredPending
                    lookupPendingAuth sessionId
                pure $ result === Nothing

    describe "AccessToken" $ do
        prop "round-trip: lookup after store returns the value" $
            \(tokenId :: AccessTokenId) (user :: OAuthUser m) -> ioProperty $ do
                result <- runM $ do
                    storeAccessToken tokenId user
                    lookupAccessToken tokenId
                pure $ result === Just user

        prop "idempotence: store twice is same as store once" $
            \(tokenId :: AccessTokenId) (user :: OAuthUser m) -> ioProperty $ do
                result <- runM $ do
                    storeAccessToken tokenId user
                    storeAccessToken tokenId user
                    lookupAccessToken tokenId
                pure $ result === Just user

        prop "overwrite: second store with same key replaces first" $
            \(tokenId :: AccessTokenId) (user1 :: OAuthUser m) (user2 :: OAuthUser m) -> ioProperty $ do
                result <- runM $ do
                    storeAccessToken tokenId user1
                    storeAccessToken tokenId user2
                    lookupAccessToken tokenId
                pure $ result === Just user2

    describe "RefreshToken" $ do
        prop "round-trip: lookup after store returns the value" $
            \(tokenId :: RefreshTokenId) (clientId :: ClientId) (user :: OAuthUser m) -> ioProperty $ do
                result <- runM $ do
                    storeRefreshToken tokenId (clientId, user)
                    lookupRefreshToken tokenId
                pure $ result === Just (clientId, user)

        prop "idempotence: store twice is same as store once" $
            \(tokenId :: RefreshTokenId) (clientId :: ClientId) (user :: OAuthUser m) -> ioProperty $ do
                result <- runM $ do
                    storeRefreshToken tokenId (clientId, user)
                    storeRefreshToken tokenId (clientId, user)
                    lookupRefreshToken tokenId
                pure $ result === Just (clientId, user)

        prop "overwrite: second store with same key replaces first" $
            \(tokenId :: RefreshTokenId) (c1 :: ClientId) (u1 :: OAuthUser m) (c2 :: ClientId) (u2 :: OAuthUser m) -> ioProperty $ do
                result <- runM $ do
                    storeRefreshToken tokenId (c1, u1)
                    storeRefreshToken tokenId (c2, u2)
                    lookupRefreshToken tokenId
                pure $ result === Just (c2, u2)

        prop "updateRefreshToken: overwrite semantics" $
            \(tokenId :: RefreshTokenId) (c1 :: ClientId) (u1 :: OAuthUser m) (c2 :: ClientId) (u2 :: OAuthUser m) -> ioProperty $ do
                result <- runM $ do
                    storeRefreshToken tokenId (c1, u1)
                    updateRefreshToken tokenId (c2, u2)
                    lookupRefreshToken tokenId
                pure $ result === Just (c2, u2)
