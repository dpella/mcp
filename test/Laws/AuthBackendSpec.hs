{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Laws.AuthBackendSpec
Description : Polymorphic typeclass law tests for AuthBackend
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC

This module provides polymorphic property-based tests for the 'AuthBackend'
typeclass laws. These tests are implementation-agnostic and can be used with
any 'AuthBackend' instance (in-memory, LDAP, database, etc.).

== Usage

@
import Laws.AuthBackendSpec (authBackendLaws, authBackendKnownCredentials)
import MyApp.Auth (AppM, runAppM)

spec :: Spec
spec = do
    describe "AppM AuthBackend instance" $ do
        authBackendLaws runAppM
        authBackendKnownCredentials runAppM validUser validPass invalidPass
@

The 'authBackendLaws' function requires:
1. An 'AuthBackend' instance for your monad
2. A runner function: @forall a. m a -> IO a@

The 'authBackendKnownCredentials' function additionally requires:
3. Known valid credentials (username, correct password, wrong password)

== Tested Laws

* **Determinism**: Same (username, password) pair always returns same Bool
* **Independence**: Validating user1 doesn't affect validating user2

== Implementation-Specific Tests

* **Accepts valid credentials**: Known user + correct password → True
* **Rejects invalid password**: Known user + wrong password → False
* **Rejects unknown user**: Nonexistent username → False

These laws ensure that any 'AuthBackend' implementation behaves correctly
and consistently, regardless of the underlying authentication backend.
-}
module Laws.AuthBackendSpec (
    authBackendLaws,
    authBackendKnownCredentials,
) where

import Data.Maybe (isJust)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.QuickCheck (arbitrary, forAllShrinkShow, ioProperty, property, (===))

-- Auth backend types and typeclass (also imports orphan Arbitrary instances)
import Servant.OAuth2.IDP.Auth.Backend (
    AuthBackend (..),
    PlaintextPassword,
    Username,
    mkUsername,
 )

{- | Polymorphic spec: tests the AuthBackend INTERFACE, not implementation.

This spec is polymorphic over the monad 'm', allowing it to test any
'AuthBackend' instance. The caller provides a runner function to
execute 'm' actions in 'IO'.

Example usage:

@
-- Test in-memory implementation
spec :: Spec
spec = authBackendLaws runInMemory

-- Test LDAP implementation
spec :: Spec
spec = authBackendLaws runLDAP
@

The runner function typically:
1. Sets up necessary environment (credential store, LDAP config, etc.)
2. Runs the 'm' action
3. Cleans up resources

For simple in-memory implementations, it might just unwrap a 'ReaderT':

@
runInMemory :: ReaderT CredentialStore IO a -> IO a
runInMemory action = do
    store <- createDemoCredentialStore
    runReaderT action store
@
-}
authBackendLaws ::
    forall m.
    ( AuthBackend m
    , Eq (AuthBackendUser m)
    , Show (AuthBackendUser m)
    ) =>
    -- | Runner function to execute 'm' in 'IO'
    (forall a. m a -> IO a) ->
    Spec
authBackendLaws runM = describe "AuthBackend laws" $ do
    it "determinism: same (username, password) always returns same Bool" $
        property $
            forAllShrinkShow arbitrary (const []) show $ \(user :: Username) ->
                forAllShrinkShow arbitrary (const []) (const "<password>") $ \(pass :: PlaintextPassword) ->
                    ioProperty $ do
                        result1 <- runM $ validateCredentials user pass
                        result2 <- runM $ validateCredentials user pass
                        pure $ result1 === result2

    it "independence: validating user1 doesn't affect validating user2" $
        property $
            forAllShrinkShow arbitrary (const []) show $ \(user1 :: Username) ->
                forAllShrinkShow arbitrary (const []) (const "<password>") $ \(pass1 :: PlaintextPassword) ->
                    forAllShrinkShow arbitrary (const []) show $ \(user2 :: Username) ->
                        forAllShrinkShow arbitrary (const []) (const "<password>") $ \(pass2 :: PlaintextPassword) ->
                            ioProperty $ do
                                -- Validate user1 first
                                _ <- runM $ validateCredentials user1 pass1
                                -- Then validate user2
                                result1 <- runM $ validateCredentials user2 pass2
                                -- Validate user2 again to ensure result is the same
                                result2 <- runM $ validateCredentials user2 pass2
                                pure $ result1 === result2

{- | Implementation-specific tests for known credentials.

These tests verify that a concrete 'AuthBackend' implementation correctly:

1. Accepts valid credentials (returns True)
2. Rejects invalid password for valid user (returns False)
3. Rejects unknown user (returns False)

The caller must provide:

* A known valid username
* The correct password for that username
* An incorrect password for that username

Example usage:

@
spec :: Spec
spec = do
    let validUser = Username "demo"
    let validPass = mkPlaintextPassword "demo123"
    let invalidPass = mkPlaintextPassword "wrong"
    authBackendKnownCredentials runAppM validUser validPass invalidPass
@

== Note on Unknown User Test

The test for unknown users uses a clearly invalid username
("nonexistent_user_12345") that is highly unlikely to exist in any
real credential store. This avoids false negatives while testing
the rejection behavior.
-}
authBackendKnownCredentials ::
    forall m.
    ( AuthBackend m
    , Eq (AuthBackendUser m)
    , Show (AuthBackendUser m)
    ) =>
    -- | Runner function to execute 'm' in 'IO'
    (forall a. m a -> IO a) ->
    -- | Known valid username
    Username ->
    -- | Correct password for the valid username
    PlaintextPassword ->
    -- | Incorrect password for the valid username
    PlaintextPassword ->
    Spec
authBackendKnownCredentials runM validUser validPass invalidPass = describe "Known credentials" $ do
    it "accepts valid credentials (returns Just user)" $ do
        result <- runM $ validateCredentials validUser validPass
        result `shouldSatisfy` isJust

    it "rejects invalid password for valid user (returns Nothing)" $ do
        result <- runM $ validateCredentials validUser invalidPass
        result `shouldBe` Nothing

    it "rejects unknown user (returns Nothing)" $ do
        -- Use a clearly invalid username that won't exist in any real store
        let unknownUser = case mkUsername "nonexistent_user_12345" of
                Just u -> u
                Nothing -> error "Failed to create test username (should never happen)"
        result <- runM $ validateCredentials unknownUser validPass
        result `shouldBe` Nothing
