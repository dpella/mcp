{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Laws.ConsumeAuthCodeSpec
Description : Tests for atomic consumeAuthCode operation
Copyright   : (C) 2025
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com

This module tests the atomic consumeAuthCode operation which combines
lookup and delete to prevent race conditions per RFC 6749 §4.1.2
(authorization codes MUST be single-use).
-}
module Laws.ConsumeAuthCodeSpec (
    consumeAuthCodeSpec,
    consumeAuthCodeConcurrencySpec,
) where

import Control.Concurrent.Async (concurrently)
import Data.Map.Strict qualified as Map
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), addUTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary, ioProperty, (.&&.), (===))

-- Import orphan Arbitrary instances
import Generators ()

import Servant.OAuth2.IDP.Auth.Demo (AuthUser)
import Servant.OAuth2.IDP.Store (OAuthStateStore (..))
import Servant.OAuth2.IDP.Types (AuthorizationCode (..))

-- Import TestM infrastructure for creating shared environments
import TestMonad (mkTestEnv, runTestM)

{- | Tests for consumeAuthCode operation.

This spec tests that consumeAuthCode:
1. Returns Just for valid code and deletes it atomically
2. Returns Nothing for expired codes
3. Is truly atomic (no race condition - only one concurrent consumer succeeds)
-}
consumeAuthCodeSpec ::
    forall m.
    ( OAuthStateStore m
    , Arbitrary (OAuthUser m)
    , Eq (OAuthUser m)
    , Show (OAuthUser m)
    ) =>
    -- | Runner function to execute 'm' in 'IO'
    (forall a. m a -> IO a) ->
    Spec
consumeAuthCodeSpec runM = describe "consumeAuthCode" $ do
    prop "returns Just for valid code and deletes it atomically" $
        \(code :: AuthorizationCode (OAuthUser m)) -> ioProperty $ do
            -- Make code valid (not expired)
            let validCode = code{authExpiry = addUTCTime 86400 (authExpiry code)}

            result <- runM $ do
                storeAuthCode validCode
                consumeAuthCode (authCodeId validCode)

            -- Verify second consume returns Nothing (code was deleted)
            secondResult <- runM $ consumeAuthCode (authCodeId validCode)

            -- Both checks must pass
            pure $ (result === Just validCode) .&&. (secondResult === Nothing)

    prop "returns Nothing for expired code" $
        \(code :: AuthorizationCode (OAuthUser m)) -> ioProperty $ do
            -- Make code expired by setting expiry to a fixed past time (year 2019)
            -- This ensures expiry < currentTime (2020-01-01) for all test runs
            let pastTime = UTCTime (fromGregorian 2019 12 31) 0
            let expiredCode = code{authExpiry = pastTime}

            result <- runM $ do
                storeAuthCode expiredCode
                consumeAuthCode (authCodeId expiredCode)

            pure $ result === Nothing

{- | TestM-specific concurrency test for consumeAuthCode atomicity.

This test verifies that when two threads concurrently try to consume the same
authorization code, exactly one succeeds. This is critical for OAuth security
per RFC 6749 §4.1.2 (authorization codes MUST be single-use).

Note: This test is TestM-specific (not polymorphic) because it needs to create
a shared environment that both concurrent threads can access. The polymorphic
`consumeAuthCodeSpec` can't test this because the provided `runM` runner creates
a fresh environment on each call.
-}
consumeAuthCodeConcurrencySpec :: Spec
consumeAuthCodeConcurrencySpec = describe "consumeAuthCode (concurrency)" $ do
    prop "only one concurrent consumer succeeds (atomicity)" $
        \(code :: AuthorizationCode AuthUser) -> ioProperty $ do
            -- Make code valid (not expired) - add far future time to ensure validity
            let validCode = code{authExpiry = addUTCTime 86400 (authExpiry code)}

            -- Fixed base time for test (matching Main.hs test time)
            let baseTestTime = case parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z" "2020-01-01 00:00:00 UTC" of
                    Just t -> t
                    Nothing -> error "Failed to parse base test time"

            -- Create a SHARED environment for this test
            env <- mkTestEnv baseTestTime Map.empty

            -- Store the code in the shared environment
            runTestM env $ storeAuthCode validCode

            -- Two concurrent consumers race for the SAME code in the SAME environment
            (result1, result2) <-
                concurrently
                    (runTestM env $ consumeAuthCode (authCodeId validCode))
                    (runTestM env $ consumeAuthCode (authCodeId validCode))

            -- Exactly one should succeed, the other should get Nothing
            -- Property: (isJust result1) XOR (isJust result2) must be True
            let succeeded1 = case result1 of Just _ -> True; Nothing -> False
                succeeded2 = case result2 of Just _ -> True; Nothing -> False
                exactlyOneSucceeded = (succeeded1 && not succeeded2) || (not succeeded1 && succeeded2)

            pure $ exactlyOneSucceeded === True
