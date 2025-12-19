{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Servant.OAuth2.IDP.CryptoEntropySpec
Description : Property tests for cryptographic RNG entropy (FR-055)
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC

Property tests verifying cryptographic entropy in token generation.

These tests guard against regressions where someone might accidentally
switch from CSPRNG (crypton) to weak RNG (System.Random).

== Coverage

- PKCE code verifier generation (32 bytes entropy)
- No collisions in large samples (1000 tokens)
- Sufficient entropy (minimum 43 characters for base64url of 32 bytes)
- No predictable patterns (character distribution)
-}
module Servant.OAuth2.IDP.CryptoEntropySpec (spec) where

import Control.Monad (replicateM)
import Data.Set qualified as Set
import Data.Text qualified as T
import Servant.OAuth2.IDP.Test.Internal (generateCodeVerifier, generatePKCE)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (ioProperty)
import Test.QuickCheck.Monadic (monadicIO, run)

spec :: Spec
spec = do
    describe "PKCE Code Verifier Generation (FR-055)" $ do
        prop "generates sufficient entropy - no collisions in 1000 tokens" $ \() -> ioProperty $ do
            tokens <- replicateM 1000 generateCodeVerifier
            let unique = Set.fromList tokens
            pure $ Set.size unique == 1000 -- No collisions
        it "tokens are 32+ bytes of entropy (43 chars base64url)" $ do
            verifier <- generateCodeVerifier
            -- Base64url encoding of 32 bytes = 43 characters (without padding)
            T.length verifier `shouldBe` 43

        it "tokens use valid base64url charset" $ do
            verifier <- generateCodeVerifier
            -- Base64url charset: A-Z, a-z, 0-9, -, _
            -- No padding (=) should be present in unpadded encoding
            T.all (\c -> c `elem` ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_" :: String)) verifier
                `shouldBe` True

        it "no predictable patterns - character distribution is varied" $ do
            verifier <- generateCodeVerifier
            -- Check that not all characters are the same (trivial pattern check)
            let chars = T.unpack verifier
                uniqueChars = Set.fromList chars
            Set.size uniqueChars `shouldSatisfy` (> 10) -- At least 10 different characters
        prop "different invocations produce different results" $ \() -> monadicIO $ do
            v1 <- run generateCodeVerifier
            v2 <- run generateCodeVerifier
            pure $ v1 /= v2

    describe "PKCE Pair Generation (FR-055)" $ do
        prop "generates unique verifier-challenge pairs" $ \() -> ioProperty $ do
            pairs <- replicateM 100 generatePKCE
            let verifiers = Set.fromList $ map fst pairs
                challenges = Set.fromList $ map snd pairs
            pure $ Set.size verifiers == 100 && Set.size challenges == 100

        it "different PKCE generations produce different pairs" $ do
            -- Test that two PKCE generations produce different pairs
            (v1, c1) <- generatePKCE
            (v2, c2) <- generatePKCE
            (v1 /= v2) `shouldBe` True
            (c1 /= c2) `shouldBe` True

        it "verifier and challenge are both non-empty" $ do
            (verifier, challenge) <- generatePKCE
            T.null verifier `shouldBe` False
            T.null challenge `shouldBe` False

        it "verifier is 43 chars and challenge is 43 chars (SHA256 base64url)" $ do
            (verifier, challenge) <- generatePKCE
            T.length verifier `shouldBe` 43
            T.length challenge `shouldBe` 43 -- SHA256 (32 bytes) -> 43 chars base64url
