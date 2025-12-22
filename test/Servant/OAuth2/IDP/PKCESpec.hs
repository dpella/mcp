{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Servant.OAuth2.IDP.PKCESpec
Description : Tests for PKCE (Proof Key for Code Exchange) functions
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC

Tests for PKCE implementation per RFC 7636.
These tests verify code verifier generation, challenge computation,
and validation logic using domain newtypes from Servant.OAuth2.IDP.Types.
-}
module Servant.OAuth2.IDP.PKCESpec (spec) where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Text qualified as T
import Servant.OAuth2.IDP.PKCE (generateCodeChallenge, generateCodeVerifier, validateCodeVerifier)
import Servant.OAuth2.IDP.Types (mkCodeChallenge, mkCodeVerifier, unCodeChallenge, unCodeVerifier)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (ioProperty)

spec :: Spec
spec = do
    describe "generateCodeVerifier" $ do
        it "generates a valid CodeVerifier (43-128 chars, unreserved charset)" $ do
            verifier <- generateCodeVerifier
            let text = unCodeVerifier verifier
                -- RFC 7636: unreserved = ALPHA / DIGIT / "-" / "." / "_" / "~"
                isUnreservedChar c =
                    isAsciiUpper c
                        || isAsciiLower c
                        || isDigit c
                        || c == '-'
                        || c == '.'
                        || c == '_'
                        || c == '~'
            T.length text `shouldSatisfy` (\len -> len >= 43 && len <= 128)
            T.all isUnreservedChar text `shouldBe` True

        it "generates different verifiers on each call" $ do
            v1 <- generateCodeVerifier
            v2 <- generateCodeVerifier
            v1 `shouldNotBe` v2

    describe "generateCodeChallenge" $ do
        it "produces a valid CodeChallenge from a CodeVerifier (base64url, 43-128 chars)" $ do
            verifier <- generateCodeVerifier
            let challenge = generateCodeChallenge verifier
                text = unCodeChallenge challenge
                isBase64UrlChar c =
                    isAsciiUpper c
                        || isAsciiLower c
                        || isDigit c
                        || c == '-'
                        || c == '_'
            T.length text `shouldSatisfy` (\len -> len >= 43 && len <= 128)
            T.all isBase64UrlChar text `shouldBe` True

        it "produces the same challenge for the same verifier (deterministic)" $ do
            verifier <- generateCodeVerifier
            let c1 = generateCodeChallenge verifier
            let c2 = generateCodeChallenge verifier
            c1 `shouldBe` c2

        it "produces different challenges for different verifiers" $ do
            v1 <- generateCodeVerifier
            v2 <- generateCodeVerifier
            let c1 = generateCodeChallenge v1
            let c2 = generateCodeChallenge v2
            c1 `shouldNotBe` c2

    describe "validateCodeVerifier" $ do
        it "accepts valid verifier-challenge pair (round-trip property)" $ do
            verifier <- generateCodeVerifier
            let challenge = generateCodeChallenge verifier
            validateCodeVerifier verifier challenge `shouldBe` True

        prop "round-trip property: generated challenges always validate" $ \() -> ioProperty $ do
            verifier <- generateCodeVerifier
            let challenge = generateCodeChallenge verifier
            pure $ validateCodeVerifier verifier challenge

        it "rejects mismatched verifier-challenge pair" $ do
            v1 <- generateCodeVerifier
            v2 <- generateCodeVerifier
            let c1 = generateCodeChallenge v1
            validateCodeVerifier v2 c1 `shouldBe` False

        it "rejects incorrect verifier with correct challenge" $ do
            correctVerifier <- generateCodeVerifier
            wrongVerifier <- generateCodeVerifier
            let challenge = generateCodeChallenge correctVerifier
            validateCodeVerifier wrongVerifier challenge `shouldBe` False

    describe "RFC 7636 test vectors" $ do
        it "validates RFC 7636 Appendix B example" $ do
            -- From RFC 7636 Appendix B:
            -- code_verifier = "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk"
            -- code_challenge = "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM"
            let verifierText = "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk"
            let challengeText = "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM"
            case (mkCodeVerifier verifierText, mkCodeChallenge challengeText) of
                (Just verifier, Just challenge) ->
                    validateCodeVerifier verifier challenge `shouldBe` True
                _ -> expectationFailure "RFC test vector should parse as valid CodeVerifier/CodeChallenge"
