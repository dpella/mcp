{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{- HLINT ignore "Avoid partial function" -}

{- |
Module      : Servant.OAuth2.IDP.TokenRequestSpec
Description : Tests for OAuth token request types
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC
-}
module Servant.OAuth2.IDP.TokenRequestSpec (spec) where

import Data.Maybe (fromJust)
import Data.Text (Text)
import Test.Hspec
import Web.FormUrlEncoded (urlDecodeAsForm, urlEncodeAsForm)

import Servant.OAuth2.IDP.API (TokenRequest (..))
import Servant.OAuth2.IDP.Types (
    ResourceIndicator (..),
    mkAuthCodeId,
    mkCodeVerifier,
    mkRefreshTokenId,
 )

spec :: Spec
spec = do
    describe "TokenRequest FromForm" $ do
        describe "AuthorizationCodeGrant" $ do
            it "parses valid authorization_code grant form" $ do
                let formData :: [(Text, Text)]
                    formData =
                        [ ("grant_type", "authorization_code")
                        , ("code", "code_abc123")
                        , ("code_verifier", "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-._~")
                        ]
                let encoded = urlEncodeAsForm formData
                case urlDecodeAsForm encoded of
                    Left err -> expectationFailure $ "Failed to parse: " <> show err
                    Right (AuthorizationCodeGrant code verifier mResource) -> do
                        code `shouldBe` fromJust (mkAuthCodeId "code_abc123")
                        case mkCodeVerifier "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-._~" of
                            Just expected -> verifier `shouldBe` expected
                            Nothing -> expectationFailure "Invalid test CodeVerifier"
                        mResource `shouldBe` Nothing
                    Right other -> expectationFailure $ "Expected AuthorizationCodeGrant, got: " <> show other

            it "parses authorization_code grant with resource parameter" $ do
                let formData :: [(Text, Text)]
                    formData =
                        [ ("grant_type", "authorization_code")
                        , ("code", "code_xyz789")
                        , ("code_verifier", "1234567890abcdefghijklmnopqrstuvwxyz-._~ABCDEFGHIJKLMNOPQRSTUVWXYZ")
                        , ("resource", "https://api.example.com")
                        ]
                let encoded = urlEncodeAsForm formData
                case urlDecodeAsForm encoded of
                    Left err -> expectationFailure $ "Failed to parse: " <> show err
                    Right (AuthorizationCodeGrant code verifier mResource) -> do
                        code `shouldBe` fromJust (mkAuthCodeId "code_xyz789")
                        case mkCodeVerifier "1234567890abcdefghijklmnopqrstuvwxyz-._~ABCDEFGHIJKLMNOPQRSTUVWXYZ" of
                            Just expected -> verifier `shouldBe` expected
                            Nothing -> expectationFailure "Invalid test CodeVerifier"
                        mResource `shouldBe` Just (ResourceIndicator "https://api.example.com")
                    Right other -> expectationFailure $ "Expected AuthorizationCodeGrant, got: " <> show other

            it "fails when code is missing" $ do
                let formData :: [(Text, Text)]
                    formData =
                        [ ("grant_type", "authorization_code")
                        , ("code_verifier", "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-._~")
                        ]
                let encoded = urlEncodeAsForm formData
                case urlDecodeAsForm encoded :: Either Text TokenRequest of
                    Left _err -> pure () -- Expected failure
                    Right _ -> expectationFailure "Should have failed with missing code"

            it "fails when code_verifier is missing" $ do
                let formData :: [(Text, Text)]
                    formData =
                        [ ("grant_type", "authorization_code")
                        , ("code", "code_abc123")
                        ]
                let encoded = urlEncodeAsForm formData
                case urlDecodeAsForm encoded :: Either Text TokenRequest of
                    Left _err -> pure () -- Expected failure
                    Right _ -> expectationFailure "Should have failed with missing code_verifier"

            it "fails when code_verifier is too short" $ do
                let formData :: [(Text, Text)]
                    formData =
                        [ ("grant_type", "authorization_code")
                        , ("code", "code_abc123")
                        , ("code_verifier", "tooshort")
                        ]
                let encoded = urlEncodeAsForm formData
                case urlDecodeAsForm encoded :: Either Text TokenRequest of
                    Left _err -> pure () -- Expected failure
                    Right _ -> expectationFailure "Should have failed with invalid code_verifier"

        describe "RefreshTokenGrant" $ do
            it "parses valid refresh_token grant form" $ do
                let formData :: [(Text, Text)]
                    formData =
                        [ ("grant_type", "refresh_token")
                        , ("refresh_token", "rt_refresh123")
                        ]
                let encoded = urlEncodeAsForm formData
                case urlDecodeAsForm encoded of
                    Left err -> expectationFailure $ "Failed to parse: " <> show err
                    Right (RefreshTokenGrant refreshToken mResource) -> do
                        refreshToken `shouldBe` fromJust (mkRefreshTokenId "rt_refresh123")
                        mResource `shouldBe` Nothing
                    Right other -> expectationFailure $ "Expected RefreshTokenGrant, got: " <> show other

            it "parses refresh_token grant with resource parameter" $ do
                let formData :: [(Text, Text)]
                    formData =
                        [ ("grant_type", "refresh_token")
                        , ("refresh_token", "rt_refresh456")
                        , ("resource", "https://api.example.com")
                        ]
                let encoded = urlEncodeAsForm formData
                case urlDecodeAsForm encoded of
                    Left err -> expectationFailure $ "Failed to parse: " <> show err
                    Right (RefreshTokenGrant refreshToken mResource) -> do
                        refreshToken `shouldBe` fromJust (mkRefreshTokenId "rt_refresh456")
                        mResource `shouldBe` Just (ResourceIndicator "https://api.example.com")
                    Right other -> expectationFailure $ "Expected RefreshTokenGrant, got: " <> show other

            it "fails when refresh_token is missing" $ do
                let formData :: [(Text, Text)]
                    formData = [("grant_type", "refresh_token")]
                let encoded = urlEncodeAsForm formData
                case urlDecodeAsForm encoded :: Either Text TokenRequest of
                    Left _err -> pure () -- Expected failure
                    Right _ -> expectationFailure "Should have failed with missing refresh_token"

        describe "Invalid grant types" $ do
            it "fails with unsupported grant_type" $ do
                let formData :: [(Text, Text)]
                    formData =
                        [ ("grant_type", "client_credentials")
                        , ("client_id", "test_client")
                        ]
                let encoded = urlEncodeAsForm formData
                case urlDecodeAsForm encoded :: Either Text TokenRequest of
                    Left _err -> pure () -- Expected failure
                    Right _ -> expectationFailure "Should have failed with unsupported grant_type"

            it "fails with missing grant_type" $ do
                let formData :: [(Text, Text)]
                    formData = [("code", "code_abc123")]
                let encoded = urlEncodeAsForm formData
                case urlDecodeAsForm encoded :: Either Text TokenRequest of
                    Left _err -> pure () -- Expected failure
                    Right _ -> expectationFailure "Should have failed with missing grant_type"

            it "fails with invalid grant_type" $ do
                let formData :: [(Text, Text)]
                    formData = [("grant_type", "invalid_grant")]
                let encoded = urlEncodeAsForm formData
                case urlDecodeAsForm encoded :: Either Text TokenRequest of
                    Left _err -> pure () -- Expected failure
                    Right _ -> expectationFailure "Should have failed with invalid grant_type"
