{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Servant.OAuth2.IDP.APISpec
Description : Tests for OAuth API request/response types
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC
-}
module Servant.OAuth2.IDP.APISpec (spec) where

import Data.Aeson (decode, encode)
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Value (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (isJust, isNothing)
import Data.Set qualified as Set
import Servant.OAuth2.IDP.API (ClientRegistrationRequest (..), ClientRegistrationResponse (..), TokenResponse (..))
import Servant.OAuth2.IDP.Types (
    AccessToken (..),
    ClientAuthMethod (..),
    GrantType (..),
    RefreshToken (..),
    ResponseType (..),
    Scopes (..),
    TokenType (..),
    mkClientId,
    mkClientName,
    mkClientSecret,
    mkRedirectUri,
    mkScope,
 )
import Test.Hspec

-- Helper to unwrap Maybe in tests (partial function safe only for valid test data)
unsafeMk :: Maybe a -> a
unsafeMk (Just x) = x
unsafeMk Nothing = error "unsafeMk: test data construction failed - this should never happen with valid literals"

spec :: Spec
spec = do
    describe "FR-062: ClientRegistrationResponse with type-safe newtypes" $ do
        context "ToJSON instance unwraps newtypes correctly" $ do
            it "serializes client_id as unwrapped Text" $ do
                let clientId = unsafeMk $ mkClientId "client_abc123"
                    clientSecret = unsafeMk $ mkClientSecret ""
                    clientName = unsafeMk $ mkClientName "Test Client"
                    redirectUri = unsafeMk $ mkRedirectUri "https://example.com/callback"
                    response = ClientRegistrationResponse clientId clientSecret clientName (redirectUri :| []) (GrantAuthorizationCode :| []) (ResponseCode :| []) AuthNone
                    encoded = encode response
                    decoded = decode encoded :: Maybe Value

                case decoded of
                    Just (Object obj) -> do
                        KM.lookup "client_id" obj `shouldBe` Just (String "client_abc123")
                        KM.lookup "client_secret" obj `shouldBe` Just (String "")
                        KM.lookup "client_name" obj `shouldBe` Just (String "Test Client")
                    _ -> expectationFailure "Expected JSON object"

            it "serializes client_secret as unwrapped Text (empty for public clients)" $ do
                let clientId = unsafeMk $ mkClientId "client_public"
                    clientSecret = unsafeMk $ mkClientSecret "" -- Empty for public clients
                    clientName = unsafeMk $ mkClientName "Public Client"
                    redirectUri = unsafeMk $ mkRedirectUri "https://example.com/callback"
                    response = ClientRegistrationResponse clientId clientSecret clientName (redirectUri :| []) (GrantAuthorizationCode :| []) (ResponseCode :| []) AuthNone
                    encoded = encode response
                    decoded = decode encoded :: Maybe Value

                case decoded of
                    Just (Object obj) ->
                        KM.lookup "client_secret" obj `shouldBe` Just (String "")
                    _ -> expectationFailure "Expected JSON object"

            it "serializes client_name as unwrapped Text" $ do
                let clientId = unsafeMk $ mkClientId "client_xyz"
                    clientSecret = unsafeMk $ mkClientSecret "secret_confidential"
                    clientName = unsafeMk $ mkClientName "My Application"
                    redirectUri = unsafeMk $ mkRedirectUri "https://app.example.com/auth"
                    response = ClientRegistrationResponse clientId clientSecret clientName (redirectUri :| []) (GrantAuthorizationCode :| []) (ResponseCode :| []) AuthNone
                    encoded = encode response
                    decoded = decode encoded :: Maybe Value

                case decoded of
                    Just (Object obj) ->
                        KM.lookup "client_name" obj `shouldBe` Just (String "My Application")
                    _ -> expectationFailure "Expected JSON object"

    describe "FR-064: ClientRegistrationRequest with NonEmpty lists" $ do
        context "Valid JSON with non-empty lists" $ do
            it "parses valid registration request with single redirect_uri" $ do
                let json =
                        "{\
                        \  \"client_name\": \"Test Client\",\
                        \  \"redirect_uris\": [\"https://example.com/callback\"],\
                        \  \"grant_types\": [\"authorization_code\"],\
                        \  \"response_types\": [\"code\"],\
                        \  \"token_endpoint_auth_method\": \"none\"\
                        \}"
                    decoded = decode json :: Maybe ClientRegistrationRequest
                decoded `shouldSatisfy` isJust

            it "parses valid registration request with multiple redirect_uris" $ do
                let json =
                        "{\
                        \  \"client_name\": \"Test Client\",\
                        \  \"redirect_uris\": [\"https://example.com/callback1\", \"https://example.com/callback2\"],\
                        \  \"grant_types\": [\"authorization_code\", \"refresh_token\"],\
                        \  \"response_types\": [\"code\", \"token\"],\
                        \  \"token_endpoint_auth_method\": \"none\"\
                        \}"
                    decoded = decode json :: Maybe ClientRegistrationRequest
                decoded `shouldSatisfy` isJust

        context "Empty lists should fail to parse (NonEmpty enforcement)" $ do
            it "rejects empty redirect_uris array" $ do
                let json =
                        "{\
                        \  \"client_name\": \"Test Client\",\
                        \  \"redirect_uris\": [],\
                        \  \"grant_types\": [\"authorization_code\"],\
                        \  \"response_types\": [\"code\"],\
                        \  \"token_endpoint_auth_method\": \"none\"\
                        \}"
                    decoded = decode json :: Maybe ClientRegistrationRequest
                decoded `shouldSatisfy` isNothing

            it "rejects empty grant_types array" $ do
                let json =
                        "{\
                        \  \"client_name\": \"Test Client\",\
                        \  \"redirect_uris\": [\"https://example.com/callback\"],\
                        \  \"grant_types\": [],\
                        \  \"response_types\": [\"code\"],\
                        \  \"token_endpoint_auth_method\": \"none\"\
                        \}"
                    decoded = decode json :: Maybe ClientRegistrationRequest
                decoded `shouldSatisfy` isNothing

            it "rejects empty response_types array" $ do
                let json =
                        "{\
                        \  \"client_name\": \"Test Client\",\
                        \  \"redirect_uris\": [\"https://example.com/callback\"],\
                        \  \"grant_types\": [\"authorization_code\"],\
                        \  \"response_types\": [],\
                        \  \"token_endpoint_auth_method\": \"none\"\
                        \}"
                    decoded = decode json :: Maybe ClientRegistrationRequest
                decoded `shouldSatisfy` isNothing

    describe "FR-063: TokenResponse with type-safe newtypes" $ do
        context "ToJSON instance unwraps newtypes correctly" $ do
            it "serializes access_token as unwrapped Text" $ do
                let accessToken = AccessToken "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.test"
                    tokenType = TokenType "Bearer"
                    response = TokenResponse accessToken tokenType (Just 3600) Nothing Nothing
                    encoded = encode response
                    decoded = decode encoded :: Maybe Value

                case decoded of
                    Just (Object obj) -> do
                        KM.lookup "access_token" obj `shouldBe` Just (String "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.test")
                        KM.lookup "token_type" obj `shouldBe` Just (String "Bearer")
                    _ -> expectationFailure "Expected JSON object"

            it "serializes token_type as unwrapped Text" $ do
                let accessToken = AccessToken "test_token_abc123"
                    tokenType = TokenType "Bearer"
                    response = TokenResponse accessToken tokenType Nothing Nothing Nothing
                    encoded = encode response
                    decoded = decode encoded :: Maybe Value

                case decoded of
                    Just (Object obj) ->
                        KM.lookup "token_type" obj `shouldBe` Just (String "Bearer")
                    _ -> expectationFailure "Expected JSON object"

            it "serializes refresh_token as unwrapped Text when present" $ do
                let accessToken = AccessToken "access_xyz"
                    tokenType = TokenType "Bearer"
                    refreshToken = RefreshToken "rt_refresh_token_123"
                    response = TokenResponse accessToken tokenType (Just 3600) (Just refreshToken) Nothing
                    encoded = encode response
                    decoded = decode encoded :: Maybe Value

                case decoded of
                    Just (Object obj) ->
                        KM.lookup "refresh_token" obj `shouldBe` Just (String "rt_refresh_token_123")
                    _ -> expectationFailure "Expected JSON object"

            it "omits refresh_token field when Nothing" $ do
                let accessToken = AccessToken "access_only"
                    tokenType = TokenType "Bearer"
                    response = TokenResponse accessToken tokenType (Just 3600) Nothing Nothing
                    encoded = encode response
                    decoded = decode encoded :: Maybe Value

                case decoded of
                    Just (Object obj) ->
                        KM.lookup "refresh_token" obj `shouldBe` Nothing
                    _ -> expectationFailure "Expected JSON object"

            it "serializes scope as space-delimited string when present" $ do
                let accessToken = AccessToken "access_with_scope"
                    tokenType = TokenType "Bearer"
                    scope1 = unsafeMk $ mkScope "mcp:read"
                    scope2 = unsafeMk $ mkScope "mcp:write"
                    scopes = Set.fromList [scope1, scope2]
                    response = TokenResponse accessToken tokenType (Just 3600) Nothing (Just (Scopes scopes))
                    encoded = encode response
                    decoded = decode encoded :: Maybe Value

                case decoded of
                    Just (Object obj) -> do
                        -- Scope order is determined by Set's Ord instance
                        let scopeValue = KM.lookup "scope" obj
                        scopeValue
                            `shouldSatisfy` ( \case
                                                Just (String s) -> s == "mcp:read mcp:write" || s == "mcp:write mcp:read"
                                                _ -> False
                                            )
                    _ -> expectationFailure "Expected JSON object"
