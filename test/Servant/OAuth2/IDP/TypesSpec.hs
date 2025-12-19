{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Servant.OAuth2.IDP.TypesSpec
Description : Tests for OAuth type newtypes and validation
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC
-}
module Servant.OAuth2.IDP.TypesSpec (spec) where

import Data.Aeson (decode, encode)
import Data.Either (isLeft)
import Data.Maybe (isJust)
import Data.Set qualified as Set
import Data.Text (Text)
import Servant.OAuth2.IDP.Types (AccessToken (..), ClientName (..), ClientSecret (..), RefreshToken (..), Scope (..), Scopes (..), TokenType (..), mkClientName, mkClientSecret, mkRedirectUri, parseScopes, serializeScopeSet)
import Test.Hspec
import Web.HttpApiData (parseUrlPiece, toUrlPiece)

spec :: Spec
spec = do
    describe "mkRedirectUri" $ do
        context "FR-050: Exact hostname matching" $ do
            it "rejects substring localhost bypass in query param" $
                mkRedirectUri "http://evil.com/callback?localhost=bypass" `shouldBe` Nothing

            it "rejects substring localhost bypass in path" $
                mkRedirectUri "http://evil.com/localhost/callback" `shouldBe` Nothing

            it "rejects localhost as subdomain" $
                mkRedirectUri "http://localhost.evil.com/callback" `shouldBe` Nothing

        context "Allowlist acceptance" $ do
            it "accepts http://localhost:3000/callback" $
                mkRedirectUri "http://localhost:3000/callback" `shouldSatisfy` isJust

            it "accepts http://127.0.0.1:8080/callback" $
                mkRedirectUri "http://127.0.0.1:8080/callback" `shouldSatisfy` isJust

            it "accepts http://[::1]:3000/callback" $
                mkRedirectUri "http://[::1]:3000/callback" `shouldSatisfy` isJust

        context "HTTPS always accepted (non-private IPs)" $ do
            it "accepts https://example.com/callback" $
                mkRedirectUri "https://example.com/callback" `shouldSatisfy` isJust

        context "HTTP rejected for non-localhost" $ do
            it "rejects http://example.com/callback (non-localhost, non-HTTPS)" $
                mkRedirectUri "http://example.com/callback" `shouldBe` Nothing

        context "FR-051: Private IP range blocking" $ do
            it "rejects https://10.0.0.1/callback (Class A private)" $
                mkRedirectUri "https://10.0.0.1/callback" `shouldBe` Nothing

            it "rejects https://10.255.255.255/callback (Class A private boundary)" $
                mkRedirectUri "https://10.255.255.255/callback" `shouldBe` Nothing

            it "rejects https://172.16.0.1/callback (Class B private start)" $
                mkRedirectUri "https://172.16.0.1/callback" `shouldBe` Nothing

            it "rejects https://172.31.255.255/callback (Class B private end)" $
                mkRedirectUri "https://172.31.255.255/callback" `shouldBe` Nothing

            it "rejects https://192.168.1.1/callback (Class C private)" $
                mkRedirectUri "https://192.168.1.1/callback" `shouldBe` Nothing

            it "rejects https://169.254.169.254/latest/meta-data (cloud metadata SSRF)" $
                mkRedirectUri "https://169.254.169.254/latest/meta-data" `shouldBe` Nothing

            it "accepts https://example.com/callback (public domain)" $
                mkRedirectUri "https://example.com/callback" `shouldSatisfy` isJust

            it "accepts https://8.8.8.8/callback (public IP)" $
                mkRedirectUri "https://8.8.8.8/callback" `shouldSatisfy` isJust

            it "accepts http://localhost:3000/callback (localhost still allowed)" $
                mkRedirectUri "http://localhost:3000/callback" `shouldSatisfy` isJust

        context "FR-051: Security bypass vectors" $ do
            it "rejects overflow bypass 172.288.0.1" $
                mkRedirectUri "https://172.288.0.1/callback" `shouldBe` Nothing

            it "rejects decimal IP notation 167772161 (10.0.0.1)" $
                mkRedirectUri "https://167772161/callback" `shouldBe` Nothing

            it "rejects hex IP notation 0xa000001" $
                mkRedirectUri "https://0xa000001/callback" `shouldBe` Nothing

            it "rejects octal IP notation 012.0.0.1" $
                mkRedirectUri "https://012.0.0.1/callback" `shouldBe` Nothing

            -- Boundary tests for public IPs (should ACCEPT)
            it "accepts public IP 172.32.0.0 (just outside Class B private)" $
                mkRedirectUri "https://172.32.0.0/callback" `shouldSatisfy` isJust

            it "accepts public IP 172.15.255.255 (just below Class B private)" $
                mkRedirectUri "https://172.15.255.255/callback" `shouldSatisfy` isJust

        context "FR-051: IPv6 private range blocking" $ do
            it "rejects fe80::1 (IPv6 link-local)" $
                mkRedirectUri "https://[fe80::1]/callback" `shouldBe` Nothing

            it "rejects fd00::1 (IPv6 unique local)" $
                mkRedirectUri "https://[fd00::1]/callback" `shouldBe` Nothing

            it "rejects fc00::1 (IPv6 unique local alternative)" $
                mkRedirectUri "https://[fc00::1]/callback" `shouldBe` Nothing

    describe "FR-060: Scope parsing and serialization" $ do
        context "Scope newtype single value" $ do
            it "accepts valid single scope via FromHttpApiData" $
                parseUrlPiece "openid" `shouldBe` Right (Scope "openid")

            it "rejects empty scope" $
                (parseUrlPiece "" :: Either Text Scope) `shouldSatisfy` isLeft

            it "rejects scope with whitespace" $
                (parseUrlPiece "open id" :: Either Text Scope) `shouldSatisfy` isLeft

            it "round-trips through ToHttpApiData" $
                let scope = Scope "profile"
                 in parseUrlPiece (toUrlPiece scope) `shouldBe` Right scope

        context "Space-delimited scope list parsing (RFC 6749 Section 3.3)" $ do
            it "parses space-delimited scopes into Set" $
                parseScopes "openid profile email"
                    `shouldBe` Just (Set.fromList [Scope "openid", Scope "profile", Scope "email"])

            it "handles single scope" $
                parseScopes "openid" `shouldBe` Just (Set.fromList [Scope "openid"])

            it "handles empty string" $
                parseScopes "" `shouldBe` Just Set.empty

            it "filters out empty scopes from consecutive spaces" $
                parseScopes "openid  profile" `shouldBe` Just (Set.fromList [Scope "openid", Scope "profile"])

            it "trims whitespace around scopes" $
                parseScopes "  openid   profile  " `shouldBe` Just (Set.fromList [Scope "openid", Scope "profile"])

        context "Set Scope serialization to space-delimited string" $ do
            it "serializes Set to space-delimited string" $
                let scopes = Set.fromList [Scope "email", Scope "openid", Scope "profile"]
                 in serializeScopeSet scopes `shouldSatisfy` (\s -> s `elem` ["email openid profile", "email profile openid", "openid email profile", "openid profile email", "profile email openid", "profile openid email"])

            it "handles empty Set" $
                serializeScopeSet Set.empty `shouldBe` ""

            it "handles single scope Set" $
                serializeScopeSet (Set.fromList [Scope "openid"]) `shouldBe` "openid"

        context "Scopes newtype for HTTP API (FR-060)" $ do
            it "parses space-delimited scopes via FromHttpApiData" $
                parseUrlPiece "openid profile"
                    `shouldBe` Right (Scopes (Set.fromList [Scope "openid", Scope "profile"]))

            it "parses single scope" $
                parseUrlPiece "openid" `shouldBe` Right (Scopes (Set.fromList [Scope "openid"]))

            it "parses empty string to empty Set" $
                parseUrlPiece "" `shouldBe` Right (Scopes Set.empty)

            it "handles multiple spaces between scopes" $
                parseUrlPiece "openid  profile"
                    `shouldBe` Right (Scopes (Set.fromList [Scope "openid", Scope "profile"]))

            it "serializes to space-delimited via ToHttpApiData" $
                let scopeList = Scopes (Set.fromList [Scope "openid", Scope "profile"])
                 in toUrlPiece scopeList `shouldSatisfy` (\s -> s `elem` ["openid profile", "profile openid"])

            it "round-trips through FromHttpApiData and ToHttpApiData" $
                let original = Scopes (Set.fromList [Scope "email", Scope "openid"])
                    serialized = toUrlPiece original
                    parsed = parseUrlPiece serialized
                 in parsed `shouldBe` Right original

    describe "FR-062: ClientSecret newtype" $ do
        context "Smart constructor mkClientSecret" $ do
            it "accepts empty string for public clients" $
                mkClientSecret "" `shouldSatisfy` isJust

            it "accepts non-empty secret" $
                mkClientSecret "my-secret-123" `shouldSatisfy` isJust

            it "unwraps correctly" $
                case mkClientSecret "test-secret" of
                    Just (ClientSecret s) -> s `shouldBe` "test-secret"
                    Nothing -> expectationFailure "mkClientSecret should accept non-empty string"

        context "JSON serialization" $ do
            it "round-trips through JSON" $
                let secret = ClientSecret "secret-value"
                    encoded = encode secret
                    decoded = decode encoded
                 in decoded `shouldBe` Just secret

            it "serializes empty secret" $
                let secret = ClientSecret ""
                    encoded = encode secret
                    decoded = decode encoded
                 in decoded `shouldBe` Just secret

    describe "FR-062: ClientName newtype" $ do
        context "Smart constructor mkClientName" $ do
            it "rejects empty string" $
                mkClientName "" `shouldBe` Nothing

            it "accepts non-empty name" $
                mkClientName "My Application" `shouldSatisfy` isJust

            it "unwraps correctly" $
                case mkClientName "Test App" of
                    Just (ClientName n) -> n `shouldBe` "Test App"
                    Nothing -> expectationFailure "mkClientName should accept non-empty string"

            it "accepts name with special characters" $
                mkClientName "My App (v1.0)" `shouldSatisfy` isJust

            it "accepts name with unicode" $
                mkClientName "My App ™" `shouldSatisfy` isJust

        context "JSON serialization" $ do
            it "round-trips through JSON" $
                let name = ClientName "My Application"
                    encoded = encode name
                    decoded = decode encoded
                 in decoded `shouldBe` Just name

    describe "FR-063: AccessToken newtype" $ do
        context "JSON serialization" $ do
            it "serializes to JSON as string" $
                let token = AccessToken "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.example"
                    encoded = encode token
                    decoded = decode encoded
                 in decoded `shouldBe` Just token

            it "unwraps to Text correctly" $
                let token = AccessToken "test-token-123"
                 in unAccessToken token `shouldBe` "test-token-123"

    describe "FR-063: TokenType newtype" $ do
        context "JSON serialization" $ do
            it "serializes Bearer token type to JSON" $
                let tokenType = TokenType "Bearer"
                    encoded = encode tokenType
                    decoded = decode encoded
                 in decoded `shouldBe` Just tokenType

            it "serializes arbitrary token type to JSON" $
                let tokenType = TokenType "Custom"
                    encoded = encode tokenType
                    decoded = decode encoded
                 in decoded `shouldBe` Just tokenType

            it "unwraps to Text correctly" $
                let tokenType = TokenType "Bearer"
                 in unTokenType tokenType `shouldBe` "Bearer"

    describe "FR-063: RefreshToken newtype" $ do
        context "JSON serialization" $ do
            it "serializes to JSON as string" $
                let token = RefreshToken "rt_a1b2c3d4e5f6"
                    encoded = encode token
                    decoded = decode encoded
                 in decoded `shouldBe` Just token

            it "unwraps to Text correctly" $
                let token = RefreshToken "rt_refresh_123"
                 in unRefreshToken token `shouldBe` "rt_refresh_123"
