{-# LANGUAGE OverloadedStrings #-}

{- HLINT ignore "Avoid partial function" -}

{- |
Module      : Servant.OAuth2.IDP.TraceSpec
Description : Tests for OAuthTrace ADT with domain newtypes
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC

Test suite for OAuthTrace ADT in Servant.OAuth2.IDP.Trace.
Per TDD protocol: these tests are written FIRST and will fail until implementation exists.
-}
module Servant.OAuth2.IDP.TraceSpec (spec) where

import Data.Maybe (fromJust)
import Servant.OAuth2.IDP.Auth.Backend (Username, mkUsername)
import Servant.OAuth2.IDP.Errors (ValidationError (..))
import Servant.OAuth2.IDP.Trace
import Servant.OAuth2.IDP.Types (
    ClientId,
    OAuthGrantType (..),
    RedirectUri,
    Scope,
    SessionId,
    mkClientId,
    mkRedirectUri,
    mkScope,
    mkSessionId,
 )
import Test.Hspec

-- Test fixtures
testClientId :: ClientId
testClientId = fromJust $ mkClientId "test_client_123"

testRedirectUri :: RedirectUri
testRedirectUri = case mkRedirectUri "https://example.com/callback" of
    Just uri -> uri
    Nothing -> error "Test fixture: invalid redirect URI"

testScope :: Scope
testScope = case mkScope "read" of
    Just s -> s
    Nothing -> error "Test fixture: invalid scope"

testSessionId :: SessionId
testSessionId = fromJust $ mkSessionId "12345678-1234-1234-1234-123456789abc"

testUsername :: Username
testUsername = case mkUsername "testuser" of
    Just u -> u
    Nothing -> error "Test fixture: invalid username"

spec :: Spec
spec = do
    describe "OperationResult" $ do
        it "Success constructor exists and has Show instance" $ do
            show Success `shouldBe` "Success"

        it "Failure constructor exists and has Show instance" $ do
            show Failure `shouldBe` "Failure"

        it "has Eq instance for Success" $ do
            Success `shouldBe` Success

        it "has Eq instance for Failure" $ do
            Failure `shouldBe` Failure

        it "Success and Failure are not equal" $ do
            Success `shouldNotBe` Failure

    describe "DenialReason" $ do
        it "UserDenied constructor exists" $ do
            show UserDenied `shouldContain` "UserDenied"

        it "InvalidRequest constructor exists" $ do
            show InvalidRequest `shouldContain` "InvalidRequest"

        it "UnauthorizedClient constructor exists" $ do
            show UnauthorizedClient `shouldContain` "UnauthorizedClient"

        it "ServerError constructor exists with Text parameter" $ do
            show (ServerError "test error") `shouldContain` "ServerError"

        it "has Eq instance" $ do
            UserDenied `shouldBe` UserDenied
            InvalidRequest `shouldBe` InvalidRequest
            UnauthorizedClient `shouldBe` UnauthorizedClient
            ServerError "err" `shouldBe` ServerError "err"

    describe "OAuthTrace constructors with domain types" $ do
        it "TraceClientRegistration uses ClientId and RedirectUri" $ do
            let trace = TraceClientRegistration testClientId testRedirectUri
            show trace `shouldContain` "TraceClientRegistration"

        it "TraceAuthorizationRequest uses ClientId, [Scope], OperationResult" $ do
            let trace = TraceAuthorizationRequest testClientId [testScope] Success
            show trace `shouldContain` "TraceAuthorizationRequest"

        it "TraceLoginPageServed uses SessionId" $ do
            let trace = TraceLoginPageServed testSessionId
            show trace `shouldContain` "TraceLoginPageServed"

        it "TraceLoginAttempt uses Username and OperationResult" $ do
            let trace = TraceLoginAttempt testUsername Success
            show trace `shouldContain` "TraceLoginAttempt"

        it "TracePKCEValidation uses OperationResult" $ do
            let trace = TracePKCEValidation Success
            show trace `shouldContain` "TracePKCEValidation"

        it "TraceAuthorizationGranted uses ClientId and Username" $ do
            let trace = TraceAuthorizationGranted testClientId testUsername
            show trace `shouldContain` "TraceAuthorizationGranted"

        it "TraceAuthorizationDenied uses ClientId and DenialReason" $ do
            let trace = TraceAuthorizationDenied testClientId UserDenied
            show trace `shouldContain` "TraceAuthorizationDenied"

        it "TraceTokenExchange uses OAuthGrantType and OperationResult" $ do
            let trace = TraceTokenExchange OAuthAuthorizationCode Success
            show trace `shouldContain` "TraceTokenExchange"

        it "TraceTokenRefresh uses OperationResult" $ do
            let trace = TraceTokenRefresh Success
            show trace `shouldContain` "TraceTokenRefresh"

        it "TraceSessionExpired uses SessionId" $ do
            let trace = TraceSessionExpired testSessionId
            show trace `shouldContain` "TraceSessionExpired"

        it "TraceValidationError uses ValidationError" $ do
            let trace = TraceValidationError (ClientNotRegistered testClientId)
            show trace `shouldContain` "TraceValidationError"

    describe "OAuthTrace Eq instance" $ do
        it "equal traces compare equal" $ do
            let trace1 = TracePKCEValidation Success
                trace2 = TracePKCEValidation Success
            trace1 `shouldBe` trace2

        it "different traces compare unequal" $ do
            let trace1 = TracePKCEValidation Success
                trace2 = TracePKCEValidation Failure
            trace1 `shouldNotBe` trace2
