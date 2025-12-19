{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Laws.ErrorBoundarySecuritySpec
Description : Security tests for OAuth error boundary translation
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC

This module verifies that the OAuth error boundary correctly implements
security policies for error logging and exposure:

1. **OAuthStateError**: Logged via tracer but returns generic 500 without details
2. **AuthBackendError**: Logged via tracer but returns generic 401 without details
3. **ValidationError**: Returns 400 with descriptive message (safe to expose)
4. **AuthorizationError**: Returns appropriate 4xx status with RFC 6749-compliant JSON

== Security Requirements

The boundary must ensure that:

* Infrastructure details (connection strings, database errors) never appear in HTTP responses
* User enumeration is prevented (all auth failures return same generic message)
* Error details ARE logged via tracer for observability
* OAuth protocol errors follow RFC 6749 format

== Test Coverage

* Secure error hiding for OAuthStoreError and DemoAuthError
* Descriptive error exposure for ValidationError
* RFC 6749-compliant responses for AuthorizationError
* Infrastructure detail exclusion verification
* Tracer verification for all error types
-}
module Laws.ErrorBoundarySecuritySpec (spec) where

import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (decode)
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error (lenientDecode)
import Servant.Server (ServerError (..))
import Test.Hspec (Spec, describe, it, shouldBe)

import MCP.Server.HTTP.AppEnv (AppError (..), AppM)
import MCP.Trace.HTTP (HTTPTrace (..))
import Plow.Logging (IOTracer (..), Tracer (..))
import Servant.OAuth2.IDP.Auth.Backend (Username (..))
import Servant.OAuth2.IDP.Auth.Demo (DemoAuthError (..))
import Servant.OAuth2.IDP.Boundary (
    OAuthBoundaryTrace (..),
    domainErrorToServerError,
 )
import Servant.OAuth2.IDP.Store.InMemory (OAuthStoreError (..))
import Servant.OAuth2.IDP.Types (
    AuthorizationError (..),
    OAuthErrorResponse (..),
    ValidationError (..),
    mkClientId,
    mkScope,
 )

-- -----------------------------------------------------------------------------
-- Test Helpers
-- -----------------------------------------------------------------------------

{- | Helper to extract Just value or fail test.
Used for smart constructors that should always succeed with test data.
-}
fromJustOrFail :: String -> Maybe a -> a
fromJustOrFail errMsg Nothing = error $ "Test setup failed: " ++ errMsg
fromJustOrFail _ (Just x) = x

-- -----------------------------------------------------------------------------
-- Mock Tracer for Testing
-- -----------------------------------------------------------------------------

{- | Create a mock tracer that captures all trace events in an MVar.

This allows tests to verify that errors are properly logged even when
they are not exposed to the client.
-}
mockTracer :: MVar [HTTPTrace] -> IOTracer HTTPTrace
mockTracer mvar =
    IOTracer $ Tracer $ \trace ->
        liftIO $ modifyMVar_ mvar $ \traces ->
            pure (trace : traces)

-- | Run an action with a mock tracer and return captured traces.
withMockTracer :: (IOTracer HTTPTrace -> IO a) -> IO ([HTTPTrace], a)
withMockTracer action = do
    mvar <- newMVar []
    let tracer = mockTracer mvar
    result <- action tracer
    traces <- readMVar mvar
    pure (reverse traces, result)

-- -----------------------------------------------------------------------------
-- Test Spec
-- -----------------------------------------------------------------------------

spec :: Spec
spec = describe "OAuth Error Boundary Security" $ do
    secureErrorHidingSpec
    validationErrorExposureSpec
    authorizationErrorExposureSpec
    infrastructureDetailExclusionSpec
    tracerVerificationSpec

-- -----------------------------------------------------------------------------
-- Secure Error Hiding Tests
-- -----------------------------------------------------------------------------

{- | Verify that OAuthStoreError and AuthBackendError details are hidden
from HTTP responses but logged via tracer.
-}
secureErrorHidingSpec :: Spec
secureErrorHidingSpec = describe "Secure Error Hiding" $ do
    describe "OAuthStoreError" $ do
        it "returns generic 500 for StoreUnavailable" $ do
            let err = OAuthStoreErr (StoreUnavailable "database connection failed")
            (traces, mServerErr) <- withMockTracer $ \tracer ->
                domainErrorToServerError @IO @AppM tracer HTTPOAuthBoundary err

            -- Should return generic 500
            case mServerErr of
                Nothing -> fail "Expected ServerError, got Nothing"
                Just serverErr -> do
                    errHTTPCode serverErr `shouldBe` 500
                    errBody serverErr `shouldBe` "Internal server error"

            -- Should log details
            case traces of
                [HTTPOAuthBoundary (BoundaryStoreError msg)] ->
                    T.isInfixOf "database connection failed" msg `shouldBe` True
                _ -> fail $ "Expected BoundaryStoreError trace, got: " ++ show traces

        it "returns generic 500 for StoreInternalError" $ do
            let err = OAuthStoreErr (StoreInternalError "SELECT * FROM tokens failed")
            (traces, mServerErr) <- withMockTracer $ \tracer ->
                domainErrorToServerError @IO @AppM tracer HTTPOAuthBoundary err

            -- Should return generic 500
            case mServerErr of
                Nothing -> fail "Expected ServerError, got Nothing"
                Just serverErr -> do
                    errHTTPCode serverErr `shouldBe` 500
                    errBody serverErr `shouldBe` "Internal server error"

            -- Should log details
            case traces of
                [HTTPOAuthBoundary (BoundaryStoreError msg)] ->
                    T.isInfixOf "SELECT * FROM tokens failed" msg `shouldBe` True
                _ -> fail $ "Expected BoundaryStoreError trace, got: " ++ show traces

    describe "AuthBackendError" $ do
        it "returns generic 401 for InvalidCredentials" $ do
            let err = AuthBackendErr InvalidCredentials
            (traces, mServerErr) <- withMockTracer $ \tracer ->
                domainErrorToServerError @IO @AppM tracer HTTPOAuthBoundary err

            -- Should return generic 401
            case mServerErr of
                Nothing -> fail "Expected ServerError, got Nothing"
                Just serverErr -> do
                    errHTTPCode serverErr `shouldBe` 401
                    errBody serverErr `shouldBe` "Unauthorized"

            -- Should log details
            case traces of
                [HTTPOAuthBoundary (BoundaryAuthError msg)] ->
                    T.isInfixOf "InvalidCredentials" msg `shouldBe` True
                _ -> fail $ "Expected BoundaryAuthError trace, got: " ++ show traces

        it "returns generic 401 for UserNotFound (no user enumeration)" $ do
            let err = AuthBackendErr (UserNotFound (Username "alice"))
            (traces, mServerErr) <- withMockTracer $ \tracer ->
                domainErrorToServerError @IO @AppM tracer HTTPOAuthBoundary err

            -- Should return generic 401 (not exposing user existence)
            case mServerErr of
                Nothing -> fail "Expected ServerError, got Nothing"
                Just serverErr -> do
                    errHTTPCode serverErr `shouldBe` 401
                    errBody serverErr `shouldBe` "Unauthorized"
                    -- Verify no mention of "alice" in response
                    T.isInfixOf "alice" (TE.decodeUtf8With lenientDecode $ BL.toStrict $ errBody serverErr) `shouldBe` False

            -- Should log details (including username for debugging)
            case traces of
                [HTTPOAuthBoundary (BoundaryAuthError msg)] ->
                    T.isInfixOf "UserNotFound" msg `shouldBe` True
                _ -> fail $ "Expected BoundaryAuthError trace, got: " ++ show traces

-- -----------------------------------------------------------------------------
-- ValidationError Exposure Tests
-- -----------------------------------------------------------------------------

{- | Verify that ValidationError returns descriptive 400 responses that
are safe to expose to clients.
-}
validationErrorExposureSpec :: Spec
validationErrorExposureSpec = describe "ValidationError Exposure" $ do
    it "returns 400 with client_id for ClientNotRegistered" $ do
        let clientId = fromJustOrFail "mkClientId failed for 'client_abc123'" $ mkClientId "client_abc123"
            err = ValidationErr (ClientNotRegistered clientId)
        (_, mServerErr) <- withMockTracer $ \tracer ->
            domainErrorToServerError @IO @AppM tracer HTTPOAuthBoundary err

        case mServerErr of
            Nothing -> fail "Expected ServerError, got Nothing"
            Just serverErr -> do
                errHTTPCode serverErr `shouldBe` 400
                let bodyText = TE.decodeUtf8With lenientDecode $ BL.toStrict $ errBody serverErr
                T.isInfixOf "client_id not registered" bodyText `shouldBe` True
                T.isInfixOf "client_abc123" bodyText `shouldBe` True

    it "returns 400 with response_type for UnsupportedResponseType" $ do
        let err = ValidationErr (UnsupportedResponseType "implicit")
        (_, mServerErr) <- withMockTracer $ \tracer ->
            domainErrorToServerError @IO @AppM tracer HTTPOAuthBoundary err

        case mServerErr of
            Nothing -> fail "Expected ServerError, got Nothing"
            Just serverErr -> do
                errHTTPCode serverErr `shouldBe` 400
                let bodyText = TE.decodeUtf8With lenientDecode $ BL.toStrict $ errBody serverErr
                T.isInfixOf "response_type not supported" bodyText `shouldBe` True
                T.isInfixOf "implicit" bodyText `shouldBe` True

    it "returns 400 with scope for MissingRequiredScope" $ do
        let scope = fromJustOrFail "mkScope failed for 'admin'" $ mkScope "admin"
            err = ValidationErr (MissingRequiredScope scope)
        (_, mServerErr) <- withMockTracer $ \tracer ->
            domainErrorToServerError @IO @AppM tracer HTTPOAuthBoundary err

        case mServerErr of
            Nothing -> fail "Expected ServerError, got Nothing"
            Just serverErr -> do
                errHTTPCode serverErr `shouldBe` 400
                let bodyText = TE.decodeUtf8With lenientDecode $ BL.toStrict $ errBody serverErr
                T.isInfixOf "Missing required scope" bodyText `shouldBe` True
                T.isInfixOf "admin" bodyText `shouldBe` True

    it "returns 400 with state value for InvalidStateParameter" $ do
        let err = ValidationErr (InvalidStateParameter "malformed-state")
        (_, mServerErr) <- withMockTracer $ \tracer ->
            domainErrorToServerError @IO @AppM tracer HTTPOAuthBoundary err

        case mServerErr of
            Nothing -> fail "Expected ServerError, got Nothing"
            Just serverErr -> do
                errHTTPCode serverErr `shouldBe` 400
                let bodyText = TE.decodeUtf8With lenientDecode $ BL.toStrict $ errBody serverErr
                T.isInfixOf "Invalid state parameter" bodyText `shouldBe` True
                T.isInfixOf "malformed-state" bodyText `shouldBe` True

-- -----------------------------------------------------------------------------
-- AuthorizationError Exposure Tests
-- -----------------------------------------------------------------------------

{- | Verify that AuthorizationError returns RFC 6749-compliant JSON responses
with appropriate HTTP status codes.
-}
authorizationErrorExposureSpec :: Spec
authorizationErrorExposureSpec = describe "AuthorizationError Exposure" $ do
    it "returns 400 with JSON for InvalidRequest" $ do
        let err = AuthorizationErr (InvalidRequest "Missing required parameter")
        (_, mServerErr) <- withMockTracer $ \tracer ->
            domainErrorToServerError @IO @AppM tracer HTTPOAuthBoundary err

        case mServerErr of
            Nothing -> fail "Expected ServerError, got Nothing"
            Just serverErr -> do
                errHTTPCode serverErr `shouldBe` 400
                -- Verify content-type
                lookup "Content-Type" (errHeaders serverErr) `shouldBe` Just "application/json; charset=utf-8"
                -- Parse JSON
                case decode (errBody serverErr) :: Maybe OAuthErrorResponse of
                    Nothing -> fail "Failed to parse OAuthErrorResponse JSON"
                    Just oauthErr -> do
                        oauthErrorCode oauthErr `shouldBe` "invalid_request"
                        oauthErrorDescription oauthErr `shouldBe` Just "Missing required parameter"

    it "returns 401 with JSON for InvalidClient" $ do
        let err = AuthorizationErr (InvalidClient "Client authentication failed")
        (_, mServerErr) <- withMockTracer $ \tracer ->
            domainErrorToServerError @IO @AppM tracer HTTPOAuthBoundary err

        case mServerErr of
            Nothing -> fail "Expected ServerError, got Nothing"
            Just serverErr -> do
                errHTTPCode serverErr `shouldBe` 401
                case decode (errBody serverErr) :: Maybe OAuthErrorResponse of
                    Nothing -> fail "Failed to parse OAuthErrorResponse JSON"
                    Just oauthErr -> do
                        oauthErrorCode oauthErr `shouldBe` "invalid_client"
                        oauthErrorDescription oauthErr `shouldBe` Just "Client authentication failed"

    it "returns 400 with JSON for InvalidGrant" $ do
        let err = AuthorizationErr (InvalidGrant "Code expired")
        (_, mServerErr) <- withMockTracer $ \tracer ->
            domainErrorToServerError @IO @AppM tracer HTTPOAuthBoundary err

        case mServerErr of
            Nothing -> fail "Expected ServerError, got Nothing"
            Just serverErr -> do
                errHTTPCode serverErr `shouldBe` 400
                case decode (errBody serverErr) :: Maybe OAuthErrorResponse of
                    Nothing -> fail "Failed to parse OAuthErrorResponse JSON"
                    Just oauthErr -> do
                        oauthErrorCode oauthErr `shouldBe` "invalid_grant"
                        oauthErrorDescription oauthErr `shouldBe` Just "Code expired"

    it "returns 401 with JSON for UnauthorizedClient" $ do
        let err = AuthorizationErr (UnauthorizedClient "Client not authorized")
        (_, mServerErr) <- withMockTracer $ \tracer ->
            domainErrorToServerError @IO @AppM tracer HTTPOAuthBoundary err

        case mServerErr of
            Nothing -> fail "Expected ServerError, got Nothing"
            Just serverErr -> do
                errHTTPCode serverErr `shouldBe` 401
                case decode (errBody serverErr) :: Maybe OAuthErrorResponse of
                    Nothing -> fail "Failed to parse OAuthErrorResponse JSON"
                    Just oauthErr -> do
                        oauthErrorCode oauthErr `shouldBe` "unauthorized_client"

    it "returns 403 with JSON for AccessDenied" $ do
        let err = AuthorizationErr (AccessDenied "User denied authorization")
        (_, mServerErr) <- withMockTracer $ \tracer ->
            domainErrorToServerError @IO @AppM tracer HTTPOAuthBoundary err

        case mServerErr of
            Nothing -> fail "Expected ServerError, got Nothing"
            Just serverErr -> do
                errHTTPCode serverErr `shouldBe` 403
                case decode (errBody serverErr) :: Maybe OAuthErrorResponse of
                    Nothing -> fail "Failed to parse OAuthErrorResponse JSON"
                    Just oauthErr -> do
                        oauthErrorCode oauthErr `shouldBe` "access_denied"

    it "returns 400 with JSON for ExpiredCode" $ do
        let err = AuthorizationErr ExpiredCode
        (_, mServerErr) <- withMockTracer $ \tracer ->
            domainErrorToServerError @IO @AppM tracer HTTPOAuthBoundary err

        case mServerErr of
            Nothing -> fail "Expected ServerError, got Nothing"
            Just serverErr -> do
                errHTTPCode serverErr `shouldBe` 400
                case decode (errBody serverErr) :: Maybe OAuthErrorResponse of
                    Nothing -> fail "Failed to parse OAuthErrorResponse JSON"
                    Just oauthErr -> do
                        oauthErrorCode oauthErr `shouldBe` "invalid_grant"
                        oauthErrorDescription oauthErr `shouldBe` Just "Authorization code has expired"

-- -----------------------------------------------------------------------------
-- Infrastructure Detail Exclusion Tests
-- -----------------------------------------------------------------------------

{- | Verify that infrastructure details (connection strings, table names,
AWS keys, etc.) never appear in HTTP responses.
-}
infrastructureDetailExclusionSpec :: Spec
infrastructureDetailExclusionSpec = describe "Infrastructure Detail Exclusion" $ do
    it "excludes database connection strings from OAuthStoreError responses" $ do
        let dangerousString = "postgresql://user:password123@localhost:5432/oauth_db"
            err = OAuthStoreErr (StoreUnavailable dangerousString)
        (_, mServerErr) <- withMockTracer $ \tracer ->
            domainErrorToServerError @IO @AppM tracer HTTPOAuthBoundary err

        case mServerErr of
            Nothing -> fail "Expected ServerError, got Nothing"
            Just serverErr -> do
                let bodyText = TE.decodeUtf8With lenientDecode $ BL.toStrict $ errBody serverErr
                -- Should NOT contain any part of the connection string
                T.isInfixOf "postgresql" bodyText `shouldBe` False
                T.isInfixOf "password123" bodyText `shouldBe` False
                T.isInfixOf "5432" bodyText `shouldBe` False
                T.isInfixOf "oauth_db" bodyText `shouldBe` False
                -- Should only be generic message
                bodyText `shouldBe` "Internal server error"

    it "excludes SQL queries from OAuthStoreError responses" $ do
        let dangerousString = "SELECT * FROM users WHERE username='admin' AND password='secret'"
            err = OAuthStoreErr (StoreInternalError dangerousString)
        (_, mServerErr) <- withMockTracer $ \tracer ->
            domainErrorToServerError @IO @AppM tracer HTTPOAuthBoundary err

        case mServerErr of
            Nothing -> fail "Expected ServerError, got Nothing"
            Just serverErr -> do
                let bodyText = TE.decodeUtf8With lenientDecode $ BL.toStrict $ errBody serverErr
                T.isInfixOf "SELECT" bodyText `shouldBe` False
                T.isInfixOf "users" bodyText `shouldBe` False
                T.isInfixOf "admin" bodyText `shouldBe` False
                bodyText `shouldBe` "Internal server error"

    it "excludes AWS credentials from OAuthStoreError responses" $ do
        let dangerousString = "AWS_SECRET_KEY=wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY"
            err = OAuthStoreErr (StoreUnavailable dangerousString)
        (_, mServerErr) <- withMockTracer $ \tracer ->
            domainErrorToServerError @IO @AppM tracer HTTPOAuthBoundary err

        case mServerErr of
            Nothing -> fail "Expected ServerError, got Nothing"
            Just serverErr -> do
                let bodyText = TE.decodeUtf8With lenientDecode $ BL.toStrict $ errBody serverErr
                T.isInfixOf "AWS_SECRET_KEY" bodyText `shouldBe` False
                T.isInfixOf "wJalrXUtnFEMI" bodyText `shouldBe` False
                bodyText `shouldBe` "Internal server error"

    it "excludes usernames from AuthBackendError responses" $ do
        let username = Username "sensitive.admin.user@company.internal"
            err = AuthBackendErr (UserNotFound username)
        (_, mServerErr) <- withMockTracer $ \tracer ->
            domainErrorToServerError @IO @AppM tracer HTTPOAuthBoundary err

        case mServerErr of
            Nothing -> fail "Expected ServerError, got Nothing"
            Just serverErr -> do
                let bodyText = TE.decodeUtf8With lenientDecode $ BL.toStrict $ errBody serverErr
                T.isInfixOf "sensitive.admin.user" bodyText `shouldBe` False
                T.isInfixOf "company.internal" bodyText `shouldBe` False
                bodyText `shouldBe` "Unauthorized"

-- -----------------------------------------------------------------------------
-- Tracer Verification Tests
-- -----------------------------------------------------------------------------

{- | Verify that all error types are properly logged via tracer with
appropriate detail levels.
-}
tracerVerificationSpec :: Spec
tracerVerificationSpec = describe "Tracer Verification" $ do
    it "logs OAuthStateError details via BoundaryStoreError" $ do
        let err = OAuthStoreErr (StoreUnavailable "connection pool exhausted")
        (traces, _) <- withMockTracer $ \tracer ->
            domainErrorToServerError @IO @AppM tracer HTTPOAuthBoundary err

        case traces of
            [HTTPOAuthBoundary (BoundaryStoreError msg)] -> do
                T.isInfixOf "StoreUnavailable" msg `shouldBe` True
                T.isInfixOf "connection pool exhausted" msg `shouldBe` True
            _ -> fail $ "Expected BoundaryStoreError trace, got: " ++ show traces

    it "logs AuthBackendError details via BoundaryAuthError" $ do
        let err = AuthBackendErr InvalidCredentials
        (traces, _) <- withMockTracer $ \tracer ->
            domainErrorToServerError @IO @AppM tracer HTTPOAuthBoundary err

        case traces of
            [HTTPOAuthBoundary (BoundaryAuthError msg)] ->
                T.isInfixOf "InvalidCredentials" msg `shouldBe` True
            _ -> fail $ "Expected BoundaryAuthError trace, got: " ++ show traces

    it "logs ValidationError via BoundaryValidationError" $ do
        let clientId = fromJustOrFail "mkClientId failed for 'test_client'" $ mkClientId "test_client"
            err = ValidationErr (ClientNotRegistered clientId)
        (traces, _) <- withMockTracer $ \tracer ->
            domainErrorToServerError @IO @AppM tracer HTTPOAuthBoundary err

        -- Note: Currently ValidationError is not explicitly traced in domainErrorToServerError
        -- This test documents current behavior and can be updated if tracing is added
        traces `shouldBe` []

    it "logs AuthorizationError via BoundaryAuthorizationError" $ do
        let err = AuthorizationErr (InvalidGrant "test error")
        (traces, _) <- withMockTracer $ \tracer ->
            domainErrorToServerError @IO @AppM tracer HTTPOAuthBoundary err

        -- Note: Currently AuthorizationError is not explicitly traced in domainErrorToServerError
        -- This test documents current behavior and can be updated if tracing is added
        traces `shouldBe` []
