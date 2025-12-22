{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

1. **OAuthStateError**: Returns generic 500 without details
2. **AuthBackendError**: Returns generic 401 without details
3. **ValidationError**: Returns 400 with descriptive message (safe to expose)
4. **AuthorizationError**: Returns appropriate 4xx status with RFC 6749-compliant JSON

== Security Requirements

The boundary must ensure that:

* Infrastructure details (connection strings, database errors) never appear in HTTP responses
* User enumeration is prevented (all auth failures return same generic message)
* OAuth protocol errors follow RFC 6749 format

== Test Coverage

* Secure error hiding for OAuthStoreError and DemoAuthError
* Descriptive error exposure for ValidationError
* RFC 6749-compliant responses for AuthorizationError
* Infrastructure detail exclusion verification
-}
module Laws.ErrorBoundarySecuritySpec (spec) where

import Data.Aeson (decode)
import Data.ByteString.Lazy qualified as BL
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error (lenientDecode)
import MCP.Server.HTTP.AppEnv (AppError (..), appErrorToServerError)
import Servant.OAuth2.IDP.Auth.Backend (mkUsername)
import Servant.OAuth2.IDP.Auth.Demo (DemoAuthError (..))
import Servant.OAuth2.IDP.Errors (
    AccessDeniedReason (..),
    AuthorizationError (..),
    InvalidClientReason (..),
    InvalidGrantReason (..),
    InvalidRequestReason (..),
    MalformedReason (..),
    OAuthErrorCode (..),
    OAuthErrorResponse (..),
    UnauthorizedClientReason (..),
    ValidationError (..),
    oauthErrorCode,
    oauthErrorDescription,
 )
import Servant.OAuth2.IDP.Store.InMemory (OAuthStoreError (..))
import Servant.OAuth2.IDP.Types (
    mkAuthCodeId,
    mkClientId,
    mkScope,
 )
import Servant.Server (ServerError (..), errBody, errHTTPCode, errHeaders)
import Test.Hspec (Spec, describe, it, shouldBe)

-- | Helper to extract Just or fail test
fromJustOrFail :: String -> Maybe a -> a
fromJustOrFail msg Nothing = error msg
fromJustOrFail _ (Just x) = x

-- -----------------------------------------------------------------------------
-- Test Spec
-- -----------------------------------------------------------------------------

spec :: Spec
spec = describe "OAuth Error Boundary Security" $ do
    secureErrorHidingSpec
    validationErrorExposureSpec
    authorizationErrorExposureSpec
    infrastructureDetailExclusionSpec

-- -----------------------------------------------------------------------------
-- Secure Error Hiding Tests
-- -----------------------------------------------------------------------------

{- | Verify that OAuthStoreError and AuthBackendError details are hidden
from HTTP responses.
-}
secureErrorHidingSpec :: Spec
secureErrorHidingSpec = describe "Secure Error Hiding" $ do
    describe "OAuthStoreError" $ do
        it "returns generic 500 for StoreUnavailable" $ do
            let err = OAuthStoreErr (StoreUnavailable "database connection failed")
                serverErr = appErrorToServerError err

            errHTTPCode serverErr `shouldBe` 500
            -- Should NOT leak backend details
            let bodyText = TE.decodeUtf8With lenientDecode $ BL.toStrict $ errBody serverErr
            T.isInfixOf "database connection failed" bodyText `shouldBe` False
            T.isInfixOf "Internal Server Error" bodyText `shouldBe` True

        it "returns generic 500 for StoreInternalError" $ do
            let err = OAuthStoreErr (StoreInternalError "SELECT * FROM tokens failed")
                serverErr = appErrorToServerError err

            errHTTPCode serverErr `shouldBe` 500
            -- Should NOT leak backend details
            let bodyText = TE.decodeUtf8With lenientDecode $ BL.toStrict $ errBody serverErr
            T.isInfixOf "SELECT * FROM tokens failed" bodyText `shouldBe` False
            T.isInfixOf "Internal Server Error" bodyText `shouldBe` True

    describe "AuthBackendError" $ do
        it "returns generic 401 for InvalidCredentials" $ do
            let err = AuthBackendErr InvalidCredentials
                serverErr = appErrorToServerError err

            errHTTPCode serverErr `shouldBe` 401
            errBody serverErr `shouldBe` "Unauthorized"

        it "returns generic 401 for UserNotFound (no user enumeration)" $ do
            let username = fromJustOrFail "mkUsername failed for 'alice'" $ mkUsername "alice"
                err = AuthBackendErr (UserNotFound username)
                serverErr = appErrorToServerError err

            errHTTPCode serverErr `shouldBe` 401
            errBody serverErr `shouldBe` "Unauthorized"
            -- Verify no mention of "alice" in response
            let bodyText = TE.decodeUtf8With lenientDecode $ BL.toStrict $ errBody serverErr
            T.isInfixOf "alice" bodyText `shouldBe` False

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
            serverErr = appErrorToServerError err

        errHTTPCode serverErr `shouldBe` 400
        let bodyText = TE.decodeUtf8With lenientDecode $ BL.toStrict $ errBody serverErr
        T.isInfixOf "client_id not registered" bodyText `shouldBe` True
        T.isInfixOf "client_abc123" bodyText `shouldBe` True

    it "returns 400 with response_type for UnsupportedResponseType" $ do
        let err = ValidationErr (UnsupportedResponseType "implicit")
            serverErr = appErrorToServerError err

        errHTTPCode serverErr `shouldBe` 400
        let bodyText = TE.decodeUtf8With lenientDecode $ BL.toStrict $ errBody serverErr
        T.isInfixOf "response_type not supported" bodyText `shouldBe` True
        T.isInfixOf "implicit" bodyText `shouldBe` True

    it "returns 400 with scope for MissingRequiredScope" $ do
        let scope = fromJustOrFail "mkScope failed for 'admin'" $ mkScope "admin"
            err = ValidationErr (MissingRequiredScope scope)
            serverErr = appErrorToServerError err

        errHTTPCode serverErr `shouldBe` 400
        let bodyText = TE.decodeUtf8With lenientDecode $ BL.toStrict $ errBody serverErr
        T.isInfixOf "Missing required scope" bodyText `shouldBe` True
        T.isInfixOf "admin" bodyText `shouldBe` True

    it "returns 400 with state value for InvalidStateParameter" $ do
        let err = ValidationErr (InvalidStateParameter "malformed-state")
            serverErr = appErrorToServerError err

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
        let err = AuthorizationErr (InvalidRequest (MalformedRequest (UnparseableBody "test error")))
            serverErr = appErrorToServerError err

        errHTTPCode serverErr `shouldBe` 400
        -- Verify content-type
        lookup "Content-Type" (errHeaders serverErr) `shouldBe` Just "application/json; charset=utf-8"
        -- Parse JSON
        case decode (errBody serverErr) :: Maybe OAuthErrorResponse of
            Nothing -> fail "Failed to parse OAuthErrorResponse JSON"
            Just oauthErr -> do
                oauthErrorCode oauthErr `shouldBe` ErrInvalidRequest
                oauthErrorDescription oauthErr `shouldBe` Just "Unparseable request body: test error"

    it "returns 401 with JSON for InvalidClient" $ do
        let err = AuthorizationErr (InvalidClient InvalidClientCredentials)
            serverErr = appErrorToServerError err

        errHTTPCode serverErr `shouldBe` 401
        case decode (errBody serverErr) :: Maybe OAuthErrorResponse of
            Nothing -> fail "Failed to parse OAuthErrorResponse JSON"
            Just oauthErr -> do
                oauthErrorCode oauthErr `shouldBe` ErrInvalidClient
                oauthErrorDescription oauthErr `shouldBe` Just "Invalid client credentials"

    it "returns 400 with JSON for InvalidGrant" $ do
        let codeId = fromJustOrFail "mkAuthCodeId failed" $ mkAuthCodeId "test_code_123"
            err = AuthorizationErr (InvalidGrant (CodeExpired codeId))
            serverErr = appErrorToServerError err

        errHTTPCode serverErr `shouldBe` 400
        case decode (errBody serverErr) :: Maybe OAuthErrorResponse of
            Nothing -> fail "Failed to parse OAuthErrorResponse JSON"
            Just oauthErr -> do
                oauthErrorCode oauthErr `shouldBe` ErrInvalidGrant
                T.isInfixOf "expired" (fromMaybe "" (oauthErrorDescription oauthErr)) `shouldBe` True

    it "returns 401 with JSON for UnauthorizedClient" $ do
        let scope = fromJustOrFail "mkScope failed" $ mkScope "admin"
            err = AuthorizationErr (UnauthorizedClient (ScopeNotAllowed scope))
            serverErr = appErrorToServerError err

        errHTTPCode serverErr `shouldBe` 401
        case decode (errBody serverErr) :: Maybe OAuthErrorResponse of
            Nothing -> fail "Failed to parse OAuthErrorResponse JSON"
            Just oauthErr -> do
                oauthErrorCode oauthErr `shouldBe` ErrUnauthorizedClient

    it "returns 403 with JSON for AccessDenied" $ do
        let err = AuthorizationErr (AccessDenied UserDenied)
            serverErr = appErrorToServerError err

        errHTTPCode serverErr `shouldBe` 403
        case decode (errBody serverErr) :: Maybe OAuthErrorResponse of
            Nothing -> fail "Failed to parse OAuthErrorResponse JSON"
            Just oauthErr -> do
                oauthErrorCode oauthErr `shouldBe` ErrAccessDenied

    it "returns 400 with JSON for ExpiredCode" $ do
        let err = AuthorizationErr ExpiredCode
            serverErr = appErrorToServerError err

        errHTTPCode serverErr `shouldBe` 400
        case decode (errBody serverErr) :: Maybe OAuthErrorResponse of
            Nothing -> fail "Failed to parse OAuthErrorResponse JSON"
            Just oauthErr -> do
                oauthErrorCode oauthErr `shouldBe` ErrInvalidGrant
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
            serverErr = appErrorToServerError err

        let bodyText = TE.decodeUtf8With lenientDecode $ BL.toStrict $ errBody serverErr
        -- Should NOT contain any part of the connection string
        T.isInfixOf "postgresql" bodyText `shouldBe` False
        T.isInfixOf "password123" bodyText `shouldBe` False
        T.isInfixOf "5432" bodyText `shouldBe` False
        T.isInfixOf "oauth_db" bodyText `shouldBe` False
        -- Should only be generic message
        T.isInfixOf "Internal Server Error" bodyText `shouldBe` True

    it "excludes SQL queries from OAuthStoreError responses" $ do
        let dangerousString = "SELECT * FROM users WHERE username='admin' AND password='secret'"
            err = OAuthStoreErr (StoreInternalError dangerousString)
            serverErr = appErrorToServerError err

        let bodyText = TE.decodeUtf8With lenientDecode $ BL.toStrict $ errBody serverErr
        T.isInfixOf "SELECT" bodyText `shouldBe` False
        T.isInfixOf "users" bodyText `shouldBe` False
        T.isInfixOf "admin" bodyText `shouldBe` False
        T.isInfixOf "Internal Server Error" bodyText `shouldBe` True

    it "excludes AWS credentials from OAuthStoreError responses" $ do
        let dangerousString = "AWS_SECRET_KEY=wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY"
            err = OAuthStoreErr (StoreUnavailable dangerousString)
            serverErr = appErrorToServerError err

        let bodyText = TE.decodeUtf8With lenientDecode $ BL.toStrict $ errBody serverErr
        T.isInfixOf "AWS_SECRET_KEY" bodyText `shouldBe` False
        T.isInfixOf "wJalrXUtnFEMI" bodyText `shouldBe` False
        T.isInfixOf "Internal Server Error" bodyText `shouldBe` True

    it "excludes usernames from AuthBackendError responses" $ do
        let username = fromJustOrFail "mkUsername failed" $ mkUsername "sensitive.admin.user@company.internal"
            err = AuthBackendErr (UserNotFound username)
            serverErr = appErrorToServerError err

        let bodyText = TE.decodeUtf8With lenientDecode $ BL.toStrict $ errBody serverErr
        T.isInfixOf "sensitive.admin.user" bodyText `shouldBe` False
        T.isInfixOf "company.internal" bodyText `shouldBe` False
        bodyText `shouldBe` "Unauthorized"
