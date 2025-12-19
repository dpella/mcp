{-# LANGUAGE OverloadedStrings #-}

module MCP.Server.OAuth.TypesSpec (spec) where

import Data.Text (Text)
import Data.Text qualified as T
import Network.HTTP.Types.Status (statusCode)
import Network.URI (parseURI)
import Servant.OAuth2.IDP.Types
import Test.Hspec

-- Helper functions to extract fields from OAuthErrorResponse
errorCode :: OAuthErrorResponse -> Text
errorCode = oauthErrorCode

errorDescription :: OAuthErrorResponse -> Maybe Text
errorDescription = oauthErrorDescription

spec :: Spec
spec = do
    describe "AuthorizationError" $ do
        describe "authorizationErrorToResponse" $ do
            it "InvalidRequest maps to 400 with invalid_request code" $ do
                let (status, resp) = authorizationErrorToResponse (InvalidRequest "Missing parameter")
                statusCode status `shouldBe` 400
                errorCode resp `shouldBe` "invalid_request"
                errorDescription resp `shouldBe` Just "Missing parameter"

            it "InvalidClient maps to 401 with invalid_client code" $ do
                let (status, resp) = authorizationErrorToResponse (InvalidClient "Client not found")
                statusCode status `shouldBe` 401
                errorCode resp `shouldBe` "invalid_client"
                errorDescription resp `shouldBe` Just "Client not found"

            it "InvalidGrant maps to 400 with invalid_grant code" $ do
                let (status, resp) = authorizationErrorToResponse (InvalidGrant "Code already used")
                statusCode status `shouldBe` 400
                errorCode resp `shouldBe` "invalid_grant"
                errorDescription resp `shouldBe` Just "Code already used"

            it "UnauthorizedClient maps to 401 with unauthorized_client code" $ do
                let (status, resp) = authorizationErrorToResponse (UnauthorizedClient "Not authorized")
                statusCode status `shouldBe` 401
                errorCode resp `shouldBe` "unauthorized_client"
                errorDescription resp `shouldBe` Just "Not authorized"

            it "UnsupportedGrantType maps to 400 with unsupported_grant_type code" $ do
                let (status, resp) = authorizationErrorToResponse (UnsupportedGrantType "Grant type not supported")
                statusCode status `shouldBe` 400
                errorCode resp `shouldBe` "unsupported_grant_type"
                errorDescription resp `shouldBe` Just "Grant type not supported"

            it "InvalidScope maps to 400 with invalid_scope code" $ do
                let (status, resp) = authorizationErrorToResponse (InvalidScope "Unknown scope")
                statusCode status `shouldBe` 400
                errorCode resp `shouldBe` "invalid_scope"
                errorDescription resp `shouldBe` Just "Unknown scope"

            it "AccessDenied maps to 403 with access_denied code" $ do
                let (status, resp) = authorizationErrorToResponse (AccessDenied "User denied")
                statusCode status `shouldBe` 403
                errorCode resp `shouldBe` "access_denied"
                errorDescription resp `shouldBe` Just "User denied"

            it "ExpiredCode maps to 400 with invalid_grant code" $ do
                let (status, resp) = authorizationErrorToResponse ExpiredCode
                statusCode status `shouldBe` 400
                errorCode resp `shouldBe` "invalid_grant"
                errorDescription resp `shouldBe` Just "Authorization code has expired"

            it "InvalidRedirectUri maps to 400 with invalid_request code" $ do
                let (status, resp) = authorizationErrorToResponse InvalidRedirectUri
                statusCode status `shouldBe` 400
                errorCode resp `shouldBe` "invalid_request"
                errorDescription resp `shouldBe` Just "Invalid redirect_uri"

            it "PKCEVerificationFailed maps to 400 with invalid_grant code" $ do
                let (status, resp) = authorizationErrorToResponse PKCEVerificationFailed
                statusCode status `shouldBe` 400
                errorCode resp `shouldBe` "invalid_grant"
                errorDescription resp `shouldBe` Just "PKCE verification failed"

        describe "AuthorizationError constructors" $ do
            it "InvalidRequest can be constructed and pattern matched" $ do
                let err = InvalidRequest "test"
                case err of
                    InvalidRequest msg -> msg `shouldBe` "test"
                    _ -> expectationFailure "Pattern match failed"

            it "InvalidClient can be constructed and pattern matched" $ do
                let err = InvalidClient "test"
                case err of
                    InvalidClient msg -> msg `shouldBe` "test"
                    _ -> expectationFailure "Pattern match failed"

            it "InvalidGrant can be constructed and pattern matched" $ do
                let err = InvalidGrant "test"
                case err of
                    InvalidGrant msg -> msg `shouldBe` "test"
                    _ -> expectationFailure "Pattern match failed"

            it "UnauthorizedClient can be constructed and pattern matched" $ do
                let err = UnauthorizedClient "test"
                case err of
                    UnauthorizedClient msg -> msg `shouldBe` "test"
                    _ -> expectationFailure "Pattern match failed"

            it "UnsupportedGrantType can be constructed and pattern matched" $ do
                let err = UnsupportedGrantType "test"
                case err of
                    UnsupportedGrantType msg -> msg `shouldBe` "test"
                    _ -> expectationFailure "Pattern match failed"

            it "InvalidScope can be constructed and pattern matched" $ do
                let err = InvalidScope "test"
                case err of
                    InvalidScope msg -> msg `shouldBe` "test"
                    _ -> expectationFailure "Pattern match failed"

            it "AccessDenied can be constructed and pattern matched" $ do
                let err = AccessDenied "test"
                case err of
                    AccessDenied msg -> msg `shouldBe` "test"
                    _ -> expectationFailure "Pattern match failed"

            it "ExpiredCode can be constructed and pattern matched" $ do
                let err = ExpiredCode
                case err of
                    ExpiredCode -> pure ()
                    _ -> expectationFailure "Pattern match failed"

            it "InvalidRedirectUri can be constructed and pattern matched" $ do
                let err = InvalidRedirectUri
                case err of
                    InvalidRedirectUri -> pure ()
                    _ -> expectationFailure "Pattern match failed"

            it "PKCEVerificationFailed can be constructed and pattern matched" $ do
                let err = PKCEVerificationFailed
                case err of
                    PKCEVerificationFailed -> pure ()
                    _ -> expectationFailure "Pattern match failed"

    describe "ValidationError" $ do
        describe "validationErrorToResponse" $ do
            it "RedirectUriMismatch returns descriptive 400 error" $ do
                let clientId = ClientId "client123"
                    uri = case parseURI "https://example.com/callback" of
                        Just u -> u
                        Nothing -> error "Test URI parsing failed"
                    redirectUri = RedirectUri uri
                    err = RedirectUriMismatch clientId redirectUri
                    (status, message) = validationErrorToResponse err
                statusCode status `shouldBe` 400
                T.unpack message `shouldContain` "redirect_uri"
                T.unpack message `shouldContain` "client123"

            it "UnsupportedResponseType returns descriptive 400 error" $ do
                let err = UnsupportedResponseType "implicit"
                    (status, message) = validationErrorToResponse err
                statusCode status `shouldBe` 400
                T.unpack message `shouldContain` "response_type"
                T.unpack message `shouldContain` "implicit"

            it "ClientNotRegistered returns descriptive 400 error" $ do
                let clientId = ClientId "unknown_client"
                    err = ClientNotRegistered clientId
                    (status, message) = validationErrorToResponse err
                statusCode status `shouldBe` 400
                T.unpack message `shouldContain` "client_id"
                T.unpack message `shouldContain` "unknown_client"

            it "MissingRequiredScope returns descriptive 400 error" $ do
                let scope = Scope "admin"
                    err = MissingRequiredScope scope
                    (status, message) = validationErrorToResponse err
                statusCode status `shouldBe` 400
                T.unpack message `shouldContain` "scope"
                T.unpack message `shouldContain` "admin"

            it "InvalidStateParameter returns descriptive 400 error" $ do
                let err = InvalidStateParameter "invalid state"
                    (status, message) = validationErrorToResponse err
                statusCode status `shouldBe` 400
                T.unpack message `shouldContain` "state"
                T.unpack message `shouldContain` "invalid state"

        describe "ValidationError constructors" $ do
            it "RedirectUriMismatch can be constructed and pattern matched" $ do
                let clientId = ClientId "client123"
                    uri = case parseURI "https://example.com/callback" of
                        Just u -> u
                        Nothing -> error "Test URI parsing failed"
                    redirectUri = RedirectUri uri
                    err = RedirectUriMismatch clientId redirectUri
                case err of
                    RedirectUriMismatch cid ruri -> do
                        cid `shouldBe` clientId
                        ruri `shouldBe` redirectUri
                    _ -> expectationFailure "Pattern match failed"

            it "UnsupportedResponseType can be constructed and pattern matched" $ do
                let err = UnsupportedResponseType "implicit"
                case err of
                    UnsupportedResponseType rt -> rt `shouldBe` "implicit"
                    _ -> expectationFailure "Pattern match failed"

            it "ClientNotRegistered can be constructed and pattern matched" $ do
                let clientId = ClientId "unknown"
                    err = ClientNotRegistered clientId
                case err of
                    ClientNotRegistered cid -> cid `shouldBe` clientId
                    _ -> expectationFailure "Pattern match failed"

            it "MissingRequiredScope can be constructed and pattern matched" $ do
                let scope = Scope "admin"
                    err = MissingRequiredScope scope
                case err of
                    MissingRequiredScope s -> s `shouldBe` scope
                    _ -> expectationFailure "Pattern match failed"

            it "InvalidStateParameter can be constructed and pattern matched" $ do
                let err = InvalidStateParameter "bad state"
                case err of
                    InvalidStateParameter msg -> msg `shouldBe` "bad state"
                    _ -> expectationFailure "Pattern match failed"
