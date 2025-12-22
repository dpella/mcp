{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module MCP.Server.HTTP.AppEnvSpec (spec) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (decode)
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Functor.Contravariant (contramap)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.List (isInfixOf)
import Data.Maybe (isJust)
import Plow.Logging (IOTracer (..), Tracer (..))
import Servant (ServerError (..))
import Servant.OAuth2.IDP.Auth.Backend (mkUsername)
import Servant.OAuth2.IDP.Auth.Demo (DemoAuthError (..))
import Servant.OAuth2.IDP.Errors (
    AccessDeniedReason (..),
    AuthorizationError (..),
    LoginFlowError (..),
    OAuthErrorCode (..),
    OAuthErrorResponse (..),
    ValidationError (..),
 )
import Servant.OAuth2.IDP.Store.InMemory (OAuthStoreError (..))
import Servant.OAuth2.IDP.Trace (OAuthTrace (..), OperationResult (..))
import Servant.OAuth2.IDP.Types (mkClientId, mkSessionId)
import Test.Hspec

import MCP.Server.HTTP.AppEnv (AppError (..), appErrorToServerError)
import MCP.Trace.HTTP (HTTPTrace (..))

spec :: Spec
spec = do
    describe "AppEnv" $ do
        describe "envOAuthTracer" $ do
            it "should route OAuthTrace events through HTTPTrace using contramap HTTPOAuth" $ do
                -- Track captured traces
                capturedRef <- newIORef []

                -- Create HTTPTrace tracer that captures events
                let httpTracer = IOTracer $ Tracer $ \trace ->
                        liftIO $ modifyIORef capturedRef (trace :)

                -- Create OAuthTrace tracer using contramap (this is what we're testing)
                let oauthTracer = contramap HTTPOAuth httpTracer

                -- Create test user with smart constructor
                let testUser = case mkUsername "testuser" of
                        Just u -> u
                        Nothing -> error "Test fixture: invalid username"

                -- Emit an OAuthTrace event
                let oauthEvent = TraceLoginAttempt testUser Success
                case oauthTracer of
                    IOTracer (Tracer f) -> f oauthEvent

                -- Verify the event was wrapped in HTTPOAuth and captured
                captured <- readIORef capturedRef
                captured `shouldBe` [HTTPOAuth oauthEvent]

        describe "appErrorToServerError" $ do
            -- Helper to check if a ByteString contains a substring
            let containsText needle haystack = needle `isInfixOf` LBS8.unpack haystack

            describe "OAuthStoreErr" $ do
                it "maps StoreUnavailable to 500 Internal Server Error" $ do
                    let err = OAuthStoreErr (StoreUnavailable "Database connection failed")
                        serverErr = appErrorToServerError err
                    errHTTPCode serverErr `shouldBe` 500
                    -- Should NOT leak backend details
                    errBody serverErr `shouldNotSatisfy` containsText "Database connection failed"
                    errBody serverErr `shouldSatisfy` containsText "Internal Server Error"
                    errHeaders serverErr `shouldContain` [("Content-Type", "text/plain; charset=utf-8")]

                it "maps StoreInternalError to 500 Internal Server Error" $ do
                    let err = OAuthStoreErr (StoreInternalError "Redis timeout")
                        serverErr = appErrorToServerError err
                    errHTTPCode serverErr `shouldBe` 500
                    -- Should NOT leak backend details
                    errBody serverErr `shouldNotSatisfy` containsText "Redis timeout"
                    errBody serverErr `shouldSatisfy` containsText "Internal Server Error"
                    errHeaders serverErr `shouldContain` [("Content-Type", "text/plain; charset=utf-8")]

            describe "ValidationErr" $ do
                it "maps ClientNotRegistered to 400 Bad Request with descriptive text" $ do
                    let testClientId = case mkClientId "test_client_123" of
                            Just cid -> cid
                            Nothing -> error "Test fixture: invalid client ID"
                        err = ValidationErr (ClientNotRegistered testClientId)
                        serverErr = appErrorToServerError err
                    errHTTPCode serverErr `shouldBe` 400
                    errBody serverErr `shouldSatisfy` containsText "client_id not registered"
                    errBody serverErr `shouldSatisfy` containsText "test_client_123"
                    errHeaders serverErr `shouldContain` [("Content-Type", "text/plain; charset=utf-8")]

                it "maps EmptyRedirectUris to 400 Bad Request with descriptive text" $ do
                    let err = ValidationErr EmptyRedirectUris
                        serverErr = appErrorToServerError err
                    errHTTPCode serverErr `shouldBe` 400
                    errBody serverErr `shouldSatisfy` containsText "redirect_uri"
                    errBody serverErr `shouldSatisfy` containsText "at least one"
                    errHeaders serverErr `shouldContain` [("Content-Type", "text/plain; charset=utf-8")]

            describe "AuthorizationErr" $ do
                it "maps ExpiredCode to 400 with JSON OAuth error response" $ do
                    let err = AuthorizationErr ExpiredCode
                        serverErr = appErrorToServerError err
                    errHTTPCode serverErr `shouldBe` 400
                    let decoded = decode (errBody serverErr) :: Maybe OAuthErrorResponse
                    decoded `shouldSatisfy` isJust
                    let Just oauthResp = decoded
                    oauthErrorCode oauthResp `shouldBe` ErrInvalidGrant
                    oauthErrorDescription oauthResp `shouldBe` Just "Authorization code has expired"
                    errHeaders serverErr `shouldContain` [("Content-Type", "application/json; charset=utf-8")]

                it "maps AccessDenied to 403 with JSON OAuth error response" $ do
                    let err = AuthorizationErr (AccessDenied UserDenied)
                        serverErr = appErrorToServerError err
                    errHTTPCode serverErr `shouldBe` 403
                    let decoded = decode (errBody serverErr) :: Maybe OAuthErrorResponse
                    decoded `shouldSatisfy` isJust
                    let Just oauthResp = decoded
                    oauthErrorCode oauthResp `shouldBe` ErrAccessDenied
                    errHeaders serverErr `shouldContain` [("Content-Type", "application/json; charset=utf-8")]

            describe "LoginFlowErr" $ do
                it "maps CookiesRequired to 400 with HTML error page" $ do
                    let err = LoginFlowErr CookiesRequired
                        serverErr = appErrorToServerError err
                    errHTTPCode serverErr `shouldBe` 400
                    errBody serverErr `shouldSatisfy` containsText "<!DOCTYPE HTML>"
                    errBody serverErr `shouldSatisfy` containsText "Cookies Required"
                    errHeaders serverErr `shouldContain` [("Content-Type", "text/html; charset=utf-8")]

                it "maps SessionExpired to 400 with HTML error page" $ do
                    let testSessionId = case mkSessionId "12345678-1234-1234-1234-123456789abc" of
                            Just sid -> sid
                            Nothing -> error "Test fixture: invalid session ID"
                        err = LoginFlowErr (SessionExpired testSessionId)
                        serverErr = appErrorToServerError err
                    errHTTPCode serverErr `shouldBe` 400
                    errBody serverErr `shouldSatisfy` containsText "<!DOCTYPE HTML>"
                    errBody serverErr `shouldSatisfy` containsText "Session Expired"
                    errHeaders serverErr `shouldContain` [("Content-Type", "text/html; charset=utf-8")]

            describe "AuthBackendErr" $ do
                it "maps InvalidCredentials to 401 Unauthorized" $ do
                    let err = AuthBackendErr InvalidCredentials
                        serverErr = appErrorToServerError err
                    errHTTPCode serverErr `shouldBe` 401
                    errBody serverErr `shouldSatisfy` containsText "Unauthorized"
                    errHeaders serverErr `shouldContain` [("Content-Type", "text/plain; charset=utf-8")]

                it "maps UserNotFound to 401 Unauthorized without leaking existence" $ do
                    let testUsername = case mkUsername "testuser" of
                            Just u -> u
                            Nothing -> error "Test fixture: invalid username"
                        err = AuthBackendErr (UserNotFound testUsername)
                        serverErr = appErrorToServerError err
                    errHTTPCode serverErr `shouldBe` 401
                    -- Should NOT leak username
                    errBody serverErr `shouldNotSatisfy` containsText "testuser"
                    errBody serverErr `shouldSatisfy` containsText "Unauthorized"
                    errHeaders serverErr `shouldContain` [("Content-Type", "text/plain; charset=utf-8")]
