{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Trace.OAuthSpec
Description : Tests for OAuth trace rendering and integration
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT

Tests for SC-003: OAuth flow traces.
Verifies that OAuth trace constructors exist and render correctly.
-}
module Trace.OAuthSpec (spec) where

import Control.Monad.IO.Class (liftIO)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text qualified as T
import MCP.Trace.OAuth (OAuthTrace (..), renderOAuthTrace)
import Plow.Logging (IOTracer (..), Tracer (..), traceWith)
import Test.Hspec

spec :: Spec
spec = do
    describe "MCP.Trace.OAuth" $ do
        describe "renderOAuthTrace" $ do
            it "renders OAuthClientRegistration" $ do
                let trace = OAuthClientRegistration{clientId = "client-123", clientName = "Test Client"}
                    rendered = renderOAuthTrace trace
                rendered `shouldSatisfy` T.isInfixOf "Client registered"
                rendered `shouldSatisfy` T.isInfixOf "client-123"
                rendered `shouldSatisfy` T.isInfixOf "Test Client"

            it "renders OAuthAuthorizationRequest with scopes and state" $ do
                let trace =
                        OAuthAuthorizationRequest
                            { clientId = "client-123"
                            , scopes = ["read", "write"]
                            , hasState = True
                            }
                    rendered = renderOAuthTrace trace
                rendered `shouldSatisfy` T.isInfixOf "Authorization request"
                rendered `shouldSatisfy` T.isInfixOf "client-123"
                rendered `shouldSatisfy` T.isInfixOf "read"
                rendered `shouldSatisfy` T.isInfixOf "write"
                rendered `shouldSatisfy` T.isInfixOf "(with state)"

            it "renders OAuthAuthorizationRequest without state" $ do
                let trace =
                        OAuthAuthorizationRequest
                            { clientId = "client-123"
                            , scopes = []
                            , hasState = False
                            }
                    rendered = renderOAuthTrace trace
                rendered `shouldSatisfy` T.isInfixOf "(none)"
                rendered `shouldNotSatisfy` T.isInfixOf "(with state)"

            it "renders OAuthLoginPageServed" $ do
                let trace = OAuthLoginPageServed{sessionId = "session-abc"}
                    rendered = renderOAuthTrace trace
                rendered `shouldSatisfy` T.isInfixOf "Login page served"
                rendered `shouldSatisfy` T.isInfixOf "session-abc"

            it "renders OAuthLoginAttempt success" $ do
                let trace = OAuthLoginAttempt{username = "testuser", success = True}
                    rendered = renderOAuthTrace trace
                rendered `shouldSatisfy` T.isInfixOf "Login attempt"
                rendered `shouldSatisfy` T.isInfixOf "testuser"
                rendered `shouldSatisfy` T.isInfixOf "SUCCESS"

            it "renders OAuthLoginAttempt failure" $ do
                let trace = OAuthLoginAttempt{username = "testuser", success = False}
                    rendered = renderOAuthTrace trace
                rendered `shouldSatisfy` T.isInfixOf "FAILED"

            it "renders OAuthPKCEValidation success" $ do
                let trace =
                        OAuthPKCEValidation
                            { pkceVerifier = "verifier123456789"
                            , pkceChallenge = "challenge123456789"
                            , pkceIsValid = True
                            }
                    rendered = renderOAuthTrace trace
                rendered `shouldSatisfy` T.isInfixOf "PKCE validation"
                rendered `shouldSatisfy` T.isInfixOf "SUCCESS"
                -- Should truncate long verifiers/challenges
                rendered `shouldSatisfy` T.isInfixOf "verifier12"
                rendered `shouldSatisfy` T.isInfixOf "challenge1"

            it "renders OAuthPKCEValidation failure" $ do
                let trace =
                        OAuthPKCEValidation
                            { pkceVerifier = "verifier123456789"
                            , pkceChallenge = "challenge123456789"
                            , pkceIsValid = False
                            }
                    rendered = renderOAuthTrace trace
                rendered `shouldSatisfy` T.isInfixOf "FAILED"

            it "renders OAuthAuthorizationGranted" $ do
                let trace = OAuthAuthorizationGranted{clientId = "client-123", userId = "user-456"}
                    rendered = renderOAuthTrace trace
                rendered `shouldSatisfy` T.isInfixOf "Authorization granted"
                rendered `shouldSatisfy` T.isInfixOf "client-123"
                rendered `shouldSatisfy` T.isInfixOf "user-456"

            it "renders OAuthAuthorizationDenied" $ do
                let trace = OAuthAuthorizationDenied{clientId = "client-123", reason = "User declined"}
                    rendered = renderOAuthTrace trace
                rendered `shouldSatisfy` T.isInfixOf "Authorization denied"
                rendered `shouldSatisfy` T.isInfixOf "client-123"
                rendered `shouldSatisfy` T.isInfixOf "User declined"

            it "renders OAuthTokenExchange success" $ do
                let trace = OAuthTokenExchange{grantType = "authorization_code", success = True}
                    rendered = renderOAuthTrace trace
                rendered `shouldSatisfy` T.isInfixOf "Token exchange"
                rendered `shouldSatisfy` T.isInfixOf "authorization_code"
                rendered `shouldSatisfy` T.isInfixOf "SUCCESS"

            it "renders OAuthTokenExchange failure" $ do
                let trace = OAuthTokenExchange{grantType = "refresh_token", success = False}
                    rendered = renderOAuthTrace trace
                rendered `shouldSatisfy` T.isInfixOf "refresh_token"
                rendered `shouldSatisfy` T.isInfixOf "FAILED"

            it "renders OAuthTokenRefresh success" $ do
                let trace = OAuthTokenRefresh{success = True}
                    rendered = renderOAuthTrace trace
                rendered `shouldSatisfy` T.isInfixOf "Token refresh"
                rendered `shouldSatisfy` T.isInfixOf "SUCCESS"

            it "renders OAuthTokenRefresh failure" $ do
                let trace = OAuthTokenRefresh{success = False}
                    rendered = renderOAuthTrace trace
                rendered `shouldSatisfy` T.isInfixOf "FAILED"

            it "renders OAuthSessionExpired" $ do
                let trace = OAuthSessionExpired{sessionId = "session-xyz"}
                    rendered = renderOAuthTrace trace
                rendered `shouldSatisfy` T.isInfixOf "Session expired"
                rendered `shouldSatisfy` T.isInfixOf "session-xyz"

            it "renders OAuthValidationError" $ do
                let trace = OAuthValidationError{errorType = "invalid_request", validationDetail = "Missing redirect_uri"}
                    rendered = renderOAuthTrace trace
                rendered `shouldSatisfy` T.isInfixOf "Validation error"
                rendered `shouldSatisfy` T.isInfixOf "invalid_request"
                rendered `shouldSatisfy` T.isInfixOf "Missing redirect_uri"

        describe "IOTracer integration" $ do
            it "captures traces via IOTracer" $ do
                -- Create a test tracer that captures traces to an IORef
                tracesRef <- newIORef ([] :: [OAuthTrace])
                let testTracer = IOTracer $ Tracer $ \trace -> liftIO $ modifyIORef' tracesRef (trace :)

                -- Emit some test traces
                traceWith testTracer $ OAuthClientRegistration{clientId = "test-client", clientName = "Test"}
                traceWith testTracer $ OAuthLoginAttempt{username = "testuser", success = True}
                traceWith testTracer $ OAuthTokenExchange{grantType = "authorization_code", success = True}

                -- Verify traces were captured (reverse to get chronological order)
                traces <- reverse <$> readIORef tracesRef
                length traces `shouldBe` 3
                case traces of
                    [t1, t2, t3] -> do
                        t1 `shouldBe` OAuthClientRegistration{clientId = "test-client", clientName = "Test"}
                        t2 `shouldBe` OAuthLoginAttempt{username = "testuser", success = True}
                        t3 `shouldBe` OAuthTokenExchange{grantType = "authorization_code", success = True}
                    _ -> expectationFailure "Expected exactly 3 traces"

            it "captures multiple authorization flow traces" $ do
                -- Create a test tracer
                tracesRef <- newIORef ([] :: [OAuthTrace])
                let testTracer = IOTracer $ Tracer $ \trace -> liftIO $ modifyIORef' tracesRef (trace :)

                -- Simulate authorization flow
                traceWith testTracer $
                    OAuthAuthorizationRequest
                        { clientId = "client-123"
                        , scopes = ["read", "write"]
                        , hasState = True
                        }
                traceWith testTracer $ OAuthLoginPageServed{sessionId = "session-abc"}
                traceWith testTracer $ OAuthLoginAttempt{username = "demo", success = True}
                traceWith testTracer $ OAuthAuthorizationGranted{clientId = "client-123", userId = "user-demo"}

                -- Verify all authorization flow traces (reverse to get chronological order)
                traces <- reverse <$> readIORef tracesRef
                length traces `shouldBe` 4

                -- Check types (chronological order)
                case traces of
                    [t1, t2, t3, t4] -> do
                        case t1 of
                            OAuthAuthorizationRequest{} -> return ()
                            _ -> expectationFailure "Expected OAuthAuthorizationRequest"

                        case t2 of
                            OAuthLoginPageServed{} -> return ()
                            _ -> expectationFailure "Expected OAuthLoginPageServed"

                        case t3 of
                            OAuthLoginAttempt{} -> return ()
                            _ -> expectationFailure "Expected OAuthLoginAttempt"

                        case t4 of
                            OAuthAuthorizationGranted{} -> return ()
                            _ -> expectationFailure "Expected OAuthAuthorizationGranted"
                    _ -> expectationFailure "Expected exactly 4 traces"
