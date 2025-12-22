{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- HLINT ignore "Avoid partial function" -}

{- |
Module      : Trace.FilterSpec
Description : Tests for trace filtering functionality
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT

Tests for SC-006: filterTracer with predicates.
Verifies that filter predicates work correctly to select subsystem traces.
-}
module Trace.FilterSpec (spec) where

import Control.Monad.IO.Class (liftIO)
import Data.Functor.Contravariant (contramap)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Maybe (fromJust)
import Data.Text (Text)
import MCP.Trace.HTTP (HTTPTrace (..))
import MCP.Trace.Protocol (ProtocolTrace (..))
import MCP.Trace.Server (ServerTrace (..))
import MCP.Trace.StdIO (StdIOTrace (..))
import MCP.Trace.Types (
    MCPTrace (..),
    isErrorTrace,
    isHTTPTrace,
    isOAuthTrace,
    isProtocolTrace,
    isServerTrace,
    isStdIOTrace,
 )
import Plow.Logging (IOTracer (..), Tracer (..), filterTracer, traceWith)
import Servant.OAuth2.IDP.Auth.Backend (Username, mkUsername)
import Servant.OAuth2.IDP.Errors (ValidationError (..))
import Servant.OAuth2.IDP.Trace (DenialReason (..), OAuthTrace (..), OperationResult (..))
import Servant.OAuth2.IDP.Types (OAuthGrantType (..), mkClientId, mkRedirectUri)
import Test.Hspec

{- | Test helper: create username or fail with descriptive error
This is safe for test cases with hardcoded valid usernames
-}
mkUsernameOrFail :: Text -> Username
mkUsernameOrFail t = case mkUsername t of
    Just u -> u
    Nothing -> error $ "Test setup failed: invalid username: " ++ show t

spec :: Spec
spec = do
    describe "MCP.Trace.Types Filter Predicates" $ do
        describe "isOAuthTrace" $ do
            it "matches OAuth traces nested in HTTP" $ do
                let redirectUri = fromJust $ mkRedirectUri "http://localhost/callback"
                    trace = MCPHttp $ HTTPOAuth $ TraceClientRegistration (fromJust $ mkClientId "test") redirectUri
                isOAuthTrace trace `shouldBe` True

            it "does not match non-OAuth HTTP traces" $ do
                let trace = MCPHttp $ HTTPRequestReceived{tracePath = "/mcp", traceMethod = "POST", traceHasAuth = False}
                isOAuthTrace trace `shouldBe` False

            it "does not match other subsystem traces" $ do
                isOAuthTrace (MCPServer (ServerInit{serverName = "test", serverVersion = "1.0"})) `shouldBe` False
                isOAuthTrace (MCPProtocol (ProtocolRequestReceived{requestId = "req-1", method = "tools/list"})) `shouldBe` False
                isOAuthTrace (MCPStdIO (StdIOMessageReceived{messageSize = 100})) `shouldBe` False

        describe "isHTTPTrace" $ do
            it "matches HTTP traces" $ do
                let trace = MCPHttp $ HTTPRequestReceived{tracePath = "/mcp", traceMethod = "POST", traceHasAuth = False}
                isHTTPTrace trace `shouldBe` True

            it "matches HTTP traces with OAuth nested" $ do
                let trace = MCPHttp $ HTTPOAuth $ TraceTokenExchange OAuthAuthorizationCode Success
                isHTTPTrace trace `shouldBe` True

            it "does not match other subsystem traces" $ do
                isHTTPTrace (MCPServer (ServerInit{serverName = "test", serverVersion = "1.0"})) `shouldBe` False
                isHTTPTrace (MCPProtocol (ProtocolRequestReceived{requestId = "req-1", method = "tools/list"})) `shouldBe` False
                isHTTPTrace (MCPStdIO (StdIOMessageReceived{messageSize = 100})) `shouldBe` False

        describe "isServerTrace" $ do
            it "matches server traces" $ do
                isServerTrace (MCPServer (ServerInit{serverName = "test", serverVersion = "1.0"})) `shouldBe` True
                isServerTrace (MCPServer ServerShutdown) `shouldBe` True

            it "does not match other subsystem traces" $ do
                isServerTrace (MCPProtocol (ProtocolRequestReceived{requestId = "req-1", method = "tools/list"})) `shouldBe` False
                isServerTrace (MCPStdIO (StdIOMessageReceived{messageSize = 100})) `shouldBe` False
                isServerTrace (MCPHttp $ HTTPRequestReceived{tracePath = "/mcp", traceMethod = "POST", traceHasAuth = False}) `shouldBe` False

        describe "isProtocolTrace" $ do
            it "matches protocol traces" $ do
                let trace = MCPProtocol $ ProtocolRequestReceived{requestId = "req-1", method = "tools/list"}
                isProtocolTrace trace `shouldBe` True

            it "does not match other subsystem traces" $ do
                isProtocolTrace (MCPServer (ServerInit{serverName = "test", serverVersion = "1.0"})) `shouldBe` False
                isProtocolTrace (MCPStdIO (StdIOMessageReceived{messageSize = 100})) `shouldBe` False
                isProtocolTrace (MCPHttp $ HTTPRequestReceived{tracePath = "/mcp", traceMethod = "POST", traceHasAuth = False}) `shouldBe` False

        describe "isStdIOTrace" $ do
            it "matches StdIO traces" $ do
                let trace = MCPStdIO $ StdIOMessageReceived{messageSize = 100}
                isStdIOTrace trace `shouldBe` True

            it "does not match other subsystem traces" $ do
                isStdIOTrace (MCPServer (ServerInit{serverName = "test", serverVersion = "1.0"})) `shouldBe` False
                isStdIOTrace (MCPProtocol (ProtocolRequestReceived{requestId = "req-1", method = "tools/list"})) `shouldBe` False
                isStdIOTrace (MCPHttp $ HTTPRequestReceived{tracePath = "/mcp", traceMethod = "POST", traceHasAuth = False}) `shouldBe` False

        describe "isErrorTrace" $ do
            it "matches protocol error traces" $ do
                isErrorTrace (MCPProtocol (ProtocolParseError{errorMessage = "Invalid JSON", rawInput = Nothing})) `shouldBe` True
                isErrorTrace (MCPProtocol (ProtocolMethodNotFound{requestId = "req-1", method = "unknown/method"})) `shouldBe` True
                isErrorTrace (MCPProtocol (ProtocolInvalidParams{requestId = "req-1", method = "tools/call", errorDetail = "Missing param"})) `shouldBe` True

            it "matches StdIO error traces" $ do
                isErrorTrace (MCPStdIO (StdIOReadError{stdioErrorMessage = "EOF"})) `shouldBe` True

            it "matches HTTP error traces" $ do
                isErrorTrace (MCPHttp (HTTPAuthFailure{traceAuthReason = "Invalid token"})) `shouldBe` True

            it "matches OAuth error traces" $ do
                isErrorTrace (MCPHttp (HTTPOAuth (TraceAuthorizationDenied (fromJust $ mkClientId "client-1") UserDenied))) `shouldBe` True
                let redirectUri = fromJust $ mkRedirectUri "https://wrong.com/callback"
                isErrorTrace (MCPHttp (HTTPOAuth (TraceValidationError (RedirectUriMismatch (fromJust $ mkClientId "client-1") redirectUri)))) `shouldBe` True

            it "does not match non-error traces" $ do
                isErrorTrace (MCPServer (ServerInit{serverName = "test", serverVersion = "1.0"})) `shouldBe` False
                isErrorTrace (MCPProtocol (ProtocolRequestReceived{requestId = "req-1", method = "tools/list"})) `shouldBe` False
                isErrorTrace (MCPStdIO (StdIOMessageReceived{messageSize = 100})) `shouldBe` False
                isErrorTrace (MCPHttp (HTTPRequestReceived{tracePath = "/mcp", traceMethod = "POST", traceHasAuth = False})) `shouldBe` False
                let redirectUri = fromJust $ mkRedirectUri "http://localhost/callback"
                isErrorTrace (MCPHttp (HTTPOAuth (TraceClientRegistration (fromJust $ mkClientId "test") redirectUri))) `shouldBe` False

    describe "filterTracer integration (SC-006)" $ do
        it "filters to only OAuth traces from mixed events" $ do
            -- Create a test tracer that captures traces to an IORef
            tracesRef <- newIORef ([] :: [MCPTrace])
            let captureTracer = IOTracer $ Tracer $ \trace -> liftIO $ modifyIORef' tracesRef (trace :)

            -- Apply OAuth-only filter
            let oauthOnlyTracer = IOTracer $ filterTracer isOAuthTrace (unIOTracer captureTracer)
                unIOTracer (IOTracer t) = t

            -- Emit mixed events (OAuth + HTTP + Protocol + Server)
            let redirectUri1 = fromJust $ mkRedirectUri "http://localhost/callback"
            traceWith oauthOnlyTracer $ MCPHttp $ HTTPOAuth $ TraceClientRegistration (fromJust $ mkClientId "client-1") redirectUri1
            traceWith oauthOnlyTracer $ MCPHttp $ HTTPRequestReceived{tracePath = "/mcp", traceMethod = "POST", traceHasAuth = False}
            traceWith oauthOnlyTracer $ MCPHttp $ HTTPOAuth $ TraceLoginAttempt (mkUsernameOrFail "demo") Success
            traceWith oauthOnlyTracer $ MCPProtocol $ ProtocolRequestReceived{requestId = "req-1", method = "tools/list"}
            traceWith oauthOnlyTracer $ MCPServer (ServerInit{serverName = "test", serverVersion = "1.0"})
            traceWith oauthOnlyTracer $ MCPHttp $ HTTPOAuth $ TraceTokenExchange OAuthAuthorizationCode Success

            -- Verify only OAuth traces appear (reverse to get chronological order)
            traces <- reverse <$> readIORef tracesRef
            length traces `shouldBe` 3

            -- All traces should be OAuth traces
            all isOAuthTrace traces `shouldBe` True

            -- Verify specific OAuth traces in chronological order
            case traces of
                [t1, t2, t3] -> do
                    case t1 of
                        MCPHttp (HTTPOAuth TraceClientRegistration{}) -> return ()
                        _ -> expectationFailure "Expected TraceClientRegistration"

                    case t2 of
                        MCPHttp (HTTPOAuth TraceLoginAttempt{}) -> return ()
                        _ -> expectationFailure "Expected TraceLoginAttempt"

                    case t3 of
                        MCPHttp (HTTPOAuth TraceTokenExchange{}) -> return ()
                        _ -> expectationFailure "Expected TraceTokenExchange"
                _ -> expectationFailure "Expected exactly 3 OAuth traces"

        it "filters to only HTTP traces (including nested OAuth)" $ do
            tracesRef <- newIORef ([] :: [MCPTrace])
            let captureTracer = IOTracer $ Tracer $ \trace -> liftIO $ modifyIORef' tracesRef (trace :)
                httpOnlyTracer = IOTracer $ filterTracer isHTTPTrace (unIOTracer captureTracer)
                unIOTracer (IOTracer t) = t

            -- Emit mixed events
            traceWith httpOnlyTracer $ MCPHttp $ HTTPRequestReceived{tracePath = "/mcp", traceMethod = "POST", traceHasAuth = False}
            traceWith httpOnlyTracer $ MCPServer (ServerInit{serverName = "test", serverVersion = "1.0"})
            let redirectUri = fromJust $ mkRedirectUri "http://localhost/callback"
            traceWith httpOnlyTracer $ MCPHttp $ HTTPOAuth $ TraceClientRegistration (fromJust $ mkClientId "client-1") redirectUri
            traceWith httpOnlyTracer $ MCPProtocol $ ProtocolRequestReceived{requestId = "req-1", method = "tools/list"}
            traceWith httpOnlyTracer $ MCPHttp $ HTTPAuthFailure{traceAuthReason = "Invalid token"}

            -- Verify only HTTP traces (including OAuth nested)
            traces <- reverse <$> readIORef tracesRef
            length traces `shouldBe` 3
            all isHTTPTrace traces `shouldBe` True

        it "filters to only error traces" $ do
            tracesRef <- newIORef ([] :: [MCPTrace])
            let captureTracer = IOTracer $ Tracer $ \trace -> liftIO $ modifyIORef' tracesRef (trace :)
                errorOnlyTracer = IOTracer $ filterTracer isErrorTrace (unIOTracer captureTracer)
                unIOTracer (IOTracer t) = t

            -- Emit mixed events (errors + normal traces)
            traceWith errorOnlyTracer $ MCPProtocol $ ProtocolParseError{errorMessage = "Invalid JSON", rawInput = Nothing}
            traceWith errorOnlyTracer $ MCPServer (ServerInit{serverName = "test", serverVersion = "1.0"})
            traceWith errorOnlyTracer $ MCPStdIO $ StdIOReadError{stdioErrorMessage = "EOF"}
            traceWith errorOnlyTracer $ MCPHttp $ HTTPRequestReceived{tracePath = "/mcp", traceMethod = "POST", traceHasAuth = False}
            traceWith errorOnlyTracer $ MCPHttp $ HTTPAuthFailure{traceAuthReason = "Invalid token"}
            traceWith errorOnlyTracer $ MCPProtocol $ ProtocolRequestReceived{requestId = "req-1", method = "tools/list"}

            -- Verify only error traces
            traces <- reverse <$> readIORef tracesRef
            length traces `shouldBe` 3
            all isErrorTrace traces `shouldBe` True

        it "works with contramap to filter before type conversion" $ do
            -- This test verifies that filterTracer composes with contramap
            -- Use case: filter HTTP traces, then contramap to MCPTrace
            tracesRef <- newIORef ([] :: [HTTPTrace])
            let httpTracer = IOTracer $ Tracer $ \trace -> liftIO $ modifyIORef' tracesRef (trace :)

                -- Create MCPTrace tracer by contramapping and filtering
                mcpTracer :: IOTracer MCPTrace
                mcpTracer =
                    IOTracer $
                        filterTracer isHTTPTrace $
                            contramap
                                ( \case
                                    MCPHttp ht -> ht
                                    _ -> error "Non-HTTP trace passed filter"
                                )
                                (unIOTracer httpTracer)
                unIOTracer (IOTracer t) = t

            -- Emit mixed events
            traceWith mcpTracer $ MCPHttp $ HTTPRequestReceived{tracePath = "/mcp", traceMethod = "POST", traceHasAuth = False}
            traceWith mcpTracer $ MCPServer (ServerInit{serverName = "test", serverVersion = "1.0"})
            let redirectUri = fromJust $ mkRedirectUri "http://localhost/callback"
            traceWith mcpTracer $ MCPHttp $ HTTPOAuth $ TraceClientRegistration (fromJust $ mkClientId "test") redirectUri

            -- Verify only HTTP traces were captured (and converted)
            traces <- reverse <$> readIORef tracesRef
            length traces `shouldBe` 2
            case traces of
                [HTTPRequestReceived{}, HTTPOAuth TraceClientRegistration{}] -> return ()
                _ -> expectationFailure "Expected HTTPRequestReceived and HTTPOAuth traces"
