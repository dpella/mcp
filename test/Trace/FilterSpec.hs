{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

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
import MCP.Trace.HTTP (HTTPTrace (..))
import MCP.Trace.OAuth (OAuthTrace (..))
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
import Test.Hspec

spec :: Spec
spec = do
    describe "MCP.Trace.Types Filter Predicates" $ do
        describe "isOAuthTrace" $ do
            it "matches OAuth traces nested in HTTP" $ do
                let trace = MCPHttp $ HTTPOAuth $ OAuthClientRegistration{clientId = "test", clientName = "Test"}
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
                let trace = MCPHttp $ HTTPOAuth $ OAuthTokenExchange{grantType = "authorization_code", success = True}
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
                isErrorTrace (MCPHttp (HTTPOAuth (OAuthAuthorizationDenied{clientId = "client-1", reason = "User denied"}))) `shouldBe` True
                isErrorTrace (MCPHttp (HTTPOAuth (OAuthValidationError{errorType = "invalid_request", validationDetail = "Missing param"}))) `shouldBe` True

            it "does not match non-error traces" $ do
                isErrorTrace (MCPServer (ServerInit{serverName = "test", serverVersion = "1.0"})) `shouldBe` False
                isErrorTrace (MCPProtocol (ProtocolRequestReceived{requestId = "req-1", method = "tools/list"})) `shouldBe` False
                isErrorTrace (MCPStdIO (StdIOMessageReceived{messageSize = 100})) `shouldBe` False
                isErrorTrace (MCPHttp (HTTPRequestReceived{tracePath = "/mcp", traceMethod = "POST", traceHasAuth = False})) `shouldBe` False
                isErrorTrace (MCPHttp (HTTPOAuth (OAuthClientRegistration{clientId = "test", clientName = "Test"}))) `shouldBe` False

    describe "filterTracer integration (SC-006)" $ do
        it "filters to only OAuth traces from mixed events" $ do
            -- Create a test tracer that captures traces to an IORef
            tracesRef <- newIORef ([] :: [MCPTrace])
            let captureTracer = IOTracer $ Tracer $ \trace -> liftIO $ modifyIORef' tracesRef (trace :)

            -- Apply OAuth-only filter
            let oauthOnlyTracer = IOTracer $ filterTracer isOAuthTrace (unIOTracer captureTracer)
                unIOTracer (IOTracer t) = t

            -- Emit mixed events (OAuth + HTTP + Protocol + Server)
            traceWith oauthOnlyTracer $ MCPHttp $ HTTPOAuth $ OAuthClientRegistration{clientId = "client-1", clientName = "Test Client"}
            traceWith oauthOnlyTracer $ MCPHttp $ HTTPRequestReceived{tracePath = "/mcp", traceMethod = "POST", traceHasAuth = False}
            traceWith oauthOnlyTracer $ MCPHttp $ HTTPOAuth $ OAuthLoginAttempt{username = "demo", success = True}
            traceWith oauthOnlyTracer $ MCPProtocol $ ProtocolRequestReceived{requestId = "req-1", method = "tools/list"}
            traceWith oauthOnlyTracer $ MCPServer (ServerInit{serverName = "test", serverVersion = "1.0"})
            traceWith oauthOnlyTracer $ MCPHttp $ HTTPOAuth $ OAuthTokenExchange{grantType = "authorization_code", success = True}

            -- Verify only OAuth traces appear (reverse to get chronological order)
            traces <- reverse <$> readIORef tracesRef
            length traces `shouldBe` 3

            -- All traces should be OAuth traces
            all isOAuthTrace traces `shouldBe` True

            -- Verify specific OAuth traces in chronological order
            case traces of
                [t1, t2, t3] -> do
                    case t1 of
                        MCPHttp (HTTPOAuth OAuthClientRegistration{}) -> return ()
                        _ -> expectationFailure "Expected OAuthClientRegistration"

                    case t2 of
                        MCPHttp (HTTPOAuth OAuthLoginAttempt{}) -> return ()
                        _ -> expectationFailure "Expected OAuthLoginAttempt"

                    case t3 of
                        MCPHttp (HTTPOAuth OAuthTokenExchange{}) -> return ()
                        _ -> expectationFailure "Expected OAuthTokenExchange"
                _ -> expectationFailure "Expected exactly 3 OAuth traces"

        it "filters to only HTTP traces (including nested OAuth)" $ do
            tracesRef <- newIORef ([] :: [MCPTrace])
            let captureTracer = IOTracer $ Tracer $ \trace -> liftIO $ modifyIORef' tracesRef (trace :)
                httpOnlyTracer = IOTracer $ filterTracer isHTTPTrace (unIOTracer captureTracer)
                unIOTracer (IOTracer t) = t

            -- Emit mixed events
            traceWith httpOnlyTracer $ MCPHttp $ HTTPRequestReceived{tracePath = "/mcp", traceMethod = "POST", traceHasAuth = False}
            traceWith httpOnlyTracer $ MCPServer (ServerInit{serverName = "test", serverVersion = "1.0"})
            traceWith httpOnlyTracer $ MCPHttp $ HTTPOAuth $ OAuthClientRegistration{clientId = "client-1", clientName = "Test"}
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
            traceWith mcpTracer $ MCPHttp $ HTTPOAuth $ OAuthClientRegistration{clientId = "test", clientName = "Test"}

            -- Verify only HTTP traces were captured (and converted)
            traces <- reverse <$> readIORef tracesRef
            length traces `shouldBe` 2
            case traces of
                [HTTPRequestReceived{}, HTTPOAuth OAuthClientRegistration{}] -> return ()
                _ -> expectationFailure "Expected HTTPRequestReceived and HTTPOAuth traces"
