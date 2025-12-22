{-# LANGUAGE OverloadedStrings #-}

{- HLINT ignore "Avoid partial function" -}

{- |
Module      : Trace.RenderSpec
Description : Tests for trace rendering functions
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT

Tests for SC-004: Render functions totality and correctness.
Verifies that all trace types can be rendered without runtime errors.
-}
module Trace.RenderSpec (spec) where

import Data.Maybe (fromJust)
import Data.Text qualified as T
import MCP.Trace.HTTP (HTTPTrace (..), renderHTTPTrace)
import MCP.Trace.Protocol (ProtocolTrace (..), renderProtocolTrace)
import MCP.Trace.Server (ServerTrace (..), renderServerTrace)
import MCP.Trace.StdIO (StdIOTrace (..), renderStdIOTrace)
import MCP.Trace.Types (MCPTrace (..), renderMCPTrace)
import Servant.OAuth2.IDP.Trace (OAuthTrace (..))
import Servant.OAuth2.IDP.Types (mkClientId, mkRedirectUri)
import Test.Hspec

spec :: Spec
spec = do
    describe "MCP.Trace.Server rendering" $ do
        describe "renderServerTrace" $ do
            it "renders ServerInit without error" $ do
                let trace = ServerInit{serverName = "test-server", serverVersion = "1.0.0"}
                    rendered = renderServerTrace trace
                rendered `shouldSatisfy` (not . T.null)
                rendered `shouldSatisfy` T.isInfixOf "test-server"
                rendered `shouldSatisfy` T.isInfixOf "1.0.0"

            it "renders ServerShutdown without error" $ do
                let trace = ServerShutdown
                    rendered = renderServerTrace trace
                rendered `shouldSatisfy` (not . T.null)
                rendered `shouldSatisfy` T.isInfixOf "Shutdown"

            it "renders ServerInitialized with client info" $ do
                let trace = ServerInitialized{clientInfo = Just "test-client"}
                    rendered = renderServerTrace trace
                rendered `shouldSatisfy` (not . T.null)
                rendered `shouldSatisfy` T.isInfixOf "test-client"

            it "renders ServerInitialized without client info" $ do
                let trace = ServerInitialized{clientInfo = Nothing}
                    rendered = renderServerTrace trace
                rendered `shouldSatisfy` (not . T.null)
                rendered `shouldSatisfy` T.isInfixOf "no client"

            it "renders ServerCapabilityNegotiation with capabilities" $ do
                let trace = ServerCapabilityNegotiation{negotiatedCapabilities = ["tools", "prompts", "resources"]}
                    rendered = renderServerTrace trace
                rendered `shouldSatisfy` (not . T.null)
                rendered `shouldSatisfy` T.isInfixOf "tools"
                rendered `shouldSatisfy` T.isInfixOf "prompts"

            it "renders ServerCapabilityNegotiation with empty capabilities" $ do
                let trace = ServerCapabilityNegotiation{negotiatedCapabilities = []}
                    rendered = renderServerTrace trace
                rendered `shouldSatisfy` (not . T.null)

            it "renders ServerStateChange without error" $ do
                let trace = ServerStateChange{fromState = "starting", toState = "ready"}
                    rendered = renderServerTrace trace
                rendered `shouldSatisfy` (not . T.null)
                rendered `shouldSatisfy` T.isInfixOf "starting"
                rendered `shouldSatisfy` T.isInfixOf "ready"

    describe "MCP.Trace.Protocol rendering" $ do
        describe "renderProtocolTrace" $ do
            it "renders ProtocolRequestReceived without error" $ do
                let trace = ProtocolRequestReceived{requestId = "req-123", method = "tools/list"}
                    rendered = renderProtocolTrace trace
                rendered `shouldSatisfy` (not . T.null)
                rendered `shouldSatisfy` T.isInfixOf "req-123"
                rendered `shouldSatisfy` T.isInfixOf "tools/list"

            it "renders ProtocolResponseSent with error=True" $ do
                let trace = ProtocolResponseSent{requestId = "req-123", isError = True}
                    rendered = renderProtocolTrace trace
                rendered `shouldSatisfy` (not . T.null)
                rendered `shouldSatisfy` T.isInfixOf "req-123"
                rendered `shouldSatisfy` T.isInfixOf "True"

            it "renders ProtocolResponseSent with error=False" $ do
                let trace = ProtocolResponseSent{requestId = "req-123", isError = False}
                    rendered = renderProtocolTrace trace
                rendered `shouldSatisfy` (not . T.null)
                rendered `shouldSatisfy` T.isInfixOf "False"

            it "renders ProtocolNotificationReceived without error" $ do
                let trace = ProtocolNotificationReceived{method = "notifications/initialized"}
                    rendered = renderProtocolTrace trace
                rendered `shouldSatisfy` (not . T.null)
                rendered `shouldSatisfy` T.isInfixOf "notifications/initialized"

            it "renders ProtocolParseError with raw input" $ do
                let trace = ProtocolParseError{errorMessage = "Invalid JSON", rawInput = Just "{invalid}"}
                    rendered = renderProtocolTrace trace
                rendered `shouldSatisfy` (not . T.null)
                rendered `shouldSatisfy` T.isInfixOf "Invalid JSON"
                rendered `shouldSatisfy` T.isInfixOf "{invalid}"

            it "renders ProtocolParseError without raw input" $ do
                let trace = ProtocolParseError{errorMessage = "Invalid JSON", rawInput = Nothing}
                    rendered = renderProtocolTrace trace
                rendered `shouldSatisfy` (not . T.null)
                rendered `shouldSatisfy` T.isInfixOf "Invalid JSON"

            it "renders ProtocolMethodNotFound without error" $ do
                let trace = ProtocolMethodNotFound{requestId = "req-123", method = "unknown/method"}
                    rendered = renderProtocolTrace trace
                rendered `shouldSatisfy` (not . T.null)
                rendered `shouldSatisfy` T.isInfixOf "req-123"
                rendered `shouldSatisfy` T.isInfixOf "unknown/method"

            it "renders ProtocolInvalidParams without error" $ do
                let trace = ProtocolInvalidParams{requestId = "req-123", method = "tools/call", errorDetail = "Missing name"}
                    rendered = renderProtocolTrace trace
                rendered `shouldSatisfy` (not . T.null)
                rendered `shouldSatisfy` T.isInfixOf "req-123"
                rendered `shouldSatisfy` T.isInfixOf "tools/call"
                rendered `shouldSatisfy` T.isInfixOf "Missing name"

    describe "MCP.Trace.StdIO rendering" $ do
        describe "renderStdIOTrace" $ do
            it "renders StdIOServer nested events" $ do
                let trace = StdIOServer (ServerInit{serverName = "test", serverVersion = "1.0"})
                    rendered = renderStdIOTrace trace
                rendered `shouldSatisfy` (not . T.null)
                rendered `shouldSatisfy` T.isInfixOf "test"
                rendered `shouldSatisfy` T.isInfixOf "1.0"

            it "renders StdIOProtocol nested events" $ do
                let trace = StdIOProtocol (ProtocolRequestReceived{requestId = "req-1", method = "tools/list"})
                    rendered = renderStdIOTrace trace
                rendered `shouldSatisfy` (not . T.null)
                rendered `shouldSatisfy` T.isInfixOf "req-1"
                rendered `shouldSatisfy` T.isInfixOf "tools/list"

            it "renders StdIOMessageReceived without error" $ do
                let trace = StdIOMessageReceived{messageSize = 1024}
                    rendered = renderStdIOTrace trace
                rendered `shouldSatisfy` (not . T.null)
                rendered `shouldSatisfy` T.isInfixOf "1024"

            it "renders StdIOMessageSent without error" $ do
                let trace = StdIOMessageSent{messageSize = 512}
                    rendered = renderStdIOTrace trace
                rendered `shouldSatisfy` (not . T.null)
                rendered `shouldSatisfy` T.isInfixOf "512"

            it "renders StdIOReadError without error" $ do
                let trace = StdIOReadError{stdioErrorMessage = "Connection closed"}
                    rendered = renderStdIOTrace trace
                rendered `shouldSatisfy` (not . T.null)
                rendered `shouldSatisfy` T.isInfixOf "Connection closed"

            it "renders StdIOEOF without error" $ do
                let trace = StdIOEOF
                    rendered = renderStdIOTrace trace
                rendered `shouldSatisfy` (not . T.null)

    describe "MCP.Trace.HTTP rendering" $ do
        describe "renderHTTPTrace" $ do
            it "renders HTTPServerStarting without error" $ do
                let trace = HTTPServerStarting{tracePort = 8080, traceBaseUrl = "http://localhost:8080"}
                    rendered = renderHTTPTrace trace
                rendered `shouldSatisfy` (not . T.null)
                rendered `shouldSatisfy` T.isInfixOf "8080"
                rendered `shouldSatisfy` T.isInfixOf "localhost"

            it "renders HTTPServerStarted without error" $ do
                let trace = HTTPServerStarted
                    rendered = renderHTTPTrace trace
                rendered `shouldSatisfy` (not . T.null)

            it "renders HTTPRequestReceived with authentication" $ do
                let trace = HTTPRequestReceived{tracePath = "/mcp", traceMethod = "POST", traceHasAuth = True}
                    rendered = renderHTTPTrace trace
                rendered `shouldSatisfy` (not . T.null)
                rendered `shouldSatisfy` T.isInfixOf "/mcp"
                rendered `shouldSatisfy` T.isInfixOf "POST"
                rendered `shouldSatisfy` T.isInfixOf "authenticated"

            it "renders HTTPRequestReceived without authentication" $ do
                let trace = HTTPRequestReceived{tracePath = "/mcp", traceMethod = "GET", traceHasAuth = False}
                    rendered = renderHTTPTrace trace
                rendered `shouldSatisfy` (not . T.null)
                rendered `shouldSatisfy` T.isInfixOf "no auth"

            it "renders HTTPAuthRequired without error" $ do
                let trace = HTTPAuthRequired{traceAuthPath = "/protected"}
                    rendered = renderHTTPTrace trace
                rendered `shouldSatisfy` (not . T.null)
                rendered `shouldSatisfy` T.isInfixOf "/protected"

            it "renders HTTPAuthSuccess without error" $ do
                let trace = HTTPAuthSuccess{traceAuthUserId = "user-123"}
                    rendered = renderHTTPTrace trace
                rendered `shouldSatisfy` (not . T.null)
                rendered `shouldSatisfy` T.isInfixOf "user-123"

            it "renders HTTPAuthFailure without error" $ do
                let trace = HTTPAuthFailure{traceAuthReason = "Invalid token"}
                    rendered = renderHTTPTrace trace
                rendered `shouldSatisfy` (not . T.null)
                rendered `shouldSatisfy` T.isInfixOf "Invalid token"

            it "renders HTTPOAuthEnabled without error" $ do
                let trace = HTTPOAuthEnabled{traceAuthEndpoint = "/authorize", traceTokenEndpoint = "/token"}
                    rendered = renderHTTPTrace trace
                rendered `shouldSatisfy` (not . T.null)
                rendered `shouldSatisfy` T.isInfixOf "/authorize"
                rendered `shouldSatisfy` T.isInfixOf "/token"

            it "renders HTTPOAuthProviders without error" $ do
                let trace = HTTPOAuthProviders{traceProviderNames = ["google", "github"]}
                    rendered = renderHTTPTrace trace
                rendered `shouldSatisfy` (not . T.null)
                rendered `shouldSatisfy` T.isInfixOf "google"
                rendered `shouldSatisfy` T.isInfixOf "github"

            it "renders HTTPPKCEEnabled without error" $ do
                let trace = HTTPPKCEEnabled
                    rendered = renderHTTPTrace trace
                rendered `shouldSatisfy` (not . T.null)
                rendered `shouldSatisfy` T.isInfixOf "PKCE"

            it "renders HTTPResourceParameterDebug with resource" $ do
                let trace = HTTPResourceParameterDebug{traceResourceParam = Just "my-resource", traceContext = "token request"}
                    rendered = renderHTTPTrace trace
                rendered `shouldSatisfy` (not . T.null)
                rendered `shouldSatisfy` T.isInfixOf "my-resource"
                rendered `shouldSatisfy` T.isInfixOf "token request"

            it "renders HTTPResourceParameterDebug without resource" $ do
                let trace = HTTPResourceParameterDebug{traceResourceParam = Nothing, traceContext = "token request"}
                    rendered = renderHTTPTrace trace
                rendered `shouldSatisfy` (not . T.null)
                rendered `shouldSatisfy` T.isInfixOf "not provided"

            it "renders HTTPProtocol nested events" $ do
                let trace = HTTPProtocol (ProtocolRequestReceived{requestId = "req-1", method = "tools/list"})
                    rendered = renderHTTPTrace trace
                rendered `shouldSatisfy` (not . T.null)
                rendered `shouldSatisfy` T.isInfixOf "req-1"

            it "renders HTTPOAuth nested events" $ do
                let redirectUri = fromJust $ mkRedirectUri "http://localhost/callback"
                    trace = HTTPOAuth (TraceClientRegistration (fromJust $ mkClientId "client-1") redirectUri)
                    rendered = renderHTTPTrace trace
                rendered `shouldSatisfy` (not . T.null)
                rendered `shouldSatisfy` T.isInfixOf "client-1"

            it "renders HTTPServer nested events" $ do
                let trace = HTTPServer (ServerInit{serverName = "test", serverVersion = "1.0"})
                    rendered = renderHTTPTrace trace
                rendered `shouldSatisfy` (not . T.null)
                rendered `shouldSatisfy` T.isInfixOf "test"

    describe "MCP.Trace.Types rendering" $ do
        describe "renderMCPTrace" $ do
            it "renders MCPServer traces" $ do
                let trace = MCPServer (ServerInit{serverName = "test", serverVersion = "1.0"})
                    rendered = renderMCPTrace trace
                rendered `shouldSatisfy` (not . T.null)
                rendered `shouldSatisfy` T.isInfixOf "test"

            it "renders MCPProtocol traces" $ do
                let trace = MCPProtocol (ProtocolRequestReceived{requestId = "req-1", method = "tools/list"})
                    rendered = renderMCPTrace trace
                rendered `shouldSatisfy` (not . T.null)
                rendered `shouldSatisfy` T.isInfixOf "req-1"

            it "renders MCPStdIO traces" $ do
                let trace = MCPStdIO (StdIOMessageReceived{messageSize = 100})
                    rendered = renderMCPTrace trace
                rendered `shouldSatisfy` (not . T.null)
                rendered `shouldSatisfy` T.isInfixOf "100"

            it "renders MCPHttp traces" $ do
                let trace = MCPHttp (HTTPRequestReceived{tracePath = "/mcp", traceMethod = "POST", traceHasAuth = False})
                    rendered = renderMCPTrace trace
                rendered `shouldSatisfy` (not . T.null)
                rendered `shouldSatisfy` T.isInfixOf "/mcp"

            it "renders nested OAuth traces via MCPHttp" $ do
                let redirectUri = fromJust $ mkRedirectUri "http://localhost/callback"
                    trace = MCPHttp $ HTTPOAuth $ TraceClientRegistration (fromJust $ mkClientId "client-1") redirectUri
                    rendered = renderMCPTrace trace
                rendered `shouldSatisfy` (not . T.null)
                rendered `shouldSatisfy` T.isInfixOf "client-1"
