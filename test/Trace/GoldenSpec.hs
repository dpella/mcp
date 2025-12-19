{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Trace.GoldenSpec
Description : Golden tests for trace rendering output format
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT

Tests for SC-005: Golden tests for render output.
Verifies that render functions produce expected formatted output for representative examples.
-}
module Trace.GoldenSpec (spec) where

import MCP.Trace.HTTP (HTTPTrace (..), renderHTTPTrace)
import MCP.Trace.OAuth (OAuthTrace (..), renderOAuthTrace)
import MCP.Trace.Protocol (ProtocolTrace (..), renderProtocolTrace)
import MCP.Trace.Server (ServerTrace (..), renderServerTrace)
import MCP.Trace.StdIO (StdIOTrace (..), renderStdIOTrace)
import MCP.Trace.Types (MCPTrace (..), renderMCPTrace)
import Test.Hspec

spec :: Spec
spec = do
    describe "Golden tests for trace rendering" $ do
        describe "ServerTrace golden outputs" $ do
            it "ServerInit produces expected format" $ do
                let trace = ServerInit{serverName = "mcp-server", serverVersion = "2.0.1"}
                renderServerTrace trace `shouldBe` "[Server] Init: mcp-server v2.0.1"

            it "ServerShutdown produces expected format" $ do
                renderServerTrace ServerShutdown `shouldBe` "[Server] Shutdown"

            it "ServerInitialized with client produces expected format" $ do
                let trace = ServerInitialized{clientInfo = Just "Claude Desktop"}
                renderServerTrace trace `shouldBe` "[Server] Initialized with client: Claude Desktop"

            it "ServerInitialized without client produces expected format" $ do
                let trace = ServerInitialized{clientInfo = Nothing}
                renderServerTrace trace `shouldBe` "[Server] Initialized (no client info)"

            it "ServerCapabilityNegotiation produces expected format" $ do
                let trace = ServerCapabilityNegotiation{negotiatedCapabilities = ["tools", "prompts"]}
                renderServerTrace trace `shouldBe` "[Server] Capabilities negotiated: [tools, prompts]"

            it "ServerStateChange produces expected format" $ do
                let trace = ServerStateChange{fromState = "initializing", toState = "ready"}
                renderServerTrace trace `shouldBe` "[Server] State change: initializing -> ready"

        describe "ProtocolTrace golden outputs" $ do
            it "ProtocolRequestReceived produces expected format" $ do
                let trace = ProtocolRequestReceived{requestId = "msg-42", method = "tools/list"}
                renderProtocolTrace trace `shouldBe` "[Protocol] Request received: method='tools/list', id='msg-42'"

            it "ProtocolResponseSent with error produces expected format" $ do
                let trace = ProtocolResponseSent{requestId = "msg-42", isError = True}
                renderProtocolTrace trace `shouldBe` "[Protocol] Response sent: id='msg-42', error=True"

            it "ProtocolResponseSent without error produces expected format" $ do
                let trace = ProtocolResponseSent{requestId = "msg-42", isError = False}
                renderProtocolTrace trace `shouldBe` "[Protocol] Response sent: id='msg-42', error=False"

            it "ProtocolNotificationReceived produces expected format" $ do
                let trace = ProtocolNotificationReceived{method = "notifications/tools/list_changed"}
                renderProtocolTrace trace `shouldBe` "[Protocol] Notification received: method='notifications/tools/list_changed'"

            it "ProtocolParseError without raw input produces expected format" $ do
                let trace = ProtocolParseError{errorMessage = "Unexpected end of JSON", rawInput = Nothing}
                renderProtocolTrace trace `shouldBe` "[Protocol] Parse error: Unexpected end of JSON"

            it "ProtocolParseError with raw input produces expected format" $ do
                let trace = ProtocolParseError{errorMessage = "Invalid JSON", rawInput = Just "{\"broken"}
                renderProtocolTrace trace `shouldBe` "[Protocol] Parse error: Invalid JSON (input: '{\"broken')"

            it "ProtocolMethodNotFound produces expected format" $ do
                let trace = ProtocolMethodNotFound{requestId = "msg-99", method = "unknown/method"}
                renderProtocolTrace trace `shouldBe` "[Protocol] Method not found: method='unknown/method', id='msg-99'"

            it "ProtocolInvalidParams produces expected format" $ do
                let trace = ProtocolInvalidParams{requestId = "msg-100", method = "tools/call", errorDetail = "Missing required field: name"}
                renderProtocolTrace trace `shouldBe` "[Protocol] Invalid params: method='tools/call', id='msg-100' (error: Missing required field: name)"

        describe "StdIOTrace golden outputs" $ do
            it "StdIOMessageReceived produces expected format" $ do
                let trace = StdIOMessageReceived{messageSize = 2048}
                renderStdIOTrace trace `shouldBe` "[StdIO] Message received (2048 bytes)"

            it "StdIOMessageSent produces expected format" $ do
                let trace = StdIOMessageSent{messageSize = 1024}
                renderStdIOTrace trace `shouldBe` "[StdIO] Message sent (1024 bytes)"

            it "StdIOReadError produces expected format" $ do
                let trace = StdIOReadError{stdioErrorMessage = "Broken pipe"}
                renderStdIOTrace trace `shouldBe` "[StdIO] Read error: Broken pipe"

            it "StdIOEOF produces expected format" $ do
                renderStdIOTrace StdIOEOF `shouldBe` "[StdIO] End of file"

            it "StdIOServer nested event produces expected format with prefix" $ do
                let trace = StdIOServer (ServerInit{serverName = "stdio-server", serverVersion = "1.0"})
                renderStdIOTrace trace `shouldBe` "[StdIO:Server] [Server] Init: stdio-server v1.0"

            it "StdIOProtocol nested event produces expected format with prefix" $ do
                let trace = StdIOProtocol (ProtocolRequestReceived{requestId = "req-1", method = "prompts/list"})
                renderStdIOTrace trace `shouldBe` "[StdIO:Protocol] [Protocol] Request received: method='prompts/list', id='req-1'"

        describe "HTTPTrace golden outputs" $ do
            it "HTTPServerStarting produces expected format" $ do
                let trace = HTTPServerStarting{tracePort = 8080, traceBaseUrl = "http://localhost:8080"}
                renderHTTPTrace trace `shouldBe` "[HTTP] Server starting on port 8080 (http://localhost:8080)"

            it "HTTPServerStarted produces expected format" $ do
                renderHTTPTrace HTTPServerStarted `shouldBe` "[HTTP] Server started"

            it "HTTPRequestReceived with auth produces expected format" $ do
                let trace = HTTPRequestReceived{tracePath = "/mcp", traceMethod = "POST", traceHasAuth = True}
                renderHTTPTrace trace `shouldBe` "[HTTP] Request received: POST /mcp (authenticated)"

            it "HTTPRequestReceived without auth produces expected format" $ do
                let trace = HTTPRequestReceived{tracePath = "/health", traceMethod = "GET", traceHasAuth = False}
                renderHTTPTrace trace `shouldBe` "[HTTP] Request received: GET /health (no auth)"

            it "HTTPAuthRequired produces expected format" $ do
                let trace = HTTPAuthRequired{traceAuthPath = "/mcp/protected"}
                renderHTTPTrace trace `shouldBe` "[HTTP] Authentication required for /mcp/protected"

            it "HTTPAuthSuccess produces expected format" $ do
                let trace = HTTPAuthSuccess{traceAuthUserId = "user-abc123"}
                renderHTTPTrace trace `shouldBe` "[HTTP] Authentication successful for user user-abc123"

            it "HTTPAuthFailure produces expected format" $ do
                let trace = HTTPAuthFailure{traceAuthReason = "Token expired"}
                renderHTTPTrace trace `shouldBe` "[HTTP] Authentication failed: Token expired"

            it "HTTPOAuthEnabled produces expected format" $ do
                let trace = HTTPOAuthEnabled{traceAuthEndpoint = "/authorize", traceTokenEndpoint = "/token"}
                renderHTTPTrace trace `shouldBe` "[HTTP] OAuth authentication enabled - Auth: /authorize, Token: /token"

            it "HTTPOAuthProviders produces expected format" $ do
                let trace = HTTPOAuthProviders{traceProviderNames = ["google", "github", "microsoft"]}
                renderHTTPTrace trace `shouldBe` "[HTTP] OAuth providers: google, github, microsoft"

            it "HTTPPKCEEnabled produces expected format" $ do
                renderHTTPTrace HTTPPKCEEnabled `shouldBe` "[HTTP] PKCE enabled (required by MCP spec)"

            it "HTTPResourceParameterDebug with resource produces expected format" $ do
                let trace = HTTPResourceParameterDebug{traceResourceParam = Just "my-api-resource", traceContext = "token exchange"}
                renderHTTPTrace trace `shouldBe` "[HTTP] Resource parameter (token exchange): my-api-resource"

            it "HTTPResourceParameterDebug without resource produces expected format" $ do
                let trace = HTTPResourceParameterDebug{traceResourceParam = Nothing, traceContext = "authorization"}
                renderHTTPTrace trace `shouldBe` "[HTTP] Resource parameter (authorization): not provided"

        describe "OAuthTrace golden outputs (via renderOAuthTrace)" $ do
            it "OAuthClientRegistration produces expected format" $ do
                let trace = OAuthClientRegistration{clientId = "client-xyz", clientName = "My App"}
                renderOAuthTrace trace `shouldBe` "Client registered: client-xyz (My App)"

            it "OAuthAuthorizationRequest with scopes and state produces expected format" $ do
                let trace = OAuthAuthorizationRequest{clientId = "client-123", scopes = ["read", "write"], hasState = True}
                renderOAuthTrace trace `shouldBe` "Authorization request from client-123 for scopes [read, write] (with state)"

            it "OAuthAuthorizationRequest without state produces expected format" $ do
                let trace = OAuthAuthorizationRequest{clientId = "client-456", scopes = [], hasState = False}
                renderOAuthTrace trace `shouldBe` "Authorization request from client-456 for scopes (none)"

            it "OAuthLoginPageServed produces expected format" $ do
                let trace = OAuthLoginPageServed{sessionId = "sess-abc"}
                renderOAuthTrace trace `shouldBe` "Login page served for session sess-abc"

            it "OAuthLoginAttempt success produces expected format" $ do
                let trace = OAuthLoginAttempt{username = "demo", success = True}
                renderOAuthTrace trace `shouldBe` "Login attempt for user demo: SUCCESS"

            it "OAuthLoginAttempt failure produces expected format" $ do
                let trace = OAuthLoginAttempt{username = "admin", success = False}
                renderOAuthTrace trace `shouldBe` "Login attempt for user admin: FAILED"

            it "OAuthAuthorizationGranted produces expected format" $ do
                let trace = OAuthAuthorizationGranted{clientId = "client-789", userId = "user-456"}
                renderOAuthTrace trace `shouldBe` "Authorization granted to client client-789 by user user-456"

            it "OAuthAuthorizationDenied produces expected format" $ do
                let trace = OAuthAuthorizationDenied{clientId = "client-999", reason = "User declined"}
                renderOAuthTrace trace `shouldBe` "Authorization denied for client client-999: User declined"

            it "OAuthTokenExchange success produces expected format" $ do
                let trace = OAuthTokenExchange{grantType = "authorization_code", success = True}
                renderOAuthTrace trace `shouldBe` "Token exchange (authorization_code): SUCCESS"

            it "OAuthTokenExchange failure produces expected format" $ do
                let trace = OAuthTokenExchange{grantType = "refresh_token", success = False}
                renderOAuthTrace trace `shouldBe` "Token exchange (refresh_token): FAILED"

            it "OAuthTokenRefresh success produces expected format" $ do
                let trace = OAuthTokenRefresh{success = True}
                renderOAuthTrace trace `shouldBe` "Token refresh: SUCCESS"

            it "OAuthTokenRefresh failure produces expected format" $ do
                let trace = OAuthTokenRefresh{success = False}
                renderOAuthTrace trace `shouldBe` "Token refresh: FAILED"

            it "OAuthSessionExpired produces expected format" $ do
                let trace = OAuthSessionExpired{sessionId = "sess-expired"}
                renderOAuthTrace trace `shouldBe` "Session expired: sess-expired"

            it "OAuthValidationError produces expected format" $ do
                let trace = OAuthValidationError{errorType = "invalid_request", validationDetail = "Missing redirect_uri"}
                renderOAuthTrace trace `shouldBe` "Validation error [invalid_request]: Missing redirect_uri"

        describe "MCPTrace golden outputs (via renderMCPTrace delegation)" $ do
            it "MCPServer delegates to renderServerTrace" $ do
                let trace = MCPServer (ServerInit{serverName = "test-server", serverVersion = "1.0.0"})
                renderMCPTrace trace `shouldBe` "[Server] Init: test-server v1.0.0"

            it "MCPProtocol delegates to renderProtocolTrace" $ do
                let trace = MCPProtocol (ProtocolRequestReceived{requestId = "msg-1", method = "tools/call"})
                renderMCPTrace trace `shouldBe` "[Protocol] Request received: method='tools/call', id='msg-1'"

            it "MCPStdIO delegates to renderStdIOTrace" $ do
                let trace = MCPStdIO (StdIOMessageReceived{messageSize = 512})
                renderMCPTrace trace `shouldBe` "[StdIO] Message received (512 bytes)"

            it "MCPHttp delegates to renderHTTPTrace" $ do
                let trace = MCPHttp (HTTPRequestReceived{tracePath = "/api/mcp", traceMethod = "POST", traceHasAuth = True})
                renderMCPTrace trace `shouldBe` "[HTTP] Request received: POST /api/mcp (authenticated)"

            it "MCPHttp with nested OAuth delegates correctly" $ do
                let trace = MCPHttp $ HTTPOAuth $ OAuthClientRegistration{clientId = "client-golden", clientName = "Golden Test"}
                renderMCPTrace trace `shouldBe` "[HTTP:OAuth] Client registered: client-golden (Golden Test)"
