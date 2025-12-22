{-# LANGUAGE OverloadedStrings #-}

{- HLINT ignore "Avoid partial function" -}

{- |
Module      : Trace.GoldenSpec
Description : Golden tests for trace rendering output format
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT

Tests for SC-005: Golden tests for render output.
Verifies that render functions produce expected formatted output for representative examples.
-}
module Trace.GoldenSpec (spec) where

import Data.Maybe (fromJust)
import MCP.Trace.HTTP (HTTPTrace (..), renderHTTPTrace)
import MCP.Trace.Protocol (ProtocolTrace (..), renderProtocolTrace)
import MCP.Trace.Server (ServerTrace (..), renderServerTrace)
import MCP.Trace.StdIO (StdIOTrace (..), renderStdIOTrace)
import MCP.Trace.Types (MCPTrace (..), renderMCPTrace)
import Servant.OAuth2.IDP.Auth.Backend (mkUsername)
import Servant.OAuth2.IDP.Errors (ValidationError (..))
import Servant.OAuth2.IDP.Trace (DenialReason (..), OAuthTrace (..), OperationResult (..), renderOAuthTrace)
import Servant.OAuth2.IDP.Types (OAuthGrantType (..), mkClientId, mkRedirectUri, mkScope, mkSessionId)
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
            it "TraceClientRegistration produces expected format" $ do
                let redirectUri = fromJust $ mkRedirectUri "http://localhost/callback"
                    trace = TraceClientRegistration (fromJust $ mkClientId "client-xyz") redirectUri
                renderOAuthTrace trace `shouldBe` "Client registered: client-xyz (http://localhost/callback)"

            it "TraceAuthorizationRequest with scopes produces expected format" $ do
                let trace = TraceAuthorizationRequest (fromJust $ mkClientId "client-123") [fromJust $ mkScope "read", fromJust $ mkScope "write"] Success
                renderOAuthTrace trace `shouldBe` "Authorization request from client-123 for scopes [read, write]: SUCCESS"

            it "TraceAuthorizationRequest without scopes produces expected format" $ do
                let trace = TraceAuthorizationRequest (fromJust $ mkClientId "client-456") [] Failure
                renderOAuthTrace trace `shouldBe` "Authorization request from client-456 for scopes (none): FAILED"

            it "TraceLoginPageServed produces expected format" $ do
                let trace = TraceLoginPageServed (fromJust $ mkSessionId "12345678-1234-5678-1234-567812345678")
                renderOAuthTrace trace `shouldBe` "Login page served for session 12345678-1234-5678-1234-567812345678"

            it "TraceLoginAttempt success produces expected format" $ do
                let trace = TraceLoginAttempt (fromJust $ mkUsername "demo") Success
                renderOAuthTrace trace `shouldBe` "Login attempt for user demo: SUCCESS"

            it "TraceLoginAttempt failure produces expected format" $ do
                let trace = TraceLoginAttempt (fromJust $ mkUsername "admin") Failure
                renderOAuthTrace trace `shouldBe` "Login attempt for user admin: FAILED"

            it "TraceAuthorizationGranted produces expected format" $ do
                let trace = TraceAuthorizationGranted (fromJust $ mkClientId "client-789") (fromJust $ mkUsername "user-456")
                renderOAuthTrace trace `shouldBe` "Authorization granted to client client-789 by user user-456"

            it "TraceAuthorizationDenied produces expected format" $ do
                let trace = TraceAuthorizationDenied (fromJust $ mkClientId "client-999") UserDenied
                renderOAuthTrace trace `shouldBe` "Authorization denied for client client-999: User denied"

            it "TraceTokenExchange success produces expected format" $ do
                let trace = TraceTokenExchange OAuthAuthorizationCode Success
                renderOAuthTrace trace `shouldBe` "Token exchange (authorization_code): SUCCESS"

            it "TraceTokenExchange failure produces expected format" $ do
                let trace = TraceTokenExchange OAuthClientCredentials Failure
                renderOAuthTrace trace `shouldBe` "Token exchange (client_credentials): FAILED"

            it "TraceTokenRefresh success produces expected format" $ do
                let trace = TraceTokenRefresh Success
                renderOAuthTrace trace `shouldBe` "Token refresh: SUCCESS"

            it "TraceTokenRefresh failure produces expected format" $ do
                let trace = TraceTokenRefresh Failure
                renderOAuthTrace trace `shouldBe` "Token refresh: FAILED"

            it "TraceSessionExpired produces expected format" $ do
                let trace = TraceSessionExpired (fromJust $ mkSessionId "87654321-4321-8765-4321-876543218765")
                renderOAuthTrace trace `shouldBe` "Session expired: 87654321-4321-8765-4321-876543218765"

            it "TraceValidationError produces expected format" $ do
                let redirectUri = fromJust $ mkRedirectUri "https://wrong.com/callback"
                    trace = TraceValidationError (RedirectUriMismatch (fromJust $ mkClientId "client-1") redirectUri)
                renderOAuthTrace trace `shouldBe` "Validation error: Redirect URI mismatch for client client-1: https://wrong.com/callback"

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
                let redirectUri = fromJust $ mkRedirectUri "http://localhost/callback"
                    trace = MCPHttp $ HTTPOAuth $ TraceClientRegistration (fromJust $ mkClientId "client-golden") redirectUri
                renderMCPTrace trace `shouldBe` "[HTTP:OAuth] Client registered: client-golden (http://localhost/callback)"
