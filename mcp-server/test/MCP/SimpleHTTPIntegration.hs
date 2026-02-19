{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module:      MCP.SimpleHTTPIntegration
License:     MPL-2.0
Maintainer:  <matti@dpella.io>, <lobo@dpella.io>

Integration tests for the SimpleHTTP transport.
-}
module MCP.SimpleHTTPIntegration where

import Control.Concurrent.MVar (MVar, newMVar)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, toJSON)
import Data.Aeson qualified as Aeson
import Data.Attoparsec.ByteString.Char8 as C
import Data.Attoparsec.ByteString.Lazy as P
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.IntMap qualified as IM
import Data.Map qualified as Map
import MCP.Protocol
import MCP.Server.Common
import MCP.Server.HTTP (simpleHttpApp)
import MCP.TestServer (
    availablePrompts,
    availableResourceTemplates,
    availableResources,
    availableTools,
    initializeTestState,
    mb_handler_finalize,
    processHandlers,
 )
import MCP.TestUtils
import Network.HTTP.Types qualified as HTTP
import Network.Wai (Application)
import Network.Wai.Test (SResponse (..))
import Test.Hspec
import Test.Hspec.Wai (
    WaiSession,
    get,
    shouldRespondWith,
    withState,
 )

-- ---------------------------------------------------------------------------
-- Test application setup
-- ---------------------------------------------------------------------------

-- | Create a test application using SimpleHTTP (no auth)
createTestApp :: IO ((), Application)
createTestApp = do
    state_var <- createSimpleHTTPTestState
    let app = simpleHttpApp state_var
    return ((), app)

-- | Create test server state for SimpleHTTP (no handler_init, same as Stdio)
createSimpleHTTPTestState :: IO (MVar MCPServerState)
createSimpleHTTPTestState = do
    let impl = Implementation "test-server" "1.0.0" Nothing
    let server_caps =
            ServerCapabilities
                { logging = Just LoggingCapability
                , prompts = Just (PromptsCapability{listChanged = Nothing})
                , resources =
                    Just
                        (ResourcesCapability{listChanged = Nothing, subscribe = Nothing})
                , tools = Just (ToolsCapability{listChanged = Just True})
                , completions = Just CompletionsCapability
                , experimental = Nothing
                }
    newMVar
        MCPServerState
            { mcp_server_initialized = False
            , mcp_handler_state = initializeTestState
            , mcp_handler_init = Nothing -- no user type in SimpleHTTP
            , mcp_handler_finalize = mb_handler_finalize
            , mcp_client_capabilities = Nothing
            , mcp_log_level = Just Info
            , mcp_pending_responses = IM.empty
            , mcp_pending_responses_next = 1
            , mcp_server_capabilities = server_caps
            , mcp_implementation = impl
            , mcp_instructions = Nothing
            , mcp_process_handlers = processHandlers
            }

-- ---------------------------------------------------------------------------
-- Test specifications
-- ---------------------------------------------------------------------------

-- | Complete SimpleHTTP integration test suite
simpleHTTPIntegrationSpec :: Spec
simpleHTTPIntegrationSpec = describe "SimpleHTTP Integration Tests" $ do
    withState createTestApp $ do
        protocolFlowSpec
        endpointsSpec
        preInitializationSpec

-- ---------------------------------------------------------------------------
-- Protocol flow tests
-- ---------------------------------------------------------------------------

protocolFlowSpec :: SpecWith ((), Application)
protocolFlowSpec = describe "Protocol Flow" $ do
    it "initialization handshake works" $ do
        let init_req = toJSON createInitializeRequest
        resp_init <- mcpPostRequest headers init_req
        withValidSimpleHTTPResponse resp_init 1 validateInitializationResponse

        let notify_request = toJSON createInitializedNotification
        mcpPostRequestOk headers notify_request

    it "rejects request with wrong jsonrpc version" $ do
        let invalid_request =
                toJSON $
                    createJSONRPCRequest (Just "1.0") (1 :: Int) "ping" Aeson.Null
        mcpPostRequestExpects headers invalid_request 400

    it "accepts GET /mcp" $
        get "/mcp" `shouldRespondWith` 200

-- ---------------------------------------------------------------------------
-- Endpoint tests
-- ---------------------------------------------------------------------------

endpointsSpec :: SpecWith ((), Application)
endpointsSpec = describe "Endpoints" $ do
    it "handles tools/list request" $
        withInitializedSimpleHTTP $ do
            let req = toJSON $ createListToolsRequest 2
            resp <- mcpPostRequest headers req
            withValidSimpleHTTPResponse resp 2 $ \(ListToolsResult{tools = ls_tools}) ->
                length ls_tools `shouldBe` length availableTools

    it "handles tools/call request" $
        withInitializedSimpleHTTP $ do
            let req = toJSON $ createCallToolRequest 3 "addition-tool" [("arg1", toJSON (5 :: Int)), ("arg2", toJSON (7 :: Int))]
            resp <- mcpPostRequest headers req
            withValidSimpleHTTPResponse resp 3 $ \(CallToolResult{structuredContent = structured}) ->
                case structured of
                    Just map_results ->
                        Map.lookup "result" map_results `shouldBe` Just (toJSON (12 :: Int))
                    Nothing -> expectationFailure "Expected structured content"

    it "handles resources/list request" $
        withInitializedSimpleHTTP $ do
            let req = toJSON $ createListResourcesRequest 4
            resp <- mcpPostRequest headers req
            withValidSimpleHTTPResponse resp 4 $ \(ListResourcesResult{resources = ls_resources}) ->
                length ls_resources `shouldBe` length availableResources

    it "handles prompts/list request" $
        withInitializedSimpleHTTP $ do
            let req = toJSON $ createPromptListRequest 5
            resp <- mcpPostRequest headers req
            withValidSimpleHTTPResponse resp 5 $ \(ListPromptsResult{prompts = ls_prompts}) ->
                length ls_prompts `shouldBe` length availablePrompts

    it "handles resources/templates/list request" $
        withInitializedSimpleHTTP $ do
            let req = toJSON $ createListResourceTemplatesRequest 6
            resp <- mcpPostRequest headers req
            withValidSimpleHTTPResponse resp 6 $ \(ListResourceTemplatesResult{resourceTemplates = templates}) ->
                length templates `shouldBe` length availableResourceTemplates

    it "handles ping after initialization" $
        withInitializedSimpleHTTP $ do
            let req = toJSON createPingRequest
            mcpPostRequestOk headers req

-- ---------------------------------------------------------------------------
-- Pre-initialization tests
-- ---------------------------------------------------------------------------

preInitializationSpec :: SpecWith ((), Application)
preInitializationSpec = describe "Pre-Initialization Enforcement" $ do
    it "rejects tools/list before initialization" $ do
        let req = toJSON $ createListToolsRequest 1
        resp <- mcpPostRequest headers req
        withValidSimpleHTTPErrorResponse resp 1 $ \err_info ->
            code err_info `shouldBe` sERVER_NOT_INITIALIZED

    it "allows ping before initialization" $ do
        let req = toJSON createPingRequest
        mcpPostRequestOk headers req

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Standard headers for SimpleHTTP requests (no auth needed)
headers :: [(HTTP.HeaderName, BS.ByteString)]
headers = [("Content-Type", "application/json")]

-- | Initialize server then run action
withInitializedSimpleHTTP :: WaiSession () a -> WaiSession () a
withInitializedSimpleHTTP f = do
    let init_request = toJSON createInitializeRequest
    mcpPostRequestOk headers init_request
    let notify_request = toJSON createInitializedNotification
    mcpPostRequestOk headers notify_request
    f

-- | Parse and validate a SimpleHTTP JSON-RPC response
withValidSimpleHTTPResponse ::
    (FromJSON a) =>
    SResponse ->
    Int ->
    (a -> Expectation) ->
    WaiSession () ()
withValidSimpleHTTPResponse resp expected_id props = do
    liftIO $
        case parseJSONRPCResponse resp of
            Right (JSONRPCResponse rpc_vrs req_id json_result) -> do
                case Aeson.fromJSON json_result of
                    Aeson.Error err -> expectationFailure $ "Failed to parse result: " <> err
                    Aeson.Success val -> do
                        rpc_vrs `shouldBe` rPC_VERSION
                        req_id `shouldBe` toRequestId expected_id
                        props val
            Left err_msg -> expectationFailure err_msg

-- | Parse and validate a SimpleHTTP JSON-RPC error response
withValidSimpleHTTPErrorResponse ::
    SResponse ->
    Int ->
    (JSONRPCErrorInfo -> Expectation) ->
    WaiSession () ()
withValidSimpleHTTPErrorResponse resp expected_id props = do
    liftIO $
        case parseJSONRPCErrorResponse resp of
            Right (JSONRPCError rpc_vrs req_id err_info) -> do
                rpc_vrs `shouldBe` rPC_VERSION
                req_id `shouldBe` toRequestId expected_id
                props err_info
            Left err_msg -> expectationFailure err_msg

-- | Validates initialization response
validateInitializationResponse :: InitializeResult -> Expectation
validateInitializationResponse InitializeResult{protocolVersion = init_protocol_version} = do
    init_protocol_version `shouldBe` pROTOCOL_VERSION

-- ** SSE parsing (same as Integration.hs)

-- | Parse JSON-RPC response from SSE response body
parseJSONRPCResponse :: SResponse -> Either String JSONRPCResponse
parseJSONRPCResponse resp =
    case extractSSEData (simpleBody resp) of
        Just bs_response ->
            case Aeson.decodeStrict bs_response of
                Just json_resp -> Right json_resp
                Nothing -> Left $ "Failed to decode JSON-RPC response from: " <> show bs_response
        Nothing -> Left $ "Failed to extract JSON data from SSE response: " <> show (simpleBody resp)

-- | Parse JSON-RPC error response from SSE response body
parseJSONRPCErrorResponse :: SResponse -> Either String JSONRPCError
parseJSONRPCErrorResponse resp =
    case extractSSEData (simpleBody resp) of
        Just bs_response ->
            case Aeson.decodeStrict bs_response of
                Just json_err -> Right json_err
                Nothing -> Left $ "Failed to decode JSON-RPC error from: " <> show bs_response
        Nothing -> Left $ "Failed to extract JSON data from SSE response: " <> show (simpleBody resp)

-- | Extract JSON data from SSE format
extractSSEData :: LBS.ByteString -> Maybe BS.ByteString
extractSSEData sseBody = snd <$> P.maybeResult (P.parse parseEvent sseBody)
  where
    parseEvent = do
        _ <- P.string "event:"
        msg_type <- P.takeWhileIncluding (not . C.isEndOfLine)
        _ <- C.string "data:"
        json_data <- P.takeWhile (not . C.isEndOfLine)
        return (msg_type, json_data)
