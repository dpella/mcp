{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module:      MCP.Integration
License:     MPL-2.0
Maintainer:  <matti@dpella.io>, <lobo@dpella.io>

Integration tests for Model Context Protocol (MCP) server implementation
following the Servant testing cookbook approach
-}
module MCP.Integration where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, object, toJSON, (.=))
import Data.Aeson qualified as Aeson
import Data.Attoparsec.ByteString.Char8 as C
import Data.Attoparsec.ByteString.Lazy as P
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Map qualified as Map
import Data.Text (Text)
import MCP.Protocol
import MCP.TestServer (
    availablePrompts,
    availableResourceTemplates,
    availableResources,
    availableTools,
    completionValues,
    createTestAppWithJWT,
    csvTestData,
    imageTestData,
    jsonTestData,
 )
import MCP.TestUtils
import MCP.Types
import Network.HTTP.Types qualified as HTTP
import Network.Wai.Test (SResponse (..))
import Servant
import Servant.Auth.Server qualified as AuthServer
import Test.Hspec
import Test.Hspec.Wai (
    WaiSession,
    get,
    post,
    request,
    shouldRespondWith,
    withState,
 )

-- * Integration Test Specifications

-- | Complete integration test suite using hspec-wai
integrationSpec :: Spec
integrationSpec = describe "MCP Integration Tests" $ do
    withState createTestAppWithJWT $ do
        serverLifecycleSpec
        authenticationSpec
        protocolFlowSpec
        endpointsSpec
        errorHandlingSpec
        preInitializationSpec
        protocolMethodsSpec
        toolErrorSpec
        resourceErrorSpec
        promptErrorSpec
        resourceTemplateSpec
        completionSpec
        unimplementedHandlerSpec
        nullParamsSpec

-- | Test server startup and basic HTTP behavior
serverLifecycleSpec :: SpecWith (AuthServer.JWTSettings, Application)
serverLifecycleSpec = describe "Server Lifecycle" $ do
    let ping_request =
            toJSON $
                createJSONRPCRequest Nothing (1 :: Int) "ping" Aeson.Null

    it "responds with 401 for unauthenticated GET requests" $
        get "/mcp" `shouldRespondWith` 401

    it "responds with 401 for unauthenticated POST requests" $ do
        let headers = [("Content-Type", "application/json")]
        mcpPostRequestExpects headers ping_request 401

    it "initialization handshake works with proper JWT authentication" $
        withAuthenticatedRequest $ \headers -> do
            -- the client sends the initialize request
            let init_req = toJSON createInitializeRequest
            resp_init <- mcpPostRequest headers init_req
            withValidJSONRPCResponse resp_init 1 validateInitializationResponse

            -- after successful initialization, the client sends notification to
            -- indicate readiness
            let notify_request = toJSON createInitializedNotification
            mcpPostRequestOk headers notify_request

-- | Test JWT authentication scenarios
authenticationSpec :: SpecWith (AuthServer.JWTSettings, Application)
authenticationSpec = describe "Authentication" $ do
    let ping_request = toJSON createPingRequest
    it "rejects requests without JWT token" $ do
        let headers = [("Content-Type", "application/json")]
        mcpPostRequestExpects headers ping_request 401

    it "rejects requests with invalid JWT token" $ do
        let headers =
                [ ("Content-Type", "application/json")
                , ("Authorization", "Bearer invalid-token")
                ]
        mcpPostRequestExpects headers ping_request 401

    it "rejects requests with malformed Authorization header" $ do
        let headers =
                [ ("Content-Type", "application/json")
                , ("Authorization", "NotBearer token")
                ]
        mcpPostRequestExpects headers ping_request 401

-- | Test MCP protocol flows (with proper JWT authentication)
protocolFlowSpec :: SpecWith (AuthServer.JWTSettings, Application)
protocolFlowSpec = describe "Protocol Flow" $ do
    it "rejects authenticated requests with missing jsonrpc field" $ do
        withAuthenticatedRequest $ \headers -> do
            let invalid_request = object ["id" .= (1 :: Int), "method" .= ("ping" :: Text)]
            mcpPostRequestExpects headers invalid_request 400

    it "rejects authenticated request with wrong jsonrpc version" $ do
        withAuthenticatedRequest $ \headers -> do
            let invalid_request =
                    toJSON $
                        createJSONRPCRequest (Just "1.0") (1 :: Int) "ping" Aeson.Null
            mcpPostRequestExpects headers invalid_request 400

    it "rejects authenticated request with wrong id" $ do
        withAuthenticatedRequest $ \headers -> do
            let invalid_id = object ["unexpected" .= ("value" :: Text)]
            let invalid_request =
                    toJSON $
                        createJSONRPCRequest Nothing invalid_id "ping" Aeson.Null
            mcpPostRequestExpects headers invalid_request 400

-- | Test various MCP endpoints to ensure they respond correctly
endpointsSpec :: SpecWith (AuthServer.JWTSettings, Application)
endpointsSpec = describe "Endpoint Health Check" $ do
    it "handles list/tools request successfully" $ do
        withInitializedServer $ \headers -> do
            let req_id = 2
            let expected_tools = ["addition-tool", "constant-msg-tool"]
            let list_tool_req = toJSON $ createListToolsRequest req_id
            -- the client sends the list tools request
            resp_list_tool <- mcpPostRequest headers list_tool_req
            -- verify response's id and listed tools
            withValidJSONRPCResponse resp_list_tool req_id $
                validateToolsListResponse expected_tools

    it "handles tool/call requests successfully" $ do
        withInitializedServer $ \headers -> do
            -- the client sends a tool/call request to the addition tool
            let req_id1 = 3
            let call_req1 =
                    toJSON $
                        createCallToolRequest req_id1 "addition-tool" [("arg1", toJSON (5 :: Int)), ("arg2", toJSON (7 :: Int))]
            resp_call <- mcpPostRequest headers call_req1
            withValidJSONRPCResponse resp_call req_id1 $
                validateToolCallResponse (toJSON (12 :: Int))

            -- the client sends a tool/call request to the constant message tool
            let req_id2 = 4
            let call_req2 =
                    toJSON $
                        createCallToolRequest req_id2 "constant-msg-tool" []
            resp_call2 <- mcpPostRequest headers call_req2
            withValidJSONRPCResponse resp_call2 req_id2 $
                validateToolCallResponse (toJSON ("Hello, World!" :: Text))

    it "handles prompt/list requests successfully" $ do
        withInitializedServer $ \headers -> do
            let req_id = 5
            let expected_prompts = ["code-review"]
            let list_prompt_req = toJSON $ createPromptListRequest req_id
            -- the client sends the list prompts request
            resp_list_prompt <- mcpPostRequest headers list_prompt_req
            -- verify response's id and listed prompts
            withValidJSONRPCResponse resp_list_prompt req_id $
                validatePromptListResponse expected_prompts

    it "handles get/prompt requests successfully" $ do
        withInitializedServer $ \headers -> do
            -- the client sends a get/prompt request for the "code-review" prompt
            let req_id = 6
            let args = Map.fromList [("code", "def foo():\n    return 42")]
            let get_prompt_req = toJSON $ createGetPromptRequest req_id "code-review" (Just args)
            resp_get_prompt <- mcpPostRequest headers get_prompt_req
            -- verify response's id and retrieved prompt
            withValidJSONRPCResponse resp_get_prompt req_id $
                validateGetPromptResponse "A prompt to request a code review from the LLM" [User, Assistant]

    it "handles list/resources requests successfully" $ do
        withInitializedServer $ \headers -> do
            let req_id = 7
            let list_resources_req = toJSON $ createListResourcesRequest req_id
            -- the client sends the list resources request
            resp_list_resources <- mcpPostRequest headers list_resources_req
            -- verify response's id and listed resources
            withValidJSONRPCResponse resp_list_resources req_id $
                validateListResourcesResponse
                    ["resource://example/document", "resource://example/data", "resource://example/image"]

    it "handles read/resource requests successfully" $ do
        withInitializedServer $ \headers -> do
            let requests =
                    [ (8, "resource://example/document", csvTestData)
                    , (9, "resource://example/data", jsonTestData)
                    , (10, "resource://example/image", imageTestData)
                    ]

            mapM_
                ( \(req_id, req_uri, expected_content) -> do
                    let read_resource = toJSON $ createReadResourceRequest req_id req_uri
                    -- the client sends the read resource request
                    resp_read_resource <- mcpPostRequest headers read_resource
                    -- verify response's id and read resource contents
                    withValidJSONRPCResponse resp_read_resource req_id $
                        validateReadResourceResponse req_uri expected_content
                )
                requests

-- | Test error handling scenarios
errorHandlingSpec :: SpecWith (AuthServer.JWTSettings, Application)
errorHandlingSpec = describe "Error Handling" $ do
    it "handles malformed JSON requests" $ do
        let headers = [("Content-Type", "application/json")]
        mcpPostRequestExpects headers "invalid json" 400

    it "handles requests with wrong content type" $
        post "/mcp" "some text" `shouldRespondWith` 415

    it "handles empty requests" $ do
        let headers = [("Content-Type", "application/json")]
        -- should auth fail first (401)?
        mcpPostRequestExpects headers "" 400

    it "handles large requests appropriately" $ do
        withAuthenticatedRequest $ \headers -> do
            let large_value = toJSON $ replicate 1000 ('x' :: Char)
            let large_json =
                    toJSON $
                        createJSONRPCRequest Nothing (1 :: Int) "ping" large_value
            mcpPostRequestOk headers large_json

-- | Test that methods are rejected before initialization
preInitializationSpec :: SpecWith (AuthServer.JWTSettings, Application)
preInitializationSpec = describe "Pre-Initialization Enforcement" $ do
    it "rejects tools/list before initialization" $
        withAuthenticatedRequest $ \headers -> do
            let req = toJSON $ createListToolsRequest 1
            resp <- mcpPostRequest headers req
            withValidJSONRPCErrorResponse resp 1 $ \err_info ->
                code err_info `shouldBe` sERVER_NOT_INITIALIZED

    it "rejects tools/call before initialization" $
        withAuthenticatedRequest $ \headers -> do
            let req = toJSON $ createCallToolRequest 1 "addition-tool" [("arg1", toJSON (1 :: Int)), ("arg2", toJSON (2 :: Int))]
            resp <- mcpPostRequest headers req
            withValidJSONRPCErrorResponse resp 1 $ \err_info ->
                code err_info `shouldBe` sERVER_NOT_INITIALIZED

    it "rejects resources/list before initialization" $
        withAuthenticatedRequest $ \headers -> do
            let req = toJSON $ createListResourcesRequest 1
            resp <- mcpPostRequest headers req
            withValidJSONRPCErrorResponse resp 1 $ \err_info ->
                code err_info `shouldBe` sERVER_NOT_INITIALIZED

    it "rejects prompts/list before initialization" $
        withAuthenticatedRequest $ \headers -> do
            let req = toJSON $ createPromptListRequest 1
            resp <- mcpPostRequest headers req
            withValidJSONRPCErrorResponse resp 1 $ \err_info ->
                code err_info `shouldBe` sERVER_NOT_INITIALIZED

    it "rejects resources/templates/list before initialization" $
        withAuthenticatedRequest $ \headers -> do
            let req = toJSON $ createListResourceTemplatesRequest 1
            resp <- mcpPostRequest headers req
            withValidJSONRPCErrorResponse resp 1 $ \err_info ->
                code err_info `shouldBe` sERVER_NOT_INITIALIZED

    it "rejects completion/complete before initialization" $
        withAuthenticatedRequest $ \headers -> do
            let req = toJSON $ createCompleteRequest 1 "code-review" "code" ""
            resp <- mcpPostRequest headers req
            withValidJSONRPCErrorResponse resp 1 $ \err_info ->
                code err_info `shouldBe` sERVER_NOT_INITIALIZED

    it "allows ping before initialization" $
        withAuthenticatedRequest $ \headers -> do
            let req = toJSON createPingRequest
            mcpPostRequestOk headers req

    it "allows initialize before initialization" $
        withAuthenticatedRequest $ \headers -> do
            let req = toJSON createInitializeRequest
            resp <- mcpPostRequest headers req
            withValidJSONRPCResponse resp 1 validateInitializationResponse

-- | Test various protocol-level methods
protocolMethodsSpec :: SpecWith (AuthServer.JWTSettings, Application)
protocolMethodsSpec = describe "Protocol Methods" $ do
    it "handles ping after initialization" $
        withInitializedServer $ \headers -> do
            let req = toJSON createPingRequest
            mcpPostRequestOk headers req

    it "handles logging/setLevel" $
        withInitializedServer $ \headers -> do
            let req = toJSON $ createSetLevelRequest 2 Debug
            mcpPostRequestOk headers req

    it "returns method_not_found for unknown methods" $
        withInitializedServer $ \headers -> do
            let req =
                    toJSON $
                        createJSONRPCRequest Nothing (2 :: Int) "unknown/method" (object [])
            resp <- mcpPostRequest headers req
            withValidJSONRPCErrorResponse resp 2 $ \err_info ->
                code err_info `shouldBe` mETHOD_NOT_FOUND

    it "handles GET /mcp with valid authentication" $
        withAuthenticatedRequest $ \headers -> do
            let get_headers = filter (\(h, _) -> h /= "Content-Type") headers
            let auth_header = filter (\(h, _) -> h == "Authorization") get_headers
            resp <- getWithHeaders "/mcp" auth_header
            liftIO $ HTTP.statusCode (simpleStatus resp) `shouldBe` 200

-- | Test tool call error scenarios
toolErrorSpec :: SpecWith (AuthServer.JWTSettings, Application)
toolErrorSpec = describe "Tool Call Errors" $ do
    it "returns error for non-existent tool" $
        withInitializedServer $ \headers -> do
            let req = toJSON $ createCallToolRequest 2 "nonexistent-tool" []
            resp <- mcpPostRequest headers req
            withValidJSONRPCErrorResponse resp 2 $ \err_info ->
                code err_info `shouldBe` 404

    it "returns error for missing required arguments" $
        withInitializedServer $ \headers -> do
            -- addition-tool requires arg1 and arg2, call with no args
            let req = toJSON $ createCallToolRequest 2 "addition-tool" []
            resp <- mcpPostRequest headers req
            withValidJSONRPCErrorResponse resp 2 $ \err_info ->
                code err_info `shouldBe` 400

    it "returns error for partially missing required arguments" $
        withInitializedServer $ \headers -> do
            -- addition-tool requires arg1 and arg2, call with only arg1
            let req = toJSON $ createCallToolRequest 2 "addition-tool" [("arg1", toJSON (5 :: Int))]
            resp <- mcpPostRequest headers req
            withValidJSONRPCErrorResponse resp 2 $ \err_info ->
                code err_info `shouldBe` 400

    it "returns error for invalid argument types" $
        withInitializedServer $ \headers -> do
            -- addition-tool expects numbers, pass strings
            let req = toJSON $ createCallToolRequest 2 "addition-tool" [("arg1", toJSON ("not-a-number" :: Text)), ("arg2", toJSON (5 :: Int))]
            resp <- mcpPostRequest headers req
            withValidJSONRPCErrorResponse resp 2 $ \err_info ->
                code err_info `shouldBe` 400

-- | Test resource error scenarios
resourceErrorSpec :: SpecWith (AuthServer.JWTSettings, Application)
resourceErrorSpec = describe "Resource Errors" $ do
    it "returns error for non-existent resource" $
        withInitializedServer $ \headers -> do
            let req = toJSON $ createReadResourceRequest 2 "resource://nonexistent/thing"
            resp <- mcpPostRequest headers req
            withValidJSONRPCErrorResponse resp 2 $ \err_info ->
                code err_info `shouldBe` 404

-- | Test prompt error scenarios
promptErrorSpec :: SpecWith (AuthServer.JWTSettings, Application)
promptErrorSpec = describe "Prompt Errors" $ do
    it "returns error for non-existent prompt" $
        withInitializedServer $ \headers -> do
            let req = toJSON $ createGetPromptRequest 2 "nonexistent-prompt" Nothing
            resp <- mcpPostRequest headers req
            withValidJSONRPCErrorResponse resp 2 $ \err_info ->
                code err_info `shouldBe` 404

-- | Test resource template operations
resourceTemplateSpec :: SpecWith (AuthServer.JWTSettings, Application)
resourceTemplateSpec = describe "Resource Templates" $ do
    it "handles resources/templates/list successfully" $
        withInitializedServer $ \headers -> do
            let req = toJSON $ createListResourceTemplatesRequest 2
            resp <- mcpPostRequest headers req
            withValidJSONRPCResponse resp 2 $ \(ListResourceTemplatesResult{resourceTemplates = templates}) -> do
                let template_names = fmap (\ResourceTemplate{name = n} -> n) templates
                template_names `shouldMatchList` ["user-profile", "log-file"]

    it "handles resources/templates/list with null params" $
        withInitializedServer $ \headers -> do
            let req =
                    toJSON $
                        createJSONRPCRequest Nothing (2 :: Int) "resources/templates/list" Aeson.Null
            resp <- mcpPostRequest headers req
            withValidJSONRPCResponse resp 2 $ \(ListResourceTemplatesResult{resourceTemplates = templates}) ->
                length templates `shouldBe` length availableResourceTemplates

-- | Test completion operations
completionSpec :: SpecWith (AuthServer.JWTSettings, Application)
completionSpec = describe "Completions" $ do
    it "handles completion/complete for known prompt argument" $
        withInitializedServer $ \headers -> do
            let req = toJSON $ createCompleteRequest 2 "code-review" "code" ""
            resp <- mcpPostRequest headers req
            withValidJSONRPCResponse resp 2 $ \(CompleteResult{completion = CompletionResult{values = vals}}) ->
                vals `shouldBe` completionValues

    it "handles completion/complete with prefix filter" $
        withInitializedServer $ \headers -> do
            let req = toJSON $ createCompleteRequest 2 "code-review" "code" "def"
            resp <- mcpPostRequest headers req
            withValidJSONRPCResponse resp 2 $ \(CompleteResult{completion = CompletionResult{values = vals}}) ->
                vals `shouldBe` ["def foo():"]

    it "returns empty completions for unknown prompt" $
        withInitializedServer $ \headers -> do
            let req = toJSON $ createCompleteRequest 2 "nonexistent" "arg" ""
            resp <- mcpPostRequest headers req
            withValidJSONRPCResponse resp 2 $ \(CompleteResult{completion = CompletionResult{values = vals}}) ->
                vals `shouldBe` []

    it "returns empty completions for unknown argument" $
        withInitializedServer $ \headers -> do
            let req = toJSON $ createCompleteRequest 2 "code-review" "nonexistent-arg" ""
            resp <- mcpPostRequest headers req
            withValidJSONRPCResponse resp 2 $ \(CompleteResult{completion = CompletionResult{values = vals}}) ->
                vals `shouldBe` []

-- | Test methods whose handlers are not configured
unimplementedHandlerSpec :: SpecWith (AuthServer.JWTSettings, Application)
unimplementedHandlerSpec = describe "Unimplemented Handler Methods" $ do
    it "returns method_not_found for resources/subscribe" $
        withInitializedServer $ \headers -> do
            let req = toJSON $ createSubscribeRequest 2 "resource://example/document"
            resp <- mcpPostRequest headers req
            withValidJSONRPCErrorResponse resp 2 $ \err_info ->
                code err_info `shouldBe` mETHOD_NOT_FOUND

-- | Test that null params work for list methods
nullParamsSpec :: SpecWith (AuthServer.JWTSettings, Application)
nullParamsSpec = describe "Null Params Handling" $ do
    it "handles tools/list with null params" $
        withInitializedServer $ \headers -> do
            let req =
                    toJSON $
                        createJSONRPCRequest Nothing (2 :: Int) "tools/list" Aeson.Null
            resp <- mcpPostRequest headers req
            withValidJSONRPCResponse resp 2 $ \(ListToolsResult{tools = ls_tools}) ->
                length ls_tools `shouldBe` length availableTools

    it "handles resources/list with null params" $
        withInitializedServer $ \headers -> do
            let req =
                    toJSON $
                        createJSONRPCRequest Nothing (2 :: Int) "resources/list" Aeson.Null
            resp <- mcpPostRequest headers req
            withValidJSONRPCResponse resp 2 $ \(ListResourcesResult{resources = ls_resources}) ->
                length ls_resources `shouldBe` length availableResources

    it "handles prompts/list with null params" $
        withInitializedServer $ \headers -> do
            let req =
                    toJSON $
                        createJSONRPCRequest Nothing (2 :: Int) "prompts/list" Aeson.Null
            resp <- mcpPostRequest headers req
            withValidJSONRPCResponse resp 2 $ \(ListPromptsResult{prompts = ls_prompts}) ->
                length ls_prompts `shouldBe` length availablePrompts

    it "handles resources/templates/list with null params" $
        withInitializedServer $ \headers -> do
            let req =
                    toJSON $
                        createJSONRPCRequest Nothing (2 :: Int) "resources/templates/list" Aeson.Null
            resp <- mcpPostRequest headers req
            withValidJSONRPCResponse resp 2 $ \(ListResourceTemplatesResult{resourceTemplates = templates}) ->
                length templates `shouldBe` length availableResourceTemplates

    it "rejects resources/read with null params" $
        withInitializedServer $ \headers -> do
            let req =
                    toJSON $
                        createJSONRPCRequest Nothing (2 :: Int) "resources/read" Aeson.Null
            resp <- mcpPostRequest headers req
            withValidJSONRPCErrorResponse resp 2 $ \err_info ->
                code err_info `shouldBe` iNVALID_PARAMS

-- * Helper Functions

-- ** Validation

{- | Validates that the JSON-RPC response can be parsed and applies the given
properties to the parsed result
-}
withValidJSONRPCResponse ::
    (FromJSON a) =>
    SResponse ->
    Int ->
    (a -> Expectation) ->
    WaiSession AuthServer.JWTSettings ()
withValidJSONRPCResponse resp expected_id props = do
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

-- | Validates that the initialization response conforms to the protocol
validateInitializationResponse :: InitializeResult -> Expectation
validateInitializationResponse InitializeResult{protocolVersion = init_protocol_version} = do
    init_protocol_version `shouldBe` pROTOCOL_VERSION

-- | Validates that the tools list response contains the expected tools
validateToolsListResponse :: [Text] -> ListToolsResult -> Expectation
validateToolsListResponse expected_tools = \case
    (ListToolsResult{tools = ls_tools}) -> do
        let tools_names = fmap (\(Tool{name = t_name}) -> t_name) ls_tools
        tools_names `shouldMatchList` expected_tools

-- | Validates that the tool call response contains the expected result
validateToolCallResponse :: Aeson.Value -> CallToolResult -> Expectation
validateToolCallResponse expected_result = \case
    (CallToolResult{structuredContent = structured}) -> do
        case structured of
            Just map_results ->
                Map.lookup "result" map_results `shouldBe` Just expected_result
            Nothing -> expectationFailure "Expected structured content in tool call result"

-- | Validates that the prompt list response contains the expected prompts
validatePromptListResponse :: [Text] -> ListPromptsResult -> Expectation
validatePromptListResponse expected_prompts = \case
    (ListPromptsResult{prompts = ls_prompts}) -> do
        let prompt_names = fmap (\(Prompt{name = p_name}) -> p_name) ls_prompts
        prompt_names `shouldMatchList` expected_prompts

{- | Validates that the get prompt response contains the expected description
and messages with correct roles
-}
validateGetPromptResponse :: Text -> [Role] -> GetPromptResult -> Expectation
validateGetPromptResponse expected_description expected_roles = \case
    (GetPromptResult{description = p_description, messages = p_messages}) -> do
        p_description `shouldBe` Just expected_description
        length p_messages `shouldBe` length expected_roles
        fmap (\(PromptMessage{role = m_role}) -> m_role) p_messages
            `shouldSatisfy` all (`elem` expected_roles)

-- | Validates that the list resources response is as expected
validateListResourcesResponse :: [Text] -> ListResourcesResult -> Expectation
validateListResourcesResponse expected_resources = \case
    (ListResourcesResult{resources = ls_resources}) -> do
        let resource_uris = fmap (\(Resource{uri = r_uri}) -> r_uri) ls_resources
        resource_uris `shouldMatchList` expected_resources

-- | Validates that reading a resource response is as expected
validateReadResourceResponse :: Text -> Text -> ReadResourceResult -> Expectation
validateReadResourceResponse expected_uri expected_content = \case
    (ReadResourceResult{contents = r_contents}) -> do
        let uris_contents =
                fmap
                    ( \case
                        TextResource (TextResourceContents{uri = r_uri, text = r_content}) -> (r_uri, r_content)
                        BlobResource (BlobResourceContents{uri = r_uri, blob = r_content}) -> (r_uri, r_content)
                    )
                    r_contents
        case uris_contents of
            [(res_uri, res_content)] -> do
                res_uri `shouldBe` expected_uri
                res_content `shouldBe` expected_content
            _ -> expectationFailure $ "Expected exactly 1 resource, got " <> show (length uris_contents)

{- | Validates that the JSON-RPC error response can be parsed and applies
the given properties to the parsed error info
-}
withValidJSONRPCErrorResponse ::
    SResponse ->
    Int ->
    (JSONRPCErrorInfo -> Expectation) ->
    WaiSession AuthServer.JWTSettings ()
withValidJSONRPCErrorResponse resp expected_id props = do
    liftIO $
        case parseJSONRPCErrorResponse resp of
            Right (JSONRPCError rpc_vrs req_id err_info) -> do
                rpc_vrs `shouldBe` rPC_VERSION
                req_id `shouldBe` toRequestId expected_id
                props err_info
            Left err_msg -> expectationFailure err_msg

-- | Helper for making GET requests with specific headers
getWithHeaders ::
    BS.ByteString ->
    [(HTTP.HeaderName, BS.ByteString)] ->
    WaiSession st SResponse
getWithHeaders path headers =
    Test.Hspec.Wai.request HTTP.methodGet path headers ""

-- ** Parsing

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

{- | Extract JSON data from Server-Sent Events (SSE) format
SSE format: "event: message\ndata:{json}\n\n"
-}
extractSSEData :: LBS.ByteString -> Maybe BS.ByteString
extractSSEData sseBody = snd <$> P.maybeResult (P.parse parseEvent sseBody)
  where
    parseEvent = do
        _ <- P.string "event:"
        msg_type <- P.takeWhileIncluding (not . C.isEndOfLine)
        _ <- C.string "data:"
        json_data <- P.takeWhile (not . C.isEndOfLine)
        return (msg_type, json_data)
