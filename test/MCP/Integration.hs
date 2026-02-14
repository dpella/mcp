{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module:      MCP.Integration
-- Copyright:   (c) DPella AB 2025
-- License:     LicenseRef-AllRightsReserved
-- Maintainer:  <matti@dpella.io>, <lobo@dpella.io>
--
-- Integration tests for Model Context Protocol (MCP) server implementation
-- following the Servant testing cookbook approach
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
  availableResources,
  availableTools,
  createTestAppWithJWT,
  csvTestData,
  imageTestData,
  jsonTestData,
 )
import MCP.TestUtils
import MCP.Types
import Network.Wai.Test (SResponse (..))
import Servant
import Servant.Auth.Server qualified as AuthServer
import Test.Hspec
import Test.Hspec.Wai (
  WaiSession,
  get,
  post,
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
      let expected_tools = fmap (\Tool{name = t_name} -> t_name) availableTools
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
      let expected_prompts = fmap (\Prompt{name = p_name} -> p_name) availablePrompts
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
        validateListResourcesResponse (fmap (\Resource{uri = r_uri} -> r_uri) availableResources)

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

-- * Helper Functions

-- ** Validation

-- | Validates that the JSON-RPC response can be parsed and applies the given
-- properties to the parsed result
withValidJSONRPCResponse
  :: (FromJSON a)
  => SResponse
  -> Int
  -> (a -> Expectation)
  -> WaiSession AuthServer.JWTSettings ()
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
    -- Ensure all expected tools are present
    length ls_tools `shouldBe` length expected_tools
    mapM_ (`shouldSatisfy` (`elem` tools_names)) expected_tools

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
    -- Ensure all expected prompts are present
    length ls_prompts `shouldBe` length expected_prompts
    mapM_ (`shouldSatisfy` (`elem` expected_prompts)) prompt_names

-- | Validates that the get prompt response contains the expected description
-- and messages with correct roles
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
    -- Add your validation logic here based on the expected structure of resources
    length ls_resources `shouldBe` length expected_resources
    mapM_ (`shouldSatisfy` (`elem` expected_resources)) resource_uris

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

-- | Extract JSON data from Server-Sent Events (SSE) format
-- SSE format: "event: message\ndata:{json}\n\n"
extractSSEData :: LBS.ByteString -> Maybe BS.ByteString
extractSSEData sseBody = snd <$> P.maybeResult (P.parse parseEvent sseBody)
  where
    parseEvent = do
      _ <- P.string "event:"
      msg_type <- P.takeWhileIncluding (not . C.isEndOfLine)
      _ <- C.string "data:"
      json_data <- P.takeWhile (not . C.isEndOfLine)
      return (msg_type, json_data)
