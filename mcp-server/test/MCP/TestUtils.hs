{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module:      MCP.TestUtils
Copyright:   (c) DPella AB 2025
License:     MPL-2.0
Maintainer:  <matti@dpella.io>, <lobo@dpella.io>

This module provides utility functions and types for testing the MCP server
implementation.
-}
module MCP.TestUtils where

import Crypto.JOSE.Error as Crypto
import Data.Aeson (ToJSON, Value (..), encode, toJSON)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import MCP.Protocol qualified as MCP
import MCP.TestServer (TestUser (..))
import MCP.Types qualified as MCP
import Network.HTTP.Types qualified as HTTP
import Network.Wai.Test (SResponse)
import Servant.Auth.Server qualified as AuthServer
import Test.Hspec.Wai (
    ResponseMatcher,
    WaiExpectation,
    WaiSession,
    getState,
    liftIO,
    request,
    shouldRespondWith,
 )

-- * Utility Functions for MCP testing

-- ** Test Combinators

-- | Create a post request with the provided headers
postWith ::
    BS.ByteString ->
    [(HTTP.HeaderName, BS.ByteString)] ->
    BSL.ByteString ->
    WaiSession st SResponse
postWith = request HTTP.methodPost

-- | Helper function for making MCP requests with authentication
mcpPostRequest ::
    [(HTTP.HeaderName, BS.ByteString)] -> Value -> WaiSession st SResponse
mcpPostRequest headers request_object =
    postWith "/mcp" headers (encode request_object)

{- | Helper function for making MCP requests with authentication and expecting a
specific response
-}
mcpPostRequestExpects ::
    [(HTTP.HeaderName, BS.ByteString)] ->
    Value ->
    ResponseMatcher ->
    WaiExpectation st
mcpPostRequestExpects headers request_object expected_response =
    mcpPostRequest headers request_object `shouldRespondWith` expected_response

{- | Helper function for making MCP requests with authentication and expecting
200
-}
mcpPostRequestOk :: [(HTTP.HeaderName, BS.ByteString)] -> Value -> WaiExpectation st
mcpPostRequestOk headers request_object =
    mcpPostRequestExpects headers request_object 200

-- | Create an authenticated request with JWT token
withAuthenticatedRequest ::
    ([(HTTP.HeaderName, BS.ByteString)] -> WaiSession AuthServer.JWTSettings a) ->
    WaiSession AuthServer.JWTSettings a
withAuthenticatedRequest = withAuthenticatedRequestForSession "test-user-id"

-- | Create an authenticated request with JWT token
withAuthenticatedRequestForSession ::
    Text -> -- Session ID to include in JWT
    ([(HTTP.HeaderName, BS.ByteString)] -> WaiSession AuthServer.JWTSettings a) ->
    WaiSession AuthServer.JWTSettings a
withAuthenticatedRequestForSession session_id action = do
    jwt_cfg <- getState
    let test_user = TestUser session_id "Test User" "test@example.com"
    eitherToken <- liftIO $ generateTestJWT test_user jwt_cfg
    case eitherToken of
        Left _ -> error "Failed to generate test JWT token"
        Right token -> do
            let tokenStr = BSL.toStrict token
                headers =
                    [ ("Content-Type", "application/json")
                    , ("Authorization", "Bearer " <> tokenStr)
                    ]
            action headers
  where
    -- generate a JWT token for the given user using the provided JWT settings
    generateTestJWT ::
        (AuthServer.ToJWT a) =>
        a ->
        AuthServer.JWTSettings ->
        IO (Either Crypto.Error BSL.ByteString)
    generateTestJWT user jwt_cfg' = AuthServer.makeJWT user jwt_cfg' Nothing

{- | Computes an action after initializing the server (authenticate, initialize,
and send initialized notification)
-}
withInitializedServer ::
    ([(HTTP.HeaderName, BS.ByteString)] -> WaiSession AuthServer.JWTSettings a) ->
    WaiSession AuthServer.JWTSettings a
withInitializedServer action =
    withAuthenticatedRequest $ \headers -> do
        let init_request = toJSON createInitializeRequest
        mcpPostRequestOk headers init_request
        let notify_request = toJSON createInitializedNotification
        mcpPostRequestOk headers notify_request
        action headers

-- ** Request builders

-- | Create a JSON-RPC request object.
createJSONRPCRequest ::
    (ToJSON a) =>
    Maybe Text ->
    a ->
    Text ->
    Value ->
    MCP.JSONRPCRequest
createJSONRPCRequest mb_jsonrpc req_id method params =
    MCP.JSONRPCRequest
        { jsonrpc = fromMaybe "2.0" mb_jsonrpc
        , id = toRequestId req_id
        , method = method
        , params = params
        }

-- | Create a JSON-RPC request ID from an integer
toRequestId :: (ToJSON a) => a -> MCP.RequestId
toRequestId = MCP.RequestId . toJSON

-- | Create a JSON-RPC initialize request
createInitializeRequest :: MCP.InitializeRequest
createInitializeRequest =
    MCP.InitializeRequest
        { id = toRequestId (1 :: Int)
        , params = init_params
        }
  where
    init_params =
        MCP.InitializeParams
            { protocolVersion = MCP.pROTOCOL_VERSION
            , capabilities = client_caps
            , clientInfo = client_info
            }

    client_caps :: MCP.ClientCapabilities
    client_caps =
        MCP.ClientCapabilities
            { roots = Just $ MCP.RootsCapability (Just True)
            , sampling = Nothing
            , elicitation = Just MCP.ElicitationCapability
            , experimental = Nothing
            }

    client_info :: MCP.Implementation
    client_info =
        MCP.Implementation
            { name = "test-client"
            , version = "0.1.0"
            , title = Nothing
            }

-- | Create a JSON-RPC ping request
createPingRequest :: MCP.PingRequest
createPingRequest =
    MCP.PingRequest
        { id = toRequestId (1 :: Int)
        , params = Nothing
        }

-- | Create a JSON-RPC call tool request
createCallToolRequest :: Int -> Text -> [(Text, Value)] -> MCP.CallToolRequest
createCallToolRequest req_id t_name map_args =
    MCP.CallToolRequest
        { id = toRequestId req_id
        , params = tool_call_params
        }
  where
    tool_call_params :: MCP.CallToolParams
    tool_call_params =
        MCP.CallToolParams
            { name = t_name
            , arguments = case map_args of
                [] -> Nothing
                _ -> Just $ Map.fromList map_args
            }

-- | Create a JSON-RPC tools list request
createListToolsRequest :: Int -> MCP.ListToolsRequest
createListToolsRequest int_id =
    MCP.ListToolsRequest
        { id = toRequestId int_id
        , params = Just $ MCP.ListToolsParams Nothing
        }

-- | Create a JSON-RPC resources list request
createListResourcesRequest :: Int -> MCP.ListResourcesRequest
createListResourcesRequest req_id =
    MCP.ListResourcesRequest
        { id = toRequestId req_id
        , params = Just $ MCP.ListResourcesParams Nothing
        }

-- | Create a JSON-RPC resources read request
createReadResourceRequest :: Int -> Text -> MCP.ReadResourceRequest
createReadResourceRequest req_id txt_uri =
    MCP.ReadResourceRequest
        { id = toRequestId req_id
        , params = MCP.ReadResourceParams txt_uri
        }

-- | Create a JSON-RPC resources subscribe request
createSubscribeRequest :: Int -> Text -> MCP.SubscribeRequest
createSubscribeRequest req_id txt_uri =
    MCP.SubscribeRequest
        { id = toRequestId req_id
        , params = MCP.SubscribeParams txt_uri
        }

-- | Create a JSON-RPC list prompts request
createPromptListRequest :: Int -> MCP.ListPromptsRequest
createPromptListRequest req_id =
    MCP.ListPromptsRequest
        { id = toRequestId req_id
        , params = Just $ MCP.ListPromptsParams Nothing
        }

-- | Create a JSON-RPC get prompt request
createGetPromptRequest :: Int -> Text -> Maybe (Map.Map Text Text) -> MCP.GetPromptRequest
createGetPromptRequest req_id p_name args =
    MCP.GetPromptRequest
        { id = toRequestId req_id
        , params = MCP.GetPromptParams p_name args
        }

-- ** Notification helpers

-- | Create a JSON-RPC initialized notification
createInitializedNotification :: MCP.InitializedNotification
createInitializedNotification =
    MCP.InitializedNotification{params = Nothing}

-- | Create a JSON-RPC logging/setLevel request
createSetLevelRequest :: Int -> MCP.LoggingLevel -> MCP.SetLevelRequest
createSetLevelRequest req_id lvl =
    MCP.SetLevelRequest
        { id = toRequestId req_id
        , params = MCP.SetLevelParams lvl
        }

-- | Create a JSON-RPC request for a resource template list
createListResourceTemplatesRequest :: Int -> MCP.ListResourceTemplatesRequest
createListResourceTemplatesRequest req_id =
    MCP.ListResourceTemplatesRequest
        { id = toRequestId req_id
        , params = Just $ MCP.ListResourceTemplatesParams Nothing
        }

-- | Create a JSON-RPC request for completion/complete
createCompleteRequest :: Int -> Text -> Text -> Text -> MCP.CompleteRequest
createCompleteRequest req_id ref_name arg_name arg_value =
    MCP.CompleteRequest
        { id = toRequestId req_id
        , params =
            MCP.CompleteParams
                { ref = MCP.PromptRef $ MCP.PromptReference "ref/prompt" ref_name Nothing
                , argument = MCP.CompletionArgument arg_name arg_value
                , context = Nothing
                }
        }
