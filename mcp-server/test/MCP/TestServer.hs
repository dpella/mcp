{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module:      MCP.TestServer
Copyright:   (c) DPella AB 2025
License:     MPL-2.0
Maintainer:  <matti@dpella.io>, <lobo@dpella.io>

This module provides server configuration utilities for testing the MCP server
implementation.
-}
module MCP.TestServer where

import Control.Concurrent.MVar (MVar, newMVar)
import Data.Aeson (FromJSON, ToJSON, fromJSON, object, toJSON, (.=))
import Data.Aeson qualified as Aeson
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import MCP.Server
import Network.Wai (Application)
import Servant (Context (..), Proxy (..), serveWithContext)
import Servant.Auth.Server qualified as AuthServer

-- * Server Configuration Types and Functions

-- ** Test user

-- | A simple test user for MCP tests
data TestUser = TestUser
    { userId :: Text
    , userName :: Text
    , userEmail :: Text
    }
    deriving (Eq, Show, Generic)

instance ToJSON TestUser

instance FromJSON TestUser

instance AuthServer.ToJWT TestUser

instance AuthServer.FromJWT TestUser

type instance MCPHandlerState = TestStateHandler

type instance MCPHandlerUser = TestUser

-- ** MCP Handler State

-- | A simple state handler for MCP tests
newtype TestStateHandler = TestStateHandler {handle_curr_user :: Maybe Text}

-- | Initialize a fresh test state handler
initializeTestState :: TestStateHandler
initializeTestState = TestStateHandler{handle_curr_user = Nothing}

-- | Initialization of the handler with the current user
mb_handler_init :: Maybe (TestUser -> TestStateHandler -> IO TestStateHandler)
mb_handler_init =
    Just $ \user state -> do
        let usr_id = userId user
        return state{handle_curr_user = Just usr_id}

-- | Finalization of the handler, clearing the current user
mb_handler_finalize :: Maybe (TestStateHandler -> IO TestStateHandler)
mb_handler_finalize =
    Just $ \state -> do
        return state{handle_curr_user = Nothing}

-- ** Process Handlers

-- | Process handlers to be used in tests
processHandlers :: ProcessHandlers
processHandlers =
    defaultProcessHandlers
        { listResourcesHandler = Just processListResourcesHandler
        , readResourceHandler = Just processReadResourceHandler
        , listToolsHandler = Just processListToolsHandler
        , callToolHandler = Just processCallToolHandler
        , listPromptsHandler = Just processListPromptsHandler
        , getPromptHandler = Just processGetPromptHandler
        , listResourceTemplatesHandler = Just processListResourceTemplatesHandler
        , completeHandler = Just processCompleteHandler
        }
  where
    -- Example process handler for listing resources
    processListResourcesHandler :: ListResourcesParams -> MCPServerT (ProcessResult ListResourcesResult)
    processListResourcesHandler =
        return $
            return $
                ProcessSuccess $
                    ListResourcesResult
                        { resources = availableResources
                        , nextCursor = Nothing
                        , _meta = Nothing
                        }

    -- Example process handler for reading a resource
    processReadResourceHandler :: ReadResourceParams -> MCPServerT (ProcessResult ReadResourceResult)
    processReadResourceHandler (ReadResourceParams{uri = req_uri}) = do
        let rsc_content =
                case req_uri of
                    "resource://example/document" ->
                        Just $
                            TextResource $
                                TextResourceContents
                                    { uri = req_uri
                                    , text = csvTestData
                                    , mimeType = Just "text/plain"
                                    , _meta = Nothing
                                    }
                    "resource://example/data" ->
                        Just $
                            TextResource $
                                TextResourceContents
                                    { uri = req_uri
                                    , text = jsonTestData
                                    , mimeType = Just "application/json"
                                    , _meta = Nothing
                                    }
                    "resource://example/image" ->
                        Just $
                            BlobResource $
                                BlobResourceContents
                                    { uri = req_uri
                                    , blob = imageTestData
                                    , mimeType = Just "image/png"
                                    , _meta = Nothing
                                    }
                    _ -> Nothing
        case rsc_content of
            Just rsc_content' ->
                return $ ProcessSuccess (ReadResourceResult{contents = [rsc_content'], _meta = Nothing})
            Nothing ->
                return $ ProcessRPCError 404 "Resource not found"

    -- Example process handler for listing tools
    processListToolsHandler :: ListToolsParams -> MCPServerT (ProcessResult ListToolsResult)
    processListToolsHandler =
        return $
            return $
                ProcessSuccess $
                    ListToolsResult
                        { tools = availableTools
                        , nextCursor = Nothing
                        , _meta = Nothing
                        }

    -- Example process handler for calling tools
    processCallToolHandler :: CallToolParams -> MCPServerT (ProcessResult CallToolResult)
    processCallToolHandler (CallToolParams t_name t_args) = do
        case t_name of
            "addition-tool" -> case t_args of
                Just args_map -> do
                    let parseInt v =
                            case fromJSON @Int v of
                                Aeson.Success i -> Just i
                                Aeson.Error _ -> Nothing
                    let mb_arg1 = Map.lookup "arg1" args_map >>= parseInt
                    let mb_arg2 = Map.lookup "arg2" args_map >>= parseInt
                    case (mb_arg1, mb_arg2) of
                        (Just arg1, Just arg2) -> do
                            let t_result = arg1 + arg2
                            let result_value = Map.fromList [("result", toJSON t_result)]
                            let ctx_result =
                                    TextBlock $ TextContent "text" ("The result is: " <> T.pack (show t_result)) Nothing Nothing
                            return $ ProcessSuccess (CallToolResult [ctx_result] (Just result_value) Nothing Nothing)
                        _ -> return $ ProcessRPCError 400 "addition-tool: Missing or invalid arguments"
                Nothing -> return $ ProcessRPCError 400 "addition-tool: Missing arguments"
            "constant-msg-tool" -> do
                let t_result = "Hello, World!" :: Text
                let result_value = Map.fromList [("result", toJSON t_result)]
                let ctx_result =
                        TextBlock $ TextContent "text" ("This is a constant message: " <> t_result) Nothing Nothing
                return $ ProcessSuccess (CallToolResult [ctx_result] (Just result_value) Nothing Nothing)
            _ -> return $ ProcessRPCError 404 "Tool not found"

    -- Example list prompts handler
    processListPromptsHandler :: ListPromptsParams -> MCPServerT (ProcessResult ListPromptsResult)
    processListPromptsHandler =
        return $
            return $
                ProcessSuccess $
                    ListPromptsResult
                        { prompts = availablePrompts
                        , nextCursor = Nothing
                        , _meta = Nothing
                        }

    -- Example get prompt handler
    processGetPromptHandler :: GetPromptParams -> MCPServerT (ProcessResult GetPromptResult)
    processGetPromptHandler = \case
        GetPromptParams{name = "code-review", arguments = Just map_args} -> do
            let msgs =
                    [
                        ( User
                        , T.unlines $
                            [ "Help me revise the syntax of the following python code snippet:"
                            , "```python"
                            , Map.findWithDefault "" "code" map_args
                            , "```"
                            ]
                        )
                    , (Assistant, "Yes, I can help with that. I will make sure to check common issues and keep practices in mind.")
                    ]
            let toMsg msg_role msg_content =
                    PromptMessage
                        { role = msg_role
                        , content =
                            TextBlock $
                                TextContent
                                    { textType = "text"
                                    , text = msg_content
                                    , annotations = Nothing
                                    , _meta = Nothing
                                    }
                        }
            return $
                ProcessSuccess $
                    GetPromptResult
                        { description = Just "A prompt to request a code review from the LLM"
                        , messages = fmap (uncurry toMsg) msgs
                        , _meta = Nothing
                        }
        _ -> return $ ProcessRPCError 404 "Prompt not found"

    -- Resource templates handler
    processListResourceTemplatesHandler :: ListResourceTemplatesParams -> MCPServerT (ProcessResult ListResourceTemplatesResult)
    processListResourceTemplatesHandler =
        return $
            return $
                ProcessSuccess $
                    ListResourceTemplatesResult
                        { resourceTemplates = availableResourceTemplates
                        , nextCursor = Nothing
                        , _meta = Nothing
                        }

    -- Completion handler: prefix-based completions for the code-review prompt's "code" argument
    processCompleteHandler :: CompleteParams -> MCPServerT (ProcessResult CompleteResult)
    processCompleteHandler (CompleteParams cref (CompletionArgument arg_name arg_value) _ctx) =
        case cref of
            PromptRef (PromptReference _ "code-review" _) ->
                case arg_name of
                    "code" ->
                        let suggestions = filter (T.isPrefixOf arg_value) completionValues
                        in return $
                            ProcessSuccess $
                                CompleteResult
                                    { completion =
                                        CompletionResult
                                            { values = suggestions
                                            , total = Just (length suggestions)
                                            , hasMore = Just False
                                            }
                                    , _meta = Nothing
                                    }
                    _ -> emptyCompletion
            _ -> emptyCompletion
      where
        emptyCompletion =
            return $
                ProcessSuccess $
                    CompleteResult
                        { completion = CompletionResult{values = [], total = Just 0, hasMore = Just False}
                        , _meta = Nothing
                        }

-- | Completion suggestion values for the code-review prompt
completionValues :: [Text]
completionValues = ["def foo():", "class MyClass:", "import os"]

-- ** Available Tools

-- | Tools available in the test server
availableTools :: [Tool]
availableTools =
    [ Tool
        { name = "addition-tool"
        , title = Just "Addition Tool"
        , description = Just "An example tool that adds two numbers"
        , inputSchema =
            InputSchema
                { schemaType = "object"
                , properties =
                    Just $
                        Map.fromList
                            [ ("arg1", object ["type" .= ("number" :: Text)])
                            , ("arg2", object ["type" .= ("number" :: Text)])
                            ]
                , required = Just ["arg1", "arg2"]
                }
        , outputSchema =
            Just $
                InputSchema
                    { schemaType = "object"
                    , properties = Just $ Map.fromList [("result", object ["type" .= ("number" :: Text)])]
                    , required = Just ["result"]
                    }
        , annotations = Nothing
        , _meta = Nothing
        }
    , Tool
        { name = "constant-msg-tool"
        , title = Just "Constant Message Tool"
        , description = Just "Another example tool"
        , inputSchema =
            InputSchema
                { schemaType = "object"
                , properties = Nothing
                , required = Nothing
                }
        , outputSchema =
            Just $
                InputSchema
                    { schemaType = "object"
                    , properties = Just (Map.fromList [("message", object ["type" .= ("string" :: Text)])])
                    , required = Just ["message"]
                    }
        , annotations = Nothing
        , _meta = Nothing
        }
    ]

-- ** Available Prompts

-- | Prompts available in the test server
availablePrompts :: [Prompt]
availablePrompts =
    [ Prompt
        { name = "code-review"
        , title = Just "Code Review Prompt"
        , description = Just "Asks the LLM to analyze code quality and suggest improvements"
        , arguments =
            Just
                [ PromptArgument
                    { name = "code"
                    , title = Just "Code Snippet"
                    , description = Just "The code snippet to review"
                    , required = Just True
                    }
                ]
        , _meta = Nothing
        }
    ]

-- ** Available Resources

-- | Resources available in the test server
availableResources :: [Resource]
availableResources =
    [ Resource
        { uri = "resource://example/document"
        , name = "text-document"
        , title = Just "An example resource for testing"
        , description = Just "This is a sample text document."
        , mimeType = Just "text/plain"
        , size = Nothing
        , annotations = Nothing
        , _meta = Nothing
        }
    , Resource
        { uri = "resource://example/data"
        , name = "json-data"
        , title = Just "Example Resource 2"
        , description = Just "Another example resource for testing"
        , mimeType = Just "application/json"
        , size = Nothing
        , annotations = Nothing
        , _meta = Nothing
        }
    , Resource
        { uri = "resource://example/image"
        , name = "image-resource"
        , title = Just "An inspiring image"
        , description = Just "Image of a sunset"
        , mimeType = Just "image/png"
        , size = Nothing
        , annotations = Nothing
        , _meta = Nothing
        }
    ]

-- ** Available Resource Templates

-- | Resource templates available in the test server
availableResourceTemplates :: [ResourceTemplate]
availableResourceTemplates =
    [ ResourceTemplate
        { name = "user-profile"
        , title = Just "User Profile Template"
        , uriTemplate = "resource://example/users/{userId}"
        , description = Just "Returns a user profile by ID"
        , mimeType = Just "application/json"
        , annotations = Nothing
        , _meta = Nothing
        }
    , ResourceTemplate
        { name = "log-file"
        , title = Just "Log File Template"
        , uriTemplate = "resource://example/logs/{date}"
        , description = Just "Returns logs for a specific date"
        , mimeType = Just "text/plain"
        , annotations = Nothing
        , _meta = Nothing
        }
    ]

-- *** Sample Resource Contents

-- | Sample CSV data for testing
csvTestData :: Text
csvTestData = "name,age,city\nJohn,25,New York\nJane,30,London\nBob,35,Paris"

-- | Sample JSON data for testing
jsonTestData :: Text
jsonTestData =
    T.pack $
        show $
            object ["key1" .= ("value1" :: Text), "key2" .= (42 :: Int), "key3" .= (True :: Bool)]

-- | Sample image data for testing (base64 encoded)
imageTestData :: Text
imageTestData = "<base64-encoded-image-data>"

-- ** Server State Creation

-- | Create test server state for MCP tests
createTestServerState :: IO (MVar MCPServerState)
createTestServerState = do
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
            , mcp_handler_init = mb_handler_init
            , mcp_handler_finalize = mb_handler_finalize
            , mcp_client_capabilities = Nothing
            , mcp_log_level = Just Info -- Set to Debug if you want to see json requests
            , mcp_pending_responses = mempty
            , mcp_pending_responses_next = 1
            , mcp_server_capabilities = server_caps
            , mcp_implementation = impl
            , mcp_instructions = Nothing
            , mcp_process_handlers = processHandlers
            }

-- ** Application Setup

-- | Create test application with access to JWT configuration
createTestAppWithJWT :: IO (AuthServer.JWTSettings, Application)
createTestAppWithJWT = do
    state_var <- createTestServerState
    key <- AuthServer.generateKey
    let jwt_cfg = AuthServer.defaultJWTSettings key
    let cookie_cfg = AuthServer.defaultCookieSettings
    let auth_cfg = cookie_cfg :. jwt_cfg :. EmptyContext
    let app = serveWithContext (Proxy @MCPAPI) auth_cfg (mcpAPI state_var)
    return (jwt_cfg, app)
