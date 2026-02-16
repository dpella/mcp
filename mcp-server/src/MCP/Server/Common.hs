{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module:      MCP.Server.Common
License:     MPL-2.0
Maintainer:  <matti@dpella.io>, <lobo@dpella.io>

Transport-agnostic core of the MCP server.

This module provides the shared types, state management, request routing,
and tool framework used by both the HTTP and stdio transports.
-}
module MCP.Server.Common (
    -- * Processing results
    ProcessResult (..),

    -- * Server monad
    MCPServerT,

    -- * Handlers
    ProcessHandlers (..),
    defaultProcessHandlers,

    -- * Tool helpers
    ToolHandler (..),
    toolHandler,
    withToolHandlers,
    toolTextResult,
    toolTextError,

    -- * Server state
    MCPServerState (..),
    initMCPServerState,

    -- * Type families
    MCPHandlerState,
    MCPHandlerUser,

    -- * Core processing (transport-agnostic)
    processMethod,
    recurReplaceMeta,
    isValidRequestId,

    -- * Re-exports
    module MCP.Protocol,
    module MCP.Types,
) where

import Control.Concurrent.MVar (MVar)
import Control.Monad.Except
import Control.Monad.State.Lazy
import Data.Aeson (Value, fromJSON, object, toJSON)
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import MCP.Protocol
import MCP.Types

{- | Result of processing an MCP request.

Distinguishes between server errors, RPC protocol errors,
and successful responses.
-}
data ProcessResult a where
    -- | Internal server error
    ProcessServerError :: Text -> ProcessResult a
    -- | JSON-RPC error with code and message
    ProcessRPCError :: Int -> Text -> ProcessResult a
    -- | Successful result
    ProcessSuccess :: a -> ProcessResult a
    {- | Request additional information from the client before completing.

    This allows a handler to send a JSON-RPC request back to the client
    (e.g. @sampling\/createMessage@ or @elicitation\/create@) and resume
    processing once the client responds.

    Supported on both transports:

    * __HTTP__: the request is yielded as an SSE event; the server blocks
    (via 'MVar') until the client POSTs a matching response.
    * __Stdio__: the request is written to stdout and the response is read
    synchronously from stdin (blocks until the client responds).
    -}
    ProcessClientInput ::
        -- | JSON-RPC method name for the client request
        Text ->
        -- | Parameters for the client request
        Aeson.Value ->
        -- | Continuation that receives the client's response
        (Aeson.Value -> MCPServerT (ProcessResult a)) ->
        ProcessResult a

instance Functor ProcessResult where
    fmap f (ProcessSuccess a) = ProcessSuccess (f a)
    fmap _ (ProcessServerError err) = ProcessServerError err
    fmap _ (ProcessRPCError code msg) = ProcessRPCError code msg
    fmap f (ProcessClientInput mthd param cont) =
        ProcessClientInput mthd param $ \res -> do
            result <- cont res
            return $ fmap f result

{- | Monad transformer for MCP server operations.

Provides access to the MCP server state and IO operations,
with the state available to the handlers.
-}
type MCPServerT a = ExceptT Text (StateT MCPServerState IO) a

{- | Handlers for MCP protocol methods.

Each handler is optional, allowing servers to implement only
the capabilities they need. Handlers run in the MCPServerT monad
with access to server state and IO.
-}
data ProcessHandlers
    = ProcessHandlers
    { listResourcesHandler :: Maybe (ListResourcesParams -> MCPServerT (ProcessResult ListResourcesResult))
    -- ^ List available resources
    , readResourceHandler :: Maybe (ReadResourceParams -> MCPServerT (ProcessResult ReadResourceResult))
    -- ^ Read a specific resource
    , listToolsHandler :: Maybe (ListToolsParams -> MCPServerT (ProcessResult ListToolsResult))
    -- ^ List available tools
    , callToolHandler :: Maybe (CallToolParams -> MCPServerT (ProcessResult CallToolResult))
    -- ^ Execute a tool
    , listPromptsHandler :: Maybe (ListPromptsParams -> MCPServerT (ProcessResult ListPromptsResult))
    -- ^ List prompt templates
    , getPromptHandler :: Maybe (GetPromptParams -> MCPServerT (ProcessResult GetPromptResult))
    -- ^ Get a specific prompt
    , listResourceTemplatesHandler ::
        Maybe (ListResourceTemplatesParams -> MCPServerT (ProcessResult ListResourceTemplatesResult))
    -- ^ List resource templates
    , completeHandler :: Maybe (CompleteParams -> MCPServerT (ProcessResult CompleteResult))
    -- ^ Provide completions
    , subscribeHandler :: Maybe (SubscribeParams -> MCPServerT (ProcessResult Result))
    -- ^ Subscribe to resource updates
    , unsubscribeHandler :: Maybe (UnsubscribeParams -> MCPServerT (ProcessResult Result))
    -- ^ Unsubscribe from resource updates
    }

-- | A ToolHandler, including the name, description, and input schema.
data ToolHandler = ToolHandler
    { tool_name :: Text
    , tool_description :: Maybe Text
    , tool_title :: Maybe Text
    , tool_input :: InputSchema
    , tool_output :: Maybe InputSchema
    , tool_annotations :: Maybe ToolAnnotations
    , tool_meta :: Maybe Metadata
    , tool_handler :: Maybe (Map Text Value) -> MCPServerT (ProcessResult CallToolResult)
    }

-- | Construct a tool handler with the required fields
toolHandler ::
    Text ->
    Maybe Text ->
    InputSchema ->
    (Maybe (Map Text Value) -> MCPServerT (ProcessResult CallToolResult)) ->
    ToolHandler
toolHandler name desc input handler =
    ToolHandler
        { tool_name = name
        , tool_description = desc
        , tool_title = Nothing
        , tool_input = input
        , tool_output = Nothing
        , tool_annotations = Nothing
        , tool_meta = Nothing
        , tool_handler = handler
        }

-- | Create the raw process handlers from a list of tool handlers.
withToolHandlers :: [ToolHandler] -> ProcessHandlers -> ProcessHandlers
withToolHandlers ths ph =
    ph
        { listToolsHandler = Just list_handler
        , callToolHandler = Just call_handler
        }
  where
    tool_handler_to_tool :: ToolHandler -> Tool
    tool_handler_to_tool ToolHandler{..} =
        Tool
            { name = tool_name
            , description = tool_description
            , title = tool_title
            , inputSchema = tool_input
            , outputSchema = tool_output
            , annotations = tool_annotations
            , _meta = tool_meta
            }
    list_handler = const $ do
        return $
            ProcessSuccess $
                ListToolsResult
                    { tools = fmap tool_handler_to_tool ths
                    , nextCursor = Nothing
                    , _meta = Nothing
                    }
    tool_map :: Map Text ToolHandler
    tool_map = Map.fromList $ fmap (\th -> (tool_name th, th)) ths

    missing_required :: Set Text -> MCPServerT (ProcessResult CallToolResult)
    missing_required reqs = return $ ProcessRPCError iNVALID_PARAMS $ "Missing required arguments: " <> T.intercalate ", " (Set.toList reqs)

    call_handler :: CallToolParams -> MCPServerT (ProcessResult CallToolResult)
    call_handler CallToolParams{name = t_name, arguments = args} =
        case Map.lookup t_name tool_map of
            Nothing -> return $ ProcessRPCError mETHOD_NOT_FOUND ("Tool not found: " <> t_name)
            Just ToolHandler{tool_input = InputSchema{required = required}, tool_handler = handler} ->
                -- Handle missing arguments
                case (args, required) of
                    (_, Nothing) -> handler args
                    (Nothing, Just req) | not $ null req -> missing_required (Set.fromList req)
                    (Nothing, Just _) -> handler args
                    (Just provided, Just req)
                        | p_keys <- Map.keysSet provided
                        , missing <- Set.fromList req Set.\\ p_keys
                        , not (Set.null missing) ->
                            missing_required missing
                    (Just _provided, _) -> handler args

-- | A tool result containing text content.
toolTextResult :: [Text] -> CallToolResult
toolTextResult res =
    CallToolResult
        { content = map toTContent res
        , isError = Just False
        , _meta = Nothing
        , structuredContent = Nothing
        }
  where
    toTContent :: Text -> ContentBlock
    toTContent =
        TextBlock . \t -> TextContent{textType = "text", text = t, annotations = Nothing, _meta = Nothing}

-- | A tool result containing text content.
toolTextError :: Text -> CallToolResult
toolTextError err =
    CallToolResult
        { content = [TextBlock $ TextContent{textType = "text", text = err, annotations = Nothing, _meta = Nothing}]
        , isError = Just True
        , _meta = Nothing
        , structuredContent = Nothing
        }

{- | Default process handlers with no implementations.

Use this as a starting point and override specific handlers
as needed for your MCP server implementation.
-}
defaultProcessHandlers :: ProcessHandlers
defaultProcessHandlers =
    ProcessHandlers
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing

{- | Initialize MCP server state with given options and capabilities.

Creates a new server state with:
* The initial handler state
* Server capabilities declaration
* Implementation metadata
* Optional instructions for clients
* Custom process handlers
-}
initMCPServerState ::
    -- | Initial handler state
    MCPHandlerState ->
    -- | Initialize handler state when the client sends @initialize@
    Maybe (MCPHandlerUser -> MCPHandlerState -> IO MCPHandlerState) ->
    -- | Finalize handler state after each request
    Maybe (MCPHandlerState -> IO MCPHandlerState) ->
    -- | Server capabilities to advertise to clients
    ServerCapabilities ->
    -- | Server name and version metadata
    Implementation ->
    -- | Optional instructions for clients
    Maybe Text ->
    -- | Handlers for MCP methods
    ProcessHandlers ->
    MCPServerState
initMCPServerState init_state handler_init handler_finalize =
    MCPServerState False init_state handler_init handler_finalize Nothing (Just Warning) IM.empty 0

{- | Type family used to configure the handler state threaded through 'MCPServerT'.
Users must provide a type instance before using the MCP server, e.g.
@type instance MCPHandlerState = MyState@.
-}
type family MCPHandlerState

{- | Type family used to configure the user type for JWT authentication.
Users must provide a type instance before using the MCP server, e.g.
@type instance MCPHandlerUser = MyUser@.
-}
type family MCPHandlerUser

{- | MCP server state maintained across requests.

Contains all information needed to process MCP requests including
initialization status, handler state, and capability declarations.
-}
data MCPServerState = MCPServerState
    { mcp_server_initialized :: Bool
    -- ^ Whether initialize has been called
    , mcp_handler_state :: MCPHandlerState
    -- ^ Current handler state for this session
    , mcp_handler_init :: Maybe (MCPHandlerUser -> MCPHandlerState -> IO MCPHandlerState)
    -- ^ Initialize the handler state on server initialization
    , mcp_handler_finalize :: Maybe (MCPHandlerState -> IO MCPHandlerState)
    -- ^ Finalize the handler state after each request
    , mcp_client_capabilities :: Maybe ClientCapabilities
    -- ^ Client's declared capabilities
    , mcp_log_level :: Maybe LoggingLevel
    -- ^ Log level
    , mcp_pending_responses :: IntMap (MVar Aeson.Value)
    -- ^ Pending responses from the client
    , mcp_pending_responses_next :: Int
    -- ^ Next pending response ID
    , mcp_server_capabilities :: ServerCapabilities
    -- ^ Server's declared capabilities
    , mcp_implementation :: Implementation
    -- ^ Server implementation metadata
    , mcp_instructions :: Maybe Text
    -- ^ Optional instructions for clients
    , mcp_process_handlers :: ProcessHandlers
    -- ^ Handlers for MCP methods
    }

{- | Normalize @_meta@ fields in JSON-RPC responses.
Promotes the contents of @_meta@ objects up one level and removes the @_meta@ key.
-}
recurReplaceMeta :: Aeson.Value -> Aeson.Value
recurReplaceMeta (Aeson.Array arr) =
    Aeson.Array $ fmap recurReplaceMeta arr
recurReplaceMeta o@(Aeson.Object _)
    | Aeson.Object obj' <- replaceMeta o =
        Aeson.Object $ fmap recurReplaceMeta obj'
recurReplaceMeta v = v

-- | Replace @_meta@ at the top level of an object
replaceMeta :: Aeson.Value -> Aeson.Value
replaceMeta (Aeson.Object obj)
    | Just (Aeson.Object meta) <- KM.lookup "_meta" obj =
        Aeson.Object $ KM.delete "_meta" obj `KM.union` meta
replaceMeta v = v

-- | Check if a RequestId is valid according to JSON-RPC 2.0 spec.
isValidRequestId :: RequestId -> Bool
isValidRequestId (RequestId v) =
    case v of
        Aeson.Number _ -> True
        Aeson.String _ -> True
        Aeson.Null -> True
        _ -> False

-- | Methods that can be called without initialization
allowedWithoutInitialization :: Set Text
allowedWithoutInitialization = Set.fromList ["ping", "initialize"]

-- | Methods that are allowed without parameters (null params -> empty object)
allowedWithoutParams :: Set Text
allowedWithoutParams =
    Set.fromList
        [ "resources/list"
        , "resources/templates/list"
        , "tools/list"
        , "prompts/list"
        ]

{- | Route an incoming JSON-RPC method to the appropriate handler.

This is the transport-agnostic core of the MCP server, used by both
the HTTP and stdio transports.  It enforces initialization order
(only @\"ping\"@ and @\"initialize\"@ are accepted before the server
has been initialized) and parses method parameters before delegating
to the 'ProcessHandlers'.
-}
processMethod :: Bool -> Text -> Aeson.Value -> StateT MCPServerState IO (ProcessResult Aeson.Value)
processMethod False mthd _ | not $ mthd `Set.member` allowedWithoutInitialization = server_not_initialized
processMethod _ mthd Aeson.Null | mthd `Set.member` allowedWithoutParams = processMethod True mthd (object [])
processMethod _ "resources/list" mb_arg = runProcessHandler listResourcesHandler mb_arg
processMethod _ "resources/read" mb_arg = runProcessHandler readResourceHandler mb_arg
processMethod _ "resources/subscribe" mb_arg = runProcessHandler subscribeHandler mb_arg
processMethod _ "resources/unsubscribe" mb_arg = runProcessHandler unsubscribeHandler mb_arg
processMethod _ "tools/list" mb_arg = runProcessHandler listToolsHandler mb_arg
processMethod _ "tools/call" mb_arg = runProcessHandler callToolHandler mb_arg
processMethod _ "prompts/list" mb_arg = runProcessHandler listPromptsHandler mb_arg
processMethod _ "prompts/get" mb_arg = runProcessHandler getPromptHandler mb_arg
processMethod _ "resources/templates/list" mb_arg = runProcessHandler listResourceTemplatesHandler mb_arg
processMethod _ "completion/complete" mb_arg = runProcessHandler completeHandler mb_arg
processMethod _ "ping" _ = return $ ProcessSuccess (toJSON $ object [])
processMethod _ "logging/setLevel" p
    | Aeson.Error e <- fromJSON @SetLevelParams p = invalid_params e
processMethod _ "logging/setLevel" p
    | Aeson.Success (SetLevelParams{level = level}) <- fromJSON @SetLevelParams p = do
        modify $ \s -> s{mcp_log_level = Just level}
        return $ ProcessSuccess Aeson.Null
processMethod _ "initialize" p
    | Aeson.Error e <- fromJSON @InitializeParams p = invalid_params e
processMethod _ "initialize" p
    | Aeson.Success (InitializeParams{capabilities = capabilities}) <- fromJSON @InitializeParams p = do
        MCPServerState{..} <- get
        modify $ \s ->
            s
                { mcp_server_initialized = True
                , mcp_client_capabilities = Just capabilities
                }
        let result =
                InitializeResult
                    { protocolVersion = pROTOCOL_VERSION
                    , capabilities = mcp_server_capabilities
                    , serverInfo = mcp_implementation
                    , instructions = mcp_instructions
                    , _meta = Nothing
                    }
        return $ ProcessSuccess $ toJSON result
processMethod _ _ _ = method_not_found

-- | Standard JSON-RPC error responses
missing_params :: StateT MCPServerState IO (ProcessResult Aeson.Value)
missing_params = return $ ProcessRPCError iNVALID_PARAMS "Missing params"

invalid_params :: String -> StateT MCPServerState IO (ProcessResult Aeson.Value)
invalid_params = return . ProcessRPCError iNVALID_PARAMS . ("Invalid params: " <>) . T.pack

server_not_initialized :: StateT MCPServerState IO (ProcessResult Aeson.Value)
server_not_initialized = return $ ProcessRPCError sERVER_NOT_INITIALIZED "Server not initialized"

method_not_found :: StateT MCPServerState IO (ProcessResult Aeson.Value)
method_not_found = return $ ProcessRPCError mETHOD_NOT_FOUND "Method not found"

-- | Helper to run a process handler with argument parsing
runProcessHandler ::
    forall a b.
    (Aeson.FromJSON a, Aeson.ToJSON b) =>
    (ProcessHandlers -> Maybe (a -> MCPServerT (ProcessResult b))) ->
    Aeson.Value ->
    StateT MCPServerState IO (ProcessResult Aeson.Value)
runProcessHandler _ Aeson.Null = missing_params
runProcessHandler handler arg_value =
    case fromJSON @a arg_value of
        Aeson.Error err -> invalid_params err
        Aeson.Success arg -> do
            handlerImpl <- gets (handler . mcp_process_handlers)
            case handlerImpl of
                Just impl -> do
                    impl_res <- runExceptT $ impl arg
                    case impl_res of
                        Left err -> return $ ProcessServerError err
                        Right res -> return $ fmap toJSON res
                Nothing -> method_not_found
