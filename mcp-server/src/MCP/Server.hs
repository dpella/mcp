{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module:      MCP.Server
Copyright:   (c) DPella AB 2025
License:     MPL-2.0
Maintainer:  <matti@dpella.io>, <lobo@dpella.io>

Model Context Protocol (MCP) server implementation for DPella.

This module implements the MCP protocol, allowing AI assistants and other
tools to interact with DPella through a standardized JSON-RPC interface.

The MCP server provides:
* Resource access (read files, databases, etc.)
* Tool execution (run DPella commands)
* Prompt templates
* Completion support

Authentication is handled via JWT tokens in the Authorization header.
The server maintains a REPL state for each authenticated session.
-}
module MCP.Server (
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

    -- * Stdio transport
    serveStdio,

    -- * Core processing (transport-agnostic)
    processMethod,
    recurReplaceMeta,

    -- * Servant API
    MCPAPI,
    mcpAPI,
    handleMCPRequest,
    handleMCPEvents,

    -- * Re-exports
    module MCP.Protocol,
    module MCP.Types,
) where

import Control.Concurrent.MVar
import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.State.Lazy
import Data.IORef
import Data.Aeson (Value, encode, fromJSON, object, toJSON, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Tuple (swap)
import System.IO (BufferMode (..), Handle, hFlush, hIsEOF, hSetBuffering)
import MCP.Protocol
import MCP.Types
import Network.HTTP.Media ((//))
import Servant
import Servant.Auth.Server (Auth, AuthResult (..), JWT)
import Servant.Auth.Server qualified as AuthServer
import Servant.Types.SourceT
import Servant.Types.SourceT qualified as Source

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
    -- | Ask for more information
    ProcessClientInput ::
        -- | Method name
        Text ->
        -- | Parameters
        Aeson.Value ->
        -- | Continuation
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
    missing_required reqs = return $ ProcessRPCError iNVALID_PARAMS $ "Missing required arguments: " <> T.pack (show reqs)

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

The REPL state is configured to collect output for returning to clients.
-}
initMCPServerState ::
    -- | initial handler state
    MCPHandlerState ->
    -- | Initialize state on server initialization
    Maybe (MCPHandlerUser -> MCPHandlerState -> IO MCPHandlerState) ->
    -- | Finalize state after each request
    Maybe (MCPHandlerState -> IO MCPHandlerState) ->
    ServerCapabilities ->
    Implementation ->
    Maybe Text ->
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
DPella configuration, initialization status, REPL state, and
capability declarations.
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

-- | Frame for JSON-RPC messages.
data JSONRPCFrame

instance FramingRender JSONRPCFrame where
    framingRender _ f = fmap (\x -> f x <> "\n")

-- | Event for JSON-RPC messages.
data JSONRPCEvent

instance Accept JSONRPCEvent where
    contentType _ = "text" // "event-stream"

instance MimeRender JSONRPCEvent JSONRPCMessage where
    mimeRender _ val =
        ("event: message\n" <>) $
            ("data:" <> encode (toJSON val) <> "\n")

{- | Servant API type for the MCP endpoint.

Accepts JSON-RPC requests with JWT authentication via servant-auth.
All MCP methods are multiplexed through this single endpoint.
-}
type MCPAPI =
    "mcp"
        :> Auth '[JWT] MCPHandlerUser
        -- \^ JWT authentication using servant-auth
        :> ReqBody '[JSON] JSONRPCMessage
        -- \^ JSON-RPC request
        :> StreamPost JSONRPCFrame JSONRPCEvent (SourceIO JSONRPCMessage)
        -- \^ Stream of JSON-RPC responses
        :<|> "mcp"
            :> Auth '[JWT] MCPHandlerUser
            :> StreamGet JSONRPCFrame JSONRPCEvent (SourceIO JSONRPCMessage)

-- | Type of the MCP API
mcpAPI :: MVar MCPServerState -> Server MCPAPI
mcpAPI state_var =
    handleMCPRequest state_var
        :<|> handleMCPEvents

-- | This handles the Get /mcp requests. Does nothing for now except authenticate.
handleMCPEvents ::
    AuthResult MCPHandlerUser ->
    Handler (SourceIO JSONRPCMessage)
handleMCPEvents auth_result = do
    case auth_result of
        AuthServer.NoSuchUser -> throwError err401{errBody = "Invalid authentication credentials"}
        AuthServer.BadPassword -> throwError err401{errBody = "Authentication failed"}
        AuthServer.Indefinite -> throwError err401{errBody = "Authentication error"}
        AuthServer.Authenticated _ -> return $ fromStepT $ Stop

{- | Handle incoming MCP JSON-RPC requests.

This is the main entry point for MCP requests. It:

1. Validates authentication via servant-auth (automatically handled)
2. Parses the JSON-RPC request
3. Routes to the appropriate handler based on method name
4. Manages server state updates
5. Returns properly formatted JSON-RPC responses

The server enforces initialization before accepting most methods.
Only "ping" and "initialize" can be called before initialization.
-}
handleMCPRequest ::
    MVar MCPServerState ->
    AuthResult MCPHandlerUser ->
    JSONRPCMessage ->
    Handler (SourceIO JSONRPCMessage)
handleMCPRequest state_var auth_result request_value =
    fromStepT <$> do
        case auth_result of
            AuthServer.NoSuchUser -> mcpAuthError "Invalid authentication credentials"
            AuthServer.BadPassword -> mcpAuthError "Authentication failed"
            AuthServer.Indefinite -> mcpAuthError "Authentication error"
            AuthServer.Authenticated auth_user -> do
                MCPServerState
                    { mcp_server_initialized = server_initialized
                    , mcp_log_level = log_level
                    } <-
                    liftIO $ readMVar state_var
                case log_level of
                    Nothing -> return ()
                    Just level -> when (level >= Debug) $
                        liftIO $ BSL.putStrLn $ "[request] " <> encode request_value

                case request_value of
                    NotificationMessage _ ->
                        return $
                            flip Yield Stop $
                                NotificationMessage $
                                    JSONRPCNotification rPC_VERSION "ok" Aeson.Null
                    ErrorMessage err -> do
                        throwError err400{errBody = encode err}
                    -- Here we fill in the MVar for any pending responses from the client
                    ResponseMessage (JSONRPCResponse _ (RequestId req_id) result) -> do
                        cur_st <- liftIO $ readMVar state_var
                        case Aeson.fromJSON req_id of
                            Aeson.Error err -> return (Source.Error $ "Invalid request ID: " <> err)
                            Aeson.Success req_index ->
                                case IM.lookup req_index (mcp_pending_responses cur_st) of
                                    Nothing ->
                                        return $ Source.Error "Server received unexpected response"
                                    Just mvar -> liftIO $ do
                                        modifyMVar_ state_var $ \cur_st' ->
                                            return cur_st'{mcp_pending_responses = IM.delete req_index (mcp_pending_responses cur_st')}
                                        putMVar mvar result
                                        return Stop
                    RequestMessage (JSONRPCRequest jsonrpc req_id method params) -> do
                        -- Validate JSON-RPC version
                        let bs_rpc_version = BSL.fromStrict (encodeUtf8 rPC_VERSION)
                        when (jsonrpc /= rPC_VERSION) $
                            throwError err400{errBody = "Invalid jsonrpc version, must be '" <> bs_rpc_version <> "'"}

                        -- Validate request ID
                        when (not $ isValidRequestId req_id) $
                            throwError err400{errBody = "Invalid request ID, must be string, number, or null"}

                        -- Initialize handler state on first initialize request
                        when (not server_initialized && method == "initialize") $
                            liftIO $
                                modifyMVar_ state_var $
                                    \cur_st@MCPServerState
                                        { mcp_handler_state = handler_st
                                        , mcp_handler_init = mb_handler_init
                                        } -> case mb_handler_init of
                                            Nothing -> return cur_st
                                            Just handler_init -> do
                                                h_st' <- handler_init auth_user handler_st
                                                return cur_st{mcp_handler_state = h_st'}

                        -- Process the request routing to the appropriate handler
                        res <- liftIO $ modifyMVar state_var $ fmap swap <$> runStateT (processMethod server_initialized method params)

                        -- Log the response if debug level
                        cur_log_level <- liftIO $ mcp_log_level <$> readMVar state_var
                        case cur_log_level of
                            Just level | level >= Debug -> liftIO $ do
                                let prefix = "[response] " <> BSL.fromStrict (encodeUtf8 method) <> " -> "
                                let body = case res of
                                        ProcessSuccess val -> encode val
                                        ProcessRPCError code msg -> encode $ object ["error" .= msg, "code" .= code]
                                        ProcessServerError err -> encode $ object ["serverError" .= err]
                                        ProcessClientInput{} -> "client-input"
                                BSL.putStrLn $ prefix <> body
                            _ -> return ()

                        -- Get the final result to return
                        final_result <- liftIO $ handleProcessResult req_id res

                        -- Finalize handler state after use
                        liftIO $
                            modifyMVar_ state_var $
                                \st@MCPServerState
                                    { mcp_handler_state = handler_st
                                    , mcp_handler_finalize = mb_finalizer
                                    } -> do
                                        case mb_finalizer of
                                            Nothing -> return st
                                            Just handler_finalize -> do
                                                -- Call the finalizer to clean up the handler state
                                                handler_st' <- handler_finalize handler_st
                                                -- Update the state with the finalized handler state
                                                let st' = st{mcp_handler_state = handler_st'}
                                                -- Return the updated state
                                                return st'
                        return final_result
  where
    -- Handle the result of processing a request
    handleProcessResult ::
        (Aeson.ToJSON a) =>
        RequestId ->
        ProcessResult a ->
        IO (StepT IO JSONRPCMessage)
    handleProcessResult req_id =
        \case
            ProcessServerError err ->
                return (Source.Error $ T.unpack err)
            ProcessRPCError code msg ->
                return $
                    flip Yield Stop $
                        ErrorMessage $
                            JSONRPCError rPC_VERSION req_id $
                                JSONRPCErrorInfo code msg Nothing
            ProcessClientInput ci_mthd ci_params ci_cont -> do
                mvar <- liftIO newEmptyMVar
                r_id <- liftIO $ modifyMVar state_var $ \cur_st -> do
                    let pr = mcp_pending_responses cur_st
                    let pr_id = mcp_pending_responses_next cur_st
                    let pr' = IM.insert pr_id mvar pr
                    let pr_id' = pr_id + 1
                    return
                        ( cur_st{mcp_pending_responses = pr', mcp_pending_responses_next = pr_id'}
                        , pr_id
                        )
                let msg =
                        RequestMessage $
                            JSONRPCRequest rPC_VERSION (RequestId $ Aeson.Number $ fromIntegral r_id) ci_mthd ci_params
                return $
                    Yield msg $
                        Effect $ do
                            -- Wait for the response
                            ci_resp <- liftIO $ takeMVar mvar
                            -- Make sure to use the current updated state
                            cur_st <- liftIO $ readMVar state_var
                            (result, cur_st') <-
                                liftIO $
                                    flip runStateT cur_st $
                                        runExceptT $
                                            ci_cont ci_resp
                            liftIO $ modifyMVar_ state_var $ \_ -> return cur_st'
                            case result of
                                Left err -> do
                                    return $ Source.Error $ T.unpack err
                                Right response ->
                                    handleProcessResult req_id response
            ProcessSuccess response -> do
                return $
                    flip Yield Stop $
                        ResponseMessage $
                            JSONRPCResponse rPC_VERSION req_id (recurReplaceMeta $ toJSON response)

    -- Helper to throw unauthorized errors
    mcpAuthError :: Text -> Handler (StepT IO JSONRPCMessage)
    mcpAuthError err = throwError err401{errBody = encode $ object ["error" .= err]}

{- | Run the MCP server over stdio transport.

Reads JSON-RPC messages line-by-line from the input handle,
processes them through the standard MCP message handler, and writes
JSON-RPC responses line-by-line to the output handle.

Stdio transport does not use JWT authentication; it is assumed that
the process boundary provides authentication.

The server runs until EOF is reached on the input handle.
-}
serveStdio ::
    -- | Input handle (typically stdin)
    Handle ->
    -- | Output handle (typically stdout)
    Handle ->
    -- | Initial server state
    MCPServerState ->
    IO ()
serveStdio h_in h_out initial_state = do
    hSetBuffering h_in LineBuffering
    hSetBuffering h_out LineBuffering
    state_ref <- newIORef initial_state
    loop state_ref
  where
    loop :: IORef MCPServerState -> IO ()
    loop state_ref = do
        eof <- hIsEOF h_in
        if eof
            then return ()
            else do
                line <- BS.hGetLine h_in
                case Aeson.eitherDecodeStrict' line of
                    Left err -> do
                        -- JSON parse error — write error response with null id
                        writeMsg $
                            ErrorMessage $
                                JSONRPCError rPC_VERSION (RequestId Aeson.Null) $
                                    JSONRPCErrorInfo pARSE_ERROR (T.pack err) Nothing
                        loop state_ref
                    Right msg -> do
                        processStdioMessage state_ref msg
                        loop state_ref

    processStdioMessage :: IORef MCPServerState -> JSONRPCMessage -> IO ()
    processStdioMessage state_ref = \case
        NotificationMessage _ ->
            -- Notifications don't produce a response
            return ()
        ErrorMessage _ ->
            -- Client sent an error — nothing to respond to
            return ()
        ResponseMessage _ ->
            -- Unexpected response from client outside of ProcessClientInput
            return ()
        RequestMessage (JSONRPCRequest jsonrpc req_id method params) -> do
            -- Validate JSON-RPC version
            if jsonrpc /= rPC_VERSION
                then
                    writeMsg $
                        ErrorMessage $
                            JSONRPCError rPC_VERSION req_id $
                                JSONRPCErrorInfo iNVALID_REQUEST "Invalid jsonrpc version" Nothing
                else do
                    -- Validate request ID
                    if not (isValidRequestId req_id)
                        then
                            writeMsg $
                                ErrorMessage $
                                    JSONRPCError rPC_VERSION req_id $
                                        JSONRPCErrorInfo iNVALID_REQUEST "Invalid request ID" Nothing
                        else do
                            cur_st <- readIORef state_ref
                            let initialized = mcp_server_initialized cur_st

                            -- Process the request
                            (res, new_st) <- runStateT (processMethod initialized method params) cur_st
                            writeIORef state_ref new_st

                            -- Handle ProcessClientInput by synchronous read/write
                            final_res <- resolveClientInput state_ref res

                            -- Convert result to response message
                            case final_res of
                                ProcessServerError err ->
                                    writeMsg $
                                        ErrorMessage $
                                            JSONRPCError rPC_VERSION req_id $
                                                JSONRPCErrorInfo iNTERNAL_ERROR err Nothing
                                ProcessRPCError rpc_code rpc_msg ->
                                    writeMsg $
                                        ErrorMessage $
                                            JSONRPCError rPC_VERSION req_id $
                                                JSONRPCErrorInfo rpc_code rpc_msg Nothing
                                ProcessSuccess response ->
                                    writeMsg $
                                        ResponseMessage $
                                            JSONRPCResponse rPC_VERSION req_id (recurReplaceMeta response)
                                ProcessClientInput{} ->
                                    -- Should not happen after resolveClientInput
                                    writeMsg $
                                        ErrorMessage $
                                            JSONRPCError rPC_VERSION req_id $
                                                JSONRPCErrorInfo iNTERNAL_ERROR "Unresolved client input" Nothing

                            -- Finalize handler state
                            modifyIORef' state_ref $ \st ->
                                case mcp_handler_finalize st of
                                    Nothing -> st
                                    Just _ -> st -- finalize needs IO, handle below
                            st <- readIORef state_ref
                            case mcp_handler_finalize st of
                                Nothing -> return ()
                                Just finalizer -> do
                                    h_st' <- finalizer (mcp_handler_state st)
                                    writeIORef state_ref st{mcp_handler_state = h_st'}

    -- | Resolve ProcessClientInput by writing a request to the client and
    -- reading the response synchronously from stdin.
    resolveClientInput :: IORef MCPServerState -> ProcessResult Aeson.Value -> IO (ProcessResult Aeson.Value)
    resolveClientInput state_ref = \case
        ProcessClientInput ci_mthd ci_params ci_cont -> do
            -- Assign a request ID
            st <- readIORef state_ref
            let r_id = mcp_pending_responses_next st
            writeIORef state_ref st{mcp_pending_responses_next = r_id + 1}

            -- Write the server-to-client request
            writeMsg $
                RequestMessage $
                    JSONRPCRequest rPC_VERSION (RequestId $ Aeson.Number $ fromIntegral r_id) ci_mthd ci_params

            -- Read the client's response synchronously
            resp_line <- BS.hGetLine h_in
            case Aeson.eitherDecodeStrict' @JSONRPCMessage resp_line of
                Right (ResponseMessage (JSONRPCResponse _ _ result)) -> do
                    -- Run the continuation with the client's response
                    cur_st <- readIORef state_ref
                    (cont_result, new_st) <- runStateT (runExceptT $ ci_cont result) cur_st
                    writeIORef state_ref new_st
                    case cont_result of
                        Left err -> return $ ProcessServerError err
                        Right next_res -> resolveClientInput state_ref next_res
                Right (ErrorMessage (JSONRPCError _ _ (JSONRPCErrorInfo _ err_msg _))) ->
                    return $ ProcessServerError err_msg
                _ ->
                    return $ ProcessServerError "Expected response to client input request"
        other -> return other

    writeMsg :: JSONRPCMessage -> IO ()
    writeMsg msg = do
        BSL.hPut h_out (encode msg)
        BSL.hPut h_out "\n"
        hFlush h_out

-- | Normalize @_meta@ fields in JSON-RPC responses.
-- Promotes the contents of @_meta@ objects up one level and removes the @_meta@ key.
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
allowedWithoutParams :: Text -> Bool
allowedWithoutParams = T.isSuffixOf "/list"

-- | Main processing function routing JSON-RPC methods to handlers.
-- This is the transport-agnostic core of the MCP server.
processMethod :: Bool -> Text -> Aeson.Value -> StateT MCPServerState IO (ProcessResult Aeson.Value)
processMethod False mthd _ | not $ mthd `Set.member` allowedWithoutInitialization = server_not_initialized
processMethod _ mthd Aeson.Null | allowedWithoutParams mthd = processMethod True mthd (object [])
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
server_not_initialized = return $ ProcessRPCError (-32002) "Server not initialized"

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
