{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module:      MCP.Server.HTTP.Internal
License:     MPL-2.0
Maintainer:  <matti@dpella.io>, <lobo@dpella.io>

Shared internals for HTTP-based MCP transports.

This module contains the SSE framing types and the core request handler
used by both 'MCP.Server.HTTP' (JWT) and 'MCP.Server.SimpleHTTP' (Bearer
token).  Transport modules handle authentication and then delegate to
'handleMCPRequestCore'.
-}
module MCP.Server.HTTP.Internal (
    -- * SSE framing
    JSONRPCFrame,
    JSONRPCEvent,

    -- * POST responses
    MCPPostResponses,
    MCPPostResult (..),

    -- * Core request handler
    handleMCPRequestCore,
) where

import Control.Concurrent.MVar
import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.State.Lazy
import Data.Aeson (encode, object, toJSON, (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.SOP (I (..), NS (..), unI, unZ)
import Data.IntMap qualified as IM
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import MCP.Server.Common
import Network.HTTP.Media ((//))
import Servant
import Servant.API.MultiVerb (AsUnion (..), RespondEmpty, RespondStreaming)
import Servant.Types.SourceT
import Servant.Types.SourceT qualified as Source

-- ---------------------------------------------------------------------------
-- SSE framing types
-- ---------------------------------------------------------------------------

-- | Frame for JSON-RPC messages — appends a newline after each chunk.
data JSONRPCFrame

instance FramingRender JSONRPCFrame where
    framingRender _ f = fmap (\x -> f x <> "\n")

-- | Content-type for Server-Sent Events.
data JSONRPCEvent

instance Accept JSONRPCEvent where
    contentType _ = "text" // "event-stream"

instance MimeRender JSONRPCEvent JSONRPCMessage where
    mimeRender _ = eventFields

-- | The fields of the Server-Sent Event that carries a JSON-RPC message.
eventFields :: JSONRPCMessage -> BSL.ByteString
eventFields val = "event: message\ndata:" <> encode (toJSON val) <> "\n"

-- | A JSON-RPC message as one complete Server-Sent Event.
sseEvent :: JSONRPCMessage -> BS.ByteString
sseEvent val = BSL.toStrict (eventFields val <> "\n")

-- ---------------------------------------------------------------------------
-- POST responses
-- ---------------------------------------------------------------------------

{- | The responses a POST to the MCP endpoint can produce.

The Streamable HTTP transport requires @202 Accepted@ with no body when the
posted message is a notification or a response. A request gets its JSON-RPC
messages as a stream of Server-Sent Events.
-}
type MCPPostResponses =
    '[ RespondEmpty 202 "The notification or response was accepted"
     , RespondStreaming 200 "The JSON-RPC messages of the request, as Server-Sent Events" JSONRPCFrame JSONRPCEvent
     ]

-- | What the server answers to a message posted to the MCP endpoint.
data MCPPostResult
    = -- | The message was a notification or a response. The server sends no body.
      MCPAccepted
    | -- | The message was a request. Each JSON-RPC message of the answer is one
      -- complete Server-Sent Event.
      MCPEventStream (SourceIO BS.ByteString)

instance AsUnion MCPPostResponses MCPPostResult where
    toUnion MCPAccepted = Z (I ())
    toUnion (MCPEventStream events) = S (Z (I events))
    fromUnion (Z (I ())) = MCPAccepted
    fromUnion (S events) = MCPEventStream (unI (unZ events))

-- ---------------------------------------------------------------------------
-- Core request handler
-- ---------------------------------------------------------------------------

{- | Transport-agnostic HTTP request handler.

Both 'MCP.Server.HTTP.handleMCPRequest' and
'MCP.Server.SimpleHTTP.handleSimpleHTTPRequest' delegate here after
performing their own authentication checks.

When @mb_user@ is @'Just' user@, the server calls @mcp_handler_init user@
on the first @initialize@ request (JWT\/HTTP transport).  When @'Nothing'@,
the init hook is skipped (SimpleHTTP \/ Stdio convention).
-}
handleMCPRequestCore ::
    MVar MCPServerState ->
    -- | Authenticated user, if available (for @mcp_handler_init@)
    Maybe MCPHandlerUser ->
    JSONRPCMessage ->
    Handler MCPPostResult
handleMCPRequestCore state_var mb_user request_value =
    do
        MCPServerState
            { mcp_server_initialized = server_initialized
            , mcp_log_level = log_level
            } <-
            liftIO $ readMVar state_var

        -- Log the request if debug level
        case log_level of
            Nothing -> return ()
            Just level ->
                when (level >= Debug) $
                    liftIO $
                        BSL.putStrLn $
                            "[request] " <> encode request_value

        case request_value of
            -- A notification needs no answer.
            NotificationMessage _ ->
                return MCPAccepted
            ErrorMessage err -> do
                throwError err400{errBody = encode err}
            -- Fill in the MVar for any pending responses from the client
            ResponseMessage (JSONRPCResponse _ (RequestId req_id) result) -> do
                cur_st <- liftIO $ readMVar state_var
                case Aeson.fromJSON req_id of
                    Aeson.Error err ->
                        throwError err400{errBody = "Invalid request ID: " <> BSL.pack err}
                    Aeson.Success req_index ->
                        case IM.lookup req_index (mcp_pending_responses cur_st) of
                            Nothing ->
                                throwError err400{errBody = "Server received unexpected response"}
                            Just mvar -> do
                                liftIO $ do
                                    modifyMVar_ state_var $ \cur_st' ->
                                        return cur_st'{mcp_pending_responses = IM.delete req_index (mcp_pending_responses cur_st')}
                                    putMVar mvar result
                                return MCPAccepted
            RequestMessage (JSONRPCRequest jsonrpc req_id method params) -> do
                -- Validate JSON-RPC version
                let bs_rpc_version = BSL.fromStrict (encodeUtf8 rPC_VERSION)
                when (jsonrpc /= rPC_VERSION) $
                    throwError err400{errBody = "Invalid jsonrpc version, must be '" <> bs_rpc_version <> "'"}

                -- Validate request ID
                when (not $ isValidRequestId req_id) $
                    throwError err400{errBody = "Invalid request ID, must be string, number, or null"}

                -- Initialize handler state on first initialize request
                -- (only when an authenticated user is available)
                case mb_user of
                    Just auth_user ->
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
                    Nothing -> return ()

                -- Process the request routing to the appropriate handler.
                -- Set mcp_current_user while the handler runs, then clear it
                -- before releasing the shared state. Handlers read this via
                -- getCurrentUser.
                res <- liftIO $ modifyMVar state_var $
                    runWithCurrentUser (processMethod server_initialized method params)

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
                                        handler_st' <- handler_finalize handler_st
                                        return st{mcp_handler_state = handler_st'}
                return (MCPEventStream (sseEvent <$> fromStepT final_result))
  where
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
                            ci_resp <- liftIO $ takeMVar mvar
                            result <-
                                liftIO $
                                    modifyMVar state_var $
                                        runWithCurrentUser $
                                            runExceptT $
                                                ci_cont ci_resp
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

    runWithCurrentUser :: StateT MCPServerState IO a -> MCPServerState -> IO (MCPServerState, a)
    runWithCurrentUser action cur_st = do
        let cur_st_with_user = cur_st{mcp_current_user = mb_user}
        (result, final_st) <- runStateT action cur_st_with_user
        pure (final_st{mcp_current_user = Nothing}, result)
