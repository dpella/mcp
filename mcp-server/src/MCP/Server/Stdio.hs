{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module:      MCP.Server.Stdio
License:     MPL-2.0
Maintainer:  <matti@dpella.io>, <lobo@dpella.io>

Stdio transport for the MCP server.

Reads JSON-RPC messages line-by-line from an input handle and writes
responses to an output handle. This transport does not use JWT
authentication; it is assumed that the process boundary provides
authentication.
-}
module MCP.Server.Stdio (
    serveStdio,
) where

import Control.Monad.Except
import Control.Monad.State.Lazy
import Data.Aeson (encode)
import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.IORef
import Data.Text qualified as T
import MCP.Server.Common
import System.IO (BufferMode (..), Handle, hFlush, hIsEOF, hSetBuffering)

{- | Run the MCP server over stdio transport.

Reads JSON-RPC messages line-by-line from the input handle,
processes them through the standard MCP message handler, and writes
JSON-RPC responses line-by-line to the output handle.

Stdio transport does not use JWT authentication; it is assumed that
the process boundary provides authentication.

The server runs until EOF is reached on the input handle.

__Note:__ When a handler returns 'ProcessClientInput', the server writes a
request to the client and blocks on the input handle until a response
arrives.  There is no timeout — if the client never responds, the server
will block indefinitely.
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
                            st <- readIORef state_ref
                            case mcp_handler_finalize st of
                                Nothing -> return ()
                                Just finalizer -> do
                                    h_st' <- finalizer (mcp_handler_state st)
                                    writeIORef state_ref st{mcp_handler_state = h_st'}

    -- \| Resolve ProcessClientInput by writing a request to the client and
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
