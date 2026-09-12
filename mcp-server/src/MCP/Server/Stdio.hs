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

import Control.Concurrent.MVar
import Control.Monad (void)
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson (encode)
import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy.Char8 qualified as BSL
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
    state_var <- newMVar initial_state
    loop state_var
  where
    loop :: MVar MCPServerState -> IO ()
    loop state_var = do
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
                        loop state_var
                    Right msg -> do
                        processStdioMessage state_var msg
                        loop state_var

    processStdioMessage :: MVar MCPServerState -> JSONRPCMessage -> IO ()
    processStdioMessage state_var = \case
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
                            cur_st <- readMVar state_var
                            let initialized = mcp_server_initialized cur_st

                            -- Process the request
                            res <- runReaderT (processMethod initialized method params) (MCPRequestState Nothing state_var)

                            -- Handle ProcessClientInput by synchronous read/write
                            final_res <- resolveClientInput state_var res

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
                            st <- readMVar state_var
                            case mcp_handler_finalize st of
                                Nothing -> return ()
                                Just finalizer -> do
                                    h_st' <- finalizer (mcp_handler_state st)
                                    void $ swapMVar state_var st{mcp_handler_state = h_st'}

    -- \| Resolve ProcessClientInput by writing a request to the client and
    -- reading the response synchronously from stdin.
    resolveClientInput :: MVar MCPServerState -> ProcessResult Aeson.Value -> IO (ProcessResult Aeson.Value)
    resolveClientInput state_var = \case
        ProcessClientInput ci_mthd ci_params ci_cont -> do
            -- Assign a request ID
            st <- readMVar state_var
            let r_id = mcp_pending_responses_next st
            _ <- swapMVar state_var st{mcp_pending_responses_next = r_id + 1}

            -- Write the server-to-client request
            writeMsg $
                RequestMessage $
                    JSONRPCRequest rPC_VERSION (RequestId $ Aeson.Number $ fromIntegral r_id) ci_mthd ci_params

            -- Read the client's response synchronously
            resp_line <- BS.hGetLine h_in
            case Aeson.eitherDecodeStrict' @JSONRPCMessage resp_line of
                Right (ResponseMessage (JSONRPCResponse _ _ result)) -> do
                    -- Run the continuation with the client's response
                    cur_st <- readMVar state_var
                    stateMVar <- newMVar cur_st
                    let requestState = MCPRequestState Nothing stateMVar
                    cont_result <- runReaderT (runExceptT $ ci_cont result) requestState
                    new_st <- readMVar stateMVar 
                    _ <- swapMVar state_var new_st
                    case cont_result of
                        Left err -> return $ ProcessServerError err
                        Right next_res -> resolveClientInput state_var next_res
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
