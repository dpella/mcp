{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module:      MCP.Server.HTTP
License:     MPL-2.0
Maintainer:  <matti@dpella.io>, <lobo@dpella.io>

Servant-based HTTP transport for the MCP server.

This module provides a JWT-authenticated Servant API that accepts
JSON-RPC requests via POST and returns responses as SSE streams.
-}
module MCP.Server.HTTP (
    -- * Servant API
    MCPAPI,
    mcpAPI,
    handleMCPRequest,
    handleMCPEvents,
) where

import Control.Concurrent.MVar
import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.State.Lazy
import Data.Aeson (encode, object, toJSON, (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.IntMap qualified as IM
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Tuple (swap)
import MCP.Server.Common
import Network.HTTP.Media ((//))
import Servant
import Servant.Auth.Server (Auth, AuthResult (..), JWT)
import Servant.Auth.Server qualified as AuthServer
import Servant.Types.SourceT
import Servant.Types.SourceT qualified as Source

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
                    Just level ->
                        when (level >= Debug) $
                            liftIO $
                                BSL.putStrLn $
                                    "[request] " <> encode request_value

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
