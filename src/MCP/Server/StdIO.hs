{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : MCP.Server.StdIO
Description : MCP server implementation for stdin/stdout communication
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC

This module provides MCP server implementation for stdin/stdout streams.
-}
module MCP.Server.StdIO (
    -- * Server Runner
    runServer,
    ServerConfig (..),
) where

import Control.Exception (catch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ask)
import Control.Monad.State.Strict (get, put)
import Data.Aeson (decode, fromJSON, object, toJSON)
import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as LBS
import Data.Functor.Contravariant (contramap)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TEE
import Plow.Logging (IOTracer, traceWith)
import System.IO (Handle)
import System.IO.Error (isEOFError)

import MCP.Protocol
import MCP.Server (MCPServer (..), MCPServerM, ServerConfig (..), ServerState (..), initialServerState, runMCPServer, sendError, sendResponse)
import MCP.Trace.Operation (OperationTrace (..))
import MCP.Trace.Protocol (ProtocolTrace (..))
import MCP.Trace.Server (ServerTrace (..))
import MCP.Trace.StdIO (StdIOTrace (..))
import MCP.Types

-- | Convert a RequestId to Text for tracing
requestIdToText :: RequestId -> T.Text
requestIdToText (RequestId val) = T.pack (show val)

-- | Traced version of sendResponse that emits StdIOMessageSent
tracedSendResponse :: (MonadIO m, Aeson.ToJSON a) => IOTracer StdIOTrace -> Handle -> RequestId -> a -> m ()
tracedSendResponse tracer handle reqId result = do
    let response = JSONRPCResponse "2.0" reqId (toJSON result)
        encoded = Aeson.encode response
        size = fromIntegral (LBS.length encoded)
    liftIO $ traceWith tracer $ StdIOMessageSent{messageSize = size}
    sendResponse handle reqId result

-- | Traced version of sendError that emits StdIOMessageSent
tracedSendError :: (MonadIO m) => IOTracer StdIOTrace -> Handle -> RequestId -> JSONRPCErrorInfo -> m ()
tracedSendError tracer handle reqId errorInfo = do
    let response = JSONRPCError "2.0" reqId errorInfo
        encoded = Aeson.encode response
        size = fromIntegral (LBS.length encoded)
    liftIO $ traceWith tracer $ StdIOMessageSent{messageSize = size}
    sendError handle reqId errorInfo

-- | Handle an incoming JSON-RPC message
handleMessage :: (MCPServer MCPServerM) => IOTracer StdIOTrace -> BSC.ByteString -> MCPServerM (Maybe ())
handleMessage tracer input = do
    let protoTracer = contramap StdIOProtocol tracer
        serverTracer = contramap StdIOServer tracer
    case decode (LBS.fromStrict input) :: Maybe JSONRPCMessage of
        Nothing -> do
            -- Trace parse error
            let inputText = TE.decodeUtf8With TEE.lenientDecode (LBS.toStrict $ LBS.take 100 $ LBS.fromStrict input)
            liftIO $ traceWith protoTracer $ ProtocolParseError "Failed to decode JSON-RPC message" (Just inputText)
            config <- ask
            tracedSendError tracer (configOutput config) (RequestId (toJSON ("unknown" :: T.Text))) $
                JSONRPCErrorInfo (-32700) "Parse error" Nothing
            return Nothing
        Just msg -> case msg of
            RequestMessage req -> do
                -- Trace request received
                let JSONRPCRequest _ reqId method _ = req
                liftIO $ traceWith protoTracer $ ProtocolRequestReceived (requestIdToText reqId) method
                handleRequest tracer protoTracer serverTracer req
                return (Just ())
            NotificationMessage notif -> do
                -- Trace notification received
                let JSONRPCNotification _ method _ = notif
                liftIO $ traceWith protoTracer $ ProtocolNotificationReceived method
                handleNotification notif
                return (Just ())
            _ -> do
                config <- ask
                tracedSendError tracer (configOutput config) (RequestId (toJSON ("unknown" :: T.Text))) $
                    JSONRPCErrorInfo (-32600) "Invalid Request" Nothing
                return Nothing

-- | Handle a JSON-RPC request
handleRequest :: (MCPServer MCPServerM) => IOTracer StdIOTrace -> IOTracer ProtocolTrace -> IOTracer ServerTrace -> JSONRPCRequest -> MCPServerM ()
handleRequest tracer protoTracer serverTracer (JSONRPCRequest _ reqId method params) = do
    let opTracer = contramap StdIOOperation tracer
    config <- ask
    state <- get

    case method of
        "initialize" -> case params of
            Just p -> case fromJSON p of
                Aeson.Success initParams -> handleInitialize tracer serverTracer reqId initParams
                Aeson.Error e ->
                    tracedSendError tracer (configOutput config) reqId $
                        JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
            Nothing ->
                tracedSendError tracer (configOutput config) reqId $
                    JSONRPCErrorInfo (-32602) "Missing params" Nothing
        "ping" -> handlePing tracer reqId
        "resources/list" -> do
            liftIO $ traceWith opTracer OperationResourcesList
            if not (serverInitialized state)
                then
                    tracedSendError tracer (configOutput config) reqId $
                        JSONRPCErrorInfo (-32002) "Server not initialized" Nothing
                else case params of
                    Just p -> case fromJSON p of
                        Aeson.Success listParams -> do
                            result <- handleListResources listParams
                            tracedSendResponse tracer (configOutput config) reqId result
                        Aeson.Error e ->
                            tracedSendError tracer (configOutput config) reqId $
                                JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
                    Nothing -> do
                        result <- handleListResources (ListResourcesParams Nothing)
                        tracedSendResponse tracer (configOutput config) reqId result
        "resources/read" -> do
            if not (serverInitialized state)
                then
                    tracedSendError tracer (configOutput config) reqId $
                        JSONRPCErrorInfo (-32002) "Server not initialized" Nothing
                else case params of
                    Just p -> case fromJSON p of
                        Aeson.Success readParams@(ReadResourceParams uri) -> do
                            liftIO $ traceWith opTracer $ OperationResourceRead uri
                            result <- handleReadResource readParams
                            tracedSendResponse tracer (configOutput config) reqId result
                        Aeson.Error e ->
                            tracedSendError tracer (configOutput config) reqId $
                                JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
                    Nothing ->
                        tracedSendError tracer (configOutput config) reqId $
                            JSONRPCErrorInfo (-32602) "Missing params" Nothing
        "resources/templates/list" -> do
            liftIO $ traceWith opTracer OperationResourceTemplatesList
            if not (serverInitialized state)
                then
                    tracedSendError tracer (configOutput config) reqId $
                        JSONRPCErrorInfo (-32002) "Server not initialized" Nothing
                else case params of
                    Just p -> case fromJSON p of
                        Aeson.Success listParams -> do
                            result <- handleListResourceTemplates listParams
                            tracedSendResponse tracer (configOutput config) reqId result
                        Aeson.Error e ->
                            tracedSendError tracer (configOutput config) reqId $
                                JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
                    Nothing -> do
                        result <- handleListResourceTemplates (ListResourceTemplatesParams Nothing)
                        tracedSendResponse tracer (configOutput config) reqId result
        "prompts/list" -> do
            liftIO $ traceWith opTracer OperationPromptsList
            if not (serverInitialized state)
                then
                    tracedSendError tracer (configOutput config) reqId $
                        JSONRPCErrorInfo (-32002) "Server not initialized" Nothing
                else case params of
                    Just p -> case fromJSON p of
                        Aeson.Success listParams -> do
                            result <- handleListPrompts listParams
                            tracedSendResponse tracer (configOutput config) reqId result
                        Aeson.Error e ->
                            tracedSendError tracer (configOutput config) reqId $
                                JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
                    Nothing -> do
                        result <- handleListPrompts (ListPromptsParams Nothing)
                        tracedSendResponse tracer (configOutput config) reqId result
        "prompts/get" -> do
            if not (serverInitialized state)
                then
                    tracedSendError tracer (configOutput config) reqId $
                        JSONRPCErrorInfo (-32002) "Server not initialized" Nothing
                else case params of
                    Just p -> case fromJSON p of
                        Aeson.Success getParams@(GetPromptParams name args) -> do
                            liftIO $ traceWith opTracer $ OperationPromptGet name (toJSON <$> args)
                            result <- handleGetPrompt getParams
                            tracedSendResponse tracer (configOutput config) reqId result
                        Aeson.Error e ->
                            tracedSendError tracer (configOutput config) reqId $
                                JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
                    Nothing ->
                        tracedSendError tracer (configOutput config) reqId $
                            JSONRPCErrorInfo (-32602) "Missing params" Nothing
        "tools/list" -> do
            liftIO $ traceWith opTracer OperationToolsList
            if not (serverInitialized state)
                then
                    tracedSendError tracer (configOutput config) reqId $
                        JSONRPCErrorInfo (-32002) "Server not initialized" Nothing
                else case params of
                    Just p -> case fromJSON p of
                        Aeson.Success listParams -> do
                            result <- handleListTools listParams
                            tracedSendResponse tracer (configOutput config) reqId result
                        Aeson.Error e ->
                            tracedSendError tracer (configOutput config) reqId $
                                JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
                    Nothing -> do
                        result <- handleListTools (ListToolsParams Nothing)
                        tracedSendResponse tracer (configOutput config) reqId result
        "tools/call" -> do
            if not (serverInitialized state)
                then
                    tracedSendError tracer (configOutput config) reqId $
                        JSONRPCErrorInfo (-32002) "Server not initialized" Nothing
                else case params of
                    Just p -> case fromJSON p of
                        Aeson.Success callParams@(CallToolParams name args) -> do
                            result <- handleCallTool callParams
                            let argsJson = maybe (toJSON Aeson.Null) toJSON args
                                resultJson = toJSON result
                                success = maybe True not (MCP.Protocol.isError result)
                            liftIO $ traceWith opTracer $ OperationToolCall name argsJson resultJson Nothing success
                            tracedSendResponse tracer (configOutput config) reqId result
                        Aeson.Error e ->
                            tracedSendError tracer (configOutput config) reqId $
                                JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
                    Nothing ->
                        tracedSendError tracer (configOutput config) reqId $
                            JSONRPCErrorInfo (-32602) "Missing params" Nothing
        "completion/complete" -> do
            if not (serverInitialized state)
                then
                    tracedSendError tracer (configOutput config) reqId $
                        JSONRPCErrorInfo (-32002) "Server not initialized" Nothing
                else case params of
                    Just p -> case fromJSON p of
                        Aeson.Success completeParams@(CompleteParams ref (CompletionArgument argName argValue) _context) -> do
                            let refType = case ref of
                                    PromptRef _ -> "prompt"
                                    ResourceTemplateRef _ -> "resource_template"
                            liftIO $ traceWith opTracer $ OperationComplete refType (argName <> "=" <> argValue)
                            result <- handleComplete completeParams
                            tracedSendResponse tracer (configOutput config) reqId result
                        Aeson.Error e ->
                            tracedSendError tracer (configOutput config) reqId $
                                JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
                    Nothing ->
                        tracedSendError tracer (configOutput config) reqId $
                            JSONRPCErrorInfo (-32602) "Missing params" Nothing
        "logging/setLevel" -> case params of
            Just p -> case fromJSON p of
                Aeson.Success setLevelParams -> do
                    _ <- handleSetLevel setLevelParams
                    -- SetLevel response is just an empty object
                    sendResponse (configOutput config) reqId (object [])
                Aeson.Error e ->
                    tracedSendError tracer (configOutput config) reqId $
                        JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
            Nothing ->
                tracedSendError tracer (configOutput config) reqId $
                    JSONRPCErrorInfo (-32602) "Missing params" Nothing
        _ -> do
            -- Trace method not found
            liftIO $ traceWith protoTracer $ ProtocolMethodNotFound (requestIdToText reqId) method
            tracedSendError tracer (configOutput config) reqId $
                JSONRPCErrorInfo (-32601) "Method not found" Nothing

handleInitialize :: IOTracer StdIOTrace -> IOTracer ServerTrace -> RequestId -> InitializeParams -> MCPServerM ()
handleInitialize stdioTracer serverTracer reqId params = do
    config <- ask
    state <- get

    let InitializeParams{capabilities = clientCaps, clientInfo = clientImpl} = params

    put
        state
            { serverInitialized = True
            , clientCapabilities = Just clientCaps
            , serverInfo = Just (configServerInfo config)
            }

    -- Emit ServerInitialized trace
    let Implementation clientName _ _ = clientImpl
    liftIO $ traceWith serverTracer (ServerInitialized (Just clientName))

    -- Emit ServerCapabilityNegotiation trace
    let caps = extractCapabilityNames (serverCapabilities state)
    liftIO $ traceWith serverTracer (ServerCapabilityNegotiation caps)

    let result =
            InitializeResult
                { protocolVersion = mcpProtocolVersion
                , capabilities = serverCapabilities state
                , serverInfo = configServerInfo config
                , instructions = Nothing
                , _meta = Nothing
                }

    tracedSendResponse stdioTracer (configOutput config) reqId result

-- | Extract capability names from ServerCapabilities
extractCapabilityNames :: ServerCapabilities -> [T.Text]
extractCapabilityNames (ServerCapabilities res prpts tls comps logCap _exp) =
    concat
        [ maybe [] (const ["resources"]) res
        , maybe [] (const ["prompts"]) prpts
        , maybe [] (const ["tools"]) tls
        , maybe [] (const ["completions"]) comps
        , maybe [] (const ["logging"]) logCap
        ]

handlePing :: IOTracer StdIOTrace -> RequestId -> MCPServerM ()
handlePing tracer reqId = do
    config <- ask
    -- Ping response is just an empty object in MCP
    tracedSendResponse tracer (configOutput config) reqId (object [])

-- | Handle a JSON-RPC notification
handleNotification :: JSONRPCNotification -> MCPServerM ()
handleNotification _ = do
    return ()

-- | Run the MCP server with the given configuration
runServer :: (MCPServer MCPServerM) => ServerConfig -> IOTracer StdIOTrace -> IO ()
runServer config tracer = do
    let initialState = initialServerState (configCapabilities config)

    -- Emit ServerInit trace
    let Implementation{name = serverName, version = serverVersion} = configServerInfo config
    traceWith (contramap StdIOServer tracer) $ ServerInit serverName serverVersion

    let loop = do
            eofOrLine <-
                liftIO $
                    catch
                        (Right <$> BSC.hGetLine (configInput config))
                        ( \e ->
                            if isEOFError e
                                then return (Left Nothing)
                                else return (Left (Just (T.pack (show e))))
                        )
            case eofOrLine of
                Left maybeErr -> do
                    -- Trace EOF or read error
                    case maybeErr of
                        Nothing -> liftIO $ traceWith tracer StdIOEOF
                        Just errMsg -> liftIO $ traceWith tracer $ StdIOReadError{stdioErrorMessage = errMsg}
                    return ()
                Right line -> do
                    -- Trace message received from stdin
                    liftIO $ traceWith tracer $ StdIOMessageReceived{messageSize = BSC.length line}
                    result <- handleMessage tracer line
                    case result of
                        Just () -> loop
                        Nothing -> return ()

    result <- runMCPServer config initialState loop

    -- Emit ServerShutdown trace
    traceWith (contramap StdIOServer tracer) ServerShutdown

    case result of
        Left err -> Prelude.error $ "MCPServer error: " <> show err
        Right _ -> return ()
