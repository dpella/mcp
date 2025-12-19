{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : MCP.Server
Description : MCP server core types and interface
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC

This module provides the core types and interface for MCP server implementations.
-}
module MCP.Server (
    -- * Server Interface
    MCPServer (..),
    ServerState (..),
    ServerConfig (..),
    MCPServerM,
    runMCPServer,
    initialServerState,

    -- * Utilities
    sendResponse,
    sendNotification,
    sendError,
) where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State.Strict (StateT, runStateT)
import Data.Aeson (ToJSON, encode, toJSON)
import Data.ByteString.Lazy.Char8 qualified as LBSC
import Data.Default
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import System.IO (Handle, hFlush)

import MCP.Protocol
import MCP.Types

-- | Server state tracking initialization, capabilities, and subscriptions
data ServerState = ServerState
    { serverInitialized :: Bool
    , serverCapabilities :: ServerCapabilities
    , clientCapabilities :: Maybe ClientCapabilities
    , serverInfo :: Maybe Implementation
    , subscriptions :: Map Text ()
    }
    deriving (Show)

-- | Configuration for running an MCP server
data ServerConfig = ServerConfig
    { configInput :: Handle
    , configOutput :: Handle
    , configServerInfo :: Implementation
    , configCapabilities :: ServerCapabilities
    }

-- | The monad stack for MCP server operations
type MCPServerM = ReaderT ServerConfig (StateT ServerState (ExceptT Text IO))

-- | Run an MCPServerM computation with the given config and initial state
runMCPServer :: ServerConfig -> ServerState -> MCPServerM a -> IO (Either Text (a, ServerState))
runMCPServer config state action = runExceptT $ runStateT (runReaderT action config) state

{- | Create the initial server state with the given capabilities
The server starts uninitialized and must receive an @initialize@ request
before it can handle other requests.
-}
initialServerState :: ServerCapabilities -> ServerState
initialServerState caps =
    ServerState
        { serverInitialized = False
        , serverCapabilities = caps
        , clientCapabilities = Nothing
        , serverInfo = Nothing
        , subscriptions = Map.empty
        }

-- | Type class for implementing MCP server handlers
class (Monad m) => MCPServer m where
    handleListResources :: ListResourcesParams -> m ListResourcesResult
    handleReadResource :: ReadResourceParams -> m ReadResourceResult
    handleListResourceTemplates :: ListResourceTemplatesParams -> m ListResourceTemplatesResult
    handleListPrompts :: ListPromptsParams -> m ListPromptsResult
    handleGetPrompt :: GetPromptParams -> m GetPromptResult
    handleListTools :: ListToolsParams -> m ListToolsResult
    handleCallTool :: CallToolParams -> m CallToolResult
    handleComplete :: CompleteParams -> m CompleteResult
    handleSetLevel :: SetLevelParams -> m ()

    -- | Default, trivial implementations for all handlers
    handleListResources _ = return def

    handleReadResource _ = return def
    handleListResourceTemplates _ = return def
    handleListPrompts _ = return def
    handleGetPrompt _ = return def
    handleListTools _ = return def
    handleCallTool _ = return def
    handleComplete _ = return def
    handleSetLevel _ = return ()

-- | Send a JSON-RPC response
sendResponse :: (MonadIO m, ToJSON a) => Handle -> RequestId -> a -> m ()
sendResponse handle reqId result = liftIO $ do
    let response = JSONRPCResponse "2.0" reqId (toJSON result)
    LBSC.hPutStrLn handle (encode response)
    hFlush handle

-- | Send a JSON-RPC error response
sendError :: (MonadIO m) => Handle -> RequestId -> JSONRPCErrorInfo -> m ()
sendError handle reqId errorInfo = liftIO $ do
    let response = JSONRPCError "2.0" reqId errorInfo
    LBSC.hPutStrLn handle (encode response)
    hFlush handle

-- | Send a JSON-RPC notification
sendNotification :: (MonadIO m, ToJSON a) => Handle -> Text -> a -> m ()
sendNotification handle method params = liftIO $ do
    let notification = JSONRPCNotification "2.0" method (Just (toJSON params))
    LBSC.hPutStrLn handle (encode notification)
    hFlush handle
