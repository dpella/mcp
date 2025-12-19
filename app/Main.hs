{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
-- hlint is wrong; this is needed to permit this module to create
-- values in types that share field names, such as every type
-- that has a "_meta" field; is this expected behavior?
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
Module      : Main
Description : MCP StdIO server executable
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC

Main entry point for the MCP StdIO server executable.
-}
module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Functor.Contravariant (contramap)
import Data.Text qualified as T
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Plow.Logging (IOTracer (..), Tracer (..))
import Plow.Logging.Async (withAsyncHandleTracer)
import System.IO (stderr, stdin, stdout)

import MCP.Protocol
import MCP.Server
import MCP.Server.StdIO
import MCP.Trace.Types (MCPTrace (..), renderMCPTrace)
import MCP.Types

-- | A no-op tracer that discards all trace events
nullIOTracer :: IOTracer a
nullIOTracer = IOTracer (Tracer (\_ -> pure ()))

-- | Minimal MCP Server implementation
instance MCPServer MCPServerM where
    -- handleListResources inherits the default implementation

    handleReadResource _params = do
        let textContent = TextResourceContents{uri = "example://hello", text = "Hello from MCP Haskell server!", mimeType = Just "text/plain", _meta = Nothing}
        let content = TextResource textContent
        return $ ReadResourceResult{contents = [content], _meta = Nothing}

    -- handleListResourceTemplates inherits the default implementation

    -- handleListPrompts inherits the default implementation

    handleGetPrompt _params = do
        let textContent = TextContent{text = "Hello prompt!", textType = "text", annotations = Nothing, _meta = Nothing}
        let content = TextContentType textContent
        let message = PromptMessage{role = User, content = content}
        return $ GetPromptResult{messages = [message], description = Nothing, _meta = Nothing}

    handleListTools _params = do
        let getCurrentDateTool =
                Tool
                    { name = "getCurrentDate"
                    , title = Nothing
                    , description = Just "Get the current date and time"
                    , inputSchema = InputSchema "object" Nothing Nothing
                    , outputSchema = Nothing
                    , annotations = Nothing
                    , _meta = Nothing
                    }
        return $ ListToolsResult{tools = [getCurrentDateTool], nextCursor = Nothing, _meta = Nothing}

    handleCallTool CallToolParams{name = toolName} = do
        case toolName of
            "getCurrentDate" -> do
                currentTime <- liftIO getCurrentTime
                let dateStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S UTC" currentTime
                let textContent = TextContent{text = T.pack dateStr, textType = "text", annotations = Nothing, _meta = Nothing}
                let content = TextContentType textContent
                return $ CallToolResult{content = [content], structuredContent = Nothing, isError = Nothing, _meta = Nothing}
            _ -> do
                let textContent = TextContent{text = "Tool not found", textType = "text", annotations = Nothing, _meta = Nothing}
                let content = TextContentType textContent
                return $ CallToolResult{content = [content], structuredContent = Nothing, isError = Just True, _meta = Nothing}

    -- handleComplete inherits the default implementation

    handleSetLevel _params = do
        -- handleSetLevel is a no-op in this minimal example
        -- In a real implementation, this would configure the tracer's log level
        return ()

main :: IO ()
main = do
    let serverInfo =
            Implementation
                { name = "mcp-haskell-example"
                , title = Nothing
                , version = "0.1.0"
                }

    let resourcesCap =
            ResourcesCapability
                { subscribe = Just False
                , listChanged = Just False
                }
    let promptsCap =
            PromptsCapability
                { listChanged = Just False
                }
    let toolsCap =
            ToolsCapability
                { listChanged = Just False
                }

    let capabilities =
            ServerCapabilities
                { resources = Just resourcesCap
                , prompts = Just promptsCap
                , tools = Just toolsCap
                , completions = Nothing
                , logging = Nothing
                , experimental = Nothing
                }

    let config =
            MCP.Server.StdIO.ServerConfig
                { configInput = stdin
                , configOutput = stdout
                , configServerInfo = serverInfo
                , configCapabilities = capabilities
                }

    -- Setup async tracing to stderr (stdout used for JSON-RPC) with 1000-message buffer
    withAsyncHandleTracer stderr 1000 $ \textTracer -> do
        let stdioTracer = contramap (renderMCPTrace . MCPStdIO) textTracer
        MCP.Server.StdIO.runServer config stdioTracer
