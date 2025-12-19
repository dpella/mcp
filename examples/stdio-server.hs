{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
Module      : Main
Description : Example Stdio MCP Server
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC

This example demonstrates how to run the MCP server over stdio transport. The
server will accept messages from stdin, and respond to them on stdout; it
terminates when it receives an EOF.

To test:

1. Compile: @cabal build mcp-stdio@
2. Run: @cabal run mcp-stdio@
3. Echo JSON-RPC messages to the server's stdin:

> echo '{"jsonrpc":"2.0","id":1,"method":"ping"}' | cabal run mcp-stdio -- --log
4. Send messages from a file (one per line) to the server:
> cat $MESSAGE_FILE | cabal run mcp-stdio -- --log
5. Send a message to the server using the tool `getCurrentDate`:
> echo '{"jsonrpc":"2.0","id":1,"method":"callTool","params":{"name":"getCurrentDate"}}' | cabal run mcp-stdio -- --log
-}
module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Text qualified as T
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Options.Applicative
import Plow.Logging (IOTracer (..), Tracer (..))

import System.IO (stdin, stdout)

import MCP.Protocol
import MCP.Server
import MCP.Server.StdIO
import MCP.Types

-- | A no-op tracer that discards all trace events
nullIOTracer :: IOTracer a
nullIOTracer = IOTracer (Tracer (\_ -> pure ()))

-- | Command line options
data Options = Options
    deriving (Show)

-- | Parser for command line options
optionsParser :: Parser Options
optionsParser = pure Options

-- | Full parser with help
opts :: ParserInfo Options
opts =
    info
        (optionsParser <**> helper)
        ( fullDesc
            <> progDesc "Run an MCP server over stdio transport"
            <> header "mcp-stdio - Stdio MCP Server Example"
        )

-- | Example MCP Server implementation (copied from Main.hs)
instance MCPServer MCPServerM where
    -- handleListResources inherits the default implementation

    handleReadResource _params = do
        let textContent =
                TextResourceContents
                    { uri = "example://mcp/stdio/hello"
                    , text = "Hello from MCP Haskell Stdio server!"
                    , mimeType = Just "text/plain"
                    , _meta = Nothing
                    }
        let content = TextResource textContent
        return $ ReadResourceResult{contents = [content], _meta = Nothing}

    -- handleListResourceTemplates inherits the default implementation

    -- handleListPrompts inherits the default implementation

    handleGetPrompt _params = do
        let textContent = TextContent{text = "Hello Stdio prompt!", textType = "text", annotations = Nothing, _meta = Nothing}
        let content = TextContentType textContent
        let message = PromptMessage{role = User, content = content}
        return $ GetPromptResult{messages = [message], description = Nothing, _meta = Nothing}

    handleListTools _params = do
        let getCurrentDateTool =
                Tool
                    { name = "getCurrentDate"
                    , title = Nothing
                    , description = Just "Get the current date and time via Stdio"
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
                let dateStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S UTC (via Stdio)" currentTime
                let textContent = TextContent{text = T.pack dateStr, textType = "text", annotations = Nothing, _meta = Nothing}
                let content = TextContentType textContent
                return $ CallToolResult{content = [content], structuredContent = Nothing, isError = Nothing, _meta = Nothing}
            _ -> do
                let textContent = TextContent{text = "Tool not found", textType = "text", annotations = Nothing, _meta = Nothing}
                let content = TextContentType textContent
                return $ CallToolResult{content = [content], structuredContent = Nothing, isError = Just True, _meta = Nothing}

-- handleComplete inherits the default implementation

-- handleSetLevel inherits the default implementation

main :: IO ()
main = do
    _ <- execParser opts

    putStrLn "Starting MCP Haskell Stdio Server..."

    let serverInfo =
            Implementation
                { name = "mcp-haskell-stdio-example"
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
            ServerConfig
                { configInput = stdin
                , configOutput = stdout
                , configServerInfo = serverInfo
                , configCapabilities = capabilities
                }

    putStrLn "Stdio server configured, starting..."

    putStrLn ""
    putStrLn "Example test input:"
    putStrLn "'{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\"}'"

    runServer config nullIOTracer
