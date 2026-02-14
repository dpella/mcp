# DPella MCP — Model Context Protocol Server for Haskell

A complete implementation of the [Model Context Protocol (MCP)](https://modelcontextprotocol.io/) for Haskell, built on Servant and `servant-auth-server`.

## Overview

This package provides a type-safe implementation of the Model Context Protocol in Haskell. MCP is an open protocol that standardizes how applications provide context to Large Language Models (LLMs), enabling AI models to securely connect to data sources and tools.

## Features

- **Latest Protocol Support**: Implements MCP protocol version 2025-06-18
- **Complete MCP Protocol Implementation**: All MCP message types, requests, responses, and notifications
- **Type-Safe Design**: Full Haskell type system integration with automatic JSON serialization via Aeson
- **HTTP Transport**: Servant-based HTTP server with streaming SSE responses
- **JWT Authentication**: Secure authentication via `servant-auth-server`
- **Extensible Server Interface**: Configurable handler framework for implementing custom MCP servers
- **Tool Framework**: Helper functions for defining tools with input validation and structured results

## Architecture

The implementation is organized into three main modules:

### `MCP.Types`
- Core MCP data types (Content, Resource, Tool, Prompt, Capability, etc.)
- Automatic JSON serialization/deserialization via Aeson
- Type-safe mapping of the complete MCP schema

### `MCP.Protocol`
- JSON-RPC 2.0 message wrappers (request, response, error, notification)
- All client and server request/response types
- Notification types for bidirectional communication
- Union types for organizing related messages
- Type classes `IsJSONRPCRequest` and `IsJSONRPCNotification` for generic serialization

### `MCP`
- Core server infrastructure with `MCPServerT` monad transformer
- `ProcessHandlers` record for implementing custom method handlers
- `ToolHandler` framework for defining tools with metadata and handlers
- JWT-authenticated Servant API at the `/mcp` endpoint
- Server state management and request routing

## MCP Protocol Support

| Operation | Description | Status |
|-----------|-------------|--------|
| `initialize` | Start session and negotiate capabilities | Supported |
| `ping` | Health check | Supported |
| `resources/list` | List available resources | Supported |
| `resources/templates/list` | List available resource templates | Supported |
| `resources/read` | Read resource contents | Supported |
| `resources/subscribe` | Subscribe to resource updates | Supported |
| `resources/unsubscribe` | Unsubscribe from resource updates | Supported |
| `prompts/list` | List available prompts | Supported |
| `prompts/get` | Get prompt with arguments | Supported |
| `tools/list` | List available tools | Supported |
| `tools/call` | Execute a tool | Supported |
| `completion/complete` | Auto-completion with context | Supported |
| `logging/setLevel` | Set logging level | Supported |
| `sampling/createMessage` | Request LLM sampling | Supported |
| `roots/list` | List client root directories | Supported |
| `elicitation/create` | Request user input via forms | Supported |

## Install

Add the package to your `build-depends`:

```cabal
build-depends:
    base
  , servant
  , servant-server
  , servant-auth-server
  , aeson
  , mcp
```

This project targets GHC 9.12 (see `cabal.project`).

## Quick Start

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Concurrent.MVar (newMVar)
import MCP

-- Define your handler state and user types
type instance MCPHandlerState = ()
type instance MCPHandlerUser  = MyUser

-- Create server state with capabilities
mkServerState :: IO (MVar MCPServerState)
mkServerState = do
  let impl = Implementation "my-server" "1.0.0" Nothing
      caps = ServerCapabilities
        { logging      = Nothing
        , prompts      = Nothing
        , resources    = Nothing
        , tools        = Just (ToolsCapability { listChanged = Just True })
        , completions  = Nothing
        , experimental = Nothing
        }
      handlers = withToolHandlers myTools defaultProcessHandlers
  newMVar $ initMCPServerState () Nothing Nothing caps impl Nothing handlers

-- Define tools using the ToolHandler framework
myTools :: [ToolHandler]
myTools =
  [ toolHandler "greet" (Just "Say hello") greetSchema $ \_args ->
      return $ ProcessSuccess $ toolTextResult ["Hello!"]
  ]
```

## Project Structure

```
src/
├── MCP.hs               # Server infrastructure and Servant API
└── MCP/
    ├── Aeson.hs          # Custom Aeson parsing options
    ├── Protocol.hs       # JSON-RPC protocol messages
    └── Types.hs          # Core MCP data types

test/
├── Main.hs              # Test entry point
└── MCP/
    ├── Integration.hs    # Integration tests (hspec-wai)
    ├── TestServer.hs     # Test server configuration
    └── TestUtils.hs      # Test utilities and request builders
```

## Development

```bash
cabal build
cabal test
```

## License

MPL-2.0 — see `LICENSE` in this repository.

## References

- [Model Context Protocol Specification](https://modelcontextprotocol.io/)
- [MCP TypeScript SDK](https://github.com/modelcontextprotocol/typescript-sdk)
- [Official MCP Documentation](https://modelcontextprotocol.io/introduction)
