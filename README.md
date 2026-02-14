# DPella MCP — Model Context Protocol for Haskell

A complete implementation of the [Model Context Protocol (MCP)](https://modelcontextprotocol.io/) for Haskell, split into two packages:

- **`mcp-protocol`** — Pure protocol types with minimal dependencies (aeson, base, containers, text)
- **`mcp`** — Servant-based HTTP server with JWT authentication

## Overview

This repository provides a type-safe implementation of the Model Context Protocol in Haskell. MCP is an open protocol that standardizes how applications provide context to Large Language Models (LLMs), enabling AI models to securely connect to data sources and tools.

## Features

- **Latest Protocol Support**: Implements MCP protocol version 2025-06-18
- **Complete MCP Protocol Implementation**: All MCP message types, requests, responses, and notifications
- **Type-Safe Design**: Full Haskell type system integration with automatic JSON serialization via Aeson
- **HTTP Transport**: Servant-based HTTP server with streaming SSE responses
- **JWT Authentication**: Secure authentication via `servant-auth-server`
- **Extensible Server Interface**: Configurable handler framework for implementing custom MCP servers
- **Tool Framework**: Helper functions for defining tools with input validation and structured results

## Architecture

The implementation is organized into two packages:

### `mcp-protocol`

Core protocol types with minimal dependencies — suitable for building clients or alternative server implementations.

- **`MCP.Types`**: Core MCP data types (Content, Resource, Tool, Prompt, Capability, etc.)
- **`MCP.Protocol`**: JSON-RPC 2.0 message wrappers, all client/server request/response types, notification types
- **`MCP.Aeson`**: Custom Aeson parsing options

### `mcp`

Servant-based server implementation. Re-exports `MCP.Protocol` and `MCP.Types` for convenience.

- **`MCP.Server`**: Core server infrastructure with `MCPServerT` monad transformer, `ProcessHandlers` record, `ToolHandler` framework, JWT-authenticated Servant API, server state management and request routing

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

For a server implementation, add `mcp` to your `build-depends`:

```cabal
build-depends:
    base
  , servant
  , servant-server
  , servant-auth-server
  , aeson
  , mcp
```

If you only need the protocol types (e.g. for a client), depend on `mcp-protocol` instead:

```cabal
build-depends:
    base
  , aeson
  , mcp-protocol
```

This project targets GHC 9.12 (see `cabal.project`).

## Quick Start

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Concurrent.MVar (newMVar)
import MCP.Server

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
mcp-protocol/               # Core protocol types package
├── src/MCP/
│   ├── Aeson.hs             # Custom Aeson parsing options
│   ├── Protocol.hs          # JSON-RPC protocol messages
│   └── Types.hs             # Core MCP data types
└── mcp-protocol.cabal

mcp/                  # Server implementation package
├── src/MCP/
│   └── Server.hs            # Server infrastructure and Servant API
├── test/
│   ├── Main.hs              # Test entry point
│   └── MCP/
│       ├── Integration.hs   # Integration tests (hspec-wai)
│       ├── TestServer.hs    # Test server configuration
│       └── TestUtils.hs     # Test utilities and request builders
└── mcp.cabal
```

## Development

```bash
cabal build all
cabal test all
```

## License

MPL-2.0 — see `LICENSE` in this repository.

## References

- [Model Context Protocol Specification](https://modelcontextprotocol.io/)
- [MCP TypeScript SDK](https://github.com/modelcontextprotocol/typescript-sdk)
- [Official MCP Documentation](https://modelcontextprotocol.io/introduction)
