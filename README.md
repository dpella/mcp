# MCP — Model Context Protocol for Haskell

[![mcp on Hackage](https://img.shields.io/hackage/v/mcp.svg)](https://hackage.haskell.org/package/mcp)
[![mcp-types on Hackage](https://img.shields.io/hackage/v/mcp-types.svg)](https://hackage.haskell.org/package/mcp-types)

A complete implementation of the [Model Context Protocol (MCP)](https://modelcontextprotocol.io/) for Haskell, split into two packages:

- **`mcp-types`** — Pure protocol types with minimal dependencies (aeson, base, containers, text)
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

### `mcp-types`

Core protocol types with minimal dependencies — suitable for building clients or alternative server implementations.

- **`MCP.Types`**: Core MCP data types (Content, Resource, Tool, Prompt, Capability, etc.)
- **`MCP.Protocol`**: JSON-RPC 2.0 message wrappers, all client/server request/response types, notification types
- **`MCP.Aeson`**: Custom Aeson parsing options

### `mcp`

Servant-based server implementation. Re-exports `MCP.Protocol` and `MCP.Types` for convenience.

- **`MCP.Server`**: Core server infrastructure with `MCPServerT` monad transformer, `ProcessHandlers` record, `ToolHandler` framework, JWT-authenticated Servant API, server state management and request routing

For OAuth 2.0 authorization in MCP clients, see [`oauth2-server`](https://hackage.haskell.org/package/oauth2-server).

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

If you only need the protocol types (e.g. for a client), depend on `mcp-types` instead:

```cabal
build-depends:
    base
  , aeson
  , mcp-types
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

## Example Server

A fully documented example server lives in `mcp-server/example/`. It demonstrates
every major feature: tools, resources, resource templates, prompts, completions,
logging, lifecycle hooks, and JWT authentication.

### Run the example

```bash
cabal run mcp-example
```

The server starts on `http://localhost:8080/mcp` and prints a JWT bearer token to
stdout. Use it in the `Authorization` header for all requests:

```bash
# Initialize the session (replace $TOKEN with the printed token)
curl -s -X POST http://localhost:8080/mcp \
  -H 'Content-Type: application/json' \
  -H "Authorization: Bearer $TOKEN" \
  -d '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{},"clientInfo":{"name":"curl","version":"1.0"}}}'

# Send initialized notification
curl -s -X POST http://localhost:8080/mcp \
  -H 'Content-Type: application/json' \
  -H "Authorization: Bearer $TOKEN" \
  -d '{"jsonrpc":"2.0","method":"notifications/initialized","params":null}'

# List available tools
curl -s -X POST http://localhost:8080/mcp \
  -H 'Content-Type: application/json' \
  -H "Authorization: Bearer $TOKEN" \
  -d '{"jsonrpc":"2.0","id":2,"method":"tools/list","params":{}}'

# Call the add tool
curl -s -X POST http://localhost:8080/mcp \
  -H 'Content-Type: application/json' \
  -H "Authorization: Bearer $TOKEN" \
  -d '{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"add","arguments":{"a":17,"b":25}}}'
```

See `mcp-server/example/SKILL.md` for curl examples covering all 15 endpoints.

## Project Structure

```
mcp-types/               # Core protocol types package
├── src/MCP/
│   ├── Aeson.hs             # Custom Aeson parsing options
│   ├── Protocol.hs          # JSON-RPC protocol messages
│   └── Types.hs             # Core MCP data types
└── mcp-types.cabal

mcp-server/               # Server implementation package
├── src/MCP/
│   ├── Server.hs            # Re-exports Common, HTTP, and Stdio
│   └── Server/
│       ├── Common.hs        # Types, state, request routing, tool helpers
│       ├── HTTP.hs          # Servant-based HTTP transport with JWT auth
│       └── Stdio.hs         # Stdio transport
├── test/
│   ├── Main.hs              # Test entry point
│   └── MCP/
│       ├── Integration.hs       # HTTP integration tests (hspec-wai)
│       ├── StdioIntegration.hs  # Stdio integration tests
│       ├── TestServer.hs        # Test server configuration
│       └── TestUtils.hs         # Test utilities and request builders
├── example/
│   ├── Main.hs              # Example server (tools, resources, prompts, etc.)
│   ├── mcp-example.cabal    # Standalone cabal project
│   ├── mcp-config.json      # Claude Desktop MCP configuration template
│   └── SKILL.md             # Build, run, and test instructions
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
