# mcp — MCP server for Haskell

[![mcp on Hackage](https://img.shields.io/hackage/v/mcp.svg)](https://hackage.haskell.org/package/mcp)

A complete server implementation of the
[Model Context Protocol (MCP)](https://modelcontextprotocol.io/) for Haskell,
built on Servant.

Implements MCP protocol version **2025-06-18**. Re-exports the core protocol types from
[`mcp-types`](https://hackage.haskell.org/package/mcp-types) for convenience.

## Modules

- **`MCP.Server`** — Re-exports everything below for convenience.
- **`MCP.Server.Common`** — Transport-agnostic core: types, state management,
  request routing, `ProcessHandlers`, `ToolHandler` framework.
- **`MCP.Server.HTTP`** — Servant-based HTTP transports with streaming SSE
  responses at the `/mcp` endpoint.  Provides both a JWT-authenticated API
  (`MCPAPI` / `mcpAPI`) and a simple unauthenticated API
  (`SimpleHTTPAPI` / `simpleHttpApp`) for local development or use behind a
  reverse proxy.
- **`MCP.Server.Stdio`** — Stdio transport reading/writing JSON-RPC messages
  line-by-line, suitable for subprocess-based integrations.

## Install

```cabal
build-depends:
    base
  , servant
  , servant-server
  , servant-auth-server
  , aeson
  , mcp
```

If you only need the protocol types (e.g. for a client), depend on
[`mcp-types`](https://hackage.haskell.org/package/mcp-types) instead.

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

A fully documented example server lives in
[`mcp-server/example/`](https://github.com/DPella/mcp/tree/main/mcp-server/example).

## Features

- **Three transports**: HTTP with JWT auth, simple unauthenticated HTTP, and stdio
- **JWT authentication** via `servant-auth-server` (authenticated HTTP transport)
- **Simple HTTP** for local development or behind a reverse proxy (`simpleHttpApp`)
- **Extensible handler framework**: `ProcessHandlers` record with optional
  handlers for each MCP method
- **Tool helpers**: `ToolHandler`, `toolHandler`, `withToolHandlers`,
  `toolTextResult`, `toolTextError`
- **Server-to-client requests**: `ProcessClientInput` for sampling,
  elicitation, and other client callbacks on both transports
- **MCP 2025-06-18**: Full protocol version support

## License

MPL-2.0
