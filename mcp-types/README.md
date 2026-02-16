# mcp-types — MCP protocol types for Haskell

[![mcp-types on Hackage](https://img.shields.io/hackage/v/mcp-types.svg)](https://hackage.haskell.org/package/mcp-types)

Core type definitions and JSON-RPC protocol layer for the
[Model Context Protocol (MCP)](https://modelcontextprotocol.io/).

This package contains only the pure protocol types with minimal dependencies,
Implements MCP protocol version **2025-06-18**, making it suitable for
building both clients and servers on any framework.
For a ready-made Servant-based server, see the
[`mcp`](https://hackage.haskell.org/package/mcp) package.

## Modules

- **`MCP.Types`** — Core MCP data types: Content, Resource, Tool, Prompt,
  Capability, Implementation, and more.
- **`MCP.Protocol`** — JSON-RPC 2.0 message wrappers, all client/server
  request/response types, notification types, and protocol constants.
  Re-exports the [`jsonrpc`](https://hackage.haskell.org/package/jsonrpc)
  transport types.
- **`MCP.Aeson`** — Custom Aeson parsing options used internally.

## Install

```cabal
build-depends:
    base
  , aeson
  , mcp-types
```

## MCP Protocol Support

| Operation | Description |
|-----------|-------------|
| `initialize` | Start session and negotiate capabilities |
| `ping` | Health check |
| `resources/list` | List available resources |
| `resources/templates/list` | List resource templates |
| `resources/read` | Read resource contents |
| `resources/subscribe` | Subscribe to resource updates |
| `resources/unsubscribe` | Unsubscribe from resource updates |
| `prompts/list` | List available prompts |
| `prompts/get` | Get prompt with arguments |
| `tools/list` | List available tools |
| `tools/call` | Execute a tool |
| `completion/complete` | Auto-completion with context |
| `logging/setLevel` | Set logging level |
| `sampling/createMessage` | Request LLM sampling |
| `roots/list` | List client root directories |
| `elicitation/create` | Request user input via forms |

## License

MPL-2.0
