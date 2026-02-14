# MCP — Model Context Protocol Server for Haskell

A Haskell implementation of the [Model Context Protocol](https://modelcontextprotocol.io/) (MCP) server, built on Servant and `servant-auth-server`.

MCP is a standardized JSON-RPC interface that allows AI assistants and other tools to interact with services through a uniform protocol.

## Features

- JSON-RPC 2.0 protocol layer
- Resource access (list and read)
- Tool execution with argument validation
- Prompt templates with parameterization
- Completion/autocomplete support
- JWT authentication via `servant-auth-server`
- Streaming responses via Server-Sent Events

## Install

Add the package to your build-depends:

```cabal
build-depends:
    base
  , servant
  , servant-server
  , servant-auth-server
  , aeson
  , mcp  -- this package
```

This project targets GHC 9.12 (see `cabal.project`).

## Development

```bash
cabal build
cabal test
```

## License

See `LICENSE` in this repository (MPL-2.0).
