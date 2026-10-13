# Changelog

## Unreleased

### Changed
- **Breaking**: A POST to `/mcp` that carries a notification or a response is
  now answered with `202 Accepted` and no body, as the Streamable HTTP
  transport requires. A request is still answered with its JSON-RPC messages as
  Server-Sent Events. A response to a request the server never sent is answered
  with `400 Bad Request`.
- **Breaking**: The POST route of `MCPAPI` and `SimpleHTTPAPI` is a `MultiVerb`
  over `MCPPostResponses`, and `handleMCPRequest` answers `MCPPostResult`.
- Requires `servant` and `servant-server` 0.20.3 or later, and
  `servant-auth-server` before 0.4.11.

## 0.3.0.1 — Package Split

### Changed
- **Breaking**: Split `mcp` into two packages:
  - `mcp-types`: Core protocol types (`MCP.Types`, `MCP.Protocol`, `MCP.Aeson`) with minimal dependencies.
  - `mcp`: Servant-based server (`MCP.Server`) depending on `mcp-types`.
- The `MCP` module has been renamed to `MCP.Server`.
- `MCP.Aeson` is now an exposed module in `mcp-types` (was `other-modules`).

### Migration
1. Change `import MCP` to `import MCP.Server`.
2. If you only need types, depend on `mcp-types` instead of `mcp`.

## 0.1.0.0

### Added
- Initial release.
- JSON-RPC 2.0 protocol layer for MCP (`MCP.Protocol`).
- Core MCP types: resources, tools, prompts, capabilities, and content types (`MCP.Types`).
- MCP server implementation with JWT authentication, request routing, and handler framework (`MCP`).
- Integration test suite covering server lifecycle, authentication, protocol flows, endpoints, and error handling.
