# Changelog

## 0.3.1.0

### Added
- Simple unauthenticated HTTP transport (`SimpleHTTPAPI` / `simpleHttpApp`) in
  `MCP.Server.HTTP`, for local development or use behind a reverse proxy.
- `MCP.Server.HTTP.Internal`: shared handler core factored out of the HTTP
  transport, eliminating duplication between the JWT and simple transports.
- `--simple-http` flag in the example server.
- Integration tests for the simple HTTP transport.

## 0.3.0.1

### Added
- Protocol types extracted into `mcp-types`; server remains as `mcp`.
- `MCP.Server.Common`: transport-agnostic types, state, request routing, and tool helpers.
- `MCP.Server.HTTP`: Servant-based HTTP transport with JWT authentication and SSE responses.
- `MCP.Server.Stdio`: stdio transport reading/writing JSON-RPC messages line-by-line.
- `MCP.Server`: re-exports all three submodules for backwards compatibility.
- Re-exports `MCP.Protocol` and `MCP.Types` from `mcp-types` for convenience.
- Integration test suite covering both HTTP and stdio transports.

### Migration
- Change `import MCP` to `import MCP.Server`.
- If you only need types, depend on `mcp-types` instead of `mcp`.
