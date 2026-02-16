# Changelog

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
