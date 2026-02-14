# Changelog

## 0.1.0.0

### Added
- Protocol types extracted into `mcp-protocol`; server remains as `mcp`.
- `MCP.Server`: Servant-based MCP server with JWT authentication, request routing, and handler framework.
- Re-exports `MCP.Protocol` and `MCP.Types` from `mcp-protocol` for convenience.
- Integration test suite covering server lifecycle, authentication, protocol flows, endpoints, and error handling.

### Migration
- Change `import MCP` to `import MCP.Server`.
- If you only need types, depend on `mcp-protocol` instead of `mcp`.
