# Changelog

## 0.1.0.0

### Added
- Initial release, split from the `mcp` package.
- `MCP.Server`: Servant-based MCP server with JWT authentication, request routing, and handler framework.
- Re-exports `MCP.Protocol` and `MCP.Types` from `mcp-protocol` for convenience.
- Integration test suite covering server lifecycle, authentication, protocol flows, endpoints, and error handling.

### Migration from `mcp`
- Replace `mcp` with `mcp-server` (and optionally `mcp-protocol`) in your `build-depends`.
- Change `import MCP` to `import MCP.Server`.
