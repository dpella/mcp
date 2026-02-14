# Changelog

## 0.2.0.0 — Package Split

### Changed
- **Breaking**: Split `mcp` into two packages:
  - `mcp-protocol`: Core protocol types (`MCP.Types`, `MCP.Protocol`, `MCP.Aeson`) with minimal dependencies.
  - `mcp`: Servant-based server (`MCP.Server`) depending on `mcp-protocol`.
- The `MCP` module has been renamed to `MCP.Server`.
- `MCP.Aeson` is now an exposed module in `mcp-protocol` (was `other-modules`).

### Migration
1. Change `import MCP` to `import MCP.Server`.
2. If you only need types, depend on `mcp-protocol` instead of `mcp`.

## 0.1.0.0

### Added
- Initial release.
- JSON-RPC 2.0 protocol layer for MCP (`MCP.Protocol`).
- Core MCP types: resources, tools, prompts, capabilities, and content types (`MCP.Types`).
- MCP server implementation with JWT authentication, request routing, and handler framework (`MCP`).
- Integration test suite covering server lifecycle, authentication, protocol flows, endpoints, and error handling.
