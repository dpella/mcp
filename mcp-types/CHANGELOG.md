# Changelog

## 0.1.1

### Added
- `sERVER_NOT_INITIALIZED` error code constant (-32002) in `MCP.Protocol`.

## 0.1.0

### Added
- Initial release, split from the `mcp` package.
- `MCP.Types`: Core MCP data types (Content, Resource, Tool, Prompt, Capability, etc.)
- `MCP.Protocol`: JSON-RPC 2.0 message wrappers and all client/server request/response types.
- `MCP.Aeson`: Custom Aeson parsing options.

### Fixed
- Make `params` optional in `JSONRPCRequest`, `JSONRPCNotification`,
  `ViaJSONRPCRequest`, and `ViaJSONRPCNotification` `FromJSON` instances.
  JSON-RPC 2.0 allows omitting the `params` field; previously a missing
  `params` caused requests to be silently misparsed as notifications
  (losing the `id` and never receiving a response).
