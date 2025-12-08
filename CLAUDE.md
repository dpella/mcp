# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Issue Tracking with bd (beads)

**IMPORTANT**: This project uses **bd (beads)** for ALL issue tracking. Do NOT use markdown TODOs, task lists, or other tracking methods.

### Quick Reference

```bash
bd ready --json                    # Check for unblocked work
bd create "Title" -t bug|feature|task -p 0-4 --json
bd update <id> --status in_progress --json
bd close <id> --reason "Done" --json
```

### Workflow

1. **Check ready work**: `bd ready --json`
2. **Claim task**: `bd update <id> --status in_progress`
3. **Work on it**: Implement, test, document
4. **Discover new work?** `bd create "Found bug" -p 1 --deps discovered-from:<parent-id> --json`
5. **Complete**: `bd close <id> --reason "Done"`
6. **Commit together**: Always commit `.beads/issues.jsonl` with code changes

### Priorities

- `0` - Critical (security, data loss, broken builds)
- `1` - High (major features, important bugs)
- `2` - Medium (default)
- `3` - Low (polish, optimization)
- `4` - Backlog (future ideas)

## Build Commands

This is a Haskell project using Cabal as its build system.

### Common development commands:
- `cabal build` - Build the project (all executables)
- `cabal run mcp` - Run the StdIO MCP server
- `cabal run mcp-http` - Run the HTTP MCP server example
- `cabal test` - Run the test suite
- `cabal repl` - Start a GHCi REPL with the project loaded
- `cabal clean` - Clean build artifacts

## Project Architecture

This is an implementation of the Model Context Protocol (MCP) for Haskell. The project structure follows standard Haskell conventions:

### Core Modules:
- **src/MCP/Types.hs** - Core MCP data types and JSON serialization
- **src/MCP/Protocol.hs** - JSON-RPC protocol messages and types
- **src/MCP/Server.hs** - Core server infrastructure and MCPServer typeclass

### Transport Implementations:
- **src/MCP/Server/StdIO.hs** - StdIO transport for process-based clients
- **src/MCP/Server/HTTP.hs** - HTTP transport following MCP specification

### Application:
- **app/Main.hs** - Example MCP server implementation (StdIO mode)
- **test/Main.hs** - Test suite entry point (currently a placeholder)
- **schema.ts** - Contains the complete MCP protocol TypeScript schema definitions (2025-06-18)

The project uses GHC2021 language extensions and targets base ^>=4.18.2.1.

## MCP Implementation Notes

The schema.ts file contains the full MCP protocol specification (version 2025-06-18) including:
- Client/Server request and response types
- Notification types for both directions
- Resource, Tool, and Prompt management
- JSON-RPC message formats
- Capability negotiation structures
- Enhanced content blocks and metadata support
- Elicitation system for user input
- OAuth 2.1 authorization framework

When implementing MCP functionality, refer to the schema.ts for the exact message formats and type definitions required by the protocol.

## Transport Options

The implementation supports two transport methods:

### StdIO Transport (MCP.Server.StdIO)
- Uses stdin/stdout for JSON-RPC communication  
- Suitable for process-based MCP clients like Claude Desktop
- Default transport used by `cabal run mcp`

### HTTP Transport (MCP.Server.HTTP)
- RESTful HTTP API with multiple endpoints:
  - `/mcp` - Main MCP endpoint for JSON-RPC requests
  - `/.well-known/oauth-authorization-server` - OAuth metadata discovery
  - `/register` - Dynamic client registration
  - `/authorize` - OAuth authorization with PKCE
  - `/token` - Token exchange endpoint
- Follows the official MCP HTTP transport specification
- Built with Servant and Warp for production use
- Complete OAuth 2.0 implementation:
  - Metadata discovery for automatic configuration
  - Dynamic client registration (no pre-registration needed)
  - Authorization code flow with mandatory PKCE (S256)
  - Bearer token authentication
  - Refresh token support with rotation
  - In-memory storage for demo (can be replaced with persistent storage)
- Security features:
  - PKCE required for all OAuth flows (MCP compliance)
  - 10-minute authorization code expiry
  - 1-hour access token validity
  - Public client support (no client secret)
  - Redirect URI validation
- Configurable OAuth providers (Google, GitHub, custom)
- JWT support via servant-auth-server

Both transports use the same MCPServer typeclass, so server implementations work with either transport method.

## OAuth Implementation Details

When implementing OAuth features or modifying the authentication flow:

### Key Data Types:
- **HTTPServerConfig** - Main server configuration with httpBaseUrl, httpProtocolVersion, etc.
- **OAuthConfig** - Comprehensive OAuth configuration with timing, demo settings, and parameters
- **OAuthState** - Server state containing auth codes, tokens, and registered clients
- **ClientRegistrationRequest/Response** - Dynamic client registration
- **AuthorizationCode** - Stores PKCE challenge and expiry
- **TokenResponse** - Standard OAuth token response format
- **OAuthMetadata** - Server metadata for discovery

### Important Implementation Notes:
1. **Client Registration**: Returns configurable client_secret (default empty string for public clients)
2. **PKCE Validation**: Uses validateCodeVerifier from MCP.Server.Auth
3. **Token Generation**: 
   - Authorization codes: UUID v4 with configurable prefix (default "code_")
   - Access tokens: JWT tokens using servant-auth-server's makeJWT
   - Refresh tokens: UUID v4 with configurable prefix (default "rt_")
4. **Expiry Times**: Configurable auth code and access token expiry (defaults: 10 min, 1 hour)
5. **Demo Mode**: Configurable auto-approval and demo user generation
6. **JWT Integration**: Proper JWT tokens fix authentication loops with MCP clients
7. **Production Ready**: All hardcoded values extracted to configuration parameters

### Configuration Options:

**HTTPServerConfig** now includes:
- `httpBaseUrl`: Base URL for OAuth endpoints (e.g., "https://api.example.com")
- `httpProtocolVersion`: MCP protocol version (default "2025-06-18")

**OAuthConfig** includes comprehensive settings:
- Timing: `authCodeExpirySeconds`, `accessTokenExpirySeconds`
- OAuth parameters: `supportedScopes`, `supportedResponseTypes`, etc.
- Demo mode: `autoApproveAuth`, `demoUserIdTemplate`, `demoEmailDomain`
- Token prefixes: `authCodePrefix`, `refreshTokenPrefix`, `clientIdPrefix`
- Templates: `authorizationSuccessTemplate` for custom responses

**For demo/testing**, use `defaultDemoOAuthConfig` and override specific fields.
**For production**, configure all parameters according to your security requirements.

### Testing OAuth Flow:
```bash
# Start server with OAuth
cabal run mcp-http -- --oauth

# Test metadata discovery
curl http://localhost:8080/.well-known/oauth-authorization-server

# Run full OAuth demo
./examples/oauth-client-demo.sh
```