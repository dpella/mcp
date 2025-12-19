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
  - `/.well-known/oauth-protected-resource` - Protected resource metadata
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
  - Protected resource metadata (RFC 8414 Section 3)
  - WWW-Authenticate headers on 401 responses with bearer token realm
- Configurable OAuth providers (Google, GitHub, custom)
- JWT support via servant-auth-server

Both transports use the same MCPServer typeclass, so server implementations work with either transport method.

### Entry Points (MCP.Server.HTTP)

The HTTP module provides two entry points:
- **mcpApp**: Basic MCP HTTP server without OAuth (unprotected `/mcp` endpoint)
- **mcpAppWithOAuth**: Full OAuth 2.1-protected server with login flow, imported from `Servant.OAuth2.IDP`

Use `mcpApp` for development/testing, `mcpAppWithOAuth` for production with authentication.

## OAuth Implementation Details

The OAuth implementation uses a **typeclass-based architecture** for pluggable backends.

### Typeclass Architecture (004-oauth-auth-typeclasses)

**OAuthStateStore** (`Servant.OAuth2.IDP.Store`):
- Manages OAuth state persistence (codes, tokens, clients, sessions)
- Associated types: `OAuthStateError m`, `OAuthStateEnv m`, `OAuthUser m`
- Methods: `storeAuthCode`, `lookupAuthCode`, `deleteAuthCode`, `storeAccessToken`, `lookupAccessToken`, `storeRefreshToken`, `lookupRefreshToken`, `updateRefreshToken`, `storeClient`, `lookupClient`, `storePendingAuth`, `lookupPendingAuth`, `deletePendingAuth`
- Default: In-memory TVar implementation (`Servant.OAuth2.IDP.Store.InMemory`)

**AuthBackend** (`Servant.OAuth2.IDP.Auth.Backend`):
- Validates user credentials and returns authenticated user data
- Associated types: `AuthBackendError m`, `AuthBackendEnv m`, `AuthBackendUser m`
- Method: `validateCredentials :: Username -> PlaintextPassword -> m (Maybe (AuthBackendUser m))`
- Default: Demo hardcoded credentials (`Servant.OAuth2.IDP.Auth.Demo`)

**MonadTime** (`Control.Monad.Time`):
- Re-export of `Control.Monad.Time.MonadTime`
- Used by OAuthStateStore for expiry checks

### OAuth Modules:
- **Servant.OAuth2.IDP.Types** - Newtypes: `AuthCodeId`, `ClientId`, `SessionId`, `AccessTokenId`, `RefreshTokenId`, `RedirectUri`, `Scope`, `CodeChallenge`, `CodeVerifier`
- **Servant.OAuth2.IDP.Store** - OAuthStateStore typeclass definition
- **Servant.OAuth2.IDP.Store.InMemory** - TVar-based default implementation
- **Servant.OAuth2.IDP.Boundary** - Servant handler boundary translation
- **Servant.OAuth2.IDP.Auth.Backend** - AuthBackend typeclass definition
- **Servant.OAuth2.IDP.Auth.Demo** - Demo credentials implementation

### Composite Types (Three-Layer Cake Pattern)

**AppEnv** (`MCP.Server.HTTP.AppEnv`):
```haskell
data AppEnv = AppEnv
  { envOAuth  :: OAuthTVarEnv        -- OAuth state storage
  , envAuth   :: DemoCredentialEnv   -- Credential authentication
  , envConfig :: HTTPServerConfig    -- Server config
  , envTracer :: IOTracer HTTPTrace  -- Structured tracing
  }
```

**AppError** (Sum type for all error sources):
```haskell
data AppError
  = OAuthStoreErr OAuthStoreError  -- Storage errors → 500
  | AuthBackendErr DemoAuthError   -- Auth failures → 401
  | ValidationErr Text             -- Input errors → 400
  | ServerErr ServerError          -- Servant passthrough
```

Uses `generic-lens` with `HasType` constraints for composable environment/error access.

### Key Data Types:
- **HTTPServerConfig** - Server config with httpBaseUrl, httpProtocolVersion
- **OAuthConfig** - OAuth settings (timing, demo settings, parameters)
- **AuthorizationCode user** - PKCE code with expiry, client, user, scopes (parameterized by user type)
- **ClientInfo** - Registered client metadata (redirect URIs, grant types)
- **PendingAuthorization** - OAuth params awaiting login approval
- **AuthUser** - Authenticated user (ToJWT/FromJWT)
- **ExpiryConfig** - Configurable expiry times

### Implementing Custom Backends

**Custom OAuthStateStore** (e.g., PostgreSQL):
```haskell
instance (MonadIO m, MonadTime m) => OAuthStateStore (ReaderT PostgresEnv m) where
  type OAuthStateError (ReaderT PostgresEnv m) = SqlError
  type OAuthStateEnv (ReaderT PostgresEnv m) = PostgresEnv
  type OAuthUser (ReaderT PostgresEnv m) = DbUser  -- Your user type (contains user ID as field)
  storeAuthCode = ...  -- INSERT into auth_codes table
  lookupAuthCode = ... -- SELECT with expiry check
```

**Custom AuthBackend** (e.g., LDAP):
```haskell
instance MonadIO m => AuthBackend (ReaderT LdapConfig m) where
  type AuthBackendError (ReaderT LdapConfig m) = LdapError
  type AuthBackendEnv (ReaderT LdapConfig m) = LdapConfig
  type AuthBackendUser (ReaderT LdapConfig m) = LdapUser  -- Your user type (contains user ID as field)
  validateCredentials user pass = do
    result <- ldapBind user pass
    pure result  -- Return user on success, Nothing on failure
```

**Type Bridging**: When using both typeclasses together, ensure the user types align:
```haskell
-- Handlers requiring both backends use type equality constraint:
handleLogin :: (AuthBackend m, OAuthStateStore m,
                AuthBackendUser m ~ OAuthUser m) => ...
```

### Important Implementation Notes:
1. **Client Registration**: Returns configurable client_secret (default empty string for public clients)
2. **PKCE Validation**: Uses validateCodeVerifier from Servant.OAuth2.IDP.Auth
3. **Token Generation**:
   - Authorization codes: UUID v4 with configurable prefix (default "code_")
   - Access tokens: JWT tokens using servant-auth-server's makeJWT
   - Refresh tokens: UUID v4 with configurable prefix (default "rt_")
4. **Expiry Times**: Configurable auth code and access token expiry (defaults: 10 min, 1 hour)
5. **Demo Mode**: Configurable auto-approval and demo user generation
6. **JWT Integration**: Proper JWT tokens fix authentication loops with MCP clients
7. **Production Ready**: All hardcoded values extracted to configuration parameters
8. **Thread Safety**: In-memory implementation uses STM transactions

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

### Interactive Login Flow (002-login-auth-page):

The OAuth authorization endpoint now implements an interactive login page instead of auto-approval:

**Key Implementation Patterns**:
1. **Session Management**: Uses session cookies (`mcp_session`) to track pending authorizations
   - `PendingAuthorization` type stores OAuth parameters during login
   - Session IDs are UUIDs tracked in `OAuthState.pendingAuthorizations`
   - Configurable expiry via `loginSessionExpirySeconds` (default: 10 minutes)

2. **Credential Storage**: New `CredentialStore` type with `HashedPassword` (SHA256)
   - Use `mkHashedPassword` smart constructor for password hashing
   - `validateCredential` performs constant-time comparison
   - `defaultDemoCredentialStore` provides demo/demo123 and admin/admin456

3. **HTML Content Type**: Custom `HTML` content type for Servant
   - `Accept` instance for `text/html`
   - `MimeRender` instance to serve HTML responses
   - Used for login page, error pages, and redirects

4. **Form Handling**: `LoginForm` type with `FromForm` instance
   - Includes hidden `session_id` field to link form submission to pending auth
   - Validates username/password on submission
   - Handles both "approve" and "deny" actions

5. **Error Handling**: Comprehensive edge case coverage
   - Invalid OAuth parameters → error page (don't redirect untrusted URIs)
   - Expired sessions → detect via timestamp comparison
   - Cookies disabled → check for missing/mismatched session cookie
   - Unregistered client → error page with client_id
   - Invalid redirect_uri → error page (security: never redirect to unvalidated URIs)

6. **Security Patterns**:
   - Never redirect to untrusted redirect_uri (validate against registered clients)
   - Constant-time password comparison to prevent timing attacks
   - Session cookies with HttpOnly semantics
   - PKCE code_challenge stored in PendingAuthorization for later validation

**Testing with Interactive Login**:
```bash
# Start server with OAuth (displays demo credentials on startup)
cabal run mcp-http -- --oauth

# Test metadata discovery
curl http://localhost:8080/.well-known/oauth-authorization-server
curl http://localhost:8080/.well-known/oauth-protected-resource

# Test 401 with WWW-Authenticate header
curl -i http://localhost:8080/mcp

# Interactive login flow (requires browser or headless automation)
# 1. Open /authorize in browser
# 2. Enter credentials: demo/demo123 or admin/admin456
# 3. Click "Approve" or "Deny"
# 4. Redirect contains authorization code (approve) or error (deny)

# Note: ./examples/oauth-client-demo.sh is INCOMPATIBLE with interactive login
# The script expects auto-approval mode (autoApproveAuth=true)
# To use with script, would need headless browser automation (puppeteer, selenium)
```

## Active Technologies
- Haskell GHC2021 (GHC 9.4+) + servant-server 0.19-0.20, servant-auth-server 0.4, warp 3.3, jose 0.10-0.11, aeson 2.1-2.2 (001-claude-mcp-connector)
- In-memory (TVar-based state for OAuth codes, tokens, clients) (001-claude-mcp-connector)
- Haskell GHC2021 (GHC 9.4+) + servant-server 0.19-0.20, warp 3.3, aeson 2.1-2.2, cryptonite 0.30, jose 0.10-0.11 (002-login-auth-page)
- In-memory (TVar-based state management, consistent with existing OAuth state storage) (002-login-auth-page)
- N/A (traces emitted to user-provided IOTracer) (003-structured-tracing)
- Haskell GHC2021 (GHC 9.4+ via base ^>=4.18.2.1) + servant-server 0.19-0.20, servant-auth-server 0.4, jose 0.10-0.11, cryptonite 0.30, stm 2.5, mtl 2.3, aeson 2.1-2.2, generic-lens (to add) (004-oauth-auth-typeclasses)
- In-memory (TVar-based) for default implementation; typeclass enables PostgreSQL/Redis backends (004-oauth-auth-typeclasses)
- Haskell GHC2021 (GHC 9.4+ via base ^>=4.18.2.1) + servant-server 0.19-0.20, servant-auth-server 0.4, jose 0.10-0.11, cryptonite 0.30, stm 2.5, mtl 2.3, aeson 2.1-2.2, generic-lens (to add), network-uri (to add) (004-oauth-auth-typeclasses)

## Recent Changes
- 004-oauth-auth-typeclasses: Refactored OAuth to typeclass-based architecture
  - Added OAuthStateStore typeclass for pluggable state persistence
  - Added AuthBackend typeclass for pluggable credential validation
  - Created OAuth.Types module with validated newtypes (AuthCodeId, ClientId, SessionId, etc.)
  - Implemented three-layer cake pattern with AppEnv/AppError composite types
  - Default implementations: in-memory TVar (OAuthStateStore), demo credentials (AuthBackend)
  - Uses generic-lens for composable error/environment handling
- 002-login-auth-page: Implemented interactive login page with credential authentication
  - Replaced auto-approval OAuth flow with secure login form
  - Added session cookie management and credential validation
  - Implemented comprehensive edge case handling (expired sessions, cookies disabled, invalid parameters)
  - Created reusable HTML rendering patterns for Servant
  - Security: constant-time password comparison, SHA256 hashing, redirect URI validation
- 001-claude-mcp-connector: Added Haskell GHC2021 (GHC 9.4+) + servant-server 0.19-0.20, servant-auth-server 0.4, warp 3.3, jose 0.10-0.11, aeson 2.1-2.2
