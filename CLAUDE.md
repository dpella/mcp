# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands

This is a Haskell project using Cabal as its build system.

### Common development commands:
- `cabal build` - Build the project (all executables)
- `cabal run mcp` - Run the StdIO MCP server
- `cabal run mcp-http` - Run the HTTP MCP server example
- `cabal test` - Run the test suite
- `cabal repl` - Start a GHCi REPL with the project loaded
- `cabal clean` - Clean build artifacts
- `hlint .` - Run linter on all files (**CRITICAL:** MUST run after edits and fix ALL warnings and/or errors. hlint . must return zero hints before any task is complete)

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

### Servant.OAuth2.IDP Module Structure

**Core Invariant**: All `Servant.OAuth2.IDP.*` modules have **zero MCP dependencies** - this enables future package extraction to standalone OAuth library.

#### Protocol & Configuration
- **Servant.OAuth2.IDP.Config** - `OAuthEnv` record (protocol-level OAuth configuration: timing, PKCE, token parameters, supported grant types, etc.)
- **Servant.OAuth2.IDP.Metadata** - `OAuthMetadata`, `ProtectedResourceMetadata` types (RFC 8414/RFC 9728 discovery)
- **Servant.OAuth2.IDP.PKCE** - PKCE functions (`generateCodeVerifier`, `validateCodeVerifier`, `generateCodeChallenge`)

#### Types & Errors
- **Servant.OAuth2.IDP.Types** - Core newtypes (`AuthCodeId`, `ClientId`, `SessionId`, `AccessTokenId`, `RefreshTokenId`, `RedirectUri`, `Scope`, `CodeChallenge`, `CodeVerifier`, `OAuthGrantType`) with crypto-random ID generators
- **Servant.OAuth2.IDP.Errors** - Comprehensive error types:
  - `ValidationError` - Semantic validation errors
  - `AuthorizationError` - OAuth protocol errors with reason ADTs
  - `LoginFlowError` - Login flow errors (HTML rendering)
  - `OAuthErrorCode` - RFC 6749 compliant error codes
  - `OAuthError m` - Unified error type for all OAuth errors
  - `oauthErrorToServerError` - Converts to Servant `ServerError`

#### State Management
- **Servant.OAuth2.IDP.Store** - OAuthStateStore typeclass definition
- **Servant.OAuth2.IDP.Store.InMemory** - TVar-based default implementation
- **Servant.OAuth2.IDP.Auth.Backend** - AuthBackend typeclass definition
- **Servant.OAuth2.IDP.Auth.Demo** - Demo credentials implementation

#### Handlers
- **Servant.OAuth2.IDP.Handlers** - Re-exports all handlers
- **Servant.OAuth2.IDP.Handlers.Authorization** - OAuth /authorize endpoint
- **Servant.OAuth2.IDP.Handlers.Login** - Interactive login flow
- **Servant.OAuth2.IDP.Handlers.Metadata** - OAuth metadata discovery endpoints
- **Servant.OAuth2.IDP.Handlers.Registration** - Dynamic client registration
- **Servant.OAuth2.IDP.Handlers.Token** - Token exchange endpoint
- **Servant.OAuth2.IDP.Handlers.HTML** - HTML rendering (login page, error pages)

#### Infrastructure
- **Servant.OAuth2.IDP.Trace** - `OAuthTrace` ADT with domain types for structured tracing
- **Servant.OAuth2.IDP.Server** - OAuth API composition and server wiring
- **Servant.OAuth2.IDP.API** - Servant API type definitions
- **Servant.OAuth2.IDP.Test.Internal** - Test-only unsafe constructors (for testing)

### Composite Types (Three-Layer Cake Pattern)

**AppEnv** (`MCP.Server.HTTP.AppEnv`):
```haskell
data AppEnv = AppEnv
  { envOAuth       :: OAuthTVarEnv           -- OAuth state storage
  , envAuth        :: DemoCredentialEnv      -- Credential authentication
  , envConfig      :: HTTPServerConfig       -- Server config
  , envTracer      :: IOTracer HTTPTrace     -- HTTP-level tracing
  , envOAuthEnv    :: OAuthEnv               -- Protocol-level OAuth config
  , envOAuthTracer :: IOTracer OAuthTrace    -- OAuth-specific tracing
  , envJWT         :: JWTSettings            -- JWT signing/validation
  , envServerState :: TVar ServerState       -- MCP server state
  , envTimeProvider :: Maybe (TVar UTCTime)  -- Test time control (Nothing = real time)
  }
```

**AppError** (Sum type for all error sources):
```haskell
data AppError
  = OAuthStoreErr OAuthStoreError       -- Storage errors → 500
  | AuthBackendErr DemoAuthError        -- Auth failures → 401
  | ValidationErr ValidationError       -- Semantic validation → 400
  | AuthorizationErr AuthorizationError -- OAuth protocol errors → 400/401/403
  | LoginFlowErr LoginFlowError         -- Login flow errors → 400 (HTML)
```

Uses `generic-lens` with `HasType` constraints for composable environment/error access.

**OAuthError** (`Servant.OAuth2.IDP.Errors`) - Unified error type for OAuth handlers:
```haskell
data OAuthError m
  = OAuthValidation ValidationError     -- Semantic validation → 400
  | OAuthAuthorization AuthorizationError -- OAuth protocol → 400/401/403
  | OAuthLoginFlow LoginFlowError         -- Login flow → 400 (HTML)
  | OAuthStore (OAuthStateError m)        -- Storage backend → 500
```

- Parameterized by monad `m` for associated `OAuthStateError` type
- `oauthErrorToServerError` converts to Servant `ServerError` with proper HTTP status/body
- `appErrorToServerError` in `MCP.Server.HTTP.AppEnv` wraps `oauthErrorToServerError` for MCP layer

### Configuration Types: OAuthEnv vs MCPOAuthConfig

The OAuth configuration uses two distinct types:

**OAuthEnv** (`Servant.OAuth2.IDP.Config`) - Protocol-level OAuth settings:
- **Timing**: `oauthAuthCodeExpiry`, `oauthAccessTokenExpiry`, `oauthLoginSessionExpiry`
- **OAuth Parameters**: `oauthSupportedScopes`, `oauthSupportedResponseTypes`, `oauthSupportedGrantTypes`, `oauthSupportedAuthMethods`, `oauthSupportedCodeChallengeMethods`
- **Token Prefixes**: `oauthAuthCodePrefix`, `oauthRefreshTokenPrefix`, `oauthClientIdPrefix`
- **Security**: `oauthRequireHTTPS`, `oauthBaseUrl`
- **Resource Server**: `oauthResourceServerBaseUrl`, `oauthResourceServerMetadata`
- **Branding**: `oauthServerName` (for HTML templates, e.g., "MCP Server", "OAuth Server"), `oauthScopeDescriptions` (human-readable scope descriptions for consent page)
- **Location**: Lives in `AppEnv.envOAuthEnv` (always present when OAuth wired)

**MCPOAuthConfig** (`MCP.Server.Auth`) - MCP-specific settings:
- **Demo Mode**: `autoApproveAuth`, `demoUserIdTemplate`, `demoEmailDomain`, `demoUserName`
- **Providers**: `oauthProviders` (list of external OAuth identity providers)
- **Templates**: `authorizationSuccessTemplate` (HTML template for success page)
- **Client Secrets**: `publicClientSecret` (secret returned for public clients)
- **Location**: Lives in `HTTPServerConfig.httpMCPOAuthConfig :: Maybe MCPOAuthConfig` (presence = OAuth enabled)

**DemoOAuthBundle** - Convenience type for test/demo setups:
- Bundles `OAuthEnv` + `MCPOAuthConfig` together
- `defaultDemoOAuthBundle` provides sensible test defaults
- Exported from `MCP.Server.HTTP` for test convenience

**Other Key Types**:
- **HTTPServerConfig** - Server config with httpBaseUrl, httpProtocolVersion, httpMCPOAuthConfig
- **AuthorizationCode user** - PKCE code with expiry, client, user, scopes (parameterized by user type)
- **ClientInfo** - Registered client metadata (redirect URIs, grant types)
- **PendingAuthorization** - OAuth params awaiting login approval
- **AuthUser** - Authenticated user (ToJWT/FromJWT)

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
2. **PKCE Validation**: Uses `validateCodeVerifier` from `Servant.OAuth2.IDP.PKCE`
3. **Token Generation**:
   - Authorization codes: UUID v4 with configurable prefix (from OAuthEnv)
   - Access tokens: JWT tokens using servant-auth-server's makeJWT
   - Refresh tokens: UUID v4 with configurable prefix (from OAuthEnv)
4. **Expiry Times**: Configurable auth code and access token expiry (in OAuthEnv)
5. **Demo Mode**: Configurable auto-approval and demo user generation (in MCPOAuthConfig)
6. **JWT Integration**: Proper JWT tokens fix authentication loops with MCP clients
7. **Production Ready**: All values configurable via parameters
8. **Thread Safety**: In-memory implementation uses STM transactions
9. **Smart Constructor Hygiene**: All domain newtypes export only smart constructors (not raw constructors) to prevent validation bypass
10. **Type-Driven Design**: Domain types flow throughout the system without mid-flow Text conversions

### Configuration Usage

**For demo/testing**, use `defaultDemoOAuthBundle` (from `MCP.Server.HTTP`):
```haskell
let bundle = defaultDemoOAuthBundle
    oauthEnv = bundleEnv bundle
    mcpConfig = bundleMCPConfig bundle
```

**For production**, configure `OAuthEnv` and `MCPOAuthConfig` separately:
```haskell
-- Protocol-level configuration (Servant.OAuth2.IDP.Config)
oauthEnv = OAuthEnv
  { oauthRequireHTTPS = True
  , oauthBaseUrl = parseURI "https://api.example.com"
  , oauthAuthCodeExpiry = 600  -- 10 minutes
  , oauthAccessTokenExpiry = 3600  -- 1 hour
  , ...
  }

-- MCP-specific configuration (MCP.Server.Auth)
mcpConfig = MCPOAuthConfig
  { autoApproveAuth = False  -- Require user approval
  , demoUserIdTemplate = Nothing  -- Disable demo users
  , ...
  }
```

### Interactive Login Flow

The OAuth authorization endpoint implements an interactive login page with credential authentication:

**Key Implementation Patterns**:
1. **Session Management**: Uses session cookies (`mcp_session`) to track pending authorizations
   - `PendingAuthorization` type stores OAuth parameters during login
   - Session IDs are UUIDs tracked in `OAuthState.pendingAuthorizations`
   - Configurable expiry via `loginSessionExpirySeconds` (default: 10 minutes)

2. **Credential Storage**: `CredentialStore` type with `HashedPassword` (SHA256)
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

## Technology Stack
- **Language**: Haskell GHC2021 (GHC 9.4+ via base ^>=4.18.2.1)
- **Web Framework**: servant-server 0.19-0.20, servant-auth-server 0.4, warp 3.3
- **Cryptography**: jose 0.10-0.11, cryptonite 0.30
- **Serialization**: aeson 2.1-2.2
- **Concurrency**: stm 2.5
- **Effects**: mtl 2.3, monad-time
- **Lenses**: generic-lens
- **Networking**: network-uri
- **Storage**: In-memory (TVar-based) by default; typeclass enables PostgreSQL/Redis backends
- **Tracing**: User-provided IOTracer for structured trace emission

## Active Technologies
- Haskell GHC2021 (GHC 9.4+ via base ^>=4.18.2.1) + servant-server 0.19-0.20, servant-auth-server 0.4, aeson 2.1-2.2, jose 0.10-0.11, crypton, monad-time (006-servant-oauth2-idp-package)
- In-memory (TVar-based via STM); typeclass abstraction for backends (006-servant-oauth2-idp-package)

## Recent Changes
- 006-servant-oauth2-idp-package: Added Haskell GHC2021 (GHC 9.4+ via base ^>=4.18.2.1) + servant-server 0.19-0.20, servant-auth-server 0.4, aeson 2.1-2.2, jose 0.10-0.11, crypton, monad-time
