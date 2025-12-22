# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with the servant-oauth2-idp package.

## What This Is

**servant-oauth2-idp** is a STANDALONE OAuth 2.1 Identity Provider implementation for Servant.

**CRITICAL**: This package has ZERO MCP dependencies. It is designed to be extracted as a standalone OAuth library for general Servant applications.

## Build Commands

This is a Haskell library using Cabal as its build system.

### Common development commands:
- `cabal build` - Build the library
- `cabal test` - Run the test suite
- `cabal repl` - Start a GHCi REPL with the library loaded
- `cabal clean` - Clean build artifacts
- `hlint .` - Run linter on all files (**CRITICAL:** MUST run after edits and fix ALL warnings/errors. hlint . must return zero hints before any task is complete)

## Package Overview

### Synopsis
A complete OAuth 2.1 Authorization Server implementation for Servant with pluggable storage and authentication backends.

### Version
0.1.0.0

### Key Features
- OAuth 2.1 compliant authorization server
- PKCE (RFC 7636) with S256 code challenge method (mandatory)
- JWT access tokens via jose
- Dynamic client registration (RFC 7591)
- Metadata discovery endpoints (RFC 8414, RFC 9728)
- Interactive login flow with HTML UI
- Pluggable storage backends via OAuthStateStore typeclass
- Pluggable authentication via AuthBackend typeclass
- In-memory TVar-based reference implementation
- Structured tracing support
- Production-ready security patterns

## Module Structure

### Core Protocol Modules

- **Servant.OAuth2.IDP.Config** - `OAuthEnv` record (protocol-level OAuth configuration: timing, PKCE, token parameters, supported grant types, etc.)
- **Servant.OAuth2.IDP.Types** - Core newtypes (`AuthCodeId`, `ClientId`, `SessionId`, `AccessTokenId`, `RefreshTokenId`, `RedirectUri`, `Scope`, `CodeChallenge`, `CodeVerifier`, `OAuthGrantType`) with crypto-random ID generators
- **Servant.OAuth2.IDP.Metadata** - `OAuthMetadata`, `ProtectedResourceMetadata` types (RFC 8414/RFC 9728 discovery)
- **Servant.OAuth2.IDP.PKCE** - PKCE functions (`generateCodeVerifier`, `validateCodeVerifier`, `generateCodeChallenge`)
- **Servant.OAuth2.IDP.API** - Servant API type definitions

### Error Handling

- **Servant.OAuth2.IDP.Errors** - Comprehensive error types:
  - `ValidationError` - Semantic validation errors
  - `AuthorizationError` - OAuth protocol errors with reason ADTs
  - `LoginFlowError` - Login flow errors (HTML rendering)
  - `OAuthErrorCode` - RFC 6749 compliant error codes
  - `OAuthError m` - Unified error type for all OAuth errors
  - `oauthErrorToServerError` - Converts to Servant `ServerError`

### Storage & Authentication Typeclasses

- **Servant.OAuth2.IDP.Store** - OAuthStateStore typeclass definition
- **Servant.OAuth2.IDP.Store.InMemory** - TVar-based default implementation
- **Servant.OAuth2.IDP.Auth.Backend** - AuthBackend typeclass definition
- **Servant.OAuth2.IDP.Auth.Demo** - Demo credentials implementation

### Handler Modules

- **Servant.OAuth2.IDP.Handlers** - Re-exports all handlers
- **Servant.OAuth2.IDP.Handlers.Authorization** - OAuth /authorize endpoint
- **Servant.OAuth2.IDP.Handlers.Login** - Interactive login flow
- **Servant.OAuth2.IDP.Handlers.Metadata** - OAuth metadata discovery endpoints
- **Servant.OAuth2.IDP.Handlers.Registration** - Dynamic client registration
- **Servant.OAuth2.IDP.Handlers.Token** - Token exchange endpoint
- **Servant.OAuth2.IDP.Handlers.HTML** - HTML rendering (login page, error pages)

### Infrastructure

- **Servant.OAuth2.IDP.Trace** - `OAuthTrace` ADT with domain types for structured tracing
- **Servant.OAuth2.IDP.Server** - OAuth API composition and server wiring
- **Servant.OAuth2.IDP.Test.Internal** - Test-only unsafe constructors (for testing)

## Key Typeclasses

### OAuthStateStore

Storage interface for OAuth 2.1 server state. Manages authorization codes, tokens, clients, and sessions.

**Purpose**: Abstracts over different storage backends (in-memory, PostgreSQL, Redis) to enable pluggable persistence.

**Associated Types**:
- `type OAuthStateError m :: Type` - Backend-specific error type (e.g., `Void` for in-memory, `SqlError` for PostgreSQL)
- `type OAuthStateEnv m :: Type` - Backend configuration (e.g., `TVar OAuthState` for in-memory, `ConnectionPool` for PostgreSQL)
- `type OAuthUser m :: Type` - Full user record stored with tokens and authorization codes

**Key Operations**:
- `storeAuthCode`, `lookupAuthCode`, `deleteAuthCode`, `consumeAuthCode` - Authorization code management
- `storeAccessToken`, `lookupAccessToken` - Access token management
- `storeRefreshToken`, `lookupRefreshToken`, `updateRefreshToken` - Refresh token management
- `storeClient`, `lookupClient` - Client registration management
- `storePendingAuth`, `lookupPendingAuth`, `deletePendingAuth` - Pending authorization session management

**Algebraic Laws** (documented in module, tested in `Laws/OAuthStateStoreSpec.hs`):
- Round-trip Law: Store then lookup returns the stored value (when not expired)
- Delete Law: Delete then lookup returns Nothing
- Idempotence Law: Storing same value twice is equivalent to storing once
- Overwrite Law: Storing new value with same ID overwrites previous
- Expiry Law: Lookups for expired values return Nothing
- Atomicity Law: `consumeAuthCode` provides single-use guarantee

**Example Implementation**:
```haskell
instance (MonadIO m, MonadTime m) => OAuthStateStore (ReaderT PostgresEnv m) where
  type OAuthStateError (ReaderT PostgresEnv m) = SqlError
  type OAuthStateEnv (ReaderT PostgresEnv m) = PostgresEnv
  type OAuthUser (ReaderT PostgresEnv m) = DbUser
  storeAuthCode = ...  -- INSERT into auth_codes table
  lookupAuthCode = ... -- SELECT with expiry check
```

**Default Implementation**: `Servant.OAuth2.IDP.Store.InMemory` provides TVar-based in-memory storage using STM transactions.

### AuthBackend

User credential validation interface. Enables integration with external identity providers.

**Purpose**: Abstracts credential validation to enable pluggable authentication (LDAP, Active Directory, database, etc.).

**Associated Types**:
- `type AuthBackendError m :: Type` - Implementation-specific error type
- `type AuthBackendEnv m :: Type` - Implementation-specific environment (credential store, LDAP config, etc.)
- `type AuthBackendUser m :: Type` - Full authenticated user type (analogous to `OAuthUser`)

**Key Operation**:
- `validateCredentials :: Username -> PlaintextPassword -> m (Maybe (AuthBackendUser m))` - Returns `Just user` if valid, `Nothing` otherwise

**Security Requirements**:
- Username matching SHOULD be case-insensitive (implementation-defined)
- Password comparison MUST be constant-time to prevent timing attacks
- Invalid username and invalid password SHOULD be indistinguishable to prevent user enumeration

**Algebraic Laws** (documented in module, tested in `Laws/AuthBackendSpec.hs`):
- Determinism: Same inputs produce same outputs
- Independence: Validation of one user doesn't affect others

**Example Implementation**:
```haskell
instance MonadIO m => AuthBackend (ReaderT CredentialStore m) where
  type AuthBackendError (ReaderT CredentialStore m) = Text
  type AuthBackendEnv (ReaderT CredentialStore m) = CredentialStore
  type AuthBackendUser (ReaderT CredentialStore m) = AuthUser

  validateCredentials username password = do
    store <- ask
    case Map.lookup username (storeCredentials store) of
      Nothing -> pure Nothing  -- User not found (same as invalid password)
      Just hash -> do
        let candidateHash = mkHashedPassword (storeSalt store) password
        if hash == candidateHash  -- Constant-time via ScrubbedBytes Eq
          then pure $ Just (userFromUsername username)
          else pure Nothing
```

**Default Implementation**: `Servant.OAuth2.IDP.Auth.Demo` provides hardcoded demo credentials (demo/demo123, admin/admin456).

### MonadTime

Time abstraction for expiry checks and testability.

**Purpose**: Enables deterministic time in tests while using real time in production.

**Key Operation**:
- `currentTime :: m UTCTime` - Get current time

**Note**: Re-exported from `Control.Monad.Time` package.

## Configuration

### OAuthEnv (Servant.OAuth2.IDP.Config)

Protocol-level OAuth configuration. This is the main configuration record for the OAuth server.

**Key Fields**:

**Security**:
- `oauthRequireHTTPS :: Bool` - Require HTTPS for redirect URIs (except localhost)
- `oauthBaseUrl :: URI` - Base URL for OAuth endpoints (e.g., "https://api.example.com")
- `resourceServerBaseUrl :: URI` - Base URL for the resource server

**Timing**:
- `oauthAuthCodeExpiry :: NominalDiffTime` - Authorization code expiry duration (default: 600 seconds)
- `oauthAccessTokenExpiry :: NominalDiffTime` - Access token expiry duration (default: 3600 seconds)
- `oauthLoginSessionExpiry :: NominalDiffTime` - Login session expiry duration (default: 600 seconds)

**OAuth Parameters**:
- `oauthSupportedScopes :: [Scope]` - Supported OAuth scopes (can be empty)
- `oauthSupportedResponseTypes :: NonEmpty ResponseType` - Supported response types (RFC requires at least one)
- `oauthSupportedGrantTypes :: NonEmpty OAuthGrantType` - Supported grant types (RFC requires at least one)
- `oauthSupportedAuthMethods :: NonEmpty ClientAuthMethod` - Supported token endpoint authentication methods
- `oauthSupportedCodeChallengeMethods :: NonEmpty CodeChallengeMethod` - Supported PKCE methods (S256 is mandatory)

**Token Prefixes** (for generated IDs):
- `oauthAuthCodePrefix :: Text` - Prefix for authorization codes (e.g., "auth_")
- `oauthRefreshTokenPrefix :: Text` - Prefix for refresh tokens (e.g., "rt_")
- `oauthClientIdPrefix :: Text` - Prefix for client IDs (e.g., "client_")

**Branding**:
- `oauthServerName :: Text` - Server name for HTML templates (e.g., "MCP Server", "OAuth Server")
- `oauthScopeDescriptions :: Map Scope Text` - Human-readable scope descriptions for consent pages

**Metadata**:
- `resourceServerMetadata :: ProtectedResourceMetadata` - RFC 9728 protected resource metadata for discovery

## OAuth 2.1 Implementation Details

### OAuth 2.1 Compliance

This implementation follows OAuth 2.1 specifications:

- **PKCE Required**: All authorization code flows MUST use PKCE (RFC 7636)
- **S256 Code Challenge**: Only S256 method is supported (SHA-256 based)
- **Dynamic Client Registration**: RFC 7591 compliant registration endpoint
- **Metadata Discovery**: RFC 8414 (authorization server) and RFC 9728 (protected resource)
- **JWT Access Tokens**: Uses jose library for JWT generation and validation
- **Bearer Token Authentication**: RFC 6750 compliant bearer tokens
- **Refresh Token Rotation**: Supports refresh token updates

### Token Formats

- **Authorization Codes**: UUID v4 with configurable prefix (e.g., "auth_12345678-1234-1234-1234-123456789abc")
- **Access Tokens**: JWT tokens using servant-auth-server's makeJWT
- **Refresh Tokens**: UUID v4 with configurable prefix (e.g., "rt_12345678-1234-1234-1234-123456789abc")
- **Client IDs**: UUID v4 with configurable prefix (e.g., "client_12345678-1234-1234-1234-123456789abc")

### Metadata Endpoints

- `/.well-known/oauth-authorization-server` - OAuth authorization server metadata (RFC 8414)
- `/.well-known/oauth-protected-resource` - Protected resource metadata (RFC 9728)

### Security Features

- **PKCE Mandatory**: All OAuth flows require PKCE with S256 method
- **10-minute Authorization Code Expiry**: Configurable via `oauthAuthCodeExpiry`
- **1-hour Access Token Validity**: Configurable via `oauthAccessTokenExpiry`
- **Public Client Support**: No client secret required (configurable `publicClientSecret`)
- **Redirect URI Validation**: Validates against registered client redirect URIs
- **Single-Use Authorization Codes**: `consumeAuthCode` provides atomic single-use guarantee
- **Constant-Time Password Comparison**: Via ScrubbedBytes Eq instance
- **Memory Scrubbing**: Sensitive data (passwords, salts) uses ScrubbedBytes from memory package

### Interactive Login Flow

The OAuth authorization endpoint implements an interactive login page with credential authentication:

**Key Features**:
- Session cookies (`mcp_session`) track pending authorizations
- `PendingAuthorization` type stores OAuth parameters during login
- Configurable session expiry (default: 10 minutes)
- HTML form-based credential submission
- Approve/Deny actions
- Error pages for invalid states (expired sessions, unregistered clients, etc.)
- Never redirects to untrusted redirect_uri (security: validate against registered clients)

## Testing

### Test Architecture

The test suite uses a polymorphic approach with a custom `TestM` monad for deterministic testing.

**TestMonad** (`test/TestMonad.hs`):
- `TestM` - ReaderT-based monad with IORef state
- `TestEnv` - Environment with mock time and OAuth state
- Helper functions: `setTime`, `advanceTime`, `addTestCredential`, `getOAuthState`

**Purpose**: Enables polymorphic property tests that work with any monad implementing the typeclasses.

### Test Structure

**Laws Tests** (polymorphic, typeclass-agnostic):
- `Laws/OAuthStateStoreSpec.hs` - OAuthStateStore algebraic laws
- `Laws/AuthBackendSpec.hs` - AuthBackend algebraic laws
- `Laws/ConsumeAuthCodeSpec.hs` - Atomicity tests for consumeAuthCode
- `Laws/AuthCodeFunctorSpec.hs` - Functor laws for AuthorizationCode
- `Laws/BoundarySpec.hs` - Boundary conditions and edge cases
- `Laws/AuthBackendAssociatedTypesSpec.hs` - Associated type constraints
- `Laws/AuthBackendSignatureSpec.hs` - Method signature tests
- `Laws/OAuthUserTypeSpec.hs` - OAuthUser type constraints
- `Laws/ErrorBoundarySecuritySpec.hs` - Error handling security tests

**Implementation Tests** (specific to this package):
- `Servant/OAuth2/IDP/APISpec.hs` - API endpoint tests
- `Servant/OAuth2/IDP/BearerMethodSpec.hs` - Bearer token authentication
- `Servant/OAuth2/IDP/BrandingSpec.hs` - HTML branding tests
- `Servant/OAuth2/IDP/ConfigSpec.hs` - Configuration validation
- `Servant/OAuth2/IDP/TokenRequestSpec.hs` - Token endpoint tests
- `Servant/OAuth2/IDP/LucidRenderingSpec.hs` - HTML rendering tests
- `Servant/OAuth2/IDP/TypesSpec.hs` - Type validation tests
- `Servant/OAuth2/IDP/CryptoEntropySpec.hs` - Cryptographic randomness tests
- `Servant/OAuth2/IDP/ErrorsSpec.hs` - Error type tests
- `Servant/OAuth2/IDP/Handlers/MetadataSpec.hs` - Metadata endpoint tests
- `Servant/OAuth2/IDP/MetadataSpec.hs` - Metadata type tests
- `Servant/OAuth2/IDP/PKCESpec.hs` - PKCE validation tests
- `Servant/OAuth2/IDP/TraceSpec.hs` - Tracing tests

### Running Tests

```bash
cabal test                           # Run all tests
cabal test --test-show-details=direct  # Show detailed output
```

### Writing Polymorphic Tests

Tests should be polymorphic over the monad and accept a runner function:

```haskell
spec :: Spec
spec = describe "OAuthStateStore Laws" $ do
  -- Test with TestM monad
  let runTest action = do
        env <- mkTestEnv <$> getCurrentTime <*> pure Map.empty
        runTestM env action

  prop "round-trip law: store then lookup" $ \code -> ioProperty $ runTest $ do
    storeAuthCode code
    result <- lookupAuthCode (authCodeId code)
    pure $ result === Just code
```

## Technology Stack

- **Language**: Haskell GHC2021 (GHC 9.4+ via base ^>=4.18.2.1)
- **Web Framework**: servant-server 0.19-0.20, servant-auth-server 0.4, warp 3.2-3.3
- **Cryptography**: jose 0.10-0.11, crypton 0.34-2.0, memory 0.18
- **Serialization**: aeson 2.1-2.2
- **Concurrency**: stm 2.5, async 2.2
- **Effects**: mtl 2.3, monad-time 0.4
- **Lenses**: generic-lens 2.2
- **HTML**: lucid 2.11, servant-lucid 0.9
- **Networking**: network-uri 2.6
- **Testing**: hspec 2.10-2.12, hspec-wai 0.11, QuickCheck 2.14-2.16

## Type-Driven Design Principles

### Smart Constructor Hygiene

**CRITICAL**: All domain newtypes export only smart constructors (not raw constructors) to prevent validation bypass.

**Pattern**:
```haskell
-- In module Servant.OAuth2.IDP.Types
newtype AuthCodeId = AuthCodeId {unAuthCodeId :: Text}

-- Smart constructor (exported)
mkAuthCodeId :: Text -> Maybe AuthCodeId
mkAuthCodeId t
  | T.null t = Nothing
  | otherwise = Just (AuthCodeId t)

-- Generator for cryptographically random IDs (exported)
generateAuthCodeId :: MonadRandom m => Text -> m AuthCodeId
generateAuthCodeId prefix = ...

-- Extractor (exported)
unAuthCodeId :: AuthCodeId -> Text
```

**Never export raw constructors** - forces validation through smart constructors.

### Domain Type Flow

Domain types flow throughout the system without mid-flow Text conversions:

```haskell
-- GOOD: Domain types preserved
handleAuthorize :: ClientId -> RedirectUri -> CodeChallenge -> m AuthCodeId

-- BAD: Text soup
handleAuthorize :: Text -> Text -> Text -> m Text
```

### Credential Security

Sensitive data uses `ScrubbedBytes` from the memory package:

- **PlaintextPassword** - Scrubbed on GC, no Show instance
- **HashedPassword** - Scrubbed on GC, no Show instance
- **Salt** - Scrubbed on GC, no Show instance

**Constant-time comparison** via ScrubbedBytes Eq instance prevents timing attacks.

## Implementation Patterns

### Three-Layer Cake Architecture

Both typeclasses follow the three-layer cake pattern:

1. **Layer 1 (Orchestration)**: Application monad with `MonadReader` env
2. **Layer 2 (Capability)**: Typeclass with associated types
3. **Layer 3 (Business Logic)**: Pure functions using typeclass interface

**Example**:
```haskell
handleLogin ::
  ( AuthBackend m
  , OAuthStateStore m
  , AuthBackendUser m ~ OAuthUser m  -- Type equality constraint
  , MonadError e m
  , AsType (AuthBackendError m) e
  , MonadReader env m
  , HasType (AuthBackendEnv m) env
  , HasType (OAuthStateEnv m) env
  ) => LoginRequest -> m LoginResponse
```

### Type Bridging

When using both typeclasses together, ensure user types align:

```haskell
-- Handlers requiring both backends use type equality constraint:
handleLogin :: (AuthBackend m, OAuthStateStore m,
                AuthBackendUser m ~ OAuthUser m) => ...
```

This ensures the authenticated user from `AuthBackend` can be stored in `OAuthStateStore`.

### Error Handling

The `OAuthError m` type unifies all OAuth errors:

```haskell
data OAuthError m
  = OAuthValidation ValidationError     -- Semantic validation → 400
  | OAuthAuthorization AuthorizationError -- OAuth protocol → 400/401/403
  | OAuthLoginFlow LoginFlowError         -- Login flow → 400 (HTML)
  | OAuthStore (OAuthStateError m)        -- Storage backend → 500
```

Convert to Servant `ServerError` via `oauthErrorToServerError`:

```haskell
handler :: (OAuthStateStore m, ...) => m Response
handler = do
  result <- doSomething
  case result of
    Left err -> throwError (oauthErrorToServerError err)
    Right val -> pure val
```

## Important Notes

### Password Hashing Warning

**INSECURE DEMO IMPLEMENTATION**: The current `mkHashedPassword` uses SHA256 which is NOT suitable for production.

**Production MUST use**:
- **Argon2id** (recommended) - memory-hard, GPU-resistant
- **bcrypt** - time-tested, moderate security
- **PBKDF2** (minimum) - with high iteration count (≥100,000)

See `Servant.OAuth2.IDP.Auth.Backend` documentation for Argon2id example.

### Thread Safety

The in-memory implementation (`Servant.OAuth2.IDP.Store.InMemory`) uses STM transactions for thread safety.

Custom backends (PostgreSQL, Redis) MUST ensure:
- `consumeAuthCode` is atomic (single-use guarantee)
- Concurrent lookups don't return expired values
- Concurrent stores don't create race conditions

### Time Handling

Implementations MUST respect the `MonadTime` constraint for expiry checks:

```haskell
lookupAuthCode :: AuthCodeId -> m (Maybe (AuthorizationCode (OAuthUser m)))
lookupAuthCode codeId = do
  now <- currentTime  -- Use MonadTime, not IO's getCurrentTime
  maybeCode <- lookupInStorage codeId
  case maybeCode of
    Just code | authCodeExpiry code > now -> pure (Just code)
    _ -> pure Nothing
```

This enables deterministic time in tests via `TestM`.

## Future Directions

### Planned Enhancements

- PostgreSQL backend implementation
- Redis backend implementation
- Rate limiting support
- OAuth 2.1 device flow
- OpenID Connect support
- Client credentials flow
- Resource owner password flow (if needed for legacy systems)

### Package Extraction

This package is designed to be extracted as a standalone OAuth library. To use in other projects:

```haskell
-- In your cabal file
build-depends:
  servant-oauth2-idp ^>= 0.1.0.0

-- In your application
import Servant.OAuth2.IDP.Server (oauthServer)
import Servant.OAuth2.IDP.Config (OAuthEnv(..))
import Servant.OAuth2.IDP.Store.InMemory (mkOAuthTVarEnv)
import Servant.OAuth2.IDP.Auth.Demo (mkDemoCredentialEnv)

main :: IO ()
main = do
  oauthEnv <- mkOAuthTVarEnv
  authEnv <- mkDemoCredentialEnv
  let config = OAuthEnv { ... }  -- Configure OAuth parameters
  run 8080 $ oauthServer config oauthEnv authEnv
```

No MCP dependencies required - this is a fully standalone OAuth 2.1 server library.
