# servant-oauth2-idp

A complete OAuth 2.1 Authorization Server implementation for Servant with pluggable storage and authentication backends.

[![MIT License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

## Synopsis

`servant-oauth2-idp` provides a production-ready OAuth 2.1 Identity Provider (Authorization Server) implementation for Haskell web services built with Servant. It implements RFC-compliant OAuth 2.1 protocol flows with modern security patterns including mandatory PKCE, JWT access tokens, and metadata discovery endpoints.

The library is designed around a typeclass-based architecture that allows you to plug in your own storage backends (PostgreSQL, Redis, etc.) and authentication systems (LDAP, Active Directory, etc.) while maintaining full OAuth 2.1 compliance.

## Key Features

- **OAuth 2.1 Compliance**: Implements OAuth 2.1 specification with mandatory PKCE (RFC 7636)
- **JWT Access Tokens**: Production-ready JWT tokens using the jose library
- **Dynamic Client Registration**: RFC 7591 compliant dynamic client registration
- **Metadata Discovery**: RFC 8414 (authorization server) and RFC 9728 (protected resource) discovery endpoints
- **Interactive Login Flow**: Built-in HTML-based login UI with session management
- **Pluggable Storage**: Abstract storage layer via `OAuthStateStore` typeclass (supports custom backends)
- **Pluggable Authentication**: Abstract authentication via `AuthBackend` typeclass (LDAP, database, etc.)
- **In-Memory Reference Implementation**: TVar-based storage for development and testing
- **Structured Tracing**: Built-in tracing support for observability
- **Production Security**: Constant-time password comparison, memory scrubbing for credentials, single-use authorization codes

## Installation

### Using Cabal

Add to your `*.cabal` file:

```cabal
build-depends:
    servant-oauth2-idp ^>= 0.1.0.0
```

### Using Stack

Add to your `stack.yaml`:

```yaml
extra-deps:
  - servant-oauth2-idp-0.1.0.0
```

Then add to your package dependencies:

```yaml
dependencies:
  - servant-oauth2-idp
```

## Quick Start

Here's a minimal example showing how to create an OAuth 2.1 server with the in-memory storage backend:

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (liftIO)
import Network.Wai.Handler.Warp (run)
import Network.URI (parseURI)
import Servant
import Data.Default (def)
import qualified Data.Map.Strict as Map

-- Core imports
import Servant.OAuth2.IDP.Server (oauthServer)
import Servant.OAuth2.IDP.Config (OAuthEnv(..))
import Servant.OAuth2.IDP.Store.InMemory (mkOAuthTVarEnv)
import Servant.OAuth2.IDP.Auth.Demo (mkDemoCredentialEnv)
import Servant.OAuth2.IDP.Types (Scope(..), ResponseType(..), OAuthGrantType(..))
import Servant.OAuth2.IDP.Metadata (ProtectedResourceMetadata(..))

-- Configuration
mkOAuthConfig :: IO OAuthEnv
mkOAuthConfig = do
  let Just baseUrl = parseURI "http://localhost:8080"
      Just resourceUrl = parseURI "http://localhost:8080"
  pure $ OAuthEnv
    { oauthRequireHTTPS = False  -- Dev only; use True in production
    , oauthBaseUrl = baseUrl
    , resourceServerBaseUrl = resourceUrl
    , oauthAuthCodeExpiry = 600      -- 10 minutes
    , oauthAccessTokenExpiry = 3600  -- 1 hour
    , oauthLoginSessionExpiry = 600  -- 10 minutes
    , oauthSupportedScopes = [Scope "read", Scope "write"]
    , oauthSupportedResponseTypes = pure ResponseTypeCode
    , oauthSupportedGrantTypes = pure GrantTypeAuthorizationCode
    , oauthSupportedAuthMethods = pure ClientSecretPost
    , oauthSupportedCodeChallengeMethods = pure S256
    , oauthAuthCodePrefix = "auth_"
    , oauthRefreshTokenPrefix = "rt_"
    , oauthClientIdPrefix = "client_"
    , oauthServerName = "My OAuth Server"
    , oauthScopeDescriptions = Map.fromList
        [ (Scope "read", "Read access to your data")
        , (Scope "write", "Write access to your data")
        ]
    , resourceServerMetadata = def  -- Use default metadata
    }

main :: IO ()
main = do
  putStrLn "Starting OAuth 2.1 server on http://localhost:8080"

  -- Initialize storage and auth backends
  oauthEnv <- mkOAuthTVarEnv
  authEnv <- mkDemoCredentialEnv  -- Demo credentials: demo/demo123, admin/admin456
  config <- mkOAuthConfig

  -- Start server
  run 8080 $ oauthServer config oauthEnv authEnv
```

### Testing the Server

Once running, test the OAuth endpoints:

```bash
# Discover OAuth metadata
curl http://localhost:8080/.well-known/oauth-authorization-server

# Register a client
curl -X POST http://localhost:8080/register \
  -H "Content-Type: application/json" \
  -d '{"redirect_uris": ["http://localhost:3000/callback"]}'

# Test protected resource (should return 401 with WWW-Authenticate header)
curl -i http://localhost:8080/mcp
```

For the full OAuth authorization flow with PKCE, see the [OAuth 2.1 Flow Example](#oauth-21-authorization-flow) below.

## API Overview

### Core Typeclasses

The library provides two main typeclasses for customization:

#### OAuthStateStore

Storage interface for OAuth server state (authorization codes, tokens, clients, sessions).

```haskell
class (Monad m, MonadTime m) => OAuthStateStore m where
  type OAuthStateError m :: Type
  type OAuthStateEnv m :: Type
  type OAuthUser m :: Type

  -- Authorization code management
  storeAuthCode :: AuthorizationCode (OAuthUser m) -> m ()
  lookupAuthCode :: AuthCodeId -> m (Maybe (AuthorizationCode (OAuthUser m)))
  consumeAuthCode :: AuthCodeId -> m (Either (OAuthStateError m) (AuthorizationCode (OAuthUser m)))

  -- Token management
  storeAccessToken :: AccessToken (OAuthUser m) -> m ()
  lookupAccessToken :: AccessTokenId -> m (Maybe (AccessToken (OAuthUser m)))

  -- Client registration
  storeClient :: ClientInfo -> m ()
  lookupClient :: ClientId -> m (Maybe ClientInfo)

  -- ... and more
```

**Default Implementation**: `Servant.OAuth2.IDP.Store.InMemory` (TVar-based, thread-safe via STM)

**Custom Backend Example** (PostgreSQL):
```haskell
instance (MonadIO m, MonadTime m) => OAuthStateStore (ReaderT PostgresEnv m) where
  type OAuthStateError (ReaderT PostgresEnv m) = SqlError
  type OAuthStateEnv (ReaderT PostgresEnv m) = PostgresEnv
  type OAuthUser (ReaderT PostgresEnv m) = DbUser

  storeAuthCode code = do
    pool <- asks connectionPool
    liftIO $ execute pool "INSERT INTO auth_codes ..." code

  lookupAuthCode codeId = do
    now <- currentTime  -- MonadTime for expiry checks
    pool <- asks connectionPool
    result <- liftIO $ query pool "SELECT * FROM auth_codes WHERE ..." codeId
    pure $ case result of
      [code] | authCodeExpiry code > now -> Just code
      _ -> Nothing
```

#### AuthBackend

User credential validation interface for authentication.

```haskell
class Monad m => AuthBackend m where
  type AuthBackendError m :: Type
  type AuthBackendEnv m :: Type
  type AuthBackendUser m :: Type

  validateCredentials :: Username -> PlaintextPassword
                      -> m (Maybe (AuthBackendUser m))
```

**Default Implementation**: `Servant.OAuth2.IDP.Auth.Demo` (hardcoded demo credentials)

**Custom Backend Example** (LDAP):
```haskell
instance MonadIO m => AuthBackend (ReaderT LdapConfig m) where
  type AuthBackendError (ReaderT LdapConfig m) = LdapError
  type AuthBackendEnv (ReaderT LdapConfig m) = LdapConfig
  type AuthBackendUser (ReaderT LdapConfig m) = LdapUser

  validateCredentials username password = do
    config <- ask
    result <- liftIO $ ldapBind (ldapHost config) username password
    pure $ case result of
      Right user -> Just user
      Left _ -> Nothing
```

### Key Modules

- **Servant.OAuth2.IDP.Server**: Main server composition and wiring
- **Servant.OAuth2.IDP.Config**: `OAuthEnv` configuration type
- **Servant.OAuth2.IDP.Types**: Core domain types (ClientId, AuthCodeId, Scope, etc.)
- **Servant.OAuth2.IDP.Errors**: Comprehensive error types with Servant integration
- **Servant.OAuth2.IDP.PKCE**: PKCE utilities (generate/validate code verifiers and challenges)
- **Servant.OAuth2.IDP.Metadata**: RFC 8414 and RFC 9728 metadata types
- **Servant.OAuth2.IDP.Handlers**: OAuth endpoint handlers (authorize, token, register, etc.)
- **Servant.OAuth2.IDP.Store.InMemory**: In-memory TVar-based storage implementation
- **Servant.OAuth2.IDP.Auth.Demo**: Demo credential authentication

## OAuth 2.1 Authorization Flow

Complete example of the authorization code flow with PKCE:

```bash
# 1. Register a client
CLIENT_INFO=$(curl -s -X POST http://localhost:8080/register \
  -H "Content-Type: application/json" \
  -d '{"redirect_uris": ["http://localhost:3000/callback"]}')

CLIENT_ID=$(echo $CLIENT_INFO | jq -r '.client_id')

# 2. Generate PKCE verifier and challenge (S256)
CODE_VERIFIER=$(openssl rand -base64 64 | tr -d '\n' | tr -d '=' | tr '+/' '-_')
CODE_CHALLENGE=$(echo -n $CODE_VERIFIER | openssl dgst -sha256 -binary | base64 | tr -d '\n' | tr -d '=' | tr '+/' '-_')

# 3. Direct user to authorization endpoint (in browser)
AUTH_URL="http://localhost:8080/authorize?client_id=$CLIENT_ID&redirect_uri=http://localhost:3000/callback&response_type=code&code_challenge=$CODE_CHALLENGE&code_challenge_method=S256&scope=read+write"
echo "Open in browser: $AUTH_URL"

# User logs in with demo/demo123 and approves
# Browser redirects to: http://localhost:3000/callback?code=AUTH_CODE

# 4. Exchange authorization code for access token
curl -X POST http://localhost:8080/token \
  -H "Content-Type: application/x-www-form-urlencoded" \
  -d "grant_type=authorization_code&code=AUTH_CODE&redirect_uri=http://localhost:3000/callback&client_id=$CLIENT_ID&code_verifier=$CODE_VERIFIER"
```

## Configuration

### OAuthEnv

The `OAuthEnv` type configures OAuth protocol parameters:

```haskell
data OAuthEnv = OAuthEnv
  { -- Security
    oauthRequireHTTPS :: Bool
  , oauthBaseUrl :: URI
  , resourceServerBaseUrl :: URI

    -- Timing
  , oauthAuthCodeExpiry :: NominalDiffTime      -- Default: 600s (10 min)
  , oauthAccessTokenExpiry :: NominalDiffTime   -- Default: 3600s (1 hour)
  , oauthLoginSessionExpiry :: NominalDiffTime  -- Default: 600s (10 min)

    -- OAuth Parameters
  , oauthSupportedScopes :: [Scope]
  , oauthSupportedResponseTypes :: NonEmpty ResponseType
  , oauthSupportedGrantTypes :: NonEmpty OAuthGrantType
  , oauthSupportedAuthMethods :: NonEmpty ClientAuthMethod
  , oauthSupportedCodeChallengeMethods :: NonEmpty CodeChallengeMethod

    -- Token Prefixes
  , oauthAuthCodePrefix :: Text
  , oauthRefreshTokenPrefix :: Text
  , oauthClientIdPrefix :: Text

    -- Branding
  , oauthServerName :: Text
  , oauthScopeDescriptions :: Map Scope Text

    -- Metadata
  , resourceServerMetadata :: ProtectedResourceMetadata
  }
```

**Production Configuration Example**:

```haskell
productionConfig :: IO OAuthEnv
productionConfig = do
  let Just baseUrl = parseURI "https://auth.example.com"
      Just resourceUrl = parseURI "https://api.example.com"
  pure $ OAuthEnv
    { oauthRequireHTTPS = True  -- CRITICAL for production
    , oauthBaseUrl = baseUrl
    , resourceServerBaseUrl = resourceUrl
    , oauthAuthCodeExpiry = 600
    , oauthAccessTokenExpiry = 3600
    , oauthLoginSessionExpiry = 600
    , oauthSupportedScopes = [Scope "read", Scope "write", Scope "admin"]
    , oauthSupportedResponseTypes = pure ResponseTypeCode
    , oauthSupportedGrantTypes = pure GrantTypeAuthorizationCode
    , oauthSupportedAuthMethods = pure ClientSecretPost
    , oauthSupportedCodeChallengeMethods = pure S256
    , oauthAuthCodePrefix = "authz_"
    , oauthRefreshTokenPrefix = "refresh_"
    , oauthClientIdPrefix = "client_"
    , oauthServerName = "Example Corp OAuth"
    , oauthScopeDescriptions = Map.fromList
        [ (Scope "read", "Read access to your account")
        , (Scope "write", "Modify your account data")
        , (Scope "admin", "Administrative access")
        ]
    , resourceServerMetadata = def
    }
```

## Security Considerations

### Production Requirements

**CRITICAL**: The demo authentication backend uses SHA256 for password hashing, which is **NOT SUITABLE FOR PRODUCTION**.

Production deployments MUST use proper password hashing:

```haskell
-- Use Argon2id (recommended)
import Crypto.Argon2

hashPassword :: PlaintextPassword -> IO HashedPassword
hashPassword (PlaintextPassword pass) = do
  let options = defaultHashOptions
        { hashIterations = 3
        , hashMemory = 65536  -- 64 MB
        , hashParallelism = 4
        }
  hashed <- hashPassword options pass
  pure $ HashedPassword hashed
```

Alternative options (in order of preference):
1. **Argon2id** - Memory-hard, GPU-resistant (recommended)
2. **bcrypt** - Time-tested, moderate security
3. **PBKDF2** - Minimum acceptable (use ≥100,000 iterations)

### Other Security Features

- **Constant-Time Comparison**: Password comparison uses `ScrubbedBytes` from the memory package for constant-time equality checks (prevents timing attacks)
- **Memory Scrubbing**: Sensitive data (passwords, salts) automatically scrubbed from memory
- **Single-Use Authorization Codes**: `consumeAuthCode` provides atomic single-use guarantee
- **PKCE Mandatory**: All authorization flows require PKCE with S256 method
- **HTTPS Enforcement**: Configurable HTTPS requirement for redirect URIs (except localhost)
- **Redirect URI Validation**: All redirects validated against registered client URIs

## Testing

Run the test suite:

```bash
cabal test
cabal test --test-show-details=direct  # Verbose output
```

The test suite includes:

- **Typeclass Law Tests**: Algebraic laws for `OAuthStateStore` and `AuthBackend`
- **Protocol Compliance Tests**: OAuth 2.1 and RFC compliance
- **Security Tests**: PKCE validation, constant-time operations, etc.
- **API Tests**: Endpoint behavior validation
- **Edge Case Tests**: Boundary conditions, expiry handling, etc.

## Architecture

### Design Principles

- **Type-Driven Design**: Smart constructors enforce invariants at compile time
- **Zero MCP Dependencies**: Completely standalone OAuth 2.1 library
- **Pluggable Backends**: Typeclass-based architecture for storage and authentication
- **RFC Compliance**: Follows OAuth 2.1, PKCE (RFC 7636), Dynamic Registration (RFC 7591), and Metadata Discovery (RFC 8414, RFC 9728)
- **Production Ready**: Thread-safe, secure defaults, structured tracing

### Module Organization

```
Servant.OAuth2.IDP/
├── API.hs                    -- Servant API types
├── Config.hs                 -- OAuthEnv configuration
├── Types.hs                  -- Core domain types
├── Metadata.hs               -- RFC 8414/9728 metadata types
├── PKCE.hs                   -- PKCE utilities
├── Errors.hs                 -- Error types and conversion
├── Store.hs                  -- OAuthStateStore typeclass
├── Store/
│   └── InMemory.hs          -- TVar-based implementation
├── Auth/
│   ├── Backend.hs           -- AuthBackend typeclass
│   └── Demo.hs              -- Demo credential implementation
├── Handlers/
│   ├── Authorization.hs     -- /authorize endpoint
│   ├── Login.hs             -- Interactive login flow
│   ├── Token.hs             -- /token endpoint
│   ├── Registration.hs      -- /register endpoint
│   ├── Metadata.hs          -- /.well-known/* endpoints
│   └── HTML.hs              -- HTML rendering
├── Server.hs                -- Server composition
└── Trace.hs                 -- Structured tracing
```

## Roadmap

Future enhancements planned:

- PostgreSQL backend implementation
- Redis backend implementation
- Rate limiting support
- OAuth 2.1 device flow
- OpenID Connect support
- Client credentials flow
- Example implementations for common backends (LDAP, Active Directory)

## Contributing

Contributions are welcome! Please:

1. Open an issue to discuss the change before starting work
2. Ensure all tests pass (`cabal test`)
3. Run the linter and fix warnings (`hlint .`)
4. Format code with fourmolu (`fourmolu -i src/ test/`)
5. Submit a pull request with clear description of changes

### Development Setup

```bash
# Clone the repository
git clone https://github.com/your-org/servant-oauth2-idp
cd servant-oauth2-idp

# Build the project
cabal build

# Run tests
cabal test

# Run linter
hlint .

# Format code
fourmolu -i src/ test/
```

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Authors

- Matthias Pall Gissurarson (mpg@mpg.is)
- Alberto Valverde, PakSCADA LLC (alberto.valverde@pakenergy.com)

## Issue Tracker

Please report issues on the GitHub issue tracker: [https://github.com/your-org/servant-oauth2-idp/issues](https://github.com/your-org/servant-oauth2-idp/issues)

## Acknowledgments

This library is extracted from the MCP (Model Context Protocol) Haskell implementation and designed to be a standalone OAuth 2.1 server library for Servant applications.
