# Data Model: Claude.ai MCP Connector Compatibility

**Date**: 2025-12-08
**Feature**: 001-claude-mcp-connector

## New Types

### ProtectedResourceMetadata (MCP.Server.Auth)

RFC9728-compliant Protected Resource Metadata for OAuth discovery.

```haskell
data ProtectedResourceMetadata = ProtectedResourceMetadata
    { resource :: Text
      -- ^ The protected resource's identifier (MCP server URL)
      -- Required. MUST be an absolute URI with https scheme.

    , authorizationServers :: [Text]
      -- ^ List of authorization server issuer identifiers
      -- Required for MCP. At least one entry.

    , scopesSupported :: Maybe [Text]
      -- ^ Scope values the resource server understands
      -- Optional. e.g., ["mcp:read", "mcp:write"]

    , bearerMethodsSupported :: Maybe [Text]
      -- ^ Token presentation methods supported
      -- Optional. Default: ["header"] per RFC9728

    , resourceName :: Maybe Text
      -- ^ Human-readable name for end-user display
      -- Optional. e.g., "My MCP Server"

    , resourceDocumentation :: Maybe Text
      -- ^ URL of developer documentation
      -- Optional.
    }
    deriving (Show, Generic)
```

**JSON Serialization** (RFC9728 field names):

| Haskell Field | JSON Key | Required |
|---------------|----------|----------|
| `resource` | `resource` | Yes |
| `authorizationServers` | `authorization_servers` | Yes (MCP) |
| `scopesSupported` | `scopes_supported` | No |
| `bearerMethodsSupported` | `bearer_methods_supported` | No |
| `resourceName` | `resource_name` | No |
| `resourceDocumentation` | `resource_documentation` | No |

**Validation Rules**:
- `resource` must be valid HTTPS URL without fragment
- `authorizationServers` must be non-empty list
- Each authorization server must be valid HTTPS URL

**Example**:
```json
{
  "resource": "https://mcp.example.com",
  "authorization_servers": ["https://mcp.example.com"],
  "scopes_supported": ["mcp:read", "mcp:write"],
  "bearer_methods_supported": ["header"],
  "resource_name": "Example MCP Server"
}
```

### ProtectedResourceAuth Combinator (MCP.Server.Auth)

Custom Servant Auth combinator that automatically returns 401 with WWW-Authenticate header on authentication failure. This is handled at the framework level - apps don't manually handle 401 responses.

```haskell
-- | Type-level tag for MCP authentication
data ProtectedResourceAuth

-- | Configuration for ProtectedResourceAuth - the protected resource metadata URL
data ProtectedResourceAuthConfig = ProtectedResourceAuthConfig
    { resourceMetadataUrl :: Text
      -- ^ URL to the protected resource metadata endpoint
      -- e.g., "https://mcp.example.com/.well-known/oauth-protected-resource"
    }
    deriving (Show, Generic)

-- | Type family instance: what data the auth scheme needs
type instance AuthServerData ProtectedResourceAuth = ProtectedResourceAuthConfig
```

**How it works:**

1. The `ProtectedResourceAuth` type is used in API type signatures: `ProtectedResourceAuth :> "mcp" :> ...`
2. When authentication fails, servant-auth calls `throwAll`
3. Our custom `ThrowAll` instance returns 401 with WWW-Authenticate header:

```haskell
-- | Custom error response with WWW-Authenticate header
instance ThrowAll (Handler a) where
    throwAll = throwError $ err401
        { errHeaders = [("WWW-Authenticate", wwwAuthenticateValue)]
        , errBody = encode $ object ["error" .= ("Authentication required" :: Text)]
        }
      where
        -- Note: The actual URL comes from context, shown simplified here
        wwwAuthenticateValue = "Bearer resource_metadata=\"<metadata-url>\""
```

**Usage in API definition:**

```haskell
-- Before (manual handling):
type MCPAPI = Auth '[JWT] AuthUser :> "mcp" :> ReqBody '[JSON] Value :> Post '[JSON] Value

-- After (automatic WWW-Authenticate on 401):
type MCPAPI = ProtectedResourceAuth :> Auth '[JWT] AuthUser :> "mcp" :> ReqBody '[JSON] Value :> Post '[JSON] Value
```

**Key benefit:** The library handles the 401 response format correctly. Apps using the library automatically get RFC9728-compliant WWW-Authenticate headers without any manual error handling code.

## Modified Types

### HTTPServerConfig (MCP.Server.HTTP)

Add field for Protected Resource Metadata configuration:

```haskell
data HTTPServerConfig = HTTPServerConfig
    { httpPort :: Port
    , httpBaseUrl :: Text
    , httpServerInfo :: Implementation
    , httpCapabilities :: ServerCapabilities
    , httpEnableLogging :: Bool
    , httpOAuthConfig :: Maybe OAuthConfig
    , httpJWK :: Maybe JWK
    , httpProtocolVersion :: Text
    -- NEW:
    , httpProtectedResourceMetadata :: Maybe ProtectedResourceMetadata
      -- ^ Custom protected resource metadata.
      -- When Nothing, auto-generated from httpBaseUrl.
    }
```

**Behavior**:
- If `httpProtectedResourceMetadata` is `Nothing`, generate default metadata using `httpBaseUrl`
- If `Just metadata`, use provided metadata directly

## Existing Types (Unchanged)

### OAuthMetadata (MCP.Server.Auth)

Already implements RFC8414 Authorization Server Metadata. No changes needed.

### OAuthConfig (MCP.Server.Auth)

Already supports all required OAuth configuration. No changes needed.

### AuthorizationCode, TokenResponse, ClientRegistrationRequest/Response

Already implemented correctly. No changes needed.

## State Changes

### OAuthState (MCP.Server.HTTP)

No changes to OAuth state management. Existing structure handles:
- `authCodes :: Map Text AuthorizationCode`
- `accessTokens :: Map Text AuthUser`
- `refreshTokens :: Map Text (Text, AuthUser)`
- `registeredClients :: Map Text ClientInfo`

## API Type Changes

### OAuthAPI (MCP.Server.HTTP)

Add Protected Resource Metadata endpoint:

```haskell
type OAuthAPI =
    -- NEW: Protected Resource Metadata (RFC9728)
    ".well-known" :> "oauth-protected-resource" :> Get '[JSON] ProtectedResourceMetadata

    -- EXISTING: Authorization Server Metadata (RFC8414)
    :<|> ".well-known" :> "oauth-authorization-server" :> Get '[JSON] OAuthMetadata

    -- EXISTING: Dynamic Client Registration
    :<|> "register" :> ReqBody '[JSON] ClientRegistrationRequest
                    :> Post '[JSON] ClientRegistrationResponse

    -- EXISTING: Authorization endpoint (add resource param)
    :<|> "authorize"
        :> QueryParam' '[Required] "response_type" Text
        :> QueryParam' '[Required] "client_id" Text
        :> QueryParam' '[Required] "redirect_uri" Text
        :> QueryParam' '[Required] "code_challenge" Text
        :> QueryParam' '[Required] "code_challenge_method" Text
        :> QueryParam "scope" Text
        :> QueryParam "state" Text
        :> QueryParam "resource" Text  -- NEW: RFC8707
        :> Get '[PlainText] Text

    -- EXISTING: Token endpoint
    :<|> "token" :> ReqBody '[FormUrlEncoded] [(Text, Text)]
                :> Post '[JSON] TokenResponse
```

## Relationships

```
HTTPServerConfig
    │
    ├── httpOAuthConfig :: Maybe OAuthConfig
    │       └── (existing OAuth settings)
    │
    └── httpProtectedResourceMetadata :: Maybe ProtectedResourceMetadata
            │
            └── authorizationServers ──► points to same-origin OAuthMetadata
```

## Lifecycle / State Transitions

No new state machines. The OAuth flow remains:

```
Unregistered Client
    │
    ▼ POST /register
Registered Client (has client_id)
    │
    ▼ GET /authorize (with PKCE challenge)
Has Authorization Code
    │
    ▼ POST /token (with code_verifier)
Has Access Token + Refresh Token
    │
    ├── Use token for /mcp requests
    │
    └── POST /token (refresh_token grant)
        └── New Access Token
```

## Module Exports

### MCP.Server.Auth (additions)

```haskell
module MCP.Server.Auth (
    -- ... existing exports ...

    -- NEW: Protected Resource Metadata
    ProtectedResourceMetadata(..),

    -- NEW: ProtectedResourceAuth Combinator
    ProtectedResourceAuth,
    ProtectedResourceAuthConfig(..),
) where
```

### MCP.Server.HTTP (additions)

```haskell
module MCP.Server.HTTP (
    -- ... existing exports ...

    -- HTTPServerConfig already exported, just add field
    -- ProtectedResourceAuth combinator re-exported for convenience
) where
```
