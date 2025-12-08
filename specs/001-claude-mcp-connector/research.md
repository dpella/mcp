# Research: Claude.ai MCP Connector Compatibility

**Date**: 2025-12-08
**Feature**: 001-claude-mcp-connector

## RFC9728 Protected Resource Metadata

### Decision: Implement minimal RFC9728-compliant ProtectedResourceMetadata type

**Rationale**: Claude.ai needs to discover the authorization server via the Protected Resource Metadata endpoint. RFC9728 specifies required and optional fields.

### Required Fields (per RFC9728)

| Field | Type | Description |
|-------|------|-------------|
| `resource` | URL (HTTPS, no fragment) | The protected resource's identifier (MCP server URL) |

### Recommended Fields (for MCP compatibility)

| Field | Type | Description |
|-------|------|-------------|
| `authorization_servers` | JSON array of URLs | List of authorization server issuer identifiers |
| `scopes_supported` | JSON array of strings | Scope values used in authorization requests |
| `bearer_methods_supported` | JSON array | Supported token methods: `["header"]` for MCP |

### Optional Fields (include if useful)

| Field | Type | Description |
|-------|------|-------------|
| `resource_name` | String | Human-readable name for display |
| `resource_documentation` | URL | Developer documentation |

### Alternatives Considered

1. **Full RFC9728 implementation**: Include all optional fields (jwks_uri, DPoP support, mTLS, etc.)
   - Rejected: Over-engineering for MVP; adds complexity without Claude.ai benefit

2. **Minimal single-field**: Only `resource` field
   - Rejected: Claude.ai likely requires `authorization_servers` for discovery

## WWW-Authenticate Header Implementation

### Decision: Custom Auth combinator at framework level (not manual handler code)

**Rationale**: Per [servant-auth issue #75](https://github.com/haskell-servant/servant-auth/issues/75), servant-auth intentionally doesn't auto-send WWW-Authenticate because it supports multiple auth schemes. Rather than manually handling 401s in app code, we create a custom Auth combinator (`ProtectedResourceAuth`) that handles this at the framework level. This means:
- Apps using the library get correct behavior automatically
- No manual error handling code in app handlers
- Clean extraction path for `servant-auth-mcp` package

### Header Format (per RFC9728 Section 5.1)

```
WWW-Authenticate: Bearer resource_metadata="https://example.com/.well-known/oauth-protected-resource"
```

### Implementation Approach

```haskell
-- In MCP.Server.Auth, define custom combinator:
data ProtectedResourceAuth

data ProtectedResourceAuthConfig = ProtectedResourceAuthConfig
    { resourceMetadataUrl :: Text
    }

-- Custom ThrowAll instance returns 401 with WWW-Authenticate
instance ThrowAll (Handler a) where
    throwAll = throwError $ err401
        { errHeaders = [("WWW-Authenticate", wwwAuthenticateValue)]
        , errBody = encode $ object ["error" .= ("Authentication required" :: Text)]
        }

-- Usage in API type:
type ProtectedMCPAPI = ProtectedResourceAuth :> Auth '[JWT] AuthUser :> "mcp" :> ...
```

### Alternatives Considered

1. **WAI Middleware**: Wrap entire app to intercept 401s and add header
   - Rejected: Less explicit, harder to reason about

2. **Manual handler code**: Each 401 throwError manually adds header
   - Rejected: Duplicated code, apps must handle correctly, error-prone

3. **servant-errors package**: Use for unified error responses
   - Rejected: Adds dependency; combinator approach is cleaner

## Resource Parameter (RFC8707)

### Decision: Accept but don't strictly validate resource parameter

**Rationale**: MCP spec requires clients send `resource` parameter. For MVP with same-origin auth server, we accept it but don't need strict audience binding since tokens are already scoped to this server.

### Implementation

- Accept `resource` query parameter in `/authorize` endpoint
- Accept `resource` form parameter in `/token` endpoint
- Log parameter value for debugging
- Future: Validate token audience matches resource

## Servant-Auth Integration Pattern

### Decision: Custom ProtectedResourceAuth combinator wrapping protected endpoints

**Rationale**: Rather than modifying handler code, we create a custom Auth combinator that:
1. Composes with servant-auth's existing `Auth` combinator
2. Automatically intercepts authentication failures
3. Returns 401 with RFC9728-compliant WWW-Authenticate header
4. Requires no changes to app handler code

### Code Location

- Type: `MCP.Server.Auth` (new `ProtectedResourceAuth`, `ProtectedResourceAuthConfig`, `ProtectedResourceMetadata`)
- Endpoint: `MCP.Server.HTTP` (add to `OAuthAPI` type, use `ProtectedResourceAuth` in API definition)
- Handler: Unchanged - combinator handles 401 automatically

### Extractability for Future Package

Structure code so these can be extracted as `servant-auth-mcp`:
1. `ProtectedResourceAuth` combinator + `ProtectedResourceAuthConfig` type
2. `ProtectedResourceMetadata` type + JSON instances
3. `ThrowAll` instance for WWW-Authenticate header
4. API type for protected resource endpoint

## HTTPServerConfig Changes

### Decision: Add optional ProtectedResourceMetadata field

**Rationale**: Users should be able to configure the metadata per their deployment.

### New Field

```haskell
data HTTPServerConfig = HTTPServerConfig
    { ...
    , httpProtectedResourceMetadata :: Maybe ProtectedResourceMetadata
    -- When Nothing, auto-generate from httpBaseUrl
    }
```

## Testing Strategy

### Decision: Golden tests for endpoint responses + manual Claude.ai validation

**Rationale**: JSON structure must match RFC specs exactly. Property tests are nice-to-have but golden tests catch regression.

### Test Cases

1. `GET /.well-known/oauth-protected-resource` returns valid JSON
2. `POST /mcp` without auth returns 401 with WWW-Authenticate header
3. OAuth flow end-to-end (existing demo script, extended)
4. Final validation: Add server to Claude.ai

## Sources

- [RFC 9728 - OAuth 2.0 Protected Resource Metadata](https://datatracker.ietf.org/doc/html/rfc9728)
- [RFC 8707 - Resource Indicators for OAuth 2.0](https://www.rfc-editor.org/rfc/rfc8707.html)
- [servant-auth-server on Hackage](https://hackage.haskell.org/package/servant-auth-server)
- [servant-auth issue #75 - WWW-Authenticate header](https://github.com/haskell-servant/servant-auth/issues/75)
- [MCP Authorization Spec](file:///home/claude/mcp/docs/mcp_auth_spec.md)
