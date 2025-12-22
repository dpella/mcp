# Quickstart: Remove MCP Imports from Servant.OAuth2.IDP

**Branch**: `005-servant-oauth-extraction`
**Date**: 2025-12-18

## Overview

This refactoring removes all `MCP.*` imports from `Servant.OAuth2.IDP.*` modules, preparing them for extraction to a separate package.

## Changes Summary

| Change Type | Count | Description |
|-------------|-------|-------------|
| New Modules | 4 | Trace, Config, Metadata, PKCE |
| Modified Modules | 15 | Import updates, signature changes |
| Removed Exports | 4 | From MCP.Server.Auth |

## New Module Imports

After this refactoring, use these imports:

```haskell
-- OAuth configuration
import Servant.OAuth2.IDP.Config (OAuthEnv (..))

-- OAuth trace events
import Servant.OAuth2.IDP.Trace (OAuthTrace (..))

-- OAuth metadata types (RFC 8414, RFC 9728)
import Servant.OAuth2.IDP.Metadata (OAuthMetadata (..), ProtectedResourceMetadata (..))

-- PKCE validation
import Servant.OAuth2.IDP.PKCE (validateCodeVerifier, generateCodeChallenge)

-- MonadTime (direct from monad-time, not via MCP)
import Control.Monad.Time (MonadTime (..))
```

## Handler Signature Changes

### Before

```haskell
import MCP.Server.Auth (OAuthConfig (..))
import MCP.Server.HTTP.AppEnv (HTTPServerConfig (..))
import MCP.Trace.HTTP (HTTPTrace (..))

handleAuthorization ::
    ( OAuthStateStore m
    , MonadReader env m
    , HasType HTTPServerConfig env
    , HasType (IOTracer HTTPTrace) env
    ) => AuthorizationRequest -> m AuthorizationResponse
```

### After

```haskell
import Servant.OAuth2.IDP.Config (OAuthEnv (..))
import Servant.OAuth2.IDP.Trace (OAuthTrace (..))

handleAuthorization ::
    ( OAuthStateStore m
    , MonadReader env m
    , HasType OAuthEnv env
    , HasType (IOTracer OAuthTrace) env
    ) => AuthorizationRequest -> m AuthorizationResponse
```

## Configuration Access Changes

### Before

```haskell
config <- view (typed @HTTPServerConfig)
let baseUrl = httpBaseUrl config
let oauthCfg = fromMaybe defaultOAuthConfig (httpOAuthConfig config)
let prefix = authCodePrefix oauthCfg
let expiry = fromIntegral (authCodeExpirySeconds oauthCfg)
```

### After

```haskell
oauthEnv <- view (typed @OAuthEnv)
let baseUrl = oauthBaseUrl oauthEnv
let prefix = oauthAuthCodePrefix oauthEnv
let expiry = oauthAuthCodeExpiry oauthEnv  -- Already NominalDiffTime
```

## Trace Emission Changes

### Before

```haskell
import MCP.Trace.HTTP (HTTPTrace (..))
import MCP.Trace.OAuth qualified as OAuthTrace

tracer <- view (typed @(IOTracer HTTPTrace))
liftIO $ emit tracer (HTTPOAuth $ OAuthTrace.OAuthClientRegistration clientId name)
```

### After

```haskell
import Servant.OAuth2.IDP.Trace (OAuthTrace (..))

tracer <- view (typed @(IOTracer OAuthTrace))
liftIO $ emit tracer (TraceClientRegistration clientId name)
```

## MCP Integration

For MCP applications, add `OAuthEnv` to your `AppEnv`:

```haskell
import Servant.OAuth2.IDP.Config (OAuthEnv (..))
import Servant.OAuth2.IDP.Trace (OAuthTrace)

data AppEnv = AppEnv
    { ...
    , envOAuthEnv :: OAuthEnv          -- NEW
    , envOAuthTracer :: IOTracer OAuthTrace  -- NEW (or adapt existing)
    }

-- Build OAuthEnv from existing config
mkOAuthEnv :: HTTPServerConfig -> OAuthEnv
mkOAuthEnv cfg = OAuthEnv
    { oauthBaseUrl = httpBaseUrl cfg
    , oauthAuthCodeExpiry = fromIntegral (authCodeExpirySeconds oauthCfg)
    , ... -- See data-model.md for full mapping
    }
  where
    oauthCfg = fromMaybe defaultOAuthConfig (httpOAuthConfig cfg)
```

## Verification

After implementation, verify the refactoring succeeded:

```bash
# No MCP imports in Servant modules
rg "^import MCP\." src/Servant/
# Should return empty

# Build succeeds
cabal build

# Tests pass
cabal test
```

## Breaking Changes

### Removed from MCP.Server.Auth

These exports are removed - import from Servant instead:

| Removed | New Location |
|---------|--------------|
| `OAuthMetadata` | `Servant.OAuth2.IDP.Metadata` |
| `ProtectedResourceMetadata` | `Servant.OAuth2.IDP.Metadata` |
| `validateCodeVerifier` | `Servant.OAuth2.IDP.PKCE` |
| `generateCodeChallenge` | `Servant.OAuth2.IDP.PKCE` |

### Still in MCP.Server.Auth

These remain unchanged:

- `OAuthConfig` - MCP-specific configuration
- `OAuthProvider` - Provider configuration
- `OAuthGrantType` - Grant type enum
- `TokenInfo` - Token introspection
- `extractBearerToken` - Utility function
- `PKCEChallenge` - PKCE data
- `generateCodeVerifier` - IO action (needs cryptonite)

## Timeline

This is a pure refactoring with no runtime behavior changes. Implementation phases:

1. **Phase A**: Create new Servant modules (additive, safe)
2. **Phase B**: Update Servant imports (internal changes)
3. **Phase C**: Update MCP integration (add OAuthEnv to AppEnv)
4. **Phase D**: Clean break (remove old exports)

Each phase should build and test successfully before proceeding.
