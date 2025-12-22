# Module Dependencies Contract

**Branch**: `005-servant-oauth-extraction`
**Date**: 2025-12-18

## Invariant

After this refactoring, the following invariant MUST hold:

```
∀ module M ∈ Servant.OAuth2.IDP.* :
  ∀ import I ∈ imports(M) :
    ¬(I starts with "MCP.")
```

In plain English: **No module under `Servant.OAuth2.IDP.*` may import any module from the `MCP.*` namespace.**

## Allowed Dependencies (Servant.OAuth2.IDP.*)

### External Packages

| Package | Modules | Purpose |
|---------|---------|---------|
| `base` | `Control.*, Data.*, GHC.*` | Standard library |
| `monad-time` | `Control.Monad.Time` | MonadTime typeclass |
| `aeson` | `Data.Aeson` | JSON serialization |
| `text` | `Data.Text` | Text type |
| `bytestring` | `Data.ByteString` | ByteString type |
| `time` | `Data.Time` | Time types |
| `cryptonite` | `Crypto.Hash` | SHA256 for PKCE |
| `memory` | `Data.ByteArray` | ByteArray operations |
| `base64-bytestring` | `Data.ByteString.Base64.URL` | Base64URL encoding |
| `servant` | `Servant.*` | Servant types |
| `servant-server` | `Servant.Server.*` | Servant server |
| `servant-auth-server` | `Servant.Auth.Server.*` | JWT support |
| `http-types` | `Network.HTTP.Types.*` | HTTP types |
| `generic-lens` | `Data.Generics.*` | Generic lens |
| `containers` | `Data.Map, Data.Set` | Container types |
| `stm` | `Control.Concurrent.STM` | STM operations |
| `mtl` | `Control.Monad.*` | Monad transformers |
| `transformers` | `Control.Monad.Trans.*` | Transformers |
| `plow-logging` | `Plow.Logging` | IOTracer abstraction |

### Internal (Servant.OAuth2.IDP.*)

Modules may import other `Servant.OAuth2.IDP.*` modules.

Dependency graph (no cycles):
```
Types.hs          <- (no internal deps)
Config.hs         <- Types
Errors.hs         <- Types
Trace.hs          <- Types, Errors, Auth/Backend
Metadata.hs       <- Types
PKCE.hs           <- Types
Store.hs          <- Types, Errors
Store/InMemory.hs <- Store, Types
Boundary.hs       <- Types
Auth/Backend.hs   <- Types
Auth/Demo.hs      <- Auth/Backend, Types
Handlers/*.hs     <- Store, Config, Trace, Types, PKCE, Metadata, Auth/*
Server.hs         <- Handlers, API, Store, Config, Trace
API.hs            <- Types, Metadata
```

## Forbidden Dependencies (Servant.OAuth2.IDP.*)

| Namespace | Reason |
|-----------|--------|
| `MCP.*` | Package extraction goal |

**Note**: `Plow.Logging` (IOTracer, traceWith) is ALLOWED in Servant modules. Plow is an external package providing generic tracing infrastructure, not MCP-specific. The `IOTracer` abstraction is protocol-agnostic and suitable for reuse.

## MCP.* Module Dependencies

MCP modules MAY import from Servant.

**Note**: `MCP.Trace.OAuth` is DELETED. `MCP.Trace.HTTP` imports `OAuthTrace` and `renderOAuthTrace` directly from `Servant.OAuth2.IDP.Trace`.

```haskell
-- Allowed (MCP imports FROM Servant)
import Servant.OAuth2.IDP.Config (OAuthEnv (..))
import Servant.OAuth2.IDP.Trace (OAuthTrace (..), renderOAuthTrace)
import Servant.OAuth2.IDP.Metadata (OAuthMetadata (..), ProtectedResourceMetadata (..))
import Servant.OAuth2.IDP.PKCE (validateCodeVerifier, generateCodeChallenge)
import Servant.OAuth2.IDP.Errors (ValidationError (..), AuthorizationError (..), LoginFlowError (..))
```

## Verification Script

```bash
#!/bin/bash
# verify-no-mcp-imports.sh

echo "Checking for MCP imports in Servant.OAuth2.IDP modules..."
VIOLATIONS=$(rg "^import MCP\." src/Servant/)

if [ -n "$VIOLATIONS" ]; then
    echo "ERROR: Found MCP imports in Servant modules:"
    echo "$VIOLATIONS"
    exit 1
else
    echo "OK: No MCP imports found in Servant modules"
    exit 0
fi
```

## CI Integration

Add to CI pipeline:
```yaml
- name: Verify module dependencies
  run: |
    if rg "^import MCP\." src/Servant/; then
      echo "ERROR: MCP imports found in Servant modules"
      exit 1
    fi
```
