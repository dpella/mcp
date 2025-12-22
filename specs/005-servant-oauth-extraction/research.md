# Research: Remove MCP Imports from Servant.OAuth2.IDP

**Branch**: `005-servant-oauth-extraction`
**Date**: 2025-12-18

## R-001: MonadTime Package Availability

**Question**: Is `Control.Monad.Time` from the `monad-time` package already a direct dependency?

**Investigation**:
```haskell
-- src/MCP/Server/Time.hs
module MCP.Server.Time (MonadTime (..)) where
import Control.Monad.Time (MonadTime (..))
```

The MCP.Server.Time module is a thin re-export layer. The `monad-time` package is already a transitive dependency.

**Decision**: Direct import from `Control.Monad.Time` is safe - no new package dependency needed.

**Rationale**: Eliminates unnecessary indirection. The Servant modules can directly depend on `monad-time` which is already in the dependency tree.

**Alternatives Considered**:
- Keep MCP.Server.Time re-export: Rejected - creates unnecessary coupling
- Define own MonadTime typeclass: Rejected - reinventing the wheel

---

## R-002: Test Module Documentation Typo

**Question**: What is `MCP.Server.OAuth.Test.Internal` that Test.Internal references?

**Investigation**:

The file `src/Servant/OAuth2/IDP/Test/Internal.hs` has:
- Line 7: Doc comment says `Module : MCP.Server.OAuth.Test.Internal` (TYPO)
- Line 37: Actual declaration is `module Servant.OAuth2.IDP.Test.Internal`
- Line 22: Doc example shows `import MCP.Server.OAuth.Test.Internal` (TYPO in example)

The references to `MCP.Server.OAuth.Test.Internal` are in documentation comments and examples, not actual import statements. The module exists as `Servant.OAuth2.IDP.Test.Internal`.

**Decision**: Fix the documentation typo. The module is correctly named `Servant.OAuth2.IDP.Test.Internal`; only the doc header and usage example need updating.

**Rationale**: This is a minor documentation fix, not dead code removal.

**Alternatives Considered**:
- Leave typo: Rejected - causes confusion and doesn't match actual module name

---

## R-003: OAuthTrace Design

**Question**: Should `Servant.OAuth2.IDP.Trace.OAuthTrace` duplicate `MCP.Trace.OAuth.OAuthTrace` or move it?

**Investigation**:
```haskell
-- src/MCP/Trace/OAuth.hs
{- | OAuth 2.0 flow events.

This type is semantically independent of MCP for future package separation.
-}
data OAuthTrace = ...
```

The existing `MCP.Trace.OAuth.OAuthTrace` was designed to be MCP-independent. However:

1. `MCP.Trace.HTTP` imports and embeds it: `HTTPOAuth OAuthTrace`
2. Moving it would require updating `MCP.Trace.HTTP` to import from Servant
3. This creates a dependency in the wrong direction (MCP depending on Servant)

**Decision**: Create a new `Servant.OAuth2.IDP.Trace` module with its own `OAuthTrace` type. The Servant handlers emit Servant trace events. MCP adapts at the boundary using `contramap` to convert to `MCP.Trace.HTTP.HTTPTrace`.

**Rationale**:
- Clean separation: Servant modules have no knowledge of MCP trace infrastructure
- Flexibility: Different trace granularity for different contexts
- Contramap pattern: Standard approach for trace adaptation

**Alternatives Considered**:
- Move `MCP.Trace.OAuth.OAuthTrace` to Servant: Rejected - creates wrong dependency direction
- Re-export from MCP: Rejected - doesn't achieve package separation goal
- Share via third package: Rejected - overkill for this use case

---

## R-004: OAuthEnv vs HTTPServerConfig + OAuthConfig

**Question**: What configuration fields are actually needed by Servant handlers?

**Investigation** (grep of handler files):

From `HTTPServerConfig`:
- `httpBaseUrl :: Text` - constructing OAuth redirect URLs and metadata

From `OAuthConfig` (nested in `HTTPServerConfig.httpOAuthConfig`):
- `authCodeExpirySeconds :: Int` - authorization code lifetime
- `accessTokenExpirySeconds :: Int` - access token lifetime
- `loginSessionExpirySeconds :: Int` - pending authorization lifetime
- `authCodePrefix :: Text` - prefix for generated auth codes
- `refreshTokenPrefix :: Text` - prefix for generated refresh tokens
- `clientIdPrefix :: Text` - prefix for generated client IDs
- `supportedScopes :: [Scope]` - metadata response
- `supportedResponseTypes :: [ResponseType]` - metadata response
- `supportedGrantTypes :: [GrantType]` - metadata response
- `supportedAuthMethods :: [ClientAuthMethod]` - metadata response
- `supportedCodeChallengeMethods :: [CodeChallengeMethod]` - metadata response

Demo-mode fields (MCP-specific, NOT needed in handlers):
- `autoApproveAuth :: Bool`
- `demoUserIdTemplate :: Maybe Text`
- `demoEmailDomain :: Text`
- `demoUserName :: Text`

**Decision**: Create `OAuthEnv` with protocol-agnostic fields only. Demo mode logic stays in MCP-specific code paths.

**Rationale**:
- `OAuthEnv` represents pure OAuth 2.1 configuration
- Demo mode is an MCP implementation detail, not protocol-required
- Handlers parameterized by `OAuthEnv` are reusable in non-demo contexts

**Type Design**:
```haskell
data OAuthEnv = OAuthEnv
    { oauthBaseUrl :: Text
    , oauthAuthCodeExpiry :: NominalDiffTime
    , oauthAccessTokenExpiry :: NominalDiffTime
    , oauthLoginSessionExpiry :: NominalDiffTime
    , oauthAuthCodePrefix :: Text
    , oauthRefreshTokenPrefix :: Text
    , oauthClientIdPrefix :: Text
    , oauthSupportedScopes :: [Scope]
    , oauthSupportedResponseTypes :: [ResponseType]
    , oauthSupportedGrantTypes :: [GrantType]
    , oauthSupportedAuthMethods :: [ClientAuthMethod]
    , oauthSupportedCodeChallengeMethods :: [CodeChallengeMethod]
    }
```

**Alternatives Considered**:
- Include demo fields: Rejected - couples Servant to MCP demo functionality
- Use typeclass for config: Rejected - explicit record simpler, no laws needed
- Pass individual fields: Rejected - too many parameters, hard to maintain

---

## R-005: Handler Signature Refactoring

**Question**: How should handler type signatures change to use `OAuthEnv` instead of `HTTPServerConfig`?

**Investigation**:

Current pattern:
```haskell
handleToken ::
    ( OAuthStateStore m
    , AuthBackend m
    , MonadReader env m
    , HasType HTTPServerConfig env
    , HasType (IOTracer HTTPTrace) env
    , ...
    ) => ...
```

Fields accessed:
```haskell
config <- view (typed @HTTPServerConfig)
let baseUrl = httpBaseUrl config
let oauthCfg = fromMaybe defaultOAuthConfig (httpOAuthConfig config)
let prefix = authCodePrefix oauthCfg
```

**Decision**: Replace `HasType HTTPServerConfig env` with `HasType OAuthEnv env`. Update field access to use `OAuthEnv` fields directly.

New pattern:
```haskell
handleToken ::
    ( OAuthStateStore m
    , AuthBackend m
    , MonadReader env m
    , HasType OAuthEnv env
    , HasType (IOTracer OAuthTrace) env
    , ...
    ) => ...
```

Field access:
```haskell
oauthEnv <- view (typed @OAuthEnv)
let baseUrl = oauthBaseUrl oauthEnv
let prefix = oauthAuthCodePrefix oauthEnv
```

**Rationale**: Direct field access without `Maybe` unwrapping. All configuration is required at construction time.

---

## R-006: MCP.Server.Auth Module Split

**Question**: What stays in `MCP.Server.Auth` after moving types to Servant?

**Investigation**:

Current exports:
```haskell
module MCP.Server.Auth (
    -- Re-exported
    module Servant.OAuth2.IDP.Auth.Backend,

    -- OAuth Configuration (MCP-specific)
    OAuthConfig (..),
    OAuthProvider (..),
    OAuthGrantType (..),

    -- Token Validation
    TokenInfo (..),
    extractBearerToken,

    -- PKCE Support
    PKCEChallenge (..),
    generateCodeVerifier,     -- IO action (stays in MCP)
    generateCodeChallenge,    -- MOVE to Servant
    validateCodeVerifier,     -- MOVE to Servant

    -- Metadata Discovery
    OAuthMetadata (..),       -- MOVE to Servant

    -- Protected Resource Metadata
    ProtectedResourceMetadata (..), -- MOVE to Servant
    ProtectedResourceAuth,
    ProtectedResourceAuthConfig (..),
)
```

**Decision**:

Types staying in `MCP.Server.Auth`:
- `OAuthConfig` - MCP-specific with demo mode fields
- `OAuthProvider` - MCP provider configuration
- `OAuthGrantType` - MCP grant type enum
- `TokenInfo` - token introspection response
- `extractBearerToken` - utility function
- `PKCEChallenge` - PKCE data container
- `generateCodeVerifier` - IO action using cryptonite
- `ProtectedResourceAuth` - type-level auth tag
- `ProtectedResourceAuthConfig` - config for above

Types moving to `Servant.OAuth2.IDP.*`:
- `OAuthMetadata` -> `Servant.OAuth2.IDP.Metadata`
- `ProtectedResourceMetadata` -> `Servant.OAuth2.IDP.Metadata`
- `validateCodeVerifier` -> `Servant.OAuth2.IDP.PKCE`
- `generateCodeChallenge` -> `Servant.OAuth2.IDP.PKCE`

**Rationale**:
- IO actions stay in MCP (generateCodeVerifier uses cryptonite random)
- Pure functions move to Servant (validateCodeVerifier, generateCodeChallenge)
- Metadata types move to Servant (used in OAuth discovery endpoints)

---

## Summary of Decisions

| Item | Decision |
|------|----------|
| MonadTime | Direct import from `Control.Monad.Time` |
| Doc typo | Fix `MCP.Server.OAuth.Test.Internal` references in doc comments |
| OAuthTrace | New type in `Servant.OAuth2.IDP.Trace`, MCP adapts via contramap |
| OAuthEnv | New record with protocol-agnostic fields only |
| Demo mode | Stays in MCP, not part of OAuthEnv |
| Handler signatures | Replace `HasType HTTPServerConfig` with `HasType OAuthEnv` |
| MCP.Server.Auth | Keep IO actions, move pure functions and metadata types |
