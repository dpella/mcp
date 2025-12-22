# Implementation Plan: Remove MCP Imports from Servant.OAuth2.IDP

**Branch**: `005-servant-oauth-extraction` | **Date**: 2025-12-18 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/005-servant-oauth-extraction/spec.md`

## Summary

Prepare `Servant.OAuth2.IDP.*` modules for extraction to a separate package by removing all `MCP.*` dependencies. This is a pure refactoring: move types to new Servant modules, use explicit record parameters, clean break with no backwards-compatibility re-exports.

## Technical Context

**Language/Version**: Haskell GHC2021 (GHC 9.4+ via base ^>=4.18.2.1)
**Primary Dependencies**: servant-server 0.19-0.20, servant-auth-server 0.4, aeson 2.1-2.2, monad-time, network-uri, cryptonite 0.30, generic-lens, QuickCheck >= 2.14 (library dep for Arbitrary instances in type modules)
**Storage**: N/A (refactoring only, no data changes)
**Testing**: cabal test (HSpec test suite)
**Target Platform**: Linux server
**Project Type**: Single Haskell library
**Performance Goals**: N/A (no runtime changes)
**Constraints**: Must maintain API compatibility for OAuth functionality
**Scale/Scope**: ~17 files modified, 5 new files created, 1 file deleted (includes Phase F type precision fixes)

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

Reference: `.specify/memory/constitution.md`

| Principle | Status | Evidence/Notes |
|-----------|--------|----------------|
| I. Type-Driven Design | :white_check_mark: | `OAuthEnv` record designed with explicit typed fields; `OAuthTrace` ADT with typed constructors; moved types preserve existing validation (CodeChallenge, CodeVerifier newtypes) |
| II. Deep Module Architecture | :white_check_mark: | New modules expose minimal public interface; `OAuthEnv` hides configuration complexity behind single record; PKCE internals hidden behind `validateCodeVerifier` |
| III. Denotational Semantics | :white_check_mark: | PKCE validation has clear mathematical semantics (S256 hash comparison); no new abstractions requiring law documentation |
| IV. Total Functions | :white_check_mark: | `validateCodeVerifier :: CodeVerifier -> CodeChallenge -> Bool` - total function returning Bool; `generateCodeChallenge :: Text -> Text` - total |
| V. Pure Core, Impure Shell | :white_check_mark: | All new modules are pure (`Trace`, `Config`, `Metadata`, `PKCE`); impure operations remain at boundary in handlers |
| VI. Property-Based Testing | :white_check_mark: | Existing tests preserved; PKCE round-trip testable: `validateCodeVerifier (generateCodeVerifier) (generateCodeChallenge verifier) == True` |

## GOLDEN RULE: Smart Constructor Hygiene

> **Only code in the type-defining module needs audit for correct construction.**

This rule emerged from the 2025-12-22 cleanup session and drives the following architectural decisions:

1. **Typeclass instances ARE the boundary** - `FromJSON`, `FromHttpApiData`, etc. live in the type-defining module and go through smart constructors. No separate `Boundary.hs` needed.

2. **Arbitrary instances live with types** - QuickCheck's `Arbitrary` instances belong in the type-defining module (e.g., `Types.hs`), not a separate `test/Generators.hs`. GHC dead code elimination removes unused instances from production binaries.

3. **No `unsafe*` exports** - Smart constructors (`mkFoo`) are the ONLY way to construct validated types. Tests use the same public API as production code—no special privileges.

4. **Generators live with types** - ID generation functions (`generateAuthCodeId`, `generateClientId`, etc.) live in `Types.hs` where they can use internal constructors correctly.

**Audit scope**: When reviewing code safety, you only need to audit the type-defining module. If the smart constructor is correct, all consumers are correct by construction.

## Project Structure

### Documentation (this feature)

```text
specs/005-servant-oauth-extraction/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
└── contracts/           # Phase 1 output (N/A for refactoring)
```

### Source Code (repository root)

```text
src/
├── Servant/OAuth2/IDP/
│   ├── API.hs              # MODIFY: Import Metadata from new location
│   ├── Auth/
│   │   ├── Backend.hs      # MODIFY: Smart constructor hygiene (Username export)
│   │   └── Demo.hs         # UNCHANGED
│   ├── Boundary.hs         # (DELETED - typeclass instances are the boundary)
│   ├── Config.hs           # NEW: OAuthEnv record
│   ├── Handlers/
│   │   ├── Authorization.hs  # MODIFY: Use OAuthEnv, OAuthTrace
│   │   ├── Helpers.hs        # (DELETED - generators moved to Types.hs)
│   │   ├── HTML.hs           # UNCHANGED
│   │   ├── Login.hs          # MODIFY: Use OAuthEnv, OAuthTrace
│   │   ├── Metadata.hs       # MODIFY: Use OAuthEnv, import from Servant
│   │   ├── Registration.hs   # MODIFY: Use OAuthEnv, OAuthTrace
│   │   └── Token.hs          # MODIFY: Use OAuthEnv, OAuthTrace, PKCE, typed handler params
│   ├── Errors.hs             # NEW: ValidationError, AuthorizationError (with ADT payloads), LoginFlowError, OAuthErrorCode
│   ├── Handlers.hs           # UNCHANGED (re-exports)
│   ├── Metadata.hs           # NEW: OAuthMetadata, ProtectedResourceMetadata
│   ├── PKCE.hs               # NEW: validateCodeVerifier, generateCodeChallenge
│   ├── Server.hs             # MODIFY: Use OAuthEnv, OAuthTrace
│   ├── Store/
│   │   └── InMemory.hs       # MODIFY: MonadTime import
│   ├── Store.hs              # MODIFY: MonadTime import
│   ├── Test/
│   │   └── Internal.hs       # MODIFY: MonadTime import, fix doc comment typo
│   ├── Trace.hs              # NEW: OAuthTrace ADT
│   └── Types.hs              # MODIFY: Smart constructor hygiene (8 exports), ClientInfo.clientName
└── MCP/
    ├── Server/
    │   ├── Auth.hs           # MODIFY: Remove moved types
    │   ├── HTTP/
    │   │   └── AppEnv.hs     # MODIFY: Add OAuthEnv, tracer adapter
    │   └── Time.hs           # UNCHANGED
    └── Trace/
        ├── HTTP.hs           # UNCHANGED (still uses MCP.Trace.OAuth)
        └── OAuth.hs          # (DELETED - OAuthTrace moved to Servant.OAuth2.IDP.Trace)
```

**Structure Decision**: Single library structure. New modules added under `Servant.OAuth2.IDP.*` namespace to maintain package extraction path.

## Complexity Tracking

> No violations requiring justification. All changes follow Constitution principles.

| Violation | Why Needed | Simpler Alternative Rejected Because |
|-----------|------------|-------------------------------------|
| N/A | N/A | N/A |

---

## Phase 0: Research

### R-001: MonadTime Package Availability

**Question**: Is `Control.Monad.Time` from the `monad-time` package already a direct dependency?

**Finding**: Yes. The `MCP.Server.Time` module is a thin re-export:
```haskell
module MCP.Server.Time (MonadTime (..)) where
import Control.Monad.Time (MonadTime (..))
```

**Decision**: Direct import from `Control.Monad.Time` is safe - no new dependency needed.

### R-002: Test Module Documentation Typo

**Question**: What is `MCP.Server.OAuth.Test.Internal` that Test.Internal references?

**Finding**: The file `src/Servant/OAuth2/IDP/Test/Internal.hs` declares `module Servant.OAuth2.IDP.Test.Internal` (line 37), but the doc comment header says `Module : MCP.Server.OAuth.Test.Internal` (line 7). The references at lines 22, etc. are in documentation examples, not actual imports.

**Decision**: Fix the documentation typo. The module exists and is correctly named; only doc comments need updating.

### R-003: OAuthTrace Design

**Question**: Should `OAuthTrace` duplicate `MCP.Trace.OAuth.OAuthTrace` or import it?

**Finding**: `MCP.Trace.OAuth.OAuthTrace` already exists and is MCP-independent (comment in file: "This module is intentionally MCP-independent to enable future package separation").

**Decision**: Move the type from `MCP.Trace.OAuth` to `Servant.OAuth2.IDP.Trace`, then have MCP re-export or adapt. This achieves true separation. However, examining the architecture more closely:

- `MCP.Trace.HTTP` imports `MCP.Trace.OAuth` and embeds it via `HTTPOAuth OAuthTrace`
- Changing this creates a larger refactoring scope

**Revised Decision**: Create `Servant.OAuth2.IDP.Trace` with a new trace type. Handlers will emit via the Servant trace type. MCP adapts at boundary via `contramap`. This is cleaner separation.

### R-004: OAuthEnv vs HTTPServerConfig + OAuthConfig

**Question**: What fields from `HTTPServerConfig` and `OAuthConfig` are actually used in Servant handlers?

**Finding** (from grep of handler files):
- `httpBaseUrl` - for constructing redirect URLs
- `httpOAuthConfig` - for:
  - `authCodeExpirySeconds`
  - `accessTokenExpirySeconds`
  - `authCodePrefix`
  - `refreshTokenPrefix`
  - `clientIdPrefix`
  - `supportedScopes`
  - `supportedResponseTypes`
  - `supportedGrantTypes`
  - `supportedAuthMethods`
  - `supportedCodeChallengeMethods`
  - `loginSessionExpirySeconds`
  - `autoApproveAuth` (demo mode)
  - `demoUserIdTemplate` (demo mode)
  - `demoEmailDomain` (demo mode)

**Decision**: `OAuthEnv` should contain:
1. Core fields: baseUrl, expiry times, prefixes, supported params
2. NOT demo mode fields - those are MCP-specific

The handlers should be parameterized to work without demo mode fields. Demo mode logic stays in MCP.

### R-005: OAuthConfig Removal Strategy (Spec Refinement 2025-12-19)

**Question**: MCPOAuthConfig field names (autoApproveAuth, demoUserIdTemplate, etc.) collide with existing OAuthConfig fields. What naming strategy?

**Finding**: Analysis of handler usage revealed:
- **Protocol fields used by Servant handlers**: `requireHTTPS`, `authCodeExpirySeconds`, `accessTokenExpirySeconds`, `loginSessionExpirySeconds`, `supportedScopes/ResponseTypes/GrantTypes/AuthMethods/CodeChallengeMethods`, `authCodePrefix`, `refreshTokenPrefix`, `clientIdPrefix`
- **Demo fields NOT used by Servant handlers**: `autoApproveAuth`, `demoUserIdTemplate`, `demoEmailDomain`, `demoUserName`, `publicClientSecret`, `authorizationSuccessTemplate`, `oauthProviders`

**Decision**: Remove `OAuthConfig` entirely (Option B from spec clarification).
- `OAuthConfig` → **REMOVED** (replaced by split types)
- Protocol fields → `OAuthEnv` (Servant.OAuth2.IDP.Config)
- Demo/MCP fields → `MCPOAuthConfig` (MCP.Server.Auth, **unprefixed** field names)
- `oauthEnabled` → **ELIMINATED** (presence of MCPOAuthConfig implies enabled)
- `credentialStore` → Already handled via AuthBackend typeclass

**Architecture After Split**:
```
HTTPServerConfig
  └── httpMCPOAuthConfig :: Maybe MCPOAuthConfig  -- OAuth enable flag + MCP-specific

AppEnv
  └── envOAuthEnv :: OAuthEnv  -- Protocol config, always present when OAuth wired
```

**Migration Aid**: Create `DemoOAuthBundle` convenience type + `defaultDemoOAuthBundle` for test migration (replaces `defaultDemoOAuthConfig`).

### R-006: handleProtectedResourceMetadata HTTPServerConfig Dependency (Spec Refinement 2025-12-19)

**Question**: How should `handleProtectedResourceMetadata` obtain resource server configuration without depending on `HTTPServerConfig` (MCP namespace)?

**Finding**: Current implementation in `Handlers/Metadata.hs`:
- Requires `HasType HTTPServerConfig env` to access `httpBaseUrl` and `httpProtectedResourceMetadata`
- Uses conditional logic: return override if present, else construct default from base URL
- This creates MCP dependency in Servant handler, violating extraction goal (FR-001)

**Decision**: Add resource server config directly to `OAuthEnv` (Spec clarification 2025-12-19):
- Add `resourceServerBaseUrl :: URI` field to `OAuthEnv`
- Add `resourceServerMetadata :: ProtectedResourceMetadata` field to `OAuthEnv` (not `Maybe` - direct config, no override pattern)
- Handler becomes trivial: `handleProtectedResourceMetadata = asks (oauthResourceServerMetadata . getTyped @OAuthEnv)`
- Construction logic moves to MCP layer: `mkOAuthEnv` builds `ProtectedResourceMetadata` from `HTTPServerConfig` fields

**Benefits**:
- Servant handler has zero MCP dependencies
- Cleaner separation: protocol config (Servant) vs application config (MCP)
- Simpler handler (no conditional logic)
- `defaultProtectedResourceMetadata` helper becomes optional (kept for testing/utilities)

---

## Phase 1: Design

### Data Model

See [data-model.md](./data-model.md).

### Key Types

#### OAuthEnv (new)

```haskell
data OAuthEnv = OAuthEnv
    { oauthRequireHTTPS :: Bool                                        -- Security flag (R-005)
    , oauthBaseUrl :: URI                                              -- URI from Network.URI
    , oauthAuthCodeExpiry :: NominalDiffTime
    , oauthAccessTokenExpiry :: NominalDiffTime
    , oauthLoginSessionExpiry :: NominalDiffTime
    , oauthAuthCodePrefix :: Text
    , oauthRefreshTokenPrefix :: Text
    , oauthClientIdPrefix :: Text
    , oauthSupportedScopes :: [Scope]                                  -- Can be empty
    , oauthSupportedResponseTypes :: NonEmpty ResponseType             -- RFC requires ≥1
    , oauthSupportedGrantTypes :: NonEmpty OAuthGrantType              -- RFC requires ≥1
    , oauthSupportedAuthMethods :: NonEmpty TokenAuthMethod            -- RFC requires ≥1
    , oauthSupportedCodeChallengeMethods :: NonEmpty CodeChallengeMethod  -- RFC requires ≥1
    , oauthResourceServerBaseUrl :: URI                                -- Base URL for resource server (R-006)
    , oauthResourceServerMetadata :: ProtectedResourceMetadata         -- RFC 9728 metadata (R-006)
    }
    deriving (Generic)
```

#### OAuthTrace (new)

```haskell
-- Supporting types for OAuthTrace (defined in Servant.OAuth2.IDP.Trace)
data OperationResult = Success | Failure
    deriving (Show, Eq)

data DenialReason
    = UserDenied
    | InvalidRequest
    | UnauthorizedClient
    | ServerError Text
    deriving (Show, Eq)

-- Main trace ADT using domain newtypes from Servant.OAuth2.IDP.Types
data OAuthTrace
    = TraceClientRegistration ClientId RedirectUri           -- Domain newtypes
    | TraceAuthorizationRequest ClientId [Scope] OperationResult
    | TraceLoginPageServed SessionId
    | TraceLoginAttempt Username OperationResult             -- Username from Types
    | TracePKCEValidation OperationResult
    | TraceAuthorizationGranted ClientId Username
    | TraceAuthorizationDenied ClientId DenialReason         -- ADT not Text
    | TraceTokenExchange OAuthGrantType OperationResult      -- OAuthGrantType from Types
    | TraceTokenRefresh OperationResult
    | TraceSessionExpired SessionId
    | TraceValidationError ValidationError                   -- ValidationError from Errors
    deriving (Show, Eq)
```

### Migration Path

1. **Phase A** (Additive): Create new Servant modules (`Trace`, `Config`, `Metadata`, `PKCE`, `Errors`)
2. **Phase B** (Update Servant): Change imports in Servant handlers to use new modules
3. **Phase C** (Update MCP): Add `OAuthEnv` to `AppEnv`, create adapter functions
4. **Phase D** (Clean Break): Remove moved types from `MCP.Server.Auth`

### Contracts

No external API contracts change. Internal module boundaries shift.

---

## Implementation Phases (for /speckit.tasks)

### Phase A: Create New Servant Modules

1. Create `src/Servant/OAuth2/IDP/Trace.hs` with `OAuthTrace` ADT and supporting types (`OperationResult`, `DenialReason`)
2. Create `src/Servant/OAuth2/IDP/Config.hs` with `OAuthEnv` record (includes `resourceServerBaseUrl` and `resourceServerMetadata` fields per R-006)
3. Create `src/Servant/OAuth2/IDP/Metadata.hs` with moved types (`OAuthMetadata`, `ProtectedResourceMetadata`)
4. Create `src/Servant/OAuth2/IDP/PKCE.hs` with moved functions (`generateCodeVerifier`, `validateCodeVerifier`, `generateCodeChallenge`)
5. Create `src/Servant/OAuth2/IDP/Errors.hs` with consolidated error types (`ValidationError`, `AuthorizationError`, `LoginFlowError`, `OAuthErrorCode`, `TokenParameter`). See Phase F for AuthorizationError ADT payloads.
6. Update `mcp-haskell.cabal` with new modules, remove `LoginFlowError` module

### Phase B: Update Servant Imports

1. Update `Store.hs` and `Store/InMemory.hs` - MonadTime import
2. Update `API.hs` - Metadata import
3. Update `Handlers/Metadata.hs` - use Servant Metadata, switch from HTTPServerConfig to OAuthEnv, simplify `handleProtectedResourceMetadata` to just return `oauthResourceServerMetadata` field (per R-006)
4. Update `Handlers/Token.hs` - use PKCE from Servant. See Phase F for handler signature changes.
5. Update `Handlers/Helpers.hs` - OAuthEnv and trace
6. Update `Handlers/Registration.hs` - OAuthEnv and trace
7. Update `Handlers/Authorization.hs` - OAuthEnv and trace
8. Update `Handlers/Login.hs` - OAuthEnv and trace
9. Update `Server.hs` - OAuthEnv and trace
10. Update `Test/Internal.hs` - fix doc comment typo, MonadTime import

### Phase C: Update MCP

1. Add `OAuthEnv` field to `AppEnv`
2. Create `mkOAuthEnv :: HTTPServerConfig -> OAuthEnv` (constructs `ProtectedResourceMetadata` from HTTPServerConfig fields or uses override if present, per R-006)
3. Create `mkOAuthTracer :: IOTracer HTTPTrace -> IOTracer OAuthTrace`
4. Update handler call sites to pass OAuthEnv

*See also Phase E below for OAuthConfig removal tasks (R-005 refinement).*

### Phase D: Clean Break

1. Remove `OAuthMetadata` from `MCP.Server.Auth` exports
2. Remove `ProtectedResourceMetadata` from `MCP.Server.Auth` exports
3. Remove `validateCodeVerifier` from `MCP.Server.Auth` exports
4. Remove `generateCodeChallenge` from `MCP.Server.Auth` exports
5. Verify: `rg "^import MCP\." src/Servant/` returns empty
6. Verify: `cabal build` succeeds
7. Verify: `cabal test` passes

*See also Phase E below for OAuthConfig removal tasks (R-005 refinement).*

---

### Phase E: OAuthConfig Removal (R-005 Refinement)

*Added 2025-12-19 per spec clarification. Depends on Phases C and D.*

#### E.1: Update MCPOAuthConfig (MCP.Server.Auth)

1. Remove `mcp` prefix from all MCPOAuthConfig fields:
   - `mcpAutoApproveAuth` → `autoApproveAuth`
   - `mcpDemoUserIdTemplate` → `demoUserIdTemplate`
   - `mcpDemoEmailDomain` → `demoEmailDomain`
   - `mcpAuthorizationSuccessTemplate` → `authorizationSuccessTemplate`

2. Add missing fields to MCPOAuthConfig:
   - `oauthProviders :: [OAuthProvider]`
   - `demoUserName :: Text`
   - `publicClientSecret :: Maybe Text`

3. Create `defaultDemoMCPOAuthConfig :: MCPOAuthConfig`

#### E.2: Remove OAuthConfig (MCP.Server.Auth)

1. Delete `OAuthConfig` data type entirely
2. Delete `defaultDemoOAuthConfig` function
3. Update all imports that reference `OAuthConfig`

#### E.3: Update HTTPServerConfig (MCP.Server.HTTP.AppEnv)

1. Replace `httpOAuthConfig :: Maybe OAuthConfig` with `httpMCPOAuthConfig :: Maybe MCPOAuthConfig`
2. Presence of `httpMCPOAuthConfig` implies OAuth enabled (replaces `oauthEnabled` field)

#### E.4: Create DemoOAuthBundle (MCP.Server.HTTP)

1. Create convenience type:
   ```haskell
   data DemoOAuthBundle = DemoOAuthBundle
       { bundleEnv :: OAuthEnv
       , bundleMCPConfig :: MCPOAuthConfig
       }
   ```

2. Create default bundle:
   ```haskell
   defaultDemoOAuthBundle :: DemoOAuthBundle
   defaultDemoOAuthBundle = DemoOAuthBundle
       { bundleEnv = defaultOAuthEnv { oauthRequireHTTPS = False }
       , bundleMCPConfig = defaultDemoMCPOAuthConfig
       }
   ```

3. Export from MCP.Server.HTTP for test convenience

#### E.5: Update Tests

1. Update `test/Security/SessionCookieSpec.hs`:
   - Replace `OAuthConfig` imports with `OAuthEnv` + `MCPOAuthConfig`
   - Replace `defaultDemoOAuthConfig` usage with `defaultDemoOAuthBundle`

2. Update `test/MCP/Server/HTTP/McpAuthSpec.hs`:
   - Replace `httpOAuthConfig = Just defaultDemoOAuthConfig` with bundle pattern

3. Update `test/MCP/Server/OAuth/Test/Fixtures.hs`:
   - Replace `defaultDemoOAuthConfig` usage with bundle pattern

4. Update `test/MCP/Server/AuthSpec.hs`:
   - Update MCPOAuthConfig tests to use unprefixed field names

#### E.6: Update Examples and Documentation

1. Update `examples/http-server.hs`:
   - Replace `defaultDemoOAuthConfig` with bundle pattern
   - Update OAuth configuration construction

2. Update `CLAUDE.md`:
   - Replace `OAuthConfig` references with `OAuthEnv` + `MCPOAuthConfig`
   - Update `defaultDemoOAuthConfig` references to bundle pattern

3. Update `README.md`:
   - Replace `httpOAuthConfig` references with `httpMCPOAuthConfig`

#### E.7: Verification

1. Verify: `rg "OAuthConfig" src/` shows only `MCPOAuthConfig` references
2. Verify: `rg "defaultDemoOAuthConfig" .` returns empty (all replaced)
3. Verify: `cabal build` succeeds
4. Verify: `cabal test` passes

---

### Phase F: Type Precision Refinements (Spec Refinement 2025-12-19)

*Added 2025-12-19 per codebase anti-pattern analysis. Can be done in parallel with Phase E.*

#### F.1: Smart Constructor Hygiene Fixes (Types.hs)

Fix 8 types that export raw constructors despite having smart constructor validation:

1. Change `AuthCodeId (..)` → `AuthCodeId` in module export list
2. Change `ClientId (..)` → `ClientId` in module export list
3. Change `SessionId (..)` → `SessionId` in module export list (UUID validation bypass)
4. Change `RefreshTokenId (..)` → `RefreshTokenId` in module export list
5. Change `UserId (..)` → `UserId` in module export list
6. Change `RedirectUri (..)` → `RedirectUri` in module export list (**critical**: SSRF protection bypass)
7. Change `Scope (..)` → `Scope` in module export list (RFC 6749 validation bypass)
8. Change `ClientName (..)` → `ClientName` in module export list

**Note**: `Types/Internal.hs` intentionally keeps `(..)` exports for boundary translation (by design).

#### F.2: Smart Constructor Hygiene Fixes (Auth/Backend.hs)

1. Change `Username (..)` → `Username` in module export list (non-empty validation bypass)

#### F.3: AuthorizationError ADT Payloads (Errors.hs)

Replace Text payloads with precise ADTs for exhaustive pattern matching:

1. Create `InvalidRequestReason` ADT:
   ```haskell
   data InvalidRequestReason
       = MissingParameter TokenParameter
       | InvalidParameterFormat TokenParameter
       | UnsupportedCodeChallengeMethod CodeChallengeMethod
       | MalformedRequest
   ```

2. Create `InvalidClientReason` ADT:
   ```haskell
   data InvalidClientReason
       = ClientNotFound ClientId
       | InvalidClientCredentials
       | ClientSecretMismatch
   ```

3. Create `InvalidGrantReason` ADT:
   ```haskell
   data InvalidGrantReason
       = CodeNotFound AuthCodeId
       | CodeExpired AuthCodeId
       | CodeAlreadyUsed AuthCodeId
       | RefreshTokenNotFound RefreshTokenId
       | RefreshTokenExpired RefreshTokenId
       | RefreshTokenRevoked RefreshTokenId
   ```

4. Create `UnauthorizedClientReason` ADT:
   ```haskell
   data UnauthorizedClientReason
       = GrantTypeNotAllowed OAuthGrantType
       | ScopeNotAllowed Scope
       | RedirectUriNotRegistered RedirectUri
   ```

5. Create `UnsupportedGrantTypeReason` ADT:
   ```haskell
   data UnsupportedGrantTypeReason
       = UnknownGrantType Text  -- Keep Text for unknown input
       | GrantTypeDisabled OAuthGrantType
   ```

6. Create `InvalidScopeReason` ADT:
   ```haskell
   data InvalidScopeReason
       = UnknownScope Text  -- Keep Text for unknown input
       | ScopeNotPermitted Scope
   ```

7. Create `AccessDeniedReason` ADT:
   ```haskell
   data AccessDeniedReason
       = UserDenied
       | ResourceOwnerDenied
       | ConsentRequired
   ```

8. Update `AuthorizationError` constructors to use new ADTs:
   ```haskell
   data AuthorizationError
       = InvalidRequest InvalidRequestReason
       | InvalidClient InvalidClientReason
       | InvalidGrant InvalidGrantReason
       | UnauthorizedClient UnauthorizedClientReason
       | UnsupportedGrantType UnsupportedGrantTypeReason
       | InvalidScope InvalidScopeReason
       | AccessDenied AccessDeniedReason
       | ExpiredCode
       | InvalidRedirectUri
       | PKCEVerificationFailed
   ```

9. Create `renderAuthorizationError :: AuthorizationError -> Text` for human-readable error_description

10. Update all handler call sites to use new ADT constructors

#### F.4: Token Handler Signature Changes (Handlers/Token.hs)

Eliminate `Map Text Text` anti-pattern by passing typed fields directly:

1. Change `handleAuthCodeGrant` signature:
   ```haskell
   -- FROM:
   handleAuthCodeGrant :: ... -> Map Text Text -> m TokenResponse
   -- TO:
   handleAuthCodeGrant :: ... -> AuthCodeId -> CodeVerifier -> Maybe ResourceIndicator -> m TokenResponse
   ```

2. Change `handleRefreshTokenGrant` signature:
   ```haskell
   -- FROM:
   handleRefreshTokenGrant :: ... -> Map Text Text -> m TokenResponse
   -- TO:
   handleRefreshTokenGrant :: ... -> RefreshTokenId -> Maybe ResourceIndicator -> m TokenResponse
   ```

3. Update `handleToken` to pass typed fields directly:
   ```haskell
   handleToken tokenRequest = case tokenRequest of
       AuthorizationCodeGrant code verifier mResource ->
           handleAuthCodeGrant code verifier mResource  -- Direct pass, no Map
       RefreshTokenGrant refreshToken mResource ->
           handleRefreshTokenGrant refreshToken mResource  -- Direct pass, no Map
   ```

4. Remove `Map.fromList` and `unAuthCodeId`/`unCodeVerifier` unwrapping from `handleToken`

5. Remove `Map.lookup` parsing logic from `handleAuthCodeGrant` and `handleRefreshTokenGrant`

#### F.5: Record Field Type Fixes (Types.hs)

1. Change `ClientInfo.clientName :: Text` → `ClientInfo.clientName :: ClientName`
2. Update `FromJSON ClientInfo` to parse through `ClientName` smart constructor
3. Update `ToJSON ClientInfo` to use `unClientName`

#### F.6: Verification

1. Verify: `rg "FIXME" src/Servant/OAuth2/IDP/` returns empty (all FIXMEs resolved)
2. Verify: `rg "\\.\\.\\.)" src/Servant/OAuth2/IDP/Types.hs` shows only legitimate enum exports
3. Verify: `rg "Map Text Text" src/Servant/OAuth2/IDP/Handlers/Token.hs` returns empty
4. Verify: `cabal build` succeeds
5. Verify: `cabal test` passes
