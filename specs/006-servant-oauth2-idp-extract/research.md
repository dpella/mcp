# Research: Extract servant-oauth2-idp Package

**Feature**: 006-servant-oauth2-idp-extract
**Date**: 2025-12-22
**Status**: Complete

## Research Topics

### 1. Git History Extraction with git-filter-repo

**Decision**: Use `git filter-repo` with `--path` and `--path-rename` options

**Rationale**:
- `git filter-repo` is the modern replacement for `git filter-branch`
- Supports preserving commit history for specific paths
- Handles path renaming to restructure the repository
- Already available in the environment (`/home/claude/.nix-profile/bin/git-filter-repo`)

**Alternatives Considered**:
- `git filter-branch`: Deprecated, slower, error-prone
- `git subtree split`: Simpler but doesn't preserve full history as cleanly
- Manual copy: Loses git history entirely

**Implementation Pattern**:
```bash
# Clone the repository
git clone /home/claude/workspace ~/vendor/servant-oauth2-idp
cd ~/vendor/servant-oauth2-idp

# Filter to keep only Servant.OAuth2.IDP files
git filter-repo \
  --path src/Servant/OAuth2/IDP/ \
  --path src/Servant/OAuth2/IDP.hs \
  --path test/Servant/OAuth2/IDP/ \
  --path test/Laws/ \
  --force

# Result: Repository with only OAuth2-related commits
```

### 2. Cabal Multi-Package Project Setup

**Decision**: Use `cabal.project` with `packages:` stanza for local development

**Rationale**:
- Standard Haskell approach for multi-package development
- Allows both packages to be built and tested together
- No need for publishing to Hackage during development
- Easy migration: remove vendor/ entry when package is published

**Alternatives Considered**:
- Git submodules: Adds complexity for development workflow
- Stack: Project uses cabal, switching adds friction
- Hackage-only: Blocks development until package is published

**Implementation Pattern**:
```cabal
-- cabal.project
packages:
  .
  vendor/servant-oauth2-idp
```

### 3. Test Infrastructure Strategy

**Decision**: Create minimal TestMonad in servant-oauth2-idp using AuthUser from Demo

**Rationale**:
- AuthUser is already defined in Servant.OAuth2.IDP.Auth.Demo (moves with package)
- AuthUser already has Arbitrary instances (defined in same module)
- Polymorphic Laws specs need concrete instances to run
- Minimal TestMonad avoids duplicating complex test infrastructure
- MCP retains its TestMonad for integration tests

**Alternatives Considered**:
- Move existing TestMonad: Has MCP.Server.Time dependency (minor, but adds coupling)
- Export only polymorphic specs: servant-oauth2-idp would have no runnable tests
- Create new test user type: Unnecessary when AuthUser already works

**Implementation Pattern**:
```haskell
-- vendor/servant-oauth2-idp/test/TestMonad.hs
newtype TestM a = TestM { unTestM :: ReaderT TestEnv IO a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader TestEnv)

instance MonadTime TestM where ...
instance OAuthStateStore TestM where
  type OAuthUser TestM = AuthUser  -- From Servant.OAuth2.IDP.Auth.Demo
  ...
instance AuthBackend TestM where
  type AuthBackendUser TestM = AuthUser
  ...
```

### 4. Module Dependency Analysis

**Decision**: servant-oauth2-idp has zero MCP dependencies (verified)

**Evidence**:
- CLAUDE.md documents "Core Invariant: All Servant.OAuth2.IDP.* modules have zero MCP dependencies"
- Grep analysis confirms no imports from MCP.* in Servant.OAuth2.IDP modules
- The only cross-reference is MCP.Server.Time re-exporting Control.Monad.Time (easily replaced)

**Dependency Direction**:
```
servant-oauth2-idp (standalone)
       ↑
       │ depends on
       │
      mcp (imports from servant-oauth2-idp)
```

### 5. Package Metadata Strategy

**Decision**: Mirror mcp's metadata with appropriate changes

**Rationale**:
- Same author, maintainer, license (MIT)
- Different category: "Web" instead of "Network" (OAuth is web-specific)
- Version 0.1.0.0 (pre-release, API may change)
- Independent homepage/repo once published

**Implementation Pattern**:
```cabal
-- servant-oauth2-idp.cabal
name:               servant-oauth2-idp
version:            0.1.0.0
synopsis:           OAuth 2.0 Identity Provider implementation for Servant
license:            MIT
author:             Matthias Pall Gissurarson, PakSCADA LLC
maintainer:         mpg@mpg.is, alberto.valverde@pakenergy.com
category:           Web
```

### 6. Test File Classification

**Decision**: Clear boundary between OAuth2-specific and MCP integration tests

**Files Moving to servant-oauth2-idp**:
| File | Reason |
|------|--------|
| `test/Servant/OAuth2/IDP/**` | Direct OAuth2 module tests |
| `test/Laws/OAuthStateStoreSpec.hs` | Tests OAuthStateStore typeclass |
| `test/Laws/AuthBackendSpec.hs` | Tests AuthBackend typeclass |
| `test/Laws/AuthBackendAssociatedTypesSpec.hs` | Tests AuthBackend associated types |
| `test/Laws/AuthBackendSignatureSpec.hs` | Tests AuthBackend signature |
| `test/Laws/AuthCodeFunctorSpec.hs` | Tests AuthorizationCode functor |
| `test/Laws/ConsumeAuthCodeSpec.hs` | Tests consumeAuthCode operation |
| `test/Laws/OAuthUserTypeSpec.hs` | Tests OAuthUser type |
| `test/Laws/BoundarySpec.hs` | Tests OAuth boundary conditions |
| `test/Laws/ErrorBoundarySecuritySpec.hs` | Tests OAuth error security |

**Files Remaining in mcp**:
| File | Reason |
|------|--------|
| `test/TestMonad.hs` | MCP's integration test infrastructure |
| `test/Functional/OAuthFlowSpec.hs` | End-to-end MCP+OAuth flow |
| `test/MCP/Server/**` | MCP-specific tests |
| `test/Trace/**` | MCP tracing tests |
| `test/Security/SessionCookieSpec.hs` | MCP security tests |

### 7. Import Update Strategy

**Decision**: Update MCP.Server.HTTP imports to use servant-oauth2-idp

**Files Requiring Updates**:
1. `src/MCP/Server/HTTP.hs` - Main HTTP server
2. `src/MCP/Server/HTTP/AppEnv.hs` - Application environment
3. `src/MCP/Server/Auth.hs` - MCP-specific auth types

**Pattern**:
```haskell
-- Before (in mcp)
import Servant.OAuth2.IDP.Config (OAuthEnv(..))

-- After (same import, but from dependency)
import Servant.OAuth2.IDP.Config (OAuthEnv(..))
-- No change needed! The import path stays the same,
-- only the source changes from local to dependency.
```

## Summary

All research topics resolved. Key findings:

1. **git-filter-repo** available and appropriate for history extraction
2. **cabal.project** standard approach for multi-package development
3. **Minimal TestMonad** using AuthUser from Demo is the cleanest approach
4. **Zero MCP dependencies** in servant-oauth2-idp confirmed
5. **Package metadata** mirrors mcp with appropriate changes
6. **Clear test boundary** between OAuth2-specific and MCP integration tests
7. **Import paths unchanged** after extraction (dependency vs local source)
