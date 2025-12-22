# Implementation Plan: Extract servant-oauth2-idp Package

**Branch**: `006-servant-oauth2-idp-package` | **Date**: 2025-12-22 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/006-servant-oauth2-idp-extract/spec.md`

## Summary

Extract the `Servant.OAuth2.IDP` namespace (20 modules) and related tests from the `mcp` package into a standalone `servant-oauth2-idp` package. The new package will be developed in `vendor/servant-oauth2-idp` with a `cabal.project` for multi-package development. Git history will be preserved via `git filter-repo` extraction to `~/vendor/servant-oauth2-idp`.

## Technical Context

**Language/Version**: Haskell GHC2021 (GHC 9.4+ via base ^>=4.18.2.1)
**Primary Dependencies**: servant-server 0.19-0.20, servant-auth-server 0.4, aeson 2.1-2.2, jose 0.10-0.11, crypton, monad-time
**Storage**: In-memory (TVar-based via STM); typeclass abstraction for backends
**Testing**: hspec 2.10-2.11, QuickCheck 2.14-2.15, hspec-wai
**Target Platform**: Linux/macOS (GHC-supported platforms)
**Project Type**: Multi-package Haskell workspace (cabal.project)
**Performance Goals**: N/A (package extraction, not new features)
**Constraints**: Zero MCP dependencies in servant-oauth2-idp; must preserve all existing functionality
**Scale/Scope**: 20 modules to extract, ~14 test files to move, 1 new TestMonad to create

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

Reference: `.specify/memory/constitution.md`

| Principle | Status | Evidence/Notes |
|-----------|--------|----------------|
| I. Type-Driven Design | ✅ | Existing types preserved; no new domain types needed for extraction |
| II. Deep Module Architecture | ✅ | servant-oauth2-idp will have clean public API; internal modules remain hidden |
| III. Denotational Semantics | ✅ | Polymorphic Laws specs (OAuthStateStore, AuthBackend) move with typeclasses |
| IV. Total Functions | ✅ | No changes to function totality; smart constructors remain |
| V. Pure Core, Impure Shell | ✅ | Typeclass-based design maintains pure/impure separation |
| VI. Property-Based Testing | ✅ | Property tests for typeclasses move to servant-oauth2-idp with new TestMonad |

## Project Structure

### Documentation (this feature)

```text
specs/006-servant-oauth2-idp-extract/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
└── tasks.md             # Phase 2 output (/speckit.tasks command)
```

### Source Code (after extraction)

```text
# Workspace root (mcp repository)
/home/claude/workspace/
├── cabal.project                    # NEW: Multi-package project file
├── mcp.cabal                        # MODIFIED: Depends on servant-oauth2-idp
├── src/
│   └── MCP/                         # Retained MCP-specific modules
│       ├── Types.hs
│       ├── Protocol.hs
│       ├── Server.hs
│       ├── Server/
│       │   ├── StdIO.hs
│       │   ├── HTTP.hs              # MODIFIED: Imports from servant-oauth2-idp
│       │   ├── HTTP/
│       │   │   └── AppEnv.hs        # MODIFIED: Imports from servant-oauth2-idp
│       │   ├── Auth.hs
│       │   └── Time.hs
│       └── Trace/                   # Retained tracing modules
├── test/
│   ├── Main.hs                      # MODIFIED: Reduced test modules
│   ├── TestMonad.hs                 # RETAINED: For mcp integration tests
│   ├── Functional/                  # RETAINED: MCP integration tests
│   ├── MCP/                         # RETAINED: MCP-specific tests
│   └── Trace/                       # RETAINED: Tracing tests
└── vendor/
    └── servant-oauth2-idp/          # NEW: Extracted package
        ├── servant-oauth2-idp.cabal
        ├── CLAUDE.md
        ├── README.md
        ├── .specify/                # Copied from parent
        ├── src/
        │   └── Servant/
        │       └── OAuth2/
        │           └── IDP/
        │               ├── API.hs
        │               ├── Config.hs
        │               ├── Types.hs
        │               ├── Metadata.hs
        │               ├── PKCE.hs
        │               ├── Store.hs
        │               ├── Store/
        │               │   └── InMemory.hs
        │               ├── Errors.hs
        │               ├── Trace.hs
        │               ├── Server.hs
        │               ├── Handlers.hs
        │               ├── Handlers/
        │               │   ├── HTML.hs
        │               │   ├── Metadata.hs
        │               │   ├── Registration.hs
        │               │   ├── Authorization.hs
        │               │   ├── Login.hs
        │               │   └── Token.hs
        │               ├── Test/
        │               │   └── Internal.hs
        │               └── Auth/
        │                   ├── Backend.hs
        │                   └── Demo.hs
        └── test/
            ├── Main.hs              # NEW: Test runner
            ├── TestMonad.hs         # NEW: Minimal test monad
            ├── Servant/
            │   └── OAuth2/
            │       └── IDP/         # MOVED: OAuth2-specific tests
            │           ├── APISpec.hs
            │           ├── BrandingSpec.hs
            │           ├── ConfigSpec.hs
            │           ├── TypesSpec.hs
            │           ├── ErrorsSpec.hs
            │           ├── PKCESpec.hs
            │           ├── MetadataSpec.hs
            │           ├── TraceSpec.hs
            │           ├── TokenRequestSpec.hs
            │           ├── LucidRenderingSpec.hs
            │           ├── CryptoEntropySpec.hs
            │           ├── BearerMethodSpec.hs
            │           └── Handlers/
            │               └── MetadataSpec.hs
            └── Laws/                # MOVED: Typeclass law tests
                ├── OAuthStateStoreSpec.hs
                ├── AuthBackendSpec.hs
                ├── AuthBackendAssociatedTypesSpec.hs
                ├── AuthBackendSignatureSpec.hs
                ├── AuthCodeFunctorSpec.hs
                ├── ConsumeAuthCodeSpec.hs
                ├── OAuthUserTypeSpec.hs
                ├── BoundarySpec.hs
                └── ErrorBoundarySecuritySpec.hs

# Separate extracted repository (for publication)
~/vendor/servant-oauth2-idp/         # Git history preserved
├── [same structure as vendor/servant-oauth2-idp above]
└── .git/                            # Extracted git history
```

**Structure Decision**: Multi-package Haskell workspace using `cabal.project` with `vendor/` directory for local development. Separate extracted repository at `~/vendor/servant-oauth2-idp` preserves git history for future publication.

## Complexity Tracking

> No constitution violations requiring justification.

| Violation | Why Needed | Simpler Alternative Rejected Because |
|-----------|------------|-------------------------------------|
| N/A | N/A | N/A |
