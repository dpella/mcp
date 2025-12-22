# Data Model: Extract servant-oauth2-idp Package

**Feature**: 006-servant-oauth2-idp-extract
**Date**: 2025-12-22
**Status**: Complete

## Overview

This document describes the entities involved in the package extraction. Since this is a refactoring task (not new feature development), the data model focuses on:

1. **Package structure** - The new servant-oauth2-idp package layout
2. **Module inventory** - Which modules move where
3. **Test file inventory** - Which tests move where
4. **Dependency relationships** - How packages depend on each other

## Package Entities

### 1. servant-oauth2-idp Package

**Identity**: Standalone Haskell package for OAuth 2.0 IDP
**Version**: 0.1.0.0
**Location (development)**: `vendor/servant-oauth2-idp/`
**Location (publication)**: `~/vendor/servant-oauth2-idp/`

```haskell
-- Package configuration (servant-oauth2-idp.cabal)
PackageConfig
  { name         :: "servant-oauth2-idp"
  , version      :: Version "0.1.0.0"
  , synopsis     :: "OAuth 2.0 Identity Provider implementation for Servant"
  , license      :: MIT
  , category     :: "Web"
  , dependencies :: [servant-server, servant-auth-server, aeson, jose, ...]
  }
```

### 2. mcp Package (Modified)

**Identity**: Model Context Protocol implementation (post-extraction)
**Change**: Adds dependency on servant-oauth2-idp, removes Servant.OAuth2.IDP modules

```haskell
-- Dependency change in mcp.cabal
build-depends:
  ...
  servant-oauth2-idp  -- NEW dependency
```

### 3. cabal.project (New)

**Identity**: Multi-package workspace configuration
**Location**: Repository root

```haskell
-- cabal.project structure
CabalProject
  { packages :: [".", "vendor/servant-oauth2-idp"]
  }
```

## Module Inventory

### Modules Moving to servant-oauth2-idp

| Module | Purpose | Dependencies |
|--------|---------|--------------|
| `Servant.OAuth2.IDP.API` | Servant API type definitions | servant |
| `Servant.OAuth2.IDP.Config` | OAuthEnv configuration | time, network-uri |
| `Servant.OAuth2.IDP.Types` | Core OAuth types, newtypes, Arbitrary instances | aeson, QuickCheck, uuid |
| `Servant.OAuth2.IDP.Metadata` | OAuth metadata types | aeson |
| `Servant.OAuth2.IDP.PKCE` | PKCE validation functions | crypton, base64-bytestring |
| `Servant.OAuth2.IDP.Store` | OAuthStateStore typeclass | mtl |
| `Servant.OAuth2.IDP.Store.InMemory` | TVar-based implementation | stm, monad-time |
| `Servant.OAuth2.IDP.Errors` | Error types and conversions | servant-server |
| `Servant.OAuth2.IDP.Trace` | OAuthTrace ADT for tracing | plow-log |
| `Servant.OAuth2.IDP.Server` | OAuth API composition | servant-server |
| `Servant.OAuth2.IDP.Handlers` | Handler re-exports | (none) |
| `Servant.OAuth2.IDP.Handlers.HTML` | HTML rendering | lucid |
| `Servant.OAuth2.IDP.Handlers.Metadata` | Metadata endpoints | servant-server |
| `Servant.OAuth2.IDP.Handlers.Registration` | Client registration | servant-server |
| `Servant.OAuth2.IDP.Handlers.Authorization` | Authorization endpoint | servant-server |
| `Servant.OAuth2.IDP.Handlers.Login` | Login flow | servant-server, lucid |
| `Servant.OAuth2.IDP.Handlers.Token` | Token exchange | servant-server, jose |
| `Servant.OAuth2.IDP.Test.Internal` | Test-only unsafe constructors | (none) |
| `Servant.OAuth2.IDP.Auth.Backend` | AuthBackend typeclass | mtl |
| `Servant.OAuth2.IDP.Auth.Demo` | Demo credentials, AuthUser | memory, QuickCheck |

**Total**: 20 modules

### Modules Remaining in mcp

| Module | Purpose | Change Required |
|--------|---------|-----------------|
| `MCP.Types` | MCP protocol types | None |
| `MCP.Protocol` | JSON-RPC messages | None |
| `MCP.Server` | MCPServer typeclass | None |
| `MCP.Server.StdIO` | StdIO transport | None |
| `MCP.Server.HTTP` | HTTP transport | Update imports |
| `MCP.Server.HTTP.AppEnv` | Application environment | Update imports |
| `MCP.Server.Auth` | MCP auth types | Update imports |
| `MCP.Server.Time` | MonadTime re-export | None |
| `MCP.Trace.*` | Tracing modules | None |

## Test File Inventory

### Tests Moving to servant-oauth2-idp

| Test File | Tests | Requires TestMonad |
|-----------|-------|-------------------|
| `Servant.OAuth2.IDP.APISpec` | API type tests | No |
| `Servant.OAuth2.IDP.BrandingSpec` | Branding config | No |
| `Servant.OAuth2.IDP.ConfigSpec` | OAuthEnv tests | No |
| `Servant.OAuth2.IDP.TypesSpec` | Type tests, JSON | No |
| `Servant.OAuth2.IDP.ErrorsSpec` | Error handling | No |
| `Servant.OAuth2.IDP.PKCESpec` | PKCE validation | No |
| `Servant.OAuth2.IDP.MetadataSpec` | Metadata types | No |
| `Servant.OAuth2.IDP.TraceSpec` | Trace ADT | No |
| `Servant.OAuth2.IDP.TokenRequestSpec` | Token parsing | No |
| `Servant.OAuth2.IDP.LucidRenderingSpec` | HTML rendering | No |
| `Servant.OAuth2.IDP.CryptoEntropySpec` | Crypto functions | No |
| `Servant.OAuth2.IDP.BearerMethodSpec` | Bearer auth | No |
| `Servant.OAuth2.IDP.Handlers.MetadataSpec` | Handler tests | No |
| `Laws.OAuthStateStoreSpec` | Typeclass laws | **Yes** |
| `Laws.AuthBackendSpec` | Typeclass laws | **Yes** |
| `Laws.AuthBackendAssociatedTypesSpec` | Associated types | **Yes** |
| `Laws.AuthBackendSignatureSpec` | Signatures | No |
| `Laws.AuthCodeFunctorSpec` | Functor laws | **Yes** |
| `Laws.ConsumeAuthCodeSpec` | Consume operation | **Yes** |
| `Laws.OAuthUserTypeSpec` | User type tests | **Yes** |
| `Laws.BoundarySpec` | Boundary tests | No |
| `Laws.ErrorBoundarySecuritySpec` | Security tests | No |

### Tests Remaining in mcp

| Test File | Reason |
|-----------|--------|
| `Functional.OAuthFlowSpec` | End-to-end MCP+OAuth integration |
| `MCP.Server.OAuth.TypesSpec` | MCP-specific OAuth types |
| `MCP.Server.OAuth.BoundarySpec` | MCP boundary tests |
| `MCP.Server.OAuth.AppSpec` | MCP app tests |
| `MCP.Server.HTTP.AppEnvSpec` | MCP environment tests |
| `MCP.Server.HTTP.McpAuthSpec` | MCP auth tests |
| `MCP.Server.AuthSpec` | MCP auth tests |
| `Security.SessionCookieSpec` | MCP security |
| `Trace.*Spec` | MCP tracing |
| `TestMonad` | MCP test infrastructure |

## TestMonad (New in servant-oauth2-idp)

```haskell
-- Minimal test monad for running polymorphic Laws specs
data TestEnv = TestEnv
  { testTime        :: IORef UTCTime
  , testOAuthState  :: IORef OAuthState
  , testCredentials :: IORef CredentialStore
  }

newtype TestM a = TestM { unTestM :: ReaderT TestEnv IO a }

instance MonadTime TestM where ...  -- Uses testTime
instance OAuthStateStore TestM where
  type OAuthStateError TestM = ()
  type OAuthStateEnv TestM = TestEnv
  type OAuthUser TestM = AuthUser  -- From Auth.Demo
  ...

instance AuthBackend TestM where
  type AuthBackendError TestM = ()
  type AuthBackendEnv TestM = TestEnv
  type AuthBackendUser TestM = AuthUser
  ...
```

## Dependency Graph

```
┌─────────────────────────────────────────────────────────────┐
│                     Hackage Packages                        │
│  servant-server, servant-auth-server, aeson, jose, etc.    │
└─────────────────────────────────────────────────────────────┘
                              ↑
                              │
              ┌───────────────┴───────────────┐
              │                               │
              ↓                               ↓
┌─────────────────────────┐   ┌─────────────────────────────┐
│   servant-oauth2-idp    │   │            mcp              │
│   (standalone package)  │   │    (MCP implementation)     │
│                         │   │                             │
│  - OAuth2 types         │   │  - MCP types & protocol     │
│  - Typeclasses          │←──│  - HTTP transport           │
│  - InMemory impl        │   │  - Uses OAuth2 for auth     │
│  - Handlers             │   │                             │
└─────────────────────────┘   └─────────────────────────────┘
              ↑                               ↑
              │                               │
              ↓                               ↓
┌─────────────────────────┐   ┌─────────────────────────────┐
│ servant-oauth2-idp-test │   │        mcp-test             │
│                         │   │                             │
│  - Laws specs           │   │  - Integration tests        │
│  - Unit tests           │   │  - Functional tests         │
│  - TestMonad            │   │  - TestMonad (mcp's)        │
└─────────────────────────┘   └─────────────────────────────┘
```

## File Operations Summary

| Operation | Count | Description |
|-----------|-------|-------------|
| Create | 1 | `cabal.project` |
| Create | 1 | `servant-oauth2-idp.cabal` |
| Create | 1 | `vendor/servant-oauth2-idp/CLAUDE.md` |
| Create | 1 | `vendor/servant-oauth2-idp/test/TestMonad.hs` |
| Create | 1 | `vendor/servant-oauth2-idp/test/Main.hs` |
| Move | 20 | Source modules to `vendor/servant-oauth2-idp/src/` |
| Move | 22 | Test files to `vendor/servant-oauth2-idp/test/` |
| Modify | 1 | `mcp.cabal` (add dependency, remove modules) |
| Modify | 1 | `test/Main.hs` (remove moved tests) |
| Copy | 1 | `.specify/` directory |

**Total operations**: ~50 file operations
