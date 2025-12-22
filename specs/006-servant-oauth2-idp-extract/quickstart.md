# Quickstart: Extract servant-oauth2-idp Package

**Feature**: 006-servant-oauth2-idp-extract
**Date**: 2025-12-22
**Status**: Complete

## Overview

This guide provides step-by-step instructions for extracting the `Servant.OAuth2.IDP` namespace into a standalone `servant-oauth2-idp` package.

## Prerequisites

- [x] git-filter-repo installed (`/home/claude/.nix-profile/bin/git-filter-repo`)
- [x] GHC 9.4+ with cabal-install
- [x] Write access to `~/vendor/` directory

## Phase 1: Package Structure Setup

### Step 1.1: Create vendor directory

```bash
mkdir -p vendor/servant-oauth2-idp
```

### Step 1.2: Create cabal.project

```bash
cat > cabal.project << 'EOF'
packages:
  .
  vendor/servant-oauth2-idp
EOF
```

### Step 1.3: Create servant-oauth2-idp.cabal

```cabal
cabal-version:      3.4
name:               servant-oauth2-idp
version:            0.1.0.0
synopsis:           OAuth 2.0 Identity Provider implementation for Servant
description:
    A standalone OAuth 2.0 Identity Provider (IdP) implementation for Servant.
    Provides typeclass-based abstractions for state management and authentication
    backends, with a default in-memory implementation suitable for development.

    Features:
    * OAuthStateStore typeclass for pluggable storage backends
    * AuthBackend typeclass for pluggable authentication
    * PKCE support (RFC 7636)
    * Dynamic client registration
    * Interactive login flow with Lucid HTML
    * Structured tracing via plow-log

license:            MIT
license-file:       LICENSE
author:             Matthias Pall Gissurarson, PakSCADA LLC
maintainer:         mpg@mpg.is, alberto.valverde@pakenergy.com
copyright:          (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
category:           Web
build-type:         Simple

common warnings
    ghc-options: -Wall -Wredundant-constraints -Wunused-packages -Werror

library
    import:           warnings
    exposed-modules:
        Servant.OAuth2.IDP.API
        Servant.OAuth2.IDP.Config
        Servant.OAuth2.IDP.Types
        Servant.OAuth2.IDP.Metadata
        Servant.OAuth2.IDP.PKCE
        Servant.OAuth2.IDP.Store
        Servant.OAuth2.IDP.Store.InMemory
        Servant.OAuth2.IDP.Errors
        Servant.OAuth2.IDP.Trace
        Servant.OAuth2.IDP.Server
        Servant.OAuth2.IDP.Handlers
        Servant.OAuth2.IDP.Handlers.HTML
        Servant.OAuth2.IDP.Handlers.Metadata
        Servant.OAuth2.IDP.Handlers.Registration
        Servant.OAuth2.IDP.Handlers.Authorization
        Servant.OAuth2.IDP.Handlers.Login
        Servant.OAuth2.IDP.Handlers.Token
        Servant.OAuth2.IDP.Test.Internal
        Servant.OAuth2.IDP.Auth.Backend
        Servant.OAuth2.IDP.Auth.Demo

    build-depends:
        base >= 4.18.2.1 && <= 4.21.0.0,
        aeson >= 2.1 && < 2.3,
        text >= 2.0 && <= 2.1.2,
        containers >= 0.6 && < 0.7,
        bytestring >= 0.11 && < 0.12,
        stm >= 2.5 && < 2.6,
        mtl >= 2.3 && < 2.4,
        servant-server >= 0.19 && < 0.21,
        servant >= 0.19 && < 0.21,
        http-types >= 0.12 && < 0.13,
        http-media >= 0.8 && < 0.9,
        http-api-data >= 0.6 && < 0.7,
        servant-auth-server >= 0.4 && < 0.5,
        jose >= 0.10 && < 0.12,
        crypton >= 0.34 && < 2.0,
        memory >= 0.18 && < 0.19,
        base64-bytestring >= 1.2 && < 1.3,
        time >= 1.12 && < 1.13,
        uuid >= 1.3 && < 1.4,
        data-default >= 0.7 && < 0.8,
        plow-log ^>= 0.1.6,
        generic-lens >= 2.2 && < 2.3,
        network-uri >= 2.6 && < 2.7,
        monad-time >= 0.4 && < 0.5,
        lucid >= 2.11 && < 2.12,
        servant-lucid >= 0.9 && < 0.10,
        QuickCheck >= 2.14 && < 2.16

    hs-source-dirs:   src
    default-language: GHC2021

test-suite servant-oauth2-idp-test
    import:           warnings
    ghc-options:      -threaded
    default-language: GHC2021
    type:             exitcode-stdio-1.0
    hs-source-dirs:   test
    main-is:          Main.hs
    other-modules:
        TestMonad
        Laws.OAuthStateStoreSpec
        Laws.AuthBackendSpec
        Laws.AuthBackendAssociatedTypesSpec
        Laws.AuthBackendSignatureSpec
        Laws.AuthCodeFunctorSpec
        Laws.ConsumeAuthCodeSpec
        Laws.OAuthUserTypeSpec
        Laws.BoundarySpec
        Laws.ErrorBoundarySecuritySpec
        Servant.OAuth2.IDP.APISpec
        Servant.OAuth2.IDP.BrandingSpec
        Servant.OAuth2.IDP.ConfigSpec
        Servant.OAuth2.IDP.TypesSpec
        Servant.OAuth2.IDP.ErrorsSpec
        Servant.OAuth2.IDP.PKCESpec
        Servant.OAuth2.IDP.MetadataSpec
        Servant.OAuth2.IDP.TraceSpec
        Servant.OAuth2.IDP.TokenRequestSpec
        Servant.OAuth2.IDP.LucidRenderingSpec
        Servant.OAuth2.IDP.CryptoEntropySpec
        Servant.OAuth2.IDP.BearerMethodSpec
        Servant.OAuth2.IDP.Handlers.MetadataSpec

    build-depends:
        base >= 4.18.2.1 && <= 4.21.0.0,
        servant-oauth2-idp,
        text >= 2.0 && <= 2.1.2,
        hspec >= 2.10 && < 2.12,
        QuickCheck >= 2.14 && < 2.16,
        time >= 1.12 && < 1.13,
        containers >= 0.6 && < 0.7,
        mtl >= 2.3 && < 2.4,
        network-uri >= 2.6 && < 2.7,
        memory >= 0.18 && < 0.19,
        monad-time >= 0.4 && < 0.5,
        stm >= 2.5 && < 2.6,
        aeson >= 2.1 && < 2.3,
        bytestring >= 0.11 && < 0.13,
        lucid >= 2.11 && < 2.12
```

### Step 1.4: Move source modules

```bash
# Create directory structure
mkdir -p vendor/servant-oauth2-idp/src/Servant/OAuth2/IDP/{Handlers,Store,Auth,Test}

# Move source files
mv src/Servant/OAuth2/IDP/*.hs vendor/servant-oauth2-idp/src/Servant/OAuth2/IDP/
mv src/Servant/OAuth2/IDP/Handlers/*.hs vendor/servant-oauth2-idp/src/Servant/OAuth2/IDP/Handlers/
mv src/Servant/OAuth2/IDP/Store/*.hs vendor/servant-oauth2-idp/src/Servant/OAuth2/IDP/Store/
mv src/Servant/OAuth2/IDP/Auth/*.hs vendor/servant-oauth2-idp/src/Servant/OAuth2/IDP/Auth/
mv src/Servant/OAuth2/IDP/Test/*.hs vendor/servant-oauth2-idp/src/Servant/OAuth2/IDP/Test/

# Remove empty directories
rmdir src/Servant/OAuth2/IDP/{Handlers,Store,Auth,Test}
rmdir src/Servant/OAuth2/IDP
rmdir src/Servant/OAuth2
rmdir src/Servant
```

### Step 1.5: Move test files

```bash
# Create test directory structure
mkdir -p vendor/servant-oauth2-idp/test/{Servant/OAuth2/IDP/Handlers,Laws}

# Move OAuth2 tests
mv test/Servant/OAuth2/IDP/*.hs vendor/servant-oauth2-idp/test/Servant/OAuth2/IDP/
mv test/Servant/OAuth2/IDP/Handlers/*.hs vendor/servant-oauth2-idp/test/Servant/OAuth2/IDP/Handlers/

# Move Laws tests (OAuth2 typeclass laws)
mv test/Laws/OAuthStateStoreSpec.hs vendor/servant-oauth2-idp/test/Laws/
mv test/Laws/AuthBackendSpec.hs vendor/servant-oauth2-idp/test/Laws/
mv test/Laws/AuthBackendAssociatedTypesSpec.hs vendor/servant-oauth2-idp/test/Laws/
mv test/Laws/AuthBackendSignatureSpec.hs vendor/servant-oauth2-idp/test/Laws/
mv test/Laws/AuthCodeFunctorSpec.hs vendor/servant-oauth2-idp/test/Laws/
mv test/Laws/ConsumeAuthCodeSpec.hs vendor/servant-oauth2-idp/test/Laws/
mv test/Laws/OAuthUserTypeSpec.hs vendor/servant-oauth2-idp/test/Laws/
mv test/Laws/BoundarySpec.hs vendor/servant-oauth2-idp/test/Laws/
mv test/Laws/ErrorBoundarySecuritySpec.hs vendor/servant-oauth2-idp/test/Laws/

# Clean up empty directories
rmdir test/Servant/OAuth2/IDP/Handlers
rmdir test/Servant/OAuth2/IDP
rmdir test/Servant/OAuth2
rmdir test/Servant
```

### Step 1.6: Create test/Main.hs for servant-oauth2-idp

```haskell
-- vendor/servant-oauth2-idp/test/Main.hs
module Main where

import Test.Hspec

-- Import all specs
import qualified Laws.OAuthStateStoreSpec
import qualified Laws.AuthBackendSpec
-- ... (all other spec imports)

main :: IO ()
main = hspec $ do
    describe "OAuthStateStore Laws" Laws.OAuthStateStoreSpec.spec
    describe "AuthBackend Laws" Laws.AuthBackendSpec.spec
    -- ... (all other specs)
```

### Step 1.7: Create TestMonad for servant-oauth2-idp

```haskell
-- vendor/servant-oauth2-idp/test/TestMonad.hs
-- Minimal test monad using AuthUser from Demo
-- (See data-model.md for full implementation)
```

### Step 1.8: Update mcp.cabal

```diff
 build-depends:
+    servant-oauth2-idp,
     ...

 exposed-modules:
-    Servant.OAuth2.IDP.API
-    Servant.OAuth2.IDP.Config
-    ... (remove all Servant.OAuth2.IDP.* modules)
```

### Step 1.9: Update mcp test/Main.hs

Remove imports and specs for moved test files.

### Step 1.10: Verify build

```bash
cabal build all
cabal test all
```

## Phase 2: Git History Extraction

### Step 2.1: Clone repository

```bash
git clone /home/claude/workspace ~/vendor/servant-oauth2-idp
```

### Step 2.2: Extract history

```bash
cd ~/vendor/servant-oauth2-idp

git filter-repo \
  --path src/Servant/OAuth2/ \
  --path test/Servant/OAuth2/ \
  --path test/Laws/OAuthStateStoreSpec.hs \
  --path test/Laws/AuthBackendSpec.hs \
  --path test/Laws/AuthBackendAssociatedTypesSpec.hs \
  --path test/Laws/AuthBackendSignatureSpec.hs \
  --path test/Laws/AuthCodeFunctorSpec.hs \
  --path test/Laws/ConsumeAuthCodeSpec.hs \
  --path test/Laws/OAuthUserTypeSpec.hs \
  --path test/Laws/BoundarySpec.hs \
  --path test/Laws/ErrorBoundarySecuritySpec.hs \
  --force
```

### Step 2.3: Copy package files

```bash
# Copy cabal file from vendor/
cp /home/claude/workspace/vendor/servant-oauth2-idp/servant-oauth2-idp.cabal ~/vendor/servant-oauth2-idp/

# Copy test infrastructure
cp -r /home/claude/workspace/vendor/servant-oauth2-idp/test/TestMonad.hs ~/vendor/servant-oauth2-idp/test/
cp -r /home/claude/workspace/vendor/servant-oauth2-idp/test/Main.hs ~/vendor/servant-oauth2-idp/test/
```

### Step 2.4: Verify history

```bash
git log --oneline | head -20
git log --follow src/Servant/OAuth2/IDP.hs | head -10
```

## Phase 3: Project Documentation

### Step 3.1: Copy .specify directory

```bash
cp -r /home/claude/workspace/.specify ~/vendor/servant-oauth2-idp/
```

### Step 3.2: Create CLAUDE.md

Create `~/vendor/servant-oauth2-idp/CLAUDE.md` with:

- Package purpose and architecture
- Build commands (`cabal build`, `cabal test`, `hlint .`)
- Module structure overview
- Typeclass documentation (OAuthStateStore, AuthBackend)
- Configuration types (OAuthEnv)
- Testing patterns

### Step 3.3: Create LICENSE

```bash
cp /home/claude/workspace/LICENSE ~/vendor/servant-oauth2-idp/
```

## Verification Checklist

- [ ] `cabal build all` succeeds in workspace root
- [ ] `cabal test all` succeeds (zero regressions)
- [ ] `hlint .` returns no warnings in vendor/servant-oauth2-idp
- [ ] Git history preserved in ~/vendor/servant-oauth2-idp
- [ ] CLAUDE.md exists and is comprehensive
- [ ] .specify directory present

## Common Issues

### Issue: Circular dependency detected

**Solution**: Ensure MCP.Server.HTTP only imports from servant-oauth2-idp, not the other way around.

### Issue: Missing module in test

**Solution**: Check that all test dependencies are listed in servant-oauth2-idp.cabal test-suite section.

### Issue: git filter-repo fails

**Solution**: Ensure the repository has no uncommitted changes. Use `--force` if needed.
