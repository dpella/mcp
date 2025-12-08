# Implementation Plan: Login Landing Page and User Authentication

**Branch**: `002-login-auth-page` | **Date**: 2025-12-08 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/002-login-auth-page/spec.md`

## Summary

Implement a login landing page and authentication endpoints for the mcp-http example application, replacing the current auto-approval OAuth flow with real user authentication. The login page will display as HTML, validate credentials against a configurable store (with demo credentials), and redirect authenticated users back to the OAuth client with authorization codes.

## Technical Context

**Language/Version**: Haskell GHC2021 (GHC 9.4+)
**Primary Dependencies**: servant-server 0.19-0.20, warp 3.3, aeson 2.1-2.2, cryptonite 0.30, jose 0.10-0.11
**Storage**: In-memory (TVar-based state management, consistent with existing OAuth state storage)
**Testing**: cabal test (test suite placeholder exists at test/Main.hs)
**Target Platform**: Linux server (HTTP transport)
**Project Type**: Single project (library with example executables)
**Performance Goals**: <1s page load (SC-005), <2s error response (SC-003), <30s complete login flow (SC-001)
**Constraints**: Demo/example application scope, minimal dependencies, embedded HTML templates
**Scale/Scope**: Demo application, single-user in-memory credential store

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

Reference: `.specify/memory/constitution.md`

| Principle | Status | Evidence/Notes |
|-----------|--------|----------------|
| I. Type-Driven Design | ✅ | LoginSession, UserCredential types designed first; smart constructors for validated credentials |
| II. Deep Module Architecture | ✅ | Login module exposes minimal API (handleLogin, renderLoginPage); internals hidden |
| III. Denotational Semantics | ✅ | Credential validation is pure predicate; session transitions documented |
| IV. Total Functions | ✅ | Authentication returns Either AuthError AuthSuccess; no partial functions |
| V. Pure Core, Impure Shell | ✅ | Credential validation pure; IO only at HTTP boundary for serving pages |
| VI. Property-Based Testing | ⚠️ | Unit tests planned; property tests deferred (simple validation logic) |

**Note**: Property-based testing partially applicable - this feature is primarily IO-bound (HTML rendering, form processing). Unit tests for credential validation and session management are prioritized.

## Project Structure

### Documentation (this feature)

```text
specs/002-login-auth-page/
├── plan.md              # This file
├── spec.md              # Feature specification
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output
│   └── login-api.md     # Login endpoint contracts
└── tasks.md             # Phase 2 output (created by /speckit.tasks)
```

### Source Code (repository root)

```text
src/
├── MCP/
│   ├── Types.hs           # Core MCP types
│   ├── Protocol.hs        # JSON-RPC protocol
│   ├── Server.hs          # MCPServer typeclass
│   └── Server/
│       ├── Auth.hs        # OAuth configuration, PKCE validation
│       ├── HTTP.hs        # HTTP transport (MODIFY: add login endpoints)
│       └── StdIO.hs       # StdIO transport (unchanged)

examples/
├── http-server.hs         # mcp-http executable (MODIFY: credential config)
└── ...

test/
└── Main.hs                # Test suite (ADD: login tests)
```

**Structure Decision**: Extend existing `MCP.Server.HTTP` module with login functionality. No new modules required - the login logic integrates with existing OAuth flow in HTTP.hs.

## Complexity Tracking

No constitution violations requiring justification. The design follows all principles:
- Types first (LoginSession, UserCredential)
- Deep modules (login logic encapsulated in HTTP.hs)
- Total functions (Either-based auth results)
- Pure validation with IO at boundaries
