# Implementation Plan: Claude.ai MCP Connector Compatibility

**Branch**: `001-claude-mcp-connector` | **Date**: 2025-12-08 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/001-claude-mcp-connector/spec.md`

## Summary

Add RFC9728 Protected Resource Metadata endpoint and a custom Auth combinator to the MCP HTTP server library, enabling Claude.ai to discover and authenticate with self-hosted MCP servers via the standard OAuth 2.1 authorization flow with Dynamic Client Registration.

**Key additions:**
1. `ProtectedResourceMetadata` type in `MCP.Server.Auth`
2. `ProtectedResourceAuth` custom Auth combinator that automatically handles 401 responses with WWW-Authenticate header at the framework level (apps don't manually handle this)
3. `/.well-known/oauth-protected-resource` endpoint in `MCP.Server.HTTP`
4. `resource` parameter support in OAuth endpoints (RFC8707)

**Architecture decision:** The WWW-Authenticate header is handled by a custom Servant Auth combinator, not by application code. This means:
- The framework returns proper 401 responses automatically
- Apps using the library get correct behavior without manual error handling
- The combinator is designed for future extraction as `servant-auth-mcp` package

## Technical Context

**Language/Version**: Haskell GHC2021 (GHC 9.4+)
**Primary Dependencies**: servant-server 0.19-0.20, servant-auth-server 0.4, warp 3.3, jose 0.10-0.11, aeson 2.1-2.2
**Storage**: In-memory (TVar-based state for OAuth codes, tokens, clients)
**Testing**: cabal test (HUnit/Hspec - test suite exists but minimal)
**Target Platform**: Linux server (behind TLS-terminating reverse proxy)
**Project Type**: Single Haskell library with example executables
**Performance Goals**: N/A for library (demo-quality OAuth implementation)
**Constraints**: Must integrate cleanly with existing servant-auth, maintain backward compatibility
**Scale/Scope**: Library extension (~200-400 lines new code across 2 modules)

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

Reference: `.specify/memory/constitution.md`

| Principle | Status | Evidence/Notes |
|-----------|--------|----------------|
| I. Type-Driven Design | ✅ | New `ProtectedResourceMetadata` type with explicit fields; illegal states avoided via record structure matching RFC9728 |
| II. Deep Module Architecture | ✅ | Types in `MCP.Server.Auth`, logic in `MCP.Server.HTTP`; minimal public interface additions |
| III. Denotational Semantics | ⚠️ | JSON serialization has implicit spec (RFC9728); round-trip property test needed |
| IV. Total Functions | ✅ | Servant handlers use `Handler` monad with explicit error returns; no partial functions planned |
| V. Pure Core, Impure Shell | ⚠️ | OAuth state mutation in STM TVars at HTTP handler level; business logic (PKCE validation) already pure |
| VI. Property-Based Testing | ⚠️ | Property tests not currently in project; add JSON round-trip test for new metadata type |

**Notes:**
- Principle III, V, VI marked ⚠️ indicate "acceptable for MVP" - the OAuth state handling follows existing patterns in codebase
- Property-based testing infrastructure can be added as follow-up; golden tests for API responses are higher priority

## Project Structure

### Documentation (this feature)

```text
specs/001-claude-mcp-connector/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output (OpenAPI specs)
└── tasks.md             # Phase 2 output (/speckit.tasks)
```

### Source Code (repository root)

```text
src/
├── MCP/
│   ├── Types.hs           # Core MCP protocol types
│   ├── Protocol.hs        # JSON-RPC message types
│   ├── Server.hs          # MCPServer typeclass and state
│   └── Server/
│       ├── StdIO.hs       # StdIO transport
│       ├── HTTP.hs        # HTTP transport (MODIFY: add protected resource endpoint, use ProtectedResourceAuth combinator)
│       └── Auth.hs        # OAuth types (MODIFY: add ProtectedResourceMetadata, ProtectedResourceAuth combinator)

app/
└── Main.hs                # StdIO example (unchanged)

examples/
├── http-server.hs         # HTTP example (MODIFY: add --base-url CLI option, use ProtectedResourceAuth)
└── oauth-client-demo.sh   # OAuth demo script (may need updates)

test/
└── Main.hs                # Test suite (ADD: JSON round-trip tests)
```

**Structure Decision**: Single Haskell library project. Changes confined to:
- `src/MCP/Server/Auth.hs` - new ProtectedResourceMetadata type + ProtectedResourceAuth combinator
- `src/MCP/Server/HTTP.hs` - new endpoint, integrate ProtectedResourceAuth combinator
- `examples/http-server.hs` - CLI option, simplified auth handling (combinator does the work)

## Complexity Tracking

> No constitution violations requiring justification. All changes follow existing patterns.

| Violation | Why Needed | Simpler Alternative Rejected Because |
|-----------|------------|-------------------------------------|
| (none) | - | - |
