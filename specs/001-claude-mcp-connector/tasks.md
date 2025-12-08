# Tasks: Claude.ai MCP Connector Compatibility

**Input**: Design documents from `/specs/001-claude-mcp-connector/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/

**Organization**: Tasks grouped by user story for independent implementation and testing.

**Architecture**: WWW-Authenticate headers are handled by a custom `ProtectedResourceAuth` combinator at the framework level. Apps don't manually handle 401 responses - the combinator does this automatically.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: User story this task belongs to (US1, US2, US3, US4)
- Paths relative to repository root

---

## Phase 1: Setup

**Purpose**: Prepare codebase for new functionality

- [ ] T001 Review existing MCP.Server.Auth module exports in src/MCP/Server/Auth.hs
- [ ] T002 Review existing MCP.Server.HTTP module structure in src/MCP/Server/HTTP.hs
- [ ] T003 [P] Review existing servant-auth integration pattern in src/MCP/Server/HTTP.hs

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core types and ProtectedResourceAuth combinator that ALL user stories depend on

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

### ProtectedResourceMetadata Type

- [ ] T004 Add ProtectedResourceMetadata data type to src/MCP/Server/Auth.hs per data-model.md
- [ ] T005 Add ToJSON instance for ProtectedResourceMetadata with RFC9728 field names in src/MCP/Server/Auth.hs
- [ ] T006 Add FromJSON instance for ProtectedResourceMetadata in src/MCP/Server/Auth.hs
- [ ] T007 Export ProtectedResourceMetadata from MCP.Server.Auth module in src/MCP/Server/Auth.hs

### ProtectedResourceAuth Combinator (Framework-level 401 handling)

- [ ] T008 Add ProtectedResourceAuth data type (type-level tag for auth scheme) to src/MCP/Server/Auth.hs
- [ ] T009 Add ProtectedResourceAuthConfig data type with resourceMetadataUrl field to src/MCP/Server/Auth.hs
- [ ] T010 Add AuthServerData type family instance for ProtectedResourceAuth in src/MCP/Server/Auth.hs
- [ ] T011 Add ThrowAll instance that returns 401 with WWW-Authenticate header in src/MCP/Server/Auth.hs
- [ ] T012 Export ProtectedResourceAuth and ProtectedResourceAuthConfig from MCP.Server.Auth module in src/MCP/Server/Auth.hs

### HTTPServerConfig Updates

- [ ] T013 Add httpProtectedResourceMetadata field to HTTPServerConfig in src/MCP/Server/HTTP.hs
- [ ] T014 Add defaultProtectedResourceMetadata helper to generate metadata from httpBaseUrl in src/MCP/Server/HTTP.hs

**Checkpoint**: Foundation ready - ProtectedResourceAuth combinator handles 401s automatically with WWW-Authenticate header

---

## Phase 3: User Story 1 - Add MCP Server as Claude Connector (Priority: P1) 🎯 MVP

**Goal**: Enable Claude.ai to discover and connect to MCP server via OAuth Protected Resource Metadata

**Independent Test**:
1. Start mcp-http with --oauth
2. GET /.well-known/oauth-protected-resource → returns valid RFC9728 JSON
3. POST /mcp without auth → returns 401 with WWW-Authenticate header (automatic via ProtectedResourceAuth combinator)

### Implementation for User Story 1

- [ ] T015 [US1] Add ProtectedResourceAPI type for /.well-known/oauth-protected-resource endpoint in src/MCP/Server/HTTP.hs
- [ ] T016 [US1] Update OAuthAPI type to include ProtectedResourceAPI in src/MCP/Server/HTTP.hs
- [ ] T017 [US1] Implement handleProtectedResourceMetadata handler in src/MCP/Server/HTTP.hs
- [ ] T018 [US1] Wire handleProtectedResourceMetadata into oauthServer in src/MCP/Server/HTTP.hs
- [ ] T019 [US1] Update MCPAPI type to use ProtectedResourceAuth combinator for protected endpoints in src/MCP/Server/HTTP.hs
- [ ] T020 [US1] Add ProtectedResourceAuthConfig to server context in runServerHTTP in src/MCP/Server/HTTP.hs

**Checkpoint**: Protected Resource Metadata endpoint works, 401s include WWW-Authenticate header (via combinator)

---

## Phase 4: User Story 2 - Dynamic Client Registration (Priority: P1)

**Goal**: Ensure DCR endpoint works correctly for Claude.ai client registration

**Independent Test**: POST /register with client metadata → returns client_id

**Note**: DCR already implemented. This phase verifies and documents existing functionality.

### Implementation for User Story 2

- [ ] T021 [US2] Verify /register endpoint returns correct RFC7591 response format in src/MCP/Server/HTTP.hs
- [ ] T022 [US2] Verify ClientRegistrationResponse includes all required fields in src/MCP/Server/HTTP.hs
- [ ] T023 [US2] Document DCR endpoint in examples/http-server.hs startup output

**Checkpoint**: DCR confirmed working per RFC7591

---

## Phase 5: User Story 3 - OAuth Authorization Flow with PKCE (Priority: P1)

**Goal**: Accept resource parameter in OAuth flow per RFC8707

**Independent Test**: Full PKCE flow with resource parameter succeeds

### Implementation for User Story 3

- [ ] T024 [US3] Add resource parameter to authorize endpoint signature in OAuthAPI type in src/MCP/Server/HTTP.hs
- [ ] T025 [US3] Update handleAuthorize function signature to accept Maybe Text for resource in src/MCP/Server/HTTP.hs
- [ ] T026 [US3] Log resource parameter value in handleAuthorize for debugging in src/MCP/Server/HTTP.hs
- [ ] T027 [US3] Accept resource parameter in token request (already form-encoded) in src/MCP/Server/HTTP.hs
- [ ] T028 [US3] Log resource parameter from token request for debugging in src/MCP/Server/HTTP.hs

**Checkpoint**: OAuth flow accepts resource parameter per RFC8707

---

## Phase 6: User Story 4 - Token Refresh (Priority: P2)

**Goal**: Verify token refresh works correctly

**Independent Test**: POST /token with refresh_token grant → returns new access_token

**Note**: Token refresh already implemented. This phase verifies existing functionality.

### Implementation for User Story 4

- [ ] T029 [US4] Verify handleRefreshTokenGrant returns correct TokenResponse in src/MCP/Server/HTTP.hs
- [ ] T030 [US4] Verify refresh_token rotation works correctly in src/MCP/Server/HTTP.hs

**Checkpoint**: Token refresh confirmed working

---

## Phase 7: Example Application Updates

**Goal**: Update mcp-http example with --base-url CLI option

### Implementation

- [ ] T031 Add --base-url CLI option to Options parser in examples/http-server.hs
- [ ] T032 Update HTTPServerConfig construction to use provided base URL in examples/http-server.hs
- [ ] T033 Update startup output to show Protected Resource Metadata endpoint URL in examples/http-server.hs
- [ ] T034 Update OAuth demo instructions in startup output in examples/http-server.hs

**Checkpoint**: mcp-http example accepts --base-url and shows correct OAuth discovery URLs

---

## Phase 8: Polish & Cross-Cutting Concerns

**Purpose**: Documentation, testing, and final validation

- [ ] T035 [P] Update CLAUDE.md with ProtectedResourceAuth combinator and Protected Resource Metadata documentation
- [ ] T036 [P] Update examples/oauth-client-demo.sh to test protected resource metadata endpoint
- [ ] T037 Run cabal build and fix any compilation errors
- [ ] T038 Run cabal test and verify existing tests pass
- [ ] T039 Manual test: verify /.well-known/oauth-protected-resource returns valid JSON
- [ ] T040 Manual test: verify POST /mcp without auth returns 401 with WWW-Authenticate header
- [ ] T041 Manual test: complete full OAuth flow using curl commands from quickstart.md
- [ ] T042 Validate implementation against specs/001-claude-mcp-connector/quickstart.md

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies
- **Foundational (Phase 2)**: Depends on Setup - BLOCKS all user stories
  - ProtectedResourceAuth combinator must be complete before US1 can use it
- **User Stories (Phase 3-6)**: All depend on Foundational phase
  - US1 (Discovery): Core MVP - must complete first
  - US2 (DCR): Verification only - can parallel with US1
  - US3 (PKCE + resource): Depends on US1 for testing
  - US4 (Refresh): Verification only - can parallel
- **Example Updates (Phase 7)**: Depends on US1 completion
- **Polish (Phase 8)**: Depends on all implementation phases

### User Story Dependencies

- **User Story 1 (P1)**: Core discovery - NO dependencies on other stories
- **User Story 2 (P1)**: DCR verification - Can run parallel with US1
- **User Story 3 (P1)**: Extends US1 - Depends on US1 for end-to-end testing
- **User Story 4 (P2)**: Refresh verification - Can run parallel with US2, US3

### Parallel Opportunities

Within Foundational phase:
```bash
# ProtectedResourceMetadata tasks (T004-T007) can run parallel with setup
# ProtectedResourceAuth tasks (T008-T012) depend on understanding servant-auth patterns
```

Within User Story 1:
```bash
# T015-T018: Protected resource endpoint tasks (sequential)
# T019-T020: ProtectedResourceAuth integration (depends on T015-T018)
```

Cross-story parallelism:
```bash
# After Foundational:
US2 tasks can run parallel with US1 tasks
US4 tasks can run parallel with US3 tasks
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup (T001-T003)
2. Complete Phase 2: Foundational (T004-T014)
3. Complete Phase 3: User Story 1 (T015-T020)
4. **STOP and VALIDATE**: Test protected resource metadata endpoint + 401 with WWW-Authenticate
5. Quick win: Claude.ai can now discover the MCP server

### Incremental Delivery

1. Setup + Foundational → ProtectedResourceAuth combinator ready
2. Add US1 (Discovery) → Core Claude.ai compatibility ✓
3. Add US2 (DCR) → Verify client registration ✓
4. Add US3 (Resource param) → Full RFC8707 compliance ✓
5. Add US4 (Refresh) → Long-running sessions ✓
6. Example updates → Developer experience
7. Polish → Production ready

---

## Notes

- [P] tasks = different files or independent within same file
- [Story] label maps task to specific user story
- **Key architectural change**: ProtectedResourceAuth combinator handles 401 + WWW-Authenticate at framework level
  - Apps don't manually handle 401 responses
  - Cleaner separation of concerns
  - Ready for extraction as servant-auth-mcp package
- Most changes are in two files: src/MCP/Server/Auth.hs and src/MCP/Server/HTTP.hs
- Commit after each phase completion
- Stop at any checkpoint to validate independently
- US2 and US4 are primarily verification - existing code may already work
