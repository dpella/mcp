# Tasks: Login Landing Page and User Authentication

**Input**: Design documents from `/specs/002-login-auth-page/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/

**Tests**: Unit tests included for credential validation (security-critical)

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

**Tracking**: All tasks are tracked using `bd` (beads) issue tracker. See [Issue Tracking](#issue-tracking-with-beads) section.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Project initialization - no code changes, preparation only

- [ ] T001 Review existing OAuth flow in `src/MCP/Server/HTTP.hs` and identify modification points
- [ ] T002 [P] Review existing `OAuthConfig` in `src/MCP/Server/Auth.hs` for extension points

**Checkpoint**: Codebase understood, ready to implement types

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core types and infrastructure that MUST be complete before ANY user story can be implemented

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

### Types (Constitution Principle I: Type-Driven Design)

- [ ] T003 Add `HTML` content type with `Accept` and `MimeRender` instances in `src/MCP/Server/HTTP.hs`
- [ ] T004 [P] Add `CredentialStore` and `HashedPassword` types in `src/MCP/Server/Auth.hs`
- [ ] T005 [P] Add `PendingAuthorization` data type in `src/MCP/Server/HTTP.hs`
- [ ] T006 [P] Add `LoginForm` data type with `FromForm` instance in `src/MCP/Server/HTTP.hs`
- [ ] T007 [P] Add `LoginError` sum type in `src/MCP/Server/HTTP.hs`
- [ ] T008 [P] Add `LoginResult` data type in `src/MCP/Server/HTTP.hs`

### State Extensions

- [ ] T009 Extend `OAuthState` with `pendingAuthorizations :: Map Text PendingAuthorization` in `src/MCP/Server/HTTP.hs`
- [ ] T010 Extend `OAuthConfig` with `credentialStore` and `loginSessionExpirySeconds` fields in `src/MCP/Server/Auth.hs`

### Pure Functions (Constitution Principle V: Pure Core)

- [ ] T011 Implement `mkHashedPassword` smart constructor using SHA256 in `src/MCP/Server/Auth.hs`
- [ ] T012 [P] Implement `validateCredential` pure function with constant-time comparison in `src/MCP/Server/Auth.hs`
- [ ] T013 [P] Implement `defaultDemoCredentialStore` with demo/demo123 and admin/admin456 in `src/MCP/Server/Auth.hs`

### HTML Templates (Pure Rendering)

- [ ] T014 [P] Implement `renderLoginPage` template function in `src/MCP/Server/HTTP.hs`
- [ ] T015 [P] Implement `renderErrorPage` template function in `src/MCP/Server/HTTP.hs`

### Tests for Foundational Types

- [ ] T016 [P] Add unit test for `validateCredential` (valid credentials) in `test/Main.hs`
- [ ] T017 [P] Add unit test for `validateCredential` (invalid credentials) in `test/Main.hs`
- [ ] T018 [P] Add unit test for `mkHashedPassword` consistency in `test/Main.hs`

**Checkpoint**: All types defined, pure functions tested, ready for endpoint implementation

---

## Phase 3: User Story 1 - User Authenticates with Credentials (Priority: P1) 🎯 MVP

**Goal**: User can enter username/password on login page and receive authorization code on successful auth

**Independent Test**: Access `/authorize` endpoint → see login form → submit demo/demo123 → get redirected with code

**Spec Reference**: [spec.md - User Story 1](./spec.md#user-story-1---user-authenticates-with-credentials-priority-p1)

### Implementation for User Story 1

- [ ] T019 [US1] Modify `handleAuthorize` to create `PendingAuthorization` and render login page in `src/MCP/Server/HTTP.hs`
- [ ] T020 [US1] Add `LoginAPI` type definition to Servant API in `src/MCP/Server/HTTP.hs`
- [ ] T021 [US1] Implement `handleLogin` endpoint for POST /login in `src/MCP/Server/HTTP.hs`
- [ ] T022 [US1] Wire up session cookie handling (Set-Cookie header) in `src/MCP/Server/HTTP.hs`
- [ ] T023 [US1] Implement auth code generation on successful login (reuse existing pattern) in `src/MCP/Server/HTTP.hs`
- [ ] T024 [US1] Implement redirect to client with code and state in `src/MCP/Server/HTTP.hs`
- [ ] T025 [US1] Handle invalid credentials - re-render login page with error in `src/MCP/Server/HTTP.hs`
- [ ] T026 [US1] Handle empty form fields - render validation errors in `src/MCP/Server/HTTP.hs`
- [ ] T027 [US1] Update `defaultDemoOAuthConfig` to include credential store in `src/MCP/Server/HTTP.hs`
- [ ] T028 [US1] Remove `autoApproveAuth` logic from authorize flow in `src/MCP/Server/HTTP.hs`

**Checkpoint**: User Story 1 complete - full login flow works with demo credentials

---

## Phase 4: User Story 2 - User Views Login Context (Priority: P2)

**Goal**: Login page displays requesting application name and requested permissions

**Independent Test**: Register client with name → authorize → see client name and scopes on login page

**Spec Reference**: [spec.md - User Story 2](./spec.md#user-story-2---user-views-login-context-priority-p2)

### Implementation for User Story 2

- [ ] T029 [US2] Extend `renderLoginPage` to accept and display client name in `src/MCP/Server/HTTP.hs`
- [ ] T030 [US2] Look up client name from `registeredClients` in `handleAuthorize` in `src/MCP/Server/HTTP.hs`
- [ ] T031 [US2] Extend `renderLoginPage` to display human-readable scope descriptions in `src/MCP/Server/HTTP.hs`
- [ ] T032 [US2] Add scope-to-description mapping function in `src/MCP/Server/HTTP.hs`

**Checkpoint**: User Story 2 complete - login page shows context information

---

## Phase 5: User Story 3 - User Denies Authorization (Priority: P3)

**Goal**: User can click "Deny" button to refuse authorization and redirect with error

**Independent Test**: Access login page → click Deny → get redirected with access_denied error

**Spec Reference**: [spec.md - User Story 3](./spec.md#user-story-3---user-denies-authorization-priority-p3)

### Implementation for User Story 3

- [ ] T033 [US3] Handle `action=deny` in `handleLogin` endpoint in `src/MCP/Server/HTTP.hs`
- [ ] T034 [US3] Generate OAuth `access_denied` error response in `src/MCP/Server/HTTP.hs`
- [ ] T035 [US3] Redirect with error and preserved state parameter in `src/MCP/Server/HTTP.hs`
- [ ] T036 [US3] Clean up `PendingAuthorization` on deny in `src/MCP/Server/HTTP.hs`

**Checkpoint**: User Story 3 complete - deny flow works correctly

---

## Phase 6: Edge Cases & Error Handling

**Purpose**: Handle error scenarios from spec edge cases

**Spec Reference**: [spec.md - Edge Cases](./spec.md#edge-cases)

- [ ] T037 Handle invalid/missing OAuth parameters - render error page in `src/MCP/Server/HTTP.hs`
- [ ] T038 [P] Handle expired sessions - check `pendingCreatedAt` vs `loginSessionExpirySeconds` in `src/MCP/Server/HTTP.hs`
- [ ] T039 [P] Handle cookies disabled - detect missing session cookie, render error in `src/MCP/Server/HTTP.hs`
- [ ] T040 Handle unregistered client_id - render appropriate error in `src/MCP/Server/HTTP.hs`
- [ ] T041 Handle invalid redirect_uri - render error (don't redirect) in `src/MCP/Server/HTTP.hs`

**Checkpoint**: All edge cases handled gracefully

---

## Phase 7: Polish & Cross-Cutting Concerns

**Purpose**: Final cleanup and validation

- [ ] T042 [P] Update `examples/http-server.hs` to log demo credentials on startup
- [ ] T043 Verify `cabal build` succeeds without warnings
- [ ] T044 Run `cabal test` and ensure all tests pass
- [ ] T045 [P] Run manual test using `examples/oauth-client-demo.sh` (update if needed)
- [ ] T046 Update CLAUDE.md if any new patterns introduced

**Checkpoint**: Feature complete, tested, documented

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - BLOCKS all user stories
- **User Stories (Phase 3-5)**: All depend on Foundational phase completion
  - US1 (P1) must complete before US2/US3 (they extend the same endpoint)
- **Edge Cases (Phase 6)**: Depends on User Story 1 (core flow must exist)
- **Polish (Phase 7)**: Depends on all user stories complete

### User Story Dependencies

- **User Story 1 (P1)**: Can start after Foundational (Phase 2) - No dependencies
- **User Story 2 (P2)**: Depends on US1 (extends `renderLoginPage` from US1)
- **User Story 3 (P3)**: Depends on US1 (extends `handleLogin` from US1)

### Within Each User Story

- Types before functions (Constitution Principle I)
- Pure functions before IO handlers (Constitution Principle V)
- Core implementation before edge cases

### Parallel Opportunities

- T002 can run parallel with T001 (different files)
- T004, T005, T006, T007, T008 can all run in parallel (different types, same file but additive)
- T012, T013 can run in parallel (independent pure functions)
- T014, T015 can run in parallel (independent template functions)
- T016, T017, T018 can run in parallel (independent tests)
- Edge case tasks T038, T039 can run in parallel (independent error handlers)

---

## Parallel Example: Foundational Types

```bash
# Launch all type definitions together (additive to same file):
Task: "Add CredentialStore and HashedPassword types in src/MCP/Server/Auth.hs"
Task: "Add PendingAuthorization data type in src/MCP/Server/HTTP.hs"
Task: "Add LoginForm data type with FromForm instance in src/MCP/Server/HTTP.hs"
Task: "Add LoginError sum type in src/MCP/Server/HTTP.hs"
Task: "Add LoginResult data type in src/MCP/Server/HTTP.hs"

# Launch all tests together:
Task: "Add unit test for validateCredential (valid credentials) in test/Main.hs"
Task: "Add unit test for validateCredential (invalid credentials) in test/Main.hs"
Task: "Add unit test for mkHashedPassword consistency in test/Main.hs"
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational (CRITICAL - blocks all stories)
3. Complete Phase 3: User Story 1
4. **STOP and VALIDATE**: Test login flow with demo/demo123
5. Deploy/demo if ready

### Incremental Delivery

1. Complete Setup + Foundational → Foundation ready
2. Add User Story 1 → Test with curl/browser → Demo (MVP!)
3. Add User Story 2 → Client info visible → Demo
4. Add User Story 3 → Deny works → Demo
5. Add Edge Cases → Robust error handling
6. Polish → Production-ready

---

## Issue Tracking with Beads

All tasks are tracked using `bd` (beads) issue tracker. The feature is organized as an epic with child tasks.

### Epic

- **Epic ID**: `002-login-auth-page`
- **Title**: Login Landing Page and User Authentication
- **Type**: feature
- **Priority**: 1 (high)

### Viewing Tasks

```bash
# Check ready work
bd ready --json

# View all tasks for this feature
bd list --json | jq '.[] | select(.title | contains("002-login"))'
```

### Workflow

1. **Start task**: `bd update <id> --status in_progress`
2. **Complete task**: `bd close <id> --reason "Done"`
3. **Found issue?**: `bd create "Bug: ..." -t bug --deps <parent-id>`

### Commit Protocol

Always commit `.beads/issues.jsonl` with code changes:
```bash
git add .beads/issues.jsonl src/MCP/Server/HTTP.hs
git commit -m "T019: Modify handleAuthorize for login page"
```

---

## Notes

- [P] tasks = different files or additive changes, no conflicts
- [Story] label maps task to specific user story for traceability
- All HTTP.hs modifications are in same file but sequential
- Run `cabal build` after each phase to catch type errors early
- Commit after each task or logical group
- Stop at any checkpoint to validate independently
