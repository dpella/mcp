# Feature Specification: Extract servant-oauth2-idp Package

**Feature Branch**: `006-servant-oauth2-idp-package`
**Created**: 2025-12-22
**Status**: Draft
**Input**: User description: "Split Servant namespace and tests into servant-oauth2-idp package leaving rest in mcp package. Create cabal.project with vendor/ reference for continuous testing. Extract with git history to ~/vendor/servant-oauth2-idp. Clone .specify constitution and create CLAUDE.md."

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Package Structure Setup (Priority: P1)

As a developer, I want the Servant.OAuth2.IDP namespace extracted into a vendor/servant-oauth2-idp directory with a proper cabal package structure so that I can develop and test both packages together during the transition period.

**Why this priority**: This is the foundation for all other work. Without the package structure, nothing else can be validated or tested.

**Independent Test**: Can be fully tested by running `cabal build all` from the workspace root and verifying both packages compile successfully.

**Acceptance Scenarios**:

1. **Given** the current mcp package with Servant.OAuth2.IDP modules, **When** I run `cabal build all`, **Then** both mcp and servant-oauth2-idp packages build successfully
2. **Given** the new package structure, **When** I examine vendor/servant-oauth2-idp/, **Then** I see a valid cabal package with proper module exports
3. **Given** the cabal.project file, **When** I check package locations, **Then** it references vendor/servant-oauth2-idp as a local package

---

### User Story 2 - Git History Extraction (Priority: P2)

As a developer, I want the servant-oauth2-idp package extracted with its full git history into ~/vendor/servant-oauth2-idp so that the commit history for OAuth2 functionality is preserved in the standalone repository.

**Why this priority**: History preservation is valuable for understanding code evolution and debugging, but the package can function without it during initial development.

**Independent Test**: Can be tested by running `git log --oneline src/Servant/OAuth2/` in the extracted repository and verifying commits match the original history.

**Acceptance Scenarios**:

1. **Given** the mcp repository with OAuth2 commits, **When** I examine ~/vendor/servant-oauth2-idp git history, **Then** commits touching Servant.OAuth2.IDP files are present
2. **Given** the extracted repository, **When** I run `git log --follow src/Servant/OAuth2/IDP.hs`, **Then** the file's full history is visible
3. **Given** both repositories exist, **When** I compare commit messages for OAuth2 files, **Then** they match between original and extracted repository

---

### User Story 3 - Project Documentation (Priority: P3)

As a developer, I want the servant-oauth2-idp package to have its own CLAUDE.md and .specify constitution so that AI assistants and speckit workflows can work with the package independently.

**Why this priority**: Documentation enables independent development but isn't blocking for the technical extraction.

**Independent Test**: Can be tested by opening the extracted package in a new Claude Code session and verifying CLAUDE.md is recognized.

**Acceptance Scenarios**:

1. **Given** the extracted package, **When** I check for CLAUDE.md, **Then** the file exists with relevant OAuth2/IDP package context
2. **Given** the extracted package, **When** I run speckit commands, **Then** the .specify directory contains the constitution and templates
3. **Given** the CLAUDE.md file, **When** I read its contents, **Then** it documents the package's purpose, build commands, and architecture

---

### Edge Cases

- What happens when the git filter-repo command encounters merge commits? (Merge commits are preserved as-is)
- How does the system handle files that moved between directories during history? (git filter-repo with path mapping handles renames)
- What if vendor/ directory already exists? (Fail with clear error rather than overwriting)
- What if ~/vendor/servant-oauth2-idp already exists? (Fail with clear error; user must remove or rename first)
- What if dependencies between mcp and servant-oauth2-idp create circular imports? (Build will fail; must be resolved before proceeding)

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST create a `vendor/` directory at workspace root containing the servant-oauth2-idp package
- **FR-002**: System MUST create a `cabal.project` file that references both the mcp package and vendor/servant-oauth2-idp
- **FR-003**: System MUST move all `src/Servant/OAuth2/IDP/**` modules from mcp to servant-oauth2-idp package
- **FR-004**: System MUST move all tests related to Servant.OAuth2.IDP modules to the new package, including `test/Servant/OAuth2/IDP/**` and `test/Laws/` tests for OAuth2 typeclasses (OAuthStateStore, AuthBackend, AuthCode laws)
- **FR-004a**: System MUST create a minimal TestMonad in servant-oauth2-idp with OAuthStateStore, AuthBackend, and MonadTime instances to run the polymorphic Laws specs (mcp retains its TestMonad for integration tests)
- **FR-005**: System MUST update mcp's cabal file to depend on servant-oauth2-idp instead of containing OAuth2 modules
- **FR-006**: System MUST NOT re-export OAuth2 types from MCP.Server.HTTP; internal imports from servant-oauth2-idp are used only for implementation, not public API
- **FR-007**: System MUST create a servant-oauth2-idp.cabal with version `0.1.0.0`, appropriate license, category, and dependencies
- **FR-008**: System MUST extract git history for Servant.OAuth2.IDP files into ~/vendor/servant-oauth2-idp using git filter-repo
- **FR-009**: System MUST copy the .specify directory (constitution and templates) to the extracted repository
- **FR-010**: System MUST create a CLAUDE.md file specific to the servant-oauth2-idp package
- **FR-011**: System MUST ensure `cabal build all` succeeds after the split
- **FR-012**: System MUST ensure `cabal test all` succeeds after the split (all existing tests pass)

### Key Entities

- **mcp package**: The original Haskell package that will retain MCP-specific functionality after extraction
- **servant-oauth2-idp package**: The new standalone package containing OAuth2 Identity Provider implementation for Servant
- **cabal.project**: Multi-package project file enabling local development of both packages together
- **vendor/ directory**: Temporary location for servant-oauth2-idp during development; referenced by cabal.project
- **~/vendor/servant-oauth2-idp**: Final extracted repository with preserved git history for independent publication

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Both packages build successfully with `cabal build all` in under 5 minutes on a standard development machine
- **SC-002**: All existing tests pass with `cabal test all` (zero regressions)
- **SC-003**: The servant-oauth2-idp package contains zero MCP dependencies (fully standalone OAuth2 implementation)
- **SC-004**: Git history in ~/vendor/servant-oauth2-idp shows 100% of commits that touched Servant.OAuth2.IDP files
- **SC-005**: CLAUDE.md file provides sufficient context for AI assistants to work with the package without referencing mcp documentation
- **SC-006**: The .specify directory in extracted repository enables all speckit workflows (/speckit.specify, /speckit.plan, /speckit.implement)

## Clarifications

### Session 2025-12-22

- Q: What initial version should servant-oauth2-idp use? → A: `0.1.0.0` (pre-release; API may change)
- Q: Should Laws tests for OAuth2 typeclasses move to servant-oauth2-idp? → A: Yes, move with the typeclasses they test
- Q: Should MCP.Server.HTTP re-export OAuth2 types for compatibility? → A: No re-exports; users import directly from servant-oauth2-idp (breaking change acceptable pre-release)
- Q: Test infrastructure strategy for servant-oauth2-idp? → A: Create minimal new TestMonad in servant-oauth2-idp; mcp keeps current TestMonad for integration tests

## Assumptions

- **A-001**: git-filter-repo is available or can be installed for history extraction
- **A-002**: The Servant.OAuth2.IDP namespace has zero MCP dependencies (as documented in CLAUDE.md "Core Invariant")
- **A-003**: User has write access to ~/vendor/ directory
- **A-004**: Current test suite adequately covers OAuth2 functionality to detect regressions
- **A-005**: The mcp executable and mcp-http executable will continue to depend on servant-oauth2-idp
