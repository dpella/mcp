# Implementation Plan: OAuth State and Authentication Typeclasses

**Branch**: `004-oauth-auth-typeclasses` | **Date**: 2025-12-11 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/004-oauth-auth-typeclasses/spec.md`

**Note**: This template is filled in by the `/speckit.plan` command. See `.specify/templates/commands/plan.md` for the execution workflow.

## Summary

Design two typeclasses (`OAuthStateStore` for OAuth 2.1 state persistence, `AuthBackend` for user credential validation) using three-layer cake architecture with polymorphic `m` monad parameter. Each typeclass defines associated error, environment, user, and user ID types. `OAuthStateStore` requires `MonadTime m` for expiry filtering. `AuthBackend.validateCredentials` returns `Maybe (UserId, User)` tuple. Handlers requiring both typeclasses use type equality constraints (`AuthBackendUser m ~ OAuthUser m`, `AuthBackendUserId m ~ OAuthUserId m`). Provide reference in-memory (`TVar`) and hard-coded credential implementations. Use `generic-lens` (`AsType`/`HasType`) for composable error/environment handling, and `hoistServer` for Servant boundary translation. **(Updated 2025-12-15: Added user type polymorphism per spec refinement; reference implementation types colocated with implementations per FR-042; error architecture refined with four domain error types and domainErrorToServerError boundary function per Phase 10; mcpApp entry point accepts natural transformation per Phase 11/FR-046)** **(Updated 2025-12-16: OAuth modules relocated to `Servant.OAuth2.IDP.*` namespace per FR-049; two entry points `mcpApp`/`mcpAppWithOAuth` in `MCP.Server.HTTP` per FR-048; type-level route composition per FR-047)** **(Updated 2025-12-17: ALL `mcpApp*` functions MUST accept natural transformation parameter per FR-048 refinement; see Phase 13)** **(Updated 2025-12-17: Removed `AuthBackendUserId m` associated type per spec refinement; `validateCredentials` now returns `Maybe (AuthBackendUser m)` only; see Phase 14)** **(Updated 2025-12-18: Security hardening per FR-050–FR-057; crypton required, SSRF prevention, CSPRNG for tokens; see Phase 15)**

## Technical Context

**Language/Version**: Haskell GHC2021 (GHC 9.4+ via base ^>=4.18.2.1)
**Primary Dependencies**: servant-server 0.19-0.20, servant-auth-server 0.4, jose 0.10-0.11, crypton (replacing deprecated cryptonite per FR-057), stm 2.5, mtl 2.3, aeson 2.1-2.2, generic-lens (to add), network-uri (to add)
**Storage**: In-memory (TVar-based) for reference implementation (not "default"); typeclass enables PostgreSQL/Redis backends
**Testing**: hspec + QuickCheck + hspec-quickcheck (property tests via `prop` combinator, polymorphic specs testing interface not implementation)
**Target Platform**: Linux server (GHC 9.4+)
**Project Type**: Single Haskell library with examples
**Performance Goals**: No measurable latency overhead (<5%) vs current TVar implementation
**Constraints**: Must maintain backward compatibility with existing OAuth flows
**Scale/Scope**: Single-instance semantics; distributed coordination delegated to backend implementations
**Type Safety**: Strong newtypes for all identifiers (AuthCodeId, ClientId, etc.), ADTs for enumerations (CodeChallengeMethod, GrantType), MonadTime for testable time-dependent operations, associated user/userId types with equality constraints for type-safe user handling

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

Reference: `.specify/memory/constitution.md`

| Principle | Status | Evidence/Notes |
|-----------|--------|----------------|
| I. Type-Driven Design | ✅ | Typeclasses with associated types (`OAuthStateError m`, `OAuthStateEnv m`, `OAuthUser m`, `AuthBackendError m`, `AuthBackendEnv m`, `AuthBackendUser m`) encode implementation-specific concerns at the type level. Type equality constraint (`AuthBackendUser m ~ OAuthUser m`) ensures compile-time agreement. Smart constructors preserved for `HashedPassword`. Illegal states prevented by type constraints. **(Updated 2025-12-17: Removed `OAuthUserId m` and `AuthBackendUserId m` per Phase 14 — user IDs are fields within user types, encoded into JWT via `ToJWT`)** |
| II. Deep Module Architecture | ✅ | Minimal public interface (typeclass methods only). Implementation details hidden in instance modules (`InMemory.hs`, `Demo.hs`). Complexity pulled into implementations, not pushed to callers. |
| III. Denotational Semantics | ✅ | FR-016 requires documented algebraic laws (round-trip, delete, idempotence, overwrite). Property-based tests will verify these laws. |
| IV. Total Functions | ✅ | All operations return results in monadic context (`m (Maybe a)`, `m (Either e a)`). No partial functions. Errors encoded via associated error types and `MonadError`. |
| V. Pure Core, Impure Shell | ✅ | Typeclasses abstract over effects (`MonadIO` in instance context, not class definition). Handler logic remains pure over the abstract interface; IO lives in implementations. |
| VI. Property-Based Testing | ✅ | FR-016 explicitly requires property tests for algebraic laws. Golden tests for API contracts preserved. |

**Gate Status**: PASS - All principles satisfied. Proceed to Phase 0.

## Project Structure

### Documentation (this feature)

```text
specs/004-oauth-auth-typeclasses/
├── plan.md              # This file (/speckit.plan command output)
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output (typeclass interfaces)
└── tasks.md             # Phase 2 output (/speckit.tasks command)
```

### Source Code (repository root)

```text
src/
├── MCP/
│   ├── Types.hs              # Core MCP protocol types (unchanged)
│   ├── Protocol.hs           # JSON-RPC message types (unchanged)
│   ├── Server.hs             # MCPServer typeclass (unchanged)
│   ├── Server/
│   │   ├── HTTP.hs           # Modified: use typeclass constraints
│   │   ├── StdIO.hs          # Unchanged (no OAuth)
│   │   ├── Auth.hs           # Modified: re-exports from Auth/*
│   │   ├── Auth/
│   │   │   ├── Backend.hs    # NEW: AuthBackend typeclass (AuthBackendUser m) + Username, PlaintextPassword newtypes (Phase 14: removed AuthBackendUserId)
│   │   │   └── Demo.hs       # NEW: Demo credential implementation + AuthUser, UserId, DemoAuthError (Phase 9: colocated)
│   │   ├── OAuth/
│   │   │   ├── Store.hs      # NEW: OAuthStateStore typeclass (OAuthUser m) + MonadTime constraint (Phase 14: removed OAuthUserId)
│   │   │   ├── Types.hs      # NEW: Newtypes (AuthCodeId, ClientId, etc.), ADTs, `AuthorizationCode userId`, AuthorizationError, ValidationError (Phase 10)
│   │   │   ├── Boundary.hs   # NEW: domainErrorToServerError, OAuthBoundaryTrace (Phase 10)
│   │   │   └── InMemory.hs   # NEW: TVar-based implementation + OAuthStoreError, OAuthTVarEnv (Phase 9: colocated)
│   │   └── Time.hs           # NEW: MonadTime instances (IO, test monads)
│   └── Trace/                # Unchanged

test/
├── Main.hs                   # Modified: import and run law specs
├── Laws/
│   ├── OAuthStateStoreSpec.hs  # NEW: Polymorphic typeclass law tests (uses prop)
│   └── AuthBackendSpec.hs      # NEW: Polymorphic typeclass law tests (uses prop)
├── Generators.hs             # NEW: Arbitrary instances for all newtypes and domain types
├── TestMonad.hs              # NEW: Test monad with controlled MonadTime
└── Trace/                    # Unchanged
```

**Structure Decision**: Single project structure. New modules added under `src/MCP/Server/Auth/` and `src/MCP/Server/OAuth/` for typeclass definitions and implementations. Reference implementation concrete types (`AuthUser`, `UserId`, errors) colocated with their implementations per Phase 9 (FR-042). Test modules added under `test/Laws/` for property-based law verification using polymorphic specs with `prop` combinator. `Arbitrary` instances in `test/Generators.hs`. **(Phase 12 Refinement**: OAuth modules will be relocated to `Servant.OAuth2.IDP.*` namespace per FR-049; see Phase 12 for updated module structure.)

## Complexity Tracking

> No Constitution Check violations requiring justification.

| Violation | Why Needed | Simpler Alternative Rejected Because |
|-----------|------------|-------------------------------------|
| N/A | - | - |

## Post-Design Constitution Re-Check

*GATE: Verified after Phase 1 design completion.*

| Principle | Status | Post-Design Evidence |
|-----------|--------|----------------------|
| I. Type-Driven Design | ✅ | `contracts/OAuthStateStore.hs` and `contracts/AuthBackend.hs` define typeclasses with associated types including user/userId. Type equality constraints ensure coherence. Types designed before implementation (spec → plan → contracts). |
| II. Deep Module Architecture | ✅ | Module structure in plan shows minimal exports (`OAuthStateStore(..)`, `AuthBackend(..)`). Implementation modules (`InMemory.hs`, `Demo.hs`) hide complexity. |
| III. Denotational Semantics | ✅ | Algebraic laws documented in `contracts/OAuthStateStore.hs` haddock (round-trip, delete, idempotence, overwrite). Test structure planned in `test/Laws/`. |
| IV. Total Functions | ✅ | All typeclass methods return `m ()` or `m (Maybe a)`. No partial functions. Error handling via `MonadError e m` with `AsType` prism constraints. |
| V. Pure Core, Impure Shell | ✅ | Typeclass definitions have no `MonadIO` constraint; implementations add it in instance context. Handler logic is pure over the abstract interface. |
| VI. Property-Based Testing | ✅ | `data-model.md` specifies laws. `quickstart.md` shows property test examples. Test modules planned: `OAuthStateStoreSpec.hs`, `AuthBackendSpec.hs`. |

**Gate Status**: PASS - All principles verified against design artifacts.

## Generated Artifacts

| Artifact | Path | Purpose |
|----------|------|---------|
| Plan | `specs/004-oauth-auth-typeclasses/plan.md` | This file |
| Research | `specs/004-oauth-auth-typeclasses/research.md` | Technical decisions and rationale |
| Data Model | `specs/004-oauth-auth-typeclasses/data-model.md` | Entity definitions and relationships |
| OAuthStateStore Contract | `specs/004-oauth-auth-typeclasses/contracts/OAuthStateStore.hs` | Typeclass interface specification |
| AuthBackend Contract | `specs/004-oauth-auth-typeclasses/contracts/AuthBackend.hs` | Typeclass interface specification |
| Quickstart | `specs/004-oauth-auth-typeclasses/quickstart.md` | Usage examples and integration guide |
| TestConfig Contract | `specs/004-oauth-auth-typeclasses/contracts/TestConfig.hs` | Conformance test interface (Phase 5) |
| OAuthServer Contract | `specs/004-oauth-auth-typeclasses/contracts/OAuthServer.hs` | Polymorphic server interface (Phase 6) |
| Type Witness Patterns | `specs/004-oauth-auth-typeclasses/plan.md#phase-7` | FR-038 type-directed polymorphism constraint (Phase 7) |
| User Type Polymorphism | `specs/004-oauth-auth-typeclasses/plan.md#phase-8` | FR-039 through FR-041 user/userId associated types (Phase 8) |
| Type Colocation | `specs/004-oauth-auth-typeclasses/plan.md#phase-9` | FR-042 reference implementation types in implementation modules (Phase 9) |
| Error Architecture | `specs/004-oauth-auth-typeclasses/plan.md#phase-10` | FR-043 through FR-045 domain error types and boundary translation (Phase 10) |
| Namespace Migration | `specs/004-oauth-auth-typeclasses/plan.md#phase-12` | FR-047 through FR-049 module relocation and entry point separation (Phase 12) |
| Natural Transformation Pattern | `specs/004-oauth-auth-typeclasses/plan.md#phase-13` | FR-048 refined: ALL mcpApp* functions accept natural transformation (Phase 13) |
| UserId Removal | `specs/004-oauth-auth-typeclasses/plan.md#phase-14` | Remove `AuthBackendUserId m` and `OAuthUserId m`; simplify `validateCredentials` (Phase 14) |

---

## Phase 3: Type Unification (mcp-51r)

**Added**: 2025-12-11 | **Blocker**: mcp-51r | **Status**: Planning

### Problem Statement

During implementation, a blocker was discovered (mcp-51r): two incompatible OAuth state type systems exist:

1. **HTTP.hs (legacy)**: Uses `Text` for redirect URIs, grant types, client IDs, etc.
   - `clientRedirectUris :: [Text]`
   - `clientGrantTypes :: [Text]`
   - Simple `Text`-based identifiers throughout

2. **OAuth.Types (new)**: Uses type-safe newtypes with invariants
   - `clientRedirectUris :: NonEmpty RedirectUri`
   - `clientGrantTypes :: Set GrantType`
   - All identifiers wrapped in newtypes (`ClientId`, `AuthCodeId`, etc.)

Full handler migration to typeclass constraints requires type unification first.

### Clarified Decisions (from spec)

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Canonical types | OAuth.Types newtypes | Type safety, illegal states unrepresentable |
| Boundary conversion | Servant `FromHttpApiData`/`ToHttpApiData` instances | Natural boundary, enables property-based round-trip tests |
| Invalid data handling | Fail at boundary (400) | Parse-don't-validate; no domain errors for structural issues |
| Migration strategy | Incremental per-handler | Lower risk, continuous integration, each migration testable |
| PendingAuthorization fields | Same newtypes | Already validated at request parse time |

### New Requirements (FR-018 through FR-021)

- **FR-018**: All OAuth.Types newtypes MUST implement `FromHttpApiData` and `ToHttpApiData`
- **FR-019**: Round-trip property tests for all `FromHttpApiData`/`ToHttpApiData` instances
- **FR-020**: `FromJSON` instances reject invalid data at parse time (400, not domain error)
- **FR-021**: Incremental handler migration with nested beads under mcp-51r

### Implementation Approach

#### Step 1: Add Servant Instances to OAuth.Types

```haskell
-- In OAuth.Types or a new OAuth.Servant module

instance FromHttpApiData ClientId where
  parseUrlPiece t
    | Text.null t = Left "ClientId cannot be empty"
    | otherwise = Right (ClientId t)

instance ToHttpApiData ClientId where
  toUrlPiece = unClientId

-- Similar for: AuthCodeId, SessionId, UserId, RefreshTokenId, RedirectUri, Scope
```

#### Step 2: Update FromJSON Instances with Invariant Checking

```haskell
-- NonEmpty RedirectUri parsing
instance FromJSON ClientInfo where
  parseJSON = withObject "ClientInfo" $ \o -> do
    name <- o .: "client_name"
    uris <- o .: "redirect_uris"
    case nonEmpty uris of
      Nothing -> fail "redirect_uris must not be empty"
      Just ne -> ClientInfo name ne <$> ...
```

#### Step 3: Incremental Handler Migration

Each handler migration is a separate bead under mcp-51r:

| Handler | Current Types | Target Types | Complexity |
|---------|---------------|--------------|------------|
| `handleClientRegistration` | `[Text]` redirect URIs | `NonEmpty RedirectUri` | Medium |
| `handleAuthorize` | `Text` client_id | `ClientId` newtype | Low |
| `handleToken` | `Text` code, client_id | `AuthCodeId`, `ClientId` | Medium |
| `handleRefresh` | `Text` refresh_token | `RefreshTokenId` | Low |
| `handleLogin` | `Text` session_id | `SessionId` | Low |
| ... (15+ handlers total) | | | |

#### Step 4: Property Tests for Boundary Instances

```haskell
-- In test/Laws/BoundarySpec.hs
prop "ClientId round-trip" $ \(cid :: ClientId) ->
  parseUrlPiece (toUrlPiece cid) === Right cid

prop "RedirectUri round-trip" $ \(uri :: RedirectUri) ->
  parseUrlPiece (toUrlPiece uri) === Right uri

-- NonEmpty requires custom generator
prop "NonEmpty RedirectUri FromJSON round-trip" $ \(uris :: NonEmpty RedirectUri) ->
  decode (encode uris) === Just uris
```

### Updated Module Organization

```text
src/MCP/Server/OAuth/
├── Types.hs           # Newtypes, ADTs, domain entities
├── Types/
│   └── Servant.hs     # NEW: FromHttpApiData/ToHttpApiData instances (or in Types.hs)
├── Store.hs           # OAuthStateStore typeclass
└── InMemory.hs        # TVar-based implementation

test/
├── Laws/
│   ├── OAuthStateStoreSpec.hs
│   ├── AuthBackendSpec.hs
│   └── BoundarySpec.hs    # NEW: Round-trip tests for Servant instances
└── Generators.hs          # Extended: Arbitrary for all newtypes
```

### Dependencies to Add

| Package | Version | Purpose |
|---------|---------|---------|
| `servant` | (already present) | `FromHttpApiData`, `ToHttpApiData` |

No new dependencies required—Servant instances use existing `servant` package.

### Migration Tracking Structure

```
mcp-51r (epic): Unify OAuth state types between HTTP.hs and OAuth.Types
├── mcp-51r.1: Add FromHttpApiData/ToHttpApiData to OAuth.Types newtypes
├── mcp-51r.2: Add round-trip property tests for boundary instances
├── mcp-51r.3: Update FromJSON instances to enforce invariants
├── mcp-51r.4: Migrate handleClientRegistration to newtypes
├── mcp-51r.5: Migrate handleAuthorize to newtypes
├── mcp-51r.6: Migrate handleToken to newtypes
├── ... (one bead per handler)
└── mcp-51r.N: Remove legacy Text-based types from HTTP.hs
```

### Constitution Compliance (Phase 3)

| Principle | Status | Evidence |
|-----------|--------|----------|
| I. Type-Driven Design | ✅ | Newtypes with invariants (NonEmpty, Set) make illegal states unrepresentable |
| II. Deep Module Architecture | ✅ | Servant instances in Types module; handlers unchanged except type annotations |
| III. Denotational Semantics | ✅ | Round-trip laws for all boundary conversions |
| IV. Total Functions | ✅ | `parseUrlPiece` returns `Either Text a`; no partial functions |
| V. Pure Core, Impure Shell | ✅ | Boundary conversion is pure; IO only in handler execution |
| VI. Property-Based Testing | ✅ | FR-019 requires round-trip property tests |

**Gate Status**: PASS - Phase 3 planning complete.

---

## Phase 4: Type-Safe HTTP Response Headers (mcp-nyr.18)

**Added**: 2025-12-12 | **Blocker**: mcp-nyr.18 | **Status**: Planning

### Problem Statement

A critical bug was discovered via git bisect: commit 1d882149 accidentally swapped `addHeader` call order in `handleLogin`, causing successful OAuth logins to redirect to a malformed URL containing the cookie value instead of the callback URI.

**Root cause**: Both `Location` and `Set-Cookie` headers are typed as `Text`, providing no compile-time protection against position swaps. This is the same class of bug that newtypes are designed to prevent.

```haskell
-- Current type signature (vulnerable):
Headers '[Header "Location" Text, Header "Set-Cookie" Text] NoContent

-- Bug: compiler accepts both orderings since both are Text
return $ addHeader clearCookie $ addHeader redirectUrl NoContent  -- WRONG
return $ addHeader redirectUrl $ addHeader clearCookie NoContent  -- RIGHT
```

### Blessed Solution (from spec clarifications 2025-12-12)

Add semantic newtypes for HTTP response header values in `OAuth.Types`:

| Newtype | Purpose | Usage |
|---------|---------|-------|
| `RedirectTarget` | OAuth callback/redirect URL | `Header "Location" RedirectTarget` |
| `SessionCookie` | Session cookie value | `Header "Set-Cookie" SessionCookie` |

**Design principle**: Names describe the value's semantic purpose, not the header name. All OAuth domain types belong in `OAuth.Types` to keep OAuth combinators framework-agnostic.

### Type-Safe Signature

```haskell
-- Fixed type signature (compile-time protection):
Headers '[Header "Location" RedirectTarget, Header "Set-Cookie" SessionCookie] NoContent

-- Now compiler rejects wrong order:
return $ addHeader clearCookie $ addHeader redirectUrl NoContent  -- TYPE ERROR
return $ addHeader redirectUrl $ addHeader clearCookie NoContent  -- OK
```

### Implementation Approach

#### Step 1: Add Newtypes to OAuth.Types

```haskell
-- | Target URL for HTTP 3xx redirects in OAuth flows.
-- Semantic name describes purpose, not header name.
newtype RedirectTarget = RedirectTarget { unRedirectTarget :: Text }
  deriving (Show, Eq)
  deriving newtype (ToHttpApiData)

-- | Session cookie value for OAuth login flow.
-- Distinct type from RedirectTarget prevents position-swap bugs.
newtype SessionCookie = SessionCookie { unSessionCookie :: Text }
  deriving (Show, Eq)
  deriving newtype (ToHttpApiData)
```

#### Step 2: Update Handler Type Signatures

```haskell
-- handleLogin return type (HTTP.hs)
handleLogin :: ... -> Handler (Headers '[Header "Location" RedirectTarget, Header "Set-Cookie" SessionCookie] NoContent)

-- handleAuthorize return type (also uses Set-Cookie)
handleAuthorize :: ... -> Handler (Headers '[Header "Set-Cookie" SessionCookie] Text)
```

#### Step 3: Update addHeader Call Sites

```haskell
-- Approve branch (line ~1003)
let redirectUrl = RedirectTarget $ toUrlPiece (pendingRedirectUri pending) <> "?code=" <> code <> stateParam
    clearCookie = SessionCookie "mcp_session=; Max-Age=0; Path=/"
return $ addHeader redirectUrl $ addHeader clearCookie NoContent

-- Deny branch (line ~946) - same pattern
let redirectUrl = RedirectTarget $ toUrlPiece (pendingRedirectUri pending) <> "?" <> errorParams <> stateParam
    clearCookie = SessionCookie "mcp_session=; Max-Age=0; Path=/"
return $ addHeader redirectUrl $ addHeader clearCookie NoContent
```

#### Step 4: Add Regression Test

```haskell
-- Verify Location header contains expected redirect URL pattern
it "redirects to callback URI with authorization code on successful login" $ do
  resp <- postLogin validCredentials validSession
  let locationHeader = lookup "Location" (responseHeaders resp)
  locationHeader `shouldSatisfy` \case
    Just loc -> "?code=" `isInfixOf` loc && not ("mcp_session" `isInfixOf` loc)
    Nothing -> False
```

### Files to Modify

| File | Change |
|------|--------|
| `src/MCP/Server/OAuth/Types.hs` | Add `RedirectTarget`, `SessionCookie` newtypes with `ToHttpApiData` |
| `src/MCP/Server/HTTP.hs` | Update type signatures and `addHeader` call sites |
| `test/HTTP/OAuthSpec.hs` | Add regression test for Location header content |

### Constitution Compliance (Phase 4)

| Principle | Status | Evidence |
|-----------|--------|----------|
| I. Type-Driven Design | ✅ | Distinct newtypes make position-swap bugs a compile error |
| II. Deep Module Architecture | ✅ | Types in OAuth.Types; HTTP.hs only changes signatures |
| III. Denotational Semantics | ✅ | Newtypes have clear semantic meaning in their names |
| IV. Total Functions | ✅ | No change to function totality |
| V. Pure Core, Impure Shell | ✅ | Types are pure; framework-agnostic per design principle |
| VI. Property-Based Testing | ✅ | Regression test added; type safety is compile-time |

**Gate Status**: PASS - Phase 4 planning complete.

---

## Phase 5: Functional Test Harness (mcp-di0)

**Added**: 2025-12-12 | **Epic**: mcp-di0 | **Status**: Planning

### Problem Statement

The OAuth typeclass refactoring requires HTTP-level black-box testing to verify:
1. Reference implementations work correctly at the HTTP boundary
2. Third-party implementations can run the same conformance suite
3. Type-safe headers (Phase 4) correctly set Location/Set-Cookie values
4. Full OAuth flows work end-to-end through the Servant API

Currently, tests are unit-level. We need hspec-wai-based functional tests that:
- Run in-process (no separate HTTP server)
- Are polymorphic over `OAuthStateStore` and `AuthBackend` implementations
- Support deterministic time control for expiry testing
- Provide helper combinators for common setup patterns

### Clarified Decisions (from spec session 2025-12-12)

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Test scope | Full OAuth flow coverage | Registration, authorization, login, token exchange, refresh, all error cases |
| State management | Per-test isolation with helper combinators | Each test isolated; combinators handle common setup |
| Implementation testing | Polymorphic specs with `runM` | Library users can test their own implementations |
| Module location | `MCP.Server.OAuth.Test.Internal` | Internal until OAuth extracted to separate library |
| hspec-wai pattern | Native DSL | `get`, `post`, `shouldRespondWith`, `matchHeaders` directly |
| Time control | `MonadTime` + abstract `advanceTime` callback | Controllable clock via `runM`; reference uses TVar |
| Application construction | `makeTestApp` with type coherence | Same `m` flows to both app and spec |
| Helper implementation | HTTP-based setup | Combinators issue real HTTP requests within `WaiSession` |
| Time advancement | Shared TVar for reference impl | Tests call `advanceTime` between HTTP requests |
| Polymorphic time interface | Abstract callback | Third-party impls provide their own mechanism |
| Test isolation | Dual strategy | Fresh app via `around` OR unique identifiers |
| Test credentials | `TestCredentials` in config | Users provide; responsible for `AuthBackend` setup |
| Invalid credentials | Obvious invalids | Empty password, nonexistent user |

### New Requirements (FR-023 through FR-035)

- **FR-023**: hspec-wai-based functional test harness, in-process via `with`
- **FR-024**: Full OAuth flow coverage (registration, authorization, login, token, refresh)
- **FR-025**: Verify HTTP-level correctness (status, headers, content types, bodies)
- **FR-026**: Error cases with obviously invalid credentials for auth failures
- **FR-027**: Per-test isolation with dual strategy (fresh app / unique IDs)
- **FR-028**: Polymorphic specs over `OAuthStateStore` and `AuthBackend`
- **FR-029**: Located in `MCP.Server.OAuth.Test.Internal`
- **FR-030**: Reference implementations tested via conformance suite
- **FR-031**: Native hspec-wai DSL, no custom wrappers
- **FR-032**: `MonadTime` constraint for controllable clock
- **FR-033**: `makeTestApp` with type-coherent `m`
- **FR-034**: Abstract `advanceTime` callback in polymorphic interface
- **FR-035**: `TestCredentials` in test configuration

### Implementation Approach

#### Step 1: Define TestConfig Record

```haskell
-- In MCP.Server.OAuth.Test.Internal
module MCP.Server.OAuth.Test.Internal
  ( TestConfig(..)
  , TestCredentials(..)
  , oauthConformanceSpec
  , withRegisteredClient
  , withAuthorizedUser
  , advanceTestTime
  ) where

-- | Configuration for polymorphic conformance test suite.
-- Implementations provide this to test their OAuthStateStore/AuthBackend.
data TestConfig m = TestConfig
  { tcMakeApp       :: IO (Application, NominalDiffTime -> IO ())
    -- ^ Create WAI Application and time control handle
  , tcRunM          :: forall a. m a -> IO a
    -- ^ Natural transformation to run polymorphic monad
  , tcCredentials   :: TestCredentials
    -- ^ Valid credentials for login tests
  }

-- | Test credentials for OAuth login flow.
data TestCredentials = TestCredentials
  { tcUsername :: Text
  , tcPassword :: Text
  }
```

#### Step 2: Create Polymorphic Conformance Spec

```haskell
-- | Polymorphic conformance test suite.
-- Run against any OAuthStateStore/AuthBackend implementation.
-- Updated 2025-12-15: Added user type equality constraints per Phase 8
oauthConformanceSpec
  :: forall m.
     ( OAuthStateStore m
     , AuthBackend m
     , MonadTime m
     , AuthBackendUser m ~ OAuthUser m      -- Phase 8: type equality
     , AuthBackendUserId m ~ OAuthUserId m  -- Phase 8: type equality
     , ToJWT (OAuthUser m)                  -- Phase 8: JWT at operation level
     )
  => TestConfig m
  -> Spec
oauthConformanceSpec config = do
  describe "OAuth Conformance Suite" $ do
    clientRegistrationSpec config
    authorizationSpec config
    loginFlowSpec config
    tokenExchangeSpec config
    tokenRefreshSpec config
    errorCasesSpec config
    expirySpec config
```

#### Step 3: Implement Helper Combinators

```haskell
-- | Register a test client and return its ID.
-- Issues real POST /register request within WaiSession.
withRegisteredClient
  :: TestConfig m
  -> (ClientId -> WaiSession st a)
  -> WaiSession st a
withRegisteredClient config action = do
  -- Generate unique client name to avoid collisions
  uuid <- liftIO UUID.nextRandom
  let clientName = "test-client-" <> UUID.toText uuid
      body = object
        [ "client_name" .= clientName
        , "redirect_uris" .= ["http://localhost/callback" :: Text]
        ]
  resp <- post "/register" (encode body)
  liftIO $ statusCode (simpleStatus resp) `shouldBe` 201
  let Just clientId = decode (simpleBody resp) >>= (.: "client_id")
  action (ClientId clientId)

-- | Complete authorization flow up to obtaining an auth code.
withAuthorizedUser
  :: TestConfig m
  -> ClientId
  -> (AuthCodeId -> WaiSession st a)
  -> WaiSession st a
withAuthorizedUser config clientId action = do
  -- Generate PKCE verifier/challenge
  let (verifier, challenge) = generatePKCE
  -- Start authorization
  let authUrl = "/authorize?client_id=" <> unClientId clientId
             <> "&redirect_uri=http://localhost/callback"
             <> "&response_type=code"
             <> "&code_challenge=" <> challenge
             <> "&code_challenge_method=S256"
  resp1 <- get authUrl
  -- Extract session cookie
  let Just sessionCookie = extractSessionCookie resp1
  -- Submit login form
  let loginBody = "username=" <> tcUsername (tcCredentials config)
               <> "&password=" <> tcPassword (tcCredentials config)
               <> "&session_id=" <> sessionCookie
               <> "&action=approve"
  resp2 <- postHtmlForm "/login" loginBody
  -- Extract auth code from Location header
  let Just location = lookup "Location" (simpleHeaders resp2)
      Just code = extractCodeFromRedirect location
  action (AuthCodeId code)
```

#### Step 4: Reference Implementation TestConfig

```haskell
-- | TestConfig for reference in-memory implementation.
-- Used by our own tests; serves as example for third-party implementers.
referenceTestConfig :: IO (TestConfig AppM)
referenceTestConfig = do
  timeTVar <- newTVarIO defaultTestTime
  env <- mkTestEnv timeTVar
  let makeApp = do
        let app = serveWithContext api ctx (hoistServer api (runAppM env) server)
        return (app, \dt -> atomically $ modifyTVar timeTVar (addUTCTime dt))
  return TestConfig
    { tcMakeApp = makeApp
    , tcRunM = runReaderT (runAppM env)
    , tcCredentials = TestCredentials "demo" "demo123"
    }
```

#### Step 5: Test Isolation Strategies

```haskell
-- Strategy 1: Fresh Application per test (strict isolation)
isolatedSpec :: TestConfig m -> Spec
isolatedSpec config =
  around (withFreshApp config) $ do
    it "test with fresh state" $ \app -> do
      with (return app) $ do
        -- Each test gets clean state
        ...

-- Strategy 2: Shared Application with unique IDs (performance)
sharedSpec :: TestConfig m -> Spec
sharedSpec config = do
  (app, advanceTime) <- runIO (tcMakeApp config)
  with (return app) $ do
    it "test with unique IDs" $ do
      withRegisteredClient config $ \clientId -> do
        -- Uses UUID-based client ID, no collision
        ...
```

#### Step 6: Expiry Testing with Time Control

```haskell
expirySpec :: TestConfig m -> Spec
expirySpec config = describe "Expiry behavior" $ do
  around (withFreshApp config) $ \app -> do
    it "rejects expired authorization codes" $ \(app, advanceTime) -> do
      with (return app) $ do
        withRegisteredClient config $ \clientId -> do
          withAuthorizedUser config clientId $ \code -> do
            -- Advance time past auth code expiry (default 10 min)
            liftIO $ advanceTime (11 * 60)
            -- Attempt token exchange
            let tokenBody = tokenExchangeRequest clientId code
            post "/token" tokenBody `shouldRespondWith` 400

    it "rejects expired login sessions" $ \(app, advanceTime) -> do
      with (return app) $ do
        -- Start authorization but don't complete login
        resp <- get "/authorize?client_id=test&..."
        let sessionCookie = extractSessionCookie resp
        -- Advance time past session expiry
        liftIO $ advanceTime (11 * 60)
        -- Attempt to submit login
        post "/login" (loginForm sessionCookie) `shouldRespondWith` 400
```

#### Step 7: Error Case Testing

```haskell
errorCasesSpec :: TestConfig m -> Spec
errorCasesSpec config = describe "Error cases" $ do
  it "returns 400 for invalid client" $ do
    get "/authorize?client_id=nonexistent&..." `shouldRespondWith` 400

  it "returns 401 for invalid credentials" $ do
    -- Use obviously invalid credentials
    post "/login" (loginForm "__invalid_user__" "") `shouldRespondWith` 401

  it "returns 400 for invalid PKCE verifier" $ do
    withRegisteredClient config $ \clientId -> do
      withAuthorizedUser config clientId $ \code -> do
        let badVerifier = "wrong_verifier_that_doesnt_match_challenge"
        post "/token" (tokenRequest clientId code badVerifier) `shouldRespondWith` 400

  it "returns 400 for malformed request" $ do
    post "/register" "not json" `shouldRespondWith` 400
```

### Updated Module Organization

```text
src/MCP/Server/OAuth/
├── Types.hs              # Newtypes, ADTs, domain entities
├── Store.hs              # OAuthStateStore typeclass
├── InMemory.hs           # TVar-based implementation
└── Test/
    └── Internal.hs       # NEW: Polymorphic conformance suite

test/
├── Main.hs               # Modified: import and run conformance specs
├── Laws/
│   ├── OAuthStateStoreSpec.hs
│   ├── AuthBackendSpec.hs
│   └── BoundarySpec.hs
├── Functional/
│   └── OAuthFlowSpec.hs  # NEW: Runs conformance suite with reference impl
├── Generators.hs
└── TestMonad.hs          # Extended: MonadTime with TVar time control
```

### Dependencies to Add

| Package | Version | Purpose |
|---------|---------|---------|
| `hspec-wai` | ^0.11 | WAI application testing in hspec |
| `hspec-wai-json` | ^0.11 | JSON matchers for hspec-wai |
| `wai-extra` | ^3.1 | Request body parsing utilities |

### Test Coverage Matrix

| Flow | Happy Path | Error Cases | Expiry |
|------|------------|-------------|--------|
| Client Registration | ✅ POST /register 201 | ✅ Invalid JSON 400 | N/A |
| Authorization | ✅ GET /authorize 200 | ✅ Invalid client 400 | N/A |
| Login | ✅ POST /login 302 | ✅ Invalid creds 401 | ✅ Expired session |
| Token Exchange | ✅ POST /token 200 | ✅ Invalid PKCE 400 | ✅ Expired code |
| Token Refresh | ✅ POST /token 200 | ✅ Invalid token 400 | ✅ Expired refresh |
| Headers | ✅ Location correct | ✅ WWW-Authenticate | N/A |

### Constitution Compliance (Phase 5)

| Principle | Status | Evidence |
|-----------|--------|----------|
| I. Type-Driven Design | ✅ | `TestConfig m` parameterized over implementation monad; type coherence enforced |
| II. Deep Module Architecture | ✅ | Single `Internal` module exports minimal API; complexity hidden in combinators |
| III. Denotational Semantics | ✅ | Test helpers have clear semantics; `advanceTime` has obvious meaning |
| IV. Total Functions | ✅ | All helpers return in `WaiSession`; no partial functions |
| V. Pure Core, Impure Shell | ✅ | Test logic is pure over abstract interface; IO at boundaries |
| VI. Property-Based Testing | ✅ | Conformance suite verifies algebraic properties via HTTP; expiry laws tested |

**Gate Status**: PASS - Phase 5 planning complete.

---

## Phase 6: Polymorphic Handler Migration (mcp-di0.16.1)

**Added**: 2025-12-12 | **Blocker**: mcp-di0.16.1 | **Status**: Planning

### Problem Statement

The conformance test harness (Phase 5) requires building a WAI `Application` from polymorphic handlers. Currently, HTTP.hs handlers use the concrete `Handler` monad directly, preventing:

1. Reuse of handlers outside Servant (e.g., Yesod, CLI tools)
2. Testing handlers against arbitrary `OAuthStateStore`/`AuthBackend` implementations
3. Proper three-layer cake architecture with delayed monomorphization

**Critical Architectural Decision** (from spec clarifications 2025-12-12):
> Handlers MUST be polymorphic over `m` with typeclass constraints. `AppM` is ONE instantiation at Servant boundary, not the handler definition. Design principle: delay monomorphization until the last possible moment.

### Clarified Decisions (from spec session 2025-12-12)

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Handler signatures | Polymorphic over `m` | FR-036: Enable framework-agnostic reuse |
| Concrete types | Only at boundaries | `AppM`/`Handler` are instantiations, not definitions |
| Constraint selection | Judicious | FR-037: `MonadIO` for mundane IO, new typeclasses only when beneficial |
| Servant's role | Reference implementation + typed API spec | Handlers are constructive proofs, usable standalone |
| Future reuse | Yesod application | Handlers will be reused; Servant provides dev/test infrastructure |

### New Requirements (FR-036, FR-037)

- **FR-036**: OAuth HTTP handler functions MUST be polymorphic over monad `m`, constrained by typeclass requirements. Concrete types like `AppM` or `Handler` are instantiations at the Servant boundary via `hoistServer`, NOT the handler definitions.
- **FR-037**: Constraint selection MUST be judicious. Use existing abstractions when available. For mundane IO library calls, add `MonadIO` directly. Create new typeclasses only when swappability or testability genuinely benefits.

### Implementation Approach

#### Step 1: Define Handler Constraints Pattern

```haskell
-- Common constraint bundle for OAuth handlers
-- Updated 2025-12-15: Added user type equality constraints per spec refinement
-- Updated 2025-12-15: Added ValidationError/AuthorizationError, removed ServerError (Phase 10)
type OAuthHandler m env e =
  ( OAuthStateStore m
  , AuthBackend m
  , MonadTime m
  , MonadIO m  -- For JWT signing, UUID generation
  , MonadReader env m
  , MonadError e m
  , HasType OAuthConfig env
  , HasType (OAuthStateEnv m) env
  , HasType (AuthBackendEnv m) env
  -- Four domain error types (Phase 10)
  , AsType (OAuthStateError m) e   -- Associated: storage failures
  , AsType (AuthBackendError m) e  -- Associated: auth failures
  , AsType ValidationError e       -- Fixed: semantic validation
  , AsType AuthorizationError e    -- Fixed: OAuth protocol errors
  -- NOTE: NO ServerError constraint; translation happens at boundary
  -- User type equality constraints (FR-039)
  , AuthBackendUser m ~ OAuthUser m
  , AuthBackendUserId m ~ OAuthUserId m
  )

-- Alternative: type family for cleaner signatures
type family OAuthConstraints m :: Constraint where
  OAuthConstraints m = (OAuthStateStore m, AuthBackend m, MonadTime m, MonadIO m)
```

#### Step 2: Migrate Handler Signatures

**Before** (concrete `Handler`):
```haskell
handleClientRegistration
  :: OAuthState
  -> OAuthConfig
  -> ClientRegistrationRequest
  -> Handler ClientRegistrationResponse
handleClientRegistration state config req = do
  clientId <- liftIO $ generateClientId
  atomically $ modifyTVar (oauthClients state) (Map.insert clientId info)
  return response
```

**After** (polymorphic `m`):
```haskell
handleClientRegistration
  :: (OAuthStateStore m, MonadIO m, MonadReader env m, HasType OAuthConfig env)
  => ClientRegistrationRequest
  -> m ClientRegistrationResponse
handleClientRegistration req = do
  config <- asks (view (typed @OAuthConfig))
  clientId <- liftIO generateClientId  -- MonadIO for UUID generation
  let info = mkClientInfo req clientId
  storeClient clientId info            -- OAuthStateStore method
  return $ mkResponse clientId info
```

#### Step 3: Create Polymorphic Server Definition

```haskell
-- In MCP.Server.OAuth.Server (NEW module)
module MCP.Server.OAuth.Server
  ( oauthServer
  , OAuthAPI
  ) where

-- | Polymorphic OAuth server. Framework-agnostic.
-- Instantiate with hoistServer at framework boundary.
-- Updated 2025-12-15: Added user type constraints per spec refinement
oauthServer
  :: forall m env e.
     ( OAuthStateStore m
     , AuthBackend m
     , MonadTime m
     , MonadIO m
     , MonadReader env m
     , MonadError e m
     , HasType OAuthConfig env
     , HasType JWTSettings env
     , HasType (OAuthStateEnv m) env
     , HasType (AuthBackendEnv m) env
     , AsType (OAuthStateError m) e
     , AsType (AuthBackendError m) e
     , AsType ValidationError e        -- Fixed type (Phase 10)
     , AsType AuthorizationError e     -- Fixed type (Phase 10)
     -- NOTE: ServerError NOT in constraints (Phase 10); translation at boundary only
     -- User type equality (FR-039)
     , AuthBackendUser m ~ OAuthUser m
     , AuthBackendUserId m ~ OAuthUserId m
     -- JWT constraint for token generation (FR-040)
     , ToJWT (OAuthUser m)
     )
  => ServerT OAuthAPI m
oauthServer =
       handleMetadata
  :<|> handleProtectedResourceMetadata
  :<|> handleClientRegistration
  :<|> handleAuthorize
  :<|> handleLogin
  :<|> handleToken
  :<|> handleMCP
```

#### Step 4: Servant Boundary Translation

```haskell
-- In MCP.Server.HTTP (modified)
module MCP.Server.HTTP where

import MCP.Server.OAuth.Server (oauthServer, OAuthAPI)

-- | Concrete application monad for Servant runtime
type AppM = ReaderT AppEnv (ExceptT AppError IO)

-- | Run polymorphic handlers in Servant's Handler monad
runAppM :: AppEnv -> AppM a -> Handler a
runAppM env action = do
  result <- liftIO $ runExceptT $ runReaderT action env
  case result of
    Left err -> throwError (toServerError err)
    Right a  -> return a

-- | Construct WAI Application from polymorphic server
mkApplication :: AppEnv -> Context '[JWTSettings, CookieSettings] -> Application
mkApplication env ctx =
  serveWithContext
    (Proxy @OAuthAPI)
    ctx
    (hoistServerWithContext
      (Proxy @OAuthAPI)
      (Proxy @'[JWTSettings, CookieSettings])
      (runAppM env)
      oauthServer)  -- Polymorphic server instantiated here
```

#### Step 5: Test Harness Integration

```haskell
-- In MCP.Server.OAuth.Test.Fixtures
module MCP.Server.OAuth.Test.Fixtures where

import MCP.Server.OAuth.Server (oauthServer, OAuthAPI)

-- | Test monad with controllable time
type TestM = ReaderT TestEnv (ExceptT TestError IO)

-- | Reference test configuration using polymorphic server
referenceTestConfig :: IO TestConfig
referenceTestConfig = do
  timeTVar <- newTVarIO defaultTestTime
  env <- mkTestEnv timeTVar
  let makeApp = do
        let ctx = defaultCookieSettings :. jwtSettings :. EmptyContext
        let app = serveWithContext
              (Proxy @OAuthAPI)
              ctx
              (hoistServerWithContext
                (Proxy @OAuthAPI)
                (Proxy @'[JWTSettings, CookieSettings])
                (runTestM env)   -- TestM instantiation
                oauthServer)     -- SAME polymorphic server!
        return (app, \dt -> atomically $ modifyTVar timeTVar (addUTCTime dt))
  return TestConfig
    { tcMakeApp = makeApp
    , tcCredentials = TestCredentials "demo" "demo123"
    }
```

### Migration Order

Handlers should be migrated incrementally. Order by dependency (least dependent first):

| Order | Handler | Dependencies | Complexity |
|-------|---------|--------------|------------|
| 1 | `handleMetadata` | None (pure) | Low |
| 2 | `handleProtectedResourceMetadata` | None (pure) | Low |
| 3 | `handleClientRegistration` | `OAuthStateStore.storeClient` | Medium |
| 4 | `handleAuthorize` | `OAuthStateStore.lookupClient`, `storePendingAuth` | Medium |
| 5 | `handleLogin` | `AuthBackend.validateCredentials`, `OAuthStateStore.*` | High |
| 6 | `handleToken` | `OAuthStateStore.*`, JWT signing | High |
| 7 | `handleMCP` | All (protected endpoint) | Medium |

### Constraint Selection Guidelines (FR-037)

| Capability | Use Typeclass? | Rationale |
|------------|----------------|-----------|
| OAuth state persistence | ✅ `OAuthStateStore` | Core swappability requirement |
| Credential validation | ✅ `AuthBackend` | Core swappability requirement |
| Time access | ✅ `MonadTime` | Testability (deterministic expiry tests) |
| JWT signing | ❌ `MonadIO` | jose library just needs IO; not worth abstracting |
| UUID generation | ❌ `MonadIO` | Standard library; not worth abstracting |
| Random bytes | ❌ `MonadIO` | cryptonite just needs IO; not worth abstracting |
| Tracing | ❌ `MonadIO` | Wired via environment; not handler concern |

### Files to Modify

| File | Change |
|------|--------|
| `src/MCP/Server/OAuth/Server.hs` | NEW: Polymorphic `oauthServer` definition |
| `src/MCP/Server/HTTP.hs` | Modify: Remove handler definitions, add `hoistServer` boundary |
| `src/MCP/Server/OAuth/Handlers/*.hs` | NEW: Individual polymorphic handler modules |
| `test/MCP/Server/OAuth/Test/Fixtures.hs` | Modify: Implement `referenceTestConfig` with polymorphic server |

### Updated Module Organization

```text
src/MCP/Server/OAuth/
├── Types.hs              # Newtypes, ADTs, domain entities
├── Store.hs              # OAuthStateStore typeclass
├── InMemory.hs           # TVar-based implementation
├── Server.hs             # NEW: Polymorphic oauthServer, OAuthAPI
├── Handlers/             # NEW: Individual polymorphic handlers
│   ├── Metadata.hs       # handleMetadata, handleProtectedResourceMetadata
│   ├── Registration.hs   # handleClientRegistration
│   ├── Authorization.hs  # handleAuthorize
│   ├── Login.hs          # handleLogin
│   ├── Token.hs          # handleToken
│   └── MCP.hs            # handleMCP
└── Test/
    └── Internal.hs       # Polymorphic conformance suite

src/MCP/Server/
├── HTTP.hs               # MODIFIED: Servant boundary only, uses OAuth.Server
└── HTTP/
    └── AppM.hs           # NEW: AppM type, AppEnv, AppError, runAppM
```

### Constitution Compliance (Phase 6)

| Principle | Status | Evidence |
|-----------|--------|----------|
| I. Type-Driven Design | ✅ | Polymorphic handler signatures encode constraints at type level; type coherence enforced across boundary |
| II. Deep Module Architecture | ✅ | Handlers hide complexity; Servant boundary is thin translation layer |
| III. Denotational Semantics | ✅ | Handlers are "constructive proofs of correct implementability" — clear semantic meaning |
| IV. Total Functions | ✅ | All handlers return in monadic context; errors via `MonadError` |
| V. Pure Core, Impure Shell | ✅ | Handlers pure over abstract interface; `MonadIO` only for library calls; boundary does instantiation |
| VI. Property-Based Testing | ✅ | Polymorphic server enables testing against arbitrary implementations |

**Gate Status**: PASS - Phase 6 planning complete.

---

## Design Principles Summary (2025-12-12 Clarifications)

These principles were clarified during spec review and apply across all phases:

### 1. Delay Monomorphization Until Last Possible Moment

```
┌─────────────────────────────────────────────────────────────────┐
│                    POLYMORPHIC CORE                             │
│  oauthServer :: (OAuthStateStore m, ...) => ServerT OAuthAPI m  │
│  handlers :: (constraints) => ... -> m Response                 │
└─────────────────────────────────────────────────────────────────┘
                              │
         ┌────────────────────┼────────────────────┐
         │                    │                    │
         ▼                    ▼                    ▼
   ┌──────────┐        ┌──────────┐        ┌──────────┐
   │ Servant  │        │  Tests   │        │  Yesod   │
   │ (AppM)   │        │ (TestM)  │        │ (future) │
   └──────────┘        └──────────┘        └──────────┘
   hoistServer         hoistServer         custom runner
```

### 2. Handlers as Constructive Proofs

Handlers are NOT Servant-specific code. They are:
- **Constructive proofs** of correct implementability
- **Framework-agnostic** — usable wherever constraints can be discharged
- **Specified and tested** here with Servant's help
- **Reusable** in Yesod app (planned future use)

### 3. Judicious Constraint Selection

| Worth Abstracting | Not Worth Abstracting |
|-------------------|----------------------|
| OAuth state persistence (swappable backends) | JWT signing (jose needs IO) |
| Credential validation (enterprise SSO) | UUID generation (standard library) |
| Time access (deterministic testing) | Random bytes (cryptonite needs IO) |

Rule: Create typeclass only when swappability or testability genuinely benefits.

### 4. Servant's Role

Servant provides:
1. **Typed API specification** — compile-time route verification
2. **Production-ready reference implementation** — battle-tested HTTP layer
3. **Test infrastructure** — hspec-wai integration via WAI Application

Servant is NOT:
- The only framework for these handlers
- The place where handler logic lives
- Required for handlers to be useful

### 5. Reference Implementation Type Colocation (Phase 9)

Reference implementation types belong with their implementations:

| Type | Location | Rationale |
|------|----------|-----------|
| `AuthUser`, `UserId` | `MCP.Server.Auth.Demo` | Demo AuthBackend's user types |
| `DemoAuthError` | `MCP.Server.Auth.Demo` | Demo AuthBackend's error type |
| `OAuthStoreError` | `MCP.Server.OAuth.InMemory` | TVar store's error type |
| `OAuthTVarEnv` | `MCP.Server.OAuth.InMemory` | TVar store's environment |

**Import rules**:
- Polymorphic code (handlers, server, tests) → NEVER import concrete types
- Application boundary (Main.hs, HTTP.hs) → OK to import for wiring
- Test fixtures → OK to import for setup

---

## Phase 7: Type Witness Patterns (FR-038)

**Added**: 2025-12-13 | **Status**: Constraint (applies to all phases)

### Problem Statement

Type-directed polymorphic functions (common in property-based testing and generic programming) require threading type information. Haskell historically used `undefined :: Type` as a type witness, but this is unsafe and deprecated.

**Bad pattern (PROHIBITED)**:
```haskell
-- NEVER do this - undefined can cause runtime errors if accidentally evaluated
identityRoundTrip "ClientId" (undefined :: ClientId)
prop_roundTrip (undefined :: ClientId)
```

### Blessed Solution (FR-038)

Use modern Haskell type witness patterns with `TypeApplications` and `AllowAmbiguousTypes`:

**Option 1: Type Applications (preferred)**
```haskell
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- Type-level argument, no value-level witness needed
identityRoundTrip @ClientId
prop_roundTrip @ClientId

-- Definition uses ScopedTypeVariables to bind the type
identityRoundTrip :: forall a. (FromHttpApiData a, ToHttpApiData a, Arbitrary a, Show a) => Spec
identityRoundTrip = prop "round-trip" $ \(x :: a) ->
  parseUrlPiece (toUrlPiece x) === Right x
```

**Option 2: Proxy (when TypeApplications not suitable)**
```haskell
{-# LANGUAGE TypeApplications #-}

-- Explicit Proxy value, safe to evaluate
identityRoundTrip (Proxy @ClientId)

-- Definition takes Proxy
identityRoundTrip :: forall a. (FromHttpApiData a, ToHttpApiData a, Arbitrary a, Show a) => Proxy a -> Spec
identityRoundTrip _ = prop "round-trip" $ \(x :: a) ->
  parseUrlPiece (toUrlPiece x) === Right x
```

### Required GHC Extensions

```haskell
{-# LANGUAGE AllowAmbiguousTypes #-}  -- Enables type-only parameters
{-# LANGUAGE TypeApplications #-}     -- Enables @Type syntax
{-# LANGUAGE ScopedTypeVariables #-}  -- Binds type vars in where clauses
```

### Impact on Test Modules

#### test/Laws/BoundarySpec.hs

```haskell
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Laws.BoundarySpec (spec) where

import Data.Proxy (Proxy(..))
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

-- | Generic round-trip test for FromHttpApiData/ToHttpApiData
httpApiRoundTrip
  :: forall a. (FromHttpApiData a, ToHttpApiData a, Arbitrary a, Show a, Eq a)
  => Spec
httpApiRoundTrip = prop (show (typeRep (Proxy @a)) <> " round-trip") $ \(x :: a) ->
  parseUrlPiece (toUrlPiece x) === Right x

-- | Generic round-trip test for FromJSON/ToJSON
jsonRoundTrip
  :: forall a. (FromJSON a, ToJSON a, Arbitrary a, Show a, Eq a)
  => Spec
jsonRoundTrip = prop (show (typeRep (Proxy @a)) <> " JSON round-trip") $ \(x :: a) ->
  decode (encode x) === Just x

spec :: Spec
spec = describe "HTTP API boundary round-trips" $ do
  describe "FromHttpApiData/ToHttpApiData" $ do
    httpApiRoundTrip @ClientId
    httpApiRoundTrip @AuthCodeId
    httpApiRoundTrip @SessionId
    httpApiRoundTrip @UserId
    httpApiRoundTrip @RefreshTokenId
    httpApiRoundTrip @RedirectUri
    httpApiRoundTrip @Scope
    httpApiRoundTrip @GrantType
    httpApiRoundTrip @CodeChallengeMethod

  describe "FromJSON/ToJSON" $ do
    jsonRoundTrip @ClientInfo
    jsonRoundTrip @AuthorizationCode
    jsonRoundTrip @TokenResponse
```

#### test/Laws/OAuthStateStoreSpec.hs

```haskell
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Polymorphic store law: lookup after store returns Just
storeRetrieveLaw
  :: forall m k v.
     ( OAuthStateStore m
     , Arbitrary k, Show k, Eq k
     , Arbitrary v, Show v, Eq v
     )
  => (k -> v -> m ())        -- ^ Store operation
  -> (k -> m (Maybe v))      -- ^ Lookup operation
  -> Proxy k                 -- ^ Key type witness (Proxy OK here for clarity)
  -> Proxy v                 -- ^ Value type witness
  -> Spec
storeRetrieveLaw store lookup _ _ =
  prop "store then lookup returns Just" $ \(k :: k) (v :: v) -> monadicIO $ do
    run $ store k v
    result <- run $ lookup k
    assert (result == Just v)

-- Usage:
spec :: Spec
spec = describe "OAuthStateStore laws" $ do
  storeRetrieveLaw @TestM
    storeClient lookupClient
    (Proxy @ClientId) (Proxy @ClientInfo)
```

### Checklist for All Test Code

Before merging any test code, verify:

- [ ] No `undefined :: Type` patterns anywhere
- [ ] Type witnesses use `@Type` or `Proxy @Type`
- [ ] Required extensions enabled: `AllowAmbiguousTypes`, `TypeApplications`, `ScopedTypeVariables`
- [ ] Type variable bindings use explicit `:: a` annotations in lambda/where clauses
- [ ] `Typeable` constraint added if `typeRep` needed for test names

### Files Affected

| File | Change Required |
|------|-----------------|
| `test/Laws/BoundarySpec.hs` | Use `@Type` for round-trip tests |
| `test/Laws/OAuthStateStoreSpec.hs` | Use `Proxy @Type` for polymorphic law tests |
| `test/Laws/AuthBackendSpec.hs` | Use `@Type` patterns |
| `test/Generators.hs` | No change (Arbitrary instances don't need witnesses) |
| `src/MCP/Server/OAuth/Test/Internal.hs` | Use `@Type` in conformance suite helpers |

### Constitution Compliance (Phase 7)

| Principle | Status | Evidence |
|-----------|--------|----------|
| I. Type-Driven Design | ✅ | Type-level witnesses enforce type safety; no runtime `undefined` |
| II. Deep Module Architecture | N/A | Constraint, not module structure |
| III. Denotational Semantics | ✅ | `@Type` and `Proxy @Type` have clear semantic meaning |
| IV. Total Functions | ✅ | No partial functions (`undefined` eliminated) |
| V. Pure Core, Impure Shell | N/A | Constraint, not architecture |
| VI. Property-Based Testing | ✅ | Safe test patterns enable reliable property testing |

**Gate Status**: PASS - Phase 7 constraint documented.

---

## Phase 8: User Type Polymorphism (FR-039 through FR-041)

**Added**: 2025-12-15 | **Status**: Planning | **Spec Session**: 2025-12-15
**PARTIALLY SUPERSEDED**: 2025-12-17 — `AuthBackendUserId m` and `OAuthUserId m` removed per Phase 14. See Phase 14 for updated design.

### Problem Statement

The original design used a fixed `AuthUser` type and `Bool` return from `validateCredentials`. This prevents library consumers from:

1. Using their own user representations (enterprise IdP integration returns different user shapes)
2. Using their own user ID representations (UUID, Integer, Text, custom newtype)
3. Type-safe flow of user identity from authentication through to token generation

**Spec Refinement** (2025-12-15): Add associated user and userId types to both typeclasses with type equality constraints ensuring compile-time agreement.

### Clarified Decisions (from spec session 2025-12-15)

| Decision | Choice | Rationale |
|----------|--------|-----------|
| User type coordination | Type equality constraints | `AuthBackendUser m ~ OAuthUser m` ensures compile-time agreement |
| UserId handling | Separate associated type | `OAuthUserId m` for state structures; distinct from full user |
| JWT constraints | At operation level | `ToJWT (OAuthUser m)` added where needed, not at typeclass level |
| validateCredentials return | `Maybe (UserId, User)` tuple | Returns both values together; no separate extraction needed |
| Data parameterization | Only AuthorizationCode | Access/refresh tokens validated by JWT signature; PendingAuth has no user |
| Terminology | "Reference implementation" | Not "default" — one example among equals |
| Time access | MonadTime on OAuthStateStore | Enables deterministic expiry testing |

### New/Updated Requirements

- **FR-039**: Type equality constraints `AuthBackendUser m ~ OAuthUser m` and `AuthBackendUserId m ~ OAuthUserId m`
- **FR-040**: JWT constraints at operation level with optional userId in claims
- **FR-041**: Only `AuthorizationCode userId` is parameterized

### Implementation Approach

#### Step 1: Update OAuthStateStore Typeclass

```haskell
-- In MCP.Server.OAuth.Store
class (MonadTime m) => OAuthStateStore m where
  -- Associated types for implementation-specific concerns
  type OAuthStateError m
  type OAuthStateEnv m

  -- NEW: User types for this implementation
  type OAuthUser m      -- Full user for JWT tokens
  type OAuthUserId m    -- User identifier for state structures

  -- Existing methods (unchanged signatures)
  storeAuthCode :: AuthCodeId -> AuthorizationCode (OAuthUserId m) -> m ()
  lookupAuthCode :: AuthCodeId -> m (Maybe (AuthorizationCode (OAuthUserId m)))
  deleteAuthCode :: AuthCodeId -> m ()

  storeClient :: ClientId -> ClientInfo -> m ()
  lookupClient :: ClientId -> m (Maybe ClientInfo)

  -- ... other methods
```

#### Step 2: Update AuthBackend Typeclass

```haskell
-- In MCP.Server.Auth.Backend
class AuthBackend m where
  -- Associated types
  type AuthBackendError m
  type AuthBackendEnv m

  -- NEW: User types for this implementation
  type AuthBackendUser m    -- Full authenticated user
  type AuthBackendUserId m  -- User identifier

  -- CHANGED: Returns tuple of (UserId, User) on success
  validateCredentials
    :: Username
    -> PlaintextPassword
    -> m (Maybe (AuthBackendUserId m, AuthBackendUser m))
```

#### Step 3: Parameterize AuthorizationCode

```haskell
-- In MCP.Server.OAuth.Types
-- CHANGED: Now parameterized over userId type
data AuthorizationCode userId = AuthorizationCode
  { authCodeId          :: AuthCodeId
  , authCodeClientId    :: ClientId
  , authCodeUserId      :: userId              -- Parameterized!
  , authCodeRedirectUri :: RedirectUri
  , authCodeScope       :: Set Scope
  , authCodeChallenge   :: CodeChallenge
  , authCodeExpiry      :: UTCTime
  }
  deriving (Eq, Show, Generic)

-- Functor instance for mapping over userId
instance Functor AuthorizationCode where
  fmap f code = code { authCodeUserId = f (authCodeUserId code) }
```

#### Step 4: Update Reference Implementations

**Note**: Per Phase 9 (FR-042), all concrete types (`AuthUser`, `UserId`, `DemoAuthError`, `OAuthStoreError`) must be defined in their respective implementation modules, not imported from shared locations.

```haskell
-- In MCP.Server.OAuth.InMemory
-- OAuthStoreError and OAuthTVarEnv defined HERE (Phase 9)
instance OAuthStateStore (ReaderT OAuthTVarEnv IO) where
  type OAuthStateError (ReaderT OAuthTVarEnv IO) = OAuthStoreError
  type OAuthStateEnv (ReaderT OAuthTVarEnv IO) = OAuthTVarEnv

  -- Reference implementation uses AuthUser/UserId from Auth.Demo
  type OAuthUser (ReaderT OAuthTVarEnv IO) = AuthUser
  type OAuthUserId (ReaderT OAuthTVarEnv IO) = UserId  -- Text or newtype

  -- Implementation unchanged, just uses concrete types
  storeAuthCode codeId code = ...
  lookupAuthCode codeId = ...

-- In MCP.Server.Auth.Demo
-- AuthUser, UserId, DemoAuthError defined HERE (Phase 9)
instance AuthBackend (ReaderT DemoCredentialEnv IO) where
  type AuthBackendError (ReaderT DemoCredentialEnv IO) = DemoAuthError
  type AuthBackendEnv (ReaderT DemoCredentialEnv IO) = DemoCredentialEnv

  -- Reference implementation user types (defined in THIS module)
  type AuthBackendUser (ReaderT DemoCredentialEnv IO) = AuthUser
  type AuthBackendUserId (ReaderT DemoCredentialEnv IO) = UserId

  -- CHANGED: Return (UserId, AuthUser) tuple
  validateCredentials username password = do
    store <- asks credentialStore
    case lookupCredential username store of
      Nothing -> return Nothing
      Just (hashedPw, userId) ->
        if validatePassword password hashedPw
          then do
            let user = AuthUser { authUserId = userId, authUsername = username }
            return $ Just (userId, user)
          else return Nothing
```

#### Step 5: Update Handler Signatures

```haskell
-- Handler that creates auth code needs both user types to match
handleLogin
  :: ( OAuthStateStore m
     , AuthBackend m
     , MonadTime m
     , MonadIO m
     , AuthBackendUser m ~ OAuthUser m      -- Type equality!
     , AuthBackendUserId m ~ OAuthUserId m  -- Type equality!
     )
  => LoginRequest
  -> m (Headers '[Header "Location" RedirectTarget, Header "Set-Cookie" SessionCookie] NoContent)
handleLogin req = do
  -- validateCredentials returns (userId, user)
  result <- validateCredentials (loginUsername req) (loginPassword req)
  case result of
    Nothing -> throwError authenticationFailed
    Just (userId, _user) -> do
      -- Store auth code with userId (type matches OAuthUserId m)
      let code = AuthorizationCode
            { authCodeUserId = userId  -- Type-safe: OAuthUserId m
            , ...
            }
      storeAuthCode codeId code
      ...

-- Handler that generates JWT needs ToJWT on user type
handleToken
  :: ( OAuthStateStore m
     , MonadIO m
     , ToJWT (OAuthUser m)  -- JWT constraint at operation level
     )
  => TokenRequest
  -> m TokenResponse
handleToken req = do
  code <- lookupAuthCode (tokenCode req)
  -- Generate JWT with user info
  let user = ...  -- Reconstruct or lookup user
  jwt <- liftIO $ makeJWT user jwtSettings Nothing
  ...
```

#### Step 6: Update Test Infrastructure

```haskell
-- In MCP.Server.OAuth.Test.Internal
-- TestConfig now implicitly carries user types through m
data TestConfig m = TestConfig
  { tcMakeApp       :: IO (Application, NominalDiffTime -> IO ())
  , tcRunM          :: forall a. m a -> IO a
  , tcCredentials   :: TestCredentials
  }

-- Conformance spec requires type equality
oauthConformanceSpec
  :: forall m.
     ( OAuthStateStore m
     , AuthBackend m
     , MonadTime m
     , AuthBackendUser m ~ OAuthUser m
     , AuthBackendUserId m ~ OAuthUserId m
     , ToJWT (OAuthUser m)
     )
  => TestConfig m
  -> Spec
```

### Migration Order

| Order | Change | Complexity | Dependencies |
|-------|--------|------------|--------------|
| 1 | Add associated types to OAuthStateStore | Medium | None |
| 2 | Add associated types to AuthBackend | Medium | None |
| 3 | Change validateCredentials signature | Medium | Step 2 |
| 4 | Parameterize AuthorizationCode | Low | None |
| 5 | Update InMemory implementation | Medium | Steps 1, 4 |
| 6 | Update Demo implementation | Medium | Steps 2, 3 |
| 7 | Add type equality constraints to handlers | High | Steps 1-6 |
| 8 | Update test infrastructure | Medium | Step 7 |

### Files to Modify

| File | Change |
|------|--------|
| `src/MCP/Server/OAuth/Store.hs` | Add `OAuthUser m`, `OAuthUserId m` associated types |
| `src/MCP/Server/Auth/Backend.hs` | Add `AuthBackendUser m`, `AuthBackendUserId m`; change validateCredentials |
| `src/MCP/Server/OAuth/Types.hs` | Parameterize `AuthorizationCode userId` |
| `src/MCP/Server/OAuth/InMemory.hs` | Define associated types for reference impl |
| `src/MCP/Server/Auth/Demo.hs` | Define associated types; update validateCredentials |
| `src/MCP/Server/OAuth/Server.hs` | Add type equality constraints to oauthServer |
| `src/MCP/Server/OAuth/Handlers/*.hs` | Add constraints where needed |
| `src/MCP/Server/OAuth/Test/Internal.hs` | Add type equality constraints to conformance spec |
| `test/Laws/AuthBackendSpec.hs` | Test new validateCredentials signature |

### Property Tests for User Types

```haskell
-- Round-trip law still holds with parameterized types
prop "AuthorizationCode userId round-trip" $
  \(code :: AuthorizationCode UserId) ->
    lookupAuthCode (authCodeId code) `shouldReturn` Just code
    -- after: storeAuthCode (authCodeId code) code

-- validateCredentials law
prop "valid credentials return (userId, user)" $
  \(username, password) -> monadicIO $ do
    result <- run $ validateCredentials username password
    case result of
      Just (userId, user) -> do
        -- userId and user should be consistent
        assert (authUserId user == userId)
      Nothing -> return ()  -- Invalid credentials OK
```

### Constitution Compliance (Phase 8)

| Principle | Status | Evidence |
|-----------|--------|----------|
| I. Type-Driven Design | ✅ | Associated types + equality constraints encode user shape at type level; illegal combinations rejected at compile time |
| II. Deep Module Architecture | ✅ | User types hidden in implementations; handlers use abstract associated types |
| III. Denotational Semantics | ✅ | Type equality `~` has precise mathematical meaning; validateCredentials returns exactly what callers need |
| IV. Total Functions | ✅ | `Maybe (UserId, User)` is total; no partial extraction needed |
| V. Pure Core, Impure Shell | ✅ | User types are pure data; IO only in implementations |
| VI. Property-Based Testing | ✅ | Property tests verify user type consistency laws |

**Gate Status**: PASS - Phase 8 planning complete.

---

## Phase 9: Reference Implementation Type Colocation (FR-042)

**Added**: 2025-12-15 | **Status**: Planning | **Spec Session**: 2025-12-15

### Problem Statement

Reference implementation concrete types (`AuthUser`, `UserId`, `OAuthStoreError`, `DemoAuthError`) were initially placed in shared modules or near the typeclass definitions. This creates confusion about:

1. Which types belong to which implementation
2. Whether these types are "special" or "default"
3. Import hygiene — polymorphic handler code accidentally importing concrete types

**Spec Clarification** (2025-12-15): Reference implementation types must be colocated with their implementations and must NOT be imported by modules implementing against the polymorphic interface.

### Design Principle

```
┌─────────────────────────────────────────────────────────────────┐
│                    POLYMORPHIC LAYER                             │
│  Handlers, Server, Test harness                                  │
│  ONLY import: OAuthStateStore, AuthBackend, associated types     │
│  NEVER import: AuthUser, UserId, OAuthStoreError, DemoAuthError  │
└─────────────────────────────────────────────────────────────────┘
                              │
         Uses associated types (OAuthUser m, OAuthUserId m, etc.)
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                  IMPLEMENTATION LAYER                            │
│  MCP.Server.Auth.Demo       │  MCP.Server.OAuth.InMemory        │
│  ├── AuthUser               │  ├── OAuthStoreError              │
│  ├── UserId                 │  ├── OAuthTVarEnv                 │
│  └── DemoAuthError          │  └── TVar-based instance          │
│  └── Demo AuthBackend inst  │                                   │
└─────────────────────────────────────────────────────────────────┘
                              │
              Monomorphization at application boundary
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                  APPLICATION BOUNDARY                            │
│  Main.hs, HTTP.hs (AppM), Test fixtures                         │
│  ONLY place that imports concrete implementation types          │
│  Wires up: AppEnv with OAuthTVarEnv + DemoCredentialEnv         │
└─────────────────────────────────────────────────────────────────┘
```

### Clarified Decision

| Aspect | Choice | Rationale |
|--------|--------|-----------|
| AuthUser location | `MCP.Server.Auth.Demo` | Colocated with demo AuthBackend instance |
| UserId location | `MCP.Server.Auth.Demo` | Used by demo implementation |
| DemoAuthError location | `MCP.Server.Auth.Demo` | Error type for demo backend |
| OAuthStoreError location | `MCP.Server.OAuth.InMemory` | Error type for TVar store |
| Import prohibition | Polymorphic code never imports these | Enforces clean abstraction boundary |

### Implementation Approach

#### Step 1: Audit Current Type Locations

Current state (before refactoring):
```text
src/MCP/Server/
├── Auth.hs              # May contain AuthUser
├── Auth/
│   ├── Backend.hs       # AuthBackend typeclass
│   └── Demo.hs          # Demo implementation
├── OAuth/
│   ├── Types.hs         # May contain AuthUser incorrectly
│   ├── Store.hs         # OAuthStateStore typeclass
│   └── InMemory.hs      # TVar implementation
└── HTTP.hs              # May import concrete types
```

#### Step 2: Move Types to Implementation Modules

**MCP.Server.Auth.Demo** should define:
```haskell
module MCP.Server.Auth.Demo
  ( -- * Demo AuthBackend instance (implicit via module import)
    -- * Demo Types (exported for application composition only)
    AuthUser(..)
  , UserId
  , DemoAuthError(..)
  , DemoCredentialEnv(..)
    -- * Smart constructors
  , mkDemoCredentialEnv
  , defaultDemoCredentials
  ) where

-- | User type for reference demo implementation.
-- NOT a "default" — custom implementations define their own.
data AuthUser = AuthUser
  { authUserId   :: UserId
  , authUsername :: Username
  , authEmail    :: Maybe Text
  }
  deriving (Eq, Show, Generic)

-- ToJWT instance stays here (not in OAuth.Types)
instance ToJWT AuthUser where
  encodeJWT user = ...

-- | User identifier for reference implementation.
type UserId = Text  -- Or: newtype UserId = UserId Text

-- | Errors specific to demo credential backend.
data DemoAuthError
  = DemoCredentialsNotFound
  | DemoPasswordMismatch
  deriving (Eq, Show)
```

**MCP.Server.OAuth.InMemory** should define:
```haskell
module MCP.Server.OAuth.InMemory
  ( -- * In-memory OAuthStateStore instance (implicit)
    -- * Implementation types (exported for application composition only)
    OAuthStoreError(..)
  , OAuthTVarEnv(..)
    -- * Smart constructors
  , mkOAuthTVarEnv
  , newOAuthTVarEnv
  ) where

-- | Errors specific to TVar-based OAuth store.
data OAuthStoreError
  = StoreNotFound Text
  | StoreDuplicateKey Text
  | StoreExpired Text
  deriving (Eq, Show)
```

#### Step 3: Update Module Exports

**MCP.Server.Auth** (re-export module) should NOT re-export concrete types:
```haskell
module MCP.Server.Auth
  ( -- * Typeclass (from Backend)
    AuthBackend(..)
  , Username(..)
  , PlaintextPassword(..)
    -- * NO re-exports of AuthUser, UserId, DemoAuthError
  ) where

import MCP.Server.Auth.Backend
-- NOT: import MCP.Server.Auth.Demo (AuthUser, UserId, ...)
```

#### Step 4: Update Handler Imports

**Before** (problematic):
```haskell
-- In OAuth/Handlers/Login.hs
module MCP.Server.OAuth.Handlers.Login where

import MCP.Server.Auth.Demo (AuthUser(..))  -- WRONG: concrete type in polymorphic code
import MCP.Server.OAuth.Store

handleLogin :: ... -> m ...
handleLogin = do
  let user = AuthUser { ... }  -- WRONG: hardcoded to demo type
```

**After** (correct):
```haskell
-- In OAuth/Handlers/Login.hs
module MCP.Server.OAuth.Handlers.Login where

import MCP.Server.Auth.Backend (AuthBackend(..))
import MCP.Server.OAuth.Store (OAuthStateStore(..))
-- NO import of AuthUser, UserId, etc.

handleLogin
  :: ( AuthBackend m
     , OAuthStateStore m
     , AuthBackendUser m ~ OAuthUser m
     , AuthBackendUserId m ~ OAuthUserId m
     )
  => ... -> m ...
handleLogin = do
  result <- validateCredentials username password
  case result of
    Just (userId, _user) -> do
      -- userId is OAuthUserId m (abstract), not concrete UserId
      let code = AuthorizationCode { authCodeUserId = userId, ... }
      ...
```

#### Step 5: Update Application Composition

**Main.hs** or **HTTP.hs** is the ONLY place that wires concrete types:
```haskell
-- In app/Main.hs or src/MCP/Server/HTTP.hs
module Main where

-- Application boundary: OK to import concrete types
import MCP.Server.Auth.Demo (AuthUser, DemoCredentialEnv, mkDemoCredentialEnv)
import MCP.Server.OAuth.InMemory (OAuthStoreError, OAuthTVarEnv, newOAuthTVarEnv)
import MCP.Server.OAuth.Server (oauthServer)

-- Composite environment wires concrete implementations
data AppEnv = AppEnv
  { envOAuth  :: OAuthTVarEnv       -- Concrete: from InMemory
  , envAuth   :: DemoCredentialEnv  -- Concrete: from Demo
  , envConfig :: HTTPServerConfig
  }

-- AppM instantiates type variables to concrete types
type AppM = ReaderT AppEnv (ExceptT AppError IO)

-- At this boundary, OAuthUser AppM = AuthUser, OAuthUserId AppM = UserId
main :: IO ()
main = do
  oauthEnv <- newOAuthTVarEnv
  authEnv <- mkDemoCredentialEnv defaultDemoCredentials
  let env = AppEnv oauthEnv authEnv defaultConfig
  -- hoistServer monomorphizes polymorphic handlers to AppM
  runServer (mkApplication env)
```

### Verification Checklist

Before marking Phase 9 complete:

- [ ] `AuthUser` defined ONLY in `MCP.Server.Auth.Demo`
- [ ] `UserId` defined ONLY in `MCP.Server.Auth.Demo`
- [ ] `DemoAuthError` defined ONLY in `MCP.Server.Auth.Demo`
- [ ] `OAuthStoreError` defined ONLY in `MCP.Server.OAuth.InMemory`
- [ ] `MCP.Server.Auth` does NOT re-export `AuthUser`, `UserId`, `DemoAuthError`
- [ ] `MCP.Server.OAuth.Store` does NOT re-export `OAuthStoreError`
- [ ] No handler module imports `MCP.Server.Auth.Demo`
- [ ] No handler module imports concrete types from `MCP.Server.OAuth.InMemory`
- [ ] Only `Main.hs`, `HTTP.hs`, and test fixtures import concrete types
- [ ] Build succeeds with no unused import warnings for concrete types in handlers

### Files to Modify

| File | Change |
|------|--------|
| `src/MCP/Server/Auth/Demo.hs` | Ensure `AuthUser`, `UserId`, `DemoAuthError` defined here |
| `src/MCP/Server/OAuth/InMemory.hs` | Ensure `OAuthStoreError` defined here |
| `src/MCP/Server/Auth.hs` | Remove any re-exports of concrete types |
| `src/MCP/Server/OAuth/Types.hs` | Remove `AuthUser` if present (it doesn't belong here) |
| `src/MCP/Server/OAuth/Handlers/*.hs` | Remove imports of concrete types |
| `src/MCP/Server/HTTP.hs` | Verify imports are at boundary only |
| `app/Main.hs` | Verify concrete type imports are correct |

### Constitution Compliance (Phase 9)

| Principle | Status | Evidence |
|-----------|--------|----------|
| I. Type-Driven Design | ✅ | Import restrictions enforced by module structure; polymorphic code cannot accidentally use concrete types |
| II. Deep Module Architecture | ✅ | Concrete types hidden in implementation modules; minimal exports at abstraction boundary |
| III. Denotational Semantics | ✅ | Clear semantic distinction: "reference implementation type" vs "associated type" |
| IV. Total Functions | N/A | No function changes |
| V. Pure Core, Impure Shell | ✅ | Polymorphic handlers are pure over abstract types; concrete wiring at application shell |
| VI. Property-Based Testing | ✅ | Test fixtures can import concrete types; test specs remain polymorphic |

**Gate Status**: PASS - Phase 9 planning complete.

---

## Phase 10: Error Architecture Refinement (FR-043 through FR-045)

**Added**: 2025-12-15 | **Status**: Planning | **Spec Session**: 2025-12-15

### Problem Statement

The original error handling design hardcoded `AppError` in handler `MonadError` constraints and included `ServerErr ServerError` as a constructor. This creates several issues:

1. **Framework coupling**: Handlers depend on Servant's `ServerError` type
2. **Imprecise HTTP status mapping**: Generic error types don't map precisely to OAuth RFC 6749 error responses
3. **Security concerns**: Associated type errors might leak infrastructure details (connection strings, table names)
4. **Reusability blocked**: Handlers can't be used in non-Servant contexts (Yesod, CLI)

**Spec Refinement** (2025-12-15): Handlers use `AsType` constraints for four domain error types only. `ServerError` appears ONLY in the natural transformation at the Servant boundary.

### Clarified Decisions (from spec session 2025-12-15)

| Decision | Choice | Rationale |
|----------|--------|-----------|
| ServerError in handlers | PROHIBITED | Handlers are framework-agnostic; ServerError is Servant-specific |
| Domain error types | Four distinct types | Precise HTTP status mapping per OAuth RFC 6749 |
| Associated vs fixed | OAuthStateError m, AuthBackendError m (associated); ValidationError, AuthorizationError (fixed) | Storage/auth errors vary by implementation; protocol errors are fixed |
| Error exposure policy | Associated types logged not exposed; fixed types safe to expose | Security: prevent infrastructure detail leakage |
| Boundary function | `domainErrorToServerError` in Boundary module | Reusable translation with logging for sensitive errors |
| Boundary signature | `MonadIO m` + tracer parameter | Function logs internally for security; returns `Maybe ServerError` |
| Reference AppError | Four constructors, no ServerErr | Natural transformation is total for this type |

### New Requirements (FR-043 through FR-045)

- **FR-043**: `AuthorizationError` type in `OAuth.Types` with RFC 6749 constructors
- **FR-044**: `ValidationError` type in `OAuth.Types` for semantic validation failures
- **FR-045**: `domainErrorToServerError` function with `MonadIO m` and tracer for secure logging

### Error Type Taxonomy

```
┌─────────────────────────────────────────────────────────────────────────┐
│                       DOMAIN ERROR TYPES                                 │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│  ASSOCIATED TYPES (implementation-specific)                             │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │ OAuthStateError m                                                │   │
│  │ - TVar errors, PostgreSQL errors, Redis errors, etc.            │   │
│  │ - HTTP 500 (generic "Internal Server Error")                    │   │
│  │ - LOGGED internally, NEVER exposed to clients                   │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │ AuthBackendError m                                               │   │
│  │ - LDAP errors, database errors, account locked, etc.            │   │
│  │ - HTTP 401 (generic "Authentication failed")                    │   │
│  │ - LOGGED internally, NEVER exposed to clients                   │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                                                         │
│  FIXED TYPES (protocol-defined, safe to expose)                         │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │ ValidationError                                                  │   │
│  │ - Semantic validation failures (mismatched redirect_uri, etc.)  │   │
│  │ - HTTP 400 Bad Request                                          │   │
│  │ - Safe to expose (no infrastructure secrets)                    │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │ AuthorizationError                                               │   │
│  │ - RFC 6749 error codes: InvalidClient, InvalidGrant, etc.       │   │
│  │ - HTTP 4xx per OAuth spec                                       │   │
│  │ - Safe to expose (protocol-defined responses)                   │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                                                         │
└─────────────────────────────────────────────────────────────────────────┘
                              │
                              │ AsType prisms
                              ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                    COMPOSITE ERROR TYPE (e)                              │
│  Consumer defines: data AppError = OAuthStateErr ... | AuthBackendErr   │
│                                  | ValidationErr ... | AuthorizationErr │
│  NO ServerErr constructor                                               │
└─────────────────────────────────────────────────────────────────────────┘
                              │
                              │ domainErrorToServerError (MonadIO, tracer)
                              ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                    FRAMEWORK BOUNDARY                                    │
│  Servant: hoistServer with natural transformation                       │
│  Maybe ServerError → Handler/ServerError                                │
│  Nothing → fallback 500                                                 │
└─────────────────────────────────────────────────────────────────────────┘
```

### Implementation Approach

#### Step 1: Define AuthorizationError in OAuth.Types

```haskell
-- | OAuth 2.0 authorization errors per RFC 6749 Section 4.1.2.1 and 5.2.
-- Fixed type (protocol-defined), NOT an associated type.
-- Safe to expose to clients in OAuth error response format.
data AuthorizationError
  = InvalidRequest Text           -- 400: Missing/invalid parameter
  | InvalidClient Text            -- 401: Client authentication failed
  | InvalidGrant Text             -- 400: Invalid authorization code/refresh token
  | UnauthorizedClient Text       -- 403: Client not authorized for grant type
  | UnsupportedGrantType Text     -- 400: Grant type not supported
  | InvalidScope Text             -- 400: Invalid/unknown/malformed scope
  | AccessDenied Text             -- 403: User denied authorization
  | ExpiredCode                   -- 400: Authorization code expired
  | InvalidRedirectUri Text       -- 400: Redirect URI mismatch
  | PKCEVerificationFailed        -- 400: Code verifier doesn't match challenge
  deriving (Eq, Show, Generic)

-- | Map AuthorizationError to HTTP status and OAuth error response
authorizationErrorToResponse :: AuthorizationError -> (Status, OAuthErrorResponse)
authorizationErrorToResponse = \case
  InvalidRequest msg -> (status400, OAuthErrorResponse "invalid_request" (Just msg))
  InvalidClient msg  -> (status401, OAuthErrorResponse "invalid_client" (Just msg))
  InvalidGrant msg   -> (status400, OAuthErrorResponse "invalid_grant" (Just msg))
  -- ... etc per RFC 6749
```

#### Step 2: Define ValidationError in OAuth.Types

```haskell
-- | Semantic validation errors at handler level.
-- Distinct from parse-time 400s (FromJSON/FromHttpApiData).
-- Fixed type, safe to expose.
data ValidationError
  = RedirectUriMismatch ClientId RedirectUri  -- redirect_uri doesn't match registered
  | UnsupportedResponseType Text              -- response_type not supported by client
  | ClientNotRegistered ClientId              -- client_id not found
  | MissingRequiredScope Scope                -- required scope not requested
  | InvalidStateParameter Text                -- state parameter validation failed
  deriving (Eq, Show, Generic)

-- | Map ValidationError to HTTP 400 response
validationErrorToResponse :: ValidationError -> (Status, Text)
validationErrorToResponse = \case
  RedirectUriMismatch cid uri -> (status400, "redirect_uri does not match registered URIs for client " <> unClientId cid)
  -- ... etc
```

#### Step 3: Define domainErrorToServerError in OAuth.Boundary

```haskell
module MCP.Server.OAuth.Boundary
  ( domainErrorToServerError
  , OAuthBoundaryTrace(..)
  ) where

import Control.Lens (preview)
import Data.Generics.Product (AsType(..))
import Servant.Server (ServerError, err400, err401, err500)

-- | Trace events for boundary error translation
data OAuthBoundaryTrace
  = TraceStorageError Text    -- Logged, not exposed
  | TraceAuthError Text       -- Logged, not exposed
  | TraceValidationError Text -- May be logged for debugging
  | TraceAuthzError Text      -- May be logged for debugging
  deriving (Show)

-- | Translate domain errors to Servant ServerError.
--
-- Security policy:
--   - OAuthStateError m: LOGGED via tracer, returns generic 500
--   - AuthBackendError m: LOGGED via tracer, returns generic 401
--   - ValidationError: Safe to expose, returns 400 with details
--   - AuthorizationError: Safe to expose, returns appropriate 4xx per RFC 6749
--
-- Returns Nothing if no prism matches (consumer handles fallback).
domainErrorToServerError
  :: forall m m' e trace.
     ( MonadIO m
     , AsType (OAuthStateError m') e
     , AsType (AuthBackendError m') e
     , AsType ValidationError e
     , AsType AuthorizationError e
     )
  => IOTracer trace
  -> (OAuthBoundaryTrace -> trace)  -- Embed boundary trace in consumer's trace type
  -> e
  -> m (Maybe ServerError)
domainErrorToServerError tracer embedTrace err = do
  -- Try each prism in priority order

  -- 1. Storage errors: log and return generic 500
  case preview (typed @(OAuthStateError m')) err of
    Just storeErr -> do
      liftIO $ traceIO tracer $ embedTrace $ TraceStorageError (show storeErr)
      return $ Just err500 { errBody = "Internal Server Error" }
    Nothing -> pure ()

  -- 2. Auth backend errors: log and return generic 401
  case preview (typed @(AuthBackendError m')) err of
    Just authErr -> do
      liftIO $ traceIO tracer $ embedTrace $ TraceAuthError (show authErr)
      return $ Just err401 { errBody = "Authentication failed" }
    Nothing -> pure ()

  -- 3. Validation errors: safe to expose
  case preview (typed @ValidationError) err of
    Just valErr -> do
      let (status, msg) = validationErrorToResponse valErr
      return $ Just $ ServerError (statusCode status) (statusMessage status) msg []
    Nothing -> pure ()

  -- 4. Authorization errors: safe to expose per RFC 6749
  case preview (typed @AuthorizationError) err of
    Just authzErr -> do
      let (status, oauthResp) = authorizationErrorToResponse authzErr
      return $ Just $ ServerError (statusCode status) (statusMessage status) (encode oauthResp)
        [("Content-Type", "application/json")]
    Nothing -> pure ()

  -- No prism matched
  return Nothing
```

#### Step 4: Update Reference AppError (remove ServerErr)

```haskell
-- In MCP.Server.HTTP.AppEnv or similar
-- Updated 2025-12-15: Removed ServerErr per Phase 10

-- | Composite error type for reference implementation.
-- Four domain error constructors only (Phase 10).
data AppError
  = OAuthStateErr OAuthStoreError     -- From InMemory implementation
  | AuthBackendErr DemoAuthError      -- From Demo implementation
  | ValidationErr ValidationError     -- Fixed type from OAuth.Types
  | AuthorizationErr AuthorizationError  -- Fixed type from OAuth.Types
  deriving (Eq, Show, Generic)

-- AsType instances for generic-lens
instance AsType OAuthStoreError AppError where
  _Typed = _OAuthStateErr
instance AsType DemoAuthError AppError where
  _Typed = _AuthBackendErr
instance AsType ValidationError AppError where
  _Typed = _ValidationErr
instance AsType AuthorizationError AppError where
  _Typed = _AuthorizationErr
```

#### Step 5: Update Natural Transformation in HTTP.hs

```haskell
-- | Run polymorphic handlers in Servant's Handler monad.
-- Uses domainErrorToServerError for domain → HTTP translation.
runAppM :: AppEnv -> AppM a -> Handler a
runAppM env action = do
  result <- liftIO $ runExceptT $ runReaderT action env
  case result of
    Right a -> return a
    Left err -> do
      -- Use boundary function for secure error translation
      mServerErr <- liftIO $ domainErrorToServerError
        (envTracer env)
        HTTPBoundaryTrace  -- Embed in HTTP trace type
        err
      case mServerErr of
        Just serverErr -> throwError serverErr
        Nothing -> throwError err500 { errBody = "Unknown error" }  -- Fallback
```

### Updated Module Organization

```text
src/MCP/Server/OAuth/
├── Types.hs              # MODIFIED: Add AuthorizationError, ValidationError
├── Store.hs              # OAuthStateStore typeclass
├── InMemory.hs           # TVar-based implementation
├── Boundary.hs           # NEW: domainErrorToServerError, OAuthBoundaryTrace
├── Server.hs             # Polymorphic oauthServer (no ServerError constraints)
└── Handlers/             # Individual handlers (no ServerError constraints)

src/MCP/Server/HTTP/
├── AppEnv.hs             # MODIFIED: AppError without ServerErr
└── ...
```

### Files to Modify

| File | Change |
|------|--------|
| `src/MCP/Server/OAuth/Types.hs` | Add `AuthorizationError`, `ValidationError` types |
| `src/MCP/Server/OAuth/Boundary.hs` | NEW: `domainErrorToServerError` function |
| `src/MCP/Server/HTTP/AppEnv.hs` | Remove `ServerErr` from `AppError` |
| `src/MCP/Server/HTTP.hs` | Update `runAppM` to use `domainErrorToServerError` |
| `src/MCP/Server/OAuth/Server.hs` | Update constraints (already done in Phase 6 section) |
| `src/MCP/Server/OAuth/Handlers/*.hs` | Update error throwing to use domain types |

### Error Throwing Patterns

**Before** (hardcoded ServerError):
```haskell
handleToken req = do
  case lookupCode codeId of
    Nothing -> throwError err400 { errBody = "invalid_grant" }  -- WRONG
```

**After** (domain error types):
```haskell
handleToken
  :: (AsType AuthorizationError e, MonadError e m, ...)
  => TokenRequest -> m TokenResponse
handleToken req = do
  case lookupCode codeId of
    Nothing -> throwError $ injectTyped $ InvalidGrant "Authorization code not found"
```

### Verification Checklist

Before marking Phase 10 complete:

- [ ] `AuthorizationError` defined in `OAuth.Types` with all RFC 6749 constructors
- [ ] `ValidationError` defined in `OAuth.Types`
- [ ] `domainErrorToServerError` implemented in `OAuth.Boundary`
- [ ] `AppError` has exactly 4 constructors (no `ServerErr`)
- [ ] No handler imports `Servant.Server (ServerError)`
- [ ] No handler constraint includes `AsType ServerError e`
- [ ] `runAppM` uses `domainErrorToServerError` for translation
- [ ] Storage/auth errors are logged, not exposed (verify with test)
- [ ] Validation/authorization errors return correct HTTP status codes
- [ ] Build succeeds with no ServerError in handler modules

### Constitution Compliance (Phase 10)

| Principle | Status | Evidence |
|-----------|--------|----------|
| I. Type-Driven Design | ✅ | Four distinct error types encode semantics at type level; `AsType` constraints compose precisely |
| II. Deep Module Architecture | ✅ | Error translation isolated in Boundary module; handlers don't know about ServerError |
| III. Denotational Semantics | ✅ | Each error type has clear semantic meaning; mapping to HTTP status is deterministic |
| IV. Total Functions | ✅ | `domainErrorToServerError` returns `Maybe` (total); caller handles `Nothing` |
| V. Pure Core, Impure Shell | ✅ | Handlers throw pure domain errors; IO logging happens at boundary |
| VI. Property-Based Testing | ✅ | Can test error mapping: each domain error → expected HTTP status |

**Gate Status**: PASS - Phase 10 planning complete.

---

## Phase 11: Application Entry Point with Natural Transformation (FR-046)

**Spec Refinement** (2025-12-15): The `mcpApp` function (or equivalent application entry point) MUST accept a natural transformation `(∀ a. m a -> Handler a)` as a parameter, enabling callers to select typeclass implementations at application composition time.

### Rationale

The current implementation likely has a concrete `mcpApp :: IO Application` or similar that internally constructs the environment and wires up implementations. FR-046 requires exposing the polymorphic server to callers, with the natural transformation as the mechanism for selecting backends.

This follows standard Servant `hoistServer` patterns and enables:
1. **Backend selection at composition time**: Callers choose in-memory, PostgreSQL, LDAP, etc.
2. **Framework-agnostic handlers**: Same handlers work in Servant, Yesod, tests, CLI
3. **Testability**: Test harnesses provide test-specific nat-trans with controllable time, mock backends

### Design

#### Step 1: Signature Pattern

```haskell
-- | Polymorphic OAuth application entry point.
-- Callers provide the natural transformation that interprets handlers.
mcpApp
  :: forall m.
     ( OAuthStateStore m
     , AuthBackend m
     , MonadTime m
     , MonadIO m
     , OAuthUser m ~ AuthBackendUser m
     , OAuthUserId m ~ AuthBackendUserId m
     , ToJWT (OAuthUser m)
     , FromJWT (OAuthUser m)
     )
  => (forall a. m a -> Handler a)  -- Natural transformation
  -> Application
mcpApp runM = serve oauthAPI (hoistServer oauthAPI runM oauthServer)
```

**Key points**:
- Rank-2 type for natural transformation (`forall a.`)
- All typeclass constraints on `m` appear here (callers must satisfy them)
- `oauthServer` remains fully polymorphic
- `hoistServer` applies the nat-trans at this boundary only

#### Step 2: Reference Implementation Entry Point

```haskell
-- | Reference implementation using in-memory TVar storage and demo credentials.
-- This is what `cabal run mcp-http -- --oauth` uses.
demoMcpApp :: IO Application
demoMcpApp = do
  -- Initialize reference implementation environment
  env <- mkDemoAppEnv
  -- Construct natural transformation for reference implementation
  let runAppM :: forall a. AppM a -> Handler a
      runAppM action = do
        result <- liftIO $ runExceptT $ runReaderT (unAppM action) env
        case result of
          Right a -> pure a
          Left err -> do
            -- Use boundary function for domain → HTTP error translation
            mServerErr <- liftIO $ domainErrorToServerError (envTracer env) err
            throwError $ fromMaybe err500 mServerErr
  -- Apply to polymorphic mcpApp
  pure $ mcpApp runAppM
```

**Separation of concerns**:
- `mcpApp` is pure and polymorphic (library code)
- `demoMcpApp` does IO setup and provides the concrete nat-trans (application code)

#### Step 3: Alternative Entry Point with Context

For applications needing Servant's `Context` (e.g., JWT settings):

```haskell
-- | Polymorphic entry point with Servant Context for auth settings.
mcpAppWithContext
  :: forall m ctx.
     ( OAuthStateStore m, AuthBackend m, MonadTime m, MonadIO m
     , OAuthUser m ~ AuthBackendUser m, OAuthUserId m ~ AuthBackendUserId m
     , ToJWT (OAuthUser m), FromJWT (OAuthUser m)
     , HasContextEntry ctx JWTSettings
     , HasContextEntry ctx CookieSettings
     )
  => Context ctx
  -> (forall a. m a -> Handler a)
  -> Application
mcpAppWithContext ctx runM =
  serveWithContext oauthAPI ctx (hoistServerWithContext oauthAPI (Proxy @ctx) runM oauthServer)
```

#### Step 4: Test Harness Integration

The test harness's `makeTestApp` (FR-033) uses the same pattern:

```haskell
-- | Construct test Application with controllable time.
makeTestApp
  :: forall m.
     ( OAuthStateStore m, AuthBackend m, ... )
  => TestEnv m
  -> IO (Application, NominalDiffTime -> IO ())
makeTestApp testEnv = do
  -- Test-specific environment setup
  timeTVar <- newTVarIO testStartTime
  let testRunM :: forall a. m a -> Handler a
      testRunM = runTestM testEnv timeTVar
      advanceTime dt = atomically $ modifyTVar timeTVar (addUTCTime dt)
  pure (mcpApp testRunM, advanceTime)
```

This ensures the SAME `mcpApp` is used in both production and tests—only the nat-trans differs.

#### Step 5: Custom Backend Example

Third-party implementers use the same pattern:

```haskell
-- | PostgreSQL-backed OAuth application.
postgresMcpApp :: PostgresConfig -> IO Application
postgresMcpApp pgConfig = do
  pool <- createPool pgConfig
  env <- mkPostgresEnv pool
  let runPostgresM :: forall a. PostgresM a -> Handler a
      runPostgresM action = do
        result <- liftIO $ runPostgres pool (runExceptT $ runReaderT action env)
        case result of
          Right a -> pure a
          Left err -> do
            -- Custom error handling for PostgresM's error type
            mServerErr <- liftIO $ domainErrorToServerError (pgTracer env) err
            throwError $ fromMaybe err500 mServerErr
  pure $ mcpApp runPostgresM
```

### Module Organization Update (Phase 11 — Superseded by Phase 12)

```text
src/MCP/Server/
├── OAuth/
│   ├── App.hs           # Phase 11: mcpApp here; Phase 12: moved to HTTP.hs
│   ├── Server.hs        # oauthServer (polymorphic server, unchanged)
│   ├── Boundary.hs      # domainErrorToServerError (unchanged)
│   └── ...
├── HTTP.hs              # MODIFY: Import and re-export mcpApp; demoMcpApp here
└── ...
```

### Files to Modify

| File | Change |
|------|--------|
| `src/MCP/Server/OAuth/App.hs` | NEW: `mcpApp`, `mcpAppWithContext` |
| `src/MCP/Server/HTTP.hs` | MODIFY: Add `demoMcpApp`, import from OAuth.App |
| `app/Main.hs` | MODIFY: Use `demoMcpApp` (or custom nat-trans if configured) |
| `test/Spec/OAuth/...` | MODIFY: Use `mcpApp` with test nat-trans via `makeTestApp` |

### Migration Path

1. **Create `OAuth.App` module** with polymorphic `mcpApp`
2. **Extract nat-trans construction** from current monolithic setup into `demoMcpApp`
3. **Update `Main.hs`** to use `demoMcpApp` (behavior unchanged, structure improved)
4. **Update test harness** to use `mcpApp` directly with test-specific nat-trans
5. **Document pattern** for third-party implementers

### Verification Checklist

Before marking Phase 11 complete:

- [ ] `mcpApp :: (∀ a. m a -> Handler a) -> Application` exists and compiles
- [ ] `mcpApp` has all necessary typeclass constraints on `m`
- [ ] `demoMcpApp :: IO Application` uses `mcpApp` internally
- [ ] `cabal run mcp-http -- --oauth` still works (uses `demoMcpApp`)
- [ ] Test harness uses `mcpApp` with test nat-trans
- [ ] All existing OAuth tests pass
- [ ] Third-party example (mock PostgresM) compiles and type-checks

### Constitution Compliance (Phase 11)

| Principle | Status | Evidence |
|-----------|--------|----------|
| I. Type-Driven Design | ✅ | Polymorphic signature encodes all requirements as constraints; callers must satisfy them |
| II. Deep Module Architecture | ✅ | Single entry point hides server complexity; callers provide only the nat-trans |
| III. Denotational Semantics | ✅ | Clear semantics: nat-trans interprets `m` actions in `Handler` |
| IV. Total Functions | ✅ | Pure function; no partiality |
| V. Pure Core, Impure Shell | ✅ | `mcpApp` is pure; IO lives in `demoMcpApp` and caller's nat-trans construction |
| VI. Property-Based Testing | ✅ | Same `mcpApp` used in tests with different nat-trans enables property testing |

**Gate Status**: PASS - Phase 11 planning complete.

> **⚠️ Superseded**: Phase 11's module organization (`OAuth.App` with `mcpApp`) was revised
> in Phase 12 (2025-12-16 spec refinement). The `mcpApp` entry point now lives in
> `MCP.Server.HTTP` per FR-046, not in `OAuth.App`. Phase 12 also introduces the
> `Servant.OAuth2.IDP.*` namespace. See Phase 12 for current architecture.

---

## Phase 12: Namespace Migration & Entry Point Separation (Spec Refinement 2025-12-16)

**Trigger**: Clarification session revealed that:
1. MCP functionality was lost after implementing `mcpApp` in OAuth2 namespace (mcp-nyr.23)
2. OAuth2 modules need explicit package boundaries for future extraction
3. Two entry points needed to make OAuth2 optional via CLI flag

**Spec Requirements**:
- FR-046: `mcpApp` must be in `MCP.Server.HTTP`
- FR-047: Type-level route composition (`type FullAPI = MCPAPI :<|> OAuthAPI`)
- FR-048: Two entry points (`mcpApp`, `mcpAppWithOAuth`) — **Refined 2025-12-17: BOTH must accept natural transformation (see Phase 13)**
- FR-049: All OAuth2 modules relocated to `Servant.OAuth2.IDP.*` namespace

### Updated Module Structure (Phase 12)

```text
src/
├── MCP/
│   ├── Types.hs                    # Core MCP protocol types (unchanged)
│   ├── Protocol.hs                 # JSON-RPC message types (unchanged)
│   ├── Server.hs                   # MCPServer typeclass (unchanged)
│   └── Server/
│       ├── HTTP.hs                 # MODIFIED: mcpApp, mcpAppWithOAuth entry points
│       ├── StdIO.hs                # Unchanged (no OAuth)
│       └── Time.hs                 # MonadTime re-export (unchanged)
│
├── Servant/
│   └── OAuth2/
│       └── IDP/                    # NEW namespace (was MCP.Server.OAuth.* + MCP.Server.Auth.*)
│           ├── Types.hs            # AuthCodeId, ClientId, AuthorizationError, ValidationError, etc.
│           ├── Store.hs            # OAuthStateStore typeclass
│           ├── Store/
│           │   └── InMemory.hs     # TVar-based implementation + OAuthStoreError, OAuthTVarEnv
│           ├── Boundary.hs         # domainErrorToServerError, OAuthBoundaryTrace
│           ├── Server.hs           # oauthServer (polymorphic Servant server)
│           ├── API.hs              # OAuthAPI type definition
│           ├── Handlers.hs         # Polymorphic OAuth handlers (extracted from HTTP.hs)
│           ├── Auth/
│           │   ├── Backend.hs      # AuthBackend typeclass + Username, PlaintextPassword
│           │   └── Demo.hs         # Demo credentials + AuthUser, UserId, DemoAuthError
│           └── Test/
│               └── Internal.hs     # Polymorphic conformance test specs (FR-029)

test/
├── Main.hs                         # Modified: import from new namespace
├── Laws/
│   ├── OAuthStateStoreSpec.hs      # Updated imports
│   └── AuthBackendSpec.hs          # Updated imports
├── Generators.hs                   # Updated imports
└── TestMonad.hs                    # Unchanged
```

### Module Mapping (Old → New)

| Old Module | New Module |
|------------|------------|
| `MCP.Server.OAuth.Types` | `Servant.OAuth2.IDP.Types` |
| `MCP.Server.OAuth.Store` | `Servant.OAuth2.IDP.Store` |
| `MCP.Server.OAuth.InMemory` | `Servant.OAuth2.IDP.Store.InMemory` |
| `MCP.Server.OAuth.Boundary` | `Servant.OAuth2.IDP.Boundary` |
| `MCP.Server.OAuth.App` | REMOVED (merged into `MCP.Server.HTTP`) |
| `MCP.Server.Auth.Backend` | `Servant.OAuth2.IDP.Auth.Backend` |
| `MCP.Server.Auth.Demo` | `Servant.OAuth2.IDP.Auth.Demo` |
| `MCP.Server.OAuth.Test.Internal` | `Servant.OAuth2.IDP.Test.Internal` |
| NEW | `Servant.OAuth2.IDP.Server` (oauthServer) |
| NEW | `Servant.OAuth2.IDP.API` (OAuthAPI type) |
| NEW | `Servant.OAuth2.IDP.Handlers` (polymorphic handlers) |

### Entry Point Architecture

#### MCP.Server.HTTP

```haskell
module MCP.Server.HTTP
  ( -- * Entry Points
    mcpApp           -- MCP-only (no OAuth2 constraints)
  , mcpAppWithOAuth  -- MCP + OAuth2 (requires OAuthStateStore/AuthBackend)
    -- * Demo Entry Point
  , demoMcpApp       -- Convenience: mcpAppWithOAuth with in-memory backends
    -- * API Types
  , MCPAPI
  , FullAPI          -- MCPAPI :<|> OAuthAPI
  ) where

import Servant.OAuth2.IDP.API (OAuthAPI)
import Servant.OAuth2.IDP.Server (oauthServer)

-- | MCP-only application (no OAuth2).
-- Use when --oauth flag is NOT provided.
mcpApp
  :: (∀ a. m a -> Handler a)  -- ^ Natural transformation for MCP handlers
  -> Application
mcpApp runM = serve (Proxy @MCPAPI) $ hoistServer (Proxy @MCPAPI) runM mcpServer

-- | MCP + OAuth2 application.
-- Use when --oauth flag IS provided.
mcpAppWithOAuth
  :: ( OAuthStateStore m, AuthBackend m
     , AuthBackendUser m ~ OAuthUser m
     , AuthBackendUserId m ~ OAuthUserId m
     , MonadTime m, MonadIO m, MonadError e m, MonadReader env m
     , AsType (OAuthStateError m) e, AsType (AuthBackendError m) e
     , AsType ValidationError e, AsType AuthorizationError e
     , HasType (OAuthStateEnv m) env, HasType (AuthBackendEnv m) env
     , ToJWT (OAuthUser m)
     )
  => (∀ a. m a -> Handler a)  -- ^ Natural transformation
  -> Application
mcpAppWithOAuth runM = serve (Proxy @FullAPI) $ hoistServer (Proxy @FullAPI) runM fullServer
  where
    fullServer = mcpServer :<|> oauthServer

-- | Type-level route composition (FR-047)
type FullAPI = MCPAPI :<|> OAuthAPI
```

#### CLI Flag Selection (app/Main.hs)

```haskell
main :: IO ()
main = do
  opts <- parseOptions
  if optOAuth opts
    then do
      -- Initialize OAuth backends
      oauthEnv <- initOAuthTVarEnv
      authEnv <- initDemoCredentialEnv
      let runM = mkDemoRunM oauthEnv authEnv
      Warp.run (optPort opts) (mcpAppWithOAuth runM)
    else do
      -- MCP-only mode
      let runM = mkMcpOnlyRunM
      Warp.run (optPort opts) (mcpApp runM)
```

### Implementation Steps

#### Step 1: Create Servant.OAuth2.IDP Namespace

1. Create directory structure: `src/Servant/OAuth2/IDP/`
2. Move and rename modules per mapping table
3. Update all internal imports within moved modules
4. Add `Servant.OAuth2.IDP` to cabal `exposed-modules`

#### Step 2: Extract Handlers and API

1. Create `Servant.OAuth2.IDP.API` with `OAuthAPI` type definition
2. Create `Servant.OAuth2.IDP.Handlers` with polymorphic handler implementations
3. Create `Servant.OAuth2.IDP.Server` with `oauthServer`
4. These were previously interleaved in `MCP.Server.HTTP`

#### Step 3: Update MCP.Server.HTTP

1. Remove OAuth handler implementations (now in `Servant.OAuth2.IDP.Handlers`)
2. Import `OAuthAPI`, `oauthServer` from `Servant.OAuth2.IDP`
3. Add `mcpApp` (MCP-only entry point)
4. Rename existing polymorphic entry point to `mcpAppWithOAuth`
5. Add `FullAPI` type alias
6. Keep `demoMcpApp` as convenience wrapper

#### Step 4: Update app/Main.hs

1. Import both entry points
2. Add CLI flag check for `--oauth`
3. Call `mcpApp` or `mcpAppWithOAuth` based on flag

#### Step 5: Update Tests

1. Update all imports to use `Servant.OAuth2.IDP.*`
2. Update test harness to use appropriate entry point
3. Verify all tests pass

#### Step 6: Update Cabal File

1. Add new modules under `Servant.OAuth2.IDP.*`
2. Remove old `MCP.Server.OAuth.*` and `MCP.Server.Auth.*` module declarations
3. Ensure clean build

### Files to Create

| File | Purpose |
|------|---------|
| `src/Servant/OAuth2/IDP/Types.hs` | Relocated from `MCP.Server.OAuth.Types` |
| `src/Servant/OAuth2/IDP/Store.hs` | Relocated from `MCP.Server.OAuth.Store` |
| `src/Servant/OAuth2/IDP/Store/InMemory.hs` | Relocated from `MCP.Server.OAuth.InMemory` |
| `src/Servant/OAuth2/IDP/Boundary.hs` | Relocated from `MCP.Server.OAuth.Boundary` |
| `src/Servant/OAuth2/IDP/API.hs` | NEW: `OAuthAPI` type definition |
| `src/Servant/OAuth2/IDP/Server.hs` | NEW: `oauthServer` polymorphic server |
| `src/Servant/OAuth2/IDP/Handlers.hs` | NEW: Extracted OAuth handlers |
| `src/Servant/OAuth2/IDP/Auth/Backend.hs` | Relocated from `MCP.Server.Auth.Backend` |
| `src/Servant/OAuth2/IDP/Auth/Demo.hs` | Relocated from `MCP.Server.Auth.Demo` |
| `src/Servant/OAuth2/IDP/Test/Internal.hs` | Relocated test specs |

### Files to Modify

| File | Change |
|------|--------|
| `src/MCP/Server/HTTP.hs` | Add `mcpApp`, rename to `mcpAppWithOAuth`, import from new namespace |
| `app/Main.hs` | CLI flag selection between entry points |
| `mcp-haskell.cabal` | Update exposed-modules |
| `test/Main.hs` | Update imports |
| `test/Laws/*.hs` | Update imports |
| `test/Generators.hs` | Update imports |

### Files to Delete

| File | Reason |
|------|--------|
| `src/MCP/Server/OAuth/App.hs` | Merged into `MCP.Server.HTTP` |
| `src/MCP/Server/OAuth/*.hs` | Relocated to `Servant.OAuth2.IDP.*` |
| `src/MCP/Server/Auth/*.hs` | Relocated to `Servant.OAuth2.IDP.Auth.*` |

### Verification Checklist

Before marking Phase 12 complete:

- [ ] All modules relocated to `Servant.OAuth2.IDP.*` namespace
- [ ] `mcpApp` entry point exists in `MCP.Server.HTTP` (MCP-only)
- [ ] `mcpAppWithOAuth` entry point exists in `MCP.Server.HTTP` (MCP + OAuth2)
- [ ] `type FullAPI = MCPAPI :<|> OAuthAPI` defined
- [ ] `cabal run mcp-http` works (MCP-only mode)
- [ ] `cabal run mcp-http -- --oauth` works (full mode)
- [ ] All existing OAuth tests pass with updated imports
- [ ] No `MCP.Server.OAuth.*` or `MCP.Server.Auth.*` modules remain
- [ ] `Servant.OAuth2.IDP.*` namespace appears clean for future package extraction
- [ ] CLAUDE.md updated with new module paths

### Constitution Compliance (Phase 12)

| Principle | Status | Evidence |
|-----------|--------|----------|
| I. Type-Driven Design | ✅ | Two entry points with different type signatures encode OAuth optionality at type level |
| II. Deep Module Architecture | ✅ | `Servant.OAuth2.IDP` is self-contained; MCP imports only what it needs |
| III. Denotational Semantics | ✅ | Clear semantics: `mcpApp` = MCP only, `mcpAppWithOAuth` = full functionality |
| IV. Total Functions | ✅ | No new partiality introduced |
| V. Pure Core, Impure Shell | ✅ | Namespace reorganization doesn't change purity boundaries |
| VI. Property-Based Testing | ✅ | Test infrastructure relocated, not changed |

**Gate Status**: PASS - Phase 12 planning complete.

### Benefits of This Refactoring

1. **MCP Independence**: `mcpApp` works without any OAuth2 dependencies
2. **Explicit Boundaries**: `Servant.OAuth2.IDP.*` namespace makes package extraction trivial
3. **Self-Documenting**: Entry point names clearly indicate capability (`mcpApp` vs `mcpAppWithOAuth`)
4. **Type Safety**: Attempting to use OAuth features with `mcpApp` is a compile error
5. **Future-Proof**: OAuth2 package extraction is "cut along the dotted line"

---

## Phase 13: Natural Transformation for ALL Entry Points (Spec Refinement 2025-12-17)

**Trigger**: Clarification session 2025-12-17 explicitly confirmed that ALL `mcpApp*` functions in `MCP.Server.HTTP` MUST accept a `runAppM` natural transformation parameter, enabling callers to choose the monad stack implementation for any entry point.

**Spec Requirement (FR-048 Refined)**:
> **Both functions MUST accept a `(∀ a. m a -> Handler a)` natural transformation parameter**, following the same pattern as FR-046. Signature pattern for `mcpAppWithOAuth`: `mcpAppWithOAuth :: (OAuthStateStore m, AuthBackend m, ...) => (∀ a. m a -> Handler a) -> Application`.

### Rationale

The natural transformation pattern provides maximum flexibility:

1. **Callers control the monad stack**: Production can wire PostgreSQL + LDAP, tests can wire in-memory + mock auth, CLI tools can wire pure monads
2. **Consistent interface**: All entry points follow the same `(∀ a. m a -> Handler a) -> Application` pattern
3. **Type safety**: Constraints on `m` differ per entry point, but the interface is uniform
4. **Standard Servant pattern**: Follows established `hoistServer` idiom

### Updated Signatures (Final)

```haskell
-- MCP.Server.HTTP

-- | MCP-only application (no OAuth2 dependencies).
-- Callers provide natural transformation for MCP handlers.
mcpApp
  :: (∀ a. m a -> Handler a)  -- ^ Natural transformation
  -> Application

-- | MCP + OAuth2 application (full functionality).
-- Callers provide natural transformation that satisfies all constraints.
mcpAppWithOAuth
  :: ( OAuthStateStore m, AuthBackend m
     , AuthBackendUser m ~ OAuthUser m
     , AuthBackendUserId m ~ OAuthUserId m
     , MonadTime m, MonadIO m, MonadError e m, MonadReader env m
     , AsType (OAuthStateError m) e, AsType (AuthBackendError m) e
     , AsType ValidationError e, AsType AuthorizationError e
     , HasType (OAuthStateEnv m) env, HasType (AuthBackendEnv m) env
     , ToJWT (OAuthUser m)
     )
  => (∀ a. m a -> Handler a)  -- ^ Natural transformation
  -> Application

-- | Demo/development convenience wrapper.
-- Uses in-memory OAuthStateStore and demo AuthBackend.
-- Returns IO Application since it initializes internal state.
demoMcpApp :: IO Application
demoMcpApp = do
  oauthEnv <- initOAuthTVarEnv
  authEnv <- initDemoCredentialEnv
  let runM = mkDemoRunM oauthEnv authEnv
  pure (mcpAppWithOAuth runM)
```

### Key Design Points

| Aspect | Decision |
|--------|----------|
| Parameter position | Natural transformation is FIRST parameter (before any config) |
| Quantification | Rank-2 type `(∀ a. m a -> Handler a)` ensures proper polymorphism |
| Constraints | Only appear on the type variable `m`, not on the function |
| Demo wrapper | `demoMcpApp :: IO Application` is convenience, NOT the primary API |

### Implementation Checklist

Phase 12 already planned `mcpAppWithOAuth` to accept natural transformation. This phase confirms:

- [x] `mcpApp` accepts `(∀ a. m a -> Handler a)` (was already planned in Phase 12)
- [x] `mcpAppWithOAuth` accepts `(∀ a. m a -> Handler a)` (was already planned in Phase 12)
- [ ] **Verify**: No `mcpApp*` function has a hardcoded monad stack
- [ ] **Verify**: All constraints are on `m`, not baked into the function body
- [ ] **Verify**: `demoMcpApp` is a convenience wrapper, not a different interface pattern

### Usage Examples

#### Production with PostgreSQL + LDAP

```haskell
main :: IO ()
main = do
  pgPool <- initPostgresPool
  ldapConfig <- loadLdapConfig
  let appEnv = AppEnv pgPool ldapConfig
  let runM :: ∀ a. AppM a -> Handler a
      runM action = do
        result <- liftIO $ runReaderT (runExceptT action) appEnv
        case result of
          Left err -> throwError (domainToServerError err)
          Right a -> pure a
  Warp.run 8080 (mcpAppWithOAuth runM)
```

#### Testing with In-Memory + Controlled Time

```haskell
spec :: Spec
spec = with makeTestApp $ do
  it "issues authorization code" $ do
    -- test using WaiSession
  where
    makeTestApp = do
      timeVar <- newTVarIO someFixedTime
      oauthEnv <- initTestOAuthEnv timeVar
      authEnv <- initTestAuthEnv
      let runM :: ∀ a. TestM a -> Handler a
          runM = runTestM timeVar oauthEnv authEnv
      pure (mcpAppWithOAuth runM)
```

#### MCP-Only Mode (No OAuth)

```haskell
main :: IO ()
main = do
  let runM :: ∀ a. McpOnlyM a -> Handler a
      runM action = liftIO (runMcpOnlyM action)
  Warp.run 8080 (mcpApp runM)
```

### Files Affected

This phase is a refinement/confirmation of Phase 12, not new implementation. Files already planned for modification in Phase 12:

| File | Verification |
|------|--------------|
| `src/MCP/Server/HTTP.hs` | Confirm both entry points accept natural transformation |
| `app/Main.hs` | Confirm callers construct and pass natural transformation |
| Test files | Confirm test harness uses natural transformation pattern |

### Constitution Compliance (Phase 13)

| Principle | Status | Evidence |
|-----------|--------|----------|
| I. Type-Driven Design | ✅ | Natural transformation parameter encodes monad stack flexibility at type level |
| II. Deep Module Architecture | ✅ | Entry point API is minimal; all complexity in caller's natural transformation |
| III. Denotational Semantics | ✅ | Clear semantic: `(∀ a. m a -> Handler a)` = "how to run your monad as Servant Handler" |
| IV. Total Functions | ✅ | No partiality; natural transformation is caller's responsibility |
| V. Pure Core, Impure Shell | ✅ | Entry points are pure functions; IO in caller's transformation |
| VI. Property-Based Testing | ✅ | Same test infrastructure; natural transformation enables test monad injection |

**Gate Status**: PASS - Phase 13 confirms and documents the natural transformation requirement for all entry points.

### Relationship to Other Phases

- **Phase 11**: Introduced natural transformation for `mcpApp` (FR-046)
- **Phase 12**: Extended pattern to `mcpAppWithOAuth` and planned implementation
- **Phase 13**: Confirms ALL `mcpApp*` functions follow this pattern (FR-048 refined)

---

## Phase 14: Remove AuthBackendUserId Associated Type (Spec Refinement 2025-12-17)

**Added**: 2025-12-17 | **Status**: Planning | **Spec Session**: 2025-12-17

### Problem Statement

The `AuthBackendUserId m` associated type in `AuthBackend` and `OAuthUserId m` in `OAuthStateStore` are **unused** in the current implementation:

1. `validateCredentials` returns `Maybe (AuthBackendUserId m, AuthBackendUser m)`, but handlers **discard the userId** and only use the full user
2. `AuthorizationCode` stores `OAuthUser m` (full user), not a user ID — per Session 2025-12-17 clarification
3. The type equality constraint `AuthBackendUserId m ~ OAuthUserId m` serves no purpose without actual userId storage

**Design Goal**: Keep the API lean. Unused associated types add implementation burden (every instance must define them) without benefit.

### Clarified Decision (from spec session 2025-12-17)

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Remove `AuthBackendUserId m` | Yes | Unused — handlers discard it |
| Remove `OAuthUserId m` | Yes | Unused — auth codes store full user per earlier clarification |
| User ID storage | Field in `AuthBackendUser m` | Implementations embed user ID in their user type |
| JWT encoding | Via `ToJWT` instance | User type controls its own JWT claims including any user ID |

### Spec Updates (FR-001, FR-002, FR-039, FR-040, FR-042)

- **FR-001**: `OAuthStateStore` defines `OAuthUser m` only (removed `OAuthUserId m`)
- **FR-002**: `validateCredentials` returns `Maybe (AuthBackendUser m)` (not tuple)
- **FR-039**: Type equality constraint is `AuthBackendUser m ~ OAuthUser m` only (removed userId constraint)
- **FR-040**: JWT claims include user identifiers via `ToJWT` instance on user type
- **FR-042**: Reference types no longer mention `OAuthUserId m` in polymorphic code

### Implementation Approach

#### Step 1: Update OAuthStateStore Typeclass

```haskell
-- In Servant.OAuth2.IDP.Store
class (MonadTime m) => OAuthStateStore m where
  type OAuthStateError m
  type OAuthStateEnv m
  type OAuthUser m      -- Full user for JWT and auth code storage
  -- REMOVED: type OAuthUserId m

  -- Methods unchanged (already use OAuthUser m, not userId)
  storeAuthCode :: AuthCodeId -> AuthorizationCode (OAuthUser m) -> m ()
  lookupAuthCode :: AuthCodeId -> m (Maybe (AuthorizationCode (OAuthUser m)))
  -- ...
```

#### Step 2: Update AuthBackend Typeclass

```haskell
-- In Servant.OAuth2.IDP.Auth.Backend
class AuthBackend m where
  type AuthBackendError m
  type AuthBackendEnv m
  type AuthBackendUser m  -- Full authenticated user
  -- REMOVED: type AuthBackendUserId m

  -- SIMPLIFIED: Returns Maybe user, not tuple
  validateCredentials
    :: Username
    -> PlaintextPassword
    -> m (Maybe (AuthBackendUser m))
```

#### Step 3: Update Reference Implementations

```haskell
-- In Servant.OAuth2.IDP.Store.InMemory
instance (MonadIO m, MonadTime m) => OAuthStateStore (ReaderT OAuthTVarEnv m) where
  type OAuthStateError (ReaderT OAuthTVarEnv m) = OAuthStoreError
  type OAuthStateEnv (ReaderT OAuthTVarEnv m) = OAuthTVarEnv
  type OAuthUser (ReaderT OAuthTVarEnv m) = AuthUser
  -- REMOVED: type OAuthUserId ...

-- In Servant.OAuth2.IDP.Auth.Demo
instance (MonadIO m) => AuthBackend (ReaderT DemoCredentialEnv m) where
  type AuthBackendError (ReaderT DemoCredentialEnv m) = DemoAuthError
  type AuthBackendEnv (ReaderT DemoCredentialEnv m) = DemoCredentialEnv
  type AuthBackendUser (ReaderT DemoCredentialEnv m) = AuthUser
  -- REMOVED: type AuthBackendUserId ...

  -- SIMPLIFIED: Return Just user, not tuple
  validateCredentials username password = do
    store <- asks credentialStore
    case lookupCredential username store of
      Nothing -> return Nothing
      Just (hashedPw, authUser) ->
        if validatePassword password hashedPw
          then return $ Just authUser
          else return Nothing
```

#### Step 4: Update Handler Signatures

```haskell
-- Handlers no longer need userId type equality constraint
handleLogin
  :: ( OAuthStateStore m
     , AuthBackend m
     , MonadTime m
     , MonadIO m
     , AuthBackendUser m ~ OAuthUser m  -- Only user type equality
     -- REMOVED: AuthBackendUserId m ~ OAuthUserId m
     )
  => LoginRequest
  -> m (Headers '[Header "Location" RedirectTarget, Header "Set-Cookie" SessionCookie] NoContent)
handleLogin req = do
  result <- validateCredentials (loginUsername req) (loginPassword req)
  case result of
    Nothing -> throwError authenticationFailed
    Just authUser -> do  -- Just user, not (userId, user) tuple
      let code = AuthorizationCode
            { authCodeUserId = authUser  -- Full user stored directly
            , ...
            }
      storeAuthCode codeId code
      ...
```

#### Step 5: Update Test Infrastructure

```haskell
-- Conformance spec simplified
oauthConformanceSpec
  :: forall m.
     ( OAuthStateStore m
     , AuthBackend m
     , MonadTime m
     , AuthBackendUser m ~ OAuthUser m  -- Only user type equality
     -- REMOVED: AuthBackendUserId m ~ OAuthUserId m
     , ToJWT (OAuthUser m)
     )
  => TestConfig m
  -> Spec
```

### Migration Order

| Order | Change | Complexity | Dependencies |
|-------|--------|------------|--------------|
| 1 | Remove `OAuthUserId m` from `OAuthStateStore` | Low | None |
| 2 | Remove `AuthBackendUserId m` from `AuthBackend` | Low | None |
| 3 | Simplify `validateCredentials` return type | Low | Step 2 |
| 4 | Update InMemory implementation | Low | Step 1 |
| 5 | Update Demo implementation | Low | Steps 2, 3 |
| 6 | Remove `AuthBackendUserId m ~ OAuthUserId m` constraints from handlers | Medium | Steps 1-5 |
| 7 | Update test infrastructure | Low | Step 6 |
| 8 | Update TestM and AppM instances | Low | Steps 1-5 |

### Files to Modify

| File | Change |
|------|--------|
| `src/Servant/OAuth2/IDP/Store.hs` | Remove `OAuthUserId m` associated type |
| `src/Servant/OAuth2/IDP/Auth/Backend.hs` | Remove `AuthBackendUserId m`; simplify `validateCredentials` |
| `src/Servant/OAuth2/IDP/Store/InMemory.hs` | Remove associated type instance |
| `src/Servant/OAuth2/IDP/Auth/Demo.hs` | Remove associated type instance; simplify `validateCredentials` |
| `src/Servant/OAuth2/IDP/Handlers/Login.hs` | Remove userId constraint; update pattern match |
| `src/Servant/OAuth2/IDP/Server.hs` | Remove userId constraint |
| `src/MCP/Server/HTTP.hs` | Remove userId constraint |
| `src/MCP/Server/HTTP/AppEnv.hs` | Remove `AuthBackendUserId AppM` instance |
| `test/TestMonad.hs` | Remove `AuthBackendUserId TestM` instance |
| `test/Laws/AuthBackendSpec.hs` | Update tests for simplified signature |
| `test/Laws/AuthBackendSignatureSpec.hs` | Update signature tests |
| `test/Laws/AuthBackendAssociatedTypesSpec.hs` | Remove userId type tests |

### Benefits of Removal

1. **Leaner API**: One fewer associated type per typeclass instance
2. **Simpler implementations**: No need to define a separate userId type
3. **Cleaner handler signatures**: One fewer type equality constraint
4. **Conceptual clarity**: User types are self-contained; ID is just a field
5. **Flexibility preserved**: `ToJWT` instance controls JWT claims including any user ID

### Constitution Compliance (Phase 14)

| Principle | Status | Evidence |
|-----------|--------|----------|
| I. Type-Driven Design | ✅ | Simpler types; user ID as field follows "make illegal states unrepresentable" (no orphan IDs) |
| II. Deep Module Architecture | ✅ | Reduced API surface; fewer associated types to implement |
| III. Denotational Semantics | ✅ | Clear semantic: `validateCredentials` returns the authenticated user, period |
| IV. Total Functions | ✅ | `Maybe (AuthBackendUser m)` is total; no change in totality |
| V. Pure Core, Impure Shell | ✅ | User types remain pure data |
| VI. Property-Based Testing | ✅ | Simpler property: "valid credentials return user" |

**Gate Status**: PASS - Phase 14 planning complete.

### Relationship to Other Phases

- **Phase 8**: Introduced `AuthBackendUserId m` and `OAuthUserId m` (now partially superseded)
- **Phase 14**: Removes these associated types per spec refinement; user IDs become fields in user types

---

## Phase 15: Security Hardening (FR-050–FR-057)

*Added: 2025-12-18 per security audit findings and spec clarification session*

### Overview

This phase addresses security issues identified in the 2025-12-18 security audit. The changes fall into two categories:

1. **Library-level fixes** (FR-050–FR-055, FR-057): Security invariants the library enforces regardless of consumer implementation
2. **Responsibility documentation** (FR-056): Clarifying what consumers must implement

### Requirements Addressed

| FR | Description | Category |
|----|-------------|----------|
| FR-050 | Exact hostname matching for localhost in `mkRedirectUri` | SSRF Prevention |
| FR-051 | Block private IP ranges in redirect URIs | SSRF Prevention |
| FR-052 | HTTPS required by default for redirect URIs | Transport Security |
| FR-053 | Remove unused `introspectToken` and `discoverOAuthMetadata` | Attack Surface Reduction |
| FR-054 | No unsafe constructor exports | API Safety |
| FR-055 | Cryptographic RNG for all security tokens | Cryptographic Security |
| FR-056 | Login page is host app responsibility | Responsibility Boundary |
| FR-057 | Require `crypton`, prohibit deprecated `cryptonite` | Dependency Security |

### Implementation Plan

#### Step 1: Dependency Migration (FR-057)

**File**: `mcp.cabal`

```cabal
-- BEFORE
build-depends:
    cryptonite >= 0.30 && < 0.31

-- AFTER
build-depends:
    crypton >= 0.34 && < 1.0
```

**Migration**:
- `crypton` is a drop-in replacement; same module names (`Crypto.*`)
- Update imports: none needed (API-compatible)
- Run full test suite to verify

#### Step 2: Fix `mkRedirectUri` Validation (FR-050, FR-051, FR-052)

**File**: `src/Servant/OAuth2/IDP/Types.hs`

```haskell
-- BEFORE (vulnerable to substring bypass)
mkRedirectUri :: Text -> Maybe RedirectUri
mkRedirectUri t = do
    uri <- parseURI (T.unpack t)
    let scheme = uriScheme uri
    if scheme == "https:"
        || (scheme == "http:" && ("localhost" `T.isInfixOf` t || "127.0.0.1" `T.isInfixOf` t))
        then Just (RedirectUri uri)
        else Nothing

-- AFTER (exact hostname matching + private IP blocking)
mkRedirectUri :: Text -> Maybe RedirectUri
mkRedirectUri t = do
    uri <- parseURI (T.unpack t)
    auth <- uriAuthority uri
    let hostname = uriRegName auth
        scheme = uriScheme uri

    -- Block private IP ranges (FR-051)
    guard $ not (isPrivateIP hostname)

    case scheme of
        "https:" -> Just (RedirectUri uri)
        "http:" ->
            -- FR-050: Exact hostname match for localhost exemption
            if hostname `elem` ["localhost", "127.0.0.1", "[::1]"]
            then Just (RedirectUri uri)
            else Nothing
        _ -> Nothing

-- FR-051: Private IP range blocking
isPrivateIP :: String -> Bool
isPrivateIP host =
    any (`isPrefixOf` host)
        [ "10."           -- 10.0.0.0/8
        , "172.16.", "172.17.", "172.18.", "172.19."  -- 172.16.0.0/12 (partial)
        , "172.20.", "172.21.", "172.22.", "172.23."
        , "172.24.", "172.25.", "172.26.", "172.27."
        , "172.28.", "172.29.", "172.30.", "172.31."
        , "192.168."      -- 192.168.0.0/16
        , "169.254."      -- Link-local / cloud metadata
        ]
    || host == "127.0.0.1"  -- Handled by localhost allowlist, but block if not localhost context
```

#### Step 3: Remove Unsafe Constructor Exports (FR-054)

**File**: `src/Servant/OAuth2/IDP/Boundary.hs` (or wherever `unsafeRedirectUri` is exported)

```haskell
-- BEFORE
module Servant.OAuth2.IDP.Boundary
    ( ...
    , unsafeRedirectUri  -- REMOVE THIS
    ) where

-- AFTER
module Servant.OAuth2.IDP.Boundary
    ( ...
    -- unsafeRedirectUri removed from exports
    ) where

-- If needed internally, move to Internal module:
-- src/Servant/OAuth2/IDP/Internal/Types.hs (not re-exported)
```

#### Step 4: Remove Unused Outbound HTTP Functions (FR-053)

**Files to modify**:
- `src/MCP/Server/Auth.hs` — remove `introspectToken`
- `src/Servant/OAuth2/IDP/Discovery.hs` (if exists) — remove `discoverOAuthMetadata`

```haskell
-- REMOVE entirely:
introspectToken :: IOTracer trace -> Text -> Text -> m IntrospectionResponse
discoverOAuthMetadata :: IOTracer trace -> Text -> m OAuthMetadata

-- These functions make outbound HTTP requests to arbitrary URLs
-- Token validation uses local JWT verification instead
```

#### Step 5: Cryptographic RNG for All Tokens (FR-055)

**Files**: All token generation code

```haskell
-- BEFORE (using System.Random or UUID)
import System.Random (randomIO)
import Data.UUID.V4 (nextRandom)

generateCodeVerifier :: IO Text
generateCodeVerifier = do
    bytes <- replicateM 32 randomIO  -- NON-CRYPTOGRAPHIC
    pure $ encodeBase64 (BS.pack bytes)

-- AFTER (using crypton CSPRNG)
import Crypto.Random (getRandomBytes)
import Data.ByteArray (convert)

generateCodeVerifier :: MonadIO m => m Text
generateCodeVerifier = liftIO $ do
    bytes <- getRandomBytes 32  -- CRYPTOGRAPHIC
    pure $ encodeBase64Url (convert bytes :: ByteString)

-- Apply to ALL security tokens:
-- - Authorization codes (if not using UUID)
-- - Session IDs
-- - PKCE code verifiers
-- - Client secrets (if generated)
-- - Any nonces or state parameters generated by library
```

**Note**: `UUID.nextRandom` uses `/dev/urandom` and is cryptographically secure. No change needed for UUID-based tokens. Focus on any `System.Random` usage.

#### Step 6: Update Security Responsibility Documentation

**File**: `spec.md` (already updated)

The spec now contains a **Security Responsibility Matrix** documenting:
- What the library guarantees (type-safe validation, CSPRNG tokens, SSRF prevention)
- What consumers must implement (password hashing, rate limiting, HTTPS termination)

No additional code changes needed; documentation already in spec.

### Files to Modify Summary

| File | Changes |
|------|---------|
| `mcp.cabal` | Replace `cryptonite` with `crypton` |
| `src/Servant/OAuth2/IDP/Types.hs` | Fix `mkRedirectUri` validation |
| `src/Servant/OAuth2/IDP/Boundary.hs` | Remove `unsafeRedirectUri` export |
| `src/MCP/Server/Auth.hs` | Remove `introspectToken` (if exists) |
| `src/Servant/OAuth2/IDP/Handlers/*.hs` | Use `Crypto.Random.getRandomBytes` for any non-UUID tokens |
| `test/` | Add tests for SSRF prevention, private IP blocking |

### Test Plan

#### SSRF Prevention Tests

```haskell
describe "mkRedirectUri" $ do
    it "rejects substring localhost bypass" $
        mkRedirectUri "http://evil.com/callback?localhost=bypass" `shouldBe` Nothing

    it "rejects private IP 10.x.x.x" $
        mkRedirectUri "https://10.0.0.1/callback" `shouldBe` Nothing

    it "rejects cloud metadata IP" $
        mkRedirectUri "https://169.254.169.254/latest/meta-data" `shouldBe` Nothing

    it "accepts exact localhost" $
        mkRedirectUri "http://localhost:3000/callback" `shouldSatisfy` isJust

    it "accepts HTTPS external" $
        mkRedirectUri "https://example.com/callback" `shouldSatisfy` isJust

    it "rejects HTTP external" $
        mkRedirectUri "http://example.com/callback" `shouldBe` Nothing
```

#### Cryptographic RNG Tests

```haskell
describe "Token Generation" $ do
    prop "generates sufficient entropy" $ \() -> ioProperty $ do
        tokens <- replicateM 1000 generateCodeVerifier
        let unique = Set.fromList tokens
        pure $ Set.size unique === 1000  -- No collisions

    it "tokens are 32+ bytes of entropy" $ do
        verifier <- generateCodeVerifier
        -- Base64url of 32 bytes = 43 characters
        T.length verifier `shouldSatisfy` (>= 43)
```

### Migration Order

| Order | Task | Risk | Dependencies |
|-------|------|------|--------------|
| 1 | Update `cryptonite` → `crypton` in cabal | Low | None |
| 2 | Fix `mkRedirectUri` validation | Medium | Build must succeed |
| 3 | Remove `unsafeRedirectUri` export | Low | Step 2 |
| 4 | Remove `introspectToken`/`discoverOAuthMetadata` | Low | Verify unused |
| 5 | Audit and fix RNG usage | Medium | Step 1 (crypton available) |
| 6 | Add SSRF prevention tests | Low | Step 2 |
| 7 | Full regression test | - | All steps |

### Constitution Compliance (Phase 15)

| Principle | Status | Evidence |
|-----------|--------|----------|
| I. Type-Driven Design | ✅ | Smart constructors enforce security invariants; unsafe paths removed |
| II. Deep Module Architecture | ✅ | Attack surface reduced by removing unused functions |
| III. Denotational Semantics | ✅ | `mkRedirectUri` has clear semantic: validated HTTPS URI or localhost |
| IV. Total Functions | ✅ | No change to totality; validation returns `Maybe` |
| V. Pure Core, Impure Shell | ✅ | RNG encapsulated in `MonadIO`; validation is pure |
| VI. Property-Based Testing | ✅ | SSRF prevention testable via property tests |

**Gate Status**: PASS - Phase 15 planning complete.

### Relationship to Other Phases

- **Phase 1-14**: Core typeclass design (unchanged)
- **Phase 15**: Security hardening layer; does not change typeclass interfaces
- **Consumers**: Must still implement their security measures per Security Responsibility Matrix
