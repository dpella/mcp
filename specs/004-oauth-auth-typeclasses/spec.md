# Feature Specification: OAuth State and Authentication Typeclasses

**Feature Branch**: `004-oauth-auth-typeclasses`
**Created**: 2025-12-11
**Status**: Draft
**Input**: User description: "Design two new typeclasses: 1) OAuth2+DCR state persistence interface for swappable production implementations, 2) User login/authentication interface. Both use polymorphic m Monad type parameter (three layer cake architecture). Provide in-memory storage and hard-coded credential implementations for examples."

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Production Deployment with Persistent Storage (Priority: P1)

A DevOps engineer deploys the MCP HTTP server in a production environment where OAuth state must persist across server restarts. They implement a PostgreSQL-backed storage layer that satisfies the OAuth state persistence interface.

**Why this priority**: Production deployments are the primary driver for making these interfaces swappable. Without persistent storage, server restarts invalidate all tokens and sessions.

**Independent Test**: Can be fully tested by implementing a mock persistent store and verifying that authorization codes, access tokens, refresh tokens, and client registrations survive simulated "restarts" (clearing in-memory state while persistent state remains).

**Acceptance Scenarios**:

1. **Given** a PostgreSQL-backed OAuth state implementation, **When** the server restarts after a user has obtained an access token, **Then** the access token remains valid.
2. **Given** a registered OAuth client stored in PostgreSQL, **When** the server restarts, **Then** the client registration is preserved and can be used for new authorization flows.
3. **Given** an in-progress authorization session, **When** implementing the state interface, **Then** pending authorizations can be stored and retrieved correctly.

---

### User Story 2 - Enterprise SSO Integration (Priority: P1)

An enterprise customer needs to integrate the MCP server with their existing identity provider (Active Directory, LDAP, Okta). They implement the authentication interface to validate credentials against their IdP instead of the hard-coded demo credentials.

**Why this priority**: Enterprise customers require integration with their existing authentication infrastructure. This is a blocking requirement for enterprise adoption.

**Independent Test**: Can be fully tested by implementing a mock LDAP connector and verifying that credential validation correctly delegates to the mock, returning success for valid credentials and failure for invalid ones.

**Acceptance Scenarios**:

1. **Given** an LDAP-backed authentication implementation, **When** a user logs in with valid LDAP credentials, **Then** authentication succeeds and an authorization code is issued.
2. **Given** an LDAP-backed authentication implementation, **When** a user logs in with invalid credentials, **Then** authentication fails with an appropriate error.
3. **Given** multiple authentication backends configured, **When** the typeclass is polymorphic over the monad, **Then** the implementation can perform IO operations (LDAP queries) as needed.

---

### User Story 3 - Development and Testing with In-Memory State (Priority: P2)

A developer runs the MCP server locally for development/testing. They use the default in-memory implementations for both OAuth state and authentication, getting the current behavior with no configuration changes.

**Why this priority**: Backward compatibility with existing usage is essential. The abstraction should not break existing development workflows.

**Independent Test**: Can be fully tested by running the existing demo server and verifying that all OAuth flows (client registration, authorization, token exchange, refresh) continue to work identically.

**Acceptance Scenarios**:

1. **Given** the default in-memory OAuth state implementation, **When** running the example server with `--oauth`, **Then** all OAuth endpoints function identically to current behavior.
2. **Given** the default hard-coded credential store implementation, **When** logging in with demo/demo123 or admin/admin456, **Then** authentication succeeds.
3. **Given** the default implementations, **When** no explicit configuration is provided, **Then** the server uses the in-memory/demo implementations automatically.

---

### User Story 4 - Custom Token Lifecycle Management (Priority: P3)

An operator needs custom token lifecycle management (e.g., revocation lists, token rotation policies, audit logging). By implementing the OAuth state interface, they can intercept all state mutations and add custom behavior.

**Why this priority**: Advanced use cases for security-conscious deployments. Less common but enabled by the abstraction.

**Independent Test**: Can be fully tested by implementing a wrapper that logs all state mutations and verifying that authorization code creation, token issuance, and token refresh all trigger the expected log entries.

**Acceptance Scenarios**:

1. **Given** a custom OAuth state implementation with audit logging, **When** a token is issued, **Then** the issuance event is logged with user, client, and timestamp.
2. **Given** a custom implementation with token revocation, **When** a token is revoked externally, **Then** subsequent validation fails.

---

### Edge Cases

- **Store unavailability**: Operations fail fast with a typed error; retry logic and circuit breakers are infrastructure concerns outside the typeclass contract.
- **Concurrent access**: Implementation-defined atomicity; each implementation ensures its own consistency guarantees (e.g., STM for in-memory, row-level locking for SQL).
- **Auth backend timeout**: Fail fast with typed error; consistent with store unavailability handling. Retry/fallback logic is outside typeclass scope.
- **Partial failures**: Compound operations (e.g., token exchange) are all-or-nothing; implementations must ensure no partial state is persisted on failure.
- **Multi-instance scaling**: Typeclass defines single-instance semantics; distributed coordination (if needed) is handled by the storage backend (e.g., PostgreSQL concurrency control), not the typeclass contract.
- **Observability**: Tracing is the responsibility of implementations; they are encouraged to include a specific tracer in their environment type (`OAuthStateEnv`, `AuthBackendEnv`), which users wire up at application composition time.

### Security Responsibility Matrix

This library provides **interfaces** (typeclasses) and **reference implementations** (demo-quality examples). Security measures are deliberately omitted from reference implementations to keep them simple and instructive. Production deployments MUST implement appropriate security controls in their typeclass instances.

| Concern | Library Provides | Consumer Responsibility |
|---------|------------------|------------------------|
| **Password Hashing** | `AuthBackend` typeclass with `validateCredentials` signature | Implement memory-hard KDF (Argon2id, bcrypt) in `AuthBackend` instance |
| **Password Salt** | None (not in interface) | Generate unique per-user salts in `AuthBackend` implementation |
| **Rate Limiting** | None (infrastructure concern) | Apply rate limiting middleware or implement in `AuthBackend` |
| **HTTPS Enforcement** | `requireHTTPS` config flag | Deploy behind TLS termination; set `requireHTTPS = True` |
| **Token Storage Security** | `OAuthStateStore` typeclass interface | Implement secure storage (encrypted at rest, access controls) |
| **Session Entropy** | None (implementation detail) | Use cryptographically secure random generation in store implementation |
| **CSRF Protection** | `SameSite=Strict` cookie flag in reference | Add CSRF tokens if supporting older browsers |
| **Input Validation** | Type-safe newtypes with smart constructors | Additional domain validation in implementations as needed |
| **Audit Logging** | Tracer parameter in environment types | Wire up tracer; implement audit trail in store operations |
| **Login Page UI** | None (library provides OAuth endpoints only) | Host application implements login route with appropriate UI, headers, CSP |
| **Security Headers** | None (infrastructure concern) | Configure CSP, HSTS, X-Frame-Options in reverse proxy or host app middleware |

**Reference Implementation Caveats** (in-memory store + demo auth):
- SHA256 password hashing (GPU-crackable) - FOR DEMO ONLY
- Hardcoded global salt - FOR DEMO ONLY
- No rate limiting - FOR DEMO ONLY
- Ephemeral storage (lost on restart) - FOR DEMO ONLY

These limitations are **features, not bugs** - they make the reference implementations simple to understand and use for development/testing. Production deployments implement the typeclasses with appropriate security measures.

## Clarifications

### Session 2025-12-18

- Q: Should the spec add explicit requirements for type-safe newtypes in `ClientRegistrationRequest`, `ClientRegistrationResponse`, and `TokenResponse`? → A: Yes. Add new FRs requiring `NonEmpty` for redirect_uris/grant_types/response_types and newtypes for ClientId/ClientSecret/ClientName in registration types. TokenResponse fields also need precise types.
- Q: How should the spec formalize security responsibility boundaries between the library's interfaces and consumer implementations? → A: Add explicit Security Responsibility Matrix section documenting library vs consumer obligations.
- Q: How should the library handle URL validation for outbound requests and redirect URIs? → A: Strict validation by default (HTTPS-only, exact hostname matching for localhost, private IP blocking) with opt-in development mode. Remove unused `introspectToken` function entirely (attack surface reduction).
- Q: How should the library handle random number generation for security-sensitive values (PKCE verifiers, tokens)? → A: Library MUST use cryptographic RNG (e.g., `Crypto.Random.getRandomBytes`) for ALL security tokens. This is a library invariant, not a consumer choice.
- Q: Should the library provide security headers (CSP, HSTS, X-Frame-Options) or leave this to consumers/infrastructure? → A: Consumer responsibility. The login page is expected to be implemented as a route on the host application that embeds the library, not provided by the library itself. Security headers are infrastructure/deployment concerns configured by consumers.
- Q: Should the spec explicitly require `crypton` and prohibit deprecated `cryptonite`? → A: Yes. MUST use `crypton`, MUST NOT depend on deprecated `cryptonite`. Ensures security patches and follows Haskell ecosystem migration.
- Q: What newtypes should replace raw `Text` for OAuth query parameters `scope`, `state`, and `resource`? → A: Use `Scope` (already exists), `OAuthState` (new, disambiguates from State monad), and `ResourceIndicator` (new, per RFC 8707). These newtypes MUST be used consistently everywhere these concepts appear: API query params, `TokenResponse.scope`, `TokenRequest.reqResource`, `PendingAuthorization` fields, and all handler code. No raw `Text` for these OAuth protocol values.

### Session 2025-12-17

- Q: Should `mcpAppWithOAuth` accept a natural transformation like `mcpApp`? → A: Yes, ALL `mcpApp*` functions MUST accept `(∀ a. m a -> Handler a)` with their respective constraints on `m`.
- Q: Should AuthorizationCode store OAuthUser m (full user) or OAuthUserId m (just ID)? → A: Store `OAuthUser m` directly. This eliminates `lookupUserById`/`storeUserInCache` methods and the user cache complexity. Simpler is better; the auth code is short-lived anyway.
- Q: Should `AuthBackendUserId m` be removed from `AuthBackend`? → A: Yes, remove it. Simplify `validateCredentials` to return `Maybe (AuthBackendUser m)`. Implementations can store user IDs as fields within their `AuthBackendUser` type and encode them into JWT claims via `ToJWT` as needed. This eliminates an unused associated type and reduces API surface.

### Session 2025-12-16

- Q: Where should the top-level `mcpApp` entry point be defined? → A: `MCP.Server.HTTP` — alongside the HTTP transport since mcpApp produces a WAI Application. This keeps it separate from StdIO transport and outside the OAuth2 namespace, enabling clean package extraction later.
- Q: How should `mcpApp` compose MCP core routes with OAuth2 routes? → A: Type-level API combination (`type FullAPI = MCPAPI :<|> OAuthAPI`) with a single polymorphic server. This preserves unified type signatures, allows polymorphic constraints to flow naturally across all handlers, and enables clean future package extraction (OAuth API type can move to separate package, MCP.Server.HTTP imports and combines).
- Q: How should the system handle OAuth2 being optional (enabled via CLI flag)? → A: Two separate entry points: `mcpApp` (MCP-only, no OAuth2 constraints) and `mcpAppWithOAuth` (MCP + OAuth2 routes, requires OAuthStateStore/AuthBackend constraints). CLI flag selects which function to call at startup. This avoids runtime type gymnastics, keeps type signatures explicit, and makes the API self-documenting.
- Q: What module boundaries should be established for future OAuth2 package extraction? → A: Move all OAuth2 modules to `Servant.OAuth2.IDP.*` namespace (outside MCP namespace) within this same package. This makes the package boundary explicit now—extraction later is "cut along the dotted line." Module mapping: `MCP.Server.OAuth.*` → `Servant.OAuth2.IDP.*`, `MCP.Server.Auth.*` → `Servant.OAuth2.IDP.Auth.*`. MCP modules (`MCP.Server.HTTP`, `MCP.Server.StdIO`, etc.) import from `Servant.OAuth2.IDP` when OAuth2 is needed. The `Servant.OAuth2.IDP` namespace signals this is a reusable OAuth2 Identity Provider implementation for Servant, not MCP-specific.

### Session 2025-12-15

- Q: How should mcpApp accept the natural transformation for callers to select typeclass implementations? → A: `mcpApp :: (∀ a. m a -> Handler a) -> Application` — callers provide the complete natural transformation; server is fully polymorphic. This follows standard Servant `hoistServer` patterns and enables maximum flexibility for backend selection.
- Q: Should ServerError (or other framework-specific errors) appear in polymorphic handler error constraints? → A: No. Handlers use `AsType` constraints for domain errors only (`AsType (OAuthStateError m) e`, `AsType ValidationError e`, etc.). `ServerError` appears ONLY in the natural transformation at the Servant boundary. Handlers are framework-agnostic domain logic; the `hoistServer` translation layer is solely responsible for mapping domain errors to HTTP responses.
- Q: What domain error types should the library define for `AsType` constraints in handlers? → A: Four distinct types for precise HTTP status mapping per OAuth spec: (1) `OAuthStateError m` (associated type) → 500 (storage/infrastructure failures, details logged not exposed to clients), (2) `AuthBackendError m` (associated type) → 401 (authentication failures), (3) `ValidationError` (fixed type) → 400 (malformed input, missing parameters), (4) `AuthorizationError` (fixed type) → OAuth-specific failures (invalid_grant, expired_code, invalid_client, etc.) mapped to appropriate 4xx codes per RFC 6749. Precision enables spec-compliant error responses.
- Q: Where should the natural transformation that maps domain errors to HTTP errors live? → A: Provide a reusable `domainErrorToServerError` function in a boundary module (e.g., `MCP.Server.OAuth.Boundary`) that works with any error type `e` having `AsType` instances for all four domain errors. Returns `m (Maybe ServerError)` to handle the case where no prism matches. Signature: `domainErrorToServerError :: (MonadIO m, AsType (OAuthStateError m') e, AsType AuthorizationError e, AsType ValidationError e, AsType (AuthBackendError m') e) => IOTracer trace -> e -> m (Maybe ServerError)`. Consumers use this in their `hoistServer` natural transformation and handle `Nothing` as they see fit (default 500, continue pattern matching, etc.).
- Q: How should the reference implementation's composite error type (AppError) be structured? → A: Four domain error constructors only: `OAuthStateErr (OAuthStateError m)`, `AuthBackendErr (AuthBackendError m)`, `ValidationErr ValidationError`, `AuthorizationErr AuthorizationError`. No `ServerErr ServerError` constructor. The natural transformation is total for this type (all constructors map to domain errors that `domainErrorToServerError` handles). Reference implementation's `hoistServer` wrapper handles the `Maybe` by providing a fallback (e.g., 500 Internal Server Error) if `Nothing`.
- Q: Should there be a separate `AuthFailure` constructor in addition to `AuthBackendErr`? → A: No. `AuthBackendErr (AuthBackendError m)` is the sole authentication error path. The `AuthBackendError m` associated type should have constructors for different failure modes (invalid credentials, user not found, account locked, etc.). No separate generic `AuthFailure` constructor—all auth failures flow through the backend's error type for consistent handling and appropriate detail level.
- Q: Is `OAuthStateError` an associated type or a fixed type? → A: Associated type (`OAuthStateError m`). Storage failures are implementation-specific (TVar errors differ from PostgreSQL errors differ from Redis errors). The boundary function MUST NOT use `Show` to generate client-facing error messages (security: could leak connection strings, table names, internal state). Instead: (1) log the full error details internally via tracer, (2) return generic "Internal Server Error" (500) to clients with no implementation details. Handler signatures use `AsType (OAuthStateError m) e`.
- Q: Where should `ValidationError` be defined? → A: `MCP.Server.OAuth.Types` alongside `AuthorizationError`. Both are fixed protocol-level error types; keeping them together maintains consistency and the "domain types in domain modules" principle.
- Q: How does `domainErrorToServerError` log `OAuthStateError m` details without exposing them to clients? → A: Add `MonadIO` constraint and tracer parameter to the function so it logs internally. Signature becomes `domainErrorToServerError :: (MonadIO m, AsType (OAuthStateError m') e, ...) => IOTracer trace -> e -> m (Maybe ServerError)`. The function extracts storage errors via prism, logs full details through the tracer, then returns generic 500. Logging is encapsulated where the error details are known.
- Q: Should `AuthBackendError m` details be logged or exposed to clients? → A: Always log, never expose (same as `OAuthStateError m`). Auth failures return generic "Authentication failed" (401) to clients. Full error details (LDAP unreachable, database timeout, account locked reason, etc.) are logged internally via tracer. This prevents information leakage that could aid attackers (user enumeration, infrastructure discovery).
- Q: How should OAuthStateStore and AuthBackend agree on the user type at instantiation time? → A: Type equality constraint. Both typeclasses define their own associated user type (`AuthBackendUser m` and `OAuthUser m`), and handlers requiring both add `AuthBackendUser m ~ OAuthUser m` constraint to ensure compile-time agreement.
- Q: Where should JWT constraints (ToJWT/FromJWT) on the associated user type be specified? → A: Constraints in operations. Individual methods/handlers that need JWT add `(ToJWT (OAuthUser m))` in their signatures rather than at typeclass level. Follows minimal constraint principle.
- Q: How should data types like AuthorizationCode handle varying user identity representations? → A: ~~Add associated type for UserId. Data types contain `UserId` (not full user struct).~~ **SUPERSEDED 2025-12-17**: Store full `OAuthUser m` in AuthorizationCode for simplicity. See Session 2025-12-17 clarification.
- Q: How should the relationship between authenticated user and user ID be expressed? → A: ~~Return tuple from validateCredentials.~~ **SUPERSEDED 2025-12-17**: `validateCredentials` returns `Maybe (AuthBackendUser m)` only. User identifiers are fields within the user type, encoded into JWT claims via `ToJWT`. No separate `AuthBackendUserId m` associated type.
- Q: Which OAuth data types need user parameterization? → A: Only AuthorizationCode (parameterized over `OAuthUser m`, storing full user). Access/refresh tokens are bearer tokens validated by JWT signature. PendingAuthorization tracks session state before authentication completes. **Updated 2025-12-17**: Changed from userId to full user for simplicity.
- Q: What is the correct terminology for AuthUser and the in-memory UserId type? → A: "Reference implementation types", not "default types". AuthUser is the user type of the reference in-memory implementation we ship; same for the UserId type (e.g., Text or UUID). They are one example among equals, not fallbacks.
- Q: What user data is encoded in JWT access tokens? → A: Full OAuthUser via ToJWT. The ToJWT instance on the user type produces appropriate claims; JWT structure is the user type's responsibility. Additionally, refresh and access tokens MAY include userId at the implementation's discretion for: (1) checking user validity during refresh, (2) matching against ACLs for access control. This is optional per-implementation.
- Q: How does OAuthStateStore access current time for expiry filtering? → A: MonadTime constraint. `OAuthStateStore m` requires `MonadTime m`; implementations use `currentTime` from the constraint. Consistent with FR-032's test strategy and enables deterministic expiry testing via controllable clocks.
- Q: Where should reference implementation concrete types (AuthUser, UserId, errors) be defined? → A: Colocated with implementations. AuthUser/UserId in `MCP.Server.Auth.Demo`, store errors in `MCP.Server.OAuth.InMemory`. Critical: these types must NOT appear in modules implementing against the polymorphic interface (handlers, HTTP boundary). Each implementation is self-contained; polymorphic code never imports concrete types.

### Session 2025-12-13

- Q: How should type-directed polymorphic functions thread type witnesses? → A: Modern Haskell patterns only: `Proxy @Type` or `@Type` type applications (AllowAmbiguousTypes/TypeApplications). Never use `undefined :: Type`.

### Session 2025-12-12

- Q: What scope of OAuth flows should the hspec-wai functional test harness cover? → A: Full OAuth flow coverage (registration, authorization with login, token exchange, refresh, all error cases)
- Q: How should the hspec-wai test harness manage OAuth state between test cases? → A: Per-test setup with helper combinators; each test is isolated but reusable combinators (e.g., `withRegisteredClient`, `withAuthorizedUser`) handle common setup patterns while allowing custom setup for edge cases
- Q: Should functional tests use real in-memory implementations or mocks? → A: Polymorphic test specs; tests accept a `runM` function to monomorphize type vars, enabling library users to test their own implementations. Reference in-memory implementation tested via this generic infrastructure, indirectly integration-testing all low-level functionality
- Q: Where should the polymorphic conformance test specs be located? → A: Internal module (`MCP.Server.OAuth.Test.Internal`) in main library; OAuth/auth code will eventually be extracted to separate library, at which point a dedicated test-support package may be considered
- Q: What hspec-wai pattern should the test harness use for HTTP requests/assertions? → A: Native hspec-wai DSL (`get`, `post`, `request`, `shouldRespondWith`, `matchHeaders`, `matchStatus`) directly; no custom wrapper layer
- Q: How should functional tests control time for expiry testing? → A: `MonadTime` constraint; tests provide a controllable clock via the `runM` natural transformation, enabling deterministic expiry testing without real delays
- Q: How should polymorphic Servant server become WAI Application for hspec-wai? → A: `makeTestApp :: RunM m -> IO Application` combinator; critically, the SAME type variable `m` must flow to both `makeTestApp` and the polymorphic spec, ensuring type coherence across application construction and test assertions
- Q: How should helper combinators interact with hspec-wai's WaiSession monad? → A: HTTP-based setup; combinators issue real HTTP requests (POST /register, POST /authorize) within WaiSession for true black-box testing
- Q: How should tests control time advancement for expiry testing? → A: Shared TVar; `makeTestApp` returns `(Application, TVar UTCTime)`; tests use `advanceTime` helper to modify the TVar between HTTP requests
- Q: How should the polymorphic test interface abstract over time control? → A: Abstract callback; polymorphic interface includes `advanceTime :: NominalDiffTime -> IO ()` callback that implementations provide; reference implementation uses TVar internally, third-party implementations use their own mechanism
- Q: How should tests achieve isolation with shared hspec-wai Application? → A: Dual strategy; fresh Application per test (via `around`) when isolation required, OR reuse Application with unique identifiers (UUID-based client IDs) when isolation is expensive. Test harness supports both patterns.
- Q: How should the polymorphic test interface handle test credentials? → A: Credentials in test config; users of the test harness provide valid `TestCredentials` (username, password) and are responsible for setting up those credentials in the `runM`/AuthBackend they provide
- Q: How should the conformance suite test invalid credentials? → A: Obvious invalids; tests use obviously wrong credentials (empty password, nonexistent user like `__invalid_user__`) that any reasonable implementation rejects
- Q: How should HTTP response headers be type-protected to prevent position-swap bugs (e.g., Location/Set-Cookie confusion)? → A: Use Servant's typed header combinators where available; add semantic newtypes for header values (e.g., `Header "Location" RedirectTarget`, `Header "Set-Cookie" SessionCookie`) where names describe the value's purpose, not the header name
- Q: Where should response header newtypes (RedirectTarget, SessionCookie) be defined? → A: OAuth.Types alongside existing newtypes. Design principle: all OAuth combinators must be usable outside Servant; Servant is a production-ready reference implementation, not the only target. Domain types belong in domain modules.
- Q: Should OAuth HTTP handlers be polymorphic over `m` or use a concrete `AppM` type? → A: Handlers MUST be polymorphic over `m` with typeclass constraints. `AppM` is ONE instantiation at Servant boundary, not the handler definition. Design principle: delay monomorphization until the last possible moment unless diminishing returns or more important tradeoff.
- Q: How should handlers requiring IO-based library calls (e.g., JWT signing) be handled? → A: Add constraints judiciously. Use existing abstractions (e.g., `MonadJWT`) if available. Otherwise, for mundane IO library calls (no missiles), just add `MonadIO` constraint and move on. Don't create abstractions for abstraction's sake — polymorphism where it enables swappability/testability, pragmatic `MonadIO` where it doesn't matter.
- Q: How should the test harness instantiate polymorphic handlers for WAI Application? → A: Polymorphic server (`oauthServer :: (constraints) => ServerT OAuthAPI m`), instantiated by test config providing `runM`. Same server definition, different interpreter. Handlers MUST be Servant-agnostic. Servant provides: (1) production-ready reference implementation, (2) typed API spec. Handlers are constructive proofs of correct implementability, usable standalone when constraints discharged. Will be reused in Yesod app; specified/implemented/tested here with Servant's help.

### Session 2025-12-11

- Q: Which type system should be the canonical source of truth for OAuth state types? → A: OAuth.Types newtypes (ClientId, RedirectUri, etc.) are canonical; HTTP.hs converts Text ↔ newtype at boundary
- Q: Where should Text ↔ newtype boundary conversions be implemented? → A: Servant combinators (FromHttpApiData/ToHttpApiData instances); these ARE the boundary and enable property-based round-trip tests
- Q: How should invalid data (e.g., empty redirect URI list) be handled at the HTTP boundary? → A: Fail at boundary (400) via FromJSON/FromHttpApiData; make illegal states unrepresentable in types
- Q: What migration strategy should be used for the 15+ existing HTTP handlers? → A: Incremental per-handler; each handler migrated separately with its own nested bead under mcp-51r
- Q: Should PendingAuthorization use OAuth.Types newtypes or raw Text for its fields? → A: Same newtypes (ClientId, RedirectUri, etc.); already validated at request parse time
- Q: How should the system handle OAuth state store unavailability? → A: Fail fast with typed error (retries are infrastructure-level concerns)
- Q: How should concurrent access to the same authorization code be handled? → A: Implementation-defined atomicity (STM, row locking, etc.)
- Q: How should partial failures in compound operations be handled? → A: Fail entirely; all-or-nothing semantics, no partial state
- Q: What happens when an authentication backend times out? → A: Fail fast with typed error (consistent with store unavailability)
- Q: Should the typeclass design account for multi-instance/distributed deployment? → A: No; single-instance semantics, distributed coordination handled by storage backend
- Q: Who is responsible for tracing/observability? → A: Implementations; encouraged to include tracer in their environment type, users wire it up
- Q: Should the typeclasses document formal laws? → A: Yes; key round-trip laws (store/lookup, delete removes, idempotence) for property-based testing
- Q: Should lookup operations filter expired entries or return raw data? → A: Filter expired; enables backend TTL mechanisms (e.g., Redis TTL) and keeps expiry enforcement in store

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST provide an `OAuthStateStore` typeclass with operations for managing authorization codes, access tokens, refresh tokens, registered clients, and pending authorizations. The typeclass MUST define associated type `OAuthUser m` (authenticated user for JWT and storage in `AuthorizationCode`).
- **FR-002**: System MUST provide an `AuthBackend` typeclass with operations for validating user credentials. The `validateCredentials` method MUST return `m (Maybe (AuthBackendUser m))` where `AuthBackendUser m` is an associated type, enabling implementations to return their own user representation on successful authentication. User identifiers, if needed, are fields within the user type and encoded into JWT claims via `ToJWT`.
- **FR-003**: Both typeclasses MUST be polymorphic over a constrained monad type parameter `m` (three-layer cake architecture), allowing implementations to require `MonadIO`, `MonadState`, etc. in their instance contexts.
- **FR-004**: System MUST provide a reference in-memory implementation of `OAuthStateStore` using `TVar` that maintains current behavior. This is one example implementation, not a fallback default.
- **FR-005**: System MUST provide a reference hard-coded credential implementation of `AuthBackend` that maintains current demo behavior (demo/demo123, admin/admin456). This is one example implementation, not a fallback default.
- **FR-006**: All existing OAuth endpoints MUST continue to function identically when using the reference implementations.
- **FR-007**: The typeclass operations MUST return results in the monadic context, allowing implementations to perform effectful operations.
- **FR-008**: The `OAuthStateStore` typeclass MUST support atomic operations where the current implementation uses `atomically` with `TVar`.
- **FR-009**: The `AuthBackend` typeclass MUST allow implementations to access configuration (e.g., credential store, salt) needed for validation.
- **FR-010**: Each typeclass MUST define an associated error type that implementations can customize for their specific failure modes.
- **FR-011**: Each typeclass MUST define an associated environment type that implementations can customize for their specific configuration/state needs.
- **FR-012**: Handler functions MUST use `MonadError e m` with prism constraints to compose domain errors into a unified error type. Four domain error types require `AsType` constraints: (1) `AsType (OAuthStateError m) e` for storage failures (associated type), (2) `AsType (AuthBackendError m) e` for authentication failures (associated type), (3) `AsType ValidationError e` for input validation (fixed type), (4) `AsType AuthorizationError e` for OAuth protocol errors (fixed type). Framework-specific errors (e.g., Servant's `ServerError`) MUST NOT appear in handler constraints; translation to framework errors happens exclusively in the natural transformation at the framework boundary.
- **FR-013**: Handler functions MUST use `MonadReader env m` with lens constraints (e.g., `HasType (OAuthStateEnv m) env`) to compose environments from multiple typeclasses into a unified environment type.
- **FR-014**: The typeclasses MUST be framework-agnostic; Servant integration MUST use `hoistServer` to translate the polymorphic error type to `ServerError` at the boundary.
- **FR-018**: All OAuth.Types newtypes (`ClientId`, `RedirectUri`, `AuthCodeId`, `GrantType`, etc.) MUST implement `FromHttpApiData` and `ToHttpApiData` instances for Servant boundary conversion. These instances serve as the canonical Text ↔ newtype conversion layer.
- **FR-019**: The `FromHttpApiData`/`ToHttpApiData` instances MUST have property-based round-trip tests verifying `parseUrlPiece . toUrlPiece ≡ Right`.
- **FR-020**: `FromJSON` instances for OAuth types with invariants (e.g., `NonEmpty RedirectUri`) MUST reject invalid data at parse time, returning appropriate JSON parse errors that Servant translates to 400 Bad Request. Domain error types MUST NOT include cases for structurally invalid input.
- **FR-021**: HTTP handler migration from Text-based to newtype-based OAuth state MUST be incremental (one handler at a time). Each handler migration is tracked as a nested bead under the parent unification task (mcp-51r). Old and new patterns may coexist during migration.
- **FR-015**: The design MUST allow the same handler logic to be used outside of Servant (e.g., in CLI tools, tests, Yesod apps, or other frameworks) by providing different error/environment interpreters. Handlers are Servant-agnostic constructive proofs of correct implementability; Servant is a production-ready reference implementation and typed API spec, not the only target.
- **FR-036**: OAuth HTTP handler functions MUST be polymorphic over the monad `m`, constrained by typeclass requirements (e.g., `(OAuthStateStore m, AuthBackend m, MonadError e m, MonadReader env m, ...) => ...`). Concrete types like `AppM` or `Handler` are instantiations at the Servant boundary via `hoistServer`, NOT the handler definitions. Design principle: delay monomorphization until the last possible moment.
- **FR-037**: Constraint selection MUST be judicious. Use existing abstractions (e.g., `MonadTime`) when available. For mundane IO library calls (JWT signing, UUID generation) that don't warrant abstraction, add `MonadIO` constraint directly. Create new typeclasses only when swappability or testability genuinely benefits. Avoid architecture astronautics.
- **FR-016**: Each typeclass MUST document key algebraic laws enabling property-based testing:
  - Round-trip: `lookupX k` after `storeX k v` returns `Just v` (if not expired)
  - Delete: `lookupX k` after `deleteX k` returns `Nothing`
  - Idempotence: `storeX k v >> storeX k v` equivalent to `storeX k v`
  - Overwrite: `storeX k v2` after `storeX k v1` makes `lookupX k` return `Just v2`
- **FR-017**: Lookup operations for time-bounded entities (auth codes, pending authorizations) MUST filter expired entries, returning `Nothing` for expired items. This enables backend TTL mechanisms (e.g., Redis EXPIRE, PostgreSQL scheduled cleanup) and keeps expiry enforcement within the store abstraction. The `OAuthStateStore` typeclass MUST require `MonadTime m` to access current time for expiry checks, enabling deterministic testing via controllable clocks.
- **FR-022**: HTTP response headers MUST use type-safe values to prevent position-swap bugs. Use Servant's typed header combinators where available. For custom/composite headers, add semantic newtypes describing the value's purpose (e.g., `RedirectTarget`, `SessionCookie`) enabling usage like `Header "Location" RedirectTarget`. The `addHeader` call order must match the type-level header list, and distinct types make swaps a compile error.
- **FR-023**: System MUST provide an hspec-wai-based functional test harness for HTTP-level black-box testing of OAuth flows. Tests run in-process using hspec-wai's `with` application pattern (no separate HTTP server process).
- **FR-024**: The functional test suite MUST cover the full OAuth flow: client registration, authorization endpoint (GET with login page), login form submission, token exchange, and token refresh.
- **FR-025**: The functional test suite MUST verify HTTP-level correctness: status codes, response headers (Location, Set-Cookie, WWW-Authenticate), content types, and response bodies.
- **FR-026**: The functional test suite MUST cover error cases: invalid client, expired codes, invalid PKCE verifier, malformed requests, and authentication failures. Authentication failure tests MUST use obviously invalid credentials (empty password, nonexistent user) that any reasonable `AuthBackend` rejects.
- **FR-027**: Each functional test MUST be isolated with its own state setup. The test harness MUST support two isolation strategies: (1) fresh Application per test via hspec's `around` for strict isolation, (2) shared Application with unique identifiers (UUID-based client IDs, etc.) for performance-sensitive scenarios. Helper combinators (e.g., `withRegisteredClient`, `withAuthorizedUser`) SHOULD be provided for common setup patterns while allowing custom setup for edge case tests. These combinators MUST use HTTP-based setup (issuing real requests within `WaiSession`) for true black-box testing.
- **FR-028**: The functional test specs MUST be polymorphic over the `OAuthStateStore` and `AuthBackend` implementations. Tests accept a `runM` function (natural transformation) to monomorphize type variables, enabling library users to run the conformance suite against their own implementations.
- **FR-029**: The polymorphic test specs MUST be located in `Servant.OAuth2.IDP.Test.Internal`. This module is importable by third-party implementations but marked internal (no stability guarantees). Module structure will be revisited when OAuth/auth code is extracted to a separate library.
- **FR-030**: The reference in-memory `OAuthStateStore` and demo `AuthBackend` implementations MUST be tested using the polymorphic conformance suite, serving as both validation and usage example.
- **FR-031**: The functional tests MUST use hspec-wai's native DSL directly (`get`, `post`, `request`, `shouldRespondWith`, `matchHeaders`, `matchStatus`). No custom wrapper layer for request/response handling.
- **FR-032**: The polymorphic test monad MUST include a `MonadTime` constraint. The `runM` natural transformation enables tests to inject a controllable clock for deterministic expiry testing (auth codes, sessions) without real-time delays.
- **FR-033**: The test harness MUST provide a `makeTestApp` combinator that constructs a WAI `Application` from the polymorphic server. The SAME type variable `m` that parameterizes `makeTestApp` MUST flow to the polymorphic test specs, ensuring type coherence between application construction and test assertions.
- **FR-034**: The polymorphic conformance test interface MUST include an abstract `advanceTime :: NominalDiffTime -> IO ()` callback that implementations provide. The reference in-memory implementation uses `TVar UTCTime` internally; third-party implementations provide their own time control mechanism (e.g., database timestamps, mock clocks).
- **FR-035**: The polymorphic conformance test interface MUST include `TestCredentials` (username, password) as part of the test configuration. Implementations are responsible for ensuring their `AuthBackend` recognizes these credentials. The reference implementation pre-configures demo credentials.
- **FR-038**: Type-directed polymorphic functions (e.g., property-based test helpers) MUST use modern Haskell type witness patterns: `Proxy @Type` or `@Type` type applications with `AllowAmbiguousTypes`/`TypeApplications`. NEVER use `undefined :: Type` to thread type information. Example: prefer `identityRoundTrip @ClientId` or `identityRoundTrip (Proxy @ClientId)` over `identityRoundTrip "ClientId" (undefined :: ClientId)`.
- **FR-039**: Handlers requiring both `OAuthStateStore` and `AuthBackend` MUST include type equality constraint `AuthBackendUser m ~ OAuthUser m` to ensure compile-time agreement on the user type. This enables the authenticated user from `AuthBackend.validateCredentials` to be stored directly in OAuth state structures like `AuthorizationCode`.
- **FR-040**: JWT constraints (`ToJWT`, `FromJWT`) on the associated user type MUST be specified at the operation/handler level, not at the typeclass level. Operations requiring JWT token generation add `(ToJWT (OAuthUser m))` to their signature. This follows the minimal constraint principle and allows user types that don't need JWT support in all contexts. The `ToJWT` instance on the user type is responsible for producing appropriate JWT claims, including any user identifiers needed for refresh validation or access control.
- **FR-041**: The `AuthorizationCode` data type MUST be parameterized over the user type: `AuthorizationCode user`. The `OAuthUser m` associated type from `OAuthStateStore` provides the concrete type at instantiation (e.g., `AuthorizationCode (OAuthUser m)`). Storing the full user simplifies token exchange (no user cache/lookup needed) and auth codes are short-lived anyway. Access tokens and refresh tokens are bearer tokens validated by JWT signature. `PendingAuthorization` tracks session state before authentication completes and does not contain user identity.
- **FR-042**: Reference implementation concrete types MUST be colocated with their implementations: `AuthUser` in `Servant.OAuth2.IDP.Auth.Demo`, store error type (the concrete `OAuthStateError` for in-memory) in `Servant.OAuth2.IDP.Store.InMemory`. These types MUST NOT be imported by modules implementing against the polymorphic interface (handlers, HTTP boundary). Polymorphic code operates only via associated types (`OAuthUser m`, `OAuthStateError m`, etc.); monomorphization to reference types happens only at application composition boundaries (e.g., `Main.hs`, test setup).
- **FR-043**: System MUST provide an `AuthorizationError` type in `Servant.OAuth2.IDP.Types` with constructors matching RFC 6749 error codes: `InvalidClient`, `InvalidGrant`, `ExpiredCode`, `InvalidScope`, `UnauthorizedClient`, `InvalidRedirectUri`, `AccessDenied`, etc. Each constructor maps to a specific HTTP status code and OAuth error response format. This type is NOT an associated type (it's protocol-defined, not implementation-defined) and uses `AsType AuthorizationError e` in handler constraints.
- **FR-044**: System MUST provide a `ValidationError` type in `Servant.OAuth2.IDP.Types` for input validation failures at the handler level (distinct from parse-time 400s handled by `FromJSON`/`FromHttpApiData`). This captures semantic validation that passes parsing but fails business rules (e.g., mismatched redirect_uri for a valid URI format). Colocated with `AuthorizationError` as both are fixed protocol-level error types.
- **FR-045**: System MUST provide a reusable `domainErrorToServerError` function in `Servant.OAuth2.IDP.Boundary` for translating domain errors to Servant's `ServerError`. Signature: `domainErrorToServerError :: (MonadIO m, AsType (OAuthStateError m') e, AsType AuthorizationError e, AsType ValidationError e, AsType (AuthBackendError m') e) => IOTracer trace -> e -> m (Maybe ServerError)`. Returns `Nothing` if no prism matches, allowing consumers to chain translators or provide fallbacks. This function pattern-matches via `AsType` prisms and maps each domain error to the appropriate HTTP status code and OAuth-compliant response body. Security policy: `OAuthStateError m'` and `AuthBackendError m'` are logged internally via tracer, returning generic responses (500/401) to clients (MUST NOT expose implementation details). `ValidationError` and `AuthorizationError` are safe to expose (protocol-defined, no infrastructure secrets). Servant consumers use this in their `hoistServer` natural transformation, handling `Nothing` according to their needs.
- **FR-046**: The `mcpApp` function MUST be defined in `MCP.Server.HTTP` (alongside the HTTP transport, separate from StdIO transport and outside the OAuth2 namespace). It MUST accept a natural transformation `(∀ a. m a -> Handler a)` as a parameter, enabling callers to select the concrete typeclass implementations (`OAuthStateStore`, `AuthBackend`, etc.) at application composition time. Signature pattern: `mcpApp :: (∀ a. m a -> Handler a) -> Application`. This follows standard Servant `hoistServer` patterns. The server remains fully polymorphic over `m`; callers provide the interpretation that wires in their chosen backends (in-memory, PostgreSQL, LDAP, etc.) and handles error translation to `ServerError`. Module placement in `MCP.Server.HTTP` enables clean future extraction of OAuth2 to a separate package.
- **FR-047**: Route composition MUST use type-level API combination (`type FullAPI = MCPAPI :<|> OAuthAPI`) with a single polymorphic server. This preserves unified type signatures across all handlers, allows typeclass constraints to flow naturally, and supports future package extraction (OAuth API type can be imported from a separate package and combined at this level).
- **FR-048**: System MUST provide two separate entry points in `MCP.Server.HTTP`: (1) `mcpApp` for MCP-only mode (no OAuth2 constraints, serves only MCP core routes), (2) `mcpAppWithOAuth` for full mode (MCP + OAuth2 routes, requires `OAuthStateStore`/`AuthBackend` constraints). **Both functions MUST accept a `(∀ a. m a -> Handler a)` natural transformation parameter**, following the same pattern as FR-046. Signature pattern for `mcpAppWithOAuth`: `mcpAppWithOAuth :: (OAuthStateStore m, AuthBackend m, ...) => (∀ a. m a -> Handler a) -> Application`. CLI flag (`--oauth`) selects which entry point to use at startup. This separation avoids runtime type gymnastics, keeps type signatures explicit and self-documenting, and ensures MCP functionality works independently of OAuth2.
- **FR-049**: All OAuth2/authentication modules MUST be relocated to the `Servant.OAuth2.IDP.*` namespace (outside `MCP.*` namespace) within this package to establish explicit package boundaries. Module mapping: `MCP.Server.OAuth.Types` → `Servant.OAuth2.IDP.Types`, `MCP.Server.OAuth.Store` → `Servant.OAuth2.IDP.Store`, `MCP.Server.OAuth.InMemory` → `Servant.OAuth2.IDP.Store.InMemory`, `MCP.Server.OAuth.Boundary` → `Servant.OAuth2.IDP.Boundary`, `MCP.Server.Auth.Backend` → `Servant.OAuth2.IDP.Auth.Backend`, `MCP.Server.Auth.Demo` → `Servant.OAuth2.IDP.Auth.Demo`. The `Servant.OAuth2.IDP` namespace signals this is a reusable OAuth2 Identity Provider implementation for Servant applications, not MCP-specific. MCP modules import from `Servant.OAuth2.IDP` when OAuth2 functionality is needed. Future package extraction becomes "cut along the dotted line."
- **FR-050**: The `mkRedirectUri` smart constructor MUST use **exact hostname matching** for localhost exemption, NOT substring matching. Valid localhost patterns: `localhost`, `127.0.0.1`, `[::1]`. URLs like `http://evil.com?localhost=bypass` MUST be rejected. Implementation: extract hostname via `uriRegName` from parsed URI, compare against allowlist.
- **FR-051**: The `mkRedirectUri` smart constructor MUST block private/internal IP ranges by default: `10.0.0.0/8`, `172.16.0.0/12`, `192.168.0.0/16`, `169.254.0.0/16` (link-local/cloud metadata), `127.0.0.0/8` (except explicit localhost allowlist). This prevents SSRF attacks against internal infrastructure.
- **FR-052**: Redirect URI validation MUST require HTTPS by default. HTTP is permitted ONLY for explicit localhost hostnames (`localhost`, `127.0.0.1`, `[::1]`) to support local development. A development mode flag MAY relax this for testing scenarios but MUST NOT be enabled in production configurations.
- **FR-053**: The unused `introspectToken` function and `discoverOAuthMetadata` function MUST be removed from the library. These functions make outbound HTTP requests to arbitrary URLs, creating SSRF attack surface. Token validation uses local JWT verification; external introspection endpoints are out of scope. Attack surface reduction: remove code that isn't used.
- **FR-054**: The library MUST NOT export unsafe constructors (e.g., `unsafeRedirectUri`) that bypass validation. All type construction MUST go through smart constructors that enforce security invariants. Internal modules may use pattern synonyms or internal constructors, but public API exports only validated construction paths.
- **FR-055**: ALL security-sensitive random values MUST use cryptographically secure random number generation. This includes: authorization codes, access tokens, refresh tokens, session IDs, PKCE code verifiers, client secrets, and any other security tokens. Implementation MUST use `Crypto.Random.getRandomBytes` from `crypton`, NOT `System.Random`. This is a library invariant enforced in library code, not delegated to consumers.
- **FR-056**: The library provides OAuth protocol endpoints (authorize, token, register, metadata) but the **login page UI is the host application's responsibility**. The library MAY provide a reference/demo login page for development, but production deployments implement their own login route with appropriate branding, security headers, CSP, and UX. The library provides: (1) types for login form data, (2) credential validation via `AuthBackend`, (3) session/pending authorization management. The host app provides: login page HTML, form handling route, security headers.
- **FR-057**: The library MUST depend on `crypton` for all cryptographic operations. The library MUST NOT depend on deprecated `cryptonite`. This ensures the library receives security patches and follows the Haskell ecosystem's migration from the unmaintained `cryptonite` to its actively maintained fork `crypton`. All imports from `Crypto.*` namespaces MUST come from `crypton` packages.
- **FR-058**: System MUST define `OAuthState` newtype in `Servant.OAuth2.IDP.Types` for the OAuth `state` parameter (CSRF protection token per RFC 6749 Section 10.12). The newtype MUST implement `FromHttpApiData`, `ToHttpApiData`, `FromJSON`, `ToJSON`. Raw `Text` MUST NOT be used for state values in API definitions, data types, or handler code.
- **FR-059**: System MUST define `ResourceIndicator` newtype in `Servant.OAuth2.IDP.Types` for the OAuth `resource` parameter (RFC 8707 Resource Indicators). The newtype MUST implement `FromHttpApiData`, `ToHttpApiData`, `FromJSON`, `ToJSON`. This type MUST be used in: (1) authorize endpoint `resource` query param, (2) `TokenRequest.reqResource` field, (3) all token handler code dealing with resource indicators. Raw `Text` MUST NOT be used for resource indicator values.
- **FR-060**: The existing `Scope` newtype MUST be used consistently for all OAuth scope values. This includes: (1) authorize endpoint `scope` query param, (2) `TokenResponse.scope` field (change from `Maybe Text` to `Maybe (Set Scope)` or space-delimited `Text` with `Scope` parsing), (3) `PendingAuthorization` scope fields, (4) all handler code. The `FromHttpApiData` instance for `Scope` MUST handle space-delimited scope strings per RFC 6749 Section 3.3.
- **FR-061**: `ClientRegistrationRequest` MUST use `NonEmpty RedirectUri` for `redirect_uris`, `NonEmpty GrantType` for `grant_types`, and `NonEmpty ResponseType` for `response_types`. The `FromJSON` instance MUST reject empty lists at parse time (400 Bad Request). This makes illegal states (clients with no redirect URIs) unrepresentable.
- **FR-062**: `ClientRegistrationResponse` MUST use type-safe newtypes: `ClientId` for `client_id`, `ClientSecret` newtype for `client_secret` (empty for public clients), `ClientName` newtype for `client_name`. List fields (`redirect_uris`, `grant_types`, `response_types`) MUST use `NonEmpty` matching the request type. System MUST define `ClientSecret` and `ClientName` newtypes in `Servant.OAuth2.IDP.Types` with appropriate `ToJSON`/`FromJSON` instances.
- **FR-063**: `TokenResponse` MUST use type-safe newtypes: `AccessToken` newtype for `access_token`, `TokenType` newtype for `token_type` (typically "Bearer"), `RefreshToken` newtype for `refresh_token` (optional field), `Scope` for `scope` field (per FR-060). System MUST define `AccessToken`, `TokenType`, and `RefreshToken` newtypes in `Servant.OAuth2.IDP.Types` with appropriate serialization instances. The `expires_in` field remains `Int` (seconds per RFC 6749).

### Key Entities

- **OAuthStateStore**: Typeclass abstracting persistence of OAuth protocol state (authorization codes, tokens, clients, sessions). Requires `MonadTime m` for expiry filtering. Defines associated types `OAuthStateError`, `OAuthStateEnv`, and `OAuthUser m` (full user for JWT and auth code storage).
- **AuthBackend**: Typeclass abstracting user authentication/credential validation. Defines associated types `AuthBackendError`, `AuthBackendEnv`, and `AuthBackendUser m` for implementation-specific failures, environment, and user representation. The `validateCredentials` method returns `Maybe (AuthBackendUser m)` on success. User identifiers are fields within `AuthBackendUser m`, encoded into JWT claims via `ToJWT`.
- **AuthorizationCode user**: Parameterized data type from `Servant.OAuth2.IDP.Types` representing an authorization code with PKCE, expiry, and authenticated user. Uses type-safe newtypes (`AuthCodeId`, `CodeChallenge`, etc.). The `user` type parameter is instantiated to `OAuthUser m` at use sites, storing the full user for direct use during token exchange.
- **ClientInfo**: Data type from `Servant.OAuth2.IDP.Types` representing a registered OAuth client. Uses type-safe newtypes (`ClientId`, `NonEmpty RedirectUri`, `Set GrantType`).
- **PendingAuthorization**: Data type from `Servant.OAuth2.IDP.Types` representing an in-progress login session. Uses type-safe newtypes (`ClientId`, `RedirectUri`, `CodeChallenge`, etc.) for all OAuth parameters (validated at request parse time).
- **AuthUser**: User type for the reference in-memory implementation we ship. Defined in `Servant.OAuth2.IDP.Auth.Demo` alongside the demo `AuthBackend` instance. NOT a "default" - it is one example among equals. Custom implementations define their own user types via the `AuthBackendUser m` and `OAuthUser m` associated types; the type equality constraint ensures they agree. Must NOT be imported by polymorphic handler code.
- **UserId (reference)**: User identifier type for the reference in-memory implementation, stored as a field within `AuthUser`. Defined in `Servant.OAuth2.IDP.Auth.Demo`. Custom implementations include their own user identifier fields within their `AuthBackendUser m` type. Must NOT be imported by polymorphic handler code.
- **Error Prisms**: Constraints using `AsType` from generic-lens to inject/project domain-specific errors into a unified error sum type. Four domain error types: `OAuthStateError m` (associated, 500, details logged not exposed), `AuthBackendError m` (associated, 401, details logged not exposed), `ValidationError` (fixed, 400, safe to expose), `AuthorizationError` (fixed, OAuth-specific 4xx per RFC 6749, safe to expose). Framework errors like `ServerError` are NOT injected via prisms but translated at the framework boundary.
- **AuthorizationError**: Protocol-defined error type in `Servant.OAuth2.IDP.Types` with constructors per RFC 6749: `InvalidClient`, `InvalidGrant`, `ExpiredCode`, `InvalidScope`, `UnauthorizedClient`, `InvalidRedirectUri`, `AccessDenied`, etc. NOT an associated type—it's fixed by OAuth spec, not implementation-variable. Maps to specific HTTP status codes and OAuth error response JSON format.
- **ValidationError**: Semantic validation failures at handler level (e.g., redirect_uri doesn't match registered client, unsupported grant_type for client). Distinct from parse-time 400s (handled by `FromJSON`/`FromHttpApiData`). Maps to 400 Bad Request.
- **Environment Lenses**: Constraints using `HasType` from generic-lens to access typeclass-specific environments from a unified environment product type.
- **RedirectTarget**: Newtype wrapping redirect URL `Text` for type-safe `Header "Location" RedirectTarget` usage in HTTP 3xx responses. Name describes the semantic purpose (where to redirect), not the header name.
- **SessionCookie**: Newtype wrapping cookie value `Text` for type-safe `Header "Set-Cookie" SessionCookie` usage. Distinct type from `RedirectTarget` makes position-swap bugs a compile error.
- **OAuthState**: Newtype in `Servant.OAuth2.IDP.Types` for the OAuth `state` parameter (CSRF protection token per RFC 6749 Section 10.12). Opaque string that clients generate and expect to receive unchanged in the redirect callback. Disambiguates from Haskell's `State` monad.
- **ResourceIndicator**: Newtype in `Servant.OAuth2.IDP.Types` for the OAuth `resource` parameter (RFC 8707 Resource Indicators). Identifies the resource server for which the access token is requested. Used in authorize and token endpoints.
- **Scope**: Existing newtype in `Servant.OAuth2.IDP.Types` for OAuth scope values. Per RFC 6749 Section 3.3, scopes are space-delimited in wire format. Used consistently in API params, `TokenResponse`, `PendingAuthorization`, and handlers.
- **ClientSecret**: Newtype in `Servant.OAuth2.IDP.Types` for OAuth client secrets. Empty string for public clients per RFC 6749. Used in `ClientRegistrationResponse`.
- **ClientName**: Newtype in `Servant.OAuth2.IDP.Types` for human-readable client names. Used in `ClientRegistrationRequest` and `ClientRegistrationResponse`.
- **AccessToken**: Newtype in `Servant.OAuth2.IDP.Types` for OAuth access tokens (opaque bearer tokens). Used in `TokenResponse.access_token`.
- **TokenType**: Newtype in `Servant.OAuth2.IDP.Types` for OAuth token types (typically "Bearer"). Used in `TokenResponse.token_type`.
- **RefreshToken**: Newtype in `Servant.OAuth2.IDP.Types` for OAuth refresh tokens. Used in `TokenResponse.refresh_token` (optional).
- **TestConfig**: Configuration record for the polymorphic conformance test suite, bundling: `runM` (natural transformation), `advanceTime` callback, `TestCredentials`, and Application factory. Implementations provide this to run the conformance suite against their backend.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: All existing OAuth integration tests pass without modification when using the reference implementations.
- **SC-002**: A developer can implement a new storage backend by providing instances for the typeclasses without modifying core server code.
- **SC-003**: The abstraction adds no measurable latency overhead (within 5%) for typical OAuth operations when using in-memory implementations.
- **SC-004**: Type errors at compile time prevent mixing incompatible implementations (e.g., using IO-based storage with a pure test monad).
- **SC-005**: The same handler functions can be used in non-Servant contexts (e.g., tests, CLI) by providing appropriate error interpreters.

## Assumptions

- The server will be parameterized over the typeclass constraints at the top level (e.g., `runServerHTTP` gains additional constraints).
- Each typeclass defines associated error and environment types; implementations define their specific failure modes and configuration needs.
- Handler functions use `MonadError e m` with `AsType` prism constraints from generic-lens to compose errors.
- Handler functions use `MonadReader env m` with `HasType` lens constraints from generic-lens to compose environments.
- Servant integration uses `hoistServer` to translate the polymorphic `m` to `Handler`, mapping errors to `ServerError`.
- The current `OAuthConfig` may be split or restructured to fit into the environment types.
- JWT signing and validation remain unchanged; only state persistence and credential validation are abstracted.
- The `generic-lens` package provides both `AsType` (for error prisms) and `HasType` (for environment lenses) constraints.
- All OAuth domain types (including response types like `RedirectTarget`, `SessionCookie`) belong in `OAuth.Types`, not in HTTP/Servant modules. Design principle: OAuth combinators must be framework-agnostic; Servant is a production-ready reference implementation, not the only target.
- Handler functions are polymorphic constructive proofs of correct implementability, NOT Servant-specific code. They will be reused in a Yesod application; Servant provides the typed API spec and production reference implementation for development and testing.
- Monomorphization to concrete types (`AppM`, `Handler`, test monads) happens at framework boundaries via `hoistServer` or equivalent, never in handler definitions.
- Reference implementation types (`AuthUser`, `UserId`, concrete `OAuthStateError` type, concrete `AuthBackendError` type) are internal to their implementation modules and never imported by polymorphic handler code. This prevents accidental coupling to the reference implementation and ensures handlers work with any conforming backend.
