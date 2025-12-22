# Feature Specification: Remove MCP Imports from Servant.OAuth2.IDP

**Branch**: `005-servant-oauth-extraction`
**Status**: In Planning
**Date**: 2025-12-18

## Summary

Prepare the `Servant.OAuth2.IDP.*` modules for extraction to a separate package by removing all `MCP.*` dependencies. This is a refactoring task that moves types and functions from MCP modules to new Servant modules, using explicit parameters and record types rather than new typeclasses.

## Clarifications

### Session 2025-12-19

- Q: How should OAuthTrace duplication be resolved? → A: Delete MCP.Trace.OAuth entirely; MCP.Trace.HTTP imports OAuthTrace directly from Servant.OAuth2.IDP.Trace (clean break, no re-exports)
- Q: Where should renderOAuthTrace live? → A: Add renderOAuthTrace to Servant.OAuth2.IDP.Trace alongside the ADT (cohesion principle). Old version in MCP.Trace.OAuth is deleted with the module. New version must be adapted to unwrap domain newtypes (unClientId, unSessionId, unUsername, unScope, etc.) when rendering to Text
- Q: Where should generateCodeVerifier be located? → A: Move to Servant.OAuth2.IDP.PKCE (module boundaries are domain-based, not IO vs pure)
- Q: Where should OAuthGrantType be located? → A: Move to Servant.OAuth2.IDP.Types (alongside other OAuth protocol types)
- Q: How should OAuthTrace constructors use domain types? → A: Use existing domain newtypes (Username, UserId, RedirectUri) + minimal new ADTs (OperationResult, DenialReason) - balanced approach
- Q: How should OAuthConfig be split between Servant and MCP? → A: Split into OAuthEnv (Servant, protocol config) + MCPOAuthConfig (MCP, demo fields only)
- Q: Where should all error types be organized? → A: Move all to Servant.OAuth2.IDP.Errors (consolidate ValidationError, AuthorizationError, LoginFlowError)
- Q: Should supported* configuration lists use NonEmpty? → A: Yes for supportedResponseTypes, supportedGrantTypes, supportedAuthMethods, supportedCodeChallengeMethods (RFC requires ≥1); keep [Scope] for supportedScopes (can be empty)
- Q: Should type precision improvements be included in this extraction? → A: Yes, include critical fixes (OAuth error codes ADT, generator return types, storage keys) - prevents future breaking changes
- Q: How should OAuthErrorResponse.oauthErrorCode be typed? → A: Create OAuthErrorCode ADT (ErrInvalidRequest | ErrInvalidClient | ErrInvalidGrant | ...) with ToJSON to snake_case per RFC 6749
- Q: Should generator functions return domain types? → A: Yes, all generators return domain types (IO AuthCodeId, m AccessTokenId, IO RefreshTokenId). CRITICAL: Domain types must be threaded throughout (no Text conversions mid-flow), and smart constructor hygiene enforced (export only smart constructors, NOT type constructors) to prove correct construction
- Q: How should LoginForm.formAction be typed? → A: Create LoginAction ADT (ActionApprove | ActionDeny) with FromHttpApiData/ToHttpApiData - enables exhaustiveness checking, prevents typos
- Q: How should TokenResponse.expires_in be typed? → A: Create newtype over NominalDiffTime (e.g., `newtype TokenValidity = TokenValidity NominalDiffTime`) with custom ToJSON instance outputting integer seconds for OAuth wire format compliance. Name denotes what it IS (token validity duration), not the field name.
- Q: MCPOAuthConfig field name collision with OAuthConfig - how to resolve? → A: Remove OAuthConfig entirely (Option B). OAuthConfig is replaced by OAuthEnv (Servant) + MCPOAuthConfig (MCP). With OAuthConfig gone, MCPOAuthConfig uses unprefixed field names (autoApproveAuth, demoUserIdTemplate, etc.). OAuthEnv lives in AppEnv.envOAuthEnv; MCPOAuthConfig lives in HTTPServerConfig.httpMCPOAuthConfig (presence = OAuth enabled). Provide DemoOAuthBundle convenience type for test migration.
- Q: How should AuthorizationError Text payloads be typed? → A: Create precise ADTs for each error constructor payload (e.g., InvalidRequestReason, InvalidClientReason, etc.). Text rendering is a UI concern; internally use precise enums for exhaustive pattern matching and performance.
- Q: How should token handler parameters be typed (currently Map Text Text)? → A: Pass typed fields directly from existing `TokenRequest` ADT (already has AuthCodeId, CodeVerifier, RefreshTokenId, ResourceIndicator). No new types needed - just fix handler signatures to accept typed params instead of Map. Text/String/Bytes only at system boundaries (FromForm instance).
- Q: Which types violate smart constructor hygiene (export raw constructors despite having validation)? → A: 9 types in Types.hs and Auth/Backend.hs export `(..)` but have smart constructors with validation. Fix by changing exports from `Type(..)` to `Type` (type only). Critical: RedirectUri (bypasses SSRF protection), SessionId (bypasses UUID validation), Scope (bypasses RFC compliance), Username (bypasses auth validation). Standard: AuthCodeId, ClientId, RefreshTokenId, UserId, ClientName.
- Q: Should ClientInfo.clientName use the ClientName newtype? → A: Yes, change `clientName :: Text` to `clientName :: ClientName` - the newtype already exists with validation.
- Q: How should MalformedRequest carry error context? → A: Create specific MalformedReason ADT enumerating causes (exhaustive pattern matching, no Text escape hatch)
- Q: How should handleProtectedResourceMetadata get resource server config without depending on HTTPServerConfig? → A: Add `resourceServerBaseUrl :: URI` and `resourceServerMetadata :: ProtectedResourceMetadata` fields to OAuthEnv (not optional override - direct config). Handler becomes trivial (just returns config value). MCP constructs ProtectedResourceMetadata when building OAuthEnv.
- Q: How should handlers access OAuthTrace tracer without importing MCP.Trace.HTTP? → A: Handlers use `HasType (IOTracer OAuthTrace) env` constraint. MCP's AppEnv provides `IOTracer OAuthTrace` field (constructed via `contramap HTTPOAuth` from main HTTPTrace tracer). Servant modules never import HTTPTrace.
- Q: How should "MCP Server" branding in HTML templates be handled? → A: Add `oauthServerName :: Text` field to OAuthEnv. HTML templates use this config value for titles ("Sign In - {serverName}"). MCP sets it to "MCP Server"; other users can customize. Scope descriptions (mcp:read etc.) also become configurable via `oauthScopeDescriptions :: Map Scope Text` or similar.

### Session 2025-12-20

- Q: Should UnsupportedCodeChallengeMethod be ValidationError or InvalidRequestReason? → A: Keep in ValidationError (line 126 correct, line 139 was erroneous duplication). Semantically a validation failure (client sent unsupported method), not a protocol-level authorization error.

### Session 2025-12-22

- Q: Should unsafe* constructors be eliminated and how? → A: Delete Boundary module entirely; delete ALL unsafe* exports from Types. Typeclass instances (ToJSON, FromJSON, ToHttpApiData, FromHttpApiData) ARE the boundary and MUST live in type-defining modules. Arbitrary instances MUST also live in type-defining modules. Module cohesion follows from which types need to "see" each other's internals for instance definitions. Only export type names (not constructors) except for types structurally unable to encode invalid values (NonEmpty, Bool, etc.).
- Q: How should all OAuth errors be unified for ServerError conversion? → A: Single `OAuthError m` sum type with constructors `OAuthValidation ValidationError | OAuthAuthorization AuthorizationError | OAuthLoginFlow LoginFlowError | OAuthStore (OAuthStateError m)`. Define `oauthErrorToServerError :: OAuthError m -> ServerError` in same module (Servant.OAuth2.IDP.Errors). Enables exhaustive pattern matching and single error-handling boundary.
- Q: How should oauthErrorToServerError render error bodies? → A: OAuthAuthorization → JSON OAuthErrorResponse per RFC 6749; OAuthValidation → descriptive text (safe to expose client errors); OAuthLoginFlow → descriptive text; OAuthStore → generic "Internal Server Error" (no backend detail leakage). Requires Show constraint on OAuthStateError m for logging (not for response body).
- Q: Should QuickCheck be a library dependency for Arbitrary instances? → A: Yes. QuickCheck becomes library dependency. GHC dead code elimination removes unused Arbitrary instances from production binaries. QuickCheck is stable and reliable.
- Q: How should test files construct values without unsafe* constructors? → A: Tests are library consumers - use smart constructors (handle Maybe/Either) and Arbitrary instances. No special test privileges; same API visibility as any other consumer code.
- Q: Where should generator functions live for constructor access? → A: Move ALL generators to Types.hs. Delete Handlers/Helpers.hs entirely. GOLDEN RULE: The only code requiring audit to verify correct value construction lives in the type-defining module. Future refactoring may split into strongly-connected type modules, but consolidate in Types.hs for now.
- Q: Should OAuth ID generation use crypto-random instead of UUID.nextRandom? → A: Yes. RFC 6749 Section 10.10 mandates sufficient entropy to prevent guessing. UUID.nextRandom uses system RNG (not crypto-secure). Create generateAuthCodeId, generateClientId, generateRefreshTokenId, generateSessionId in Types.hs using crypton/entropy (128+ bits). Return domain types directly (not Maybe) since crypto generation cannot fail. Eliminates impossible-case error handling. See bead mcp-5wk.96.10.

## Goals

1. **Package Independence**: `Servant.OAuth2.IDP.*` modules should have zero imports from `MCP.*` namespace
2. **Clean Break**: No backwards compatibility re-exports - MCP modules that moved types simply remove them
3. **Minimal API Surface**: Use explicit record parameters rather than introducing new typeclasses

## Non-Goals

- Creating a separate Hackage package (that's a future step)
- Adding new functionality to OAuth
- Changing OAuth behavior

## Requirements

### Functional Requirements

#### FR-001: Move MonadTime Import
**Priority**: P0 (Critical)
**Description**: Replace all `import MCP.Server.Time (MonadTime)` with direct `import Control.Monad.Time (MonadTime)`
**Files Affected**:
- `src/Servant/OAuth2/IDP/Store.hs`
- `src/Servant/OAuth2/IDP/Store/InMemory.hs`
- `src/Servant/OAuth2/IDP/Handlers/Authorization.hs`
- `src/Servant/OAuth2/IDP/Handlers/Login.hs`
- `src/Servant/OAuth2/IDP/Test/Internal.hs`

#### FR-002: Create Servant.OAuth2.IDP.Metadata Module
**Priority**: P0 (Critical)
**Description**: Move `OAuthMetadata` and `ProtectedResourceMetadata` types from `MCP.Server.Auth` to new `Servant.OAuth2.IDP.Metadata` module
**Types to Move**:
- `OAuthMetadata` (RFC 8414 discovery response)
- `ProtectedResourceMetadata` (RFC 9728)
- All associated JSON instances

#### FR-002b: Move OAuthGrantType to Servant.OAuth2.IDP.Types
**Priority**: P0 (Critical)
**Description**: Move `OAuthGrantType` enum from `MCP.Server.Auth` to existing `Servant.OAuth2.IDP.Types` module (core OAuth 2.1 protocol type per RFC 6749)
**Types to Move**:
- `OAuthGrantType` data type with constructors (AuthorizationCode, RefreshToken, etc.)
- All associated JSON instances

#### FR-003: Create Servant.OAuth2.IDP.PKCE Module
**Priority**: P0 (Critical)
**Description**: Move PKCE functions from `MCP.Server.Auth` to new `Servant.OAuth2.IDP.PKCE` module (domain-based boundaries, not IO vs pure)
**Functions to Move**:
- `generateCodeVerifier :: IO CodeVerifier` (IO action using cryptonite random, returns domain newtype)
- `validateCodeVerifier :: CodeVerifier -> CodeChallenge -> Bool`
- `generateCodeChallenge :: CodeVerifier -> CodeChallenge` (takes and returns domain newtypes)

#### FR-004: Create Servant.OAuth2.IDP.Config Module
**Priority**: P1 (High)
**Description**: Define `OAuthEnv` record containing protocol-agnostic OAuth configuration. OAuthConfig is REMOVED entirely and replaced by OAuthEnv (Servant) + MCPOAuthConfig (MCP).
**OAuthEnv Fields** (protocol config, moves to Servant):
- `requireHTTPS :: Bool` (security flag checked by handlers)
- `baseUrl :: URI`
- `authCodeExpiry :: NominalDiffTime`
- `accessTokenExpiry :: NominalDiffTime`
- `loginSessionExpiry :: NominalDiffTime`
- `authCodePrefix :: Text`
- `refreshTokenPrefix :: Text`
- `clientIdPrefix :: Text`
- `supportedScopes :: [Scope]` (can be empty - no required scopes)
- `supportedResponseTypes :: NonEmpty ResponseType` (RFC requires ≥1)
- `supportedGrantTypes :: NonEmpty OAuthGrantType` (RFC requires ≥1)
- `supportedAuthMethods :: NonEmpty TokenAuthMethod` (RFC requires ≥1)
- `supportedCodeChallengeMethods :: NonEmpty CodeChallengeMethod` (RFC requires ≥1)
- `resourceServerBaseUrl :: URI` (base URL for protected resource metadata)
- `resourceServerMetadata :: ProtectedResourceMetadata` (RFC 9728 metadata for resource server)
- `oauthServerName :: Text` (branding for HTML templates, e.g., "MCP Server", "OAuth Server")
- `oauthScopeDescriptions :: Map Scope Text` (human-readable scope descriptions for consent page)
**OAuthEnv Location**: Lives in `AppEnv.envOAuthEnv` (always present when OAuth wired)
**MCPOAuthConfig** (demo/MCP fields, stays in MCP.Server.Auth, NO mcp prefix):
- `autoApproveAuth :: Bool` (demo mode auto-approval)
- `oauthProviders :: [OAuthProvider]` (external IdP list for federated login)
- `demoUserIdTemplate :: Maybe Text` (template for demo user IDs, Nothing = no demo)
- `demoEmailDomain :: Text` (domain for demo emails)
- `demoUserName :: Text` (display name for demo users)
- `publicClientSecret :: Maybe Text` (secret returned for public clients)
- `authorizationSuccessTemplate :: Maybe Text` (HTML template for success page)
**MCPOAuthConfig Location**: Lives in `HTTPServerConfig.httpMCPOAuthConfig :: Maybe MCPOAuthConfig` (presence = OAuth enabled)
**Eliminated Fields**: `oauthEnabled` (redundant - MCPOAuthConfig presence implies enabled), `credentialStore` (via AuthBackend typeclass)
**Migration Aid**: Create `DemoOAuthBundle` convenience type combining OAuthEnv + MCPOAuthConfig for test/demo setups

#### FR-004b: Create Servant.OAuth2.IDP.Errors Module
**Priority**: P0 (Critical)
**Description**: Consolidate all error types from various modules into new `Servant.OAuth2.IDP.Errors` module
**ValidationError Type** (currently in Servant.OAuth2.IDP.Types, move to Errors):
**New Constructors to Add**:
- `UnsupportedCodeChallengeMethod CodeChallengeMethod` (currently uses AuthorizationError InvalidRequest)
- `MissingTokenParameter TokenParameter` (for missing code/code_verifier/refresh_token)
- `InvalidTokenParameterFormat TokenParameter Text` (for parse errors with parameter name and error detail)
- `EmptyRedirectUris` (for registration with empty redirect_uris list)
**Existing Constructors** (move from Types):
- `RedirectUriMismatch ClientId RedirectUri`
- `UnsupportedResponseType Text`
- `ClientNotRegistered ClientId`
- `MissingRequiredScope Scope`
- `InvalidStateParameter Text`
**AuthorizationError Type** (currently in Servant.OAuth2.IDP.Types, move to Errors):
- Move type and replace Text payloads with precise ADTs
- `data MalformedReason = InvalidUriSyntax Text | DuplicateParameter Text | UnparseableBody Text` (exhaustive enumeration of malformation causes not covered by other InvalidRequestReason constructors)
- `data InvalidRequestReason = MissingParameter TokenParameter | InvalidParameterFormat TokenParameter | MalformedRequest MalformedReason`
- `data InvalidClientReason = ClientNotFound ClientId | InvalidClientCredentials | ClientSecretMismatch`
- `data InvalidGrantReason = CodeNotFound AuthCodeId | CodeExpired AuthCodeId | CodeAlreadyUsed AuthCodeId | RefreshTokenNotFound RefreshTokenId | RefreshTokenExpired RefreshTokenId | RefreshTokenRevoked RefreshTokenId`
- `data UnauthorizedClientReason = GrantTypeNotAllowed OAuthGrantType | ScopeNotAllowed Scope | RedirectUriNotRegistered RedirectUri`
- `data UnsupportedGrantTypeReason = UnknownGrantType Text | GrantTypeDisabled OAuthGrantType`
- `data InvalidScopeReason = UnknownScope Text | ScopeNotPermitted Scope`
- `data AccessDeniedReason = UserDenied | ResourceOwnerDenied | ConsentRequired`
- Updated constructors: `InvalidRequest InvalidRequestReason | InvalidClient InvalidClientReason | InvalidGrant InvalidGrantReason | UnauthorizedClient UnauthorizedClientReason | UnsupportedGrantType UnsupportedGrantTypeReason | InvalidScope InvalidScopeReason | AccessDenied AccessDeniedReason | ExpiredCode | InvalidRedirectUri | PKCEVerificationFailed`
- Create `renderAuthorizationError :: AuthorizationError -> Text` for human-readable error_description (UI layer)
**LoginFlowError Type** (currently in Servant.OAuth2.IDP.LoginFlowError module, move to Errors):
- Move entire type as-is (no changes needed)
- Delete `src/Servant/OAuth2/IDP/LoginFlowError.hs` after moving
**New Supporting Types**:
- `data TokenParameter = TokenParamCode | TokenParamCodeVerifier | TokenParamRefreshToken deriving (Eq, Show)`
- `data OAuthErrorCode = ErrInvalidRequest | ErrInvalidClient | ErrInvalidGrant | ErrUnauthorizedClient | ErrUnsupportedGrantType | ErrInvalidScope | ErrAccessDenied | ErrUnsupportedResponseType | ErrServerError | ErrTemporarilyUnavailable` (RFC 6749 error codes with ToJSON to snake_case)
**Unified OAuthError Type** (root error type for all OAuth errors):
- `data OAuthError m = OAuthValidation ValidationError | OAuthAuthorization AuthorizationError | OAuthLoginFlow LoginFlowError | OAuthStore (OAuthStateError m)`
- `oauthErrorToServerError :: Show (OAuthStateError m) => OAuthError m -> ServerError` (exhaustive pattern matching to HTTP status codes)
  - `OAuthValidation` → 400 Bad Request
  - `OAuthAuthorization` → varies (400/401/403 depending on error code per RFC 6749)
  - `OAuthLoginFlow` → varies (400/401/404 depending on cause)
  - `OAuthStore` → 500 Internal Server Error (storage failures)
- **Error Body Rendering**:
  - `OAuthAuthorization` → JSON `OAuthErrorResponse` per RFC 6749 (`{"error":"...","error_description":"..."}`)
  - `OAuthValidation` → descriptive plain text (client errors safe to expose)
  - `OAuthLoginFlow` → descriptive plain text (user-facing errors)
  - `OAuthStore` → generic "Internal Server Error" text (no backend detail leakage; `Show` constraint used for logging only)
- Lives in `Servant.OAuth2.IDP.Errors` alongside other error types
- Handlers use `ExceptT (OAuthError m) m a` for uniform error handling
**Type Precision Fixes**:
- Change `OAuthErrorResponse.oauthErrorCode :: Text` to `OAuthErrorResponse.oauthErrorCode :: OAuthErrorCode`
- Update `authorizationErrorToResponse` to use OAuthErrorCode ADT (enables exhaustiveness checking)

#### FR-004c: Type Precision and Smart Constructor Hygiene
**Priority**: P0 (Critical)
**Description**: Enforce type-driven design principles across all Servant.OAuth2.IDP.* modules
**Reference**: See `.specify/memory/constitution.md` Principle I (Type-Driven Design) and Development Standards (Naming Conventions)
**Domain-Centric Naming** (MUST enforce):
- Type names MUST denote what they ARE (domain concept), not what field they populate
- Good: `TokenValidity` (describes the concept - how long a token is valid)
- Bad: `ExpiresIn` (mirrors the JSON field name, not the domain concept)
- Good: `ClientName` (describes what it IS)
- Bad: `NameField` (describes where it goes)
- This principle extends Constitution Principle I: "Domain concepts MUST have explicit types (no primitive obsession)"
**Smart Constructor Hygiene** (MUST enforce per Constitution Principle II):
- Export pattern: `module Foo (FooType, mkFooType, unFooType, ...)` - export type name but NOT constructor
- MUST NOT export: `FooType(..)` or `FooType(FooType)` - not even for tests
- MUST NOT export `unsafe*` prefix functions - delete ALL such functions
- If pattern matching is needed by consumers, export pattern synonyms instead of raw constructors
- This allows proving types are correctly constructed by construction
**Boundary = Typeclass Instances** (MUST enforce):
- `ToJSON`, `FromJSON`, `ToHttpApiData`, `FromHttpApiData` instances ARE the system boundary
- These instances MUST be defined in the same module as the type (need constructor access)
- `Arbitrary` instances MUST also live in type-defining modules (need constructor access for generation)
- Delete `Servant.OAuth2.IDP.Boundary` module entirely (no longer needed)
- Module cohesion heuristic: types that need to see each other's internals for instance definitions belong in same module
**Smart Constructor Export Fixes Required** (9 types currently violate hygiene):
- **Critical (security bypass)**:
  - `RedirectUri (..)` → `RedirectUri` (bypasses SSRF protection in mkRedirectUri)
  - `SessionId (..)` → `SessionId` (bypasses UUID format validation)
  - `Scope (..)` → `Scope` (bypasses RFC 6749 whitespace/empty validation)
  - `Username (..)` → `Username` (in Auth/Backend.hs, bypasses non-empty validation)
- **Standard (data integrity)**:
  - `AuthCodeId (..)` → `AuthCodeId` (bypasses non-empty validation)
  - `ClientId (..)` → `ClientId` (bypasses non-empty validation)
  - `RefreshTokenId (..)` → `RefreshTokenId` (bypasses non-empty validation)
  - `UserId (..)` → `UserId` (bypasses non-empty validation)
  - `ClientName (..)` → `ClientName` (bypasses non-empty validation)
- No internal modules with `(..)` exports needed - boundary instances live with types
**Generator Functions** (move from Helpers.hs to Types.hs, then delete Helpers.hs):
- `generateAuthCodeId :: Text -> IO AuthCodeId` (prefix param, crypto-random, returns type directly)
- `generateRefreshTokenId :: Text -> IO RefreshTokenId` (prefix param, crypto-random, returns type directly)
- `generateClientId :: Text -> IO ClientId` (prefix param, crypto-random, returns type directly)
- `generateSessionId :: IO SessionId` (crypto-random, returns type directly)
- `generateJWTAccessToken :: OAuthUser m -> JWTSettings -> m AccessTokenId` (JWT generation, unchanged)
- All generators return domain types directly (NOT `Maybe`/`Either`) since crypto generation cannot produce invalid values
- GOLDEN RULE: Only code in type-defining module needs audit for correct construction
**Crypto-Random Requirement** (RFC 6749 Section 10.10 compliance):
- Authorization codes, client IDs, refresh tokens, session IDs MUST use cryptographically secure randomness
- Current `Data.UUID.V4.nextRandom` uses system RNG - NOT cryptographically secure
- MUST replace with `System.Entropy.getEntropy` or `Crypto.Random` (128+ bits entropy)
- Add dependency: `crypton` or `entropy` package
- Generator signatures return type directly (not `Maybe`) since crypto generation cannot fail or produce empty output
- Eliminates awkward impossible-case error handling pattern:
  ```haskell
  -- BEFORE (anti-pattern):
  case mkAuthCodeId codeText of
      Just cid -> cid
      Nothing -> error "impossible: UUID never empty"

  -- AFTER (clean):
  generateAuthCodeId prefix  -- returns AuthCodeId directly
  ```
- Locations requiring update:
  - `Handlers/Helpers.hs:69-75` (generateAuthCode) → move to Types.hs with crypto
  - `Handlers/Helpers.hs:94-102` (generateRefreshTokenWithConfig) → move to Types.hs with crypto
  - `Handlers/Registration.hs:101-106` (client ID generation) → use generateClientId from Types.hs
  - `Handlers/Authorization.hs:158` (session ID generation) → use generateSessionId from Types.hs
- Verification: `rg 'UUID.nextRandom' src/` returns empty (except test helpers if needed)
**Type Threading** (MUST NOT convert to Text mid-flow):
- Domain types flow from generation to storage to response without unwrapping
- Storage maps use domain types as keys: `Map AuthCodeId (AuthorizationCode user)` not `Map Text ...`
- Example anti-pattern to eliminate: `AuthCodeId (unAuthCodeId x)` or `let code = unAuthCodeId id in AuthCodeId code`
**Record Field Type Fixes** (use existing newtypes instead of raw Text):
- `ClientInfo.clientName :: Text` → `ClientInfo.clientName :: ClientName` (newtype exists at Types.hs:557)
**Modules Affected**:
- `Servant.OAuth2.IDP.Types` - audit all exports for smart constructor hygiene (8 types)
- `Servant.OAuth2.IDP.Auth.Backend` - fix Username export for smart constructor hygiene
- `Servant.OAuth2.IDP.Store` - change storage key types from Text to domain types
- `Servant.OAuth2.IDP.Store.InMemory` - update OAuthState record to use domain type keys
- `Servant.OAuth2.IDP.Handlers.Helpers` - update generator signatures
**New Types to Add**:
- `data LoginAction = ActionApprove | ActionDeny` with FromHttpApiData/ToHttpApiData instances
- Update `LoginForm.formAction :: Text` to `LoginForm.formAction :: LoginAction`
- Update Login handler to pattern match on LoginAction ADT instead of string comparison
- `newtype TokenValidity = TokenValidity { unTokenValidity :: NominalDiffTime }` with custom ToJSON (outputs integer seconds for OAuth wire format) - name denotes what it IS, not the field name
- Update `TokenResponse.expires_in :: Maybe Int` to `TokenResponse.expires_in :: Maybe TokenValidity` (resolves FIXME comment)
**Token Handler Signatures** (eliminate Map Text Text anti-pattern):
- `TokenRequest` ADT already has typed fields (`AuthCodeId`, `CodeVerifier`, `RefreshTokenId`, `ResourceIndicator`) - NO new types needed
- Update `handleAuthCodeGrant :: ... -> AuthCodeId -> CodeVerifier -> Maybe ResourceIndicator -> m TokenResponse` (was `Map Text Text`)
- Update `handleRefreshTokenGrant :: ... -> RefreshTokenId -> Maybe ResourceIndicator -> m TokenResponse` (was `Map Text Text`)
- Update `handleToken` to pass typed fields directly from `TokenRequest` pattern match (remove Map.fromList/unAuthCodeId unwrapping)
- Parsing from wire format (Text) happens ONLY at Servant API boundary via `FromForm TokenRequest` instance

#### FR-005: Create Servant.OAuth2.IDP.Trace Module
**Priority**: P1 (High)
**Description**: Define `OAuthTrace` ADT for OAuth-specific trace events using domain newtypes and minimal trace-specific ADTs. Include `renderOAuthTrace :: OAuthTrace -> Text` for human-readable rendering.
**renderOAuthTrace Implementation**:
- Adapted from `MCP.Trace.OAuth.renderOAuthTrace` (old version deleted with module)
- Must unwrap domain newtypes: `unClientId`, `unSessionId`, `unUsername`, `unScope`, `unRedirectUri` (via `show` or custom URI rendering)
- Must render `OperationResult` as "SUCCESS"/"FAILED"
- Must render `DenialReason` constructors to human-readable text
- Must render `OAuthGrantType` and `ValidationError` appropriately
**Supporting Types**:
- `data OperationResult = Success | Failure` (for boolean success/failure without primitives)
- `data DenialReason = UserDenied | InvalidRequest | UnauthorizedClient | ServerError Text` (for authorization denial reasons)
**Constructors** (using domain newtypes from Servant.OAuth2.IDP.Types):
- `TraceClientRegistration ClientId RedirectUri` (use RedirectUri instead of Text)
- `TraceAuthorizationRequest ClientId [Scope] OperationResult` (use OperationResult instead of Bool)
- `TraceLoginPageServed SessionId`
- `TraceLoginAttempt Username OperationResult` (use Username and OperationResult)
- `TracePKCEValidation OperationResult` (use OperationResult)
- `TraceAuthorizationGranted ClientId Username` (use Username for authenticated user)
- `TraceAuthorizationDenied ClientId DenialReason` (use DenialReason ADT)
- `TraceTokenExchange OAuthGrantType OperationResult` (OAuthGrantType already domain type)
- `TraceTokenRefresh OperationResult`
- `TraceSessionExpired SessionId`
- `TraceValidationError ValidationError` (use ValidationError domain type, not Text primitives)

#### FR-006: Update Handler Signatures
**Priority**: P1 (High)
**Description**: Update all handler modules to use Servant types instead of MCP types
**Changes**:
- Replace `HasType HTTPServerConfig env` with `HasType OAuthEnv env`
- Replace `HasType (IOTracer HTTPTrace) env` with `HasType (IOTracer OAuthTrace) env`

#### FR-007: Update MCP.Server.HTTP.AppEnv
**Priority**: P1 (High)
**Description**: Embed `OAuthEnv` and `IOTracer OAuthTrace` in `AppEnv`
**Changes**:
- Add `envOAuthEnv :: OAuthEnv` field
- Add `envOAuthTracer :: IOTracer OAuthTrace` field (constructed via `contramap HTTPOAuth` from main tracer)
- Create `mkOAuthEnv :: HTTPServerConfig -> OAuthEnv` function (constructs ProtectedResourceMetadata from HTTPServerConfig fields)
- Tracer construction: `contramap HTTPOAuth envTracer` where `HTTPOAuth :: OAuthTrace -> HTTPTrace`

#### FR-008: Remove Types from MCP.Server.Auth
**Priority**: P2 (Medium)
**Description**: Clean break - remove OAuthConfig entirely, remove moved types, update MCPOAuthConfig to use unprefixed fields
**Types to Remove Entirely**:
- `OAuthConfig` (REMOVED - replaced by OAuthEnv in Servant + MCPOAuthConfig in MCP)
- `defaultDemoOAuthConfig` (replaced by `defaultOAuthEnv` + `defaultDemoMCPOAuthConfig` + `DemoOAuthBundle`)
**Types to Move** (to Servant.OAuth2.IDP.*):
- `OAuthMetadata` (→ Metadata)
- `ProtectedResourceMetadata` (→ Metadata)
- `OAuthGrantType` (→ Types)
- `generateCodeVerifier` (→ PKCE)
- `validateCodeVerifier` (→ PKCE)
- `generateCodeChallenge` (→ PKCE)
- `ValidationError` (→ Errors)
- `AuthorizationError` (→ Errors)
**Types to Update**:
- `MCPOAuthConfig` - remove `mcp` prefix from all fields (no collision after OAuthConfig removed), add missing fields (oauthProviders, demoUserName, publicClientSecret)
**Types Staying in MCP.Server.Auth**:
- `OAuthProvider` (MCP-specific provider configuration)
- `TokenInfo` (token introspection response, MCP-specific)
- `extractBearerToken` (utility function used by MCP handlers)
- `PKCEChallenge` (if still needed - contains both CodeChallenge and CodeVerifier for convenience)
- `ProtectedResourceAuth` (type-level auth tag for MCP)
- `ProtectedResourceAuthConfig` (config for above)
- `MCPOAuthConfig` (updated with unprefixed fields)
**New Functions in MCP.Server.HTTP**:
- `defaultOAuthEnv :: OAuthEnv` (production defaults)
- `defaultDemoMCPOAuthConfig :: MCPOAuthConfig` (demo defaults)
- `DemoOAuthBundle` type and `defaultDemoOAuthBundle :: DemoOAuthBundle` (test convenience)

## Acceptance Criteria

1. `rg "^import MCP\." src/Servant/` returns empty (no MCP imports in Servant modules)
2. `cabal build` succeeds with no errors
3. `cabal test` passes (all existing tests pass)
4. No functionality changes - this is a pure refactoring

## Dependencies

- `monad-time` package (already a dependency via MCP.Server.Time)
- `network-uri` package for URI type in OAuthEnv
- `base` package for NonEmpty from Data.List.NonEmpty (used in OAuthEnv supported* fields)
- `QuickCheck` package for Arbitrary instances in type-defining modules (GHC DCE removes from production)
- `crypton` or `entropy` package for crypto-secure random generation (RFC 6749 Section 10.10 compliance for OAuth IDs)

## Files Created (WIP - already exist)

| File | Purpose | Remaining Work |
|------|---------|----------------|
| `src/Servant/OAuth2/IDP/Trace.hs` | OAuthTrace ADT with domain types | Add `renderOAuthTrace` function (adapted from deleted MCP.Trace.OAuth, unwraps domain newtypes) |
| `src/Servant/OAuth2/IDP/Config.hs` | OAuthEnv record (protocol config) | Complete |
| `src/Servant/OAuth2/IDP/Metadata.hs` | OAuthMetadata, ProtectedResourceMetadata | Complete |
| `src/Servant/OAuth2/IDP/PKCE.hs` | PKCE functions (generate/validate, domain newtypes) | Complete |
| `src/Servant/OAuth2/IDP/Errors.hs` | All error types (ValidationError, AuthorizationError, LoginFlowError) | Complete |

## Files to Modify

| File | Changes |
|------|---------|
| `src/Servant/OAuth2/IDP/Types.hs` | Remove ValidationError, AuthorizationError (moved to Errors), add OAuthGrantType (from MCP.Server.Auth), add Arbitrary instances for all types, remove ALL unsafe* exports, absorb ALL generator functions from Helpers.hs |
| `src/Servant/OAuth2/IDP/Store.hs` | MonadTime import, Errors import |
| `src/Servant/OAuth2/IDP/Store/InMemory.hs` | MonadTime import |
| `src/Servant/OAuth2/IDP/API.hs` | Metadata import |
| `src/Servant/OAuth2/IDP/Server.hs` | Config/Trace types, Errors import |
| `src/Servant/OAuth2/IDP/Handlers/Metadata.hs` | Switch from HTTPServerConfig to OAuthEnv, simplify handleProtectedResourceMetadata to just return resourceServerMetadata field |
| `src/Servant/OAuth2/IDP/Handlers/Registration.hs` | Config/Trace types, Errors import, generators from Types.hs (Helpers.hs deleted), switch OAuthTrace import |
| `src/Servant/OAuth2/IDP/Handlers/Authorization.hs` | Config/Trace types, Errors import, MonadTime import, generators from Types.hs (Helpers.hs deleted) |
| `src/Servant/OAuth2/IDP/Handlers/Login.hs` | Config/Trace types, Errors import, MonadTime import, switch OAuthTrace import |
| `src/Servant/OAuth2/IDP/Handlers/Token.hs` | Config/Trace types, PKCE import, Errors import, generators from Types.hs (Helpers.hs deleted), switch OAuthTrace import |
| `src/Servant/OAuth2/IDP/Handlers/HTML.hs` | Use `oauthServerName` from OAuthEnv for HTML titles, use `oauthScopeDescriptions` for scope text |
| `src/Servant/OAuth2/IDP/Test/Internal.hs` | MonadTime import, fix doc comment typo |
| `src/MCP/Server/Auth.hs` | Remove moved types (clean break), create MCPOAuthConfig |
| `src/MCP/Server/HTTP/AppEnv.hs` | Add OAuthEnv field, tracer adapter, MCPOAuthConfig |
| `src/MCP/Trace/HTTP.hs` | Import OAuthTrace from Servant.OAuth2.IDP.Trace instead of MCP.Trace.OAuth |
| `src/MCP/Trace/Types.hs` | Import OAuthTrace from Servant.OAuth2.IDP.Trace instead of MCP.Trace.OAuth |
| `test/Trace/FilterSpec.hs` | Import OAuthTrace from Servant.OAuth2.IDP.Trace |
| `test/Trace/GoldenSpec.hs` | Import OAuthTrace and renderOAuthTrace from Servant.OAuth2.IDP.Trace |
| `test/Trace/RenderSpec.hs` | Import OAuthTrace from Servant.OAuth2.IDP.Trace |
| `test/Trace/OAuthSpec.hs` | Import OAuthTrace and renderOAuthTrace from Servant.OAuth2.IDP.Trace |
| `test/TestMonad.hs` | Replace unsafeUserId with smart constructor |
| `test/Laws/AuthCodeFunctorSpec.hs` | Replace unsafe* with smart constructors |
| `test/Servant/OAuth2/IDP/MetadataSpec.hs` | Replace unsafeScope with mkScope |
| `test/Servant/OAuth2/IDP/TraceSpec.hs` | Replace unsafe* with smart constructors |
| `test/Servant/OAuth2/IDP/TypesSpec.hs` | Replace unsafe* with smart constructors |
| `test/Servant/OAuth2/IDP/TokenRequestSpec.hs` | Replace unsafe* with smart constructors |
| `test/Servant/OAuth2/IDP/LucidRenderingSpec.hs` | Replace unsafeSessionId with smart constructor |
| `test/Servant/OAuth2/IDP/APISpec.hs` | Replace unsafeMk helper with smart constructors |
| `test/Servant/OAuth2/IDP/ErrorsSpec.hs` | Replace unsafe* with smart constructors |
| `test/MCP/Server/HTTP/McpAuthSpec.hs` | Replace unsafeUserId with smart constructor |
| `src/Servant/OAuth2/IDP/Handlers/Registration.hs` | Use internal ClientId constructor (same module access) |
| `src/MCP/Server/HTTP.hs` | Use mkScope smart constructor |
| `mcp-haskell.cabal` | Add new modules, remove LoginFlowError module, remove MCP.Trace.OAuth module, remove Boundary module, add QuickCheck to library deps |

## Files to Delete

| File | Reason |
|------|--------|
| `src/Servant/OAuth2/IDP/LoginFlowError.hs` | LoginFlowError moved to Errors module |
| `src/MCP/Trace/OAuth.hs` | OAuthTrace moved to Servant.OAuth2.IDP.Trace; MCP.Trace.HTTP imports directly from Servant |
| `src/Servant/OAuth2/IDP/Boundary.hs` | Boundary instances live in type-defining modules; unsafe constructors eliminated |
| `src/Servant/OAuth2/IDP/Handlers/Helpers.hs` | All generators moved to Types.hs (GOLDEN RULE: constructors live with types) |
| `test/Generators.hs` | Arbitrary instances moved to type-defining modules in src/ |

## Risks

1. **Documentation Typo**: The Test.Internal module has stale doc comments referencing `MCP.Server.OAuth.Test.Internal` instead of `Servant.OAuth2.IDP.Test.Internal` - minor fix needed
2. **API Breakage**: Clean break approach may break downstream code importing from MCP.Server.Auth - acceptable since this is pre-release
