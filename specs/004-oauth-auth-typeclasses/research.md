# Research: OAuth State and Authentication Typeclasses

**Feature Branch**: `004-oauth-auth-typeclasses`
**Date**: 2025-12-11

> **Note**: Code samples in this document are **illustrative pseudo-code**. Function bodies have not been compiled or tested. Focus on **type signatures, interfaces, and structural intent** rather than implementation details.

## Technical Decisions

### 1. Associated Type Families vs Functional Dependencies

**Decision**: Associated Type Families

**Rationale**:
- Cleaner type signatures (no need to mention all type parameters in constraints)
- Better error messages from GHC
- More composable - consumers don't need to specify types they don't use
- Modern idiomatic Haskell (GHC2021 default)
- Enables future extensibility without breaking existing code

**Alternatives Considered**:
- **Functional Dependencies**: Legacy pattern, requires bidirectional type mention in constraints, more verbose. Rejected because we're on GHC 9.4+ with GHC2021 extensions enabled.

**Example**:
```haskell
-- Chosen: Associated Type Families
class Monad m => OAuthStateStore m where
  type OAuthStateError m :: Type
  type OAuthStateEnv m :: Type

  lookupAuthCode :: AuthCodeId -> m (Maybe AuthorizationCode)

-- Rejected: Functional Dependencies
class Monad m => OAuthStateStore e env m | m -> e env where
  lookupAuthCode :: AuthCodeId -> m (Maybe AuthorizationCode)
```

### 2. Error Composition Pattern

**Decision**: Use `generic-lens` with `AsType` prisms + `MonadError e m`

**Rationale**:
- `AsType` from `generic-lens` provides automatic prism generation for sum types via `GHC.Generics`
- Works with any error sum type without boilerplate
- Enables composable error handling: `(MonadError e m, AsType (OAuthStateError m) e)`
- Handler functions remain polymorphic over the concrete error type

**Alternatives Considered**:
- **Manual prisms**: More boilerplate, error-prone. Rejected for maintainability.
- **Separate error handling per typeclass**: Doesn't compose well. Rejected.
- **mtl-style error classes**: `MonadError` is already mtl-style; `AsType` adds composition.

**Example**:
```haskell
-- Unified error type (derives Generic for AsType)
data AppError
  = OAuthStoreErr OAuthStoreError
  | AuthBackendErr AuthBackendError
  | ValidationErr ValidationError
  deriving (Generic)

-- Handler with composed error constraints
handleToken ::
  ( OAuthStateStore m
  , MonadError e m
  , AsType (OAuthStateError m) e
  ) => TokenRequest -> m TokenResponse
```

### 3. Environment Composition Pattern

**Decision**: Use `generic-lens` with `HasType` lenses + `MonadReader env m`

**Rationale**:
- `HasType` from `generic-lens` provides automatic lens generation for product types via `GHC.Generics`
- Enables composable environment access: `(MonadReader env m, HasType (OAuthStateEnv m) env)`
- Handler functions remain polymorphic over the concrete environment type
- Matches the error composition pattern for consistency

**Alternatives Considered**:
- **Manual lens fields**: More boilerplate. Rejected.
- **Separate readers per typeclass**: Doesn't compose for handlers needing multiple typeclasses. Rejected.

**Example**:
```haskell
-- Unified environment type (derives Generic for HasType)
data AppEnv = AppEnv
  { oauthEnv :: OAuthTVarEnv
  , authEnv :: DemoCredentialEnv
  , loggerEnv :: LoggerEnv
  } deriving (Generic)

-- Handler with composed environment constraints
handleLogin ::
  ( AuthBackend m
  , MonadReader env m
  , HasType (AuthBackendEnv m) env
  ) => LoginRequest -> m LoginResponse
```

### 4. Servant Boundary Translation

**Decision**: Use `hoistServerWithContext` to translate polymorphic handlers to `Handler`

**Rationale**:
- `hoistServerWithContext` is the standard Servant pattern for custom monads
- Required for `servant-auth-server` JWT integration (needs `JWTSettings` in context)
- Keeps handlers polymorphic and testable; translation happens at application edge
- Natural transformation pattern (`m ~> Handler`) is well-documented

**Alternatives Considered**:
- **Direct Handler implementations**: Ties handlers to Servant, breaks testability. Rejected.
- **hoistServer without context**: Doesn't work with servant-auth. Rejected.

**Example**:
```haskell
-- Natural transformation
nt :: AppEnv -> AppM a -> Handler a
nt env action = do
  result <- liftIO $ runExceptT $ runReaderT (unAppM action) env
  case result of
    Left err -> throwError (toServerError err)
    Right a -> pure a

-- Application builder
mkApp :: Context '[CookieSettings, JWTSettings] -> AppEnv -> Application
mkApp ctx env = serveWithContext api ctx $
  hoistServerWithContext api (Proxy @'[CookieSettings, JWTSettings]) (nt env) server
```

### 5. Time Abstraction for Testability

**Decision**: Use `MonadTime` typeclass for all time-dependent operations

**Rationale**:
- Expiry filtering requires current time (`lookupAuthCode`, `lookupPendingAuth`)
- Production uses `IO` time; tests use controlled/mock time
- Enables deterministic testing of expiry behavior without waiting or mocking `IO`
- Standard pattern in Haskell (used by `time-compat`, `chronos`, etc.)

**Alternatives Considered**:
- **Direct `IO` time calls**: Untestable. Rejected.
- **Pass time as parameter**: Clutters API. Rejected.
- **Use `unsafePerformIO` in tests**: Unsafe and still non-deterministic. Rejected.

**Example**:
```haskell
-- Time abstraction
class Monad m => MonadTime m where
  getCurrentTime :: m UTCTime

-- Production instance
instance MonadTime IO where
  getCurrentTime = Data.Time.getCurrentTime

-- Test instance with controlled time
newtype TestM a = TestM { runTestM :: ReaderT UTCTime Identity a }
  deriving (Functor, Applicative, Monad, MonadReader UTCTime)

instance MonadTime TestM where
  getCurrentTime = ask  -- Returns the fixed time from reader

-- OAuthStateStore now requires MonadTime
class (Monad m, MonadTime m) => OAuthStateStore m where
  -- lookupAuthCode uses getCurrentTime internally for expiry filtering
  lookupAuthCode :: AuthCodeId -> m (Maybe AuthorizationCode)
```

### 6. Typeclass Law Testing Strategy

**Decision**: Use `hspec` + `QuickCheck` with polymorphic test specs

**Rationale**:
- `hspec` is already used in the project (consistency)
- `QuickCheck` is the standard property-based testing library
- Tests MUST be polymorphic over the monad to test interface, not implementation
- Allows injecting different implementations (in-memory, mock, pure state) via `run` function
- Enables testing the same laws against multiple backends

**Alternatives Considered**:
- **Hedgehog**: Better shrinking but not already in project, adds dependency. Rejected for consistency.
- **Implementation-specific tests**: Tests implementation details, not laws. Rejected per requirements.
- **Monomorphic tests**: Couples tests to specific monad, can't reuse. Rejected.

**Example**:
```haskell
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

-- Polymorphic spec: tests the INTERFACE, not implementation
-- Requires Arbitrary instances for all domain types
-- Accepts a `run` function to execute actions in any compatible monad
oauthStateStoreLaws ::
  forall m.
  (OAuthStateStore m, MonadTime m) =>
  (forall a. m a -> IO a) ->  -- Runner function
  Spec
oauthStateStoreLaws runM = describe "OAuthStateStore laws" $ do

  describe "AuthorizationCode" $ do
    prop "round-trip: lookup after store returns the value (non-expired)" $
      \(code :: AuthorizationCode) -> ioProperty $ do
        let validCode = code { authExpiry = addUTCTime 86400 (authExpiry code) }
        result <- runM $ do
          storeAuthCode validCode
          lookupAuthCode (authCodeId validCode)
        pure $ result === Just validCode

    prop "delete: lookup after delete returns Nothing" $
      \(code :: AuthorizationCode) -> ioProperty $ do
        result <- runM $ do
          storeAuthCode code
          deleteAuthCode (authCodeId code)
          lookupAuthCode (authCodeId code)
        pure $ result === Nothing

    prop "idempotence: store twice is same as store once" $
      \(code :: AuthorizationCode) -> ioProperty $ do
        let validCode = code { authExpiry = addUTCTime 86400 (authExpiry code) }
        result <- runM $ do
          storeAuthCode validCode
          storeAuthCode validCode
          lookupAuthCode (authCodeId validCode)
        pure $ result === Just validCode

    prop "overwrite: second store replaces first" $
      \(code1 :: AuthorizationCode) (code2 :: AuthorizationCode) -> ioProperty $ do
        let code2' = code2 { authCodeId = authCodeId code1
                           , authExpiry = addUTCTime 86400 (authExpiry code2) }
        result <- runM $ do
          storeAuthCode code1
          storeAuthCode code2'
          lookupAuthCode (authCodeId code1)
        pure $ result === Just code2'

    prop "expiry: lookup returns Nothing for expired codes" $
      \(code :: AuthorizationCode) -> ioProperty $ do
        let expiredCode = code { authExpiry = addUTCTime (-3600) (authExpiry code) }
        result <- runM $ do
          storeAuthCode expiredCode
          lookupAuthCode (authCodeId expiredCode)
        pure $ result === Nothing

-- Usage: run against multiple implementations
spec :: Spec
spec = do
  describe "InMemory OAuthStateStore" $
    oauthStateStoreLaws runInMemory

  describe "Pure State OAuthStateStore" $
    oauthStateStoreLaws runPureState

-- Different runners for different implementations
runInMemory :: InMemoryM a -> IO a
runInMemory action = do
  env <- newOAuthTVarEnv
  runReaderT action env

runPureState :: PureStateM a -> IO a
runPureState action =
  pure $ evalState (runPureStateM action) emptyState
```

### 7. In-Memory Implementation Strategy

**Decision**: Preserve existing `TVar OAuthState` pattern in `InMemory.hs`

**Rationale**:
- Current implementation is correct and tested
- `TVar` + `atomically` provides STM guarantees required by spec
- Moving to separate module enables alternative implementations without code changes
- Environment type wraps `TVar OAuthState` for typeclass access

**Alternatives Considered**:
- **IORef**: No atomicity guarantees. Rejected.
- **MVar**: Blocking semantics not needed. Rejected.
- **Pure state monad**: Doesn't allow concurrent access. Rejected for production.

**Example**:
```haskell
data OAuthTVarEnv = OAuthTVarEnv
  { oauthStateVar :: TVar OAuthState
  , oauthExpiryConfig :: ExpiryConfig
  }

instance (MonadIO m, MonadTime m) => OAuthStateStore (ReaderT OAuthTVarEnv m) where
  type OAuthStateError (ReaderT OAuthTVarEnv m) = OAuthStoreError
  type OAuthStateEnv (ReaderT OAuthTVarEnv m) = OAuthTVarEnv

  lookupAuthCode key = do
    var <- asks oauthStateVar
    now <- getCurrentTime  -- Uses MonadTime, not IO directly
    liftIO $ atomically $ do
      state <- readTVar var
      pure $ do  -- Maybe monad
        code <- Map.lookup (unAuthCodeId key) (authCodes state)
        guard (authExpiry code > now)  -- Filter expired
        pure code
```

### 8. Expiry Filtering in Store vs Handler

**Decision**: Filter expired entries in store operations (per spec clarification)

**Rationale**:
- FR-017 specifies: "Lookup operations for time-bounded entities MUST filter expired entries"
- Enables backend TTL mechanisms (Redis EXPIRE, PostgreSQL scheduled cleanup)
- Keeps expiry enforcement within store abstraction
- Simplifies handler logic (no expiry checks needed)
- With `MonadTime`, expiry can be tested deterministically

**Alternatives Considered**:
- **Return raw data, filter in handler**: Duplicates logic, doesn't enable backend TTL. Rejected per spec.

### 9. Strong Types for Domain Values

**Decision**: Use newtypes and ADTs instead of primitive types

**Rationale**:
- Prevents mixing identifiers (can't accidentally use `ClientId` as `AuthCodeId`)
- Smart constructors enforce invariants at construction time
- ADTs for finite enumerations eliminate invalid states
- Follows Constitution Principle I (Type-Driven Design)

**Alternatives Considered**:
- **Raw `Text`/`String`/`Int`**: Type-unsafe, allows mixing identifiers. Rejected.
- **Type aliases**: No type safety, just documentation. Rejected.

**New Types**:
- Identity types: `AuthCodeId`, `ClientId`, `SessionId`, `UserId`, `RefreshTokenId`
- Value types: `RedirectUri`, `Scope`, `CodeChallenge`, `Username`, `PlaintextPassword`, `Salt`
- ADTs: `CodeChallengeMethod`, `GrantType`, `ResponseType`, `ClientAuthMethod`
- Time: `NominalDiffTime` instead of `Int` seconds

See `data-model.md` for complete type definitions.

## Dependencies to Add

### Required

| Package | Version | Purpose |
|---------|---------|---------|
| `generic-lens` | `^>=2.2` | `AsType` prisms, `HasType` lenses |
| `QuickCheck` | `^>=2.14` | Property-based testing |
| `hspec-quickcheck` | `^>=0.2` | `prop` combinator for hspec integration |
| `quickcheck-instances` | `^>=0.3` | Arbitrary instances for common types |

### Already Present

| Package | Purpose |
|---------|---------|
| `stm` | TVar-based in-memory state |
| `mtl` | MonadReader, MonadError |
| `servant-server` | HTTP framework |
| `servant-auth-server` | JWT integration |
| `cryptonite` | Password hashing |
| `hspec` | Test framework |
| `time` | UTCTime, NominalDiffTime |

### Required (for security)

| Package | Version | Purpose |
|---------|---------|---------|
| `memory` | `^>=0.18` | `ScrubbedBytes` for secure credential handling (auto-scrub, constant-time Eq, no Show) |

### Optional (for stronger types)

| Package | Version | Purpose |
|---------|---------|---------|
| `email-validate` | `^>=2.3` | `EmailAddress` validated type |
| `network-uri` | `^>=2.6` | `URI` type for RedirectUri |

## Module Organization

```text
src/MCP/Server/
├── Auth.hs                  # Re-exports, credential types
├── Auth/
│   ├── Backend.hs          # AuthBackend typeclass
│   └── Demo.hs             # Demo credential instance
├── OAuth/
│   ├── Store.hs            # OAuthStateStore typeclass
│   ├── Types.hs            # OAuth data types, newtypes, ADTs
│   └── InMemory.hs         # TVar-based instance
├── Time.hs                  # MonadTime typeclass
└── HTTP.hs                  # Servant handlers (uses typeclass constraints)

test/
├── Main.hs                  # Test entry point
├── Laws/
│   ├── OAuthStateStoreSpec.hs  # Polymorphic typeclass law tests
│   └── AuthBackendSpec.hs      # Polymorphic typeclass law tests
├── Generators.hs            # Arbitrary instances for domain types
└── TestMonad.hs             # Test monad with MonadTime, runners
```

## Terminology: Haskell Effect Patterns

This section clarifies the terminology used in this feature's design.

### Three-Layer Cake Architecture

**Source**: [Matt Parsons, 2018](https://www.parsonsmatt.org/2018/03/22/three_layer_haskell_cake.html)

The three-layer cake is an **application architecture pattern** organizing code into:

| Layer | Name | Purpose | Example in This Feature |
|-------|------|---------|-------------------------|
| 1 | Orchestration | `ReaderT IO` runtime foundation | `AppM` monad with `AppEnv` |
| 2 | Capabilities | MTL-style typeclasses for mockable abstractions | `OAuthStateStore`, `AuthBackend` |
| 3 | Business Logic | Pure functions, no effects | Token validation, PKCE verification |

**This feature implements Layer 2** - the typeclasses serve as capability abstractions that can have multiple implementations (in-memory for dev, PostgreSQL for production).

### MTL-Style

**Definition**: Writing functions with **typeclass constraints** (`MonadReader`, `MonadError`, `MonadState`) instead of concrete transformer stacks.

```haskell
-- MTL-style (polymorphic, testable)
handleToken :: (MonadReader env m, MonadError e m) => TokenRequest -> m TokenResponse

-- Non-MTL (concrete, harder to test)
handleToken :: ReaderT AppEnv (ExceptT AppError IO) TokenResponse
```

**This feature uses MTL-style** for all handler functions.

### Classy-MTL / Has* Pattern / Classy Lenses

**Definition**: A refinement of MTL-style using `Has*` typeclasses to **compose environments and errors** without coupling to concrete types.

**Traditional MTL**: Functions constrained by concrete environment type
```haskell
handleLogin :: MonadReader AppEnv m => LoginRequest -> m LoginResponse
```

**Classy-MTL**: Functions constrained by abstract capabilities
```haskell
handleLogin :: (MonadReader env m, HasType AuthBackendEnv env) => LoginRequest -> m LoginResponse
```

**This feature uses classy-MTL** via `generic-lens`:
- `HasType` for environment composition (product types)
- `AsType` for error composition (sum types)

The `generic-lens` approach is a **modern, GHC.Generics-based implementation** of classy-MTL that avoids Template Haskell boilerplate from `makeClassy`.

### ReaderT IO Pattern

**Definition**: A simplified pattern using `type App a = ReaderT AppEnv IO a` as the application monad.

**Relationship to three-layer cake**: The ReaderT IO pattern is **Layer 1** of the three-layer architecture.

**This feature uses ReaderT IO** at the orchestration layer, with Layer 2 typeclasses for testable abstractions.

### Summary: What This Feature Uses

| Pattern | How Used |
|---------|----------|
| Three-Layer Cake | Layer 2 capability typeclasses |
| MTL-Style | `MonadReader`, `MonadError` constraints |
| Classy-MTL | `AsType`/`HasType` from `generic-lens` |
| ReaderT IO | Layer 1 orchestration |

## References

- [Three Layer Haskell Cake](https://www.parsonsmatt.org/2018/03/22/three_layer_haskell_cake.html) - Matt Parsons
- [Holmusk Three-Layer](https://github.com/Holmusk/three-layer) - Production example
- [The ReaderT Design Pattern](https://www.fpcomplete.com/blog/readert-design-pattern/) - FP Complete
- [Classy MTL](https://carlo-hamalainen.net/2015/07/20/classy-mtl/) - Carlo Hämäläinen
- [generic-lens Hackage](https://hackage.haskell.org/package/generic-lens)
- [Servant hoistServerWithContext](https://docs.servant.dev/en/latest/cookbook/hoist-server-with-context/HoistServerWithContext.html)
- [Type Families in Haskell](https://serokell.io/blog/type-families-haskell) - Serokell
- [QuickCheck manual](https://hackage.haskell.org/package/QuickCheck)
- [Testing Typeclass Laws](https://austinrochford.com/posts/2014-05-27-quickcheck-laws.html)

---

## Phase 3 Research: Type Unification (mcp-51r)

**Added**: 2025-12-11 | **Context**: Blocker discovered during implementation

### 10. Servant Boundary Instances Strategy

**Decision**: Add `FromHttpApiData`/`ToHttpApiData` instances directly to OAuth.Types newtypes

**Rationale**:
- Servant instances ARE the natural boundary between HTTP (Text) and domain (newtypes)
- Enables property-based round-trip testing: `parseUrlPiece . toUrlPiece ≡ Right`
- No separate "Boundary" module needed—instances live with types (orphan-free)
- Follows "parse, don't validate" principle at the Servant layer

**Alternatives Considered**:
- **Dedicated Boundary module**: Adds indirection; instances become orphans or require re-exports. Rejected.
- **Inline conversion in handlers**: Duplicates conversion logic; no centralized validation. Rejected.
- **Custom Servant combinators**: Over-engineering for simple newtype wrapping. Rejected.

**Example**:
```haskell
-- In OAuth.Types (with types they describe)

instance FromHttpApiData ClientId where
  parseUrlPiece t
    | Text.null t = Left "ClientId cannot be empty"
    | otherwise = Right (ClientId t)

instance ToHttpApiData ClientId where
  toUrlPiece = unClientId

-- For URI-based types, validate during parsing
instance FromHttpApiData RedirectUri where
  parseUrlPiece t = case parseURI (Text.unpack t) of
    Nothing -> Left "Invalid URI format"
    Just uri
      | not (isValidRedirectUri uri) -> Left "URI must be https or localhost http"
      | otherwise -> Right (RedirectUri uri)

instance ToHttpApiData RedirectUri where
  toUrlPiece (RedirectUri uri) = Text.pack (uriToString id uri "")
```

### 11. Invariant Enforcement in FromJSON

**Decision**: Reject invalid data at JSON parse time (400 Bad Request), not as domain errors

**Rationale**:
- "Make illegal states unrepresentable" extends to the parse boundary
- Domain error types should only contain domain-level failures, not structural validation
- Servant automatically translates JSON parse failures to 400 Bad Request
- Keeps handler logic clean—if data reaches the handler, it's already valid

**Alternatives Considered**:
- **Accept and validate in handler**: Requires domain error cases for structural issues; pollutes error type. Rejected.
- **Accept and normalize**: Violates principle of least surprise; hides invalid input. Rejected.

**Example**:
```haskell
-- NonEmpty enforcement at parse time
instance FromJSON ClientInfo where
  parseJSON = withObject "ClientInfo" $ \o -> do
    name <- o .: "client_name"
    uriList <- o .: "redirect_uris" :: Parser [RedirectUri]
    case nonEmpty uriList of
      Nothing -> fail "redirect_uris must contain at least one URI"
      Just uris -> do
        grants <- o .:? "grant_types" .!= Set.singleton GrantAuthorizationCode
        responses <- o .:? "response_types" .!= Set.singleton ResponseCode
        authMethod <- o .:? "token_endpoint_auth_method" .!= AuthNone
        pure $ ClientInfo name uris grants responses authMethod
```

### 12. Incremental Handler Migration Pattern

**Decision**: Migrate handlers one at a time with coexisting old/new patterns

**Rationale**:
- Lower risk than big-bang migration
- Each handler migration is independently testable and commitable
- Enables continuous integration during migration
- Proof-of-concept (`handleMetadataAppM`) already demonstrates the pattern

**Migration Order** (by dependency and complexity):
1. **Foundation**: Servant instances + property tests (no handler changes)
2. **Simple handlers**: Metadata, health (no OAuth state)
3. **Read-only OAuth**: Client lookup, token validation
4. **Mutating OAuth**: Registration, authorization, token exchange
5. **Complex flows**: Login (session + credentials + auth code)
6. **Cleanup**: Remove legacy Text-based types

**Pattern for each handler**:
```haskell
-- Before (legacy)
handleAuthorize :: Text -> Text -> ... -> Handler AuthorizeResponse

-- After (newtype)
handleAuthorize :: ClientId -> RedirectUri -> ... -> Handler AuthorizeResponse

-- Servant API type changes accordingly
type AuthorizeAPI = "authorize"
  :> QueryParam' '[Required] "client_id" ClientId  -- Was Text
  :> QueryParam' '[Required] "redirect_uri" RedirectUri  -- Was Text
  :> ...
```

### 13. Property Test Strategy for Boundary Instances

**Decision**: Use QuickCheck with custom generators for all boundary instance round-trips

**Rationale**:
- FR-019 requires: "parseUrlPiece . toUrlPiece ≡ Right"
- Property tests catch edge cases (empty strings, special characters, unicode)
- Generators ensure coverage of valid domain values
- Complements existing typeclass law tests

**Test Structure**:
```haskell
-- test/Laws/BoundarySpec.hs
module Laws.BoundarySpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Servant (FromHttpApiData(..), ToHttpApiData(..))
import MCP.Server.OAuth.Types

spec :: Spec
spec = describe "Servant Boundary Instances" $ do
  describe "FromHttpApiData/ToHttpApiData round-trip" $ do
    prop "ClientId" $ \(cid :: ClientId) ->
      parseUrlPiece (toUrlPiece cid) === Right cid

    prop "AuthCodeId" $ \(acid :: AuthCodeId) ->
      parseUrlPiece (toUrlPiece acid) === Right acid

    prop "RedirectUri" $ \(uri :: RedirectUri) ->
      parseUrlPiece (toUrlPiece uri) === Right uri

    -- ... for all newtypes

  describe "FromJSON/ToJSON round-trip" $ do
    prop "ClientInfo" $ \(info :: ClientInfo) ->
      decode (encode info) === Just info

    prop "AuthorizationCode" $ \(code :: AuthorizationCode) ->
      decode (encode code) === Just code
```

**Generator Requirements** (additions to `test/Generators.hs`):
```haskell
-- Arbitrary for validated types must only generate valid values
instance Arbitrary RedirectUri where
  arbitrary = do
    -- Only generate valid redirect URIs
    scheme <- elements ["https://", "http://localhost:"]
    path <- listOf1 (elements ['a'..'z'])
    pure $ fromJust $ mkRedirectUri (Text.pack $ scheme ++ path)
  shrink = const []  -- Can't shrink to invalid values

instance Arbitrary ClientInfo where
  arbitrary = ClientInfo
    <$> arbitrary  -- name
    <*> (NE.fromList <$> listOf1 arbitrary)  -- NonEmpty RedirectUri
    <*> arbitrary  -- Set GrantType
    <*> arbitrary  -- Set ResponseType
    <*> arbitrary  -- ClientAuthMethod
```

## References (Phase 3)

- [Servant FromHttpApiData](https://hackage.haskell.org/package/servant/docs/Servant-API.html#t:FromHttpApiData)
- [Parse, don't validate](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/) - Alexis King
- [Making illegal states unrepresentable](https://fsharpforfunandprofit.com/posts/designing-with-types-making-illegal-states-unrepresentable/) - Scott Wlaschin
