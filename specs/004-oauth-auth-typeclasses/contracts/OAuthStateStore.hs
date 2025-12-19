{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

{- | OAuth 2.1 State Persistence Typeclass

__DISCLAIMER__: This is a design contract, not production code. Code samples
are illustrative pseudo-code. Function bodies have not been compiled or tested.
Focus on type signatures, interfaces, and structural intent.

__PARTIALLY SUPERSEDED (2025-12-17)__: Phase 14 removes 'OAuthUserId m'
associated type. 'AuthorizationCode' is parameterized by @OAuthUser m@ (full user)
directly, not by a separate user ID type. See plan.md Phase 14 for updated design.

This module defines the abstract interface for OAuth state persistence,
enabling swappable production implementations (PostgreSQL, Redis, etc.)
while maintaining backward compatibility with the existing TVar-based
in-memory implementation.

== Three-Layer Cake Architecture

This typeclass follows the three-layer cake pattern:

* Layer 1 (Orchestration): Application monad with 'MonadReader' env
* Layer 2 (Capability): This typeclass with associated types
* Layer 3 (Business Logic): Pure functions using the typeclass interface

== Usage

Handlers use this typeclass polymorphically:

@
handleToken ::
  ( OAuthStateStore m
  , MonadError e m
  , AsType (OAuthStateError m) e
  ) => TokenRequest -> m TokenResponse
@

== Algebraic Laws (FR-016)

Implementations MUST satisfy these laws:

1. __Round-trip__: @lookupX k@ after @storeX k v@ returns @Just v@ (if not expired)
2. __Delete__: @lookupX k@ after @deleteX k@ returns @Nothing@
3. __Idempotence__: @storeX k v >> storeX k v@ equivalent to @storeX k v@
4. __Overwrite__: @storeX k v2@ after @storeX k v1@ makes @lookupX k@ return @Just v2@

== Expiry Filtering (FR-017)

Lookup operations for time-bounded entities (authorization codes, pending
authorizations) MUST filter expired entries, returning 'Nothing' for expired
items. This enables backend TTL mechanisms (Redis EXPIRE, PostgreSQL cleanup).

Implementations use 'MonadTime' to obtain current time for expiry checks,
enabling deterministic testing with controlled time.

== Testing

Property tests for these laws MUST be:

* Polymorphic over the monad @m@
* Accept a @run@ function to execute @m@ in @IO@
* Test the interface, not implementation details

See @test\/Laws\/OAuthStateStoreSpec.hs@ for the polymorphic test suite.
-}
module MCP.Server.OAuth.Store (
    -- * Typeclass
    OAuthStateStore (..),

    -- * Time Abstraction
    MonadTime (..),

    -- * Identity Newtypes
    AuthCodeId (..),
    mkAuthCodeId,
    ClientId (..),
    mkClientId,
    SessionId (..),
    mkSessionId,
    AccessTokenId (..),
    RefreshTokenId (..),
    mkRefreshTokenId,
    UserId (..),
    mkUserId,

    -- * Value Newtypes
    RedirectUri (..),
    mkRedirectUri,
    Scope (..),
    mkScope,
    CodeChallenge (..),
    mkCodeChallenge,

    -- * ADTs
    CodeChallengeMethod (..),
    GrantType (..),
    ResponseType (..),
    ClientAuthMethod (..),

    -- * Domain Entities
    AuthorizationCode (..),
    ClientInfo (..),
    PendingAuthorization (..),
    AuthUser (..),
) where

import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time (NominalDiffTime, UTCTime)
import GHC.Generics (Generic)
import Network.URI (URI)

{- | Time abstraction for testable time-dependent operations.

__IMPORTANT__: Use the @monad-time@ package from Hackage instead of defining this ourselves!
See: https://hackage.haskell.org/package/monad-time

The package provides exactly this typeclass with instances for IO, transformers, etc.
Import @Control.Monad.Time@ and add @monad-time@ to dependencies.

Production uses 'IO' time; tests use controlled/mock time.
This enables deterministic testing of expiry behavior.

@
import Control.Monad.Time (MonadTime(..))
-- getCurrentTime :: MonadTime m => m UTCTime
@
-}
class (Monad m) => MonadTime m where
    {- | Get the current time.

    In production, this calls @Data.Time.getCurrentTime@.
    In tests, this returns a controlled time value.
    -}
    getCurrentTime :: m UTCTime

-- ============================================================================
-- Identity Newtypes
-- ============================================================================

-- | Authorization code identifier (unique key for auth codes).
newtype AuthCodeId = AuthCodeId {unAuthCodeId :: Text}
    deriving (Eq, Ord, Show, Generic)

-- | Smart constructor: non-empty text.
mkAuthCodeId :: Text -> Maybe AuthCodeId
mkAuthCodeId t
    | t == mempty = Nothing
    | otherwise = Just (AuthCodeId t)

-- | OAuth client identifier (unique key for registered clients).
newtype ClientId = ClientId {unClientId :: Text}
    deriving (Eq, Ord, Show, Generic)

-- | Smart constructor: non-empty text.
mkClientId :: Text -> Maybe ClientId
mkClientId t
    | t == mempty = Nothing
    | otherwise = Just (ClientId t)

-- | Login session identifier (UUID format).
newtype SessionId = SessionId {unSessionId :: Text}
    deriving (Eq, Ord, Show, Generic)

-- | Smart constructor: valid UUID format.
mkSessionId :: Text -> Maybe SessionId
mkSessionId t
    | t == mempty = Nothing
    | otherwise = Just (SessionId t) -- TODO: validate UUID format

-- | Access token identifier (JWT string).
newtype AccessTokenId = AccessTokenId {unAccessTokenId :: Text}
    deriving (Eq, Ord, Show, Generic)

-- | Refresh token identifier.
newtype RefreshTokenId = RefreshTokenId {unRefreshTokenId :: Text}
    deriving (Eq, Ord, Show, Generic)

-- | Smart constructor: non-empty text.
mkRefreshTokenId :: Text -> Maybe RefreshTokenId
mkRefreshTokenId t
    | t == mempty = Nothing
    | otherwise = Just (RefreshTokenId t)

-- | User identifier.
newtype UserId = UserId {unUserId :: Text}
    deriving (Eq, Ord, Show, Generic)

-- | Smart constructor: non-empty text.
mkUserId :: Text -> Maybe UserId
mkUserId t
    | t == mempty = Nothing
    | otherwise = Just (UserId t)

-- ============================================================================
-- Value Newtypes
-- ============================================================================

{- | Validated OAuth redirect URI.

Must be either:
* HTTPS scheme
* HTTP with localhost (for development)
-}
newtype RedirectUri = RedirectUri {unRedirectUri :: URI}
    deriving (Eq, Show, Generic)

-- | Smart constructor: validates URI scheme.
mkRedirectUri :: URI -> Maybe RedirectUri
mkRedirectUri uri = Just (RedirectUri uri) -- TODO: validate https or localhost

{- | OAuth scope identifier.

Must be non-empty with no whitespace.
-}
newtype Scope = Scope {unScope :: Text}
    deriving (Eq, Ord, Show, Generic)

-- | Smart constructor: non-empty, no whitespace.
mkScope :: Text -> Maybe Scope
mkScope t
    | t == mempty = Nothing
    | otherwise = Just (Scope t) -- TODO: check no whitespace

{- | PKCE code challenge.

Must be base64url encoded, 43-128 characters.
-}
newtype CodeChallenge = CodeChallenge {unCodeChallenge :: Text}
    deriving (Eq, Ord, Show, Generic)

-- | Smart constructor: validates base64url format and length.
mkCodeChallenge :: Text -> Maybe CodeChallenge
mkCodeChallenge t
    | t == mempty = Nothing
    | otherwise = Just (CodeChallenge t) -- TODO: validate base64url, length

-- ============================================================================
-- Algebraic Data Types
-- ============================================================================

{- | PKCE code challenge method.

Eliminates stringly-typed validation errors.
-}
data CodeChallengeMethod
    = -- | SHA256 hash (recommended, required by MCP spec)
      S256
    | -- | Plaintext (legacy, discouraged)
      Plain
    deriving (Eq, Show, Generic, Bounded, Enum)

-- | OAuth grant type.
data GrantType
    = -- | @authorization_code@ grant
      GrantAuthorizationCode
    | -- | @refresh_token@ grant
      GrantRefreshToken
    | -- | @client_credentials@ grant (future)
      GrantClientCredentials
    deriving (Eq, Ord, Show, Generic, Bounded, Enum)

-- | OAuth response type.
data ResponseType
    = -- | @code@ (authorization code flow)
      ResponseCode
    | -- | @token@ (implicit flow, deprecated)
      ResponseToken
    deriving (Eq, Ord, Show, Generic, Bounded, Enum)

-- | Client authentication method.
data ClientAuthMethod
    = -- | @none@ (public client)
      AuthNone
    | -- | @client_secret_post@
      AuthClientSecretPost
    | -- | @client_secret_basic@
      AuthClientSecretBasic
    deriving (Eq, Show, Generic, Bounded, Enum)

-- ============================================================================
-- Domain Entities
-- ============================================================================

{- | Authorization code with PKCE support.
Phase 8 (FR-041): Parameterized over userId type.
At use sites, instantiate with @OAuthUserId m@ from the typeclass.
-}
data AuthorizationCode userId = AuthorizationCode
    { authCodeId :: AuthCodeId
    -- ^ Unique identifier (key)
    , authClientId :: ClientId
    -- ^ Associated client
    , authRedirectUri :: RedirectUri
    -- ^ Validated redirect URI
    , authCodeChallenge :: CodeChallenge
    -- ^ PKCE challenge
    , authCodeChallengeMethod :: CodeChallengeMethod
    -- ^ S256 or Plain (ADT, not Text)
    , authScopes :: Set Scope
    -- ^ Granted scopes (Set, not list)
    , authUserId :: userId
    -- ^ Authenticated user identifier (parameterized, Phase 8)
    , authExpiry :: UTCTime
    -- ^ Expiration timestamp
    }
    deriving (Eq, Show, Generic, Functor)

-- | Registered OAuth client information.
data ClientInfo = ClientInfo
    { clientName :: Text
    -- ^ Human-readable name
    , clientRedirectUris :: NonEmpty RedirectUri
    -- ^ Allowed redirect URIs (non-empty by construction)
    , clientGrantTypes :: Set GrantType
    -- ^ Allowed grant types (Set of ADT)
    , clientResponseTypes :: Set ResponseType
    -- ^ Allowed response types (Set of ADT)
    , clientAuthMethod :: ClientAuthMethod
    -- ^ Client authentication method (ADT)
    }
    deriving (Eq, Show, Generic)

-- | Pending authorization session (between /authorize and /login).
data PendingAuthorization = PendingAuthorization
    { pendingClientId :: ClientId
    -- ^ Initiating client
    , pendingRedirectUri :: RedirectUri
    -- ^ Validated redirect URI
    , pendingCodeChallenge :: CodeChallenge
    -- ^ PKCE challenge
    , pendingCodeChallengeMethod :: CodeChallengeMethod
    -- ^ Challenge method (ADT)
    , pendingScope :: Maybe (Set Scope)
    -- ^ Requested scopes
    , pendingState :: Maybe Text
    -- ^ Client state parameter (opaque)
    , pendingResource :: Maybe URI
    -- ^ Resource indicator
    , pendingCreatedAt :: UTCTime
    -- ^ Session start time (for expiry)
    }
    deriving (Eq, Show, Generic)

{- | Authenticated user information.
This is the REFERENCE IMPLEMENTATION user type (not a "default").
Custom implementations define their own user types via @OAuthUser m@.
Phase 8 (FR-039): Implementations use associated types, not this fixed type.
-}
data AuthUser = AuthUser
    { authUserId' :: UserId
    -- ^ Unique user identifier
    , authUserEmail :: Maybe Text
    -- ^ Optional email (TODO: use EmailAddress from email-validate)
    , authUserName :: Maybe Text
    -- ^ Optional display name
    }
    deriving (Eq, Show, Generic)

-- ============================================================================
-- OAuthStateStore Typeclass
-- ============================================================================

{- | Abstract interface for OAuth 2.1 state persistence.

Implementations provide storage for:

* Authorization codes (with PKCE)
* Access token → user mappings
* Refresh token → (clientId, user) mappings
* Registered OAuth clients
* Pending authorization sessions

== Associated Types

* 'OAuthStateError': Implementation-specific failure modes
* 'OAuthStateEnv': Implementation-specific environment/configuration
* 'OAuthUser': Implementation-specific user type for JWT tokens (Phase 8, FR-039)
* 'OAuthUserId': Implementation-specific user identifier for state structures (Phase 8, FR-039)

== Type Equality (Phase 8, FR-039)

Handlers requiring both 'OAuthStateStore' and 'AuthBackend' MUST include:

@
( AuthBackendUser m ~ OAuthUser m
, AuthBackendUserId m ~ OAuthUserId m
)
@

This ensures compile-time agreement between the user types from both typeclasses.

== Superclass Constraint

Requires 'MonadTime' for expiry filtering. This enables:

* Production: real system time
* Testing: controlled/mock time for deterministic tests

== Instance Context

Implementations may add constraints in their instance context:

@
instance (MonadIO m, MonadTime m) => OAuthStateStore (ReaderT OAuthTVarEnv m) where
  type OAuthStateError (ReaderT OAuthTVarEnv m) = OAuthStoreError
  type OAuthStateEnv (ReaderT OAuthTVarEnv m) = OAuthTVarEnv
  type OAuthUser (ReaderT OAuthTVarEnv m) = AuthUser        -- Phase 8
  type OAuthUserId (ReaderT OAuthTVarEnv m) = UserId        -- Phase 8
  ...
@
-}
class (Monad m, MonadTime m) => OAuthStateStore m where
    {- | Implementation-specific error type.

    Examples:

    * In-memory: Minimal errors (STM doesn't fail)
    * PostgreSQL: Connection errors, constraint violations
    * Redis: Connection errors, serialization failures
    -}
    type OAuthStateError m :: Type

    {- | Implementation-specific environment type.

    Examples:

    * In-memory: @TVar OAuthState@, @ExpiryConfig@
    * PostgreSQL: Connection pool, table names
    * Redis: Connection handle, serialization config
    -}
    type OAuthStateEnv m :: Type

    {- | Implementation-specific user type for JWT tokens (Phase 8, FR-039).

    This is the full user representation used in JWT claims via 'ToJWT'.
    The 'ToJWT' constraint is added at the operation level (FR-040), not here.

    Examples:

    * Reference: @AuthUser@ with userId, email, name
    * Enterprise: Custom user type from IdP
    * PostgreSQL: User record from database
    -}
    type OAuthUser m :: Type

    {- | Implementation-specific user identifier for state structures (Phase 8, FR-039).

    Stored in 'AuthorizationCode' and other state entities.
    Distinct from full user to minimize storage in state.

    Examples:

    * Reference: @UserId@ (Text newtype)
    * Enterprise: UUID, Integer, or custom identifier
    * PostgreSQL: Database primary key type
    -}
    type OAuthUserId m :: Type

    -- Authorization Code Operations

    {- | Store an authorization code.

    The code is keyed by 'authCodeId' field. Overwrites any existing code
    with the same key (idempotent).

    Phase 8 (FR-041): Takes parameterized @AuthorizationCode (OAuthUserId m)@.
    -}
    storeAuthCode :: AuthorizationCode (OAuthUserId m) -> m ()

    {- | Lookup an authorization code by its identifier.

    Returns 'Nothing' if:

    * Code doesn't exist
    * Code has expired ('authExpiry' < current time from 'MonadTime')

    /Note/: Expiry filtering is implementation responsibility (FR-017).
    Uses 'getCurrentTime' from 'MonadTime' for testability.

    Phase 8 (FR-041): Returns parameterized @AuthorizationCode (OAuthUserId m)@.
    -}
    lookupAuthCode :: AuthCodeId -> m (Maybe (AuthorizationCode (OAuthUserId m)))

    {- | Delete an authorization code.

    Idempotent: deleting a non-existent code succeeds.
    Used after token exchange (codes are single-use).
    -}
    deleteAuthCode :: AuthCodeId -> m ()

    -- Access Token Operations

    {- | Store an access token → user mapping.

    The token string is the key. Used for MCP request authentication.

    Phase 8 (FR-039): Uses @OAuthUser m@ associated type instead of fixed @AuthUser@.
    -}
    storeAccessToken :: AccessTokenId -> OAuthUser m -> m ()

    {- | Lookup user by access token.

    Returns 'Nothing' if token doesn't exist.

    /Note/: Token expiry is handled by JWT validation, not the store.

    Phase 8 (FR-039): Returns @OAuthUser m@ associated type.
    -}
    lookupAccessToken :: AccessTokenId -> m (Maybe (OAuthUser m))

    -- Refresh Token Operations

    {- | Store a refresh token with associated client and user.

    Format: @(ClientId, OAuthUser m)@

    Phase 8 (FR-039): Uses @OAuthUser m@ associated type.
    -}
    storeRefreshToken :: RefreshTokenId -> (ClientId, OAuthUser m) -> m ()

    {- | Lookup refresh token data.

    Returns @Just (ClientId, OAuthUser m)@ if found, 'Nothing' otherwise.

    Phase 8 (FR-039): Returns @OAuthUser m@ associated type.
    -}
    lookupRefreshToken :: RefreshTokenId -> m (Maybe (ClientId, OAuthUser m))

    {- | Update refresh token data (for token rotation).

    Semantically equivalent to 'storeRefreshToken', but may be optimized
    for update-in-place in some implementations.

    Phase 8 (FR-039): Uses @OAuthUser m@ associated type.
    -}
    updateRefreshToken :: RefreshTokenId -> (ClientId, OAuthUser m) -> m ()

    -- Client Registration Operations

    {- | Store a registered OAuth client.

    Keyed by client ID. Used after dynamic client registration.
    -}
    storeClient :: ClientId -> ClientInfo -> m ()

    {- | Lookup a registered client by ID.

    Returns 'Nothing' if client isn't registered.
    -}
    lookupClient :: ClientId -> m (Maybe ClientInfo)

    -- Pending Authorization Operations

    {- | Store a pending authorization session.

    Keyed by session ID (stored in cookie). Created on @/authorize@ request,
    consumed or deleted on @/login@ completion.
    -}
    storePendingAuth :: SessionId -> PendingAuthorization -> m ()

    {- | Lookup pending authorization by session ID.

    Returns 'Nothing' if:

    * Session doesn't exist
    * Session has expired ('pendingCreatedAt' + configurable expiry)

    /Note/: Expiry filtering is implementation responsibility (FR-017).
    Uses 'getCurrentTime' from 'MonadTime' for testability.
    -}
    lookupPendingAuth :: SessionId -> m (Maybe PendingAuthorization)

    {- | Delete a pending authorization session.

    Called after login approval (success or denial) to clean up session.
    -}
    deletePendingAuth :: SessionId -> m ()
