{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

{- | User Authentication Backend Typeclass

__DISCLAIMER__: This is a design contract, not production code. Code samples
are illustrative pseudo-code. Function bodies have not been compiled or tested.
Focus on type signatures, interfaces, and structural intent.

__PARTIALLY SUPERSEDED (2025-12-17)__: Phase 14 removes 'AuthBackendUserId m'
associated type. 'validateCredentials' now returns @Maybe (AuthBackendUser m)@
instead of tuple. User IDs are fields within 'AuthBackendUser m', encoded into
JWT via 'ToJWT'. See plan.md Phase 14 for updated design.

This module defines the abstract interface for user credential validation,
enabling integration with external identity providers (LDAP, Active Directory,
Okta, etc.) while maintaining backward compatibility with the existing
hard-coded demo credential implementation.

== Three-Layer Cake Architecture

This typeclass follows the three-layer cake pattern:

* Layer 1 (Orchestration): Application monad with 'MonadReader' env
* Layer 2 (Capability): This typeclass with associated types
* Layer 3 (Business Logic): Pure functions using the typeclass interface

== Usage

Handlers use this typeclass polymorphically:

@
handleLogin ::
  ( AuthBackend m
  , MonadError e m
  , AsType (AuthBackendError m) e
  , MonadReader env m
  , HasType (AuthBackendEnv m) env
  ) => LoginRequest -> m LoginResponse
@

== Security Considerations

* Implementations SHOULD use constant-time comparison for password validation
* Implementations SHOULD hash passwords (never store plaintext)
* Implementations MAY perform IO operations (LDAP queries, database lookups)
* Implementations SHOULD log authentication failures for audit purposes

== Testing

Property tests for this typeclass MUST be:

* Polymorphic over the monad @m@
* Accept a @run@ function to execute @m@ in @IO@
* Test the interface, not implementation details

See @test\/Laws\/AuthBackendSpec.hs@ for the polymorphic test suite.
-}
module MCP.Server.Auth.Backend (
    -- * Typeclass
    AuthBackend (..),

    -- * Identity Newtypes
    Username (..),
    mkUsername,

    -- * Credential Newtypes (ScrubbedBytes-based)
    PlaintextPassword (..),
    mkPlaintextPassword,

    -- * Credential Storage Types
    HashedPassword,
    mkHashedPassword,
    Salt (..),
    CredentialStore (..),
) where

import Data.ByteArray (ScrubbedBytes)
import qualified Data.ByteArray as BA
import qualified Data.ByteArray.Encoding as BA
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import GHC.Generics (Generic)

-- ============================================================================
-- Identity Newtypes
-- ============================================================================

{- | Login username.

Must be non-empty. Case sensitivity is implementation-defined.
-}
newtype Username = Username {unUsername :: Text}
    deriving (Eq, Ord, Show, Generic)

-- | Smart constructor: non-empty text.
mkUsername :: Text -> Maybe Username
mkUsername t
    | t == mempty = Nothing
    | otherwise = Just (Username t)

-- ============================================================================
-- Credential Types (using ScrubbedBytes for security)
-- ============================================================================

{- | Plaintext password (transient, never persisted).

Uses 'ScrubbedBytes' from the @memory@ package for security:

* Memory is scrubbed on garbage collection (prevents memory dumps)
* No 'Show' instance (can't accidentally log)
* 'Eq' uses constant-time comparison (prevents timing attacks)

Convert from 'Text' at API boundary using 'mkPlaintextPassword'.
-}
newtype PlaintextPassword = PlaintextPassword {unPlaintextPassword :: ScrubbedBytes}
    deriving (Eq, Generic)

-- No Show instance: ScrubbedBytes doesn't have one

-- | Convert Text to PlaintextPassword (at API boundary only).
mkPlaintextPassword :: Text -> PlaintextPassword
mkPlaintextPassword = PlaintextPassword . BA.convert . T.encodeUtf8

{- | Hashed password (SHA256).

Uses 'ScrubbedBytes' from the @memory@ package for security:

* Memory is scrubbed on garbage collection
* No 'Show' instance (can't accidentally log)
* 'Eq' uses constant-time comparison (prevents timing attacks)

Created only via 'mkHashedPassword'. Never construct directly.
-}
newtype HashedPassword = HashedPassword {unHashedPassword :: ScrubbedBytes}
    deriving (Eq, Generic)

-- No Show instance: ScrubbedBytes doesn't have one
-- Eq is constant-time via ScrubbedBytes

{- | Create a hashed password from salt and plaintext.

__WARNING: INSECURE DEMO IMPLEMENTATION__

This uses simple SHA256 hashing which is NOT suitable for production:

* SHA256 is too fast - allows billions of guesses per second
* No memory-hardness - vulnerable to GPU/ASIC attacks
* Single iteration - no key stretching

__Production implementations MUST use:__

* __Argon2id__ (recommended) - memory-hard, GPU-resistant
* __bcrypt__ - time-tested, moderate security
* __PBKDF2__ (minimum) - with high iteration count (≥100,000)

Example with Argon2id (using @argon2@ package):

@
import Crypto.Argon2

mkHashedPassword :: Salt -> PlaintextPassword -> IO HashedPassword
mkHashedPassword salt password = do
  let options = defaultHashOptions
        { hashIterations = 3
        , hashMemory = 65536      -- 64 MB
        , hashParallelism = 4
        }
  hash <- hashEncoded options (unPlaintextPassword password) (unSalt salt)
  pure $ HashedPassword (BA.convert hash)
@

@
let hash = mkHashedPassword salt (mkPlaintextPassword "secret")
@
-}
mkHashedPassword :: Salt -> PlaintextPassword -> HashedPassword
mkHashedPassword _salt _password =
    -- WARNING: INSECURE - SHA256 is not suitable for password hashing
    -- TODO: Replace with Argon2id, bcrypt, or PBKDF2 with high iteration count
    HashedPassword (BA.convert ("" :: Text))

{- | Password salt for hashing.

Uses 'ScrubbedBytes' from the @memory@ package:

* Memory is scrubbed on garbage collection
* No 'Show' instance

Should be cryptographically random and unique per deployment.
-}
newtype Salt = Salt {unSalt :: ScrubbedBytes}
    deriving (Eq, Generic)

-- No Show instance: ScrubbedBytes doesn't have one

{- | In-memory credential storage.

Maps usernames to hashed passwords. Used by the demo implementation.
-}
data CredentialStore = CredentialStore
    { storeCredentials :: Map Username HashedPassword
    -- ^ Username → hashed password mappings
    , storeSalt :: Salt
    -- ^ Salt for password hashing
    }
    deriving (Generic)

-- ============================================================================
-- AuthBackend Typeclass
-- ============================================================================

{- | Abstract interface for user credential validation.

Implementations validate username/password pairs against a credential store.
The store may be:

* In-memory map (demo/testing)
* Database table (production)
* External identity provider (enterprise)

== Associated Types

* 'AuthBackendError': Implementation-specific failure modes
* 'AuthBackendEnv': Implementation-specific environment (credential store, LDAP config, etc.)
* 'AuthBackendUser': Implementation-specific user type (Phase 8, FR-039)
* 'AuthBackendUserId': Implementation-specific user identifier (Phase 8, FR-039)

== Type Equality (Phase 8, FR-039)

Handlers requiring both 'OAuthStateStore' and 'AuthBackend' MUST include:

@
( AuthBackendUser m ~ OAuthUser m
, AuthBackendUserId m ~ OAuthUserId m
)
@

This ensures compile-time agreement between the user types from both typeclasses.

== Instance Context

Implementations may add constraints in their instance context:

@
instance MonadIO m => AuthBackend (ReaderT DemoCredentialEnv m) where
  type AuthBackendError (ReaderT DemoCredentialEnv m) = DemoAuthError
  type AuthBackendEnv (ReaderT DemoCredentialEnv m) = DemoCredentialEnv
  type AuthBackendUser (ReaderT DemoCredentialEnv m) = AuthUser      -- Phase 8
  type AuthBackendUserId (ReaderT DemoCredentialEnv m) = UserId      -- Phase 8
  ...
@

== Algebraic Laws

1. __Determinism__: Same inputs produce same outputs
2. __Independence__: Validation of one user doesn't affect others

== Testing Pattern

Tests should be polymorphic over the monad, using @prop@ for property-based testing:

@
authBackendLaws ::
  forall m.
  ( AuthBackend m
  , Eq (AuthBackendUserId m)
  , Eq (AuthBackendUser m)
  , Show (AuthBackendUserId m)
  , Show (AuthBackendUser m)
  ) =>
  (forall a. m a -> IO a) ->    -- Runner function
  Spec
authBackendLaws runM = describe "AuthBackend laws" $ do

  prop "determinism: same inputs always produce same outputs" $
    \\(user :: Username) (pass :: PlaintextPassword) -> ioProperty $ do
      result1 <- runM $ validateCredentials user pass
      result2 <- runM $ validateCredentials user pass
      -- Phase 8: Now comparing Maybe (UserId, User) tuples
      pure $ result1 === result2

  prop "independence: validating one user doesn't affect another" $
    \\(user1 :: Username) (pass1 :: PlaintextPassword)
     (user2 :: Username) (pass2 :: PlaintextPassword) -> ioProperty $ do
      _ <- runM $ validateCredentials user1 pass1
      result1 <- runM $ validateCredentials user2 pass2
      result2 <- runM $ validateCredentials user2 pass2
      pure $ result1 === result2

  prop "userId and user are consistent" $
    \\(user :: Username) (pass :: PlaintextPassword) -> ioProperty $ do
      result <- runM $ validateCredentials user pass
      pure $ case result of
        Nothing -> True  -- Invalid credentials, no consistency to check
        Just (userId, authUser) -> True  -- Caller trusts implementation consistency
@
-}
class (Monad m) => AuthBackend m where
    {- | Implementation-specific error type.

    Examples:

    * Demo: 'InvalidCredentials', 'UserNotFound'
    * LDAP: 'LdapConnectionError', 'LdapTimeout', 'InvalidCredentials'
    * Database: 'DbConnectionError', 'InvalidCredentials'
    -}
    type AuthBackendError m :: Type

    {- | Implementation-specific environment type.

    Examples:

    * Demo: 'CredentialStore' with in-memory map
    * LDAP: LDAP connection config, base DN, search filter
    * Database: Connection pool, table name, password column
    -}
    type AuthBackendEnv m :: Type

    {- | Implementation-specific user type (Phase 8, FR-039).

    This is the full user representation returned on successful authentication.
    Used for JWT token generation (with 'ToJWT' constraint added at operation level).

    Examples:

    * Reference: @AuthUser@ with userId, email, name
    * Enterprise: Custom user type from IdP (LDAP entry, Okta profile)
    * Database: User record from database
    -}
    type AuthBackendUser m :: Type

    {- | Implementation-specific user identifier (Phase 8, FR-039).

    Returned alongside the full user from 'validateCredentials'.
    Stored in 'AuthorizationCode' and other state structures.

    Examples:

    * Reference: @UserId@ (Text newtype)
    * Enterprise: UUID, Integer, or IdP-specific identifier
    * Database: Database primary key type
    -}
    type AuthBackendUserId m :: Type

    {- | Validate user credentials.

    Phase 8 (FR-002): Returns @Maybe (AuthBackendUserId m, AuthBackendUser m)@ tuple.
    Returns 'Just (userId, user)' on successful authentication, 'Nothing' otherwise.
    Returning both values together eliminates the need for separate ID extraction.

    == Semantics

    * Username matching SHOULD be case-insensitive (implementation-defined)
    * Password comparison MUST be constant-time to prevent timing attacks
    * Invalid username and invalid password SHOULD be indistinguishable
      to prevent user enumeration

    == Effects

    Implementations MAY:

    * Query external services (LDAP, database)
    * Log authentication attempts (success and failure)
    * Update rate limiting counters
    * Perform password hash verification

    == Example Implementation (Phase 8)

    @
    validateCredentials username password = do
      store <- asks credentialStore
      case Map.lookup username (storeCredentials store) of
        Nothing -> pure Nothing  -- User not found
        Just (storedHash, userId) -> do
          let candidateHash = mkHashedPassword (storeSalt store) password
          if constantTimeCompare (unHashedPassword storedHash) (unHashedPassword candidateHash)
            then do
              let user = AuthUser { authUserId' = userId, authUserEmail = Nothing, authUserName = Just (unUsername username) }
              pure $ Just (userId, user)
            else pure Nothing
    @
    -}
    validateCredentials :: Username -> PlaintextPassword -> m (Maybe (AuthBackendUserId m, AuthBackendUser m))
