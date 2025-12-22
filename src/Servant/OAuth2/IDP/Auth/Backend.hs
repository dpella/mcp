{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Servant.OAuth2.IDP.Auth.Backend
Description : User Authentication Backend Typeclass
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC

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
module Servant.OAuth2.IDP.Auth.Backend (
    -- * Typeclass
    AuthBackend (..),

    -- * Identity Newtypes
    Username,
    mkUsername,
    usernameText,

    -- * Credential Newtypes (ScrubbedBytes-based)
    PlaintextPassword (..),
    mkPlaintextPassword,

    -- * Credential Storage Types
    HashedPassword,
    mkHashedPassword,
    Salt (..),
    CredentialStore (..),
) where

import Crypto.Hash (Digest, SHA256 (..), hashWith)
import Data.ByteArray (ScrubbedBytes)
import Data.ByteArray qualified as BA
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..), chooseInt, elements, listOf1, suchThat, vectorOf)

-- ============================================================================
-- Identity Newtypes
-- ============================================================================

{- | Login username.

Must be non-empty. Case sensitivity is implementation-defined.
-}
newtype Username = Username {unUsername :: Text}
    deriving (Eq, Ord, Show, Generic)

{- | Smart constructor for Username.

Returns 'Nothing' if the text is empty.

@
mkUsername ""         = Nothing
mkUsername "alice"    = Just (Username "alice")
@
-}
mkUsername :: Text -> Maybe Username
mkUsername t
    | T.null t = Nothing
    | otherwise = Just (Username t)

{- | Extract the Text from a Username.

@
case mkUsername "alice" of
  Just u -> usernameText u  -- "alice"
  Nothing -> error "empty username"
@
-}
usernameText :: Username -> Text
usernameText (Username t) = t

-- ============================================================================
-- Credential Types (using ScrubbedBytes for security)
-- ============================================================================

{- | Plaintext password (transient, never persisted).

Uses 'ScrubbedBytes' from the @memory@ package for security:

* Memory is scrubbed on garbage collection (prevents memory dumps)
* No 'Show' instance (can't accidentally log)
* 'Eq' uses constant-time comparison (prevents timing attacks)

Convert from 'Text' at API boundary using 'mkPlaintextPassword'.

== Example

@
let password = mkPlaintextPassword "secret"
-- password won't appear in logs or memory dumps because ScrubbedBytes redacts contents
@
-}
newtype PlaintextPassword = PlaintextPassword {unPlaintextPassword :: ScrubbedBytes}
    deriving (Eq, Generic, Show)

-- No Show instance: ScrubbedBytes doesn't have one

{- | Convert Text to PlaintextPassword (at API boundary only).

This should be called as soon as possible after receiving plaintext credentials
from the user, to ensure the sensitive data is stored in scrubbed memory.

@
let password = mkPlaintextPassword "user-input-password"
@
-}
mkPlaintextPassword :: Text -> PlaintextPassword
mkPlaintextPassword = PlaintextPassword . BA.convert . TE.encodeUtf8

{- | Hashed password (SHA256).

Uses 'ScrubbedBytes' from the @memory@ package for security:

* Memory is scrubbed on garbage collection
* No 'Show' instance (can't accidentally log)
* 'Eq' uses constant-time comparison (prevents timing attacks)

Created only via 'mkHashedPassword'. Never construct directly.

__WARNING: INSECURE DEMO IMPLEMENTATION__

The current implementation uses SHA256 which is NOT suitable for production.
See 'mkHashedPassword' documentation for secure alternatives.
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

== Example with Argon2id

Using the @argon2@ package:

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

== Current Implementation

@
let hash = mkHashedPassword salt (mkPlaintextPassword "secret")
@

Computes SHA256(salt ++ password) as a demonstration only.
-}
mkHashedPassword :: Salt -> PlaintextPassword -> HashedPassword
mkHashedPassword (Salt salt) (PlaintextPassword password) =
    -- WARNING: INSECURE - SHA256 is not suitable for password hashing
    -- This is a DEMO implementation only
    -- Production MUST use Argon2id, bcrypt, or PBKDF2
    let saltedPassword = BA.append salt password :: ScrubbedBytes
        hash = hashWith SHA256 saltedPassword :: Digest SHA256
        hashBytes = BA.convert hash
     in HashedPassword (hashBytes :: ScrubbedBytes)

{- | Password salt for hashing.

Uses 'ScrubbedBytes' from the @memory@ package:

* Memory is scrubbed on garbage collection
* No 'Show' instance

Should be cryptographically random and unique per deployment.

== Example

@
import Crypto.Random (getRandomBytes)
import qualified Data.ByteArray as BA

generateSalt :: IO Salt
generateSalt = do
  bytes <- getRandomBytes 32  -- 32 cryptographically random bytes (256 bits)
  pure $ Salt (BA.convert bytes :: ScrubbedBytes)
@
-}
newtype Salt = Salt {unSalt :: ScrubbedBytes}
    deriving (Eq, Generic)

-- No Show instance: ScrubbedBytes doesn't have one

{- | In-memory credential storage.

Maps usernames to hashed passwords. Used by the demo implementation.

== Example

@
let store = CredentialStore
      { storeCredentials = Map.fromList
          [ (Username "alice", mkHashedPassword salt (mkPlaintextPassword "secret"))
          , (Username "bob", mkHashedPassword salt (mkPlaintextPassword "hunter2"))
          ]
      , storeSalt = salt
      }
@
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

== Instance Context

Implementations may add constraints in their instance context:

@
instance MonadIO m => AuthBackend (ReaderT DemoCredentialEnv m) where
  type AuthBackendError (ReaderT DemoCredentialEnv m) = DemoAuthError
  type AuthBackendEnv (ReaderT DemoCredentialEnv m) = DemoCredentialEnv
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
  (AuthBackend m) =>
  (forall a. m a -> IO a) ->    -- Runner function
  Spec
authBackendLaws runM = describe "AuthBackend laws" $ do

  prop "determinism: same inputs always produce same outputs" $
    \\(user :: Username) (pass :: PlaintextPassword) -> ioProperty $ do
      result1 <- runM $ validateCredentials user pass
      result2 <- runM $ validateCredentials user pass
      pure $ result1 === result2

  prop "independence: validating one user doesn't affect another" $
    \\(user1 :: Username) (pass1 :: PlaintextPassword)
     (user2 :: Username) (pass2 :: PlaintextPassword) -> ioProperty $ do
      _ <- runM $ validateCredentials user1 pass1
      result1 <- runM $ validateCredentials user2 pass2
      result2 <- runM $ validateCredentials user2 pass2
      pure $ result1 === result2
@

== Example Implementation

@
instance MonadIO m => AuthBackend (ReaderT CredentialStore m) where
  type AuthBackendError (ReaderT CredentialStore m) = Text
  type AuthBackendEnv (ReaderT CredentialStore m) = CredentialStore
  type AuthBackendUser (ReaderT CredentialStore m) = AuthUser

  validateCredentials username password = do
    store <- ask
    let storedHash = Map.lookup username (storeCredentials store)
    case storedHash of
      Nothing -> pure Nothing  -- User not found (same as invalid password)
      Just hash -> do
        let candidateHash = mkHashedPassword (storeSalt store) password
        if hash == candidateHash  -- Constant-time comparison via Eq
          then do
            let user = userFromUsername username
            pure $ Just user
          else pure Nothing
@
-}
class (Monad m) => AuthBackend m where
    {- | Implementation-specific error type.

    Examples:

    * Demo: 'Text' (simple error messages)
    * LDAP: Custom ADT with 'LdapConnectionError', 'LdapTimeout', 'InvalidCredentials'
    * Database: Custom ADT with 'DbConnectionError', 'InvalidCredentials'
    -}
    type AuthBackendError m :: Type

    {- | Implementation-specific environment type.

    Examples:

    * Demo: 'CredentialStore' with in-memory map
    * LDAP: LDAP connection config, base DN, search filter
    * Database: Connection pool, table name, password column
    -}
    type AuthBackendEnv m :: Type

    {- | The full authenticated user type for this implementation.

    Contains all user data (name, email, roles, etc.) used for token generation.
    This is analogous to 'OAuthUser' in the OAuthStateStore typeclass.

    Examples:

    * Simple: @type AuthBackendUser MyMonad = AuthUser@
    * Custom: @type AuthBackendUser MyMonad = MyCustomUser@
    * LDAP: @type AuthBackendUser MyMonad = LdapUserRecord@
    -}
    type AuthBackendUser m :: Type

    {- | Validate user credentials.

    Returns 'Just user' if the username/password pair is valid,
    'Nothing' otherwise. The returned user object contains all user data
    (name, email, roles, etc.) used for token generation.

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

    == Example Implementation

    @
    validateCredentials username password = do
      store <- asks credentialStore
      case Map.lookup username (storeCredentials store) of
        Nothing -> pure Nothing  -- User not found (same as invalid password)
        Just hash -> do
          let candidateHash = mkHashedPassword (storeSalt store) password
          if hash == candidateHash  -- Constant-time via ScrubbedBytes Eq
            then do
              -- Extract user data from store
              let user = userFromUsername username
              pure $ Just user
            else pure Nothing
    @
    -}
    validateCredentials :: Username -> PlaintextPassword -> m (Maybe (AuthBackendUser m))

-- ============================================================================
-- QuickCheck Arbitrary Instances
-- ============================================================================

{- |
These Arbitrary instances live in the type-defining module to:

1. Have access to constructors for generation (required)
2. Enable QuickCheck as library dependency (dead code elimination removes unused instances)
3. Allow tests to be library consumers using smart constructors only
-}
instance Arbitrary Username where
    arbitrary = do
        -- Generate valid usernames (alphanumeric + underscore/dot)
        let validChars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ ['_', '.']
        len <- chooseInt (1, 20) -- Reasonable username length
        username <- T.pack <$> vectorOf len (elements validChars)
        maybe arbitrary pure (mkUsername username) -- Retry if validation fails (shouldn't happen)
    shrink u = [u' | s <- shrink (T.unpack (usernameText u)), not (null s), Just u' <- [mkUsername (T.pack s)]]

-- PlaintextPassword: generate via mkPlaintextPassword (ScrubbedBytes)
instance Arbitrary PlaintextPassword where
    arbitrary = do
        password <- T.pack <$> listOf1 (elements (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ ['!', '@', '#', '$', '%']))
        pure $ mkPlaintextPassword password
    shrink _ = [] -- Don't shrink passwords (sensitive data)

-- HashedPassword: generate via mkHashedPassword
instance Arbitrary HashedPassword where
    arbitrary = do
        salt <- arbitrary
        mkHashedPassword salt <$> arbitrary
    shrink _ = [] -- Don't shrink hashes

-- Salt: generate random bytes
instance Arbitrary Salt where
    arbitrary = do
        let saltChars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']
        saltText <- T.pack <$> vectorOf 32 (elements saltChars)
        pure $ Salt (BA.convert (TE.encodeUtf8 saltText))
    shrink _ = [] -- Don't shrink salts

-- CredentialStore: generate map of usernames to hashed passwords
instance Arbitrary CredentialStore where
    arbitrary = do
        salt <- arbitrary
        -- Generate 1-5 users
        users <- listOf1 arbitrary `suchThat` (\xs -> length xs <= 5)
        passwords <- vectorOf (length users) arbitrary
        let credentials = zip users passwords
        let storeCredentials = foldr (\(u, p) m -> Map.insert u (mkHashedPassword salt p) m) Map.empty credentials
        pure CredentialStore{storeCredentials, storeSalt = salt}
    shrink _ = [] -- Don't shrink credential stores (complex)
