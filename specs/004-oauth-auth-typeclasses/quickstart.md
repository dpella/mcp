# Quickstart: OAuth State and Authentication Typeclasses

**Feature Branch**: `004-oauth-auth-typeclasses`

> **Note**: Code samples in this document are **illustrative pseudo-code**. Function bodies have not been compiled or tested. Focus on **type signatures, interfaces, and structural intent** rather than implementation details.

## Overview

This feature introduces two typeclasses that abstract OAuth state persistence and user authentication:

1. **`OAuthStateStore`**: Manages authorization codes, tokens, clients, and pending authorizations
2. **`AuthBackend`**: Validates user credentials

Both typeclasses use the three-layer cake architecture with:
- Polymorphic `m` monad parameter
- Associated types for errors and environments
- Strong newtypes for domain values (no raw `Text`/`String`/`Int`)
- `MonadTime` for testable time-dependent operations

## Key Design Principles

### Strong Types Over Primitives

```haskell
-- OLD: Stringly-typed, error-prone
lookupAuthCode :: Text -> m (Maybe AuthorizationCode)
validateCredentials :: Text -> Text -> m Bool

-- NEW: Type-safe, self-documenting
lookupAuthCode :: AuthCodeId -> m (Maybe AuthorizationCode)
validateCredentials :: Username -> PlaintextPassword -> m Bool
```

### ADTs Over String Enums

```haskell
-- OLD: String validation at runtime
authCodeChallengeMethod :: Text  -- "S256" or "plain" (or invalid?)

-- NEW: Invalid states unrepresentable
data CodeChallengeMethod = S256 | Plain
authCodeChallengeMethod :: CodeChallengeMethod
```

### MonadTime for Testable Expiry

```haskell
-- Time abstraction enables deterministic testing
class Monad m => MonadTime m where
  getCurrentTime :: m UTCTime

-- Production: real time
instance MonadTime IO where
  getCurrentTime = Data.Time.getCurrentTime

-- Tests: controlled time
instance MonadTime (Reader UTCTime) where
  getCurrentTime = ask
```

## Quick Examples

### Using the Default In-Memory Implementation

```haskell
-- No changes required! The existing example server works identically:
import MCP.Server.HTTP (runServerHTTP, defaultDemoOAuthConfig)

main :: IO ()
main = do
  let config = HTTPServerConfig
        { httpOAuthConfig = Just defaultDemoOAuthConfig
        , ...
        }
  runServerHTTP config tracer
```

### Implementing a Custom Storage Backend

```haskell
{-# LANGUAGE TypeFamilies #-}

import MCP.Server.OAuth.Store

-- 1. Define your environment type
data PostgresOAuthEnv = PostgresOAuthEnv
  { pgConn :: Connection
  , pgExpiryConfig :: ExpiryConfig
  }

-- 2. Define your error type
data PostgresOAuthError
  = PgConnectionError Text
  | PgQueryError Text
  deriving (Eq, Show, Generic)

-- 3. Implement the typeclass (requires MonadTime)
instance (MonadIO m, MonadTime m) => OAuthStateStore (ReaderT PostgresOAuthEnv m) where
  type OAuthStateError (ReaderT PostgresOAuthEnv m) = PostgresOAuthError
  type OAuthStateEnv (ReaderT PostgresOAuthEnv m) = PostgresOAuthEnv

  storeAuthCode code = do
    conn <- asks pgConn
    liftIO $ execute conn
      "INSERT INTO auth_codes (code_id, client_id, ...) VALUES (?, ?, ...)"
      (unAuthCodeId (authCodeId code), unClientId (authClientId code), ...)

  lookupAuthCode key = do
    conn <- asks pgConn
    now <- getCurrentTime  -- Uses MonadTime, not IO!
    liftIO $ query conn
      "SELECT * FROM auth_codes WHERE code_id = ? AND expiry > ?"
      (unAuthCodeId key, now)

  -- ... implement remaining methods
```

### Implementing a Custom Auth Backend

```haskell
{-# LANGUAGE TypeFamilies #-}

import MCP.Server.Auth.Backend

-- 1. Define your environment type
data LdapAuthEnv = LdapAuthEnv
  { ldapHost :: Text
  , ldapPort :: Int
  , ldapBaseDN :: Text
  }

-- 2. Define your error type
data LdapAuthError
  = LdapConnectionFailed Text
  | LdapTimeout
  | LdapInvalidCredentials
  deriving (Eq, Show, Generic)

-- 3. Implement the typeclass
instance MonadIO m => AuthBackend (ReaderT LdapAuthEnv m) where
  type AuthBackendError (ReaderT LdapAuthEnv m) = LdapAuthError
  type AuthBackendEnv (ReaderT LdapAuthEnv m) = LdapAuthEnv

  validateCredentials username password = do
    env <- ask
    result <- liftIO $ ldapBind (ldapHost env) (ldapPort env)
      (ldapBaseDN env) (unUsername username) (unPlaintextPassword password)
    pure $ isRight result
```

### Composing Multiple Typeclasses in Handlers

```haskell
{-# LANGUAGE FlexibleContexts #-}

import Data.Generics.Product (HasType, getTyped)
import Data.Generics.Sum (AsType, injectTyped)

-- Handler using both typeclasses
handleLogin ::
  ( OAuthStateStore m
  , AuthBackend m
  , MonadError e m
  , AsType (OAuthStateError m) e
  , AsType (AuthBackendError m) e
  , MonadReader env m
  , HasType (OAuthStateEnv m) env
  , HasType (AuthBackendEnv m) env
  ) => SessionId -> Username -> PlaintextPassword -> m AuthorizationCode
handleLogin sessionId username password = do
  -- Use AuthBackend
  valid <- validateCredentials username password
  unless valid $ throwError (injectTyped InvalidCredentials)

  -- Use OAuthStateStore
  pending <- lookupPendingAuth sessionId
  case pending of
    Nothing -> throwError (injectTyped SessionExpired)
    Just p -> do
      code <- generateAuthCode p username
      storeAuthCode code
      deletePendingAuth sessionId
      pure code
```

## Testing: Polymorphic Specs

Tests MUST be polymorphic over the monad to test the **interface**, not implementation.

### OAuthStateStore Law Tests

```haskell
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Laws.OAuthStateStoreSpec where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import MCP.Server.OAuth.Store

-- | Polymorphic spec: tests the INTERFACE, not implementation.
-- Requires Arbitrary instances for all domain types.
-- Accepts a 'run' function to execute 'm' in 'IO'.
oauthStateStoreLaws ::
  forall m.
  (OAuthStateStore m, MonadTime m) =>
  (forall a. m a -> IO a) ->    -- Runner function (implementation-specific)
  Spec
oauthStateStoreLaws runM = describe "OAuthStateStore laws" $ do

  describe "AuthorizationCode" $ do

    prop "round-trip: lookup after store returns the value (non-expired)" $
      \(code :: AuthorizationCode) -> ioProperty $ do
        -- Ensure code is not expired by setting expiry far in future
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

  describe "ClientInfo" $ do

    prop "round-trip: lookup after store returns the value" $
      \(clientId :: ClientId) (info :: ClientInfo) -> ioProperty $ do
        result <- runM $ do
          storeClient clientId info
          lookupClient clientId
        pure $ result === Just info

    prop "delete semantics: storing Nothing-equivalent not supported" $
      \(clientId :: ClientId) (info :: ClientInfo) -> ioProperty $ do
        result <- runM $ do
          storeClient clientId info
          lookupClient clientId
        pure $ result === Just info

  describe "Expiry behavior (requires MonadTime)" $ do

    prop "lookup returns Nothing for expired codes" $
      \(code :: AuthorizationCode) -> ioProperty $ do
        -- Make code expired by setting expiry in the past
        let expiredCode = code { authExpiry = addUTCTime (-3600) (authExpiry code) }
        result <- runM $ do
          storeAuthCode expiredCode
          lookupAuthCode (authCodeId expiredCode)
        pure $ result === Nothing

    prop "lookup returns Nothing for expired pending authorizations" $
      \(sessionId :: SessionId) (pending :: PendingAuthorization) -> ioProperty $ do
        -- Make pending auth expired
        let expiredPending = pending { pendingCreatedAt = addUTCTime (-86400) (pendingCreatedAt pending) }
        result <- runM $ do
          storePendingAuth sessionId expiredPending
          lookupPendingAuth sessionId
        pure $ result === Nothing
```

### AuthBackend Law Tests

```haskell
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Laws.AuthBackendSpec where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import MCP.Server.Auth.Backend

-- | Polymorphic spec for AuthBackend.
-- Requires Arbitrary instances for Username and PlaintextPassword.
-- Accepts a 'run' function to execute 'm' in 'IO'.
authBackendLaws ::
  forall m.
  (AuthBackend m) =>
  (forall a. m a -> IO a) ->    -- Runner function (implementation-specific)
  Spec
authBackendLaws runM = describe "AuthBackend laws" $ do

  describe "Credential validation" $ do

    prop "determinism: same inputs always produce same outputs" $
      \(user :: Username) (pass :: PlaintextPassword) -> ioProperty $ do
        result1 <- runM $ validateCredentials user pass
        result2 <- runM $ validateCredentials user pass
        pure $ result1 === result2

    prop "independence: validating one user doesn't affect another" $
      \(user1 :: Username) (pass1 :: PlaintextPassword)
       (user2 :: Username) (pass2 :: PlaintextPassword) -> ioProperty $ do
        -- Validate user1 first
        _ <- runM $ validateCredentials user1 pass1
        -- Then validate user2
        result1 <- runM $ validateCredentials user2 pass2
        -- Validate user2 without validating user1 first (fresh run)
        result2 <- runM $ validateCredentials user2 pass2
        -- Results should be identical
        pure $ result1 === result2

-- | Additional tests for known credentials (implementation-specific).
-- These are NOT polymorphic laws - they test specific credential stores.
authBackendKnownCredentials ::
  forall m.
  (AuthBackend m) =>
  (forall a. m a -> IO a) ->    -- Runner function
  Username ->                    -- Known valid username
  PlaintextPassword ->           -- Known valid password
  PlaintextPassword ->           -- Known invalid password
  Spec
authBackendKnownCredentials runM validUser validPass invalidPass =
  describe "Known credentials" $ do

    it "accepts valid credentials" $ do
      result <- runM $ validateCredentials validUser validPass
      result `shouldBe` True

    it "rejects invalid password for valid user" $ do
      result <- runM $ validateCredentials validUser invalidPass
      result `shouldBe` False

    it "rejects unknown user" $ do
      let unknownUser = Username "nonexistent_user_xyz"
      result <- runM $ validateCredentials unknownUser validPass
      result `shouldBe` False
```

### Running Tests Against Multiple Implementations

```haskell
module Main where

import Test.Hspec
import Laws.OAuthStateStoreSpec
import Laws.AuthBackendSpec
import MCP.Server.Auth.Backend (Username(..), mkPlaintextPassword)
import qualified InMemory
import qualified PureState

spec :: Spec
spec = do
  describe "InMemory implementation" $ do
    oauthStateStoreLaws InMemory.runOAuth
    authBackendLaws InMemory.runAuth
    -- Test known demo credentials
    authBackendKnownCredentials
      InMemory.runAuth
      (Username "demo")
      (mkPlaintextPassword "demo123")
      (mkPlaintextPassword "wrongpassword")

  describe "Pure State implementation" $ do
    oauthStateStoreLaws PureState.runOAuth
    authBackendLaws PureState.runAuth

main :: IO ()
main = hspec spec

-- Each implementation provides runners:
-- runOAuth :: (forall a. OAuthM a -> IO a)  -- For OAuthStateStore tests
-- runAuth  :: (forall a. AuthM a -> IO a)   -- For AuthBackend tests
```

### Arbitrary Instances for Domain Types

```haskell
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Generators where

import Test.QuickCheck
import MCP.Server.OAuth.Store
import MCP.Server.Auth.Backend
import Data.Time
import qualified Data.Text as Text
import qualified Data.Set as Set
import Data.List.NonEmpty (NonEmpty(..))

-- Identity newtypes: generate non-empty text
instance Arbitrary AuthCodeId where
  arbitrary = AuthCodeId . Text.pack . getNonEmpty <$> arbitrary

instance Arbitrary ClientId where
  arbitrary = ClientId . Text.pack . getNonEmpty <$> arbitrary

instance Arbitrary SessionId where
  arbitrary = SessionId . Text.pack . getNonEmpty <$> arbitrary

instance Arbitrary UserId where
  arbitrary = UserId . Text.pack . getNonEmpty <$> arbitrary

instance Arbitrary RefreshTokenId where
  arbitrary = RefreshTokenId . Text.pack . getNonEmpty <$> arbitrary

instance Arbitrary Username where
  arbitrary = Username . Text.pack . getNonEmpty <$> arbitrary

instance Arbitrary PlaintextPassword where
  arbitrary = PlaintextPassword . Text.pack <$> arbitrary

-- Value newtypes
instance Arbitrary Scope where
  arbitrary = Scope . Text.pack . filter (not . isSpace) . getNonEmpty <$> arbitrary

instance Arbitrary CodeChallenge where
  arbitrary = CodeChallenge . Text.pack . take 64 . getNonEmpty <$> arbitrary

-- ADTs: use bounded enum
instance Arbitrary CodeChallengeMethod where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary GrantType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ResponseType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ClientAuthMethod where
  arbitrary = arbitraryBoundedEnum

-- Domain entities
instance Arbitrary AuthorizationCode where
  arbitrary = AuthorizationCode
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary  -- RedirectUri needs special handling
    <*> arbitrary
    <*> arbitrary
    <*> (Set.fromList <$> arbitrary)
    <*> arbitrary
    <*> arbitrary

instance Arbitrary ClientInfo where
  arbitrary = ClientInfo
    <$> (Text.pack <$> arbitrary)
    <*> ((:|) <$> arbitrary <*> arbitrary)  -- NonEmpty
    <*> (Set.fromList <$> arbitrary)
    <*> (Set.fromList <$> arbitrary)
    <*> arbitrary

instance Arbitrary PendingAuthorization where
  arbitrary = PendingAuthorization
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> (fmap Text.pack <$> arbitrary)
    <*> arbitrary
    <*> arbitrary

instance Arbitrary AuthUser where
  arbitrary = AuthUser
    <$> arbitrary
    <*> (fmap Text.pack <$> arbitrary)
    <*> (fmap Text.pack <$> arbitrary)

-- Time: generate times within reasonable range
instance Arbitrary UTCTime where
  arbitrary = do
    days <- choose (0, 365 * 10)  -- Within 10 years
    secs <- choose (0, 86400)
    let baseDay = fromGregorian 2020 1 1
    pure $ UTCTime (addDays days baseDay) (secondsToDiffTime secs)
```

### Test Monad with Controlled Time

```haskell
module TestMonad where

import Control.Monad.Reader
import Data.Time

-- Test monad with controllable time
newtype TestM a = TestM
  { unTestM :: ReaderT TestEnv IO a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader TestEnv)

data TestEnv = TestEnv
  { testTime :: UTCTime        -- Controlled time
  , testOAuthState :: TVar OAuthState
  }

instance MonadTime TestM where
  getCurrentTime = asks testTime  -- Returns controlled time, not real time!

-- Runner: creates environment with specific time
runTestM :: UTCTime -> TestM a -> IO a
runTestM time action = do
  stateVar <- newTVarIO emptyOAuthState
  let env = TestEnv { testTime = time, testOAuthState = stateVar }
  runReaderT (unTestM action) env

-- Example: test expiry with controlled time
testExpiry :: IO ()
testExpiry = do
  let baseTime = UTCTime (fromGregorian 2025 1 1) 0
      code = AuthorizationCode
        { authCodeId = AuthCodeId "test"
        , authExpiry = addUTCTime 3600 baseTime  -- Expires 1 hour after baseTime
        , ...
        }

  -- At baseTime: code is valid
  result1 <- runTestM baseTime $ do
    storeAuthCode code
    lookupAuthCode (authCodeId code)
  result1 `shouldBe` Just code

  -- At baseTime + 2 hours: code is expired
  result2 <- runTestM (addUTCTime 7200 baseTime) $ do
    storeAuthCode code
    lookupAuthCode (authCodeId code)
  result2 `shouldBe` Nothing
```

## Module Structure

```text
src/MCP/Server/
├── Auth.hs                  # Re-exports, credential types
├── Auth/
│   ├── Backend.hs          # AuthBackend typeclass
│   └── Demo.hs             # Demo credential implementation
├── OAuth/
│   ├── Store.hs            # OAuthStateStore typeclass + MonadTime
│   ├── Types.hs            # Newtypes, ADTs, domain entities
│   └── InMemory.hs         # TVar-based implementation
├── Time.hs                  # MonadTime instances
└── HTTP.hs                  # Servant handlers (uses typeclass constraints)

test/
├── Main.hs                  # Test entry point
├── Laws/
│   ├── OAuthStateStoreSpec.hs  # Polymorphic typeclass law tests
│   └── AuthBackendSpec.hs      # Polymorphic typeclass law tests
├── Generators.hs            # Arbitrary instances for newtypes
└── TestMonad.hs             # Test monad with MonadTime
```

## Key Types Summary

### Identity Newtypes

| Type | Purpose | Smart Constructor |
|------|---------|-------------------|
| `AuthCodeId` | Authorization code key | `mkAuthCodeId` |
| `ClientId` | OAuth client key | `mkClientId` |
| `SessionId` | Login session key (UUID) | `mkSessionId` |
| `UserId` | User identifier | `mkUserId` |
| `RefreshTokenId` | Refresh token key | `mkRefreshTokenId` |
| `Username` | Login username | `mkUsername` |

### Value Newtypes

| Type | Purpose | Smart Constructor |
|------|---------|-------------------|
| `RedirectUri` | Validated OAuth redirect URI | `mkRedirectUri` |
| `Scope` | OAuth scope (non-empty, no whitespace) | `mkScope` |
| `CodeChallenge` | PKCE challenge (base64url) | `mkCodeChallenge` |

### Secure Credential Types (ScrubbedBytes-based)

| Type | Purpose | Smart Constructor |
|------|---------|-------------------|
| `PlaintextPassword` | Transient password (auto-scrub, no Show, constant-time Eq) | `mkPlaintextPassword` |
| `HashedPassword` | SHA256 hash (auto-scrub, no Show, constant-time Eq) | `mkHashedPassword` |
| `Salt` | Password salt (auto-scrub, no Show) | N/A |

### ADTs

| Type | Values | Purpose |
|------|--------|---------|
| `CodeChallengeMethod` | `S256 \| Plain` | PKCE method |
| `GrantType` | `GrantAuthorizationCode \| GrantRefreshToken \| ...` | OAuth grant |
| `ResponseType` | `ResponseCode \| ResponseToken` | OAuth response |
| `ClientAuthMethod` | `AuthNone \| AuthClientSecretPost \| ...` | Client auth |

## Dependencies

Add to your `.cabal` file:

```cabal
build-depends:
  , generic-lens ^>=2.2
  , memory ^>=0.18          -- ScrubbedBytes for secure credentials
  , QuickCheck ^>=2.14
  , hspec-quickcheck ^>=0.2
  , quickcheck-instances ^>=0.3
  , network-uri ^>=2.6
```

## Type Witness Patterns (FR-038)

When writing type-directed polymorphic test helpers, **NEVER** use `undefined :: Type` as a type witness. Use modern Haskell patterns instead:

### Preferred: Type Applications

```haskell
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Generic round-trip test using @Type syntax
httpApiRoundTrip
  :: forall a. (FromHttpApiData a, ToHttpApiData a, Arbitrary a, Show a, Eq a, Typeable a)
  => Spec
httpApiRoundTrip = prop (show (typeRep (Proxy @a)) <> " round-trip") $ \(x :: a) ->
  parseUrlPiece (toUrlPiece x) === Right x

-- Usage: type passed at call site, no value-level witness
spec :: Spec
spec = describe "HTTP API round-trips" $ do
  httpApiRoundTrip @ClientId
  httpApiRoundTrip @AuthCodeId
  httpApiRoundTrip @SessionId
```

### Alternative: Proxy Pattern

```haskell
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | When TypeApplications syntax isn't suitable
httpApiRoundTrip
  :: forall a. (FromHttpApiData a, ToHttpApiData a, Arbitrary a, Show a, Eq a, Typeable a)
  => Proxy a -> Spec
httpApiRoundTrip _ = prop (show (typeRep (Proxy @a)) <> " round-trip") $ \(x :: a) ->
  parseUrlPiece (toUrlPiece x) === Right x

-- Usage: Proxy @Type is safe (unlike undefined)
spec :: Spec
spec = describe "HTTP API round-trips" $ do
  httpApiRoundTrip (Proxy @ClientId)
  httpApiRoundTrip (Proxy @AuthCodeId)
```

### PROHIBITED Pattern

```haskell
-- NEVER do this - undefined can cause runtime errors if accidentally evaluated
httpApiRoundTrip "ClientId" (undefined :: ClientId)  -- BAD
prop_roundTrip (undefined :: ClientId)               -- BAD
```

### Required Extensions

```haskell
{-# LANGUAGE AllowAmbiguousTypes #-}  -- Enables type-only parameters
{-# LANGUAGE TypeApplications #-}     -- Enables @Type syntax
{-# LANGUAGE ScopedTypeVariables #-}  -- Binds type vars in where clauses
```

## Next Steps

1. **Production storage**: Implement PostgreSQL/Redis backend
2. **Enterprise auth**: Implement LDAP/Okta backend
3. **Property tests**: Verify typeclass laws against all implementations
4. **Time testing**: Use `MonadTime` to test expiry edge cases deterministically
