# Quickstart: Custom Backend Implementation

**Feature Branch**: `004-oauth-auth-typeclasses`

This guide walks through implementing custom `OAuthStateStore` and `AuthBackend` backends. The default in-memory implementations work for development, but production deployments typically need PostgreSQL, Redis, or LDAP backends.

## Prerequisites

Before implementing a custom backend:

1. Review typeclass definitions in `src/Servant/OAuth2/IDP/Store.hs` (OAuthStateStore)
2. Review typeclass definitions in `src/Servant/OAuth2/IDP/Auth/Backend.hs` (AuthBackend)
3. Understand the associated types pattern used throughout

## Step 1: Define Environment and Error Types

Each backend needs its own environment (configuration/connections) and error types.

### PostgreSQL Backend Example

```haskell
-- src/MyApp/OAuth/PostgresStore.hs
{-# LANGUAGE TypeFamilies #-}

module MyApp.OAuth.PostgresStore where

import Database.PostgreSQL.Simple (Connection)
import Data.Pool (Pool)

-- Environment: connection pool for PostgreSQL
data PostgresEnv = PostgresEnv
  { pgPool :: Pool Connection
  , pgExpiryConfig :: ExpiryConfig  -- from Servant.OAuth2.IDP.Store
  }

-- Errors specific to PostgreSQL operations
data PostgresStoreError
  = PgConnectionError Text
  | PgQueryError Text
  | PgConstraintViolation Text
  deriving (Show, Eq)
```

### LDAP Backend Example

```haskell
-- src/MyApp/OAuth/LdapAuth.hs
{-# LANGUAGE TypeFamilies #-}

module MyApp.OAuth.LdapAuth where

import qualified LDAP.Search as LDAP

-- Environment: LDAP connection settings
data LdapEnv = LdapEnv
  { ldapHost :: Text
  , ldapPort :: Int
  , ldapBaseDN :: Text
  , ldapBindDN :: Text
  , ldapBindPassword :: Text  -- Consider using ScrubbedBytes
  }

-- Errors specific to LDAP operations
data LdapAuthError
  = LdapConnectionFailed Text
  | LdapSearchFailed Text
  | LdapBindFailed Text
  deriving (Show, Eq)
```

## Step 2: Implement OAuthStateStore Instance

The `OAuthStateStore` typeclass manages all OAuth state: authorization codes, tokens, clients, and pending authorizations.

### Associated Types

```haskell
class (Monad m, MonadTime m) => OAuthStateStore m where
  -- Your error type (domain-specific failures)
  type OAuthStateError m :: Type

  -- Your environment type (configuration/connections)
  type OAuthStateEnv m :: Type

  -- Your user type (stored in auth codes and tokens, contains user ID as field)
  type OAuthUser m :: Type
```

### Required Methods

Implement these methods (see `Store.hs` for full signatures):

**Authorization Codes** (single-use, with expiry):
- `storeAuthCode :: AuthCodeId -> AuthorizationCode (OAuthUser m) -> m ()`
- `lookupAuthCode :: AuthCodeId -> m (Maybe (AuthorizationCode (OAuthUser m)))`
- `deleteAuthCode :: AuthCodeId -> m ()`
- `consumeAuthCode :: AuthCodeId -> m (Maybe (AuthorizationCode (OAuthUser m)))` -- Atomic lookup+delete

**Access Tokens**:
- `storeAccessToken :: AccessTokenId -> OAuthUser m -> m ()`
- `lookupAccessToken :: AccessTokenId -> m (Maybe (OAuthUser m))`

**Refresh Tokens**:
- `storeRefreshToken :: RefreshTokenId -> (ClientId, OAuthUser m) -> m ()`
- `lookupRefreshToken :: RefreshTokenId -> m (Maybe (ClientId, OAuthUser m))`
- `updateRefreshToken :: RefreshTokenId -> RefreshTokenId -> m ()`

**Client Registration**:
- `storeClient :: ClientId -> ClientInfo -> m ()`
- `lookupClient :: ClientId -> m (Maybe ClientInfo)`

**Pending Authorizations** (login sessions):
- `storePendingAuth :: SessionId -> PendingAuthorization -> m ()`
- `lookupPendingAuth :: SessionId -> m (Maybe PendingAuthorization)`
- `deletePendingAuth :: SessionId -> m ()`

### PostgreSQL Implementation Example

```haskell
{-# LANGUAGE TypeFamilies #-}

module MyApp.OAuth.PostgresStore where

import Control.Monad.Reader (ReaderT, asks)
import Control.Monad.Time (MonadTime(..))
import Data.Pool (withResource)
import Database.PostgreSQL.Simple
import Servant.OAuth2.IDP.Store
import Servant.OAuth2.IDP.Types

-- Your user type (must match what's stored in codes/tokens)
data DbUser = DbUser
  { dbUserId :: Int
  , dbUserEmail :: Text
  , dbUserName :: Text
  } deriving (Generic, FromRow, ToRow)

instance (MonadIO m, MonadTime m) => OAuthStateStore (ReaderT PostgresEnv m) where
  type OAuthStateError (ReaderT PostgresEnv m) = PostgresStoreError
  type OAuthStateEnv (ReaderT PostgresEnv m) = PostgresEnv
  type OAuthUser (ReaderT PostgresEnv m) = DbUser

  -- Store authorization code with expiry
  storeAuthCode codeId authCode = do
    pool <- asks pgPool
    liftIO $ withResource pool $ \conn ->
      execute conn
        "INSERT INTO auth_codes (id, client_id, user_data, redirect_uri, \
        \code_challenge, code_challenge_method, scope, expires_at) \
        \VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
        ( unAuthCodeId codeId
        , unClientId (authCodeClientId authCode)
        , encode (authCodeUser authCode)  -- JSON encode user
        , unRedirectUri (authCodeRedirectUri authCode)
        , unCodeChallenge (authCodeChallenge authCode)
        , show (authCodeChallengeMethod authCode)
        , unwords $ map unScope (authCodeScope authCode)
        , authCodeExpiry authCode
        )
    pure ()

  -- Lookup with expiry check
  lookupAuthCode codeId = do
    pool <- asks pgPool
    now <- getCurrentTime
    results <- liftIO $ withResource pool $ \conn ->
      query conn
        "SELECT client_id, user_data, redirect_uri, code_challenge, \
        \code_challenge_method, scope, expires_at \
        \FROM auth_codes WHERE id = ? AND expires_at > ?"
        (unAuthCodeId codeId, now)
    pure $ listToMaybe $ map rowToAuthCode results

  -- Delete authorization code
  deleteAuthCode codeId = do
    pool <- asks pgPool
    liftIO $ withResource pool $ \conn ->
      execute conn "DELETE FROM auth_codes WHERE id = ?" (Only $ unAuthCodeId codeId)
    pure ()

  -- Atomic consume: lookup + delete in transaction
  consumeAuthCode codeId = do
    pool <- asks pgPool
    now <- getCurrentTime
    liftIO $ withResource pool $ \conn -> withTransaction conn $ do
      results <- query conn
        "SELECT client_id, user_data, redirect_uri, code_challenge, \
        \code_challenge_method, scope, expires_at \
        \FROM auth_codes WHERE id = ? AND expires_at > ? FOR UPDATE"
        (unAuthCodeId codeId, now)
      case listToMaybe results of
        Nothing -> pure Nothing
        Just row -> do
          execute conn "DELETE FROM auth_codes WHERE id = ?" (Only $ unAuthCodeId codeId)
          pure $ Just $ rowToAuthCode row

  -- Similar implementations for other methods...
  storeAccessToken = undefined  -- TODO: implement
  lookupAccessToken = undefined
  storeRefreshToken = undefined
  lookupRefreshToken = undefined
  updateRefreshToken = undefined
  storeClient = undefined
  lookupClient = undefined
  storePendingAuth = undefined
  lookupPendingAuth = undefined
  deletePendingAuth = undefined

-- Helper to convert database row to AuthorizationCode
rowToAuthCode :: (Text, ByteString, Text, Text, Text, Text, UTCTime)
              -> AuthorizationCode DbUser
rowToAuthCode (clientId, userData, redirectUri, challenge, method, scopeStr, expiry) =
  AuthorizationCode
    { authCodeClientId = ClientId clientId
    , authCodeUser = fromJust $ decode userData
    , authCodeRedirectUri = RedirectUri redirectUri
    , authCodeChallenge = CodeChallenge challenge
    , authCodeChallengeMethod = read method
    , authCodeScope = map Scope $ words scopeStr
    , authCodeExpiry = expiry
    }
```

## Step 3: Implement AuthBackend Instance

The `AuthBackend` typeclass validates user credentials (username/password).

### Associated Types

```haskell
class Monad m => AuthBackend m where
  -- Your error type (e.g., LdapError, DbError)
  type AuthBackendError m :: Type

  -- Your environment type (e.g., LdapEnv, DbConnectionPool)
  type AuthBackendEnv m :: Type

  -- Your user type (returned on successful auth, contains user ID as field)
  type AuthBackendUser m :: Type
```

### Required Method

```haskell
validateCredentials :: Username
                    -> PlaintextPassword
                    -> m (Maybe (AuthBackendUser m))
```

Returns `Just user` on valid credentials, `Nothing` otherwise. The user type should contain a user ID field that gets encoded into JWT via `ToJWT`.

### LDAP Implementation Example

```haskell
{-# LANGUAGE TypeFamilies #-}

module MyApp.OAuth.LdapAuth where

import Control.Monad.Reader (ReaderT, asks)
import qualified LDAP.Search as LDAP
import Servant.OAuth2.IDP.Auth.Backend

-- Your LDAP user type (contains user ID as field, encoded to JWT via ToJWT)
data LdapUser = LdapUser
  { ldapUid :: Text           -- User ID field
  , ldapEmail :: Text
  , ldapDisplayName :: Text
  , ldapGroups :: [Text]
  } deriving (Show, Eq, Generic)

-- ToJWT instance encodes user ID into JWT claims
instance ToJWT LdapUser where
  -- Implementation encodes ldapUid into "sub" claim

instance MonadIO m => AuthBackend (ReaderT LdapEnv m) where
  type AuthBackendError (ReaderT LdapEnv m) = LdapAuthError
  type AuthBackendEnv (ReaderT LdapEnv m) = LdapEnv
  type AuthBackendUser (ReaderT LdapEnv m) = LdapUser

  validateCredentials username password = do
    env <- ask
    result <- liftIO $ runExceptT $ do
      -- Step 1: Bind as service account to search for user
      conn <- LDAP.connect (ldapHost env) (ldapPort env)
      LDAP.bind conn (ldapBindDN env) (ldapBindPassword env)

      -- Step 2: Search for user by username
      let userFilter = "(uid=" <> unUsername username <> ")"
      entries <- LDAP.search conn (ldapBaseDN env) userFilter

      case entries of
        [] -> pure Nothing
        (entry:_) -> do
          -- Step 3: Attempt bind as the found user (validates password)
          let userDN = LDAP.entryDN entry
          bindResult <- LDAP.bind conn userDN (unPlaintextPassword password)
          case bindResult of
            Left _ -> pure Nothing
            Right _ -> do
              -- Step 4: Extract user attributes
              let user = LdapUser
                    { ldapUid = LDAP.attr entry "uid"
                    , ldapEmail = LDAP.attr entry "mail"
                    , ldapDisplayName = LDAP.attr entry "displayName"
                    , ldapGroups = LDAP.attrs entry "memberOf"
                    }
              pure $ Just user

    case result of
      Left _ -> pure Nothing  -- Connection/search errors -> auth failure
      Right maybeUser -> pure maybeUser
```

## Step 4: Wire into AppEnv

Once you have custom backends, wire them into your application's composite environment.

### Define Custom AppEnv

```haskell
-- src/MyApp/AppEnv.hs
{-# LANGUAGE DeriveGeneric #-}

module MyApp.AppEnv where

import GHC.Generics (Generic)
import Data.Generics.Product (HasType)
import MyApp.OAuth.PostgresStore (PostgresEnv, PostgresStoreError)
import MyApp.OAuth.LdapAuth (LdapEnv, LdapAuthError, LdapUser)

-- Composite environment
data MyAppEnv = MyAppEnv
  { envPostgres :: PostgresEnv       -- OAuth state storage
  , envLdap :: LdapEnv               -- LDAP authentication
  , envConfig :: MyAppConfig         -- App configuration
  , envTracer :: IOTracer MyAppTrace -- Logging
  } deriving (Generic)

-- Composite error type
data MyAppError
  = PostgresErr PostgresStoreError
  | LdapErr LdapAuthError
  | ValidationErr Text
  deriving (Show)

-- Application monad
newtype MyAppM a = MyAppM
  { unMyAppM :: ReaderT MyAppEnv (ExceptT MyAppError Handler) a
  }
  deriving newtype
    ( Functor, Applicative, Monad
    , MonadReader MyAppEnv
    , MonadError MyAppError
    , MonadIO
    )
```

### Implement Typeclass Instances

```haskell
-- OAuthStateStore instance delegates to PostgreSQL
instance OAuthStateStore MyAppM where
  type OAuthStateError MyAppM = PostgresStoreError
  type OAuthStateEnv MyAppM = PostgresEnv
  type OAuthUser MyAppM = LdapUser  -- User comes from LDAP, stored in Postgres

  storeAuthCode codeId code = do
    env <- asks envPostgres
    result <- liftIO $ runReaderT (storeAuthCode codeId code) env
    either (throwError . PostgresErr) pure result

  -- Similar delegation for other methods...

-- AuthBackend instance delegates to LDAP
instance AuthBackend MyAppM where
  type AuthBackendError MyAppM = LdapAuthError
  type AuthBackendEnv MyAppM = LdapEnv
  type AuthBackendUser MyAppM = LdapUser

  validateCredentials username password = do
    env <- asks envLdap
    liftIO $ runReaderT (validateCredentials username password) env
```

### Type Alignment

When using both `OAuthStateStore` and `AuthBackend`, ensure user types align:

```haskell
-- Handlers may require type equality constraint
handleTokenExchange
  :: ( OAuthStateStore m
     , AuthBackend m
     , OAuthUser m ~ AuthBackendUser m  -- User types must match
     )
  => TokenRequest
  -> m TokenResponse
```

## Step 5: Database Schema (PostgreSQL Example)

```sql
-- Required tables for PostgreSQL OAuthStateStore

CREATE TABLE auth_codes (
    id TEXT PRIMARY KEY,
    client_id TEXT NOT NULL,
    user_data JSONB NOT NULL,
    redirect_uri TEXT NOT NULL,
    code_challenge TEXT NOT NULL,
    code_challenge_method TEXT NOT NULL,
    scope TEXT NOT NULL,
    expires_at TIMESTAMPTZ NOT NULL,
    created_at TIMESTAMPTZ DEFAULT NOW()
);

CREATE INDEX idx_auth_codes_expires ON auth_codes(expires_at);

CREATE TABLE access_tokens (
    id TEXT PRIMARY KEY,
    user_data JSONB NOT NULL,
    created_at TIMESTAMPTZ DEFAULT NOW()
);

CREATE TABLE refresh_tokens (
    id TEXT PRIMARY KEY,
    client_id TEXT NOT NULL,
    user_data JSONB NOT NULL,
    created_at TIMESTAMPTZ DEFAULT NOW()
);

CREATE TABLE registered_clients (
    id TEXT PRIMARY KEY,
    name TEXT NOT NULL,
    redirect_uris TEXT[] NOT NULL,
    grant_types TEXT[] NOT NULL,
    response_types TEXT[] NOT NULL,
    token_endpoint_auth_method TEXT NOT NULL,
    created_at TIMESTAMPTZ DEFAULT NOW()
);

CREATE TABLE pending_authorizations (
    session_id TEXT PRIMARY KEY,
    client_id TEXT NOT NULL,
    redirect_uri TEXT NOT NULL,
    response_type TEXT NOT NULL,
    scope TEXT NOT NULL,
    state TEXT,
    code_challenge TEXT NOT NULL,
    code_challenge_method TEXT NOT NULL,
    created_at TIMESTAMPTZ NOT NULL
);

CREATE INDEX idx_pending_auth_created ON pending_authorizations(created_at);
```

## Typeclass Laws

Implementations must satisfy these algebraic laws:

### Round-Trip Law
```haskell
-- Store then lookup returns stored value (if not expired)
forall id val. do
  storeAuthCode id val
  lookupAuthCode id
  == Just val  -- when getCurrentTime < authCodeExpiry val
```

### Delete Law
```haskell
-- Lookup after delete returns Nothing
forall id val. do
  storeAuthCode id val
  deleteAuthCode id
  lookupAuthCode id
  == Nothing
```

### Consume Atomicity Law
```haskell
-- consumeAuthCode is atomic: concurrent calls see different results
forall id val. do
  storeAuthCode id val
  -- Two concurrent calls:
  result1 <- consumeAuthCode id
  result2 <- consumeAuthCode id
  -- Exactly one succeeds
  (isJust result1 && isNothing result2) || (isNothing result1 && isJust result2)
```

### Overwrite Law
```haskell
-- Storing with same ID replaces previous value
forall id val1 val2. do
  storeAuthCode id val1
  storeAuthCode id val2
  lookupAuthCode id
  == Just val2
```

## Testing Your Implementation

### Property-Based Tests

```haskell
-- Using QuickCheck/Hedgehog
prop_roundTrip :: PostgresEnv -> AuthCodeId -> AuthorizationCode DbUser -> Property
prop_roundTrip env codeId code = monadicIO $ do
  result <- run $ runReaderT $ do
    storeAuthCode codeId code
    lookupAuthCode codeId
  assert $ result == Just code

prop_consumeAtomicity :: PostgresEnv -> AuthCodeId -> AuthorizationCode DbUser -> Property
prop_consumeAtomicity env codeId code = monadicIO $ do
  (r1, r2) <- run $ runReaderT $ do
    storeAuthCode codeId code
    concurrently (consumeAuthCode codeId) (consumeAuthCode codeId)
  assert $ (isJust r1 && isNothing r2) || (isNothing r1 && isJust r2)
```

### Integration Tests

```haskell
-- Test against real database
spec :: Spec
spec = around withTestDatabase $ do
  describe "PostgresStore" $ do
    it "stores and retrieves authorization codes" $ \pool -> do
      let env = PostgresEnv pool defaultExpiryConfig
      runReaderT (storeAuthCode testCodeId testCode) env
      result <- runReaderT (lookupAuthCode testCodeId) env
      result `shouldBe` Just testCode

    it "respects expiry times" $ \pool -> do
      let env = PostgresEnv pool defaultExpiryConfig
      let expiredCode = testCode { authCodeExpiry = pastTime }
      runReaderT (storeAuthCode testCodeId expiredCode) env
      result <- runReaderT (lookupAuthCode testCodeId) env
      result `shouldBe` Nothing
```

## Security Considerations

### Password Handling
- Never log `PlaintextPassword` values (they're `ScrubbedBytes` for a reason)
- Use constant-time comparison for password validation
- Production: Use Argon2id, bcrypt, or PBKDF2 (not SHA256 like demo)

### Token Storage
- Store access/refresh tokens hashed, not plaintext
- Implement token revocation lists
- Set appropriate expiry times

### SQL Injection
- Always use parameterized queries (as shown in examples)
- Never interpolate user input into SQL strings

### Connection Security
- Use TLS for LDAP connections (ldaps://)
- Use SSL for PostgreSQL connections
- Rotate service account credentials regularly

## Troubleshooting

### Common Issues

**Type mismatch: `OAuthUser m ~ AuthBackendUser m`**
- Ensure both typeclasses use the same user type
- Check that your `MyAppM` instance declares consistent associated types

**`MonadTime` not found**
- Import `Control.Monad.Time`
- Ensure your monad has a `MonadTime` instance (or derive via `MonadIO`)

**Expiry not working**
- Verify `getCurrentTime` returns current time (not stuck)
- Check database timezone settings match Haskell's `UTCTime`

**Concurrent access issues**
- PostgreSQL: Use `FOR UPDATE` in transactions
- Implement `consumeAuthCode` atomically (not as separate lookup+delete)

## References

- `src/Servant/OAuth2/IDP/Store.hs` - OAuthStateStore typeclass definition
- `src/Servant/OAuth2/IDP/Auth/Backend.hs` - AuthBackend typeclass definition
- `src/Servant/OAuth2/IDP/Store/InMemory.hs` - Reference in-memory implementation
- `src/Servant/OAuth2/IDP/Auth/Demo.hs` - Reference demo credentials implementation
- `src/MCP/Server/HTTP/AppEnv.hs` - Example composite environment wiring
