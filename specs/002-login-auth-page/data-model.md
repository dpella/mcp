# Data Model: Login Landing Page and User Authentication

**Feature**: 002-login-auth-page
**Date**: 2025-12-08

## Entities

### 1. PendingAuthorization

Represents an OAuth authorization request awaiting user authentication.

**Purpose**: Stores OAuth parameters between `/authorize` (GET) and `/login` (POST) to maintain flow continuity.

```haskell
data PendingAuthorization = PendingAuthorization
  { pendingClientId          :: Text           -- OAuth client requesting access
  , pendingRedirectUri       :: Text           -- Where to redirect after auth
  , pendingCodeChallenge     :: Text           -- PKCE challenge (S256)
  , pendingCodeChallengeMethod :: Text         -- Always "S256" per MCP spec
  , pendingScope             :: Maybe Text     -- Requested permissions
  , pendingState             :: Maybe Text     -- Client-provided state for CSRF
  , pendingResource          :: Maybe Text     -- RFC8707 resource indicator
  , pendingCreatedAt         :: UTCTime        -- For session expiry checks
  }
  deriving (Show, Generic)
```

**Relationships**:
- Links to `ClientInfo` via `pendingClientId`
- Becomes `AuthorizationCode` upon successful authentication

**Lifecycle**:
```
Created (GET /authorize) → Validated (POST /login) → Converted to AuthCode → Deleted
                        ↘ Expired (timeout)      → Deleted
                        ↘ Denied (user action)   → Deleted
```

**Validation Rules**:
- `pendingClientId` MUST match a registered client
- `pendingRedirectUri` MUST match client's registered URIs
- `pendingCodeChallengeMethod` MUST be "S256"
- Session expires after 10 minutes (configurable)

---

### 2. CredentialStore

In-memory store for user credentials.

**Purpose**: Validates username/password pairs for authentication.

```haskell
data CredentialStore = CredentialStore
  { storeCredentials :: Map Text HashedPassword  -- username -> hashed password
  , storeSalt        :: Text                     -- Server-wide salt for hashing
  }
  deriving (Show)

newtype HashedPassword = HashedPassword { unHashedPassword :: Text }
  deriving (Show, Eq)
```

**Smart Constructor**:
```haskell
-- Create hashed password from plaintext
mkHashedPassword :: Text -> Text -> HashedPassword  -- salt -> password -> hashed
mkHashedPassword salt password =
  HashedPassword $ hashWithSHA256 (salt <> password)
```

**Validation Rules**:
- Username: non-empty Text
- Password comparison uses constant-time algorithm
- No plaintext passwords stored

---

### 3. LoginForm

Form data submitted by user during authentication.

**Purpose**: Type-safe representation of login form submission.

```haskell
data LoginForm = LoginForm
  { formUsername  :: Text   -- User-entered username
  , formPassword  :: Text   -- User-entered password (plaintext, not stored)
  , formSessionId :: Text   -- Hidden field linking to PendingAuthorization
  , formAction    :: LoginAction  -- Submit or Deny
  }
  deriving (Show, Generic)

data LoginAction = ActionLogin | ActionDeny
  deriving (Show, Eq)
```

**FromForm Instance**:
```haskell
instance FromForm LoginForm where
  fromForm form = LoginForm
    <$> parseUnique "username" form
    <*> parseUnique "password" form
    <*> parseUnique "session_id" form
    <*> (parseAction <$> parseUnique "action" form)
  where
    parseAction "deny" = ActionDeny
    parseAction _      = ActionLogin
```

---

### 4. LoginError

Sum type for all authentication error scenarios.

**Purpose**: Type-safe error handling with user-friendly messages.

```haskell
data LoginError
  = InvalidOAuthParameters Text    -- Missing/invalid OAuth params in request
  | ClientNotRegistered Text       -- Unknown client_id
  | InvalidRedirectUri Text        -- redirect_uri doesn't match client
  | SessionNotFound Text           -- session_id not in pendingAuthorizations
  | SessionExpired                 -- Session older than timeout
  | AuthenticationFailed           -- Invalid username/password
  | CookiesRequired                -- Browser cookies disabled
  | MissingFormFields [Text]       -- Required form fields empty
  deriving (Show, Eq)

-- Error rendering
loginErrorToHttp :: LoginError -> (Int, Text, Text)  -- (status, title, message)
loginErrorToHttp = \case
  InvalidOAuthParameters detail -> (400, "Invalid Request", detail)
  ClientNotRegistered cid       -> (400, "Unknown Application", "Application '" <> cid <> "' is not registered")
  InvalidRedirectUri uri        -> (400, "Invalid Redirect", "Redirect URI not allowed for this application")
  SessionNotFound _             -> (400, "Session Not Found", "Please restart the authorization flow")
  SessionExpired                -> (400, "Session Expired", "Your session has expired. Please try again.")
  AuthenticationFailed          -> (401, "Authentication Failed", "Invalid username or password")
  CookiesRequired               -> (400, "Cookies Required", "Please enable cookies to continue")
  MissingFormFields fields      -> (400, "Missing Information", "Please fill in: " <> T.intercalate ", " fields)
```

---

### 5. LoginResult

Outcome of authentication attempt.

**Purpose**: Railway-oriented result type for login processing.

```haskell
data LoginResult
  = LoginSuccess
      { resultAuthCode    :: Text       -- Generated authorization code
      , resultRedirectUri :: Text       -- Where to redirect
      , resultState       :: Maybe Text -- Client state to preserve
      }
  | LoginDenied
      { resultRedirectUri :: Text       -- Where to redirect
      , resultState       :: Maybe Text -- Client state to preserve
      }
  | LoginFailure LoginError

-- Convenience constructor
mkLoginSuccess :: PendingAuthorization -> Text -> LoginResult
mkLoginSuccess pending code = LoginSuccess
  { resultAuthCode = code
  , resultRedirectUri = pendingRedirectUri pending
  , resultState = pendingState pending
  }
```

---

## State Extensions

### OAuthState (Modified)

Extend existing `OAuthState` with pending authorizations:

```haskell
data OAuthState = OAuthState
  { authCodes            :: Map Text AuthorizationCode
  , accessTokens         :: Map Text AuthUser
  , refreshTokens        :: Map Text (Text, AuthUser)
  , registeredClients    :: Map Text ClientInfo
  , pendingAuthorizations :: Map Text PendingAuthorization  -- NEW: session_id -> pending
  }
```

### OAuthConfig (Modified)

Extend existing `OAuthConfig` with credential store:

```haskell
data OAuthConfig = OAuthConfig
  { -- ...existing fields...
  , credentialStore           :: CredentialStore  -- NEW: user credentials
  , loginSessionExpirySeconds :: Int              -- NEW: session timeout (default 600)
  }
```

---

## Entity Relationships

```
┌─────────────────┐     validates      ┌─────────────────┐
│  LoginForm      │ ──────────────────▶│ CredentialStore │
│  (username,     │                    │ (credentials)   │
│   password,     │                    └─────────────────┘
│   session_id)   │
└────────┬────────┘
         │ references
         ▼
┌─────────────────────┐     converts to    ┌───────────────────┐
│ PendingAuthorization│ ──────────────────▶│ AuthorizationCode │
│ (client_id,         │    on success      │ (existing type)   │
│  redirect_uri,      │                    └───────────────────┘
│  code_challenge...) │
└─────────────────────┘
         │
         │ registered in
         ▼
┌─────────────────┐
│   ClientInfo    │
│ (existing type) │
└─────────────────┘
```

---

## Demo Credentials

Default credentials for immediate testing:

```haskell
defaultDemoCredentialStore :: CredentialStore
defaultDemoCredentialStore = CredentialStore
  { storeCredentials = Map.fromList
      [ ("demo", mkHashedPassword "mcp-demo-salt" "demo123")
      , ("admin", mkHashedPassword "mcp-demo-salt" "admin456")
      ]
  , storeSalt = "mcp-demo-salt"
  }
```

**Test Accounts**:
| Username | Password | Purpose |
|----------|----------|---------|
| demo | demo123 | Default test user |
| admin | admin456 | Alternative test user |
