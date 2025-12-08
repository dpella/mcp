# Research: Login Landing Page and User Authentication

**Feature**: 002-login-auth-page
**Date**: 2025-12-08
**Status**: Complete

## Research Tasks

### 1. HTML Rendering in Servant

**Decision**: Use Servant's `Raw` endpoint type with inline HTML templates

**Rationale**:
- Servant doesn't have built-in HTML templating, but supports serving raw content via `Raw` or custom content types
- For a demo application, inline HTML strings (Text) keep dependencies minimal
- The existing codebase uses `PlainText` for the authorization success message; `Html` content type follows same pattern

**Alternatives Considered**:
- **lucid/blaze-html**: Full HTML DSLs - overkill for 2-3 pages, adds dependencies
- **mustache**: Template files - adds file I/O, external dependency
- **Inline Text**: Simple string interpolation - chosen for minimal footprint

**Implementation Pattern**:
```haskell
-- Custom HTML content type
data HTML

instance Accept HTML where
  contentType _ = "text/html"

instance MimeRender HTML Text where
  mimeRender _ = LBS.fromStrict . TE.encodeUtf8

-- Endpoint type
type LoginPage = "authorize" :> QueryParams... :> Get '[HTML] Text
```

### 2. Form Handling in Servant

**Decision**: Use `FormUrlEncoded` with custom data type for login form

**Rationale**:
- Servant already supports `FormUrlEncoded` via `servant` package
- Pattern exists in codebase: token endpoint uses `ReqBody '[FormUrlEncoded] [(Text, Text)]`
- Custom form type provides type safety: `LoginForm { username :: Text, password :: Text }`

**Alternatives Considered**:
- **Raw form parsing**: Manual parsing - error-prone, no type safety
- **JSON body**: Not standard for HTML forms - breaks browser compatibility
- **Multipart**: Overkill for simple text fields

**Implementation Pattern**:
```haskell
data LoginForm = LoginForm
  { formUsername :: Text
  , formPassword :: Text
  , formSessionId :: Text  -- Links back to pending authorization
  }

instance FromForm LoginForm where
  fromForm form = LoginForm
    <$> parseUnique "username" form
    <*> parseUnique "password" form
    <*> parseUnique "session_id" form

type LoginSubmit = "login" :> ReqBody '[FormUrlEncoded] LoginForm :> Post '[HTML] Text
```

### 3. Session Management for Login Flow

**Decision**: Store pending authorization in `OAuthState` TVar with session ID

**Rationale**:
- Existing `OAuthState` already manages `authCodes`, `accessTokens`, `refreshTokens`
- Adding `pendingAuthorizations :: Map Text PendingAuth` follows same pattern
- Session ID generated at `/authorize`, stored in hidden form field, validated on POST
- Cookie stores session ID for browser persistence

**Alternatives Considered**:
- **URL-only state**: Pass all OAuth params through form - vulnerable to tampering
- **Signed tokens**: JWT for session - adds complexity, not needed for demo
- **Separate TVar**: New state variable - fragments state management

**Implementation Pattern**:
```haskell
data PendingAuth = PendingAuth
  { pendingClientId :: Text
  , pendingRedirectUri :: Text
  , pendingCodeChallenge :: Text
  , pendingScope :: Maybe Text
  , pendingState :: Maybe Text
  , pendingCreatedAt :: UTCTime
  }

data OAuthState = OAuthState
  { authCodes :: Map Text AuthorizationCode
  , accessTokens :: Map Text AuthUser
  , refreshTokens :: Map Text (Text, AuthUser)
  , registeredClients :: Map Text ClientInfo
  , pendingAuthorizations :: Map Text PendingAuth  -- NEW: session_id -> pending
  }
```

### 4. Credential Storage and Validation

**Decision**: Add `CredentialStore` to `OAuthConfig` with demo defaults

**Rationale**:
- Follows existing pattern: `OAuthConfig` already has demo-mode settings
- `CredentialStore` can be a simple `Map Username HashedPassword`
- Password hashing uses existing `cryptonite` dependency (already in cabal file)
- Demo credentials configured in `defaultDemoOAuthConfig`

**Alternatives Considered**:
- **External file**: JSON/YAML credentials - adds file I/O, config complexity
- **Environment variables**: Good for production, awkward for demo
- **Hardcoded**: Simplest but not configurable

**Implementation Pattern**:
```haskell
data CredentialStore = CredentialStore
  { credentials :: Map Text Text  -- username -> hashed password
  }

data OAuthConfig = OAuthConfig
  { ...existing fields...
  , credentialStore :: CredentialStore  -- NEW
  }

-- Constant-time comparison for password validation
validateCredential :: CredentialStore -> Text -> Text -> Bool
validateCredential store username password =
  case Map.lookup username (credentials store) of
    Nothing -> False
    Just hashedPw -> constantTimeCompare (hashPassword password) hashedPw
```

### 5. Password Hashing Strategy

**Decision**: Use SHA256 with salt prefix for demo simplicity

**Rationale**:
- `cryptonite` already in dependencies, provides `hashWith SHA256`
- For demo purposes, simple hash is sufficient (not production-grade bcrypt)
- Salt can be server-configured or per-user
- Demo credentials can be pre-hashed in config

**Alternatives Considered**:
- **bcrypt/scrypt**: Production-grade but adds dependency, slow for demo
- **Argon2**: Best practice but heavy dependency
- **Plaintext**: Simplest but violates security assumption in spec

**Implementation Pattern**:
```haskell
import Crypto.Hash (hashWith, SHA256)

hashPassword :: Text -> Text -> Text  -- salt -> password -> hash
hashPassword salt password =
  let input = TE.encodeUtf8 (salt <> password)
      hash = hashWith SHA256 input
  in T.pack $ show hash
```

### 6. Error Page Rendering

**Decision**: Unified error page template with error type parameter

**Rationale**:
- Multiple error scenarios: invalid params, expired session, auth failed, cookies disabled
- Single template function with error code and message provides consistency
- HTTP status codes: 400 for bad request, 401 for auth failure

**Implementation Pattern**:
```haskell
data LoginError
  = InvalidParameters Text
  | SessionExpired
  | AuthenticationFailed
  | CookiesRequired

renderErrorPage :: LoginError -> Text
renderErrorPage err =
  let (title, message) = errorContent err
  in [html template with title and message]
```

## Summary

All technical decisions resolved. No external research required - the implementation uses existing patterns from the codebase:

| Area | Decision | Risk |
|------|----------|------|
| HTML Rendering | Custom `HTML` content type with Text templates | Low - simple, no deps |
| Form Handling | `FormUrlEncoded` with typed `LoginForm` | Low - existing pattern |
| Session Management | Extend `OAuthState` with `pendingAuthorizations` | Low - follows TVar pattern |
| Credential Storage | Add `CredentialStore` to `OAuthConfig` | Low - config extension |
| Password Hashing | SHA256 via cryptonite (existing dep) | Low - demo-appropriate |
| Error Pages | Unified template with error type | Low - simple Text template |

**Ready for Phase 1**: Data model and contract generation.
