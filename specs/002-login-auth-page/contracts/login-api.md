# API Contract: Login Endpoints

**Feature**: 002-login-auth-page
**Date**: 2025-12-08

## Overview

This document defines the HTTP endpoints for the login authentication flow. These endpoints integrate with the existing OAuth 2.1 flow in `MCP.Server.HTTP`.

---

## Endpoints

### 1. GET /authorize (Modified)

**Change from current behavior**: Instead of auto-generating an auth code, renders a login page.

**Request**:
```http
GET /authorize?response_type=code&client_id=abc&redirect_uri=https://...&code_challenge=xyz&code_challenge_method=S256&scope=mcp:read&state=123 HTTP/1.1
Host: localhost:8080
```

**Query Parameters**:
| Parameter | Required | Description |
|-----------|----------|-------------|
| response_type | Yes | Must be "code" |
| client_id | Yes | Registered OAuth client ID |
| redirect_uri | Yes | Must match registered URI for client |
| code_challenge | Yes | PKCE challenge (base64url) |
| code_challenge_method | Yes | Must be "S256" |
| scope | No | Space-separated scopes |
| state | No | Opaque value for CSRF protection |
| resource | No | RFC8707 resource indicator |

**Success Response** (200 OK):
```http
HTTP/1.1 200 OK
Content-Type: text/html; charset=utf-8
Set-Cookie: mcp_session=<session_id>; HttpOnly; SameSite=Strict; Path=/

<!DOCTYPE html>
<html>
<head><title>Sign In - MCP Server</title></head>
<body>
  <h1>Sign In</h1>
  <p>Application "<strong>{client_name}</strong>" is requesting access.</p>
  <p>Permissions requested: {scopes}</p>

  <form method="POST" action="/login">
    <input type="hidden" name="session_id" value="{session_id}">
    <label>Username: <input type="text" name="username" required></label>
    <label>Password: <input type="password" name="password" required></label>
    <button type="submit" name="action" value="login">Sign In</button>
    <button type="submit" name="action" value="deny">Deny</button>
  </form>
</body>
</html>
```

**Error Responses**:

| Status | Condition | Body |
|--------|-----------|------|
| 400 | Missing required parameter | HTML error page with details |
| 400 | Invalid response_type | HTML error page |
| 400 | Invalid code_challenge_method | HTML error page |
| 400 | Unregistered client_id | HTML error page |
| 400 | Invalid redirect_uri | HTML error page |

**Error Page Format**:
```html
<!DOCTYPE html>
<html>
<head><title>Error - MCP Server</title></head>
<body>
  <h1>{error_title}</h1>
  <p>{error_message}</p>
  <p>Please contact the application developer.</p>
</body>
</html>
```

---

### 2. POST /login (New)

**Purpose**: Process login form submission.

**Request**:
```http
POST /login HTTP/1.1
Host: localhost:8080
Content-Type: application/x-www-form-urlencoded
Cookie: mcp_session=<session_id>

username=demo&password=demo123&session_id=abc123&action=login
```

**Form Fields**:
| Field | Required | Description |
|-------|----------|-------------|
| username | Yes | User's username |
| password | Yes | User's password |
| session_id | Yes | Hidden field from login page |
| action | Yes | "login" or "deny" |

**Success Response - Authentication** (302 Found):
```http
HTTP/1.1 302 Found
Location: {redirect_uri}?code={auth_code}&state={state}
Set-Cookie: mcp_session=; Max-Age=0; Path=/
```

**Success Response - Denied** (302 Found):
```http
HTTP/1.1 302 Found
Location: {redirect_uri}?error=access_denied&error_description=User%20denied%20access&state={state}
Set-Cookie: mcp_session=; Max-Age=0; Path=/
```

**Error Responses**:

| Status | Condition | Response |
|--------|-----------|----------|
| 400 | Missing form fields | HTML error page |
| 400 | Invalid/expired session_id | HTML error page |
| 401 | Invalid credentials | HTML login page with error message |

**Authentication Failed Response** (200 OK with error):
```html
<!DOCTYPE html>
<html>
<head><title>Sign In - MCP Server</title></head>
<body>
  <h1>Sign In</h1>
  <p class="error">Invalid username or password. Please try again.</p>

  <form method="POST" action="/login">
    <input type="hidden" name="session_id" value="{session_id}">
    <label>Username: <input type="text" name="username" value="{previous_username}" required></label>
    <label>Password: <input type="password" name="password" required></label>
    <button type="submit" name="action" value="login">Sign In</button>
    <button type="submit" name="action" value="deny">Deny</button>
  </form>
</body>
</html>
```

---

## Servant API Type Definition

```haskell
-- Content type for HTML responses
data HTML

instance Accept HTML where
  contentType _ = "text/html" /: ("charset", "utf-8")

instance MimeRender HTML Text where
  mimeRender _ = LBS.fromStrict . TE.encodeUtf8

-- Login form data
data LoginForm = LoginForm
  { formUsername  :: Text
  , formPassword  :: Text
  , formSessionId :: Text
  , formAction    :: Text
  }

instance FromForm LoginForm

-- API definition
type LoginAPI =
  -- GET /authorize - render login page
  "authorize"
    :> QueryParam' '[Required] "response_type" Text
    :> QueryParam' '[Required] "client_id" Text
    :> QueryParam' '[Required] "redirect_uri" Text
    :> QueryParam' '[Required] "code_challenge" Text
    :> QueryParam' '[Required] "code_challenge_method" Text
    :> QueryParam "scope" Text
    :> QueryParam "state" Text
    :> QueryParam "resource" Text
    :> Get '[HTML] (Headers '[Header "Set-Cookie" Text] Text)

  -- POST /login - process login form
  :<|> "login"
    :> Header "Cookie" Text
    :> ReqBody '[FormUrlEncoded] LoginForm
    :> Post '[HTML] (Headers '[Header "Location" Text, Header "Set-Cookie" Text] NoContent)
```

---

## Integration with Existing OAuth Flow

The login endpoints integrate with the existing OAuth infrastructure:

1. **GET /authorize**: Creates `PendingAuthorization` in `OAuthState.pendingAuthorizations`
2. **POST /login**:
   - Validates credentials against `OAuthConfig.credentialStore`
   - On success: Creates `AuthorizationCode` in `OAuthState.authCodes` (existing)
   - Removes `PendingAuthorization`
3. **POST /token**: Unchanged - exchanges auth code for tokens (existing)

```
Client                    MCP Server                    User
   |                           |                          |
   |-- GET /authorize -------->|                          |
   |                           |-- Render login page ---->|
   |                           |                          |
   |                           |<-- POST /login ----------|
   |                           |    (username, password)  |
   |                           |                          |
   |<-- 302 redirect w/code ---|                          |
   |                           |                          |
   |-- POST /token ----------->|                          |
   |<-- access_token ----------|                          |
```

---

## Session Cookie Specification

| Attribute | Value | Rationale |
|-----------|-------|-----------|
| Name | mcp_session | Clear purpose |
| HttpOnly | Yes | Prevent XSS access |
| SameSite | Strict | CSRF protection |
| Secure | No (demo) | Would be Yes in production |
| Path | / | Available to all endpoints |
| Max-Age | 600 | 10 minute expiry |

---

## Error Codes

| Code | OAuth Error | HTTP Status | When |
|------|-------------|-------------|------|
| invalid_request | Yes | 400 | Missing/invalid parameters |
| unauthorized_client | Yes | 400 | Unregistered client |
| access_denied | Yes | 302 | User clicks Deny |
| server_error | Yes | 500 | Unexpected server error |
| - | No (HTML) | 401 | Invalid credentials |
