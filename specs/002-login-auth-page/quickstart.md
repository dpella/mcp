# Quickstart: Login Landing Page and User Authentication

**Feature**: 002-login-auth-page
**Date**: 2025-12-08
**Epic**: `mcp-9k8` (view with `bd show mcp-9k8`)

## Issue Tracking with Beads

All tasks for this feature are tracked using `bd` (beads). **Do NOT use markdown TODOs or task lists.**

### Quick Reference

```bash
# View epic and all child tasks
bd show mcp-9k8

# Check ready work (unblocked tasks)
bd ready --json

# Start working on a phase
bd update <phase-id> --status in_progress

# Complete a phase
bd close <phase-id> --reason "Done"
```

### Phase Beads (Execution Order)

| Phase | Bead ID | Title | Priority |
|-------|---------|-------|----------|
| 1 | `mcp-5el` | Setup - Review existing OAuth flow | P2 |
| 2 | `mcp-6lv` | Foundational Types and Pure Functions | P1 |
| 3 | `mcp-1q9` | User Story 1 - Authenticate (MVP) | P1 |
| 4 | `mcp-nbp` | User Story 2 - Login Context | P2 |
| 5 | `mcp-226` | User Story 3 - Deny Authorization | P3 |
| 6 | `mcp-qne` | Edge Cases and Error Handling | P2 |
| 7 | `mcp-7ef` | Polish and Validation | P3 |

### Workflow

1. **Start phase**: `bd update mcp-5el --status in_progress`
2. **Work on tasks**: Implement T001, T002 from tasks.md
3. **Complete phase**: `bd close mcp-5el --reason "Setup complete"`
4. **Move to next**: `bd update mcp-6lv --status in_progress`
5. **Commit together**: Always commit `.beads/issues.jsonl` with code

## Prerequisites

- GHC 9.4+ with cabal
- Project builds successfully (`cabal build`)
- Understanding of existing OAuth flow in `MCP.Server.HTTP`

## Quick Test After Implementation

### 1. Start the Server

```bash
cabal run mcp-http -- --oauth --port 8080
```

### 2. Test Login Flow

**Register a client** (existing functionality):
```bash
curl -X POST http://localhost:8080/register \
  -H "Content-Type: application/json" \
  -d '{
    "client_name": "Test App",
    "redirect_uris": ["http://localhost:3000/callback"],
    "grant_types": ["authorization_code"],
    "response_types": ["code"],
    "token_endpoint_auth_method": "none"
  }'
# Note the client_id from response
```

**Generate PKCE challenge**:
```bash
# Generate code_verifier (random string)
CODE_VERIFIER=$(openssl rand -base64 32 | tr -d '=/+' | cut -c1-43)

# Generate code_challenge (SHA256 hash, base64url encoded)
CODE_CHALLENGE=$(echo -n "$CODE_VERIFIER" | openssl dgst -sha256 -binary | openssl base64 | tr '/+' '_-' | tr -d '=')

echo "Verifier: $CODE_VERIFIER"
echo "Challenge: $CODE_CHALLENGE"
```

**Open login page in browser**:
```
http://localhost:8080/authorize?response_type=code&client_id=<CLIENT_ID>&redirect_uri=http://localhost:3000/callback&code_challenge=<CODE_CHALLENGE>&code_challenge_method=S256&scope=mcp:read&state=test123
```

**Login with demo credentials**:
- Username: `demo`
- Password: `demo123`

**Observe redirect** to:
```
http://localhost:3000/callback?code=<AUTH_CODE>&state=test123
```

### 3. Complete Token Exchange

```bash
curl -X POST http://localhost:8080/token \
  -H "Content-Type: application/x-www-form-urlencoded" \
  -d "grant_type=authorization_code&code=<AUTH_CODE>&code_verifier=<CODE_VERIFIER>&redirect_uri=http://localhost:3000/callback"
```

---

## Demo Credentials

| Username | Password | Notes |
|----------|----------|-------|
| demo | demo123 | Default test user |
| admin | admin456 | Alternative test user |

---

## Key Files to Modify

### Primary Changes

1. **`src/MCP/Server/HTTP.hs`**:
   - Add `HTML` content type
   - Add `LoginForm` data type with `FromForm` instance
   - Add `PendingAuthorization` data type
   - Extend `OAuthState` with `pendingAuthorizations`
   - Modify `handleAuthorize` to render login page
   - Add `handleLogin` for form processing
   - Add HTML template functions

2. **`src/MCP/Server/Auth.hs`**:
   - Add `CredentialStore` data type
   - Add `validateCredential` function
   - Extend `OAuthConfig` with credential store settings

3. **`examples/http-server.hs`**:
   - Configure demo credentials in `defaultDemoOAuthConfig`

### Test Files

4. **`test/Main.hs`**:
   - Add tests for credential validation
   - Add tests for session management
   - Add integration tests for login flow

---

## Implementation Order

Follow the phase beads in order. Each phase has detailed tasks in [tasks.md](./tasks.md).

1. **Phase 1 (`mcp-5el`)**: Setup - Review existing code
2. **Phase 2 (`mcp-6lv`)**: Foundational Types (Constitution Principle I) and Pure Functions (Principle V)
3. **Phase 3 (`mcp-1q9`)**: User Story 1 - Core authentication (MVP)
4. **Phase 4 (`mcp-nbp`)**: User Story 2 - Login context display
5. **Phase 5 (`mcp-226`)**: User Story 3 - Deny flow
6. **Phase 6 (`mcp-qne`)**: Edge cases and error handling
7. **Phase 7 (`mcp-7ef`)**: Polish and validation

### MVP Checkpoint

After completing Phase 3 (`mcp-1q9`), the core login flow works:
- `bd close mcp-1q9 --reason "MVP complete - login flow works"`
- Test with demo/demo123 credentials
- Deploy/demo if ready

---

## Acceptance Test Script

Save as `test-login-flow.sh`:

```bash
#!/bin/bash
set -e

BASE_URL="http://localhost:8080"

echo "=== Testing Login Flow ==="

# 1. Register client
echo "Registering client..."
CLIENT_RESP=$(curl -s -X POST "$BASE_URL/register" \
  -H "Content-Type: application/json" \
  -d '{
    "client_name": "Login Test",
    "redirect_uris": ["http://localhost:3000/callback"],
    "grant_types": ["authorization_code"],
    "response_types": ["code"],
    "token_endpoint_auth_method": "none"
  }')
CLIENT_ID=$(echo "$CLIENT_RESP" | jq -r '.client_id')
echo "Client ID: $CLIENT_ID"

# 2. Generate PKCE
CODE_VERIFIER=$(openssl rand -base64 32 | tr -d '=/+' | cut -c1-43)
CODE_CHALLENGE=$(echo -n "$CODE_VERIFIER" | openssl dgst -sha256 -binary | openssl base64 | tr '/+' '_-' | tr -d '=')

# 3. Get login page
echo "Fetching login page..."
LOGIN_RESP=$(curl -s -c cookies.txt -w "%{http_code}" \
  "$BASE_URL/authorize?response_type=code&client_id=$CLIENT_ID&redirect_uri=http://localhost:3000/callback&code_challenge=$CODE_CHALLENGE&code_challenge_method=S256&state=test")
HTTP_CODE="${LOGIN_RESP: -3}"
[ "$HTTP_CODE" = "200" ] && echo "✓ Login page returned 200" || echo "✗ Expected 200, got $HTTP_CODE"

# 4. Extract session_id from page
SESSION_ID=$(echo "$LOGIN_RESP" | grep -o 'name="session_id" value="[^"]*"' | cut -d'"' -f4)
echo "Session ID: $SESSION_ID"

# 5. Submit login form
echo "Submitting credentials..."
LOGIN_RESULT=$(curl -s -b cookies.txt -c cookies.txt -w "%{http_code}" -o /dev/null \
  -X POST "$BASE_URL/login" \
  -H "Content-Type: application/x-www-form-urlencoded" \
  -d "username=demo&password=demo123&session_id=$SESSION_ID&action=login")
[ "$LOGIN_RESULT" = "302" ] && echo "✓ Login redirected (302)" || echo "✗ Expected 302, got $LOGIN_RESULT"

# 6. Test invalid credentials
echo "Testing invalid credentials..."
INVALID_RESULT=$(curl -s -w "%{http_code}" -o /dev/null \
  -X POST "$BASE_URL/login" \
  -H "Content-Type: application/x-www-form-urlencoded" \
  -d "username=demo&password=wrongpassword&session_id=$SESSION_ID&action=login")
[ "$INVALID_RESULT" = "200" ] && echo "✓ Invalid creds returned 200 (error page)" || echo "✗ Expected 200, got $INVALID_RESULT"

# Cleanup
rm -f cookies.txt
echo "=== Tests Complete ==="
```

---

## Common Issues

| Issue | Cause | Solution |
|-------|-------|----------|
| 400 on /authorize | Missing query params | Check all required params present |
| Session not found | Cookie not sent | Enable cookies in browser/curl |
| Invalid credentials | Wrong password | Use demo/demo123 |
| Redirect fails | Unregistered redirect_uri | Register client with correct URIs |
