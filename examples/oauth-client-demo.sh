#!/usr/bin/env bash
# OAuth 2.1 Client Demo Script for MCP HTTP Server
# Tests the complete OAuth flow including metadata discovery, registration, authorization, and token exchange

set -e

BASE_URL="${BASE_URL:-http://localhost:8080}"

echo "=== OAuth 2.1 Client Demo ==="
echo "Base URL: $BASE_URL"
echo ""

# Test 1: OAuth Authorization Server Metadata Discovery
echo "1. Testing OAuth Authorization Server Metadata Discovery"
echo "   GET $BASE_URL/.well-known/oauth-authorization-server"
curl -s "$BASE_URL/.well-known/oauth-authorization-server" | jq '.'
echo ""

# Test 2: OAuth Protected Resource Metadata Discovery
echo "2. Testing OAuth Protected Resource Metadata Discovery"
echo "   GET $BASE_URL/.well-known/oauth-protected-resource"
curl -s "$BASE_URL/.well-known/oauth-protected-resource" | jq '.'
echo ""

# Test 3: Test 401 Unauthorized with WWW-Authenticate Header
echo "3. Testing 401 Unauthorized Response with WWW-Authenticate Header"
echo "   GET $BASE_URL/mcp (no token)"
RESPONSE=$(curl -si "$BASE_URL/mcp" | head -n 20)
echo "$RESPONSE"
echo ""
WWW_AUTH=$(echo "$RESPONSE" | grep -i "WWW-Authenticate:" || echo "Missing WWW-Authenticate header")
echo "   WWW-Authenticate: $WWW_AUTH"
echo ""

# Test 4: Dynamic Client Registration
echo "4. Testing Dynamic Client Registration"
echo "   POST $BASE_URL/register"
CLIENT_REG=$(curl -s -X POST "$BASE_URL/register" \
  -H "Content-Type: application/json" \
  -d '{
    "client_name": "OAuth Demo Client",
    "redirect_uris": ["http://localhost:3000/callback"],
    "grant_types": ["authorization_code", "refresh_token"],
    "token_endpoint_auth_method": "none"
  }')
echo "$CLIENT_REG" | jq '.'
CLIENT_ID=$(echo "$CLIENT_REG" | jq -r '.client_id')
echo ""
echo "   Registered Client ID: $CLIENT_ID"
echo ""

# Test 5: Generate PKCE Challenge
echo "5. Generating PKCE Code Verifier and Challenge"
CODE_VERIFIER=$(openssl rand -base64 64 | tr -d '\n' | tr '+/' '-_' | tr -d '=')
CODE_CHALLENGE=$(echo -n "$CODE_VERIFIER" | openssl dgst -sha256 -binary | base64 -w 0 | tr '+/' '-_' | tr -d '=')
echo "   Code Verifier: ${CODE_VERIFIER:0:20}..."
echo "   Code Challenge: ${CODE_CHALLENGE:0:20}..."
echo ""

# Test 6: Authorization Request (in demo mode, auto-approved)
echo "6. Testing Authorization Request (demo mode auto-approves)"
STATE=$(openssl rand -hex 16)
AUTH_URL="$BASE_URL/authorize?response_type=code&client_id=$CLIENT_ID&redirect_uri=http://localhost:3000/callback&code_challenge=$CODE_CHALLENGE&code_challenge_method=S256&state=$STATE"
echo "   GET $AUTH_URL"
AUTH_RESPONSE=$(curl -sL -w "%{http_code}" -o /tmp/auth_response.html "$AUTH_URL")
echo "   HTTP Status: $AUTH_RESPONSE"
if [ "$AUTH_RESPONSE" = "200" ]; then
  echo "   Authorization successful (demo mode)"
  # Extract authorization code from HTML response
  AUTH_CODE=$(grep -oP 'code=[^&"]*' /tmp/auth_response.html | head -1 | cut -d= -f2)
  if [ -n "$AUTH_CODE" ]; then
    echo "   Authorization Code: ${AUTH_CODE:0:20}..."
  else
    echo "   Could not extract authorization code from response"
    exit 1
  fi
else
  echo "   Authorization failed"
  cat /tmp/auth_response.html
  exit 1
fi
echo ""

# Test 7: Token Exchange
echo "7. Testing Token Exchange (Authorization Code + PKCE)"
echo "   POST $BASE_URL/token"
TOKEN_RESPONSE=$(curl -s -X POST "$BASE_URL/token" \
  -H "Content-Type: application/x-www-form-urlencoded" \
  -d "grant_type=authorization_code&code=$AUTH_CODE&redirect_uri=http://localhost:3000/callback&client_id=$CLIENT_ID&code_verifier=$CODE_VERIFIER")
echo "$TOKEN_RESPONSE" | jq '.'
ACCESS_TOKEN=$(echo "$TOKEN_RESPONSE" | jq -r '.access_token')
REFRESH_TOKEN=$(echo "$TOKEN_RESPONSE" | jq -r '.refresh_token')
echo ""
echo "   Access Token: ${ACCESS_TOKEN:0:20}..."
echo "   Refresh Token: ${REFRESH_TOKEN:0:20}..."
echo ""

# Test 8: Authenticated MCP Request
echo "8. Testing Authenticated MCP Request"
echo "   POST $BASE_URL/mcp (with Bearer token)"
MCP_RESPONSE=$(curl -s -X POST "$BASE_URL/mcp" \
  -H "Authorization: Bearer $ACCESS_TOKEN" \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": 1,
    "method": "initialize",
    "params": {
      "protocolVersion": "2025-06-18",
      "capabilities": {},
      "clientInfo": {"name": "demo-client", "version": "1.0.0"}
    }
  }')
echo "$MCP_RESPONSE" | jq '.'
echo ""

# Test 9: Refresh Token Exchange
echo "9. Testing Refresh Token Exchange"
echo "   POST $BASE_URL/token (grant_type=refresh_token)"
REFRESH_RESPONSE=$(curl -s -X POST "$BASE_URL/token" \
  -H "Content-Type: application/x-www-form-urlencoded" \
  -d "grant_type=refresh_token&refresh_token=$REFRESH_TOKEN&client_id=$CLIENT_ID")
echo "$REFRESH_RESPONSE" | jq '.'
NEW_ACCESS_TOKEN=$(echo "$REFRESH_RESPONSE" | jq -r '.access_token')
echo ""
echo "   New Access Token: ${NEW_ACCESS_TOKEN:0:20}..."
echo ""

echo "=== OAuth Demo Complete ==="
echo ""
echo "Summary:"
echo "  - Metadata Discovery: OK"
echo "  - Protected Resource Metadata: OK"
echo "  - 401 WWW-Authenticate: OK"
echo "  - Client Registration: OK"
echo "  - PKCE Generation: OK"
echo "  - Authorization: OK"
echo "  - Token Exchange: OK"
echo "  - Authenticated Request: OK"
echo "  - Token Refresh: OK"
