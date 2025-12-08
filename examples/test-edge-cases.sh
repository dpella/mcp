#!/bin/bash
# Test script for Phase 6 - Edge Cases and Error Handling

set -e

BASE_URL="http://localhost:8080"

echo "=== Testing Phase 6 Edge Cases ==="
echo ""

echo "Test 1: Invalid response_type (should return HTML error page)"
echo "--------------------------------------------------------------"
curl -s "${BASE_URL}/authorize?response_type=token&client_id=test&redirect_uri=http://localhost&code_challenge=abc&code_challenge_method=S256" | head -20
echo ""
echo ""

echo "Test 2: Invalid code_challenge_method (should return HTML error page)"
echo "-----------------------------------------------------------------------"
curl -s "${BASE_URL}/authorize?response_type=code&client_id=test&redirect_uri=http://localhost&code_challenge=abc&code_challenge_method=plain" | head -20
echo ""
echo ""

echo "Test 3: Unregistered client_id (should return HTML error page)"
echo "----------------------------------------------------------------"
curl -s "${BASE_URL}/authorize?response_type=code&client_id=invalid_client_999&redirect_uri=http://localhost&code_challenge=abc&code_challenge_method=S256" | head -20
echo ""
echo ""

echo "Test 4: Missing cookies in POST /login (should return HTML error page)"
echo "------------------------------------------------------------------------"
curl -s -X POST "${BASE_URL}/login" \
  -H "Content-Type: application/x-www-form-urlencoded" \
  -d "username=demo&password=demo123&session_id=test123&action=login" | head -20
echo ""
echo ""

echo "Test 5: Invalid session_id (should return HTML error page)"
echo "------------------------------------------------------------"
curl -s -X POST "${BASE_URL}/login" \
  -H "Content-Type: application/x-www-form-urlencoded" \
  -H "Cookie: mcp_session=invalid_session_999" \
  -d "username=demo&password=demo123&session_id=invalid_session_999&action=login" | head -20
echo ""
echo ""

echo "=== All edge case tests completed ==="
echo ""
echo "Note: To test invalid redirect_uri and session expiry, you need:"
echo "  - Invalid redirect_uri: Register a client, then use wrong redirect_uri"
echo "  - Session expiry: Wait 10 minutes or modify loginSessionExpirySeconds to 1 second"
