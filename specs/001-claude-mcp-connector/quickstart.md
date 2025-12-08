# Quickstart: Claude.ai MCP Connector

**Feature**: 001-claude-mcp-connector
**Date**: 2025-12-08

## Prerequisites

- GHC 9.4+ with cabal
- A public domain with TLS (e.g., via nginx, Caddy, Cloudflare Tunnel)
- Claude.ai account with connector feature access

## 1. Build the Server

```bash
cd /home/claude/mcp
cabal build mcp-http
```

## 2. Run with OAuth Enabled

```bash
# Replace with your public URL (what Claude.ai will connect to)
cabal run mcp-http -- --oauth --base-url https://your-domain.com
```

The server starts on port 8080 by default. Use `--port` to change.

## 3. Configure Reverse Proxy

Example nginx configuration:

```nginx
server {
    listen 443 ssl;
    server_name your-domain.com;

    ssl_certificate /path/to/cert.pem;
    ssl_certificate_key /path/to/key.pem;

    location / {
        proxy_pass http://127.0.0.1:8080;
        proxy_set_header Host $host;
        proxy_set_header X-Forwarded-Proto $scheme;
    }
}
```

## 4. Verify Endpoints

Test the discovery endpoints:

```bash
# Protected Resource Metadata (RFC 9728)
curl https://your-domain.com/.well-known/oauth-protected-resource

# Authorization Server Metadata (RFC 8414)
curl https://your-domain.com/.well-known/oauth-authorization-server

# Should return 401 with WWW-Authenticate header
curl -i https://your-domain.com/mcp \
  -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","id":1,"method":"ping"}'
```

Expected 401 response headers:
```
HTTP/1.1 401 Unauthorized
WWW-Authenticate: Bearer resource_metadata="https://your-domain.com/.well-known/oauth-protected-resource"
```

## 5. Add to Claude.ai

1. Go to Claude.ai Settings → Connectors
2. Click "Add Connector"
3. Enter your server URL: `https://your-domain.com`
4. Claude.ai will:
   - Discover protected resource metadata
   - Register as OAuth client dynamically
   - Redirect you to authorize
   - Exchange code for tokens
5. Once authorized, the MCP server tools become available

## OAuth Flow Summary

```
Claude.ai                          Your MCP Server
    │
    │ GET /.well-known/oauth-protected-resource
    │──────────────────────────────────────────►
    │◄──────────────────────────────────────────
    │     { authorization_servers: [...] }
    │
    │ GET /.well-known/oauth-authorization-server
    │──────────────────────────────────────────►
    │◄──────────────────────────────────────────
    │     { registration_endpoint, authorize_endpoint, ... }
    │
    │ POST /register (Dynamic Client Registration)
    │──────────────────────────────────────────►
    │◄──────────────────────────────────────────
    │     { client_id: "..." }
    │
    │ GET /authorize?client_id=...&code_challenge=...
    │──────────────────────────────────────────►
    │     (User approves in browser)
    │◄──────────────────────────────────────────
    │     Redirect with authorization code
    │
    │ POST /token (code + code_verifier)
    │──────────────────────────────────────────►
    │◄──────────────────────────────────────────
    │     { access_token, refresh_token }
    │
    │ POST /mcp (with Bearer token)
    │──────────────────────────────────────────►
    │◄──────────────────────────────────────────
    │     MCP responses
```

## Troubleshooting

### "WWW-Authenticate header missing"

- Ensure OAuth is enabled: `--oauth` flag
- Verify the base URL matches your public domain

### "Discovery failed"

- Check that `/.well-known/oauth-protected-resource` is accessible
- Verify TLS certificate is valid
- Check `authorization_servers` list in response

### "Invalid client"

- Dynamic client registration may have failed
- Check server logs for registration errors

### "PKCE validation failed"

- Ensure client is using S256 code challenge method
- Verify code_verifier matches the original challenge

## Development Testing

For local testing without TLS:

```bash
# Terminal 1: Start server
cabal run mcp-http -- --oauth --base-url http://localhost:8080 --log

# Terminal 2: Test flow
./examples/oauth-client-demo.sh
```

Note: Claude.ai requires HTTPS, so local testing is for development only.
