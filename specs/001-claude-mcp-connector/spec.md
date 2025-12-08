# Feature Specification: Claude.ai MCP Connector Compatibility

**Feature Branch**: `001-claude-mcp-connector`
**Created**: 2025-12-08
**Status**: Draft
**Input**: User description: "Extend mcp-http to work as MCP Connector in Claude.ai with proper OAuth 2.1 Protected Resource Metadata, WWW-Authenticate headers, and Dynamic Client Registration"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Add MCP Server as Claude Connector (Priority: P1)

A user wants to add their self-hosted MCP server as a "Connector" in Claude.ai. They have the mcp-http example running behind a reverse proxy with TLS termination and a public IP address.

**Why this priority**: This is the core deliverable - making the mcp-http server compatible with Claude.ai's MCP connector feature. Without this, the feature provides no value.

**Independent Test**: Can be fully tested by:
1. Starting mcp-http with --oauth flag
2. Pointing a reverse proxy (e.g., nginx with TLS) to the server
3. Adding the server URL as a MCP connector in Claude.ai settings
4. Verifying Claude.ai successfully completes OAuth flow and connects

**Acceptance Scenarios**:

1. **Given** an mcp-http server running with OAuth enabled, **When** a user adds the server URL as a connector in Claude.ai, **Then** Claude.ai discovers the protected resource metadata and authorization server automatically.

2. **Given** a valid OAuth flow completion, **When** Claude.ai makes MCP requests with the access token, **Then** the server authenticates requests and returns valid MCP responses.

3. **Given** no access token provided, **When** Claude.ai makes an MCP request, **Then** the server returns HTTP 401 with a WWW-Authenticate header pointing to the protected resource metadata URL.

---

### User Story 2 - Dynamic Client Registration (Priority: P1)

Claude.ai needs to dynamically register as an OAuth client without pre-configured credentials when connecting to a new MCP server for the first time.

**Why this priority**: Essential for Claude.ai compatibility - MCP clients must be able to register dynamically since they cannot pre-register with every possible MCP server.

**Independent Test**: Can be tested by making a POST request to /register endpoint with client metadata and verifying a client_id is returned.

**Acceptance Scenarios**:

1. **Given** a new MCP client (Claude.ai), **When** it discovers the registration endpoint from authorization server metadata, **Then** it can POST client metadata and receive a client_id.

2. **Given** a registered client_id, **When** the client initiates OAuth authorization, **Then** the server accepts the client_id and proceeds with the flow.

---

### User Story 3 - OAuth Authorization Flow with PKCE (Priority: P1)

Claude.ai completes the OAuth 2.1 authorization code flow with PKCE to obtain access tokens for the MCP server.

**Why this priority**: Required security measure - MCP specification mandates PKCE for all OAuth clients to prevent authorization code interception.

**Independent Test**: Can be tested end-to-end using the oauth-client-demo.sh script or manual curl commands with PKCE challenge/verifier.

**Acceptance Scenarios**:

1. **Given** a registered client with PKCE code_challenge, **When** it requests authorization, **Then** the server accepts the challenge and returns an authorization code.

2. **Given** an authorization code and valid code_verifier, **When** the client exchanges at /token endpoint, **Then** it receives access_token and refresh_token.

3. **Given** an invalid code_verifier, **When** the client attempts token exchange, **Then** the server rejects with "invalid_grant" error.

---

### User Story 4 - Token Refresh (Priority: P2)

Claude.ai refreshes expired access tokens without requiring user re-authorization.

**Why this priority**: Important for continuous operation but not required for initial connection - sessions work with valid tokens.

**Independent Test**: Can be tested by exchanging a refresh_token at /token endpoint and verifying a new access_token is issued.

**Acceptance Scenarios**:

1. **Given** a valid refresh_token, **When** the client requests grant_type=refresh_token, **Then** the server issues a new access_token.

---

### Edge Cases

- What happens when the authorization code expires (after 10 minutes)?
  - Server returns "invalid_grant" error with description "Authorization code expired"

- How does system handle invalid/malformed Bearer tokens?
  - Server returns HTTP 401 with WWW-Authenticate header

- What happens when redirect_uri doesn't match registered URIs?
  - Server rejects authorization request with "invalid_request" error

- What happens when code_challenge_method is not "S256"?
  - Server rejects with "invalid_request" - only S256 is supported per MCP spec

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: Server MUST expose `/.well-known/oauth-protected-resource` endpoint returning Protected Resource Metadata (RFC9728) with `authorization_servers` field

- **FR-002**: Server MUST include `WWW-Authenticate` header in all HTTP 401 responses with `resource_metadata` parameter pointing to protected resource metadata URL

- **FR-003**: Server MUST expose `/.well-known/oauth-authorization-server` endpoint returning Authorization Server Metadata (RFC8414)

- **FR-004**: Server MUST support Dynamic Client Registration (RFC7591) at `/register` endpoint

- **FR-005**: Server MUST require PKCE with S256 method for all authorization requests

- **FR-006**: Server MUST validate Bearer tokens on all /mcp requests when OAuth is enabled

- **FR-007**: Server MUST accept `resource` parameter in authorization and token requests (RFC8707)

- **FR-008**: Server MUST support `authorization_code` and `refresh_token` grant types

- **FR-009**: Library MUST allow users to configure Protected Resource Metadata per resource via HTTPServerConfig; mcp-http example exposes this via `--base-url` CLI parameter

- **FR-010**: Server MUST run on HTTP (TLS termination handled by reverse proxy) by default

- **FR-011**: Library MUST provide a custom Auth combinator (e.g., `AuthProtect "mcp"`) that wraps protected endpoints and automatically returns 401 with WWW-Authenticate header on authentication failure; app code should not manually handle 401 responses

- **FR-012**: Servant-specific OAuth components (Auth combinator, Protected Resource types) MUST be structured for future extraction as a reusable servant-auth-mcp package

### Key Entities

- **ProtectedResourceAuth Combinator**: Custom Servant auth combinator that wraps protected endpoints; automatically handles 401 responses with WWW-Authenticate header; configurable with Protected Resource Metadata URL
- **Protected Resource Metadata**: Describes the MCP server as an OAuth protected resource, includes `authorization_servers` list
- **Authorization Server Metadata**: Describes OAuth endpoints, supported scopes, grant types, and PKCE methods
- **Client Registration**: Dynamic registration allowing clients to obtain client_id without pre-registration
- **Authorization Code**: Time-limited code bound to PKCE challenge, client_id, and redirect_uri
- **Access Token**: JWT bearer token for authenticating MCP requests
- **Refresh Token**: Long-lived token for obtaining new access tokens

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Users can add the mcp-http server as a MCP connector in Claude.ai and complete the OAuth flow successfully

- **SC-002**: Claude.ai can discover both protected resource metadata and authorization server metadata automatically via well-known endpoints

- **SC-003**: All unauthenticated requests to /mcp receive HTTP 401 with properly formatted WWW-Authenticate header

- **SC-004**: Dynamic client registration completes within 1 second and returns valid client_id

- **SC-005**: The complete OAuth flow (registration + authorization + token exchange) completes successfully when tested with Claude.ai

## Clarifications

### Session 2025-12-08

- Q: Where should the new OAuth Protected Resource functionality live? → A: Library with configurable metadata - types in MCP.Server.Auth, logic in MCP.Server.HTTP, users can specify protected resource metadata per resource
- Q: How should WWW-Authenticate header be integrated with servant-auth? → A: Custom Auth combinator at framework level that wraps protected endpoints; the combinator automatically returns 401 with WWW-Authenticate header on auth failure, so apps don't handle this manually; structured for future extraction as servant-auth-mcp package
- Q: Should library support external/separate authorization servers? → A: Same-origin only for MVP; clean simple design prioritized over abstraction; foundation for later extraction

## Assumptions

- TLS termination is handled by a reverse proxy (nginx, Caddy, etc.) in front of mcp-http
- The reverse proxy forwards the original host and protocol via X-Forwarded-* headers or the base URL is configured explicitly
- Claude.ai implements the MCP authorization specification (RFC9728, RFC8414, RFC7591)
- Hardcoded demo credentials are acceptable for the example application
- Authorization server is same-origin as MCP resource server (MVP scope)
- Implementation prioritizes clarity and simplicity over premature abstraction
