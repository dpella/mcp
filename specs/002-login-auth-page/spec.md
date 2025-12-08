# Feature Specification: Login Landing Page and User Authentication

**Feature Branch**: `002-login-auth-page`
**Created**: 2025-12-08
**Status**: Draft
**Input**: User description: "We need to implement a login landing page and necessary endpoints to authenticate users in the mcp-http example application instead of auto-approving them"

## Clarifications

### Session 2025-12-08

- Q: Should the login page require cookies or provide a URL-based fallback for cookie-less browsers? → A: Require cookies; display error message if cookies disabled.
- Q: Should we maintain backward compatibility with autoApproveAuth toggle? → A: No; remove auto-approve entirely. Login page replaces it. Demo mode = preconfigured demo credentials.

## User Scenarios & Testing *(mandatory)*

### User Story 1 - User Authenticates with Credentials (Priority: P1)

An MCP client initiates OAuth authorization. The user is redirected to the login page where they enter their credentials (username/password). Upon successful authentication, they are redirected back to the MCP client with an authorization code.

**Why this priority**: Core authentication is the primary value of this feature. This enables real user authentication for the MCP HTTP server.

**Independent Test**: Can be fully tested by accessing the `/authorize` endpoint, completing the login form, and verifying the redirect contains a valid authorization code that can be exchanged for tokens.

**Acceptance Scenarios**:

1. **Given** a user navigates to the authorization endpoint with valid OAuth parameters, **When** the login page loads, **Then** they see a form with username and password fields, and a submit button.

2. **Given** a user is on the login page with valid credentials, **When** they submit the form, **Then** they are redirected to the client's redirect URI with an authorization code and state parameter.

3. **Given** a user is on the login page with invalid credentials, **When** they submit the form, **Then** they see an error message indicating authentication failed, and remain on the login page with the form preserved.

4. **Given** a user is on the login page, **When** they submit the form with empty fields, **Then** they see validation errors indicating required fields.

---

### User Story 2 - User Views Login Context (Priority: P2)

Before authenticating, the user can see which application is requesting access and what permissions are being requested, allowing them to make an informed decision about granting access.

**Why this priority**: Provides security transparency and user trust, but the core authentication flow must work first.

**Independent Test**: Can be tested by navigating to authorize endpoint with different client names and scopes, verifying the login page displays this information correctly.

**Acceptance Scenarios**:

1. **Given** a registered client requests authorization, **When** the login page loads, **Then** the user sees the application name requesting access.

2. **Given** authorization request includes scope parameter, **When** the login page loads, **Then** the user sees a human-readable description of the requested permissions.

---

### User Story 3 - User Denies Authorization (Priority: P3)

A user can choose to deny the authorization request, which redirects them back to the client application with an appropriate error.

**Why this priority**: Important for user control, but less common than successful authentication flows.

**Independent Test**: Can be tested by clicking deny on the login page and verifying the redirect includes an error response.

**Acceptance Scenarios**:

1. **Given** a user is on the login page, **When** they click a "Deny" or "Cancel" button, **Then** they are redirected to the client's redirect URI with an `access_denied` error code.

2. **Given** a user denies authorization, **When** the redirect occurs, **Then** the state parameter is preserved in the redirect.

---

### Edge Cases

- What happens when the authorization request has expired or invalid parameters? The system displays an error page explaining the issue rather than showing a login form.
- What happens when the session expires mid-authentication? The user is redirected back to the login page with their OAuth parameters preserved.
- What happens when the user's browser does not support cookies? The system displays an error message indicating cookies are required for authentication.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST display a login form when users access the `/authorize` endpoint
- **FR-002**: System MUST validate user credentials against a configured credential store
- **FR-003**: System MUST display the requesting application's name on the login page
- **FR-004**: System MUST display requested permissions/scopes in human-readable format
- **FR-005**: System MUST return `access_denied` error when users click deny
- **FR-006**: System MUST preserve OAuth state parameter through the authentication flow
- **FR-007**: System MUST return appropriate error responses for invalid or expired authorization requests
- **FR-008**: System MUST serve the login page as HTML (not JSON) with appropriate content-type headers
- **FR-009**: System MUST handle form submissions via POST request to a login processing endpoint
- **FR-010**: System MUST display clear, user-friendly error messages for authentication failures

### Key Entities

- **User Credentials**: Username and password pair used for authentication. Credentials are validated against a configurable credential store (ships with preconfigured demo credentials for out-of-the-box testing).
- **Login Session**: Temporary session tracking OAuth authorization parameters (client_id, redirect_uri, code_challenge, scope, state) through the login flow.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Users can complete the login process (from page load to successful redirect) in under 30 seconds when entering valid credentials
- **SC-002**: 100% of valid authorization requests result in either a successful authentication redirect or a visible login form
- **SC-003**: 100% of invalid credentials result in a visible error message within 2 seconds
- **SC-004**: 100% of "deny" actions result in proper OAuth error redirect with preserved state
- **SC-005**: Login page loads and is interactive within 1 second on standard network connections

## Assumptions

1. **Credential Storage**: User credentials are configured in-memory via server configuration. The example ships with preconfigured demo credentials for immediate testing. Production deployments would extend this with their own credential stores.

2. **Password Handling**: Passwords are compared securely using constant-time comparison. Preconfigured demo credentials use password hashes.

3. **Session Management**: Login sessions use the existing in-memory TVar-based state management consistent with the current OAuth state storage pattern.

4. **HTML Rendering**: The login page HTML will be embedded in the Haskell code as a template string, keeping dependencies minimal and consistent with the example application nature.
