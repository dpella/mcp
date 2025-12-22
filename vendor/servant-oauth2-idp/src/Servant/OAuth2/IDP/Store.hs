{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Servant.OAuth2.IDP.Store
Description : OAuth state storage typeclass with algebraic laws
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com

This module defines the 'OAuthStateStore' typeclass for managing OAuth 2.1 server state,
including authorization codes, access tokens, refresh tokens, registered clients, and
pending authorization sessions.

= Algebraic Laws

All storage operations must satisfy these fundamental laws:

== Round-trip Law

Storing and immediately retrieving a value (when not expired) returns the stored value:

@
storeAuthCode code >> lookupAuthCode (authCodeId code) == Just code  -- when not expired
storeClient clientId info >> lookupClient clientId == Just info
storePendingAuth sessionId auth >> lookupPendingAuth sessionId == Just auth  -- when not expired
@

== Delete Law

Deleting a value makes subsequent lookups return Nothing:

@
deleteAuthCode authCodeId >> lookupAuthCode authCodeId == Nothing
deletePendingAuth sessionId >> lookupPendingAuth sessionId == Nothing
@

== Idempotence Law

Storing the same value multiple times is equivalent to storing it once:

@
storeAuthCode code >> storeAuthCode code == storeAuthCode code
storeClient clientId info >> storeClient clientId info == storeClient clientId info
@

== Overwrite Law

Storing a new value with the same identifier overwrites the previous value:

@
storeClient clientId info1 >> storeClient clientId info2 == storeClient clientId info2
updateRefreshToken tokenId (client1, user1) >> updateRefreshToken tokenId (client2, user2)
    == updateRefreshToken tokenId (client2, user2)
@

== Expiry Law

Lookups for expired values return Nothing:

@
lookupAuthCode authCodeId == Nothing  -- when currentTime > authCodeExpiry
lookupPendingAuth sessionId == Nothing  -- when currentTime > sessionExpiry
@

Note: The expiry law applies to 'AuthorizationCode' and 'PendingAuthorization' which
have expiry timestamps. Implementations must check expiry during lookup.

= Usage

@
-- Example: Authorization code flow
storeAuthCode code
maybeCode <- lookupAuthCode (authCodeId code)
case maybeCode of
    Just code -> do
        -- Validate and exchange for token
        deleteAuthCode (authCodeId code)  -- Single use
    Nothing -> -- Code expired or invalid
@

@
-- Example: Associated types
instance OAuthStateStore MyMonad where
    type OAuthStateError MyMonad = Text
    type OAuthStateEnv MyMonad = PostgresqlEnv
@
-}
module Servant.OAuth2.IDP.Store (
    -- * Typeclass
    OAuthStateStore (..),

    -- * Re-exports
    MonadTime (..),
    AuthCodeId,
    ClientId,
    SessionId,
    AccessTokenId,
    RefreshTokenId,
    AuthorizationCode,
    ClientInfo,
    PendingAuthorization,
) where

import Control.Monad.Time (MonadTime (..))
import Data.Kind (Type)
import Servant.OAuth2.IDP.Types (
    AccessTokenId,
    AuthCodeId,
    AuthorizationCode,
    ClientId,
    ClientInfo,
    PendingAuthorization,
    RefreshTokenId,
    SessionId,
 )

{- | Storage interface for OAuth 2.1 server state.

This typeclass abstracts over different storage backends (in-memory, PostgreSQL, Redis)
for OAuth authorization codes, tokens, clients, and sessions.

The typeclass uses associated types for error handling and environment configuration,
allowing different backends to provide their own error types and configuration.

All implementations must satisfy the algebraic laws documented in the module header.
-}
class (Monad m, MonadTime m) => OAuthStateStore m where
    {- | Error type for storage operations.

    Different backends may have different failure modes:
    * In-memory: @type OAuthStateError InMemory = Void@ (cannot fail)
    * PostgreSQL: @type OAuthStateError Postgres = SqlError@
    * Redis: @type OAuthStateError Redis = RedisError@
    -}
    type OAuthStateError m :: Type

    {- | Environment/configuration type for the storage backend.

    Examples:
    * In-memory: @type OAuthStateEnv InMemory = TVar OAuthState@
    * PostgreSQL: @type OAuthStateEnv Postgres = ConnectionPool@
    * Redis: @type OAuthStateEnv Redis = RedisConnection@
    -}
    type OAuthStateEnv m :: Type

    {- | User type for JWT tokens and authentication.

    This is the full user record stored with tokens and authorization codes.

    Examples:
    * Simple: @type OAuthUser MyMonad = AuthUser@
    * Custom: @type OAuthUser MyMonad = MyCustomUser@
    -}
    type OAuthUser m :: Type

    -- * Authorization Code Operations

    --
    -- Authorization codes are single-use tokens issued during the OAuth authorization
    -- flow. They are exchanged for access tokens and must be deleted after use.

    {- | Store an authorization code.

    The implementation should store the code along with its expiry time, PKCE challenge,
    client ID, redirect URI, and associated user.

    Satisfies: Round-trip law, Idempotence law, Overwrite law
    -}
    storeAuthCode :: AuthorizationCode (OAuthUser m) -> m ()

    {- | Look up an authorization code by its identifier.

    Returns 'Nothing' if:
    * The code does not exist
    * The code has expired (implementation must check expiry time)

    Satisfies: Round-trip law, Expiry law
    -}
    lookupAuthCode :: AuthCodeId -> m (Maybe (AuthorizationCode (OAuthUser m)))

    {- | Delete an authorization code.

    Used after exchanging the code for tokens (single-use enforcement).
    Idempotent: deleting a non-existent code is not an error.

    Satisfies: Delete law
    -}
    deleteAuthCode :: AuthCodeId -> m ()

    {- | Atomically look up and delete an authorization code.

    This operation combines 'lookupAuthCode' and 'deleteAuthCode' into a single
    atomic operation to prevent race conditions. Per RFC 6749 §4.1.2, authorization
    codes MUST be single-use. This method ensures that exactly one concurrent consumer
    can successfully retrieve the code.

    Returns 'Nothing' if:
    * The code does not exist
    * The code has expired (implementation must check expiry time)
    * The code was already consumed by another request

    MUST be implemented atomically to prevent replay attacks. Two concurrent calls
    with the same code ID must result in exactly one returning Just and one returning Nothing.

    Satisfies: Round-trip law, Expiry law, Atomicity (single-use enforcement)
    -}
    consumeAuthCode :: AuthCodeId -> m (Maybe (AuthorizationCode (OAuthUser m)))

    -- * Access Token Operations

    --
    -- Access tokens are bearer tokens used to authenticate API requests.
    -- They have a limited lifetime and can be refreshed using refresh tokens.

    {- | Store an access token with its associated user.

    The implementation should store the mapping from token to authenticated user.

    Satisfies: Round-trip law, Idempotence law, Overwrite law
    -}
    storeAccessToken :: AccessTokenId -> OAuthUser m -> m ()

    {- | Look up an access token and retrieve the associated user.

    Returns 'Nothing' if the token does not exist.
    Note: Access token expiry is typically validated via JWT claims, not storage.

    Satisfies: Round-trip law
    -}
    lookupAccessToken :: AccessTokenId -> m (Maybe (OAuthUser m))

    -- * Refresh Token Operations

    --
    -- Refresh tokens are long-lived tokens used to obtain new access tokens.
    -- They store both the client ID and user to validate token refresh requests.

    {- | Store a refresh token with its associated client and user.

    Satisfies: Round-trip law, Idempotence law, Overwrite law
    -}
    storeRefreshToken :: RefreshTokenId -> (ClientId, OAuthUser m) -> m ()

    {- | Look up a refresh token and retrieve the associated client and user.

    Returns 'Nothing' if the token does not exist.

    Satisfies: Round-trip law
    -}
    lookupRefreshToken :: RefreshTokenId -> m (Maybe (ClientId, OAuthUser m))

    {- | Update an existing refresh token with new client and user data.

    Used during refresh token rotation to update the stored user/client.
    Behavior is undefined if the token does not exist (implementations may
    create a new entry or fail).

    Satisfies: Overwrite law
    -}
    updateRefreshToken :: RefreshTokenId -> (ClientId, OAuthUser m) -> m ()

    -- * Client Registration Operations

    --
    -- OAuth clients must be registered before they can participate in authorization flows.
    -- Client information includes redirect URIs, allowed scopes, and client secrets.

    {- | Store a registered client's information.

    Satisfies: Round-trip law, Idempotence law, Overwrite law
    -}
    storeClient :: ClientId -> ClientInfo -> m ()

    {- | Look up a registered client by its identifier.

    Returns 'Nothing' if the client is not registered.

    Satisfies: Round-trip law
    -}
    lookupClient :: ClientId -> m (Maybe ClientInfo)

    -- * Pending Authorization Operations

    --
    -- Pending authorizations track OAuth flows that are waiting for user approval.
    -- They are stored during the login page display and consumed when the user
    -- approves or denies the authorization.

    {- | Store a pending authorization with its session identifier.

    The implementation should store the authorization along with its expiry time.

    Satisfies: Round-trip law, Idempotence law, Overwrite law
    -}
    storePendingAuth :: SessionId -> PendingAuthorization -> m ()

    {- | Look up a pending authorization by its session identifier.

    Returns 'Nothing' if:
    * The session does not exist
    * The session has expired (implementation must check expiry time)

    Satisfies: Round-trip law, Expiry law
    -}
    lookupPendingAuth :: SessionId -> m (Maybe PendingAuthorization)

    {- | Delete a pending authorization.

    Used after the user approves or denies the authorization to prevent replay.
    Idempotent: deleting a non-existent session is not an error.

    Satisfies: Delete law
    -}
    deletePendingAuth :: SessionId -> m ()
