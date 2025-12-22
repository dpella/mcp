{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Servant.OAuth2.IDP.Handlers.Token
Description : OAuth token endpoint handlers
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC

Token endpoint handlers for authorization_code and refresh_token grants.
-}
module Servant.OAuth2.IDP.Handlers.Token (
    handleToken,
    handleAuthCodeGrant,
    handleRefreshTokenGrant,
    generateJWTAccessToken,
) where

import Control.Monad (unless)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, asks)
import Data.ByteString.Lazy qualified as LBS
import Data.Generics.Product (HasType)
import Data.Generics.Product.Typed (getTyped)
import Data.Generics.Sum.Typed (AsType, injectTyped)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Plow.Logging (IOTracer, traceWith)
import Servant.Auth.Server (JWTSettings, ToJWT, makeJWT)
import Servant.OAuth2.IDP.API (TokenRequest (..), TokenResponse (..))
import Servant.OAuth2.IDP.Config (OAuthEnv (..))
import Servant.OAuth2.IDP.Errors (
    AuthorizationError (..),
    InvalidGrantReason (..),
    InvalidRequestReason (..),
    MalformedReason (..),
 )
import Servant.OAuth2.IDP.PKCE (validateCodeVerifier)
import Servant.OAuth2.IDP.Store (OAuthStateStore (..))
import Servant.OAuth2.IDP.Trace (OAuthTrace (..), OperationResult (..))
import Servant.OAuth2.IDP.Types (
    AccessToken (..),
    AccessTokenId,
    AuthCodeId,
    AuthorizationCode (..),
    CodeVerifier,
    OAuthGrantType (..),
    RefreshToken (..),
    RefreshTokenId,
    ResourceIndicator (..),
    Scopes (..),
    TokenType (..),
    authClientId,
    authCodeChallenge,
    authScopes,
    authUserId,
    generateRefreshTokenId,
    mkAccessTokenId,
    mkTokenValidity,
    unAccessTokenId,
    unRefreshTokenId,
 )

{- | Generate JWT access token for user

Uses TypeApplications to specify the monad context (and thus the OAuthUser type).
Call with: @generateJWTAccessToken \@m user jwtSettings@
-}
generateJWTAccessToken :: forall m e. (OAuthStateStore m, ToJWT (OAuthUser m), MonadIO m, MonadError e m, AsType AuthorizationError e) => OAuthUser m -> JWTSettings -> m AccessTokenId
generateJWTAccessToken user jwtSettings = do
    accessTokenResult <- liftIO $ makeJWT user jwtSettings Nothing
    case accessTokenResult of
        Left err -> throwError $ injectTyped @AuthorizationError $ InvalidRequest $ MalformedRequest $ UnparseableBody $ T.pack $ show err
        Right accessToken -> case TE.decodeUtf8' $ LBS.toStrict accessToken of
            Left decodeErr -> throwError $ injectTyped @AuthorizationError $ InvalidRequest $ MalformedRequest $ UnparseableBody $ T.pack $ show decodeErr
            Right tokenText -> case mkAccessTokenId tokenText of
                Just tokenId -> return tokenId
                Nothing -> error "generateJWTAccessToken: JWT generation produced empty text (impossible)"

{- | Token endpoint handler (polymorphic).

Handles OAuth token requests, dispatching to appropriate grant type handler.

This handler is polymorphic over the monad @m@, requiring:

* 'OAuthStateStore m': Storage for OAuth state
* 'MonadTime m': Access to current time for expiry checks
* 'MonadIO m': Ability to generate JWTs and perform IO
* 'MonadReader env m': Access to environment containing config, tracer, and JWT settings
* 'MonadError e m': Error handling via MonadError
* 'HasType OAuthEnv env': Config can be extracted via generic-lens
* 'HasType (IOTracer OAuthTrace) env': Tracer can be extracted via generic-lens
* 'HasType JWTSettings env': JWT settings can be extracted via generic-lens
* 'AsType OAuthStoreError e': Storage errors can be injected into error type

The handler parses the grant_type parameter and dispatches to:

* 'handleAuthCodeGrant': For authorization_code grant
* 'handleRefreshTokenGrant': For refresh_token grant

== Usage

@
-- In AppM (with AppEnv)
response <- handleToken formParams

-- In custom monad
response <- handleToken formParams
@
-}
handleToken ::
    ( OAuthStateStore m
    , ToJWT (OAuthUser m)
    , MonadIO m
    , MonadReader env m
    , MonadError e m
    , AsType AuthorizationError e
    , HasType OAuthEnv env
    , HasType (IOTracer OAuthTrace) env
    , HasType JWTSettings env
    ) =>
    TokenRequest ->
    m TokenResponse
handleToken tokenRequest = case tokenRequest of
    AuthorizationCodeGrant code verifier mResource ->
        handleAuthCodeGrant code verifier mResource
    RefreshTokenGrant refreshToken mResource ->
        handleRefreshTokenGrant refreshToken mResource

{- | Authorization code grant handler (polymorphic).

Handles token exchange for authorization code grant type.

This handler is polymorphic over the monad @m@, requiring the same constraints
as 'handleToken'.

The handler:

1. Validates the authorization code
2. Verifies the code hasn't expired
3. Validates PKCE code_verifier against stored challenge
4. Generates JWT access token and refresh token
5. Stores tokens and removes the used authorization code
6. Returns TokenResponse with access token, refresh token, and scopes

== Usage

@
-- In AppM (with AppEnv)
response <- handleAuthCodeGrant code verifier mResource
@
-}
handleAuthCodeGrant ::
    forall m env e.
    ( OAuthStateStore m
    , ToJWT (OAuthUser m)
    , MonadIO m
    , MonadReader env m
    , MonadError e m
    , AsType AuthorizationError e
    , HasType OAuthEnv env
    , HasType (IOTracer OAuthTrace) env
    , HasType JWTSettings env
    ) =>
    AuthCodeId ->
    CodeVerifier ->
    Maybe ResourceIndicator ->
    m TokenResponse
handleAuthCodeGrant code codeVerifier _mResource = do
    config <- asks (getTyped @OAuthEnv)
    tracer <- asks (getTyped @(IOTracer OAuthTrace))
    jwtSettings <- asks (getTyped @JWTSettings)

    -- Atomically consume authorization code (lookup + delete, prevents replay attacks)
    mAuthCode <- consumeAuthCode code
    authCode <- case mAuthCode of
        Just ac -> return ac
        Nothing -> do
            -- consumeAuthCode returns Nothing if code doesn't exist, is expired, or already used
            liftIO $ traceWith tracer $ TraceTokenExchange OAuthAuthorizationCode Failure
            throwError $ injectTyped @AuthorizationError $ InvalidGrant (CodeNotFound code)

    -- Verify PKCE
    let authChallenge = authCodeChallenge authCode
        pkceValid = validateCodeVerifier codeVerifier authChallenge
        pkceResult = if pkceValid then Success else Failure
    liftIO $ traceWith tracer $ TracePKCEValidation pkceResult
    unless pkceValid $ do
        liftIO $ traceWith tracer $ TraceTokenExchange OAuthAuthorizationCode Failure
        throwError $ injectTyped @AuthorizationError PKCEVerificationFailed

    -- Extract user directly from auth code (no lookup needed)
    let user = authUserId authCode
        clientId = authClientId authCode

    -- Generate tokens
    accessToken <- generateJWTAccessToken @m user jwtSettings
    refreshToken <- liftIO $ generateRefreshTokenId (oauthRefreshTokenPrefix config)

    -- Store tokens (code already deleted by consumeAuthCode)
    storeAccessToken accessToken user
    storeRefreshToken refreshToken (clientId, user)

    -- Emit successful token exchange trace
    liftIO $ traceWith tracer $ TraceTokenExchange OAuthAuthorizationCode Success

    return
        TokenResponse
            { access_token = AccessToken (unAccessTokenId accessToken)
            , token_type = TokenType "Bearer"
            , expires_in = Just $ mkTokenValidity $ oauthAccessTokenExpiry config
            , refresh_token = Just (RefreshToken (unRefreshTokenId refreshToken))
            , scope = if Set.null (authScopes authCode) then Nothing else Just (Scopes (authScopes authCode))
            }

{- | Refresh token grant handler (polymorphic).

Handles token refresh for refresh_token grant type.

This handler is polymorphic over the monad @m@, requiring the same constraints
as 'handleToken'.

The handler:

1. Validates the refresh token
2. Looks up the associated user and client
3. Generates a new JWT access token
4. Updates the access token mapping
5. Returns TokenResponse with new access token (keeps same refresh token)

== Usage

@
-- In AppM (with AppEnv)
response <- handleRefreshTokenGrant refreshToken mResource
@
-}
handleRefreshTokenGrant ::
    forall m env e.
    ( OAuthStateStore m
    , ToJWT (OAuthUser m)
    , MonadIO m
    , MonadReader env m
    , MonadError e m
    , AsType AuthorizationError e
    , HasType OAuthEnv env
    , HasType (IOTracer OAuthTrace) env
    , HasType JWTSettings env
    ) =>
    RefreshTokenId ->
    Maybe ResourceIndicator ->
    m TokenResponse
handleRefreshTokenGrant refreshTokenId _mResource = do
    config <- asks (getTyped @OAuthEnv)
    tracer <- asks (getTyped @(IOTracer OAuthTrace))
    jwtSettings <- asks (getTyped @JWTSettings)

    -- Look up refresh token
    mTokenInfo <- lookupRefreshToken refreshTokenId
    (clientId, user) <- case mTokenInfo of
        Just info -> return info
        Nothing -> do
            liftIO $ traceWith tracer $ TraceTokenRefresh Failure
            throwError $ injectTyped @AuthorizationError $ InvalidGrant (RefreshTokenNotFound refreshTokenId)

    -- Generate new JWT access token
    newAccessToken <- generateJWTAccessToken @m user jwtSettings

    -- Update tokens (keep same refresh token, update with new client/user association)
    storeAccessToken newAccessToken user
    updateRefreshToken refreshTokenId (clientId, user)

    -- Emit successful token refresh trace
    liftIO $ traceWith tracer $ TraceTokenRefresh Success

    return
        TokenResponse
            { access_token = AccessToken (unAccessTokenId newAccessToken)
            , token_type = TokenType "Bearer"
            , expires_in = Just $ mkTokenValidity $ oauthAccessTokenExpiry config
            , refresh_token = Just (RefreshToken (unRefreshTokenId refreshTokenId))
            , scope = Nothing
            }
