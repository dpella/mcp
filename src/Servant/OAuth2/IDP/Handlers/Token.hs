{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
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
) where

import Control.Monad (unless)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, asks)
import Data.Functor.Contravariant (contramap)
import Data.Generics.Product (HasType)
import Data.Generics.Product.Typed (getTyped)
import Data.Generics.Sum.Typed (AsType, injectTyped)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Servant.Auth.Server (JWTSettings, ToJWT)
import Web.HttpApiData (parseUrlPiece)

import MCP.Server.Auth (
    OAuthConfig (..),
    validateCodeVerifier,
 )
import MCP.Server.HTTP.AppEnv (HTTPServerConfig (..))
import MCP.Trace.HTTP (HTTPTrace (..))
import MCP.Trace.OAuth qualified as OAuthTrace
import Plow.Logging (IOTracer, traceWith)
import Servant.OAuth2.IDP.API (TokenRequest (..), TokenResponse (..))
import Servant.OAuth2.IDP.Handlers.Helpers (generateJWTAccessToken, generateRefreshTokenWithConfig)
import Servant.OAuth2.IDP.Store (OAuthStateStore (..))
import Servant.OAuth2.IDP.Types (
    AccessToken (..),
    AccessTokenId (..),
    AuthCodeId (..),
    AuthorizationCode (..),
    AuthorizationError (..),
    CodeVerifier (..),
    RefreshToken (..),
    RefreshTokenId (..),
    ResourceIndicator (..),
    Scopes (..),
    TokenType (..),
    authClientId,
    authCodeChallenge,
    authScopes,
    authUserId,
    unAuthCodeId,
    unCodeChallenge,
    unCodeVerifier,
    unRefreshTokenId,
    unResourceIndicator,
 )

{- | Token endpoint handler (polymorphic).

Handles OAuth token requests, dispatching to appropriate grant type handler.

This handler is polymorphic over the monad @m@, requiring:

* 'OAuthStateStore m': Storage for OAuth state
* 'MonadTime m': Access to current time for expiry checks
* 'MonadIO m': Ability to generate JWTs and perform IO
* 'MonadReader env m': Access to environment containing config, tracer, and JWT settings
* 'MonadError e m': Error handling via MonadError
* 'HasType HTTPServerConfig env': Config can be extracted via generic-lens
* 'HasType (IOTracer HTTPTrace) env': Tracer can be extracted via generic-lens
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
    , HasType HTTPServerConfig env
    , HasType (IOTracer HTTPTrace) env
    , HasType JWTSettings env
    ) =>
    TokenRequest ->
    m TokenResponse
handleToken tokenRequest = case tokenRequest of
    AuthorizationCodeGrant code verifier mResource -> do
        -- Build param map for existing handler
        let paramMap =
                Map.fromList $
                    [ ("code", unAuthCodeId code)
                    , ("code_verifier", unCodeVerifier verifier)
                    ]
                        ++ maybe [] (\r -> [("resource", unResourceIndicator r)]) mResource
        handleAuthCodeGrant paramMap
    RefreshTokenGrant refreshToken mResource -> do
        -- Build param map for existing handler
        let paramMap =
                Map.fromList $
                    ("refresh_token", unRefreshTokenId refreshToken)
                        : maybe [] (\r -> [("resource", unResourceIndicator r)]) mResource
        handleRefreshTokenGrant paramMap

{- | Authorization code grant handler (polymorphic).

Handles token exchange for authorization code grant type.

This handler is polymorphic over the monad @m@, requiring the same constraints
as 'handleToken'.

The handler:

1. Extracts and validates the authorization code
2. Verifies the code hasn't expired
3. Validates PKCE code_verifier against stored challenge
4. Generates JWT access token and refresh token
5. Stores tokens and removes the used authorization code
6. Returns TokenResponse with access token, refresh token, and scopes

== Usage

@
-- In AppM (with AppEnv)
response <- handleAuthCodeGrant paramMap
@
-}
handleAuthCodeGrant ::
    ( OAuthStateStore m
    , ToJWT (OAuthUser m)
    , MonadIO m
    , MonadReader env m
    , MonadError e m
    , AsType AuthorizationError e
    , HasType HTTPServerConfig env
    , HasType (IOTracer HTTPTrace) env
    , HasType JWTSettings env
    ) =>
    Map Text Text ->
    m TokenResponse
handleAuthCodeGrant params = do
    config <- asks (getTyped @HTTPServerConfig)
    tracer <- asks (getTyped @(IOTracer HTTPTrace))
    jwtSettings <- asks (getTyped @JWTSettings)

    let oauthTracer = contramap HTTPOAuth tracer

    -- Extract and log resource parameter (RFC8707)
    let mResource = Map.lookup "resource" params
    liftIO $ traceWith tracer $ HTTPResourceParameterDebug mResource "token request (auth code)"

    -- Parse code from Text to AuthCodeId
    code <- case Map.lookup "code" params of
        Nothing -> do
            liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthValidationError "token_request" "Missing authorization code"
            throwError $ injectTyped @AuthorizationError $ InvalidRequest "Missing authorization code"
        Just codeText -> case parseUrlPiece codeText of
            Left err -> do
                liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthValidationError "token_request" ("Invalid authorization code: " <> err)
                throwError $ injectTyped @AuthorizationError $ InvalidGrant "Invalid authorization code"
            Right authCodeId -> return authCodeId

    -- Parse code_verifier from Text to CodeVerifier
    codeVerifier <- case Map.lookup "code_verifier" params of
        Nothing -> do
            liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthValidationError "token_request" "Missing code_verifier"
            throwError $ injectTyped @AuthorizationError $ InvalidRequest "Missing code_verifier"
        Just verifierText -> case parseUrlPiece verifierText of
            Left err -> do
                liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthValidationError "token_request" ("Invalid code_verifier: " <> err)
                throwError $ injectTyped @AuthorizationError $ InvalidRequest "Invalid code_verifier"
            Right verifier -> return verifier

    -- Atomically consume authorization code (lookup + delete, prevents replay attacks)
    mAuthCode <- consumeAuthCode code
    authCode <- case mAuthCode of
        Just ac -> return ac
        Nothing -> do
            -- consumeAuthCode returns Nothing if code doesn't exist, is expired, or already used
            liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthTokenExchange "authorization_code" False
            throwError $ injectTyped @AuthorizationError $ InvalidGrant "Invalid or expired authorization code"

    -- Verify PKCE
    let authChallenge = authCodeChallenge authCode
        pkceValid = validateCodeVerifier codeVerifier authChallenge
    liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthPKCEValidation (unCodeVerifier codeVerifier) (unCodeChallenge authChallenge) pkceValid
    unless pkceValid $ do
        liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthTokenExchange "authorization_code" False
        throwError $ injectTyped @AuthorizationError PKCEVerificationFailed

    -- Extract user directly from auth code (no lookup needed)
    let user = authUserId authCode
        clientId = authClientId authCode

    -- Generate tokens
    accessTokenText <- generateJWTAccessToken user jwtSettings
    refreshTokenText <- liftIO $ generateRefreshTokenWithConfig config
    let refreshToken = RefreshTokenId refreshTokenText

    -- Store tokens (code already deleted by consumeAuthCode)
    storeAccessToken (AccessTokenId accessTokenText) user
    storeRefreshToken refreshToken (clientId, user)

    -- Emit successful token exchange trace
    liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthTokenExchange "authorization_code" True

    return
        TokenResponse
            { access_token = AccessToken accessTokenText
            , token_type = TokenType "Bearer"
            , expires_in = Just $ maybe 3600 accessTokenExpirySeconds (httpOAuthConfig config)
            , refresh_token = Just (RefreshToken refreshTokenText)
            , scope = if Set.null (authScopes authCode) then Nothing else Just (Scopes (authScopes authCode))
            }

{- | Refresh token grant handler (polymorphic).

Handles token refresh for refresh_token grant type.

This handler is polymorphic over the monad @m@, requiring the same constraints
as 'handleToken'.

The handler:

1. Extracts and validates the refresh token
2. Looks up the associated user and client
3. Generates a new JWT access token
4. Updates the access token mapping
5. Returns TokenResponse with new access token (keeps same refresh token)

== Usage

@
-- In AppM (with AppEnv)
response <- handleRefreshTokenGrant paramMap
@
-}
handleRefreshTokenGrant ::
    ( OAuthStateStore m
    , ToJWT (OAuthUser m)
    , MonadIO m
    , MonadReader env m
    , MonadError e m
    , AsType AuthorizationError e
    , HasType HTTPServerConfig env
    , HasType (IOTracer HTTPTrace) env
    , HasType JWTSettings env
    ) =>
    Map Text Text ->
    m TokenResponse
handleRefreshTokenGrant params = do
    config <- asks (getTyped @HTTPServerConfig)
    tracer <- asks (getTyped @(IOTracer HTTPTrace))
    jwtSettings <- asks (getTyped @JWTSettings)

    let oauthTracer = contramap HTTPOAuth tracer

    -- Extract and log resource parameter (RFC8707)
    let mResource = Map.lookup "resource" params
    liftIO $ traceWith tracer $ HTTPResourceParameterDebug mResource "token request (refresh token)"

    -- Parse refresh_token from Text to RefreshTokenId
    refreshTokenId <- case Map.lookup "refresh_token" params of
        Nothing -> do
            liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthValidationError "token_request" "Missing refresh_token"
            throwError $ injectTyped @AuthorizationError $ InvalidRequest "Missing refresh_token"
        Just tokenText -> case parseUrlPiece tokenText of
            Left err -> do
                liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthValidationError "token_request" ("Invalid refresh_token: " <> err)
                throwError $ injectTyped @AuthorizationError $ InvalidGrant "Invalid refresh_token"
            Right rtId -> return rtId

    -- Look up refresh token
    mTokenInfo <- lookupRefreshToken refreshTokenId
    (clientId, user) <- case mTokenInfo of
        Just info -> return info
        Nothing -> do
            liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthTokenRefresh False
            throwError $ injectTyped @AuthorizationError $ InvalidGrant "Invalid refresh_token"

    -- Generate new JWT access token
    newAccessTokenText <- generateJWTAccessToken user jwtSettings

    -- Update tokens (keep same refresh token, update with new client/user association)
    storeAccessToken (AccessTokenId newAccessTokenText) user
    updateRefreshToken refreshTokenId (clientId, user)

    -- Emit successful token refresh trace
    liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthTokenRefresh True

    return
        TokenResponse
            { access_token = AccessToken newAccessTokenText
            , token_type = TokenType "Bearer"
            , expires_in = Just $ maybe 3600 accessTokenExpirySeconds (httpOAuthConfig config)
            , refresh_token = Just (RefreshToken (unRefreshTokenId refreshTokenId))
            , scope = Nothing
            }
