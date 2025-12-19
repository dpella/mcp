{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Servant.OAuth2.IDP.Handlers.Login
Description : OAuth login form submission handler
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC

Login form submission handler that validates credentials and generates authorization codes.
-}
module Servant.OAuth2.IDP.Handlers.Login (
    handleLogin,
) where

import Control.Monad (unless, when)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, asks)
import Data.Functor.Contravariant (contramap)
import Data.Generics.Product (HasType)
import Data.Generics.Product.Typed (getTyped)
import Data.Generics.Sum.Typed (AsType, injectTyped)
import Data.Maybe (fromMaybe, isJust)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Time.Clock (addUTCTime)
import Servant (
    Header,
    Headers,
    NoContent (..),
    addHeader,
 )
import Web.HttpApiData (ToHttpApiData (..), toUrlPiece)

import MCP.Server.Auth (OAuthConfig (..))
import MCP.Server.HTTP.AppEnv (HTTPServerConfig (..))
import MCP.Server.Time (MonadTime (..))
import MCP.Trace.HTTP (HTTPTrace (..))
import MCP.Trace.OAuth qualified as OAuthTrace
import Plow.Logging (IOTracer, traceWith)
import Servant.OAuth2.IDP.API (LoginForm (..))
import Servant.OAuth2.IDP.Auth.Backend (AuthBackend (..), Username (..), unUsername)
import Servant.OAuth2.IDP.Handlers.Helpers (extractSessionFromCookie, generateAuthCode)
import Servant.OAuth2.IDP.LoginFlowError (LoginFlowError (..))
import Servant.OAuth2.IDP.Store (OAuthStateStore (..))
import Servant.OAuth2.IDP.Types (
    AuthCodeId (..),
    AuthorizationCode (..),
    AuthorizationError (..),
    PendingAuthorization (..),
    RedirectTarget (..),
    SessionCookie (..),
    SessionId (..),
    pendingClientId,
    pendingCodeChallenge,
    pendingCodeChallengeMethod,
    pendingCreatedAt,
    pendingRedirectUri,
    pendingScope,
    pendingState,
    unClientId,
    unSessionId,
 )

{- | Login form submission handler (polymorphic).

Handles user credential validation and authorization code generation.

This handler is polymorphic over the monad @m@, requiring:

* 'OAuthStateStore m': Storage for pending authorizations and authorization codes
* 'AuthBackend m': Credential validation backend
* 'MonadTime m': Access to current time for expiry checks
* 'MonadIO m': Ability to generate UUIDs and perform IO
* 'MonadReader env m': Access to environment containing config and tracer
* 'HasType HTTPServerConfig env': Config can be extracted via generic-lens
* 'HasType (IOTracer HTTPTrace) env': Tracer can be extracted via generic-lens

The handler:

1. Validates session cookie matches form session_id
2. Looks up pending authorization by session ID
3. Checks if session has expired
4. Handles "deny" action by redirecting with error
5. Validates credentials via AuthBackend
6. Generates authorization code and stores it
7. Redirects with authorization code or error

== Usage

@
-- In AppM (with AppEnv)
response <- handleLogin mCookie form

-- In custom monad
response <- handleLogin mCookie form
@
-}
handleLogin ::
    forall m env e.
    ( OAuthStateStore m
    , AuthBackend m
    , AuthBackendUser m ~ OAuthUser m
    , MonadIO m
    , MonadReader env m
    , MonadError e m
    , AsType AuthorizationError e
    , AsType LoginFlowError e
    , HasType HTTPServerConfig env
    , HasType (IOTracer HTTPTrace) env
    ) =>
    Maybe Text ->
    LoginForm ->
    m (Headers '[Header "Location" RedirectTarget, Header "Set-Cookie" SessionCookie] NoContent)
handleLogin mCookie loginForm = do
    config <- asks (getTyped @HTTPServerConfig)
    tracer <- asks (getTyped @(IOTracer HTTPTrace))

    let oauthTracer = contramap HTTPOAuth tracer
        sessionId = formSessionId loginForm

    -- T039: Handle cookies disabled - check if cookie matches form session_id
    case mCookie of
        Nothing -> do
            liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthValidationError "cookies" "No cookie header present"
            throwError $ injectTyped @LoginFlowError CookiesRequired
        Just cookie ->
            -- Parse session cookie and verify it matches form session_id
            let cookieSessionId = extractSessionFromCookie cookie
             in unless (cookieSessionId == Just sessionId) $ do
                    liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthValidationError "cookies" "Session cookie mismatch"
                    throwError $ injectTyped @LoginFlowError SessionCookieMismatch

    -- Look up pending authorization
    mPending <- lookupPendingAuth sessionId
    pending <- case mPending of
        Just p -> return p
        Nothing -> do
            liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthValidationError "session" ("Session not found: " <> unSessionId sessionId)
            throwError $ injectTyped @LoginFlowError $ SessionNotFound sessionId

    -- T038: Handle expired sessions
    now <- currentTime
    let sessionExpirySeconds = fromIntegral $ maybe 600 loginSessionExpirySeconds (httpOAuthConfig config)
        expiryTime = addUTCTime sessionExpirySeconds (pendingCreatedAt pending)
    when (now > expiryTime) $ do
        liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthSessionExpired (unSessionId sessionId)
        throwError $ injectTyped @LoginFlowError $ SessionExpired sessionId

    -- Check if user denied access
    if formAction loginForm == "deny"
        then do
            -- Emit denial trace
            liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthAuthorizationDenied (unClientId $ pendingClientId pending) "User denied authorization"

            -- Clear session and redirect with error
            -- Add Secure flag if requireHTTPS is True in OAuth config
            let secureFlag = case httpOAuthConfig config of
                    Just oauthConf | requireHTTPS oauthConf -> "; Secure"
                    _ -> ""
                clearCookie = SessionCookie $ "mcp_session=; Max-Age=0; Path=/" <> secureFlag
                errorParams = "error=access_denied&error_description=User%20denied%20access"
                stateParam = case pendingState pending of
                    Just s -> "&state=" <> toUrlPiece s
                    Nothing -> ""
                redirectUrl = RedirectTarget $ toUrlPiece (pendingRedirectUri pending) <> "?" <> errorParams <> stateParam

            -- Remove pending authorization
            deletePendingAuth sessionId

            return $ addHeader redirectUrl $ addHeader clearCookie NoContent
        else do
            -- Validate credentials via AuthBackend
            let username = formUsername loginForm
                password = formPassword loginForm

            validationResult <- validateCredentials username password
            liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthLoginAttempt (unUsername username) (isJust validationResult)
            case validationResult of
                Just authUser -> do
                    -- Emit authorization granted trace
                    liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthAuthorizationGranted (unClientId $ pendingClientId pending) (unUsername username)

                    -- Generate authorization code
                    code <- liftIO $ generateAuthCode config
                    codeGenerationTime <- currentTime
                    let oauthCfg = httpOAuthConfig config
                        expirySeconds = fromIntegral $ maybe 600 authCodeExpirySeconds oauthCfg
                        expiry = addUTCTime expirySeconds codeGenerationTime
                        -- Convert pendingScope from Maybe (Set Scope) to Set Scope
                        scopes = fromMaybe Set.empty (pendingScope pending)
                        codeId = AuthCodeId code
                        authCode =
                            AuthorizationCode
                                { authCodeId = codeId
                                , authClientId = pendingClientId pending
                                , authRedirectUri = pendingRedirectUri pending
                                , authCodeChallenge = pendingCodeChallenge pending
                                , authCodeChallengeMethod = pendingCodeChallengeMethod pending
                                , authScopes = scopes
                                , authUserId = authUser -- Store full user, not just ID
                                , authExpiry = expiry
                                }

                    -- Store authorization code and remove pending authorization
                    storeAuthCode authCode
                    deletePendingAuth sessionId

                    -- Build redirect URL with code
                    -- Add Secure flag if requireHTTPS is True in OAuth config
                    let secureFlag = case httpOAuthConfig config of
                            Just oauthConf | requireHTTPS oauthConf -> "; Secure"
                            _ -> ""
                        stateParam = case pendingState pending of
                            Just s -> "&state=" <> toUrlPiece s
                            Nothing -> ""
                        redirectUrl = RedirectTarget $ toUrlPiece (pendingRedirectUri pending) <> "?code=" <> code <> stateParam
                        clearCookie = SessionCookie $ "mcp_session=; Max-Age=0; Path=/" <> secureFlag

                    return $ addHeader redirectUrl $ addHeader clearCookie NoContent
                Nothing ->
                    -- Invalid credentials - return 401 OAuth error (validateCredentials already emitted trace)
                    throwError $ injectTyped @AuthorizationError $ InvalidClient "Invalid credentials"
