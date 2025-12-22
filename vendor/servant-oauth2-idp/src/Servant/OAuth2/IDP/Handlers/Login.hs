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
import Data.Generics.Product (HasType)
import Data.Generics.Product.Typed (getTyped)
import Data.Generics.Sum.Typed (AsType, injectTyped)
import Data.Maybe (fromMaybe, isJust)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock (addUTCTime)
import Servant (
    Header,
    Headers,
    NoContent (..),
    addHeader,
 )
import Web.HttpApiData (ToHttpApiData (..), toUrlPiece)

import Control.Monad.Time (MonadTime (..))
import Plow.Logging (IOTracer, traceWith)
import Servant.OAuth2.IDP.API (LoginForm (..))
import Servant.OAuth2.IDP.Auth.Backend (AuthBackend (..))
import Servant.OAuth2.IDP.Config (OAuthEnv (..))
import Servant.OAuth2.IDP.Errors (
    AuthorizationError (..),
    InvalidClientReason (..),
    LoginFlowError (..),
 )
import Servant.OAuth2.IDP.Store (OAuthStateStore (..))
import Servant.OAuth2.IDP.Trace (DenialReason (..), OAuthTrace (..), OperationResult (..))
import Servant.OAuth2.IDP.Types (
    AuthorizationCode (..),
    LoginAction (..),
    PendingAuthorization (..),
    RedirectTarget (..),
    SessionCookie (..),
    generateAuthCodeId,
    mkSessionId,
    pendingClientId,
    pendingCodeChallenge,
    pendingCodeChallengeMethod,
    pendingCreatedAt,
    pendingRedirectUri,
    pendingScope,
    pendingState,
    unAuthCodeId,
 )

{- | Login form submission handler (polymorphic).

Handles user credential validation and authorization code generation.

This handler is polymorphic over the monad @m@, requiring:

* 'OAuthStateStore m': Storage for pending authorizations and authorization codes
* 'AuthBackend m': Credential validation backend
* 'MonadTime m': Access to current time for expiry checks
* 'MonadIO m': Ability to generate UUIDs and perform IO
* 'MonadReader env m': Access to environment containing config and tracer
* 'HasType OAuthEnv env': Config can be extracted via generic-lens
* 'HasType (IOTracer OAuthTrace) env': Tracer can be extracted via generic-lens

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
    , HasType OAuthEnv env
    , HasType (IOTracer OAuthTrace) env
    ) =>
    Maybe Text ->
    LoginForm ->
    m (Headers '[Header "Location" RedirectTarget, Header "Set-Cookie" SessionCookie] NoContent)
handleLogin mCookie loginForm = do
    config <- asks (getTyped @OAuthEnv)
    tracer <- asks (getTyped @(IOTracer OAuthTrace))

    let sessionId = formSessionId loginForm

    -- T039: Handle cookies disabled - check if cookie matches form session_id
    case mCookie of
        Nothing -> do
            -- Note: No specific ValidationError for cookie issues, LoginFlowError handles this
            throwError $ injectTyped @LoginFlowError CookiesRequired
        Just cookie ->
            -- Parse session cookie and verify it matches form session_id
            let cookies = T.splitOn ";" cookie
                sessionCookies = filter (T.isInfixOf "mcp_session=") cookies
                cookieSessionId = case sessionCookies of
                    (sessionCookie : _) ->
                        let parts = T.splitOn "=" sessionCookie
                         in case parts of
                                [_, value] -> mkSessionId (T.strip value)
                                _ -> Nothing
                    [] -> Nothing
             in unless (cookieSessionId == Just sessionId) $ do
                    -- Note: No specific ValidationError for cookie mismatch, LoginFlowError handles this
                    throwError $ injectTyped @LoginFlowError SessionCookieMismatch

    -- Look up pending authorization
    mPending <- lookupPendingAuth sessionId
    pending <- case mPending of
        Just p -> return p
        Nothing -> do
            -- Note: No specific ValidationError for session not found, LoginFlowError handles this
            throwError $ injectTyped @LoginFlowError $ SessionNotFound sessionId

    -- T038: Handle expired sessions
    now <- currentTime
    let sessionExpirySeconds = oauthLoginSessionExpiry config
        expiryTime = addUTCTime sessionExpirySeconds (pendingCreatedAt pending)
    when (now > expiryTime) $ do
        liftIO $ traceWith tracer $ TraceSessionExpired sessionId
        throwError $ injectTyped @LoginFlowError $ SessionExpired sessionId

    -- Check if user denied access
    if formAction loginForm == ActionDeny
        then do
            -- Emit denial trace
            liftIO $ traceWith tracer $ TraceAuthorizationDenied (pendingClientId pending) UserDenied

            -- Clear session and redirect with error
            -- Add Secure flag if requireHTTPS is True in OAuth config
            let secureFlag = if oauthRequireHTTPS config then "; Secure" else ""
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
            let loginResult = if isJust validationResult then Success else Failure
            liftIO $ traceWith tracer $ TraceLoginAttempt username loginResult
            case validationResult of
                Just authUser -> do
                    -- Emit authorization granted trace
                    liftIO $ traceWith tracer $ TraceAuthorizationGranted (pendingClientId pending) username

                    -- Generate authorization code
                    code <- liftIO $ generateAuthCodeId (oauthAuthCodePrefix config)
                    codeGenerationTime <- currentTime
                    let expirySeconds = oauthAuthCodeExpiry config
                        expiry = addUTCTime expirySeconds codeGenerationTime
                        -- Convert pendingScope from Maybe (Set Scope) to Set Scope
                        scopes = fromMaybe Set.empty (pendingScope pending)
                        authCode =
                            AuthorizationCode
                                { authCodeId = code
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
                    let secureFlag = if oauthRequireHTTPS config then "; Secure" else ""
                        stateParam = case pendingState pending of
                            Just s -> "&state=" <> toUrlPiece s
                            Nothing -> ""
                        redirectUrl = RedirectTarget $ toUrlPiece (pendingRedirectUri pending) <> "?code=" <> unAuthCodeId code <> stateParam
                        clearCookie = SessionCookie $ "mcp_session=; Max-Age=0; Path=/" <> secureFlag

                    return $ addHeader redirectUrl $ addHeader clearCookie NoContent
                Nothing ->
                    -- Invalid credentials - return 401 OAuth error (validateCredentials already emitted trace)
                    throwError $ injectTyped @AuthorizationError $ InvalidClient InvalidClientCredentials
