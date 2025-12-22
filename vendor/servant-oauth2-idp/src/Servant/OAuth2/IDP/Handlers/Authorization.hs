{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Servant.OAuth2.IDP.Handlers.Authorization
Description : OAuth authorization endpoint handler
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC

Authorization endpoint handler that displays interactive login page.
-}
module Servant.OAuth2.IDP.Handlers.Authorization (
    handleAuthorize,
) where

import Control.Monad (unless, when)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, asks)
import Data.Generics.Product (HasType)
import Data.Generics.Product.Typed (getTyped)
import Data.Generics.Sum.Typed (AsType, injectTyped)
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as Set
import Data.Text qualified as T
import Network.URI (parseURI)
import Servant (
    Header,
    Headers,
    addHeader,
 )
import Web.HttpApiData (ToHttpApiData (..), toUrlPiece)

import Control.Monad.Time (MonadTime (..))
import Data.Time.Clock (nominalDiffTimeToSeconds)
import Plow.Logging (IOTracer, traceWith)
import Servant.OAuth2.IDP.Config (OAuthEnv (..))
import Servant.OAuth2.IDP.Errors (
    ValidationError (..),
 )
import Servant.OAuth2.IDP.Handlers.HTML (LoginPage (..))
import Servant.OAuth2.IDP.Store (OAuthStateStore (..))
import Servant.OAuth2.IDP.Trace (
    OAuthTrace (..),
    OperationResult (..),
 )
import Servant.OAuth2.IDP.Types (
    ClientId,
    ClientInfo (..),
    CodeChallenge,
    CodeChallengeMethod (..),
    OAuthState,
    PendingAuthorization (..),
    RedirectUri,
    ResourceIndicator (..),
    ResponseType (..),
    Scopes (..),
    SessionCookie (..),
    generateSessionId,
    serializeScopeSet,
    unClientName,
    unSessionId,
 )

{- | Authorization endpoint handler (polymorphic).

Handles OAuth authorization requests and returns an interactive login page.

This handler is polymorphic over the monad @m@, requiring:

* 'OAuthStateStore m': Storage for pending authorizations
* 'MonadIO m': Ability to generate UUIDs and get current time
* 'MonadReader env m': Access to environment containing config and tracer
* 'HasType HTTPServerConfig env': Config can be extracted via generic-lens
* 'HasType (IOTracer HTTPTrace) env': Tracer can be extracted via generic-lens

The handler:

1. Validates response_type (only "code" supported)
2. Validates code_challenge_method (only "S256" supported)
3. Looks up the client to verify it's registered
4. Validates the redirect_uri is registered for this client
5. Generates a session ID and stores pending authorization
6. Returns login page HTML with session cookie

== Usage

@
-- In AppM (with AppEnv)
loginPage <- handleAuthorize responseType clientId redirectUri codeChallenge method scope state resource

-- In custom monad
loginPage <- handleAuthorize responseType clientId redirectUri codeChallenge method scope state resource
@
-}
handleAuthorize ::
    ( OAuthStateStore m
    , MonadIO m
    , MonadReader env m
    , MonadError e m
    , AsType ValidationError e
    , HasType OAuthEnv env
    , HasType (IOTracer OAuthTrace) env
    ) =>
    ResponseType ->
    ClientId ->
    RedirectUri ->
    CodeChallenge ->
    CodeChallengeMethod ->
    Maybe Scopes ->
    Maybe OAuthState ->
    Maybe ResourceIndicator ->
    m (Headers '[Header "Set-Cookie" SessionCookie] LoginPage)
handleAuthorize responseType clientId redirectUri codeChallenge codeChallengeMethod mScope mState mResource = do
    config <- asks (getTyped @OAuthEnv)
    tracer <- asks (getTyped @(IOTracer OAuthTrace))

    let responseTypeText = toUrlPiece responseType

    -- Validate response_type (only "code" supported)
    when (responseType /= ResponseCode) $ do
        liftIO $ traceWith tracer $ TraceValidationError $ UnsupportedResponseType responseTypeText
        throwError $ injectTyped @ValidationError $ UnsupportedResponseType responseTypeText

    -- Validate code_challenge_method (only "S256" supported)
    when (codeChallengeMethod /= S256) $ do
        throwError $ injectTyped @ValidationError $ UnsupportedCodeChallengeMethod codeChallengeMethod

    -- Look up client to verify it's registered
    mClientInfo <- lookupClient clientId
    clientInfo <- case mClientInfo of
        Just ci -> return ci
        Nothing -> do
            liftIO $ traceWith tracer $ TraceValidationError $ ClientNotRegistered clientId
            throwError $ injectTyped @ValidationError $ ClientNotRegistered clientId

    -- Validate redirect_uri is registered for this client
    unless (redirectUri `elem` NE.toList (clientRedirectUris clientInfo)) $ do
        liftIO $ traceWith tracer $ TraceValidationError $ RedirectUriMismatch clientId redirectUri
        throwError $ injectTyped @ValidationError $ RedirectUriMismatch clientId redirectUri

    let displayName = unClientName $ clientName clientInfo
        -- Convert Scopes to [Scope] for tracing
        scopeList = case mScope of
            Nothing -> []
            Just (Scopes scopes) -> Set.toList scopes

    -- Emit authorization request trace
    liftIO $ traceWith tracer $ TraceAuthorizationRequest clientId scopeList Success

    -- Generate session ID
    sessionId <- liftIO generateSessionId
    now <- currentTime

    -- Extract Set Scope from Scopes (already parsed by Servant)
    let scopesSet = fmap unScopes mScope
        -- Convert mResource from Maybe ResourceIndicator to Maybe URI
        resourceUri = mResource >>= (parseURI . T.unpack . unResourceIndicator)

    -- Create pending authorization
    let pending =
            PendingAuthorization
                { pendingClientId = clientId
                , pendingRedirectUri = redirectUri
                , pendingCodeChallenge = codeChallenge
                , pendingCodeChallengeMethod = codeChallengeMethod
                , pendingScope = scopesSet
                , pendingState = mState
                , pendingResource = resourceUri
                , pendingCreatedAt = now
                }

    -- Store pending authorization
    storePendingAuth sessionId pending

    -- Emit login page served trace
    liftIO $ traceWith tracer $ TraceLoginPageServed sessionId

    -- Build session cookie
    let sessionIdText = unSessionId sessionId
        sessionExpirySeconds :: Integer
        sessionExpirySeconds = truncate $ nominalDiffTimeToSeconds (oauthLoginSessionExpiry config)
        -- Add Secure flag if requireHTTPS is True in OAuth config
        secureFlag = if oauthRequireHTTPS config then "; Secure" else ""
        cookieValue = SessionCookie $ "mcp_session=" <> sessionIdText <> "; HttpOnly; SameSite=Strict; Path=/; Max-Age=" <> T.pack (show sessionExpirySeconds) <> secureFlag
        -- Convert Scopes to Text for display
        scopesText = case mScope of
            Nothing -> "default access"
            Just (Scopes scopeSet) -> serializeScopeSet scopeSet
        loginPage =
            LoginPage
                { loginClientName = displayName
                , loginScopes = scopesText
                , loginResource = fmap unResourceIndicator mResource
                , loginSessionId = sessionIdText
                , loginServerName = oauthServerName config
                , loginScopeDescriptions = oauthScopeDescriptions config
                }

    return $ addHeader cookieValue loginPage
