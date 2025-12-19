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
import Data.Functor.Contravariant (contramap)
import Data.Generics.Product (HasType)
import Data.Generics.Product.Typed (getTyped)
import Data.Generics.Sum.Typed (AsType, injectTyped)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (isJust)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.UUID.V4 qualified as UUID
import Network.URI (parseURI)
import Servant (
    Header,
    Headers,
    addHeader,
 )
import Web.HttpApiData (ToHttpApiData (..), toUrlPiece)

import Data.UUID qualified as UUID
import MCP.Server.Auth (OAuthConfig (..))
import MCP.Server.HTTP.AppEnv (HTTPServerConfig (..))
import MCP.Server.Time (MonadTime (..))
import MCP.Trace.HTTP (HTTPTrace (..))
import MCP.Trace.OAuth qualified as OAuthTrace
import Plow.Logging (IOTracer, traceWith)
import Servant.OAuth2.IDP.Handlers.HTML (LoginPage (..))
import Servant.OAuth2.IDP.Store (OAuthStateStore (..))
import Servant.OAuth2.IDP.Types (
    AuthorizationError (..),
    ClientId (..),
    ClientInfo (..),
    CodeChallenge,
    CodeChallengeMethod (..),
    OAuthState,
    PendingAuthorization (..),
    RedirectUri,
    ResourceIndicator (..),
    ResponseType (..),
    Scope (..),
    Scopes (..),
    SessionCookie (..),
    ValidationError (..),
    mkSessionId,
    serializeScopeSet,
    unClientId,
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
    , AsType AuthorizationError e
    , HasType HTTPServerConfig env
    , HasType (IOTracer HTTPTrace) env
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
    config <- asks (getTyped @HTTPServerConfig)
    tracer <- asks (getTyped @(IOTracer HTTPTrace))

    let oauthTracer = contramap HTTPOAuth tracer
        responseTypeText = toUrlPiece responseType
        clientIdText = unClientId clientId
        redirectUriText = toUrlPiece redirectUri
        codeChallengeMethodText = toUrlPiece codeChallengeMethod

    -- Log resource parameter for RFC8707 support
    liftIO $ traceWith tracer $ HTTPResourceParameterDebug (fmap unResourceIndicator mResource) "authorize endpoint"

    -- Validate response_type (only "code" supported)
    when (responseType /= ResponseCode) $ do
        liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthValidationError "response_type" ("Unsupported response type: " <> responseTypeText)
        throwError $ injectTyped @ValidationError $ UnsupportedResponseType responseTypeText

    -- Validate code_challenge_method (only "S256" supported)
    when (codeChallengeMethod /= S256) $ do
        liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthValidationError "code_challenge_method" ("Unsupported method: " <> codeChallengeMethodText)
        throwError $ injectTyped @AuthorizationError $ InvalidRequest ("Unsupported code_challenge_method: " <> codeChallengeMethodText)

    -- Look up client to verify it's registered
    mClientInfo <- lookupClient clientId
    clientInfo <- case mClientInfo of
        Just ci -> return ci
        Nothing -> do
            liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthValidationError "client_id" ("Unregistered client: " <> clientIdText)
            throwError $ injectTyped @ValidationError $ ClientNotRegistered clientId

    -- Validate redirect_uri is registered for this client
    unless (redirectUri `elem` NE.toList (clientRedirectUris clientInfo)) $ do
        liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthValidationError "redirect_uri" ("Redirect URI not registered: " <> redirectUriText)
        throwError $ injectTyped @ValidationError $ RedirectUriMismatch clientId redirectUri

    let displayName = clientName clientInfo
        -- Convert Scopes to [Text] for tracing
        scopeList = case mScope of
            Nothing -> []
            Just (Scopes scopes) -> map unScope (Set.toList scopes)

    -- Emit authorization request trace
    liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthAuthorizationRequest clientIdText scopeList (isJust mState)

    -- Generate session ID
    sessionIdText <- liftIO $ UUID.toText <$> UUID.nextRandom
    let sessionId = case mkSessionId sessionIdText of
            Just sid -> sid
            Nothing -> error "Generated invalid session UUID"
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
    liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthLoginPageServed sessionIdText

    -- Build session cookie
    let sessionExpirySeconds = maybe 600 loginSessionExpirySeconds (httpOAuthConfig config)
        -- Add Secure flag if requireHTTPS is True in OAuth config
        secureFlag = case httpOAuthConfig config of
            Just oauthConf | requireHTTPS oauthConf -> "; Secure"
            _ -> ""
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
                }

    return $ addHeader cookieValue loginPage
