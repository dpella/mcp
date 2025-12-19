{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Servant.OAuth2.IDP.Handlers.Registration
Description : OAuth dynamic client registration handler
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC

Dynamic client registration endpoint handler (RFC 7591).
-}
module Servant.OAuth2.IDP.Handlers.Registration (
    handleRegister,
) where

import Control.Monad (when)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, asks)
import Data.Functor.Contravariant (contramap)
import Data.Generics.Product (HasType)
import Data.Generics.Product.Typed (getTyped)
import Data.Generics.Sum.Typed (AsType, injectTyped)
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as Set
import Data.UUID.V4 qualified as UUID

import Data.UUID qualified as UUID
import MCP.Server.Auth (OAuthConfig (..), clientIdPrefix)
import MCP.Server.HTTP.AppEnv (HTTPServerConfig (..))
import MCP.Trace.HTTP (HTTPTrace (..))
import MCP.Trace.OAuth qualified as OAuthTrace
import Plow.Logging (IOTracer, traceWith)
import Servant.OAuth2.IDP.API (
    ClientRegistrationRequest (..),
    ClientRegistrationResponse (..),
 )
import Servant.OAuth2.IDP.Store (OAuthStateStore (..))
import Servant.OAuth2.IDP.Types (
    AuthorizationError (..),
    ClientId (..),
    ClientInfo (..),
    mkClientSecret,
    unClientName,
 )

{- | Dynamic client registration endpoint (polymorphic).

Handles client registration per RFC 7591 and MCP OAuth specification.

This handler is polymorphic over the monad @m@, requiring:

* 'OAuthStateStore m': Storage for registered clients
* 'MonadIO m': Ability to generate UUIDs
* 'MonadReader env m': Access to environment containing config and tracer
* 'HasType HTTPServerConfig env': Config can be extracted via generic-lens
* 'HasType (IOTracer HTTPTrace) env': Tracer can be extracted via generic-lens

The handler:

1. Generates a client ID with configurable prefix
2. Stores the client information via OAuthStateStore
3. Emits structured trace for registration
4. Returns client credentials (empty secret for public clients)

== Usage

@
-- In AppM (with AppEnv)
response <- handleRegister request

-- In custom monad
response <- handleRegister request
@
-}
handleRegister ::
    ( OAuthStateStore m
    , MonadIO m
    , MonadReader env m
    , MonadError e m
    , AsType AuthorizationError e
    , HasType HTTPServerConfig env
    , HasType (IOTracer HTTPTrace) env
    ) =>
    ClientRegistrationRequest ->
    m ClientRegistrationResponse
handleRegister (ClientRegistrationRequest clientName reqRedirects reqGrants reqResponses reqAuth) = do
    config <- asks (getTyped @HTTPServerConfig)
    tracer <- asks (getTyped @(IOTracer HTTPTrace))

    -- Validate redirect_uris is not empty
    when (null reqRedirects) $
        throwError $
            injectTyped @AuthorizationError $
                InvalidRequest "redirect_uris must not be empty"

    -- Generate client ID
    let prefix = maybe "client_" clientIdPrefix (httpOAuthConfig config)
    uuid <- liftIO UUID.nextRandom
    let clientIdText = prefix <> UUID.toText uuid
        clientId = ClientId clientIdText

    -- Convert NonEmpty to Set for ClientInfo
    -- Note: ClientInfo from OAuth.Types requires NonEmpty and Set
    -- reqRedirects is already NonEmpty from ClientRegistrationRequest
    let redirectsNE = reqRedirects
        grantsSet = Set.fromList (NE.toList reqGrants)
        responsesSet = Set.fromList (NE.toList reqResponses)
        clientInfo =
            ClientInfo
                { clientName = unClientName clientName
                , clientRedirectUris = redirectsNE
                , clientGrantTypes = grantsSet
                , clientResponseTypes = responsesSet
                , clientAuthMethod = reqAuth
                }

    storeClient clientId clientInfo

    -- Emit trace
    let oauthTracer = contramap HTTPOAuth tracer
    liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthClientRegistration clientIdText (unClientName clientName)

    let clientSecretNewtype = case mkClientSecret "" of
            Just cs -> cs
            Nothing -> error "mkClientSecret should never fail for empty string"

    return
        ClientRegistrationResponse
            { client_id = clientId
            , client_secret = clientSecretNewtype
            , client_name = clientName
            , redirect_uris = reqRedirects
            , grant_types = reqGrants
            , response_types = reqResponses
            , token_endpoint_auth_method = reqAuth
            }
