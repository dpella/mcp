{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : MCP.Trace.HTTP
Description : HTTP transport tracing types
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC

HTTP transport tracing types for structured logging of HTTP-specific events.
-}
module MCP.Trace.HTTP (
    HTTPTrace (..),
    OAuthBoundaryTrace (..),
    renderHTTPTrace,
) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import MCP.Trace.Operation (OperationTrace, renderOperationTrace)
import MCP.Trace.Protocol (ProtocolTrace, renderProtocolTrace)
import MCP.Trace.Server (ServerTrace, renderServerTrace)
import Servant.OAuth2.IDP.Errors (
    AuthorizationError,
    LoginFlowError,
    ValidationError,
 )
import Servant.OAuth2.IDP.Trace (OAuthTrace, renderOAuthTrace)

{- | Trace events for boundary error translation.

These events are logged for observability but never exposed to clients.
-}
data OAuthBoundaryTrace
    = -- | OAuth state storage error (details hidden from client)
      BoundaryStoreError Text
    | -- | Authentication backend error (details hidden from client)
      BoundaryAuthError Text
    | -- | Validation error (safe to expose)
      BoundaryValidationError ValidationError
    | -- | Authorization error (safe to expose)
      BoundaryAuthorizationError AuthorizationError
    | -- | Login flow error (safe to expose, renders as HTML)
      BoundaryLoginFlowError LoginFlowError
    deriving (Eq, Show)

{- | HTTP transport-specific events.

Defines trace events for observability of HTTP server operations including
server lifecycle, request handling, OAuth flows, and MCP message processing.
-}
data HTTPTrace
    = -- | HTTP server starting up
      HTTPServerStarting
        { tracePort :: Int
        , traceBaseUrl :: Text
        }
    | -- | HTTP server successfully started
      HTTPServerStarted
    | -- | HTTP request received
      HTTPRequestReceived
        { tracePath :: Text
        , traceMethod :: Text
        , traceHasAuth :: Bool
        }
    | -- | Authentication required for this request
      HTTPAuthRequired
        { traceAuthPath :: Text
        }
    | -- | Authentication succeeded
      HTTPAuthSuccess
        { traceAuthUserId :: Text
        }
    | -- | Authentication failed
      HTTPAuthFailure
        { traceAuthReason :: Text
        }
    | -- | OAuth authentication enabled
      HTTPOAuthEnabled
        { traceAuthEndpoint :: Text
        , traceTokenEndpoint :: Text
        }
    | -- | OAuth providers configured
      HTTPOAuthProviders
        { traceProviderNames :: [Text]
        }
    | -- | PKCE required by MCP spec
      HTTPPKCEEnabled
    | -- | RFC8707 resource parameter debug log
      HTTPResourceParameterDebug
        { traceResourceParam :: Maybe Text
        , traceContext :: Text
        }
    | {- | Nested protocol events in HTTP context.
      This composite constructor is part of the skeleton structure.
      -}
      HTTPProtocol ProtocolTrace
    | {- | Nested OAuth events in HTTP context.
      This composite constructor is part of the skeleton structure.
      -}
      HTTPOAuth OAuthTrace
    | -- | Nested server lifecycle events in HTTP context.
      HTTPServer ServerTrace
    | -- | Nested MCP operation events in HTTP context.
      HTTPOperation OperationTrace
    | -- | OAuth boundary error translation events
      HTTPOAuthBoundary OAuthBoundaryTrace
    deriving (Show, Eq)

{- | Render an HTTPTrace to human-readable text.

Delegates to sub-renders for nested events.
-}
renderHTTPTrace :: HTTPTrace -> Text
renderHTTPTrace (HTTPServerStarting p baseUrl) =
    "[HTTP] Server starting on port " <> T.pack (show p) <> " (" <> baseUrl <> ")"
renderHTTPTrace HTTPServerStarted =
    "[HTTP] Server started"
renderHTTPTrace (HTTPRequestReceived reqPath reqMethod hasAuthHeader) =
    "[HTTP] Request received: "
        <> reqMethod
        <> " "
        <> reqPath
        <> (if hasAuthHeader then " (authenticated)" else " (no auth)")
renderHTTPTrace (HTTPAuthRequired reqPath) =
    "[HTTP] Authentication required for " <> reqPath
renderHTTPTrace (HTTPAuthSuccess uid) =
    "[HTTP] Authentication successful for user " <> uid
renderHTTPTrace (HTTPAuthFailure rsn) =
    "[HTTP] Authentication failed: " <> rsn
renderHTTPTrace (HTTPOAuthEnabled authEp tokenEp) =
    "[HTTP] OAuth authentication enabled - Auth: " <> authEp <> ", Token: " <> tokenEp
renderHTTPTrace (HTTPOAuthProviders providers) =
    "[HTTP] OAuth providers: " <> T.intercalate ", " providers
renderHTTPTrace HTTPPKCEEnabled =
    "[HTTP] PKCE enabled (required by MCP spec)"
renderHTTPTrace (HTTPResourceParameterDebug mResource ctx) =
    "[HTTP] Resource parameter (" <> ctx <> "): " <> fromMaybe "not provided" mResource
renderHTTPTrace (HTTPProtocol pt) = "[HTTP:Protocol] " <> renderProtocolTrace pt
renderHTTPTrace (HTTPOAuth ot) = "[HTTP:OAuth] " <> renderOAuthTrace ot
renderHTTPTrace (HTTPServer st) = "[HTTP:Server] " <> renderServerTrace st
renderHTTPTrace (HTTPOperation ot) = "[HTTP] " <> renderOperationTrace ot
renderHTTPTrace (HTTPOAuthBoundary bt) = "[HTTP:OAuth:Boundary] " <> renderBoundaryTrace bt
  where
    renderBoundaryTrace :: OAuthBoundaryTrace -> Text
    renderBoundaryTrace (BoundaryStoreError msg) = "Storage error (details logged): " <> msg
    renderBoundaryTrace (BoundaryAuthError msg) = "Auth error (details logged): " <> msg
    renderBoundaryTrace (BoundaryValidationError _) = "Validation error (safe to expose)"
    renderBoundaryTrace (BoundaryAuthorizationError _) = "Authorization error (safe to expose)"
    renderBoundaryTrace (BoundaryLoginFlowError _) = "Login flow error (safe to expose, rendered as HTML)"
