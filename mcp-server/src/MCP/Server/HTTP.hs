{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module:      MCP.Server.HTTP
License:     MPL-2.0
Maintainer:  <matti@dpella.io>, <lobo@dpella.io>

Servant-based HTTP transports for the MCP server.

This module provides two HTTP transports:

* 'MCPAPI' / 'mcpAPI' — a JWT-authenticated transport using @servant-auth@.
  Use this when your server is exposed to the network and you need
  per-request identity.

* 'SimpleHTTPAPI' / 'simpleHttpApp' — an unauthenticated transport that
  accepts all requests unconditionally.  This should only be used on
  @localhost@ for local development, or behind a reverse proxy (e.g.
  Nginx, Envoy, Cloudflare Access) that handles authentication before
  requests reach the application.  Do __not__ expose it directly to the
  public internet.

Both transports accept JSON-RPC requests via POST and return responses as
SSE streams at the @\/mcp@ endpoint.
-}
module MCP.Server.HTTP (
    -- * JWT-authenticated API
    MCPAPI,
    mcpAPI,
    handleMCPRequest,
    handleMCPEvents,

    -- * Simple (unauthenticated) API
    SimpleHTTPAPI,
    simpleHttpApp,
) where

import Control.Concurrent.MVar
import Data.Aeson (encode, object, (.=))
import Data.Text (Text)
import MCP.Server.Common
import MCP.Server.HTTP.Internal
import Servant
import Servant.Auth.Server (Auth, AuthResult (..), JWT)
import Servant.Auth.Server qualified as AuthServer
import Servant.Types.SourceT

-- ---------------------------------------------------------------------------
-- JWT-authenticated API
-- ---------------------------------------------------------------------------

{- | Servant API type for the JWT-authenticated MCP endpoint.

Accepts JSON-RPC requests with JWT authentication via servant-auth.
All MCP methods are multiplexed through this single endpoint.
-}
type MCPAPI =
    "mcp"
        :> Auth '[JWT] MCPHandlerUser
        -- \^ JWT authentication using servant-auth
        :> ReqBody '[JSON] JSONRPCMessage
        -- \^ JSON-RPC request
        :> StreamPost JSONRPCFrame JSONRPCEvent (SourceIO JSONRPCMessage)
        -- \^ Stream of JSON-RPC responses
        :<|> "mcp"
            :> Auth '[JWT] MCPHandlerUser
            :> StreamGet JSONRPCFrame JSONRPCEvent (SourceIO JSONRPCMessage)

-- | Type of the MCP API
mcpAPI :: MVar MCPServerState -> Server MCPAPI
mcpAPI state_var =
    handleMCPRequest state_var
        :<|> handleMCPEvents

-- | This handles the Get /mcp requests. Does nothing for now except authenticate.
handleMCPEvents ::
    AuthResult MCPHandlerUser ->
    Handler (SourceIO JSONRPCMessage)
handleMCPEvents auth_result = do
    case auth_result of
        AuthServer.NoSuchUser -> throwError err401{errBody = "Invalid authentication credentials"}
        AuthServer.BadPassword -> throwError err401{errBody = "Authentication failed"}
        AuthServer.Indefinite -> throwError err401{errBody = "Authentication error"}
        AuthServer.Authenticated _ -> return $ fromStepT $ Stop

{- | Handle incoming MCP JSON-RPC requests.

Validates JWT authentication, then delegates to the shared
'handleMCPRequestCore' from "MCP.Server.HTTP.Internal".
-}
handleMCPRequest ::
    MVar MCPServerState ->
    AuthResult MCPHandlerUser ->
    JSONRPCMessage ->
    Handler (SourceIO JSONRPCMessage)
handleMCPRequest state_var auth_result request_value =
    case auth_result of
        AuthServer.NoSuchUser -> mcpAuthError "Invalid authentication credentials"
        AuthServer.BadPassword -> mcpAuthError "Authentication failed"
        AuthServer.Indefinite -> mcpAuthError "Authentication error"
        AuthServer.Authenticated auth_user ->
            handleMCPRequestCore state_var (Just auth_user) request_value
  where
    mcpAuthError :: Text -> Handler (SourceIO JSONRPCMessage)
    mcpAuthError err = throwError err401{errBody = encode $ object ["error" .= err]}

-- ---------------------------------------------------------------------------
-- Simple (unauthenticated) API
-- ---------------------------------------------------------------------------

{- | Servant API type for the unauthenticated MCP endpoint.

Unlike 'MCPAPI', this does not use @servant-auth@ and performs no
authentication.  See the module documentation for guidance on when it is
safe to use this transport.
-}
type SimpleHTTPAPI =
    "mcp"
        :> ReqBody '[JSON] JSONRPCMessage
        :> StreamPost JSONRPCFrame JSONRPCEvent (SourceIO JSONRPCMessage)
        :<|> "mcp"
            :> StreamGet JSONRPCFrame JSONRPCEvent (SourceIO JSONRPCMessage)

{- | Build a WAI 'Application' for the unauthenticated HTTP transport.

Every request is accepted without authentication.  This should only be
used on @localhost@ or behind an authenticating reverse proxy.
-}
simpleHttpApp :: MVar MCPServerState -> Application
simpleHttpApp state_var =
    serve (Proxy @SimpleHTTPAPI) $
        handleSimpleHTTPRequest state_var
            :<|> handleSimpleHTTPEvents

-- | Handle GET \/mcp requests (unauthenticated).
handleSimpleHTTPEvents :: Handler (SourceIO JSONRPCMessage)
handleSimpleHTTPEvents = return $ fromStepT Stop

{- | Handle incoming MCP JSON-RPC requests (unauthenticated).

Delegates directly to 'handleMCPRequestCore'.  No user type is available,
so @mcp_handler_init@ is not called.
-}
handleSimpleHTTPRequest ::
    MVar MCPServerState ->
    JSONRPCMessage ->
    Handler (SourceIO JSONRPCMessage)
handleSimpleHTTPRequest state_var = handleMCPRequestCore state_var Nothing
