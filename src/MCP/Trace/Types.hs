{-# LANGUAGE LambdaCase #-}

{- |
Module      : MCP.Trace.Types
Description : Root trace type hierarchy for MCP library
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC

Root trace type that composes all subsystem traces. Provides a unified
tracing interface with re-exports of all trace types for convenience.
-}
module MCP.Trace.Types (
    -- * Root Type
    MCPTrace (..),
    renderMCPTrace,

    -- * Filter Predicates
    isServerTrace,
    isProtocolTrace,
    isStdIOTrace,
    isHTTPTrace,
    isOAuthTrace,
    isErrorTrace,

    -- * Subsystem Types (re-exports)
    ServerTrace (..),
    renderServerTrace,
    ProtocolTrace (..),
    renderProtocolTrace,
    StdIOTrace (..),
    renderStdIOTrace,
    HTTPTrace (..),
    renderHTTPTrace,
    OAuthTrace (..),
    renderOAuthTrace,
) where

import Data.Text (Text)
import MCP.Trace.HTTP (HTTPTrace (..), renderHTTPTrace)
import MCP.Trace.Protocol (ProtocolTrace (..), renderProtocolTrace)
import MCP.Trace.Server (ServerTrace (..), renderServerTrace)
import MCP.Trace.StdIO (StdIOTrace (..), renderStdIOTrace)
import Servant.OAuth2.IDP.Trace (OAuthTrace (..), renderOAuthTrace)

{- | Root trace type for the MCP library.

Composes all subsystem traces via constructors.
Current implementation is a skeleton for Phase 2.
-}
data MCPTrace
    = -- | Core server lifecycle events
      MCPServer ServerTrace
    | -- | JSON-RPC message handling
      MCPProtocol ProtocolTrace
    | -- | StdIO transport events
      MCPStdIO StdIOTrace
    | -- | HTTP transport events (includes OAuth)
      MCPHttp HTTPTrace
    deriving (Show, Eq)

{- | Render an MCPTrace to human-readable text.

Delegates to subsystem render functions.
-}
renderMCPTrace :: MCPTrace -> Text
renderMCPTrace = \case
    MCPServer t -> renderServerTrace t
    MCPProtocol t -> renderProtocolTrace t
    MCPStdIO t -> renderStdIOTrace t
    MCPHttp t -> renderHTTPTrace t

-- -----------------------------------------------------------------------------
-- Filter Predicates
-- -----------------------------------------------------------------------------

-- | Check if trace is from the Server subsystem.
isServerTrace :: MCPTrace -> Bool
isServerTrace (MCPServer _) = True
isServerTrace _ = False

-- | Check if trace is from the Protocol subsystem.
isProtocolTrace :: MCPTrace -> Bool
isProtocolTrace (MCPProtocol _) = True
isProtocolTrace _ = False

-- | Check if trace is from the StdIO subsystem.
isStdIOTrace :: MCPTrace -> Bool
isStdIOTrace (MCPStdIO _) = True
isStdIOTrace _ = False

-- | Check if trace is from the HTTP subsystem.
isHTTPTrace :: MCPTrace -> Bool
isHTTPTrace (MCPHttp _) = True
isHTTPTrace _ = False

-- | Check if trace is from the OAuth subsystem (nested in HTTP).
isOAuthTrace :: MCPTrace -> Bool
isOAuthTrace (MCPHttp (HTTPOAuth _)) = True
isOAuthTrace _ = False

{- | Check if trace represents an error condition.

Matches all error constructors across subsystem types:

* Protocol errors: ProtocolParseError, ProtocolMethodNotFound, ProtocolInvalidParams
* StdIO errors: StdIOReadError
* HTTP errors: HTTPAuthFailure
* OAuth errors: OAuthAuthorizationDenied, OAuthValidationError
-}
isErrorTrace :: MCPTrace -> Bool
isErrorTrace (MCPProtocol (ProtocolParseError{})) = True
isErrorTrace (MCPProtocol (ProtocolMethodNotFound{})) = True
isErrorTrace (MCPProtocol (ProtocolInvalidParams{})) = True
isErrorTrace (MCPStdIO (StdIOReadError{})) = True
isErrorTrace (MCPHttp (HTTPAuthFailure{})) = True
isErrorTrace (MCPHttp (HTTPOAuth (TraceAuthorizationDenied{}))) = True
isErrorTrace (MCPHttp (HTTPOAuth (TraceValidationError{}))) = True
isErrorTrace _ = False
