{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : MCP.Trace.Server
Description : Server lifecycle tracing types
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC

Server tracing types for structured logging of server lifecycle and state changes.
-}
module MCP.Trace.Server (
    ServerTrace (..),
    renderServerTrace,
) where

import Data.Text (Text)
import Data.Text qualified as Text

-- | Server lifecycle and state change events.
data ServerTrace
    = -- | Server initialization event
      ServerInit
        { serverName :: Text
        , serverVersion :: Text
        }
    | -- | Server shutdown event
      ServerShutdown
    | ServerInitialized
        { clientInfo :: Maybe Text
        -- ^ Client name if provided
        }
    | -- \^ Server initialized with client connection

      -- | Capability negotiation completed
      ServerCapabilityNegotiation
        { negotiatedCapabilities :: [Text]
        }
    | -- | Server state transition
      ServerStateChange
        { fromState :: Text
        , toState :: Text
        }
    deriving (Show, Eq)

-- | Render a ServerTrace to human-readable text.
renderServerTrace :: ServerTrace -> Text
renderServerTrace (ServerInit name version) =
    "[Server] Init: " <> name <> " v" <> version
renderServerTrace ServerShutdown =
    "[Server] Shutdown"
renderServerTrace (ServerInitialized maybeClient) =
    case maybeClient of
        Just client -> "[Server] Initialized with client: " <> client
        Nothing -> "[Server] Initialized (no client info)"
renderServerTrace (ServerCapabilityNegotiation caps) =
    "[Server] Capabilities negotiated: [" <> Text.intercalate ", " caps <> "]"
renderServerTrace (ServerStateChange from to) =
    "[Server] State change: " <> from <> " -> " <> to
