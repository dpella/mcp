{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : MCP.Trace.Protocol
Description : JSON-RPC protocol handling tracing types
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC

Protocol tracing types for structured logging of JSON-RPC message handling.
-}
module MCP.Trace.Protocol (
    ProtocolTrace (..),
    renderProtocolTrace,
) where

import Data.Text (Text)
import Data.Text qualified as T

{- | JSON-RPC protocol handling events.

Leaf constructors representing specific protocol-level events during
JSON-RPC message handling (request/response/notification flow).
-}
data ProtocolTrace
    = ProtocolRequestReceived
        { requestId :: Text
        , method :: Text
        }
    | ProtocolResponseSent
        { requestId :: Text
        , isError :: Bool
        }
    | ProtocolNotificationReceived
        { method :: Text
        }
    | ProtocolParseError
        { errorMessage :: Text
        , rawInput :: Maybe Text -- Truncated for safety
        }
    | ProtocolMethodNotFound
        { requestId :: Text
        , method :: Text
        }
    | ProtocolInvalidParams
        { requestId :: Text
        , method :: Text
        , errorDetail :: Text
        }
    deriving (Show, Eq)

{- | Render a ProtocolTrace to human-readable text.

Produces structured log messages for protocol-level events.
-}
renderProtocolTrace :: ProtocolTrace -> Text
renderProtocolTrace (ProtocolRequestReceived reqId meth) =
    "[Protocol] Request received: method='" <> meth <> "', id='" <> reqId <> "'"
renderProtocolTrace (ProtocolResponseSent reqId err) =
    "[Protocol] Response sent: id='" <> reqId <> "', error=" <> T.pack (show err)
renderProtocolTrace (ProtocolNotificationReceived meth) =
    "[Protocol] Notification received: method='" <> meth <> "'"
renderProtocolTrace (ProtocolParseError errMsg maybeInput) =
    case maybeInput of
        Nothing -> "[Protocol] Parse error: " <> errMsg
        Just input -> "[Protocol] Parse error: " <> errMsg <> " (input: '" <> input <> "')"
renderProtocolTrace (ProtocolMethodNotFound reqId meth) =
    "[Protocol] Method not found: method='" <> meth <> "', id='" <> reqId <> "'"
renderProtocolTrace (ProtocolInvalidParams reqId meth detail) =
    "[Protocol] Invalid params: method='" <> meth <> "', id='" <> reqId <> "' (error: " <> detail <> ")"
