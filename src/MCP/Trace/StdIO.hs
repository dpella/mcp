{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : MCP.Trace.StdIO
Description : StdIO transport tracing types
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC

StdIO transport tracing types for structured logging of StdIO-specific events.
-}
module MCP.Trace.StdIO (
    StdIOTrace (..),
    renderStdIOTrace,
) where

import Data.Text (Text)
import Data.Text qualified
import MCP.Trace.Operation (OperationTrace, renderOperationTrace)
import MCP.Trace.Protocol (ProtocolTrace, renderProtocolTrace)
import MCP.Trace.Server (ServerTrace, renderServerTrace)

{- | StdIO transport-specific events.

Embeds ServerTrace and ProtocolTrace for events occurring in StdIO context.
-}
data StdIOTrace
    = -- | Nested server lifecycle events in StdIO context.
      StdIOServer ServerTrace
    | {- | Nested protocol events in StdIO context.
      This composite constructor is part of the skeleton structure.
      -}
      StdIOProtocol ProtocolTrace
    | -- | Nested MCP operation events in StdIO context.
      StdIOOperation OperationTrace
    | -- | Message received from stdin
      StdIOMessageReceived
        { messageSize :: Int
        -- ^ bytes
        }
    | -- | Message sent to stdout
      StdIOMessageSent
        { messageSize :: Int
        }
    | -- | Error reading from stdin
      StdIOReadError
        { stdioErrorMessage :: Text
        }
    | -- | End of file on stdin
      StdIOEOF
    deriving (Show, Eq)

{- | Render a StdIOTrace to human-readable text.

Delegates to renderServerTrace, renderProtocolTrace, and renderOperationTrace for nested events.
-}
renderStdIOTrace :: StdIOTrace -> Text
renderStdIOTrace (StdIOServer st) = "[StdIO:Server] " <> renderServerTrace st
renderStdIOTrace (StdIOProtocol pt) = "[StdIO:Protocol] " <> renderProtocolTrace pt
renderStdIOTrace (StdIOOperation ot) = "[StdIO] " <> renderOperationTrace ot
renderStdIOTrace (StdIOMessageReceived size) = "[StdIO] Message received (" <> showText size <> " bytes)"
renderStdIOTrace (StdIOMessageSent size) = "[StdIO] Message sent (" <> showText size <> " bytes)"
renderStdIOTrace (StdIOReadError errMsg) = "[StdIO] Read error: " <> errMsg
renderStdIOTrace StdIOEOF = "[StdIO] End of file"

-- | Helper to convert Int to Text
showText :: Int -> Text
showText = Data.Text.pack . show
