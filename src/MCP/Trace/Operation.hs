{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : MCP.Trace.Operation
Description : MCP operation tracing types
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC

MCP operation tracing types for structured logging of MCP-specific events
(tool calls, prompt operations, resource operations, completions).
-}
module MCP.Trace.Operation (
    OperationTrace (..),
    renderOperationTrace,
) where

import Data.Aeson (Value, encode)
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TEE

{- | MCP operation events.

Traces for MCP-specific operations with their arguments and results.
Keeps arguments/results as JSON values for simple serialization.
-}
data OperationTrace
    = -- | Tools list request
      OperationToolsList
    | -- | Tool call with arguments and result
      OperationToolCall
        { toolName :: Text
        , toolArguments :: Value
        , toolResult :: Value
        , toolDuration :: Maybe Double -- milliseconds
        , toolSuccess :: Bool
        }
    | -- | Prompts list request
      OperationPromptsList
    | -- | Prompt get operation
      OperationPromptGet
        { promptName :: Text
        , promptArguments :: Maybe Value
        }
    | -- | Resources list request
      OperationResourcesList
    | -- | Resource read operation
      OperationResourceRead
        { resourceUri :: Text
        }
    | -- | Resource templates list request
      OperationResourceTemplatesList
    | -- | Completion request
      OperationComplete
        { completeRefType :: Text -- "prompt" or "resource_template"
        , completeArgument :: Text
        }
    deriving (Show, Eq)

{- | Render an OperationTrace to human-readable text.

Produces structured log messages for MCP operations.
JSON values are compactly encoded.
-}
renderOperationTrace :: OperationTrace -> Text
renderOperationTrace OperationToolsList =
    "[MCP] tools/list request"
renderOperationTrace (OperationToolCall name args result mDuration success) =
    let argsJson = compactJson args
        resultJson = compactJson result
        statusText = if success then "" else " (error)"
        durationText = case mDuration of
            Nothing -> ""
            Just ms -> " (" <> T.pack (show (round ms :: Int)) <> "ms)"
     in "[MCP] tools/call: " <> name <> " " <> argsJson <> " -> " <> resultJson <> durationText <> statusText
renderOperationTrace OperationPromptsList =
    "[MCP] prompts/list request"
renderOperationTrace (OperationPromptGet name mArgs) =
    case mArgs of
        Nothing -> "[MCP] prompts/get: " <> name
        Just args -> "[MCP] prompts/get: " <> name <> " " <> compactJson args
renderOperationTrace OperationResourcesList =
    "[MCP] resources/list request"
renderOperationTrace (OperationResourceRead uri) =
    "[MCP] resources/read: " <> uri
renderOperationTrace OperationResourceTemplatesList =
    "[MCP] resources/templates/list request"
renderOperationTrace (OperationComplete refType arg) =
    "[MCP] completion/complete: " <> refType <> " argument=" <> arg

-- | Helper to compact JSON encoding
compactJson :: Value -> Text
compactJson v = TE.decodeUtf8With TEE.lenientDecode $ LBS.toStrict $ encode v
