{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

{- |
Module      : MCP.Protocol
Description : JSON-RPC protocol implementation for MCP version 2025-06-18
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC

This module implements the JSON-RPC 2.0 protocol layer for MCP version 2025-06-18,
including request/response handling, message parsing and encoding, and protocol-level
error handling. Supports all MCP operations including initialization, resources,
tools, prompts, completion, sampling, elicitation, and notifications.

New in 2025-06-18:
- Enhanced completion requests with context parameters
- Elicitation system for interactive user input
- Sampling message restrictions for LLM compatibility
- Comprehensive _meta field support throughout protocol messages
- Resource template references with correct schema compliance
-}
module MCP.Protocol (
    -- * JSON-RPC Types
    JSONRPCRequest (..),
    JSONRPCResponse (..),
    JSONRPCError (..),
    JSONRPCNotification (..),
    JSONRPCMessage (..),
    JSONRPCErrorInfo (..),
    CompletionContext (..),

    -- * Client Request Types
    InitializeRequest (..),
    InitializeParams (..),
    PingRequest (..),
    PingParams (..),
    ListResourcesRequest (..),
    ListResourcesParams (..),
    ListResourceTemplatesRequest (..),
    ListResourceTemplatesParams (..),
    ReadResourceRequest (..),
    ReadResourceParams (..),
    SubscribeRequest (..),
    SubscribeParams (..),
    UnsubscribeRequest (..),
    UnsubscribeParams (..),
    ListPromptsRequest (..),
    ListPromptsParams (..),
    GetPromptRequest (..),
    GetPromptParams (..),
    ListToolsRequest (..),
    ListToolsParams (..),
    CallToolRequest (..),
    CallToolParams (..),
    SetLevelRequest (..),
    SetLevelParams (..),
    CompleteRequest (..),
    CompleteParams (..),
    CompletionArgument (..),
    Reference (..),

    -- * Server Request Types
    CreateMessageRequest (..),
    CreateMessageParams (..),
    ListRootsRequest (..),
    ListRootsParams (..),
    ElicitRequest (..),
    ElicitParams (..),

    -- * Response Types
    InitializeResult (..),
    ListResourcesResult (..),
    ListResourceTemplatesResult (..),
    ReadResourceResult (..),
    ListPromptsResult (..),
    GetPromptResult (..),
    ListToolsResult (..),
    CallToolResult (..),
    CompleteResult (..),
    CompletionResult (..),
    CreateMessageResult (..),
    ListRootsResult (..),
    ElicitResult (..),

    -- * Schema Types
    PrimitiveSchemaDefinition (..),
    StringSchema (..),
    NumberSchema (..),
    BooleanSchema (..),
    EnumSchema (..),

    -- * Notification Types
    CancelledNotification (..),
    CancelledParams (..),
    InitializedNotification (..),
    InitializedParams (..),
    ProgressNotification (..),
    ProgressParams (..),
    ResourceListChangedNotification (..),
    ResourceUpdatedNotification (..),
    ResourceUpdatedParams (..),
    PromptListChangedNotification (..),
    ToolListChangedNotification (..),
    LoggingMessageNotification (..),
    LoggingMessageParams (..),
    RootsListChangedNotification (..),

    -- * Union Types
    ClientRequest (..),
    ServerRequest (..),
    ClientNotification (..),
    ServerNotification (..),
) where

import Control.Applicative ((<|>))
import Data.Aeson hiding (Object)
import Data.Aeson.TH
import Data.Aeson.Types (Object)
import Data.Default
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics

import MCP.Types

-- * JSON-RPC Types

-- | JSON-RPC error information
data JSONRPCErrorInfo = JSONRPCErrorInfo
    { code :: Int
    , message :: Text
    , errorData :: Maybe Value
    }
    deriving stock (Show, Eq, Generic)

instance ToJSON JSONRPCErrorInfo where
    toJSON (JSONRPCErrorInfo c m d) =
        object $
            [ "code" .= c
            , "message" .= m
            ]
                ++ maybe [] (\ed -> ["data" .= ed]) d

instance FromJSON JSONRPCErrorInfo where
    parseJSON = withObject "JSONRPCErrorInfo" $ \o ->
        JSONRPCErrorInfo <$> o .: "code" <*> o .: "message" <*> o .:? "data"

-- | A JSON-RPC request that expects a response
data JSONRPCRequest = JSONRPCRequest
    { jsonrpc :: Text -- Always "2.0"
    , id :: RequestId
    , method :: Text
    , params :: Maybe Value
    }
    deriving stock (Show, Eq, Generic)

$(deriveJSON defaultOptions{omitNothingFields = True} ''JSONRPCRequest)

-- | A successful JSON-RPC response
data JSONRPCResponse = JSONRPCResponse
    { jsonrpc :: Text -- Always "2.0"
    , id :: RequestId
    , result :: Value
    }
    deriving stock (Show, Eq, Generic)

$(deriveJSON defaultOptions ''JSONRPCResponse)

-- | A JSON-RPC error response
data JSONRPCError = JSONRPCError
    { jsonrpc :: Text -- Always "2.0"
    , id :: RequestId
    , error :: JSONRPCErrorInfo
    }
    deriving stock (Show, Eq, Generic)

$(deriveJSON defaultOptions ''JSONRPCError)

-- | A JSON-RPC notification (no response expected)
data JSONRPCNotification = JSONRPCNotification
    { jsonrpc :: Text -- Always "2.0"
    , method :: Text
    , params :: Maybe Value
    }
    deriving stock (Show, Eq, Generic)

$(deriveJSON defaultOptions{omitNothingFields = True} ''JSONRPCNotification)

-- | Any JSON-RPC message
data JSONRPCMessage
    = RequestMessage JSONRPCRequest
    | ResponseMessage JSONRPCResponse
    | ErrorMessage JSONRPCError
    | NotificationMessage JSONRPCNotification
    deriving stock (Show, Eq, Generic)

instance ToJSON JSONRPCMessage where
    toJSON (RequestMessage r) = toJSON r
    toJSON (ResponseMessage r) = toJSON r
    toJSON (ErrorMessage e) = toJSON e
    toJSON (NotificationMessage n) = toJSON n

instance FromJSON JSONRPCMessage where
    parseJSON v =
        (RequestMessage <$> parseJSON v)
            <|> (ResponseMessage <$> parseJSON v)
            <|> (ErrorMessage <$> parseJSON v)
            <|> (NotificationMessage <$> parseJSON v)

-- * Client Request Types

-- | Initialize request parameters
data InitializeParams = InitializeParams
    { protocolVersion :: Text
    , capabilities :: ClientCapabilities
    , clientInfo :: Implementation
    }
    deriving stock (Show, Eq, Generic)

$(deriveJSON defaultOptions ''InitializeParams)

-- | Initialize request
data InitializeRequest = InitializeRequest
    { method :: Text -- Always "initialize"
    , params :: InitializeParams
    }
    deriving stock (Show, Eq, Generic)

instance ToJSON InitializeRequest where
    toJSON (InitializeRequest _ p) =
        object
            [ "method" .= ("initialize" :: Text)
            , "params" .= p
            ]

instance FromJSON InitializeRequest where
    parseJSON = withObject "InitializeRequest" $ \o -> do
        m <- o .: "method"
        if m == ("initialize" :: Text)
            then InitializeRequest m <$> o .: "params"
            else fail "Expected method 'initialize'"

-- | Ping request parameters
data PingParams where
    PingParams :: {_meta :: Maybe Metadata} -> PingParams
    deriving stock (Show, Eq, Generic)

$(deriveJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = \case { "_meta" -> "_meta"; x -> x }} ''PingParams)

-- | Ping request
data PingRequest = PingRequest
    { method :: Text -- Always "ping"
    , params :: Maybe PingParams
    }
    deriving stock (Show, Eq, Generic)

instance ToJSON PingRequest where
    toJSON (PingRequest _ p) =
        object $
            ("method" .= ("ping" :: Text)) : maybe [] (\pr -> ["params" .= pr]) p

instance FromJSON PingRequest where
    parseJSON = withObject "PingRequest" $ \o -> do
        m <- o .: "method"
        if m == ("ping" :: Text)
            then PingRequest m <$> o .:? "params"
            else fail "Expected method 'ping'"

-- | List resources request parameters
data ListResourcesParams where
    ListResourcesParams ::
        {cursor :: Maybe Cursor} ->
        ListResourcesParams
    deriving stock (Show, Eq, Generic)

$(deriveJSON defaultOptions{omitNothingFields = True} ''ListResourcesParams)

-- | List resources request
data ListResourcesRequest = ListResourcesRequest
    { method :: Text -- Always "resources/list"
    , params :: Maybe ListResourcesParams
    }
    deriving stock (Show, Eq, Generic)

instance ToJSON ListResourcesRequest where
    toJSON (ListResourcesRequest _ p) =
        object $
            ("method" .= ("resources/list" :: Text)) : maybe [] (\pr -> ["params" .= pr]) p

instance FromJSON ListResourcesRequest where
    parseJSON = withObject "ListResourcesRequest" $ \o -> do
        m <- o .: "method"
        if m == ("resources/list" :: Text)
            then ListResourcesRequest m <$> o .:? "params"
            else fail "Expected method 'resources/list'"

-- | List resource templates request parameters
data ListResourceTemplatesParams where
    ListResourceTemplatesParams ::
        {cursor :: Maybe Cursor} ->
        ListResourceTemplatesParams
    deriving stock (Show, Eq, Generic)

$(deriveJSON defaultOptions{omitNothingFields = True} ''ListResourceTemplatesParams)

-- | List resource templates request
data ListResourceTemplatesRequest = ListResourceTemplatesRequest
    { method :: Text -- Always "resources/templates/list"
    , params :: Maybe ListResourceTemplatesParams
    }
    deriving stock (Show, Eq, Generic)

instance ToJSON ListResourceTemplatesRequest where
    toJSON (ListResourceTemplatesRequest _ p) =
        object $
            ("method" .= ("resources/templates/list" :: Text)) : maybe [] (\pr -> ["params" .= pr]) p

instance FromJSON ListResourceTemplatesRequest where
    parseJSON = withObject "ListResourceTemplatesRequest" $ \o -> do
        m <- o .: "method"
        if m == ("resources/templates/list" :: Text)
            then ListResourceTemplatesRequest m <$> o .:? "params"
            else fail "Expected method 'resources/templates/list'"

-- | Read resource request parameters
data ReadResourceParams where
    ReadResourceParams :: {uri :: Text} -> ReadResourceParams
    deriving stock (Show, Eq, Generic)

$(deriveJSON defaultOptions ''ReadResourceParams)

-- | Read resource request
data ReadResourceRequest = ReadResourceRequest
    { method :: Text -- Always "resources/read"
    , params :: ReadResourceParams
    }
    deriving stock (Show, Eq, Generic)

instance ToJSON ReadResourceRequest where
    toJSON (ReadResourceRequest _ p) =
        object
            [ "method" .= ("resources/read" :: Text)
            , "params" .= p
            ]

instance FromJSON ReadResourceRequest where
    parseJSON = withObject "ReadResourceRequest" $ \o -> do
        m <- o .: "method"
        if m == ("resources/read" :: Text)
            then ReadResourceRequest m <$> o .: "params"
            else fail "Expected method 'resources/read'"

-- | Subscribe request parameters
data SubscribeParams where
    SubscribeParams :: {uri :: Text} -> SubscribeParams
    deriving stock (Show, Eq, Generic)

$(deriveJSON defaultOptions ''SubscribeParams)

-- | Subscribe request
data SubscribeRequest = SubscribeRequest
    { method :: Text -- Always "resources/subscribe"
    , params :: SubscribeParams
    }
    deriving stock (Show, Eq, Generic)

instance ToJSON SubscribeRequest where
    toJSON (SubscribeRequest _ p) =
        object
            [ "method" .= ("resources/subscribe" :: Text)
            , "params" .= p
            ]

instance FromJSON SubscribeRequest where
    parseJSON = withObject "SubscribeRequest" $ \o -> do
        m <- o .: "method"
        if m == ("resources/subscribe" :: Text)
            then SubscribeRequest m <$> o .: "params"
            else fail "Expected method 'resources/subscribe'"

-- | Unsubscribe request parameters
data UnsubscribeParams where
    UnsubscribeParams :: {uri :: Text} -> UnsubscribeParams
    deriving stock (Show, Eq, Generic)

$(deriveJSON defaultOptions ''UnsubscribeParams)

-- | Unsubscribe request
data UnsubscribeRequest = UnsubscribeRequest
    { method :: Text -- Always "resources/unsubscribe"
    , params :: UnsubscribeParams
    }
    deriving stock (Show, Eq, Generic)

instance ToJSON UnsubscribeRequest where
    toJSON (UnsubscribeRequest _ p) =
        object
            [ "method" .= ("resources/unsubscribe" :: Text)
            , "params" .= p
            ]

instance FromJSON UnsubscribeRequest where
    parseJSON = withObject "UnsubscribeRequest" $ \o -> do
        m <- o .: "method"
        if m == ("resources/unsubscribe" :: Text)
            then UnsubscribeRequest m <$> o .: "params"
            else fail "Expected method 'resources/unsubscribe'"

-- | List prompts request parameters
data ListPromptsParams where
    ListPromptsParams :: {cursor :: Maybe Cursor} -> ListPromptsParams
    deriving stock (Show, Eq, Generic)

$(deriveJSON defaultOptions{omitNothingFields = True} ''ListPromptsParams)

-- | List prompts request
data ListPromptsRequest = ListPromptsRequest
    { method :: Text -- Always "prompts/list"
    , params :: Maybe ListPromptsParams
    }
    deriving stock (Show, Eq, Generic)

instance ToJSON ListPromptsRequest where
    toJSON (ListPromptsRequest _ p) =
        object $
            ("method" .= ("prompts/list" :: Text)) : maybe [] (\pr -> ["params" .= pr]) p

instance FromJSON ListPromptsRequest where
    parseJSON = withObject "ListPromptsRequest" $ \o -> do
        m <- o .: "method"
        if m == ("prompts/list" :: Text)
            then ListPromptsRequest m <$> o .:? "params"
            else fail "Expected method 'prompts/list'"

-- | Get prompt request parameters
data GetPromptParams = GetPromptParams
    { name :: Text
    , arguments :: Maybe (Map Text Text)
    }
    deriving stock (Show, Eq, Generic)

$(deriveJSON defaultOptions{omitNothingFields = True} ''GetPromptParams)

-- | Get prompt request
data GetPromptRequest = GetPromptRequest
    { method :: Text -- Always "prompts/get"
    , params :: GetPromptParams
    }
    deriving stock (Show, Eq, Generic)

instance ToJSON GetPromptRequest where
    toJSON (GetPromptRequest _ p) =
        object
            [ "method" .= ("prompts/get" :: Text)
            , "params" .= p
            ]

instance FromJSON GetPromptRequest where
    parseJSON = withObject "GetPromptRequest" $ \o -> do
        m <- o .: "method"
        if m == ("prompts/get" :: Text)
            then GetPromptRequest m <$> o .: "params"
            else fail "Expected method 'prompts/get'"

-- | List tools request parameters
data ListToolsParams where
    ListToolsParams :: {cursor :: Maybe Cursor} -> ListToolsParams
    deriving stock (Show, Eq, Generic)

$(deriveJSON defaultOptions{omitNothingFields = True} ''ListToolsParams)

-- | List tools request
data ListToolsRequest = ListToolsRequest
    { method :: Text -- Always "tools/list"
    , params :: Maybe ListToolsParams
    }
    deriving stock (Show, Eq, Generic)

instance ToJSON ListToolsRequest where
    toJSON (ListToolsRequest _ p) =
        object $
            ("method" .= ("tools/list" :: Text)) : maybe [] (\pr -> ["params" .= pr]) p

instance FromJSON ListToolsRequest where
    parseJSON = withObject "ListToolsRequest" $ \o -> do
        m <- o .: "method"
        if m == ("tools/list" :: Text)
            then ListToolsRequest m <$> o .:? "params"
            else fail "Expected method 'tools/list'"

-- | Call tool request parameters
data CallToolParams = CallToolParams
    { name :: Text
    , arguments :: Maybe (Map Text Value)
    }
    deriving stock (Show, Eq, Generic)

$(deriveJSON defaultOptions{omitNothingFields = True} ''CallToolParams)

-- | Call tool request
data CallToolRequest = CallToolRequest
    { method :: Text -- Always "tools/call"
    , params :: CallToolParams
    }
    deriving stock (Show, Eq, Generic)

instance ToJSON CallToolRequest where
    toJSON (CallToolRequest _ p) =
        object
            [ "method" .= ("tools/call" :: Text)
            , "params" .= p
            ]

instance FromJSON CallToolRequest where
    parseJSON = withObject "CallToolRequest" $ \o -> do
        m <- o .: "method"
        if m == ("tools/call" :: Text)
            then CallToolRequest m <$> o .: "params"
            else fail "Expected method 'tools/call'"

-- | Set level request parameters
data SetLevelParams where
    SetLevelParams :: {level :: LoggingLevel} -> SetLevelParams
    deriving stock (Show, Eq, Generic)

$(deriveJSON defaultOptions ''SetLevelParams)

-- | Set level request
data SetLevelRequest = SetLevelRequest
    { method :: Text -- Always "logging/setLevel"
    , params :: SetLevelParams
    }
    deriving stock (Show, Eq, Generic)

instance ToJSON SetLevelRequest where
    toJSON (SetLevelRequest _ p) =
        object
            [ "method" .= ("logging/setLevel" :: Text)
            , "params" .= p
            ]

instance FromJSON SetLevelRequest where
    parseJSON = withObject "SetLevelRequest" $ \o -> do
        m <- o .: "method"
        if m == ("logging/setLevel" :: Text)
            then SetLevelRequest m <$> o .: "params"
            else fail "Expected method 'logging/setLevel'"

-- | Completion argument
data CompletionArgument = CompletionArgument
    { name :: Text
    , value :: Text
    }
    deriving stock (Show, Eq, Generic)

$(deriveJSON defaultOptions ''CompletionArgument)

-- | Reference (prompt or resource template)
data Reference
    = PromptRef PromptReference
    | ResourceTemplateRef ResourceTemplateReference
    deriving stock (Show, Eq, Generic)

instance ToJSON Reference where
    toJSON (PromptRef p) = toJSON p
    toJSON (ResourceTemplateRef r) = toJSON r

instance FromJSON Reference where
    parseJSON v =
        (PromptRef <$> parseJSON v)
            <|> (ResourceTemplateRef <$> parseJSON v)

-- | Context for completion requests
data CompletionContext = CompletionContext
    { arguments :: Maybe (Map Text Text)
    }
    deriving stock (Show, Eq, Generic)

$(deriveJSON defaultOptions{omitNothingFields = True} ''CompletionContext)

-- | Parameters for a completion request
data CompleteParams = CompleteParams
    { ref :: Reference
    -- ^ Reference to prompt or resource template for completion
    , argument :: CompletionArgument
    -- ^ The argument to get completions for
    , context :: Maybe CompletionContext
    -- ^ Additional context for completion
    }
    deriving stock (Show, Eq, Generic)

$(deriveJSON defaultOptions{omitNothingFields = True} ''CompleteParams)

-- | Complete request
data CompleteRequest = CompleteRequest
    { method :: Text -- Always "completion/complete"
    , params :: CompleteParams
    }
    deriving stock (Show, Eq, Generic)

instance ToJSON CompleteRequest where
    toJSON (CompleteRequest _ p) =
        object
            [ "method" .= ("completion/complete" :: Text)
            , "params" .= p
            ]

instance FromJSON CompleteRequest where
    parseJSON = withObject "CompleteRequest" $ \o -> do
        m <- o .: "method"
        if m == ("completion/complete" :: Text)
            then CompleteRequest m <$> o .: "params"
            else fail "Expected method 'completion/complete'"

-- * Server Request Types

-- | Create message request parameters
data CreateMessageParams = CreateMessageParams
    { maxTokens :: Int
    , messages :: [SamplingMessage]
    , modelPreferences :: Maybe ModelPreferences
    , systemPrompt :: Maybe Text
    , includeContext :: Maybe IncludeContext
    , temperature :: Maybe Double
    , stopSequences :: Maybe [Text]
    , metadata :: Maybe (Map Text Value)
    }
    deriving stock (Show, Eq, Generic)

$(deriveJSON defaultOptions{omitNothingFields = True} ''CreateMessageParams)

-- | Create message request
data CreateMessageRequest = CreateMessageRequest
    { method :: Text -- Always "sampling/createMessage"
    , params :: CreateMessageParams
    }
    deriving stock (Show, Eq, Generic)

instance ToJSON CreateMessageRequest where
    toJSON (CreateMessageRequest _ p) =
        object
            [ "method" .= ("sampling/createMessage" :: Text)
            , "params" .= p
            ]

instance FromJSON CreateMessageRequest where
    parseJSON = withObject "CreateMessageRequest" $ \o -> do
        m <- o .: "method"
        if m == ("sampling/createMessage" :: Text)
            then CreateMessageRequest m <$> o .: "params"
            else fail "Expected method 'sampling/createMessage'"

-- | List roots request parameters
data ListRootsParams where
    ListRootsParams :: {_meta :: Maybe Metadata} -> ListRootsParams
    deriving stock (Show, Eq, Generic)

$(deriveJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = \case { "_meta" -> "_meta"; x -> x }} ''ListRootsParams)

-- | List roots request
data ListRootsRequest = ListRootsRequest
    { method :: Text -- Always "roots/list"
    , params :: Maybe ListRootsParams
    }
    deriving stock (Show, Eq, Generic)

instance ToJSON ListRootsRequest where
    toJSON (ListRootsRequest _ p) =
        object $
            ("method" .= ("roots/list" :: Text)) : maybe [] (\pr -> ["params" .= pr]) p

instance FromJSON ListRootsRequest where
    parseJSON = withObject "ListRootsRequest" $ \o -> do
        m <- o .: "method"
        if m == ("roots/list" :: Text)
            then ListRootsRequest m <$> o .:? "params"
            else fail "Expected method 'roots/list'"

-- * Response Types

-- | Initialize result
data InitializeResult = InitializeResult
    { protocolVersion :: Text
    , capabilities :: ServerCapabilities
    , serverInfo :: Implementation
    , instructions :: Maybe Text
    , _meta :: Maybe Metadata
    }
    deriving stock (Show, Eq, Generic)

$(deriveJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = \case { "_meta" -> "_meta"; x -> x }} ''InitializeResult)

-- | List resources result
data ListResourcesResult = ListResourcesResult
    { resources :: [Resource]
    , nextCursor :: Maybe Cursor
    , _meta :: Maybe Metadata
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (Default)

$(deriveJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = \case { "_meta" -> "_meta"; x -> x }} ''ListResourcesResult)

-- | List resource templates result
data ListResourceTemplatesResult = ListResourceTemplatesResult
    { resourceTemplates :: [ResourceTemplate]
    , nextCursor :: Maybe Cursor
    , _meta :: Maybe Metadata
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (Default)

$(deriveJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = \case { "_meta" -> "_meta"; x -> x }} ''ListResourceTemplatesResult)

-- | Read resource result
data ReadResourceResult = ReadResourceResult
    { contents :: [ResourceContents]
    , _meta :: Maybe Metadata
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (Default)

$(deriveJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = \case { "_meta" -> "_meta"; x -> x }} ''ReadResourceResult)

-- | List prompts result
data ListPromptsResult = ListPromptsResult
    { prompts :: [Prompt]
    , nextCursor :: Maybe Cursor
    , _meta :: Maybe Metadata
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (Default)

$(deriveJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = \case { "_meta" -> "_meta"; x -> x }} ''ListPromptsResult)

-- | Get prompt result
data GetPromptResult = GetPromptResult
    { description :: Maybe Text
    , messages :: [PromptMessage]
    , _meta :: Maybe Metadata
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (Default)

$(deriveJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = \case { "_meta" -> "_meta"; x -> x }} ''GetPromptResult)

-- | List tools result
data ListToolsResult = ListToolsResult
    { tools :: [Tool]
    , nextCursor :: Maybe Cursor
    , _meta :: Maybe Metadata
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (Default)

$(deriveJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = \case { "_meta" -> "_meta"; x -> x }} ''ListToolsResult)

-- | Call tool result
data CallToolResult = CallToolResult
    { content :: [ContentBlock]
    , structuredContent :: Maybe Value
    , isError :: Maybe Bool
    , _meta :: Maybe Metadata
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (Default)

$(deriveJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = \case { "_meta" -> "_meta"; x -> x }} ''CallToolResult)

-- | Completion result inner type
data CompletionResult = CompletionResult
    { values :: [Text]
    , total :: Maybe Int
    , hasMore :: Maybe Bool
    }
    deriving stock (Show, Eq, Generic)

instance Default CompletionResult where
    def = CompletionResult{values = [], total = Nothing, hasMore = Just True}

-- The "hasMore" field defaults to "Just True", indicating
-- that there are always more values to fetch. This is not the
-- derived default, so we need to provide the Default instance
-- for CompletionResult explicitly.

$(deriveJSON defaultOptions{omitNothingFields = True} ''CompletionResult)

-- | Complete result
data CompleteResult = CompleteResult
    { completion :: CompletionResult
    , _meta :: Maybe Metadata
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (Default)

$(deriveJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = \case { "_meta" -> "_meta"; x -> x }} ''CompleteResult)

-- | Create message result (extends SamplingMessage)
data CreateMessageResult = CreateMessageResult
    { role :: Role
    , content :: SamplingContent
    , model :: Text
    , stopReason :: Maybe Text
    , _meta :: Maybe Metadata
    }
    deriving stock (Show, Eq, Generic)

$(deriveJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = \case { "_meta" -> "_meta"; x -> x }} ''CreateMessageResult)

-- | List roots result
data ListRootsResult = ListRootsResult
    { roots :: [Root]
    , _meta :: Maybe Metadata
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (Default)

$(deriveJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = \case { "_meta" -> "_meta"; x -> x }} ''ListRootsResult)

-- * Elicitation Types

-- | Schema for string fields
data StringSchema = StringSchema
    { schemaType :: Text -- Always "string"
    , title :: Maybe Text
    , description :: Maybe Text
    , minLength :: Maybe Int
    , maxLength :: Maybe Int
    , format :: Maybe Text -- "email", "uri", "date", "date-time"
    }
    deriving stock (Show, Eq, Generic)

instance ToJSON StringSchema where
    toJSON (StringSchema _ t d minL maxL f) =
        object $
            ["type" .= ("string" :: Text)]
                ++ maybe [] (\x -> ["title" .= x]) t
                ++ maybe [] (\x -> ["description" .= x]) d
                ++ maybe [] (\x -> ["minLength" .= x]) minL
                ++ maybe [] (\x -> ["maxLength" .= x]) maxL
                ++ maybe [] (\x -> ["format" .= x]) f

instance FromJSON StringSchema where
    parseJSON = withObject "StringSchema" $ \o -> do
        ty <- o .: "type"
        if ty == ("string" :: Text)
            then
                StringSchema ty
                    <$> o .:? "title"
                    <*> o .:? "description"
                    <*> o .:? "minLength"
                    <*> o .:? "maxLength"
                    <*> o .:? "format"
            else fail "Expected type 'string'"

-- | Schema for number/integer fields
data NumberSchema = NumberSchema
    { schemaType :: Text -- "number" or "integer"
    , title :: Maybe Text
    , description :: Maybe Text
    , minimum :: Maybe Double
    , maximum :: Maybe Double
    }
    deriving stock (Show, Eq, Generic)

instance ToJSON NumberSchema where
    toJSON (NumberSchema ty t d minV maxV) =
        object $
            ["type" .= ty]
                ++ maybe [] (\x -> ["title" .= x]) t
                ++ maybe [] (\x -> ["description" .= x]) d
                ++ maybe [] (\x -> ["minimum" .= x]) minV
                ++ maybe [] (\x -> ["maximum" .= x]) maxV

instance FromJSON NumberSchema where
    parseJSON = withObject "NumberSchema" $ \o -> do
        ty <- o .: "type"
        if ty `elem` (["number", "integer"] :: [Text])
            then
                NumberSchema ty
                    <$> o .:? "title"
                    <*> o .:? "description"
                    <*> o .:? "minimum"
                    <*> o .:? "maximum"
            else fail "Expected type 'number' or 'integer'"

-- | Schema for boolean fields
data BooleanSchema = BooleanSchema
    { schemaType :: Text -- Always "boolean"
    , title :: Maybe Text
    , description :: Maybe Text
    , defaultValue :: Maybe Bool
    }
    deriving stock (Show, Eq, Generic)

instance ToJSON BooleanSchema where
    toJSON (BooleanSchema _ t d v0) =
        object $
            ["type" .= ("boolean" :: Text)]
                ++ maybe [] (\x -> ["title" .= x]) t
                ++ maybe [] (\x -> ["description" .= x]) d
                ++ maybe [] (\x -> ["default" .= x]) v0

instance FromJSON BooleanSchema where
    parseJSON = withObject "BooleanSchema" $ \o -> do
        ty <- o .: "type"
        if ty == ("boolean" :: Text)
            then BooleanSchema ty <$> o .:? "title" <*> o .:? "description" <*> o .:? "default"
            else fail "Expected type 'boolean'"

-- | Schema for enum fields
data EnumSchema = EnumSchema
    { schemaType :: Text -- Always "string"
    , title :: Maybe Text
    , description :: Maybe Text
    , enum :: [Text]
    , enumNames :: Maybe [Text] -- Display names for enum values
    }
    deriving stock (Show, Eq, Generic)

instance ToJSON EnumSchema where
    toJSON (EnumSchema _ t d e eNames) =
        object $
            [ "type" .= ("string" :: Text)
            , "enum" .= e
            ]
                ++ maybe [] (\x -> ["title" .= x]) t
                ++ maybe [] (\x -> ["description" .= x]) d
                ++ maybe [] (\x -> ["enumNames" .= x]) eNames

instance FromJSON EnumSchema where
    parseJSON = withObject "EnumSchema" $ \o -> do
        ty <- o .: "type"
        if ty == ("string" :: Text)
            then
                EnumSchema ty
                    <$> o .:? "title"
                    <*> o .:? "description"
                    <*> o .: "enum"
                    <*> o .:? "enumNames"
            else fail "Expected type 'string' with enum"

-- | Primitive schema definition union type
data PrimitiveSchemaDefinition
    = StringSchemaDef StringSchema
    | NumberSchemaDef NumberSchema
    | BooleanSchemaDef BooleanSchema
    | EnumSchemaDef EnumSchema
    deriving stock (Show, Eq, Generic)

instance ToJSON PrimitiveSchemaDefinition where
    toJSON (StringSchemaDef s) = toJSON s
    toJSON (NumberSchemaDef s) = toJSON s
    toJSON (BooleanSchemaDef s) = toJSON s
    toJSON (EnumSchemaDef s) = toJSON s

instance FromJSON PrimitiveSchemaDefinition where
    parseJSON v =
        (EnumSchemaDef <$> parseJSON v) -- Try enum first since it's also a string type
            <|> (StringSchemaDef <$> parseJSON v)
            <|> (NumberSchemaDef <$> parseJSON v)
            <|> (BooleanSchemaDef <$> parseJSON v)

-- | Elicit request parameters
data ElicitParams = ElicitParams
    { message :: Text
    , requestedSchema :: Object -- Restricted to top-level properties only
    }
    deriving stock (Show, Eq, Generic)

$(deriveJSON defaultOptions ''ElicitParams)

-- | Elicit request
data ElicitRequest = ElicitRequest
    { method :: Text -- Always "elicitation/create"
    , params :: ElicitParams
    }
    deriving stock (Show, Eq, Generic)

instance ToJSON ElicitRequest where
    toJSON (ElicitRequest _ p) =
        object
            [ "method" .= ("elicitation/create" :: Text)
            , "params" .= p
            ]

instance FromJSON ElicitRequest where
    parseJSON = withObject "ElicitRequest" $ \o -> do
        m <- o .: "method"
        if m == ("elicitation/create" :: Text)
            then ElicitRequest m <$> o .: "params"
            else fail "Expected method 'elicitation/create'"

-- | Elicit result
data ElicitResult = ElicitResult
    { action :: Text -- "accept", "decline", or "cancel"
    , content :: Maybe Value -- Present when action is "accept"
    , _meta :: Maybe Metadata
    }
    deriving stock (Show, Eq, Generic)

$(deriveJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = \case { "_meta" -> "_meta"; x -> x }} ''ElicitResult)

-- * Notification Types

-- | Cancelled notification parameters
data CancelledParams = CancelledParams
    { requestId :: RequestId
    , reason :: Maybe Text
    }
    deriving stock (Show, Eq, Generic)

$(deriveJSON defaultOptions{omitNothingFields = True} ''CancelledParams)

-- | Cancelled notification
data CancelledNotification = CancelledNotification
    { method :: Text -- Always "notifications/cancelled"
    , params :: CancelledParams
    }
    deriving stock (Show, Eq, Generic)

instance ToJSON CancelledNotification where
    toJSON (CancelledNotification _ p) =
        object
            [ "method" .= ("notifications/cancelled" :: Text)
            , "params" .= p
            ]

instance FromJSON CancelledNotification where
    parseJSON = withObject "CancelledNotification" $ \o -> do
        m <- o .: "method"
        if m == ("notifications/cancelled" :: Text)
            then CancelledNotification m <$> o .: "params"
            else fail "Expected method 'notifications/cancelled'"

-- | Initialized notification parameters
data InitializedParams where
    InitializedParams :: {_meta :: Maybe Metadata} -> InitializedParams
    deriving stock (Show, Eq, Generic)

$(deriveJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = \case { "_meta" -> "_meta"; x -> x }} ''InitializedParams)

-- | Initialized notification
data InitializedNotification = InitializedNotification
    { method :: Text -- Always "notifications/initialized"
    , params :: Maybe InitializedParams
    }
    deriving stock (Show, Eq, Generic)

instance ToJSON InitializedNotification where
    toJSON (InitializedNotification _ p) =
        object $
            ("method" .= ("notifications/initialized" :: Text)) : maybe [] (\pr -> ["params" .= pr]) p

instance FromJSON InitializedNotification where
    parseJSON = withObject "InitializedNotification" $ \o -> do
        m <- o .: "method"
        if m == ("notifications/initialized" :: Text)
            then InitializedNotification m <$> o .:? "params"
            else fail "Expected method 'notifications/initialized'"

-- | Progress notification parameters
data ProgressParams = ProgressParams
    { progressToken :: ProgressToken
    , progress :: Double
    , total :: Maybe Double
    , message :: Maybe Text
    }
    deriving stock (Show, Eq, Generic)

$(deriveJSON defaultOptions{omitNothingFields = True} ''ProgressParams)

-- | Progress notification
data ProgressNotification = ProgressNotification
    { method :: Text -- Always "notifications/progress"
    , params :: ProgressParams
    }
    deriving stock (Show, Eq, Generic)

instance ToJSON ProgressNotification where
    toJSON (ProgressNotification _ p) =
        object
            [ "method" .= ("notifications/progress" :: Text)
            , "params" .= p
            ]

instance FromJSON ProgressNotification where
    parseJSON = withObject "ProgressNotification" $ \o -> do
        m <- o .: "method"
        if m == ("notifications/progress" :: Text)
            then ProgressNotification m <$> o .: "params"
            else fail "Expected method 'notifications/progress'"

-- | Resource list changed notification
data ResourceListChangedNotification = ResourceListChangedNotification
    { method :: Text -- Always "notifications/resources/list_changed"
    , params :: Maybe InitializedParams
    }
    deriving stock (Show, Eq, Generic)

instance ToJSON ResourceListChangedNotification where
    toJSON (ResourceListChangedNotification _ p) =
        object $
            ("method" .= ("notifications/resources/list_changed" :: Text)) : maybe [] (\pr -> ["params" .= pr]) p

instance FromJSON ResourceListChangedNotification where
    parseJSON = withObject "ResourceListChangedNotification" $ \o -> do
        m <- o .: "method"
        if m == ("notifications/resources/list_changed" :: Text)
            then ResourceListChangedNotification m <$> o .:? "params"
            else fail "Expected method 'notifications/resources/list_changed'"

-- | Resource updated notification parameters
data ResourceUpdatedParams = ResourceUpdatedParams
    { uri :: Text
    }
    deriving stock (Show, Eq, Generic)

$(deriveJSON defaultOptions ''ResourceUpdatedParams)

-- | Resource updated notification
data ResourceUpdatedNotification = ResourceUpdatedNotification
    { method :: Text -- Always "notifications/resources/updated"
    , params :: ResourceUpdatedParams
    }
    deriving stock (Show, Eq, Generic)

instance ToJSON ResourceUpdatedNotification where
    toJSON (ResourceUpdatedNotification _ p) =
        object
            [ "method" .= ("notifications/resources/updated" :: Text)
            , "params" .= p
            ]

instance FromJSON ResourceUpdatedNotification where
    parseJSON = withObject "ResourceUpdatedNotification" $ \o -> do
        m <- o .: "method"
        if m == ("notifications/resources/updated" :: Text)
            then ResourceUpdatedNotification m <$> o .: "params"
            else fail "Expected method 'notifications/resources/updated'"

-- | Prompt list changed notification
data PromptListChangedNotification = PromptListChangedNotification
    { method :: Text -- Always "notifications/prompts/list_changed"
    , params :: Maybe InitializedParams
    }
    deriving stock (Show, Eq, Generic)

instance ToJSON PromptListChangedNotification where
    toJSON (PromptListChangedNotification _ p) =
        object $
            ("method" .= ("notifications/prompts/list_changed" :: Text)) : maybe [] (\pr -> ["params" .= pr]) p

instance FromJSON PromptListChangedNotification where
    parseJSON = withObject "PromptListChangedNotification" $ \o -> do
        m <- o .: "method"
        if m == ("notifications/prompts/list_changed" :: Text)
            then PromptListChangedNotification m <$> o .:? "params"
            else fail "Expected method 'notifications/prompts/list_changed'"

-- | Tool list changed notification
data ToolListChangedNotification = ToolListChangedNotification
    { method :: Text -- Always "notifications/tools/list_changed"
    , params :: Maybe InitializedParams
    }
    deriving stock (Show, Eq, Generic)

instance ToJSON ToolListChangedNotification where
    toJSON (ToolListChangedNotification _ p) =
        object $
            ("method" .= ("notifications/tools/list_changed" :: Text)) : maybe [] (\pr -> ["params" .= pr]) p

instance FromJSON ToolListChangedNotification where
    parseJSON = withObject "ToolListChangedNotification" $ \o -> do
        m <- o .: "method"
        if m == ("notifications/tools/list_changed" :: Text)
            then ToolListChangedNotification m <$> o .:? "params"
            else fail "Expected method 'notifications/tools/list_changed'"

-- | Logging message notification parameters
data LoggingMessageParams = LoggingMessageParams
    { level :: LoggingLevel
    , data' :: Value -- Can be any JSON value
    , logger :: Maybe Text
    }
    deriving stock (Show, Eq, Generic)

instance ToJSON LoggingMessageParams where
    toJSON (LoggingMessageParams lvl d lgr) =
        object $
            [ "level" .= lvl
            , "data" .= d
            ]
                ++ maybe [] (\l -> ["logger" .= l]) lgr

instance FromJSON LoggingMessageParams where
    parseJSON = withObject "LoggingMessageParams" $ \o ->
        LoggingMessageParams <$> o .: "level" <*> o .: "data" <*> o .:? "logger"

-- | Logging message notification
data LoggingMessageNotification = LoggingMessageNotification
    { method :: Text -- Always "notifications/message"
    , params :: LoggingMessageParams
    }
    deriving stock (Show, Eq, Generic)

instance ToJSON LoggingMessageNotification where
    toJSON (LoggingMessageNotification _ p) =
        object
            [ "method" .= ("notifications/message" :: Text)
            , "params" .= p
            ]

instance FromJSON LoggingMessageNotification where
    parseJSON = withObject "LoggingMessageNotification" $ \o -> do
        m <- o .: "method"
        if m == ("notifications/message" :: Text)
            then LoggingMessageNotification m <$> o .: "params"
            else fail "Expected method 'notifications/message'"

-- | Roots list changed notification
data RootsListChangedNotification = RootsListChangedNotification
    { method :: Text -- Always "notifications/roots/list_changed"
    , params :: Maybe InitializedParams
    }
    deriving stock (Show, Eq, Generic)

instance ToJSON RootsListChangedNotification where
    toJSON (RootsListChangedNotification _ p) =
        object $
            ("method" .= ("notifications/roots/list_changed" :: Text)) : maybe [] (\pr -> ["params" .= pr]) p

instance FromJSON RootsListChangedNotification where
    parseJSON = withObject "RootsListChangedNotification" $ \o -> do
        m <- o .: "method"
        if m == ("notifications/roots/list_changed" :: Text)
            then RootsListChangedNotification m <$> o .:? "params"
            else fail "Expected method 'notifications/roots/list_changed'"

-- * Union Types

-- | Any client request
data ClientRequest
    = InitializeReq InitializeRequest
    | PingReq PingRequest
    | ListResourcesReq ListResourcesRequest
    | ListResourceTemplatesReq ListResourceTemplatesRequest
    | ReadResourceReq ReadResourceRequest
    | SubscribeReq SubscribeRequest
    | UnsubscribeReq UnsubscribeRequest
    | ListPromptsReq ListPromptsRequest
    | GetPromptReq GetPromptRequest
    | ListToolsReq ListToolsRequest
    | CallToolReq CallToolRequest
    | SetLevelReq SetLevelRequest
    | CompleteReq CompleteRequest
    deriving stock (Show, Eq, Generic)

instance ToJSON ClientRequest where
    toJSON (InitializeReq r) = toJSON r
    toJSON (PingReq r) = toJSON r
    toJSON (ListResourcesReq r) = toJSON r
    toJSON (ListResourceTemplatesReq r) = toJSON r
    toJSON (ReadResourceReq r) = toJSON r
    toJSON (SubscribeReq r) = toJSON r
    toJSON (UnsubscribeReq r) = toJSON r
    toJSON (ListPromptsReq r) = toJSON r
    toJSON (GetPromptReq r) = toJSON r
    toJSON (ListToolsReq r) = toJSON r
    toJSON (CallToolReq r) = toJSON r
    toJSON (SetLevelReq r) = toJSON r
    toJSON (CompleteReq r) = toJSON r

instance FromJSON ClientRequest where
    parseJSON v =
        (InitializeReq <$> parseJSON v)
            <|> (PingReq <$> parseJSON v)
            <|> (ListResourcesReq <$> parseJSON v)
            <|> (ListResourceTemplatesReq <$> parseJSON v)
            <|> (ReadResourceReq <$> parseJSON v)
            <|> (SubscribeReq <$> parseJSON v)
            <|> (UnsubscribeReq <$> parseJSON v)
            <|> (ListPromptsReq <$> parseJSON v)
            <|> (GetPromptReq <$> parseJSON v)
            <|> (ListToolsReq <$> parseJSON v)
            <|> (CallToolReq <$> parseJSON v)
            <|> (SetLevelReq <$> parseJSON v)
            <|> (CompleteReq <$> parseJSON v)

-- | Any server request
data ServerRequest
    = PingServerReq PingRequest
    | CreateMessageReq CreateMessageRequest
    | ListRootsReq ListRootsRequest
    | ElicitReq ElicitRequest
    deriving stock (Show, Eq, Generic)

instance ToJSON ServerRequest where
    toJSON (PingServerReq r) = toJSON r
    toJSON (CreateMessageReq r) = toJSON r
    toJSON (ListRootsReq r) = toJSON r
    toJSON (ElicitReq r) = toJSON r

instance FromJSON ServerRequest where
    parseJSON v =
        (PingServerReq <$> parseJSON v)
            <|> (CreateMessageReq <$> parseJSON v)
            <|> (ListRootsReq <$> parseJSON v)
            <|> (ElicitReq <$> parseJSON v)

-- | Any client notification
data ClientNotification
    = CancelledNotif CancelledNotification
    | InitializedNotif InitializedNotification
    | ProgressNotif ProgressNotification
    | RootsListChangedNotif RootsListChangedNotification
    deriving stock (Show, Eq, Generic)

instance ToJSON ClientNotification where
    toJSON (CancelledNotif n) = toJSON n
    toJSON (InitializedNotif n) = toJSON n
    toJSON (ProgressNotif n) = toJSON n
    toJSON (RootsListChangedNotif n) = toJSON n

instance FromJSON ClientNotification where
    parseJSON v =
        (CancelledNotif <$> parseJSON v)
            <|> (InitializedNotif <$> parseJSON v)
            <|> (ProgressNotif <$> parseJSON v)
            <|> (RootsListChangedNotif <$> parseJSON v)

-- | Any server notification
data ServerNotification
    = CancelledServerNotif CancelledNotification
    | ProgressServerNotif ProgressNotification
    | ResourceListChangedNotif ResourceListChangedNotification
    | ResourceUpdatedNotif ResourceUpdatedNotification
    | PromptListChangedNotif PromptListChangedNotification
    | ToolListChangedNotif ToolListChangedNotification
    | LoggingMessageNotif LoggingMessageNotification
    deriving stock (Show, Eq, Generic)

instance ToJSON ServerNotification where
    toJSON (CancelledServerNotif n) = toJSON n
    toJSON (ProgressServerNotif n) = toJSON n
    toJSON (ResourceListChangedNotif n) = toJSON n
    toJSON (ResourceUpdatedNotif n) = toJSON n
    toJSON (PromptListChangedNotif n) = toJSON n
    toJSON (ToolListChangedNotif n) = toJSON n
    toJSON (LoggingMessageNotif n) = toJSON n

instance FromJSON ServerNotification where
    parseJSON v =
        (CancelledServerNotif <$> parseJSON v)
            <|> (ProgressServerNotif <$> parseJSON v)
            <|> (ResourceListChangedNotif <$> parseJSON v)
            <|> (ResourceUpdatedNotif <$> parseJSON v)
            <|> (PromptListChangedNotif <$> parseJSON v)
            <|> (ToolListChangedNotif <$> parseJSON v)
            <|> (LoggingMessageNotif <$> parseJSON v)
