{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- |
-- Module      : MCP.Protocol
-- Description : JSON-RPC protocol implementation for MCP
-- Copyright   : (C) 2025 Matthias Pall Gissurarson
-- License     : MIT
-- Maintainer  : mpg@mpg.is
-- Stability   : experimental
-- Portability : GHC
--
-- This module implements the JSON-RPC 2.0 protocol layer for MCP,
-- including request/response handling, message parsing and encoding,
-- and protocol-level error handling.
module MCP.Protocol (
  -- * Type Classes
  IsJSONRPCRequest (..),
  IsJSONRPCNotification (..),

  -- * JSON-RPC Types
  JSONRPCRequest (..),
  JSONRPCResponse (..),
  JSONRPCError (..),
  JSONRPCNotification (..),
  JSONRPCMessage (..),
  JSONRPCErrorInfo (..),

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
  CompletionContext (..),
  CompletionArgument (..),
  Reference (..),

  -- * Server Request Types
  CreateMessageRequest (..),
  CreateMessageParams (..),
  ListRootsRequest (..),
  ListRootsParams (..),
  ElicitRequest (..),
  ElicitParams (..),
  PrimitiveSchemaDefinition (..),

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

  -- * Standard JSON-RPC error codes
  pARSE_ERROR,
  iNVALID_REQUEST,
  mETHOD_NOT_FOUND,
  iNVALID_PARAMS,
  iNTERNAL_ERROR,

  -- * Constants
  pROTOCOL_VERSION,
  rPC_VERSION,
) where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson qualified as Aeson
import Data.Aeson.TH
import Data.Data (Proxy (..), Typeable, typeRep)
import Data.Kind (Type)
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics
import GHC.Records (HasField (getField))
import GHC.TypeError (ErrorMessage (..), TypeError)
import MCP.Aeson
import MCP.Types

-- | JSON-RPC protocol version string.
--
-- Indicates the version of the JSON-RPC protocol being used.
rPC_VERSION :: Text
rPC_VERSION = "2.0"

-- | Type that represents an empty set of parameters.
data EmptyParams
  deriving stock (Generic)

instance Show EmptyParams where
  show :: EmptyParams -> String
  show _ = show Aeson.Null

instance Eq EmptyParams where
  (==) :: EmptyParams -> EmptyParams -> Bool
  _ == _ = True

instance ToJSON EmptyParams where
  toJSON :: EmptyParams -> Value
  toJSON _ = Aeson.Null

instance FromJSON EmptyParams where
  parseJSON _ = fail "EmptyParams can only be used as a placeholder for empty params"

-- * JSON-RPC Types

-- | JSON-RPC error information.
--
-- Contains the error details for a JSON-RPC error response.
data JSONRPCErrorInfo = JSONRPCErrorInfo
  { code :: Int
  -- ^ The error type that occurred.
  , message :: Text
  -- ^ A short description of the error. The message SHOULD be limited
  -- to a concise single sentence.
  , errorData :: Maybe Value
  -- ^ Additional information about the error. The value of this member
  -- is defined by the sender (e.g. detailed error information, nested errors etc.).
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | A JSON-RPC request that expects a response.
--
-- Represents a request message in the JSON-RPC 2.0 protocol.
data JSONRPCRequest = JSONRPCRequest
  { jsonrpc :: Text -- Always "2.0"
  , id :: RequestId
  , method :: Text
  , params :: Value
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Type family to extract the type of the 'params' field from a generic representation.
type family RequestParamType (rep :: Type -> Type) :: Type where
  RequestParamType (D1 _ (C1 _ (S1 _ (K1 _ RequestId) :*: S1 _ (K1 _ p)))) = p
  RequestParamType (D1 ('MetaData nm _ _ _) _) =
    TypeError
      ( ('Text "Error when defining IsJSONRPCRequest for " :<>: 'Text nm :<>: 'Text ":")
          :$$: 'Text "The request datatype must be a record with fields 'id' and 'params'"
      )

-- | A type class for types that can be converted to/from JSON-RPC requests.
class
  ( HasField "id" a RequestId
  , HasField "params" a (RequestParams a)
  , Typeable a
  , ToJSON (RequestParams a)
  , FromJSON (RequestParams a)
  ) =>
  IsJSONRPCRequest a
  where
  type RequestParams a
  type RequestParams a = RequestParamType (Rep a)
  requestMethod :: Proxy a -> Text

  toJSONRPCRequest :: a -> JSONRPCRequest
  toJSONRPCRequest (req :: a) =
    JSONRPCRequest rPC_VERSION (getField @"id" req) (requestMethod (Proxy @a)) (toJSON (getField @"params" req))

  fromJSONRPCRequest :: JSONRPCRequest -> Either String a
  default fromJSONRPCRequest
    :: ( Generic a
       , -- Here we're saying that the generic representation of 'a' must be a record
         -- with two fields: one for RequestId and one for RequestParams a
         Rep a ~ D1 c0 (C1 i0 (S1 s0 (K1 i1 RequestId) :*: S1 s1 (K1 i30 (RequestParams a))))
       )
    => JSONRPCRequest
    -> Either String a
  fromJSONRPCRequest JSONRPCRequest{id = req_id, params = req_params} =
    case fromJSON @(RequestParams a) req_params of
      -- If parsing the params succeeds, construct the record using the generic representation,
      -- i.e. use the constructor of 'a' with the parsed params and req_id
      Success p -> Right $ to $ M1 (M1 (M1 (K1 req_id) :*: M1 (K1 p)))
      Aeson.Error err -> Left err

newtype ViaJSONRPCRequest a = ViaJSONRPCRequest {unViaJSONRPCRequest :: a}

instance (IsJSONRPCRequest a) => ToJSON (ViaJSONRPCRequest a) where
  toJSON = toJSON . toJSONRPCRequest . unViaJSONRPCRequest

instance (IsJSONRPCRequest a) => FromJSON (ViaJSONRPCRequest a) where
  parseJSON = withObject (show $ typeRep (Proxy @a)) $ \o -> do
    m <- o .: "method"
    req_id <- o .: "id"
    if m == requestMethod (Proxy @a)
      then do
        p <- o .: "params"
        case fromJSONRPCRequest (JSONRPCRequest rPC_VERSION req_id m p) of
          Right r -> return (ViaJSONRPCRequest r)
          Left err -> fail $ "Failed to parse params for " <> show (typeRep (Proxy @a)) <> ": " <> err
      else fail $ "Expected method '" <> show (requestMethod (Proxy @a)) <> "'"

-- | A successful (non-error) response to a request.
--
-- Represents a successful response message in the JSON-RPC 2.0 protocol.
data JSONRPCResponse = JSONRPCResponse
  { jsonrpc :: Text -- Always "2.0"
  , id :: RequestId
  , result :: Value
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | A response to a request that indicates an error occurred.
--
-- Represents an error response message in the JSON-RPC 2.0 protocol.
data JSONRPCError = JSONRPCError
  { jsonrpc :: Text -- Always "2.0"
  , id :: RequestId
  , error :: JSONRPCErrorInfo
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | A notification which does not expect a response.
--
-- Represents a notification message in the JSON-RPC 2.0 protocol.
data JSONRPCNotification = JSONRPCNotification
  { jsonrpc :: Text -- Always "2.0"
  , method :: Text
  , params :: Value
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Type family to extract the type of the 'params' field from a generic representation.
type family NotificationParamType (rep :: Type -> Type) :: Type where
  NotificationParamType (D1 _ (C1 _ (S1 _ (K1 _ p)))) = p
  NotificationParamType (D1 ('MetaData nm _ _ _) _) =
    TypeError
      ( ('Text "Error when defining IsJSONRPCNotification for " :<>: 'Text nm :<>: 'Text ":")
          :$$: 'Text "The notification datatype must be a record with fields 'id' and 'params'"
      )

-- | A type class for types that can be converted to/from JSON-RPC requests.
class
  ( HasField "params" a (NotificationParams a)
  , Typeable a
  , ToJSON (NotificationParams a)
  , FromJSON (NotificationParams a)
  ) =>
  IsJSONRPCNotification a
  where
  type NotificationParams a
  type NotificationParams a = NotificationParamType (Rep a)
  notificationsMethod :: Proxy a -> Text

  toJSONRPCNotification :: a -> JSONRPCNotification
  toJSONRPCNotification (req :: a) =
    JSONRPCNotification rPC_VERSION (notificationsMethod (Proxy @a)) (toJSON (getField @"params" req))

  fromJSONRPCNotification :: JSONRPCNotification -> Either String a
  default fromJSONRPCNotification
    :: ( Generic a
       , -- Here we're saying that the generic representation of 'a' must be a record
         -- with one field, which is the NotificationParams a
         Rep a ~ D1 c0 (C1 i0 (S1 s1 (K1 i30 (NotificationParams a))))
       )
    => JSONRPCNotification
    -> Either String a
  fromJSONRPCNotification JSONRPCNotification{params = notification_params} =
    case fromJSON @(NotificationParams a) notification_params of
      -- If parsing the params succeeds, construct the record using the generic representation,
      -- i.e. use the constructor of 'a' with the parsed params
      Success p -> Right $ to $ M1 (M1 (M1 (K1 p)))
      Aeson.Error err -> Left err

newtype ViaJSONRPCNotification a = ViaJSONRPCNotification {unViaJSONRPCNotification :: a}

instance (IsJSONRPCNotification a) => ToJSON (ViaJSONRPCNotification a) where
  toJSON = toJSON . toJSONRPCNotification . unViaJSONRPCNotification

instance (IsJSONRPCNotification a) => FromJSON (ViaJSONRPCNotification a) where
  parseJSON = withObject (show $ typeRep (Proxy @a)) $ \o -> do
    m <- o .: "method"
    if m == notificationsMethod (Proxy @a)
      then do
        p <- o .: "params"
        case fromJSONRPCNotification (JSONRPCNotification rPC_VERSION m p) of
          Right r -> return (ViaJSONRPCNotification r)
          Left err -> fail $ "Failed to parse params for " <> show (typeRep (Proxy @a)) <> ": " <> err
      else fail $ "Expected method '" <> show (notificationsMethod (Proxy @a)) <> "'"

-- | Any valid JSON-RPC message that can be decoded off the wire, or encoded to be sent.
--
-- Refers to any valid JSON-RPC object that can be processed by the protocol layer.
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

-- | Initialize request parameters.
--
-- Parameters sent by the client during initialization.
data InitializeParams = InitializeParams
  { protocolVersion :: Text
  -- ^ The latest version of the Model Context Protocol that the client supports.
  -- The client MAY decide to support older versions as well.
  , capabilities :: ClientCapabilities
  , clientInfo :: Implementation
  }
  deriving stock (Show, Eq, Generic)

$(deriveJSON mcpParseOpts ''InitializeParams)

-- | This request is sent from the client to the server when it first connects,
-- asking it to begin initialization.
data InitializeRequest = InitializeRequest
  { id :: RequestId
  , params :: InitializeParams
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ViaJSONRPCRequest InitializeRequest

instance IsJSONRPCRequest InitializeRequest where
  requestMethod _ = "initialize"

-- | Ping request parameters.
--
-- Parameters for a ping request (typically just metadata).
data PingParams where
  PingParams :: {_meta :: Maybe Metadata} -> PingParams
  deriving stock (Show, Eq, Generic)

$(deriveJSON mcpParseOpts ''PingParams)

-- | A ping, issued by either the server or the client, to check that the other
-- party is still alive.
--
-- The receiver must promptly respond, or else may be disconnected.
data PingRequest = PingRequest
  { id :: RequestId
  , params :: Maybe PingParams
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ViaJSONRPCRequest PingRequest

instance IsJSONRPCRequest PingRequest where
  requestMethod _ = "ping"

-- | List resources request parameters.
--
-- Parameters for requesting a paginated list of resources.
data ListResourcesParams where
  ListResourcesParams
    :: { cursor :: Maybe Cursor
       -- ^ An opaque token representing the current pagination position.
       -- If provided, the server should return results starting after this cursor.
       }
    -> ListResourcesParams
  deriving stock (Show, Eq, Generic)

$(deriveJSON mcpParseOpts ''ListResourcesParams)

-- | Sent from the client to request a list of resources the server has.
data ListResourcesRequest = ListResourcesRequest
  { id :: RequestId
  , params :: Maybe ListResourcesParams
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ViaJSONRPCRequest ListResourcesRequest

instance IsJSONRPCRequest ListResourcesRequest where
  requestMethod _ = "resources/list"

-- | List resource templates request parameters.
--
-- Parameters for requesting a paginated list of resource templates.
data ListResourceTemplatesParams where
  ListResourceTemplatesParams
    :: { cursor :: Maybe Cursor
       -- ^ An opaque token representing the current pagination position.
       -- If provided, the server should return results starting after this cursor.
       }
    -> ListResourceTemplatesParams
  deriving stock (Show, Eq, Generic)

$(deriveJSON mcpParseOpts ''ListResourceTemplatesParams)

-- | Sent from the client to request a list of resource templates the server has.
data ListResourceTemplatesRequest = ListResourceTemplatesRequest
  { id :: RequestId
  , params :: Maybe ListResourceTemplatesParams
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ViaJSONRPCRequest ListResourceTemplatesRequest

instance IsJSONRPCRequest ListResourceTemplatesRequest where
  requestMethod _ = "resources/templates/list"

-- | Read resource request parameters.
--
-- Parameters for reading a specific resource.
data ReadResourceParams where
  ReadResourceParams
    :: { uri :: Text
       -- ^ The URI of the resource to read. The URI can use any protocol;
       -- it is up to the server how to interpret it.
       }
    -> ReadResourceParams
  deriving stock (Show, Eq, Generic)

$(deriveJSON mcpParseOpts ''ReadResourceParams)

-- | Sent from the client to the server, to read a specific resource URI.
data ReadResourceRequest = ReadResourceRequest
  { id :: RequestId
  , params :: ReadResourceParams
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ViaJSONRPCRequest ReadResourceRequest

instance IsJSONRPCRequest ReadResourceRequest where
  requestMethod _ = "resources/read"

-- | Subscribe request parameters.
--
-- Parameters for subscribing to resource updates.
data SubscribeParams where
  SubscribeParams
    :: { uri :: Text
       -- ^ The URI of the resource to subscribe to. The URI can use any protocol;
       -- it is up to the server how to interpret it.
       }
    -> SubscribeParams
  deriving stock (Show, Eq, Generic)

$(deriveJSON mcpParseOpts ''SubscribeParams)

-- | Sent from the client to request resources/updated notifications from the server
-- whenever a particular resource changes.
data SubscribeRequest = SubscribeRequest
  { id :: RequestId
  , params :: SubscribeParams
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ViaJSONRPCRequest SubscribeRequest

instance IsJSONRPCRequest SubscribeRequest where
  requestMethod _ = "resources/subscribe"

-- | Unsubscribe request parameters.
--
-- Parameters for unsubscribing from resource updates.
data UnsubscribeParams where
  UnsubscribeParams
    :: { uri :: Text
       -- ^ The URI of the resource to unsubscribe from.
       }
    -> UnsubscribeParams
  deriving stock (Show, Eq, Generic)

$(deriveJSON mcpParseOpts ''UnsubscribeParams)

-- | Sent from the client to request cancellation of resources/updated
-- notifications from the server. This should follow a previous resources/subscribe request.
data UnsubscribeRequest = UnsubscribeRequest
  { id :: RequestId
  , params :: UnsubscribeParams
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ViaJSONRPCRequest UnsubscribeRequest

instance IsJSONRPCRequest UnsubscribeRequest where
  requestMethod _ = "resources/unsubscribe"

-- | List prompts request parameters.
--
-- Parameters for requesting a paginated list of prompts.
data ListPromptsParams where
  ListPromptsParams
    :: { cursor :: Maybe Cursor
       -- ^ An opaque token representing the current pagination position.
       -- If provided, the server should return results starting after this cursor.
       }
    -> ListPromptsParams
  deriving stock (Show, Eq, Generic)

$(deriveJSON mcpParseOpts ''ListPromptsParams)

-- | Sent from the client to request a list of prompts and prompt templates the server has.
data ListPromptsRequest = ListPromptsRequest
  { id :: RequestId
  , params :: Maybe ListPromptsParams
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ViaJSONRPCRequest ListPromptsRequest

instance IsJSONRPCRequest ListPromptsRequest where
  requestMethod _ = "prompts/list"

-- | Get prompt request parameters.
--
-- Parameters for retrieving a specific prompt with optional arguments.
data GetPromptParams = GetPromptParams
  { name :: Text
  -- ^ The name of the prompt or prompt template.
  , arguments :: Maybe (Map Text Text)
  -- ^ Arguments to use for templating the prompt.
  }
  deriving stock (Show, Eq, Generic)

$(deriveJSON mcpParseOpts ''GetPromptParams)

-- | Used by the client to get a prompt provided by the server.
data GetPromptRequest = GetPromptRequest
  { id :: RequestId
  , params :: GetPromptParams
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ViaJSONRPCRequest GetPromptRequest

instance IsJSONRPCRequest GetPromptRequest where
  requestMethod _ = "prompts/get"

-- | List tools request parameters.
--
-- Parameters for requesting a paginated list of tools.
data ListToolsParams where
  ListToolsParams
    :: { cursor :: Maybe Cursor
       -- ^ An opaque token representing the current pagination position.
       -- If provided, the server should return results starting after this cursor.
       }
    -> ListToolsParams
  deriving stock (Show, Eq, Generic)

$(deriveJSON mcpParseOpts ''ListToolsParams)

-- | Sent from the client to request a list of tools the server has.
data ListToolsRequest = ListToolsRequest
  { id :: RequestId
  , params :: Maybe ListToolsParams
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ViaJSONRPCRequest ListToolsRequest

instance IsJSONRPCRequest ListToolsRequest where
  requestMethod _ = "tools/list"

-- | Call tool request parameters.
--
-- Parameters for invoking a specific tool with arguments.
data CallToolParams = CallToolParams
  { name :: Text
  , arguments :: Maybe (Map Text Value)
  }
  deriving stock (Show, Eq, Generic, Ord)

$(deriveJSON mcpParseOpts ''CallToolParams)

-- | Used by the client to invoke a tool provided by the server.
data CallToolRequest = CallToolRequest
  { id :: RequestId
  , params :: CallToolParams
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ViaJSONRPCRequest CallToolRequest

instance IsJSONRPCRequest CallToolRequest where
  requestMethod _ = "tools/call"

-- | Set level request parameters.
--
-- Parameters for setting the logging level.
data SetLevelParams where
  SetLevelParams
    :: { level :: LoggingLevel
       -- ^ The level of logging that the client wants to receive from the server.
       -- The server should send all logs at this level and higher (i.e., more severe)
       -- to the client as notifications/message.
       }
    -> SetLevelParams
  deriving stock (Show, Eq, Generic)

$(deriveJSON mcpParseOpts ''SetLevelParams)

-- | A request from the client to the server, to enable or adjust logging.
data SetLevelRequest = SetLevelRequest
  { id :: RequestId
  , params :: SetLevelParams
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ViaJSONRPCRequest SetLevelRequest

instance IsJSONRPCRequest SetLevelRequest where
  requestMethod _ = "logging/setLevel"

-- | Completion argument.
--
-- Represents an argument for completion requests.
data CompletionArgument = CompletionArgument
  { name :: Text
  -- ^ The name of the argument.
  , value :: Text
  -- ^ The value of the argument to use for completion matching.
  }
  deriving stock (Show, Eq, Generic)

$(deriveJSON mcpParseOpts ''CompletionArgument)

-- | Reference to either a prompt or resource template.
--
-- Used in completion requests to specify what is being completed.
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

$(deriveJSON mcpParseOpts ''CompletionContext)

-- | Complete request parameters.
--
-- Parameters for requesting completion options.
data CompleteParams = CompleteParams
  { ref :: Reference
  , argument :: CompletionArgument
  -- ^ The argument's information.
  , context :: Maybe CompletionContext
  -- ^ Additional, optional context for completions.
  -- Previously-resolved variables in a URI template or prompt.
  }
  deriving stock (Show, Eq, Generic)

$(deriveJSON mcpParseOpts ''CompleteParams)

-- | A request from the client to the server, to ask for completion options.
data CompleteRequest = CompleteRequest
  { id :: RequestId
  , params :: CompleteParams
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ViaJSONRPCRequest CompleteRequest

instance IsJSONRPCRequest CompleteRequest where
  requestMethod _ = "completion/complete"

-- * Server Request Types

-- | Create message request parameters.
--
-- Parameters for requesting LLM sampling from the client.
data CreateMessageParams = CreateMessageParams
  { maxTokens :: Int
  -- ^ The maximum number of tokens to sample, as requested by the server.
  -- The client MAY choose to sample fewer tokens than requested.
  , messages :: [SamplingMessage]
  , modelPreferences :: Maybe ModelPreferences
  -- ^ The server's preferences for which model to select. The client MAY ignore these preferences.
  , systemPrompt :: Maybe Text
  -- ^ An optional system prompt the server wants to use for sampling.
  -- The client MAY modify or omit this prompt.
  , includeContext :: Maybe IncludeContext
  -- ^ A request to include context from one or more MCP servers (including the caller),
  -- to be attached to the prompt. The client MAY ignore this request.
  , temperature :: Maybe Double
  , stopSequences :: Maybe [Text]
  , metadata :: Maybe (Map Text Value)
  -- ^ Optional metadata to pass through to the LLM provider.
  -- The format of this metadata is provider-specific.
  }
  deriving stock (Show, Eq, Generic)

$(deriveJSON mcpParseOpts ''CreateMessageParams)

-- | A request from the server to sample an LLM via the client.
--
-- The client has full discretion over which model to select. The client should also
-- inform the user before beginning sampling, to allow them to inspect the request
-- (human in the loop) and decide whether to approve it.
data CreateMessageRequest = CreateMessageRequest
  { id :: RequestId
  , params :: CreateMessageParams
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ViaJSONRPCRequest CreateMessageRequest

instance IsJSONRPCRequest CreateMessageRequest where
  requestMethod _ = "sampling/createMessage"

-- | List roots request parameters.
--
-- Parameters for requesting the list of root directories or files.
data ListRootsParams where
  ListRootsParams :: {_meta :: Maybe Metadata} -> ListRootsParams
  deriving stock (Show, Eq, Generic)

$(deriveJSON mcpParseOpts ''ListRootsParams)

-- | Sent from the server to request a list of root URIs from the client.
--
-- Roots allow servers to ask for specific directories or files to operate on.
-- A common example for roots is providing a set of repositories or directories
-- a server should operate on.
--
-- This request is typically used when the server needs to understand the file system
-- structure or access specific locations that the client has permission to read from.
data ListRootsRequest = ListRootsRequest
  { id :: RequestId
  , params :: Maybe ListRootsParams
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ViaJSONRPCRequest ListRootsRequest

instance IsJSONRPCRequest ListRootsRequest where
  requestMethod _ = "roots/list"

-- * Response Types

-- | After receiving an initialize request from the client, the server sends this response.
data InitializeResult = InitializeResult
  { protocolVersion :: Text
  -- ^ The version of the Model Context Protocol that the server wants to use.
  -- This may not match the version that the client requested. If the client
  -- cannot support this version, it MUST disconnect.
  , capabilities :: ServerCapabilities
  , serverInfo :: Implementation
  , instructions :: Maybe Text
  -- ^ Instructions describing how to use the server and its features.
  -- This can be used by clients to improve the LLM's understanding of available
  -- tools, resources, etc. It can be thought of like a "hint" to the model.
  -- For example, this information MAY be added to the system prompt.
  , _meta :: Maybe Metadata
  }
  deriving stock (Show, Eq, Generic)

$(deriveJSON mcpParseOpts ''InitializeResult)

-- | The server's response to a resources/list request from the client.
data ListResourcesResult = ListResourcesResult
  { resources :: [Resource]
  , nextCursor :: Maybe Cursor
  -- ^ An opaque token representing the pagination position after the last returned result.
  -- If present, there may be more results available.
  , _meta :: Maybe Metadata
  }
  deriving stock (Show, Eq, Generic)

$(deriveJSON mcpParseOpts ''ListResourcesResult)

-- | The server's response to a resources/templates/list request from the client.
data ListResourceTemplatesResult = ListResourceTemplatesResult
  { resourceTemplates :: [ResourceTemplate]
  , nextCursor :: Maybe Cursor
  -- ^ An opaque token representing the pagination position after the last returned result.
  -- If present, there may be more results available.
  , _meta :: Maybe Metadata
  }
  deriving stock (Show, Eq, Generic)

$(deriveJSON mcpParseOpts ''ListResourceTemplatesResult)

-- | The server's response to a resources/read request from the client.
data ReadResourceResult = ReadResourceResult
  { contents :: [ResourceContents]
  , _meta :: Maybe Metadata
  }
  deriving stock (Show, Eq, Generic)

$(deriveJSON mcpParseOpts ''ReadResourceResult)

-- | The server's response to a prompts/list request from the client.
data ListPromptsResult = ListPromptsResult
  { prompts :: [Prompt]
  , nextCursor :: Maybe Cursor
  -- ^ An opaque token representing the pagination position after the last returned result.
  -- If present, there may be more results available.
  , _meta :: Maybe Metadata
  }
  deriving stock (Show, Eq, Generic)

$(deriveJSON mcpParseOpts ''ListPromptsResult)

-- | The server's response to a prompts/get request from the client.
data GetPromptResult = GetPromptResult
  { description :: Maybe Text
  -- ^ An optional description for the prompt.
  , messages :: [PromptMessage]
  , _meta :: Maybe Metadata
  }
  deriving stock (Show, Eq, Generic)

$(deriveJSON mcpParseOpts ''GetPromptResult)

-- | The server's response to a tools/list request from the client.
data ListToolsResult = ListToolsResult
  { tools :: [Tool]
  , nextCursor :: Maybe Cursor
  -- ^ An opaque token representing the pagination position after the last returned result.
  -- If present, there may be more results available.
  , _meta :: Maybe Metadata
  }
  deriving stock (Show, Eq, Generic)

$(deriveJSON mcpParseOpts ''ListToolsResult)

-- | The server's response to a tool call.
--
-- Any errors that originate from the tool SHOULD be reported inside the result
-- object, with `isError` set to true, _not_ as an MCP protocol-level error
-- response. Otherwise, the LLM would not be able to see that an error occurred
-- and self-correct.
--
-- However, any errors in _finding_ the tool, an error indicating that the
-- server does not support tool calls, or any other exceptional conditions,
-- should be reported as an MCP error response.
data CallToolResult = CallToolResult
  { content :: [ContentBlock]
  -- ^ A list of content objects that represent the unstructured result of the tool call.
  , structuredContent :: Maybe (Map Text Value)
  -- ^ An optional JSON object that represents the structured result of the tool call.
  , isError :: Maybe Bool
  -- ^ Whether the tool call ended in an error.
  -- If not set, this is assumed to be false (the call was successful).
  , _meta :: Maybe Metadata
  }
  deriving stock (Show, Eq, Generic)

$(deriveJSON mcpParseOpts ''CallToolResult)

-- | Completion result inner type.
--
-- Contains the actual completion data.
data CompletionResult = CompletionResult
  { values :: [Text]
  -- ^ An array of completion values. Must not exceed 100 items.
  , total :: Maybe Int
  -- ^ The total number of completion options available.
  -- This can exceed the number of values actually sent in the response.
  , hasMore :: Maybe Bool
  -- ^ Indicates whether there are additional completion options beyond those
  -- provided in the current response, even if the exact total is unknown.
  }
  deriving stock (Show, Eq, Generic)

$(deriveJSON mcpParseOpts ''CompletionResult)

-- | The server's response to a completion/complete request.
data CompleteResult = CompleteResult
  { completion :: CompletionResult
  , _meta :: Maybe Metadata
  }
  deriving stock (Show, Eq, Generic)

$(deriveJSON mcpParseOpts ''CompleteResult)

-- | The client's response to a sampling/createMessage request from the server.
--
-- The client should inform the user before returning the sampled message, to allow them
-- to inspect the response (human in the loop) and decide whether to allow the server to see it.
data CreateMessageResult = CreateMessageResult
  { role :: Role
  , content :: Content -- This stays as Content for sampling, not ContentBlock
  , model :: Text
  -- ^ The name of the model that generated the message.
  , stopReason :: Maybe Text
  -- ^ The reason why sampling stopped, if known.
  , _meta :: Maybe Metadata
  }
  deriving stock (Show, Eq, Generic)

$(deriveJSON mcpParseOpts ''CreateMessageResult)

-- | The client's response to a roots/list request from the server.
--
-- This result contains an array of Root objects, each representing a root directory
-- or file that the server can operate on.
data ListRootsResult = ListRootsResult
  { roots :: [Root]
  , _meta :: Maybe Metadata
  }
  deriving stock (Show, Eq, Generic)

$(deriveJSON mcpParseOpts ''ListRootsResult)

-- * Notification Types

-- | Cancelled notification parameters.
--
-- Parameters for a cancellation notification.
data CancelledParams = CancelledParams
  { requestId :: RequestId
  -- ^ The ID of the request to cancel.
  -- This MUST correspond to the ID of a request previously issued in the same direction.
  , reason :: Maybe Text
  -- ^ An optional string describing the reason for the cancellation.
  -- This MAY be logged or presented to the user.
  }
  deriving stock (Show, Eq, Generic)

$(deriveJSON mcpParseOpts ''CancelledParams)

-- | This notification can be sent by either side to indicate that it is cancelling
-- a previously-issued request.
--
-- The request SHOULD still be in-flight, but due to communication latency, it is always
-- possible that this notification MAY arrive after the request has already finished.
--
-- This notification indicates that the result will be unused, so any associated
-- processing SHOULD cease.
--
-- A client MUST NOT attempt to cancel its `initialize` request.
data CancelledNotification = CancelledNotification
  { params :: CancelledParams
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ViaJSONRPCNotification CancelledNotification

instance IsJSONRPCNotification CancelledNotification where
  notificationsMethod _ = "notifications/cancelled"

-- | Initialized notification parameters.
--
-- Parameters for the initialized notification (typically just metadata).
data InitializedParams where
  InitializedParams :: {_meta :: Maybe Metadata} -> InitializedParams
  deriving stock (Show, Eq, Generic)

$(deriveJSON mcpParseOpts ''InitializedParams)

-- | This notification is sent from the client to the server after initialization has finished.
data InitializedNotification = InitializedNotification
  { params :: Maybe EmptyParams
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ViaJSONRPCNotification InitializedNotification

instance IsJSONRPCNotification InitializedNotification where
  notificationsMethod _ = "notifications/initialized"

-- | Progress notification parameters.
--
-- Parameters for progress update notifications.
data ProgressParams = ProgressParams
  { progressToken :: ProgressToken
  -- ^ The progress token which was given in the initial request, used to associate
  -- this notification with the request that is proceeding.
  , progress :: Double
  -- ^ The progress thus far. This should increase every time progress is made,
  -- even if the total is unknown.
  , total :: Maybe Double
  -- ^ Total number of items to process (or total progress required), if known.
  , message :: Maybe Text
  -- ^ An optional message describing the current progress.
  }
  deriving stock (Show, Eq, Generic)

$(deriveJSON mcpParseOpts ''ProgressParams)

-- | An out-of-band notification used to inform the receiver of a progress update
-- for a long-running request.
data ProgressNotification = ProgressNotification
  { params :: ProgressParams
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ViaJSONRPCNotification ProgressNotification

instance IsJSONRPCNotification ProgressNotification where
  notificationsMethod _ = "notifications/progress"

-- | An optional notification from the server to the client, informing it that the list
-- of resources it can read from has changed.
--
-- This may be issued by servers without any previous subscription from the client.
data ResourceListChangedNotification = ResourceListChangedNotification
  { params :: Maybe EmptyParams
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ViaJSONRPCNotification ResourceListChangedNotification

instance IsJSONRPCNotification ResourceListChangedNotification where
  notificationsMethod _ = "notifications/resources/list_changed"

-- | Resource updated notification parameters.
--
-- Parameters for resource update notifications.
data ResourceUpdatedParams = ResourceUpdatedParams
  { uri :: Text
  -- ^ The URI of the resource that has been updated. This might be a sub-resource
  -- of the one that the client actually subscribed to.
  }
  deriving stock (Show, Eq, Generic)

$(deriveJSON mcpParseOpts ''ResourceUpdatedParams)

-- | A notification from the server to the client, informing it that a resource
-- has changed and may need to be read again.
--
-- This should only be sent if the client previously sent a resources/subscribe request.
data ResourceUpdatedNotification = ResourceUpdatedNotification
  { params :: ResourceUpdatedParams
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ViaJSONRPCNotification ResourceUpdatedNotification

instance IsJSONRPCNotification ResourceUpdatedNotification where
  notificationsMethod _ = "notifications/resources/updated"

-- | An optional notification from the server to the client, informing it that the list
-- of prompts it offers has changed.
--
-- This may be issued by servers without any previous subscription from the client.
data PromptListChangedNotification = PromptListChangedNotification
  { params :: Maybe EmptyParams -- this should probably not be initialized params
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ViaJSONRPCNotification PromptListChangedNotification

instance IsJSONRPCNotification PromptListChangedNotification where
  notificationsMethod _ = "notifications/prompts/list_changed"

-- | An optional notification from the server to the client, informing it that the list
-- of tools it offers has changed.
--
-- This may be issued by servers without any previous subscription from the client.
data ToolListChangedNotification = ToolListChangedNotification
  { params :: Maybe EmptyParams
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ViaJSONRPCNotification ToolListChangedNotification

instance IsJSONRPCNotification ToolListChangedNotification where
  notificationsMethod _ = "notifications/tools/list_changed"

-- | Logging message notification parameters.
--
-- Parameters for log message notifications.
data LoggingMessageParams = LoggingMessageParams
  { level :: LoggingLevel
  -- ^ The severity of this log message.
  , data' :: Value
  -- ^ The data to be logged, such as a string message or an object.
  -- Any JSON serializable type is allowed here.
  , logger :: Maybe Text
  -- ^ An optional name of the logger issuing this message.
  }
  deriving stock (Show, Eq, Generic)

$(deriveJSON mcpParseOpts ''LoggingMessageParams)

-- | Notification of a log message passed from server to client.
--
-- If no logging/setLevel request has been sent from the client, the server MAY decide
-- which messages to send automatically.
data LoggingMessageNotification = LoggingMessageNotification
  { params :: LoggingMessageParams
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ViaJSONRPCNotification LoggingMessageNotification

instance IsJSONRPCNotification LoggingMessageNotification where
  notificationsMethod _ = "notifications/message"

-- | A notification from the client to the server, informing it that the list of roots has changed.
--
-- This notification should be sent whenever the client adds, removes, or modifies any root.
-- The server should then request an updated list of roots using the ListRootsRequest.
data RootsListChangedNotification = RootsListChangedNotification
  { params :: Maybe EmptyParams
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ViaJSONRPCNotification RootsListChangedNotification

instance IsJSONRPCNotification RootsListChangedNotification where
  notificationsMethod _ = "notifications/roots/list_changed"

-- * Union Types

-- | Any request that can be sent from a client to a server.
--
-- This union type encompasses all possible client-to-server requests in the MCP protocol.
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

-- | Any request that can be sent from a server to a client.
--
-- This union type encompasses all possible server-to-client requests in the MCP protocol.
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

-- | Any notification that can be sent from a client to a server.
--
-- This union type encompasses all possible client-to-server notifications in the MCP protocol.
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

-- | Any notification that can be sent from a server to a client.
--
-- This union type encompasses all possible server-to-client notifications in the MCP protocol.
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

-- | JSON-RPC parse error.
--
-- Invalid JSON was received by the server.
-- An error occurred on the server while parsing the JSON text.
pARSE_ERROR :: Int
pARSE_ERROR = -32700

-- | JSON-RPC invalid request error.
--
-- The JSON sent is not a valid Request object.
iNVALID_REQUEST :: Int
iNVALID_REQUEST = -32600

-- | JSON-RPC method not found error.
--
-- The method does not exist or is not available.
mETHOD_NOT_FOUND :: Int
mETHOD_NOT_FOUND = -32601

-- | JSON-RPC invalid params error.
--
-- Invalid method parameter(s).
iNVALID_PARAMS :: Int
iNVALID_PARAMS = -32602

-- | JSON-RPC internal error.
--
-- Internal JSON-RPC error.
iNTERNAL_ERROR :: Int
iNTERNAL_ERROR = -32603

-- | MCP protocol version string.
--
-- Indicates the version of the Model Context Protocol
-- that this implementation supports.
pROTOCOL_VERSION :: Text
pROTOCOL_VERSION = "2025-06-18"

-- | Primitive schema definitions that only allow primitive types without nested objects or arrays.
--
-- Restricted schema definitions for elicitation requests.
data PrimitiveSchemaDefinition
  = StringSchema
      { schemaType :: Text -- Always "string"
      , title :: Maybe Text
      , description :: Maybe Text
      , minLength :: Maybe Int
      , maxLength :: Maybe Int
      , format :: Maybe Text -- "email" | "uri" | "date" | "date-time"
      }
  | NumberSchema
      { schemaType :: Text -- "number" or "integer"
      , title :: Maybe Text
      , description :: Maybe Text
      , minimum :: Maybe Double
      , maximum :: Maybe Double
      }
  | BooleanSchema
      { schemaType :: Text -- Always "boolean"
      , title :: Maybe Text
      , description :: Maybe Text
      , defaultValue :: Maybe Bool
      }
  | EnumSchema
      { schemaType :: Text -- Always "string"
      , title :: Maybe Text
      , description :: Maybe Text
      , enum :: [Text]
      , enumNames :: Maybe [Text] -- Display names for enum values
      }
  deriving stock (Show, Eq, Generic)

instance ToJSON PrimitiveSchemaDefinition where
  toJSON (StringSchema ty ttl desc minL maxL fmt) =
    object $
      ["type" .= ty]
        <> maybe [] (\t -> ["title" .= t]) ttl
        <> maybe [] (\d -> ["description" .= d]) desc
        <> maybe [] (\ml -> ["minLength" .= ml]) minL
        <> maybe [] (\mx -> ["maxLength" .= mx]) maxL
        <> maybe [] (\f -> ["format" .= f]) fmt
  toJSON (NumberSchema ty ttl desc minV maxV) =
    object $
      ["type" .= ty]
        <> maybe [] (\t -> ["title" .= t]) ttl
        <> maybe [] (\d -> ["description" .= d]) desc
        <> maybe [] (\mn -> ["minimum" .= mn]) minV
        <> maybe [] (\mx -> ["maximum" .= mx]) maxV
  toJSON (BooleanSchema ty ttl desc defVal) =
    object $
      ["type" .= ty]
        <> maybe [] (\t -> ["title" .= t]) ttl
        <> maybe [] (\d -> ["description" .= d]) desc
        <> maybe [] (\dv -> ["default" .= dv]) defVal
  toJSON (EnumSchema ty ttl desc vals names) =
    object $
      [ "type" .= ty
      , "enum" .= vals
      ]
        <> maybe [] (\t -> ["title" .= t]) ttl
        <> maybe [] (\d -> ["description" .= d]) desc
        <> maybe [] (\en -> ["enumNames" .= en]) names

instance FromJSON PrimitiveSchemaDefinition where
  parseJSON = withObject "PrimitiveSchemaDefinition" $ \o -> do
    ty <- o .: "type"
    case ty of
      "string" -> do
        enums <- o .:? "enum"
        case enums of
          Just vals ->
            EnumSchema ty
              <$> o .:? "title"
              <*> o .:? "description"
              <*> pure vals
              <*> o .:? "enumNames"
          Nothing ->
            StringSchema ty
              <$> o .:? "title"
              <*> o .:? "description"
              <*> o .:? "minLength"
              <*> o .:? "maxLength"
              <*> o .:? "format"
      "number" ->
        NumberSchema ty
          <$> o .:? "title"
          <*> o .:? "description"
          <*> o .:? "minimum"
          <*> o .:? "maximum"
      "integer" ->
        NumberSchema ty
          <$> o .:? "title"
          <*> o .:? "description"
          <*> o .:? "minimum"
          <*> o .:? "maximum"
      "boolean" ->
        BooleanSchema ty
          <$> o .:? "title"
          <*> o .:? "description"
          <*> o .:? "default"
      _ -> fail $ "Unknown schema type: " <> show ty

-- | Elicit request parameters.
--
-- Parameters for requesting additional information from the user via the client.
data ElicitParams = ElicitParams
  { message :: Text
  -- ^ The message to present to the user.
  , requestedSchema :: Map Text PrimitiveSchemaDefinition
  -- ^ A restricted subset of JSON Schema. Only top-level properties are allowed,
  -- without nesting. The schema type is always "object" with properties.
  , requiredFields :: Maybe [Text]
  -- ^ Required fields in the schema.
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON ElicitParams where
  toJSON (ElicitParams msg schema reqFields) =
    object $
      [ "message" .= msg
      , "requestedSchema"
          .= object
            ( [ "type" .= ("object" :: Text)
              , "properties" .= schema
              ]
                <> maybe [] (\rf -> ["required" .= rf]) reqFields
            )
      ]

instance FromJSON ElicitParams where
  parseJSON = withObject "ElicitParams" $ \o -> do
    msg <- o .: "message"
    schema <- o .: "requestedSchema"
    schemaProps <-
      withObject
        "requestedSchema"
        ( \so -> do
            ty <- so .: "type"
            if ty == ("object" :: Text)
              then do
                props <- so .: "properties"
                reqFields <- so .:? "required"
                pure (props, reqFields)
              else fail "Expected requestedSchema type to be 'object'"
        )
        schema
    pure $ ElicitParams msg (fst schemaProps) (snd schemaProps)

-- | A request from the server to elicit additional information from the user via the client.
data ElicitRequest = ElicitRequest
  { id :: RequestId
  , params :: ElicitParams
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ViaJSONRPCRequest ElicitRequest

instance IsJSONRPCRequest ElicitRequest where
  requestMethod _ = "elicitation/create"

-- | The client's response to an elicitation request.
data ElicitResult = ElicitResult
  { action :: Text
  -- ^ The user action in response to the elicitation.
  -- - "accept": User submitted the form/confirmed the action
  -- - "decline": User explicitly declined the action
  -- - "cancel": User dismissed without making an explicit choice
  , content :: Maybe (Map Text Value)
  -- ^ The submitted form data, only present when action is "accept".
  -- Contains values matching the requested schema.
  , _meta :: Maybe Metadata
  }
  deriving stock (Show, Eq, Generic)

$(deriveJSON mcpParseOpts ''ElicitResult)
