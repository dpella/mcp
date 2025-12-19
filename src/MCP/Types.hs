{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : MCP.Types
Description : Core types for the Model Context Protocol (MCP) version 2025-06-18
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC

This module defines the core types used in the Model Context Protocol (MCP) version 2025-06-18,
including JSON-RPC message types, client/server capabilities, resources, tools, prompts,
content blocks, sampling messages, elicitation forms, and various request/response types.

Key features of the 2025-06-18 implementation:
- BaseMetadata interface with name/title distinction
- ContentBlock type supporting text, image, audio, embedded resources, and resource links
- Enhanced metadata with _meta fields and lastModified timestamps
- Restricted SamplingContent type for LLM sampling (text, image, audio only)
- Resource link support for referencing without embedding
- Comprehensive schema validation support for tools and elicitation
-}
module MCP.Types (
    -- * Constants
    mcpProtocolVersion,

    -- * Basic Types
    RequestId (..),
    Role (..),
    Cursor (..),
    ProgressToken (..),
    LoggingLevel (..),

    -- * Content Types
    Annotations (..),
    TextContent (..),
    ImageContent (..),
    AudioContent (..),
    EmbeddedResource (..),
    ResourceLink (..),
    ContentBlock (..),

    -- * Resource Types
    ResourceContents (..),
    TextResourceContents (..),
    BlobResourceContents (..),
    Resource (..),
    ResourceTemplate (..),
    ResourceReference (..),
    ResourceTemplateReference (..),

    -- * Tool Types
    ToolAnnotations (..),
    Tool (..),
    InputSchema (..),

    -- * Prompt Types
    PromptArgument (..),
    Prompt (..),
    PromptMessage (..),
    PromptReference (..),

    -- * Model Types
    ModelHint (..),
    ModelPreferences (..),
    IncludeContext (..),
    SamplingContent (..),
    SamplingMessage (..),

    -- * Capability Types
    ClientCapabilities (..),
    ServerCapabilities (..),
    RootsCapability (..),
    PromptsCapability (..),
    ResourcesCapability (..),
    ToolsCapability (..),
    CompletionsCapability (..),
    LoggingCapability (..),
    SamplingCapability (..),
    ElicitationCapability (..),
    ExperimentalCapability (..),

    -- * Base Types
    BaseMetadata (..),

    -- * Implementation Info
    Implementation (..),

    -- * Roots
    Root (..),

    -- * Result Types
    Result (..),
    Metadata (..),
) where

import Control.Applicative (Alternative ((<|>)))
import Data.Aeson hiding (Error, Result)
import Data.Aeson.TH
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics

-- | The current MCP protocol version
mcpProtocolVersion :: Text
mcpProtocolVersion = "2025-06-18"

-- | Metadata for results and other types
newtype Metadata = Metadata (Map Text Value)
    deriving stock (Show, Eq, Generic)
    deriving newtype (ToJSON, FromJSON)

-- | A uniquely identifying ID for a request in JSON-RPC
newtype RequestId = RequestId Value
    deriving stock (Show, Eq)
    deriving newtype (ToJSON, FromJSON)

-- | The sender or recipient of messages and data in a conversation
data Role = User | Assistant
    deriving stock (Show, Eq, Generic)

instance ToJSON Role where
    toJSON User = "user"
    toJSON Assistant = "assistant"

instance FromJSON Role where
    parseJSON = withText "Role" $ \case
        "user" -> pure User
        "assistant" -> pure Assistant
        other -> fail $ "Unknown role: " <> show other

-- | An opaque token used to represent a cursor for pagination
newtype Cursor = Cursor Text
    deriving stock (Show, Eq)
    deriving newtype (ToJSON, FromJSON)

-- | A progress token, used to associate progress notifications with the original request
newtype ProgressToken = ProgressToken Value
    deriving stock (Show, Eq)
    deriving newtype (ToJSON, FromJSON)

-- | The severity of a log message
data LoggingLevel = Alert | Critical | Debug | Emergency | Error | Info | Notice | Warning
    deriving stock (Show, Eq, Generic)

instance ToJSON LoggingLevel where
    toJSON Alert = "alert"
    toJSON Critical = "critical"
    toJSON Debug = "debug"
    toJSON Emergency = "emergency"
    toJSON Error = "error"
    toJSON Info = "info"
    toJSON Notice = "notice"
    toJSON Warning = "warning"

instance FromJSON LoggingLevel where
    parseJSON = withText "LoggingLevel" $ \case
        "alert" -> pure Alert
        "critical" -> pure Critical
        "debug" -> pure Debug
        "emergency" -> pure Emergency
        "error" -> pure Error
        "info" -> pure Info
        "notice" -> pure Notice
        "warning" -> pure Warning
        other -> fail $ "Unknown logging level: " <> show other

-- | Base interface for metadata with name and optional title
data BaseMetadata = BaseMetadata
    { name :: Text
    , title :: Maybe Text
    }
    deriving stock (Show, Eq, Generic)

$(deriveJSON defaultOptions{omitNothingFields = True} ''BaseMetadata)

-- | Optional annotations for the client
data Annotations = Annotations
    { audience :: Maybe [Role]
    , priority :: Maybe Double -- 0.0 to 1.0
    , lastModified :: Maybe Text -- ISO 8601 formatted timestamp
    }
    deriving stock (Show, Eq, Generic)

$(deriveJSON defaultOptions{omitNothingFields = True} ''Annotations)

-- | Text provided to or from an LLM
data TextContent = TextContent
    { textType :: Text -- Always "text"
    , text :: Text
    , annotations :: Maybe Annotations
    , _meta :: Maybe Metadata
    }
    deriving stock (Show, Eq, Generic)

instance ToJSON TextContent where
    toJSON (TextContent _ txt anns meta) =
        object $
            [ "type" .= ("text" :: Text)
            , "text" .= txt
            ]
                ++ maybe [] (\a -> ["annotations" .= a]) anns
                ++ maybe [] (\m -> ["_meta" .= m]) meta

instance FromJSON TextContent where
    parseJSON = withObject "TextContent" $ \o -> do
        ty <- o .: "type"
        if ty == ("text" :: Text)
            then TextContent ty <$> o .: "text" <*> o .:? "annotations" <*> o .:? "_meta"
            else fail "Expected type 'text'"

-- | An image provided to or from an LLM
data ImageContent = ImageContent
    { imageType :: Text -- Always "image"
    , data' :: Text -- base64-encoded image data
    , mimeType :: Text
    , annotations :: Maybe Annotations
    , _meta :: Maybe Metadata
    }
    deriving stock (Show, Eq, Generic)

instance ToJSON ImageContent where
    toJSON (ImageContent _ dat mime anns meta) =
        object $
            [ "type" .= ("image" :: Text)
            , "data" .= dat
            , "mimeType" .= mime
            ]
                ++ maybe [] (\a -> ["annotations" .= a]) anns
                ++ maybe [] (\m -> ["_meta" .= m]) meta

instance FromJSON ImageContent where
    parseJSON = withObject "ImageContent" $ \o -> do
        ty <- o .: "type"
        if ty == ("image" :: Text)
            then ImageContent ty <$> o .: "data" <*> o .: "mimeType" <*> o .:? "annotations" <*> o .:? "_meta"
            else fail "Expected type 'image'"

-- | Audio provided to or from an LLM
data AudioContent = AudioContent
    { audioType :: Text -- Always "audio"
    , data' :: Text -- base64-encoded audio data
    , mimeType :: Text
    , annotations :: Maybe Annotations
    , _meta :: Maybe Metadata
    }
    deriving stock (Show, Eq, Generic)

instance ToJSON AudioContent where
    toJSON (AudioContent _ dat mime anns meta) =
        object $
            [ "type" .= ("audio" :: Text)
            , "data" .= dat
            , "mimeType" .= mime
            ]
                ++ maybe [] (\a -> ["annotations" .= a]) anns
                ++ maybe [] (\m -> ["_meta" .= m]) meta

instance FromJSON AudioContent where
    parseJSON = withObject "AudioContent" $ \o -> do
        ty <- o .: "type"
        if ty == ("audio" :: Text)
            then AudioContent ty <$> o .: "data" <*> o .: "mimeType" <*> o .:? "annotations" <*> o .:? "_meta"
            else fail "Expected type 'audio'"

-- | Text resource contents
data TextResourceContents = TextResourceContents
    { uri :: Text
    , text :: Text
    , mimeType :: Maybe Text
    , _meta :: Maybe Metadata
    }
    deriving stock (Show, Eq, Generic)

$(deriveJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = \case { "_meta" -> "_meta"; x -> x }} ''TextResourceContents)

-- | Blob resource contents
data BlobResourceContents = BlobResourceContents
    { uri :: Text
    , blob :: Text -- base64-encoded
    , mimeType :: Maybe Text
    , _meta :: Maybe Metadata
    }
    deriving stock (Show, Eq, Generic)

$(deriveJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = \case { "_meta" -> "_meta"; x -> x }} ''BlobResourceContents)

-- | Resource contents (text or blob)
data ResourceContents
    = TextResource TextResourceContents
    | BlobResource BlobResourceContents
    deriving stock (Show, Eq, Generic)

instance ToJSON ResourceContents where
    toJSON (TextResource t) = toJSON t
    toJSON (BlobResource b) = toJSON b

instance FromJSON ResourceContents where
    parseJSON v =
        (TextResource <$> parseJSON v)
            <|> (BlobResource <$> parseJSON v)

-- | The contents of a resource, embedded into a prompt or tool call result
data EmbeddedResource = EmbeddedResource
    { resourceType :: Text -- Always "resource"
    , resource :: ResourceContents
    , annotations :: Maybe Annotations
    , _meta :: Maybe Metadata
    }
    deriving stock (Show, Eq, Generic)

instance ToJSON EmbeddedResource where
    toJSON (EmbeddedResource _ res anns meta) =
        object $
            [ "type" .= ("resource" :: Text)
            , "resource" .= res
            ]
                ++ maybe [] (\a -> ["annotations" .= a]) anns
                ++ maybe [] (\m -> ["_meta" .= m]) meta

instance FromJSON EmbeddedResource where
    parseJSON = withObject "EmbeddedResource" $ \o -> do
        ty <- o .: "type"
        if ty == ("resource" :: Text)
            then EmbeddedResource ty <$> o .: "resource" <*> o .:? "annotations" <*> o .:? "_meta"
            else fail "Expected type 'resource'"

-- | A resource that the server is capable of reading, included in a prompt or tool call result
data ResourceLink = ResourceLink
    { resourceLinkType :: Text -- Always "resource_link"
    , uri :: Text
    , name :: Text
    , title :: Maybe Text
    , description :: Maybe Text
    , mimeType :: Maybe Text
    , size :: Maybe Int
    , annotations :: Maybe Annotations
    , _meta :: Maybe Metadata
    }
    deriving stock (Show, Eq, Generic)

instance ToJSON ResourceLink where
    toJSON (ResourceLink _ u n t d m s a meta) =
        object $
            [ "type" .= ("resource_link" :: Text)
            , "uri" .= u
            , "name" .= n
            ]
                ++ maybe [] (\x -> ["title" .= x]) t
                ++ maybe [] (\x -> ["description" .= x]) d
                ++ maybe [] (\x -> ["mimeType" .= x]) m
                ++ maybe [] (\x -> ["size" .= x]) s
                ++ maybe [] (\x -> ["annotations" .= x]) a
                ++ maybe [] (\x -> ["_meta" .= x]) meta

instance FromJSON ResourceLink where
    parseJSON = withObject "ResourceLink" $ \o -> do
        ty <- o .: "type"
        if ty == ("resource_link" :: Text)
            then
                ResourceLink ty
                    <$> o .: "uri"
                    <*> o .: "name"
                    <*> o .:? "title"
                    <*> o .:? "description"
                    <*> o .:? "mimeType"
                    <*> o .:? "size"
                    <*> o .:? "annotations"
                    <*> o .:? "_meta"
            else fail "Expected type 'resource_link'"

-- | Content blocks that can be text, image, audio, embedded resource, or resource link
data ContentBlock
    = TextContentType TextContent
    | ImageContentType ImageContent
    | AudioContentType AudioContent
    | EmbeddedResourceType EmbeddedResource
    | ResourceLinkType ResourceLink
    deriving stock (Show, Eq, Generic)

instance ToJSON ContentBlock where
    toJSON (TextContentType c) = toJSON c
    toJSON (ImageContentType c) = toJSON c
    toJSON (AudioContentType c) = toJSON c
    toJSON (EmbeddedResourceType c) = toJSON c
    toJSON (ResourceLinkType c) = toJSON c

instance FromJSON ContentBlock where
    parseJSON v =
        (TextContentType <$> parseJSON v)
            <|> (ImageContentType <$> parseJSON v)
            <|> (AudioContentType <$> parseJSON v)
            <|> (EmbeddedResourceType <$> parseJSON v)
            <|> (ResourceLinkType <$> parseJSON v)

-- | A known resource that the server is capable of reading
data Resource = Resource
    { uri :: Text
    , name :: Text
    , title :: Maybe Text
    , description :: Maybe Text
    , mimeType :: Maybe Text
    , size :: Maybe Int
    , annotations :: Maybe Annotations
    , _meta :: Maybe Metadata
    }
    deriving stock (Show, Eq, Generic)

$(deriveJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = \case { "_meta" -> "_meta"; x -> x }} ''Resource)

-- | A template description for resources available on the server
data ResourceTemplate = ResourceTemplate
    { name :: Text
    , title :: Maybe Text
    , uriTemplate :: Text
    , description :: Maybe Text
    , mimeType :: Maybe Text
    , annotations :: Maybe Annotations
    , _meta :: Maybe Metadata
    }
    deriving stock (Show, Eq, Generic)

$(deriveJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = \case { "_meta" -> "_meta"; x -> x }} ''ResourceTemplate)

-- | A reference to a resource or resource template definition
data ResourceReference = ResourceReference
    { refType :: Text -- Always "ref/resource"
    , uri :: Text
    }
    deriving stock (Show, Eq, Generic)

instance ToJSON ResourceReference where
    toJSON (ResourceReference _ u) =
        object
            [ "type" .= ("ref/resource" :: Text)
            , "uri" .= u
            ]

instance FromJSON ResourceReference where
    parseJSON = withObject "ResourceReference" $ \o -> do
        ty <- o .: "type"
        if ty == ("ref/resource" :: Text)
            then ResourceReference ty <$> o .: "uri"
            else fail "Expected type 'ref/resource'"

-- | A reference to a resource template definition
data ResourceTemplateReference = ResourceTemplateReference
    { refType :: Text -- Always "ref/resource"
    , uri :: Text
    }
    deriving stock (Show, Eq, Generic)

instance ToJSON ResourceTemplateReference where
    toJSON (ResourceTemplateReference _ u) =
        object
            [ "type" .= ("ref/resource" :: Text)
            , "uri" .= u
            ]

instance FromJSON ResourceTemplateReference where
    parseJSON = withObject "ResourceTemplateReference" $ \o -> do
        ty <- o .: "type"
        if ty == ("ref/resource" :: Text)
            then ResourceTemplateReference ty <$> o .: "uri"
            else fail "Expected type 'ref/resource'"

-- | Additional properties describing a Tool to clients
data ToolAnnotations = ToolAnnotations
    { title :: Maybe Text
    , readOnlyHint :: Maybe Bool
    , destructiveHint :: Maybe Bool
    , idempotentHint :: Maybe Bool
    , openWorldHint :: Maybe Bool
    }
    deriving stock (Show, Eq, Generic)

$(deriveJSON defaultOptions{omitNothingFields = True} ''ToolAnnotations)

-- | Input schema for a tool
data InputSchema = InputSchema
    { schemaType :: Text -- Always "object"
    , properties :: Maybe (Map Text Value)
    , required :: Maybe [Text]
    }
    deriving stock (Show, Eq, Generic)

instance ToJSON InputSchema where
    toJSON (InputSchema _ props req) =
        object $
            [ "type" .= ("object" :: Text)
            ]
                ++ maybe [] (\p -> ["properties" .= p]) props
                ++ maybe [] (\r -> ["required" .= r]) req

instance FromJSON InputSchema where
    parseJSON = withObject "InputSchema" $ \o -> do
        ty <- o .: "type"
        if ty == ("object" :: Text)
            then InputSchema ty <$> o .:? "properties" <*> o .:? "required"
            else fail "Expected type 'object'"

-- | Definition for a tool the client can call
data Tool = Tool
    { name :: Text
    , title :: Maybe Text
    , description :: Maybe Text
    , inputSchema :: InputSchema
    , outputSchema :: Maybe InputSchema
    , annotations :: Maybe ToolAnnotations
    , _meta :: Maybe Metadata
    }
    deriving stock (Show, Eq, Generic)

$(deriveJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = \case { "_meta" -> "_meta"; x -> x }} ''Tool)

-- | Describes an argument that a prompt can accept
data PromptArgument = PromptArgument
    { name :: Text
    , title :: Maybe Text
    , description :: Maybe Text
    , required :: Maybe Bool
    }
    deriving stock (Show, Eq, Generic)

$(deriveJSON defaultOptions{omitNothingFields = True} ''PromptArgument)

-- | A prompt or prompt template that the server offers
data Prompt = Prompt
    { name :: Text
    , title :: Maybe Text
    , description :: Maybe Text
    , arguments :: Maybe [PromptArgument]
    , _meta :: Maybe Metadata
    }
    deriving stock (Show, Eq, Generic)

$(deriveJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = \case { "_meta" -> "_meta"; x -> x }} ''Prompt)

-- | Describes a message returned as part of a prompt
data PromptMessage = PromptMessage
    { role :: Role
    , content :: ContentBlock
    }
    deriving stock (Show, Eq, Generic)

$(deriveJSON defaultOptions ''PromptMessage)

-- | Identifies a prompt
data PromptReference = PromptReference
    { refType :: Text -- Always "ref/prompt"
    , name :: Text
    , title :: Maybe Text
    }
    deriving stock (Show, Eq, Generic)

instance ToJSON PromptReference where
    toJSON (PromptReference _ n t) =
        object $
            [ "type" .= ("ref/prompt" :: Text)
            , "name" .= n
            ]
                ++ maybe [] (\tit -> ["title" .= tit]) t

instance FromJSON PromptReference where
    parseJSON = withObject "PromptReference" $ \o -> do
        ty <- o .: "type"
        if ty == ("ref/prompt" :: Text)
            then PromptReference ty <$> o .: "name" <*> o .:? "title"
            else fail "Expected type 'ref/prompt'"

-- | Hints to use for model selection
data ModelHint where
    ModelHint :: {name :: Maybe Text} -> ModelHint
    deriving stock (Show, Eq, Generic)

$(deriveJSON defaultOptions{omitNothingFields = True} ''ModelHint)

-- | The server's preferences for model selection
data ModelPreferences = ModelPreferences
    { hints :: Maybe [ModelHint]
    , costPriority :: Maybe Double -- 0.0 to 1.0
    , speedPriority :: Maybe Double -- 0.0 to 1.0
    , intelligencePriority :: Maybe Double -- 0.0 to 1.0
    }
    deriving stock (Show, Eq, Generic)

$(deriveJSON defaultOptions{omitNothingFields = True} ''ModelPreferences)

-- | Include context options for sampling
data IncludeContext = AllServers | None | ThisServer
    deriving stock (Show, Eq, Generic)

instance ToJSON IncludeContext where
    toJSON AllServers = "allServers"
    toJSON None = "none"
    toJSON ThisServer = "thisServer"

instance FromJSON IncludeContext where
    parseJSON = withText "IncludeContext" $ \case
        "allServers" -> pure AllServers
        "none" -> pure None
        "thisServer" -> pure ThisServer
        other -> fail $ "Unknown include context: " <> show other

-- | Restricted content type for sampling messages (text, image, audio only)
data SamplingContent
    = SamplingTextContent TextContent
    | SamplingImageContent ImageContent
    | SamplingAudioContent AudioContent
    deriving stock (Show, Eq, Generic)

instance ToJSON SamplingContent where
    toJSON (SamplingTextContent c) = toJSON c
    toJSON (SamplingImageContent c) = toJSON c
    toJSON (SamplingAudioContent c) = toJSON c

instance FromJSON SamplingContent where
    parseJSON v =
        (SamplingTextContent <$> parseJSON v)
            <|> (SamplingImageContent <$> parseJSON v)
            <|> (SamplingAudioContent <$> parseJSON v)

-- | Describes a message issued to or received from an LLM API
data SamplingMessage = SamplingMessage
    { role :: Role
    , content :: SamplingContent
    }
    deriving stock (Show, Eq, Generic)

$(deriveJSON defaultOptions ''SamplingMessage)

-- | Roots capability
data RootsCapability where
    RootsCapability :: {listChanged :: Maybe Bool} -> RootsCapability
    deriving stock (Show, Eq, Generic)

$(deriveJSON defaultOptions{omitNothingFields = True} ''RootsCapability)

-- | Prompts capability
data PromptsCapability where
    PromptsCapability ::
        {listChanged :: Maybe Bool} ->
        PromptsCapability
    deriving stock (Show, Eq, Generic)

$(deriveJSON defaultOptions{omitNothingFields = True} ''PromptsCapability)

-- | Resources capability
data ResourcesCapability = ResourcesCapability
    { listChanged :: Maybe Bool
    , subscribe :: Maybe Bool
    }
    deriving stock (Show, Eq, Generic)

$(deriveJSON defaultOptions{omitNothingFields = True} ''ResourcesCapability)

-- | Tools capability
data ToolsCapability where
    ToolsCapability :: {listChanged :: Maybe Bool} -> ToolsCapability
    deriving stock (Show, Eq, Generic)

$(deriveJSON defaultOptions{omitNothingFields = True} ''ToolsCapability)

-- | Completions capability
data CompletionsCapability = CompletionsCapability
    deriving stock (Show, Eq, Generic)

instance ToJSON CompletionsCapability where
    toJSON _ = object []

instance FromJSON CompletionsCapability where
    parseJSON = withObject "CompletionsCapability" $ \_ -> pure CompletionsCapability

-- | Logging capability
data LoggingCapability = LoggingCapability
    deriving stock (Show, Eq, Generic)

instance ToJSON LoggingCapability where
    toJSON _ = object []

instance FromJSON LoggingCapability where
    parseJSON = withObject "LoggingCapability" $ \_ -> pure LoggingCapability

-- | Sampling capability
data SamplingCapability = SamplingCapability
    deriving stock (Show, Eq, Generic)

instance ToJSON SamplingCapability where
    toJSON _ = object []

instance FromJSON SamplingCapability where
    parseJSON = withObject "SamplingCapability" $ \_ -> pure SamplingCapability

-- | Elicitation capability
data ElicitationCapability = ElicitationCapability
    deriving stock (Show, Eq, Generic)

instance ToJSON ElicitationCapability where
    toJSON _ = object []

instance FromJSON ElicitationCapability where
    parseJSON = withObject "ElicitationCapability" $ \_ -> pure ElicitationCapability

-- | Experimental capability
newtype ExperimentalCapability = ExperimentalCapability (Map Text Value)
    deriving stock (Show, Eq, Generic)
    deriving newtype (ToJSON, FromJSON)

-- | Capabilities a client may support
data ClientCapabilities = ClientCapabilities
    { roots :: Maybe RootsCapability
    , sampling :: Maybe SamplingCapability
    , elicitation :: Maybe ElicitationCapability
    , experimental :: Maybe ExperimentalCapability
    }
    deriving stock (Show, Eq, Generic)

$(deriveJSON defaultOptions{omitNothingFields = True} ''ClientCapabilities)

-- | Capabilities that a server may support
data ServerCapabilities = ServerCapabilities
    { logging :: Maybe LoggingCapability
    , prompts :: Maybe PromptsCapability
    , resources :: Maybe ResourcesCapability
    , tools :: Maybe ToolsCapability
    , completions :: Maybe CompletionsCapability
    , experimental :: Maybe ExperimentalCapability
    }
    deriving stock (Show, Eq, Generic)

$(deriveJSON defaultOptions{omitNothingFields = True} ''ServerCapabilities)

-- | Describes the name and version of an MCP implementation
data Implementation = Implementation
    { name :: Text
    , title :: Maybe Text
    , version :: Text
    }
    deriving stock (Show, Eq, Generic)

$(deriveJSON defaultOptions{omitNothingFields = True} ''Implementation)

-- | Represents a root directory or file that the server can operate on
data Root = Root
    { uri :: Text
    , name :: Maybe Text
    , _meta :: Maybe Metadata
    }
    deriving stock (Show, Eq, Generic)

$(deriveJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = \case { "_meta" -> "_meta"; x -> x }} ''Root)

-- | Base result type
data Result where
    Result :: {_meta :: Maybe Metadata} -> Result
    deriving stock (Show, Eq, Generic)

$(deriveJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = \case { "_meta" -> "_meta"; x -> x }} ''Result)
