{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : MCP.Types
Description : Core types for the Model Context Protocol (MCP)
License     : MPL-2.0
Maintainer  : matti@dpella.io, lobo@dpella.io
Stability   : experimental
Portability : GHC

This module defines the core types used in the Model Context Protocol (MCP),
including JSON-RPC message types, client/server capabilities, resources,
tools, prompts, and various request/response types.
-}
module MCP.Types (
    -- * Basic Types (re-exported from JSONRPC)
    RequestId (..),

    -- * Basic Types
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
    Content (..),
    ContentBlock (..),

    -- * Resource Types
    ResourceContents (..),
    TextResourceContents (..),
    BlobResourceContents (..),
    Resource (..),
    ResourceTemplate (..),
    ResourceReference (..),
    ResourceTemplateReference,

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

    -- * Implementation Info
    BaseMetadata (..),
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
import JSONRPC (RequestId (..))
import MCP.Aeson

{- | The sender or recipient of messages and data in a conversation.

This is used to identify whether content comes from the user or the assistant
in prompt messages and sampling requests.
-}
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

{- | An opaque token used to represent a cursor for pagination.

Used in paginated requests to indicate the starting position for the next
set of results. If provided, the server should return results starting
after this cursor.
-}
newtype Cursor = Cursor Text
    deriving stock (Show, Eq)
    deriving newtype (ToJSON, FromJSON)

{- | A progress token, used to associate progress notifications with the original request.

If specified in a request, the caller is requesting out-of-band progress
notifications (as represented by notifications/progress). The value is an
opaque token that will be attached to any subsequent notifications.
The receiver is not obligated to provide these notifications.
-}
newtype ProgressToken = ProgressToken Value
    deriving stock (Show, Eq)
    deriving newtype (ToJSON, FromJSON)

{- | The severity of a log message.

These map to syslog message severities, as specified in RFC-5424:
https://datatracker.ietf.org/doc/html/rfc5424#section-6.2.1
-}
data LoggingLevel
    = Emergency
    | Alert
    | Critical
    | Error
    | Warning
    | Notice
    | Info
    | Debug
    deriving stock (Show, Ord, Eq, Generic)

instance ToJSON LoggingLevel where
    toJSON Emergency = "emergency"
    toJSON Alert = "alert"
    toJSON Critical = "critical"
    toJSON Error = "error"
    toJSON Warning = "warning"
    toJSON Notice = "notice"
    toJSON Info = "info"
    toJSON Debug = "debug"

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

{- | Optional annotations for the client.

The client can use annotations to inform how objects are used or displayed.
-}
data Annotations = Annotations
    { audience :: Maybe [Role]
    {- ^ Describes who the intended customer of this object or data is.
    It can include multiple entries to indicate content useful for
    multiple audiences (e.g., ["user", "assistant"]).
    -}
    , priority :: Maybe Double
    {- ^ Describes how important this data is for operating the server.
    A value of 1 means "most important," and indicates that the data is
    effectively required, while 0 means "least important," and indicates
    that the data is entirely optional. Range: 0.0 to 1.0
    -}
    , lastModified :: Maybe Text
    {- ^ The moment the resource was last modified, as an ISO 8601 formatted string.
    Should be an ISO 8601 formatted string (e.g., "2025-01-12T15:00:58Z").
    Examples: last activity timestamp in an open file, timestamp when the resource
    was attached, etc.
    -}
    }
    deriving stock (Show, Eq, Generic)

$(deriveJSON mcpParseOpts ''Annotations)

{- | Metadata for results.

This result property is reserved by the protocol to allow clients and servers
to attach additional metadata to their responses.
-}
newtype Metadata = Metadata (Map Text Value)
    deriving stock (Show, Eq, Generic)
    deriving newtype (ToJSON, FromJSON)

{- | Text provided to or from an LLM.

This represents textual content that can be sent to or received from
a language model during conversations.
-}
data TextContent = TextContent
    { textType :: Text -- Always "text"
    , text :: Text
    -- ^ The text content of the message.
    , annotations :: Maybe Annotations
    -- ^ Optional annotations for the client.
    , _meta :: Maybe Metadata
    -- ^ Optional metadata field.
    }
    deriving stock (Show, Eq, Generic)

instance ToJSON TextContent where
    toJSON (TextContent _ txt anns m) =
        object $
            [ "type" .= ("text" :: Text)
            , "text" .= txt
            ]
                <> maybe [] (\a -> ["annotations" .= a]) anns
                <> maybe [] (\meta -> ["_meta" .= meta]) m

instance FromJSON TextContent where
    parseJSON = withObject "TextContent" $ \o -> do
        ty <- o .: "type"
        if ty == ("text" :: Text)
            then TextContent ty <$> o .: "text" <*> o .:? "annotations" <*> o .:? "_meta"
            else fail "Expected type 'text'"

{- | An image provided to or from an LLM.

Represents image data that can be included in messages to language models
that support multimodal input.
-}
data ImageContent = ImageContent
    { imageType :: Text -- Always "image"
    , data' :: Text
    -- ^ The base64-encoded image data.
    , mimeType :: Text
    -- ^ The MIME type of the image. Different providers may support different image types.
    , annotations :: Maybe Annotations
    -- ^ Optional annotations for the client.
    , _meta :: Maybe Metadata
    -- ^ Optional metadata field.
    }
    deriving stock (Show, Eq, Generic)

instance ToJSON ImageContent where
    toJSON (ImageContent _ dat mime anns m) =
        object $
            [ "type" .= ("image" :: Text)
            , "data" .= dat
            , "mimeType" .= mime
            ]
                <> maybe [] (\a -> ["annotations" .= a]) anns
                <> maybe [] (\meta -> ["_meta" .= meta]) m

instance FromJSON ImageContent where
    parseJSON = withObject "ImageContent" $ \o -> do
        ty <- o .: "type"
        if ty == ("image" :: Text)
            then ImageContent ty <$> o .: "data" <*> o .: "mimeType" <*> o .:? "annotations" <*> o .:? "_meta"
            else fail "Expected type 'image'"

{- | Audio provided to or from an LLM.

Represents audio data that can be included in messages to language models
that support audio input.
-}
data AudioContent = AudioContent
    { audioType :: Text -- Always "audio"
    , data' :: Text
    -- ^ The base64-encoded audio data.
    , mimeType :: Text
    -- ^ The MIME type of the audio. Different providers may support different audio types.
    , annotations :: Maybe Annotations
    -- ^ Optional annotations for the client.
    , _meta :: Maybe Metadata
    -- ^ Optional metadata field.
    }
    deriving stock (Show, Eq, Generic)

instance ToJSON AudioContent where
    toJSON (AudioContent _ dat mime anns m) =
        object $
            [ "type" .= ("audio" :: Text)
            , "data" .= dat
            , "mimeType" .= mime
            ]
                <> maybe [] (\a -> ["annotations" .= a]) anns
                <> maybe [] (\meta -> ["_meta" .= meta]) m

instance FromJSON AudioContent where
    parseJSON = withObject "AudioContent" $ \o -> do
        ty <- o .: "type"
        if ty == ("audio" :: Text)
            then AudioContent ty <$> o .: "data" <*> o .: "mimeType" <*> o .:? "annotations" <*> o .:? "_meta"
            else fail "Expected type 'audio'"

{- | Text resource contents.

Represents the contents of a resource that can be represented as text
(not binary data).
-}
data TextResourceContents = TextResourceContents
    { uri :: Text
    -- ^ The URI of this resource.
    , text :: Text
    {- ^ The text of the item. This must only be set if the item can
    actually be represented as text (not binary data).
    -}
    , mimeType :: Maybe Text
    -- ^ The MIME type of this resource, if known.
    , _meta :: Maybe Metadata
    -- ^ Optional metadata field.
    }
    deriving stock (Show, Eq, Generic)

$( deriveJSON
    mcpParseOpts
    ''TextResourceContents
 )

{- | Blob resource contents.

Represents the contents of a resource as binary data, encoded in base64.
-}
data BlobResourceContents = BlobResourceContents
    { uri :: Text
    -- ^ The URI of this resource.
    , blob :: Text
    -- ^ A base64-encoded string representing the binary data of the item.
    , mimeType :: Maybe Text
    -- ^ The MIME type of this resource, if known.
    , _meta :: Maybe Metadata
    -- ^ Optional metadata field.
    }
    deriving stock (Show, Eq, Generic)

$( deriveJSON
    mcpParseOpts
    ''BlobResourceContents
 )

{- | The contents of a specific resource or sub-resource.

A resource can contain either text or binary data.
-}
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

{- | The contents of a resource, embedded into a prompt or tool call result.

It is up to the client how best to render embedded resources for the benefit
of the LLM and/or the user.
-}
data EmbeddedResource = EmbeddedResource
    { resourceType :: Text -- Always "resource"
    , resource :: ResourceContents
    , annotations :: Maybe Annotations
    -- ^ Optional annotations for the client.
    , _meta :: Maybe Metadata
    -- ^ Optional metadata field.
    }
    deriving stock (Show, Eq, Generic)

instance ToJSON EmbeddedResource where
    toJSON (EmbeddedResource _ res anns m) =
        object $
            [ "type" .= ("resource" :: Text)
            , "resource" .= res
            ]
                <> maybe [] (\a -> ["annotations" .= a]) anns
                <> maybe [] (\meta -> ["_meta" .= meta]) m

instance FromJSON EmbeddedResource where
    parseJSON = withObject "EmbeddedResource" $ \o -> do
        ty <- o .: "type"
        if ty == ("resource" :: Text)
            then EmbeddedResource ty <$> o .: "resource" <*> o .:? "annotations" <*> o .:? "_meta"
            else fail "Expected type 'resource'"

{- | A resource that the server is capable of reading, included in a prompt or tool call result.

Note: resource links returned by tools are not guaranteed to appear in the results of `resources/list` requests.
-}
data ResourceLink = ResourceLink
    { linkType :: Text -- Always "resource_link"
    , uri :: Text
    -- ^ The URI of this resource.
    , name :: Text
    -- ^ A human-readable name for this resource.
    , title :: Maybe Text
    -- ^ Optional title for display purposes.
    , description :: Maybe Text
    -- ^ A description of what this resource represents.
    , mimeType :: Maybe Text
    -- ^ The MIME type of this resource, if known.
    , size :: Maybe Int
    -- ^ The size of the raw resource content, in bytes.
    , annotations :: Maybe Annotations
    -- ^ Optional annotations for the client.
    , _meta :: Maybe Metadata
    -- ^ Optional metadata field.
    }
    deriving stock (Show, Eq, Generic)

instance ToJSON ResourceLink where
    toJSON (ResourceLink _ u n t d mt s a m) =
        object $
            [ "type" .= ("resource_link" :: Text)
            , "uri" .= u
            , "name" .= n
            ]
                <> maybe [] (\ttl -> ["title" .= ttl]) t
                <> maybe [] (\desc -> ["description" .= desc]) d
                <> maybe [] (\mime -> ["mimeType" .= mime]) mt
                <> maybe [] (\sz -> ["size" .= sz]) s
                <> maybe [] (\ann -> ["annotations" .= ann]) a
                <> maybe [] (\meta -> ["_meta" .= meta]) m

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

{- | Content that can be text, image, audio, or embedded resource.

This union type represents the different kinds of content that can be
included in messages, prompts, and tool results.
-}
data Content
    = TextContentType TextContent
    | ImageContentType ImageContent
    | AudioContentType AudioContent
    | EmbeddedResourceType EmbeddedResource
    deriving stock (Show, Eq, Generic)

instance ToJSON Content where
    toJSON (TextContentType c) = toJSON c
    toJSON (ImageContentType c) = toJSON c
    toJSON (AudioContentType c) = toJSON c
    toJSON (EmbeddedResourceType c) = toJSON c

instance FromJSON Content where
    parseJSON v =
        (TextContentType <$> parseJSON v)
            <|> (ImageContentType <$> parseJSON v)
            <|> (AudioContentType <$> parseJSON v)
            <|> (EmbeddedResourceType <$> parseJSON v)

{- | ContentBlock union type that includes ResourceLink.

This is the unified content type used in new schema version.
-}
data ContentBlock
    = TextBlock TextContent
    | ImageBlock ImageContent
    | AudioBlock AudioContent
    | ResourceLinkBlock ResourceLink
    | EmbeddedResourceBlock EmbeddedResource
    deriving stock (Show, Eq, Generic)

instance ToJSON ContentBlock where
    toJSON (TextBlock c) = toJSON c
    toJSON (ImageBlock c) = toJSON c
    toJSON (AudioBlock c) = toJSON c
    toJSON (ResourceLinkBlock c) = toJSON c
    toJSON (EmbeddedResourceBlock c) = toJSON c

instance FromJSON ContentBlock where
    parseJSON v =
        (TextBlock <$> parseJSON v)
            <|> (ImageBlock <$> parseJSON v)
            <|> (AudioBlock <$> parseJSON v)
            <|> (ResourceLinkBlock <$> parseJSON v)
            <|> (EmbeddedResourceBlock <$> parseJSON v)

{- | A known resource that the server is capable of reading.

Resources represent files, documents, or other data sources that the
server can provide to clients.
-}
data Resource = Resource
    { uri :: Text
    -- ^ The URI of this resource.
    , name :: Text
    {- ^ A human-readable name for this resource.
    This can be used by clients to populate UI elements.
    -}
    , title :: Maybe Text
    -- ^ Optional title for display purposes.
    , description :: Maybe Text
    {- ^ A description of what this resource represents.
    This can be used by clients to improve the LLM's understanding
    of available resources. It can be thought of like a "hint" to the model.
    -}
    , mimeType :: Maybe Text
    -- ^ The MIME type of this resource, if known.
    , size :: Maybe Int
    {- ^ The size of the raw resource content, in bytes (i.e., before base64
    encoding or any tokenization), if known. This can be used by Hosts
    to display file sizes and estimate context window usage.
    -}
    , annotations :: Maybe Annotations
    -- ^ Optional annotations for the client.
    , _meta :: Maybe Metadata
    -- ^ Optional metadata field.
    }
    deriving stock (Show, Eq, Generic)

$( deriveJSON
    mcpParseOpts
    ''Resource
 )

{- | A template description for resources available on the server.

Resource templates allow servers to describe parameterized resources
that can be instantiated with specific values.
-}
data ResourceTemplate = ResourceTemplate
    { name :: Text
    {- ^ A human-readable name for the type of resource this template refers to.
    This can be used by clients to populate UI elements.
    -}
    , title :: Maybe Text
    -- ^ Optional title for display purposes.
    , uriTemplate :: Text
    -- ^ A URI template (according to RFC 6570) that can be used to construct resource URIs.
    , description :: Maybe Text
    {- ^ A description of what this template is for.
    This can be used by clients to improve the LLM's understanding
    of available resources. It can be thought of like a "hint" to the model.
    -}
    , mimeType :: Maybe Text
    {- ^ The MIME type for all resources that match this template.
    This should only be included if all resources matching this
    template have the same type.
    -}
    , annotations :: Maybe Annotations
    -- ^ Optional annotations for the client.
    , _meta :: Maybe Metadata
    -- ^ Optional metadata field.
    }
    deriving stock (Show, Eq, Generic)

$( deriveJSON
    mcpParseOpts
    ''ResourceTemplate
 )

{- | A reference to a resource or resource template definition.

Used in completion requests and other contexts where a resource
needs to be referenced.
-}
data ResourceReference = ResourceReference
    { refType :: Text -- Always "ref/resource"
    , uri :: Text
    -- ^ The URI or URI template of the resource.
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

{- | A reference to a resource template definition.

This is an alias for ResourceReference in the new schema.
-}
type ResourceTemplateReference = ResourceReference

{- | Additional properties describing a Tool to clients.

NOTE: all properties in ToolAnnotations are **hints**.
They are not guaranteed to provide a faithful description of
tool behavior (including descriptive properties like `title`).

Clients should never make tool use decisions based on ToolAnnotations
received from untrusted servers.
-}
data ToolAnnotations = ToolAnnotations
    { title :: Maybe Text
    -- ^ A human-readable title for the tool.
    , readOnlyHint :: Maybe Bool
    -- ^ If true, the tool does not modify its environment. Default: false
    , destructiveHint :: Maybe Bool
    {- ^ If true, the tool may perform destructive updates to its environment.
    If false, the tool performs only additive updates.
    (This property is meaningful only when `readOnlyHint == false`)
    Default: true
    -}
    , idempotentHint :: Maybe Bool
    {- ^ If true, calling the tool repeatedly with the same arguments
    will have no additional effect on its environment.
    (This property is meaningful only when `readOnlyHint == false`)
    Default: false
    -}
    , openWorldHint :: Maybe Bool
    {- ^ If true, this tool may interact with an "open world" of external
    entities. If false, the tool's domain of interaction is closed.
    For example, the world of a web search tool is open, whereas that
    of a memory tool is not. Default: true
    -}
    }
    deriving stock (Show, Eq, Generic)

$(deriveJSON mcpParseOpts ''ToolAnnotations)

{- | Input schema for a tool.

A JSON Schema object defining the expected parameters for the tool.
-}
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
                <> maybe [] (\p -> ["properties" .= p]) props
                <> maybe [] (\r -> ["required" .= r]) req

instance FromJSON InputSchema where
    parseJSON = withObject "InputSchema" $ \o -> do
        ty <- o .: "type"
        if ty == ("object" :: Text)
            then InputSchema ty <$> o .:? "properties" <*> o .:? "required"
            else fail "Expected type 'object'"

{- | Definition for a tool the client can call.

Tools are functions that the MCP server exposes to clients,
allowing the client to perform specific actions or retrieve information.
-}
data Tool = Tool
    { name :: Text
    -- ^ The name of the tool.
    , title :: Maybe Text
    -- ^ Optional title for display purposes.
    , description :: Maybe Text
    {- ^ A human-readable description of the tool.
    This can be used by clients to improve the LLM's understanding
    of available tools. It can be thought of like a "hint" to the model.
    -}
    , inputSchema :: InputSchema
    -- ^ A JSON Schema object defining the expected parameters for the tool.
    , outputSchema :: Maybe InputSchema
    {- ^ An optional JSON Schema object defining the structure of the tool's output
    returned in the structuredContent field of a CallToolResult.
    -}
    , annotations :: Maybe ToolAnnotations
    {- ^ Optional additional tool information.
    Display name precedence order is: title, annotations.title, then name.
    -}
    , _meta :: Maybe Metadata
    -- ^ Optional metadata field.
    }
    deriving stock (Show, Eq, Generic)

$(deriveJSON mcpParseOpts ''Tool)

{- | Describes an argument that a prompt can accept.

Prompt arguments allow prompts to be parameterized with user-provided values.
-}
data PromptArgument = PromptArgument
    { name :: Text
    -- ^ The name of the argument.
    , title :: Maybe Text
    -- ^ Optional title for display purposes.
    , description :: Maybe Text
    -- ^ A human-readable description of the argument.
    , required :: Maybe Bool
    -- ^ Whether this argument must be provided.
    }
    deriving stock (Show, Eq, Generic)

$(deriveJSON mcpParseOpts ''PromptArgument)

{- | A prompt or prompt template that the server offers.

Prompts are reusable message templates that can be parameterized
with arguments to generate specific conversations.
-}
data Prompt = Prompt
    { name :: Text
    -- ^ The name of the prompt or prompt template.
    , title :: Maybe Text
    -- ^ Optional title for display purposes.
    , description :: Maybe Text
    -- ^ An optional description of what this prompt provides.
    , arguments :: Maybe [PromptArgument]
    -- ^ A list of arguments to use for templating the prompt.
    , _meta :: Maybe Metadata
    -- ^ Optional metadata field.
    }
    deriving stock (Show, Eq, Generic)

$(deriveJSON mcpParseOpts ''Prompt)

{- | Describes a message returned as part of a prompt.

This is similar to `SamplingMessage`, but also supports the embedding of
resources from the MCP server.
-}
data PromptMessage = PromptMessage
    { role :: Role
    , content :: ContentBlock
    }
    deriving stock (Show, Eq, Generic)

$(deriveJSON mcpParseOpts ''PromptMessage)

{- | Identifies a prompt.

Used in completion requests and other contexts where a prompt
needs to be referenced.
-}
data PromptReference = PromptReference
    { refType :: Text -- Always "ref/prompt"
    , name :: Text
    -- ^ The name of the prompt or prompt template.
    , title :: Maybe Text
    -- ^ Optional title for display purposes.
    }
    deriving stock (Show, Eq, Generic)

instance ToJSON PromptReference where
    toJSON (PromptReference _ n t) =
        object $
            [ "type" .= ("ref/prompt" :: Text)
            , "name" .= n
            ]
                <> maybe [] (\ttl -> ["title" .= ttl]) t

instance FromJSON PromptReference where
    parseJSON = withObject "PromptReference" $ \o -> do
        ty <- o .: "type"
        if ty == ("ref/prompt" :: Text)
            then PromptReference ty <$> o .: "name" <*> o .:? "title"
            else fail "Expected type 'ref/prompt'"

{- | Hints to use for model selection.

Keys not declared here are currently left unspecified by the spec and are up
to the client to interpret.
-}
data ModelHint where
    ModelHint ::
        { name :: Maybe Text
        {- ^ A hint for a model name.
        The client SHOULD treat this as a substring of a model name; for example:
        - `claude-3-5-sonnet` should match `claude-3-5-sonnet-20241022`
        - `sonnet` should match `claude-3-5-sonnet-20241022`, `claude-3-sonnet-20240229`, etc.
        - `claude` should match any Claude model
        The client MAY also map the string to a different provider's model name
        or a different model family, as long as it fills a similar niche.
        -}
        } ->
        ModelHint
    deriving stock (Show, Eq, Generic)

$(deriveJSON mcpParseOpts ''ModelHint)

{- | The server's preferences for model selection, requested of the client during sampling.

Because LLMs can vary along multiple dimensions, choosing the "best" model is
rarely straightforward. Different models excel in different areas—some are
faster but less capable, others are more capable but more expensive, and so
on. This interface allows servers to express their priorities across multiple
dimensions to help clients make an appropriate selection for their use case.

These preferences are always advisory. The client MAY ignore them. It is also
up to the client to decide how to interpret these preferences and how to
balance them against other considerations.
-}
data ModelPreferences = ModelPreferences
    { hints :: Maybe [ModelHint]
    {- ^ Optional hints to use for model selection.
    If multiple hints are specified, the client MUST evaluate them in order
    (such that the first match is taken).
    The client SHOULD prioritize these hints over the numeric priorities, but
    MAY still use the priorities to select from ambiguous matches.
    -}
    , costPriority :: Maybe Double
    {- ^ How much to prioritize cost when selecting a model. A value of 0 means cost
    is not important, while a value of 1 means cost is the most important
    factor. Range: 0.0 to 1.0
    -}
    , speedPriority :: Maybe Double
    {- ^ How much to prioritize sampling speed (latency) when selecting a model. A
    value of 0 means speed is not important, while a value of 1 means speed is
    the most important factor. Range: 0.0 to 1.0
    -}
    , intelligencePriority :: Maybe Double
    {- ^ How much to prioritize intelligence and capabilities when selecting a
    model. A value of 0 means intelligence is not important, while a value of 1
    means intelligence is the most important factor. Range: 0.0 to 1.0
    -}
    }
    deriving stock (Show, Eq, Generic)

$(deriveJSON mcpParseOpts ''ModelPreferences)

{- | Include context options for sampling.

A request to include context from one or more MCP servers (including the caller),
to be attached to the prompt. The client MAY ignore this request.
-}
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

{- | Describes a message issued to or received from an LLM API.

Used in sampling requests to represent the conversation history
that should be sent to the language model.
-}
data SamplingMessage = SamplingMessage
    { role :: Role
    , content :: Content
    }
    deriving stock (Show, Eq, Generic)

$(deriveJSON mcpParseOpts ''SamplingMessage)

{- | Client capability for root directory management.

Present if the client supports listing roots (directories or files
that the server can operate on).
-}
data RootsCapability where
    RootsCapability ::
        { listChanged :: Maybe Bool
        -- ^ Whether the client supports notifications for changes to the roots list.
        } ->
        RootsCapability
    deriving stock (Show, Eq, Generic)

$(deriveJSON mcpParseOpts ''RootsCapability)

{- | Server capability for prompt management.

Present if the server offers any prompt templates.
-}
data PromptsCapability where
    PromptsCapability ::
        { listChanged :: Maybe Bool
        -- ^ Whether this server supports notifications for changes to the prompt list.
        } ->
        PromptsCapability
    deriving stock (Show, Eq, Generic)

$(deriveJSON mcpParseOpts ''PromptsCapability)

{- | Server capability for resource management.

Present if the server offers any resources to read.
-}
data ResourcesCapability = ResourcesCapability
    { listChanged :: Maybe Bool
    -- ^ Whether this server supports notifications for changes to the resource list.
    , subscribe :: Maybe Bool
    -- ^ Whether this server supports subscribing to resource updates.
    }
    deriving stock (Show, Eq, Generic)

$(deriveJSON mcpParseOpts ''ResourcesCapability)

{- | Server capability for tool management.

Present if the server offers any tools to call.
-}
data ToolsCapability where
    ToolsCapability ::
        { listChanged :: Maybe Bool
        -- ^ Whether this server supports notifications for changes to the tool list.
        } ->
        ToolsCapability
    deriving stock (Show, Eq, Generic)

$(deriveJSON mcpParseOpts ''ToolsCapability)

{- | Server capability for argument autocompletion.

Present if the server supports argument autocompletion suggestions.
-}
data CompletionsCapability = CompletionsCapability
    deriving stock (Show, Eq, Generic)

instance ToJSON CompletionsCapability where
    toJSON _ = object []

instance FromJSON CompletionsCapability where
    parseJSON = withObject "CompletionsCapability" $ \_ -> pure CompletionsCapability

{- | Server capability for logging.

Present if the server supports sending log messages to the client.
-}
data LoggingCapability = LoggingCapability
    deriving stock (Show, Eq, Generic)

instance ToJSON LoggingCapability where
    toJSON _ = object []

instance FromJSON LoggingCapability where
    parseJSON = withObject "LoggingCapability" $ \_ -> pure LoggingCapability

{- | Client capability for LLM sampling.

Present if the client supports sampling from an LLM.
-}
data SamplingCapability = SamplingCapability
    deriving stock (Show, Eq, Generic)

instance ToJSON SamplingCapability where
    toJSON _ = object []

instance FromJSON SamplingCapability where
    parseJSON = withObject "SamplingCapability" $ \_ -> pure SamplingCapability

{- | Experimental, non-standard capabilities.

Experimental capabilities that the client or server supports.
This is not a closed set: any client or server can define its own,
additional capabilities.
-}
newtype ExperimentalCapability = ExperimentalCapability (Map Text Value)
    deriving stock (Show, Eq, Generic)
    deriving newtype (ToJSON, FromJSON)

{- | Client capability for elicitation.

Present if the client supports elicitation from the server.
-}
data ElicitationCapability = ElicitationCapability
    deriving stock (Show, Eq, Generic)

instance ToJSON ElicitationCapability where
    toJSON _ = object []

instance FromJSON ElicitationCapability where
    parseJSON = withObject "ElicitationCapability" $ \_ -> pure ElicitationCapability

{- | Capabilities a client may support.

Known capabilities are defined here, in this schema, but this is not a closed set:
any client can define its own, additional capabilities.
-}
data ClientCapabilities = ClientCapabilities
    { roots :: Maybe RootsCapability
    -- ^ Present if the client supports listing roots.
    , sampling :: Maybe SamplingCapability
    -- ^ Present if the client supports sampling from an LLM.
    , elicitation :: Maybe ElicitationCapability
    -- ^ Present if the client supports elicitation from the server.
    , experimental :: Maybe ExperimentalCapability
    -- ^ Experimental, non-standard capabilities that the client supports.
    }
    deriving stock (Show, Eq, Generic)

$(deriveJSON mcpParseOpts ''ClientCapabilities)

{- | Capabilities that a server may support.

Known capabilities are defined here, in this schema, but this is not a closed set:
any server can define its own, additional capabilities.
-}
data ServerCapabilities = ServerCapabilities
    { logging :: Maybe LoggingCapability
    -- ^ Present if the server supports sending log messages to the client.
    , prompts :: Maybe PromptsCapability
    -- ^ Present if the server offers any prompt templates.
    , resources :: Maybe ResourcesCapability
    -- ^ Present if the server offers any resources to read.
    , tools :: Maybe ToolsCapability
    -- ^ Present if the server offers any tools to call.
    , completions :: Maybe CompletionsCapability
    -- ^ Present if the server supports argument autocompletion suggestions.
    , experimental :: Maybe ExperimentalCapability
    -- ^ Experimental, non-standard capabilities that the server supports.
    }
    deriving stock (Show, Eq, Generic)

$(deriveJSON mcpParseOpts ''ServerCapabilities)

{- | Base interface for metadata with name (identifier) and title (display name) properties.

This is the base type for various entities that need both programmatic
identifiers and human-readable names.
-}
data BaseMetadata = BaseMetadata
    { name :: Text
    {- ^ Intended for programmatic or logical use, but used as a display name in past specs
    or fallback (if title isn't present).
    -}
    , title :: Maybe Text
    {- ^ Intended for UI and end-user contexts — optimized to be human-readable and easily understood,
    even by those unfamiliar with domain-specific terminology.
    If not provided, the name should be used for display (except for Tool,
    where `annotations.title` should be given precedence over using `name`,
    if present).
    -}
    }
    deriving stock (Show, Eq, Generic)

$(deriveJSON mcpParseOpts ''BaseMetadata)

{- | Describes the name and version of an MCP implementation.

Used to identify the client or server software in initialization.
-}
data Implementation = Implementation
    { name :: Text
    , version :: Text
    , title :: Maybe Text
    -- ^ Optional title for UI representation.
    }
    deriving stock (Show, Eq, Generic)

$(deriveJSON mcpParseOpts ''Implementation)

{- | Represents a root directory or file that the server can operate on.

Roots allow servers to ask for specific directories or files to operate on.
A common example for roots is providing a set of repositories or directories
a server should operate on.
-}
data Root = Root
    { uri :: Text
    {- ^ The URI identifying the root. This *must* start with file:// for now.
    This restriction may be relaxed in future versions of the protocol to allow
    other URI schemes.
    -}
    , name :: Maybe Text
    {- ^ An optional name for the root. This can be used to provide a human-readable
    identifier for the root, which may be useful for display purposes or for
    referencing the root in other parts of the application.
    -}
    , _meta :: Maybe Metadata
    -- ^ Optional metadata field.
    }
    deriving stock (Show, Eq, Generic)

$(deriveJSON mcpParseOpts ''Root)

{- | Base result type.

All MCP result types include optional metadata that can be used
to attach additional information to responses.
-}
data Result where
    Result ::
        { _meta :: Maybe Metadata
        {- ^ This result property is reserved by the protocol to allow clients and servers
        to attach additional metadata to their responses.
        -}
        } ->
        Result
    deriving stock (Show, Eq, Generic)

$(deriveJSON mcpParseOpts ''Result)
