{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson hiding (Error, Result)
import Data.Aeson.KeyMap qualified as KM
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)
import JSONRPC ()
import MCP.Protocol
import MCP.Types
import Test.Hspec
import Prelude hiding (error, id, minimum, maximum)
import Prelude qualified

-- | Helper to extract a JSON Object, failing on non-objects.
asObject :: Value -> KM.KeyMap Value
asObject (Object o) = o
asObject _ = Prelude.error "expected object"

main :: IO ()
main = hspec $ do
    -- -----------------------------------------------------------------
    -- MCP.Types
    -- -----------------------------------------------------------------
    describe "MCP.Types" $ do
        describe "Role" $ do
            it "serialises User as \"user\"" $
                toJSON User `shouldBe` String "user"
            it "serialises Assistant as \"assistant\"" $
                toJSON Assistant `shouldBe` String "assistant"
            it "round-trips User" $
                decode (encode User) `shouldBe` Just User
            it "round-trips Assistant" $
                decode (encode Assistant) `shouldBe` Just Assistant
            it "rejects unknown role" $
                (decode "\"admin\"" :: Maybe Role) `shouldBe` Nothing

        describe "LoggingLevel" $ do
            it "round-trips all levels" $ do
                let levels = [Emergency, Alert, Critical, Error, Warning, Notice, Info, Debug]
                mapM_ (\l -> decode (encode l) `shouldBe` Just l) levels
            it "preserves ordering" $
                Emergency < Debug `shouldBe` True
            it "serialises Debug as \"debug\"" $
                toJSON Debug `shouldBe` String "debug"

        describe "Cursor" $ do
            it "round-trips" $ do
                let c = Cursor "abc123"
                decode (encode c) `shouldBe` Just c

        describe "ProgressToken" $ do
            it "round-trips a string token" $ do
                let t = ProgressToken (String "tok-1")
                decode (encode t) `shouldBe` Just t
            it "round-trips a numeric token" $ do
                let t = ProgressToken (Number 42)
                decode (encode t) `shouldBe` Just t

        describe "Annotations" $ do
            it "round-trips with all fields" $ do
                let ann = Annotations (Just [User, Assistant]) (Just 0.8) (Just "2025-01-12T15:00:58Z")
                decode (encode ann) `shouldBe` Just ann
            it "omits Nothing fields" $ do
                let ann = Annotations Nothing Nothing Nothing
                    obj = asObject (toJSON ann)
                KM.member "audience" obj `shouldBe` False
                KM.member "priority" obj `shouldBe` False

        describe "TextContent" $ do
            it "round-trips" $ do
                let tc = TextContent "text" "hello" Nothing Nothing
                decode (encode tc) `shouldBe` Just tc
            it "serialises type as \"text\"" $ do
                let tc = TextContent "text" "hello" Nothing Nothing
                    obj = asObject (toJSON tc)
                KM.lookup "type" obj `shouldBe` Just (String "text")
            it "rejects wrong type tag" $
                (decode "{\"type\":\"image\",\"text\":\"x\"}" :: Maybe TextContent) `shouldBe` Nothing

        describe "ImageContent" $ do
            it "round-trips" $ do
                let ic = ImageContent "image" "base64data" "image/png" Nothing Nothing
                decode (encode ic) `shouldBe` Just ic
            it "serialises data field (not data')" $ do
                let ic = ImageContent "image" "base64data" "image/png" Nothing Nothing
                    obj = asObject (toJSON ic)
                KM.member "data" obj `shouldBe` True
                KM.member "data'" obj `shouldBe` False

        describe "AudioContent" $ do
            it "round-trips" $ do
                let ac = AudioContent "audio" "base64audio" "audio/wav" Nothing Nothing
                decode (encode ac) `shouldBe` Just ac

        describe "ResourceContents" $ do
            it "round-trips TextResource" $ do
                let rc = TextResource (TextResourceContents "file:///a.txt" "contents" (Just "text/plain") Nothing)
                decode (encode rc) `shouldBe` Just rc
            it "round-trips BlobResource" $ do
                let rc = BlobResource (BlobResourceContents "file:///b.bin" "AQID" (Just "application/octet-stream") Nothing)
                decode (encode rc) `shouldBe` Just rc

        describe "EmbeddedResource" $ do
            it "round-trips" $ do
                let er = EmbeddedResource "resource" (TextResource (TextResourceContents "uri" "txt" Nothing Nothing)) Nothing Nothing
                decode (encode er) `shouldBe` Just er
            it "rejects wrong type tag" $
                (decode "{\"type\":\"text\",\"resource\":{}}" :: Maybe EmbeddedResource) `shouldBe` Nothing

        describe "ResourceLink" $ do
            it "round-trips with minimal fields" $ do
                let rl = ResourceLink "resource_link" "file:///x" "myfile" Nothing Nothing Nothing Nothing Nothing Nothing
                decode (encode rl) `shouldBe` Just rl
            it "round-trips with all fields" $ do
                let ann = Annotations (Just [User]) Nothing Nothing
                    rl = ResourceLink "resource_link" "file:///x" "myfile" (Just "My File") (Just "A file") (Just "text/plain") (Just 1024) (Just ann) Nothing
                decode (encode rl) `shouldBe` Just rl

        describe "Content (union)" $ do
            it "parses text content" $ do
                let js = "{\"type\":\"text\",\"text\":\"hello\"}"
                case decode js :: Maybe Content of
                    Just (TextContentType _) -> pure ()
                    other -> expectationFailure $ "Expected TextContentType, got: " <> show other
            it "parses image content" $ do
                let js = "{\"type\":\"image\",\"data\":\"abc\",\"mimeType\":\"image/png\"}"
                case decode js :: Maybe Content of
                    Just (ImageContentType _) -> pure ()
                    other -> expectationFailure $ "Expected ImageContentType, got: " <> show other

        describe "ContentBlock (union)" $ do
            it "parses resource_link as ResourceLinkBlock" $ do
                let js = "{\"type\":\"resource_link\",\"uri\":\"file:///x\",\"name\":\"f\"}"
                case decode js :: Maybe ContentBlock of
                    Just (ResourceLinkBlock _) -> pure ()
                    other -> expectationFailure $ "Expected ResourceLinkBlock, got: " <> show other

        describe "Resource" $ do
            it "round-trips with minimal fields" $ do
                let r = Resource "file:///a" "a" Nothing Nothing Nothing Nothing Nothing Nothing
                decode (encode r) `shouldBe` Just r
            it "omits Nothing fields" $ do
                let r = Resource "file:///a" "a" Nothing Nothing Nothing Nothing Nothing Nothing
                    obj = asObject (toJSON r)
                KM.member "description" obj `shouldBe` False
                KM.member "mimeType" obj `shouldBe` False

        describe "ResourceTemplate" $ do
            it "round-trips" $ do
                let rt = ResourceTemplate "tmpl" Nothing "file:///{path}" Nothing Nothing Nothing Nothing
                decode (encode rt) `shouldBe` Just rt

        describe "ResourceReference" $ do
            it "serialises type as \"ref/resource\"" $ do
                let rr = ResourceReference "ref/resource" "file:///a"
                    obj = asObject (toJSON rr)
                KM.lookup "type" obj `shouldBe` Just (String "ref/resource")
            it "round-trips" $ do
                let rr = ResourceReference "ref/resource" "file:///a"
                decode (encode rr) `shouldBe` Just rr
            it "rejects wrong type" $
                (decode "{\"type\":\"ref/prompt\",\"uri\":\"x\"}" :: Maybe ResourceReference) `shouldBe` Nothing

        describe "Tool" $ do
            it "round-trips" $ do
                let sch = InputSchema "object" (Just (Map.fromList [("x", object ["type" .= ("integer" :: String)])])) (Just ["x"])
                    t = Tool "add" Nothing (Just "Add numbers") sch Nothing Nothing Nothing
                decode (encode t) `shouldBe` Just t

        describe "InputSchema" $ do
            it "serialises type as \"object\"" $ do
                let s = InputSchema "object" Nothing Nothing
                    obj = asObject (toJSON s)
                KM.lookup "type" obj `shouldBe` Just (String "object")
            it "round-trips with properties" $ do
                let s = InputSchema "object" (Just (Map.singleton "a" (String "test"))) (Just ["a"])
                decode (encode s) `shouldBe` Just s

        describe "Prompt" $ do
            it "round-trips" $ do
                let p = Prompt "greet" Nothing (Just "A greeting") (Just [PromptArgument "name" Nothing Nothing (Just True)]) Nothing
                decode (encode p) `shouldBe` Just p

        describe "PromptReference" $ do
            it "serialises type as \"ref/prompt\"" $ do
                let pr = PromptReference "ref/prompt" "greet" Nothing
                    obj = asObject (toJSON pr)
                KM.lookup "type" obj `shouldBe` Just (String "ref/prompt")
            it "round-trips" $ do
                let pr = PromptReference "ref/prompt" "greet" (Just "Greeting")
                decode (encode pr) `shouldBe` Just pr

        describe "IncludeContext" $ do
            it "round-trips AllServers" $
                decode (encode AllServers) `shouldBe` Just AllServers
            it "round-trips None" $
                decode (encode None) `shouldBe` Just None
            it "round-trips ThisServer" $
                decode (encode ThisServer) `shouldBe` Just ThisServer

        describe "ModelHint" $ do
            it "round-trips" $ do
                let h = ModelHint (Just "claude-3-5-sonnet")
                decode (encode h) `shouldBe` Just h

        describe "ModelPreferences" $ do
            it "round-trips" $ do
                let mp = ModelPreferences (Just [ModelHint (Just "sonnet")]) (Just 0.5) (Just 0.8) (Just 0.9)
                decode (encode mp) `shouldBe` Just mp

        describe "Capabilities" $ do
            it "round-trips ClientCapabilities" $ do
                let cc = ClientCapabilities (Just (RootsCapability (Just True))) (Just SamplingCapability) Nothing Nothing
                decode (encode cc) `shouldBe` Just cc
            it "round-trips ServerCapabilities" $ do
                let sc = ServerCapabilities (Just LoggingCapability) (Just (PromptsCapability (Just True))) Nothing (Just (ToolsCapability Nothing)) (Just CompletionsCapability) Nothing
                decode (encode sc) `shouldBe` Just sc
            it "empty capabilities round-trip" $ do
                let cc = ClientCapabilities Nothing Nothing Nothing Nothing
                decode (encode cc) `shouldBe` Just cc

        describe "Implementation" $ do
            it "round-trips" $ do
                let impl = Implementation "test-server" "1.0.0" (Just "Test Server")
                decode (encode impl) `shouldBe` Just impl
            it "omits Nothing title" $ do
                let impl = Implementation "srv" "0.1" Nothing
                    obj = asObject (toJSON impl)
                KM.member "title" obj `shouldBe` False

        describe "Root" $ do
            it "round-trips" $ do
                let r = Root "file:///home/user" (Just "Home") Nothing
                decode (encode r) `shouldBe` Just r

        describe "Result" $ do
            it "round-trips with no metadata" $ do
                let r = Result Nothing
                decode (encode r) `shouldBe` Just r
            it "round-trips with metadata" $ do
                let m = Metadata (Map.singleton "key" (String "val"))
                    r = Result (Just m)
                decode (encode r) `shouldBe` Just r

    -- -----------------------------------------------------------------
    -- MCP.Protocol
    -- -----------------------------------------------------------------
    describe "MCP.Protocol" $ do
        describe "pROTOCOL_VERSION" $ do
            it "is \"2025-06-18\"" $
                pROTOCOL_VERSION `shouldBe` "2025-06-18"

        describe "InitializeRequest" $ do
            it "round-trips" $ do
                let initParams = InitializeParams "2025-06-18" (ClientCapabilities Nothing Nothing Nothing Nothing) (Implementation "client" "1.0" Nothing)
                    initReq = InitializeRequest (RequestId (Number 1)) initParams
                decode (encode initReq) `shouldBe` Just initReq
            it "serialises with method \"initialize\"" $ do
                let initParams = InitializeParams "2025-06-18" (ClientCapabilities Nothing Nothing Nothing Nothing) (Implementation "c" "1" Nothing)
                    initReq = InitializeRequest (RequestId (Number 1)) initParams
                    obj = asObject (toJSON initReq)
                KM.lookup "method" obj `shouldBe` Just (String "initialize")

        describe "PingRequest" $ do
            it "round-trips" $ do
                let pingReq = PingRequest (RequestId (Number 2)) Nothing
                decode (encode pingReq) `shouldBe` Just pingReq
            it "has method \"ping\"" $ do
                let obj = asObject (toJSON (PingRequest (RequestId (Number 1)) Nothing))
                KM.lookup "method" obj `shouldBe` Just (String "ping")

        describe "ListToolsRequest" $ do
            it "has method \"tools/list\"" $ do
                let obj = asObject (toJSON (ListToolsRequest (RequestId (Number 1)) Nothing))
                KM.lookup "method" obj `shouldBe` Just (String "tools/list")

        describe "CallToolRequest" $ do
            it "round-trips" $ do
                let callParams = CallToolParams "add" (Just (Map.fromList [("a", Number 1), ("b", Number 2)]))
                    callReq = CallToolRequest (RequestId (Number 3)) callParams
                decode (encode callReq) `shouldBe` Just callReq
            it "has method \"tools/call\"" $ do
                let callParams = CallToolParams "echo" Nothing
                    obj = asObject (toJSON (CallToolRequest (RequestId (Number 1)) callParams))
                KM.lookup "method" obj `shouldBe` Just (String "tools/call")

        describe "ListResourcesRequest" $ do
            it "has method \"resources/list\"" $ do
                let obj = asObject (toJSON (ListResourcesRequest (RequestId (Number 1)) Nothing))
                KM.lookup "method" obj `shouldBe` Just (String "resources/list")

        describe "ReadResourceRequest" $ do
            it "round-trips" $ do
                let readReq = ReadResourceRequest (RequestId (Number 4)) (ReadResourceParams "file:///a.txt")
                decode (encode readReq) `shouldBe` Just readReq

        describe "ListPromptsRequest" $ do
            it "has method \"prompts/list\"" $ do
                let obj = asObject (toJSON (ListPromptsRequest (RequestId (Number 1)) Nothing))
                KM.lookup "method" obj `shouldBe` Just (String "prompts/list")

        describe "GetPromptRequest" $ do
            it "round-trips" $ do
                let gpParams = GetPromptParams "greet" (Just (Map.singleton "name" "Alice"))
                    gpReq = GetPromptRequest (RequestId (Number 5)) gpParams
                decode (encode gpReq) `shouldBe` Just gpReq

        describe "SetLevelRequest" $ do
            it "round-trips" $ do
                let slReq = SetLevelRequest (RequestId (Number 6)) (SetLevelParams Warning)
                decode (encode slReq) `shouldBe` Just slReq

        describe "CompleteRequest" $ do
            it "round-trips" $ do
                let compParams = CompleteParams (PromptRef (PromptReference "ref/prompt" "greet" Nothing)) (CompletionArgument "name" "Al") Nothing
                    compReq = CompleteRequest (RequestId (Number 7)) compParams
                decode (encode compReq) `shouldBe` Just compReq

        describe "Response types" $ do
            it "InitializeResult round-trips" $ do
                let sc = ServerCapabilities Nothing Nothing Nothing (Just (ToolsCapability Nothing)) Nothing Nothing
                    res = InitializeResult "2025-06-18" sc (Implementation "srv" "1.0" Nothing) (Just "Use tools") Nothing
                decode (encode res) `shouldBe` Just res

            it "ListToolsResult round-trips" $ do
                let sch = InputSchema "object" Nothing Nothing
                    t = Tool "echo" Nothing (Just "Echo back") sch Nothing Nothing Nothing
                    res = ListToolsResult [t] Nothing Nothing
                decode (encode res) `shouldBe` Just res

            it "CallToolResult round-trips" $ do
                let res = CallToolResult [TextBlock (TextContent "text" "ok" Nothing Nothing)] Nothing Nothing Nothing
                decode (encode res) `shouldBe` Just res

            it "CallToolResult with isError round-trips" $ do
                let res = CallToolResult [TextBlock (TextContent "text" "fail" Nothing Nothing)] Nothing (Just True) Nothing
                decode (encode res) `shouldBe` Just res

            it "ListResourcesResult round-trips" $ do
                let r = Resource "file:///a" "a" Nothing Nothing Nothing Nothing Nothing Nothing
                    res = ListResourcesResult [r] Nothing Nothing
                decode (encode res) `shouldBe` Just res

            it "ReadResourceResult round-trips" $ do
                let rc = TextResource (TextResourceContents "file:///a" "hello" Nothing Nothing)
                    res = ReadResourceResult [rc] Nothing
                decode (encode res) `shouldBe` Just res

            it "CompleteResult round-trips" $ do
                let cr = CompletionResult ["Alice", "Alex"] (Just 2) Nothing
                    res = CompleteResult cr Nothing
                decode (encode res) `shouldBe` Just res

            it "GetPromptResult round-trips" $ do
                let pm = PromptMessage User (TextBlock (TextContent "text" "Hello!" Nothing Nothing))
                    res = GetPromptResult (Just "A prompt") [pm] Nothing
                decode (encode res) `shouldBe` Just res

        describe "Notification types" $ do
            it "InitializedNotification round-trips" $ do
                let n = InitializedNotification Nothing
                decode (encode n) `shouldBe` Just n
            it "InitializedNotification has correct method" $ do
                let obj = asObject (toJSON (InitializedNotification Nothing))
                KM.lookup "method" obj `shouldBe` Just (String "notifications/initialized")

            it "CancelledNotification round-trips" $ do
                let n = CancelledNotification (CancelledParams (RequestId (Number 5)) (Just "no longer needed"))
                decode (encode n) `shouldBe` Just n

            it "ProgressNotification round-trips" $ do
                let n = ProgressNotification (ProgressParams (ProgressToken (String "tok-1")) 0.5 (Just 1.0) (Just "halfway"))
                decode (encode n) `shouldBe` Just n

            it "LoggingMessageNotification round-trips" $ do
                let n = LoggingMessageNotification (LoggingMessageParams Info (String "server started") (Just "main"))
                decode (encode n) `shouldBe` Just n

            it "ResourceUpdatedNotification round-trips" $ do
                let n = ResourceUpdatedNotification (ResourceUpdatedParams "file:///a.txt")
                decode (encode n) `shouldBe` Just n

        describe "ClientRequest (union)" $ do
            it "parses an initialize request" $ do
                let initParams = InitializeParams "2025-06-18" (ClientCapabilities Nothing Nothing Nothing Nothing) (Implementation "c" "1" Nothing)
                    initReq = InitializeReq (InitializeRequest (RequestId (Number 1)) initParams)
                case decode (encode initReq) :: Maybe ClientRequest of
                    Just (InitializeReq _) -> pure ()
                    other -> expectationFailure $ "Expected InitializeReq, got: " <> show other

            it "parses a ping request" $ do
                let pingReq = PingReq (PingRequest (RequestId (Number 2)) Nothing)
                case decode (encode pingReq) :: Maybe ClientRequest of
                    Just (PingReq _) -> pure ()
                    other -> expectationFailure $ "Expected PingReq, got: " <> show other

            it "parses a tools/call request" $ do
                let callReq = CallToolReq (CallToolRequest (RequestId (Number 3)) (CallToolParams "echo" Nothing))
                case decode (encode callReq) :: Maybe ClientRequest of
                    Just (CallToolReq _) -> pure ()
                    other -> expectationFailure $ "Expected CallToolReq, got: " <> show other

        describe "ClientNotification (union)" $ do
            it "parses an initialized notification" $ do
                let n = InitializedNotif (InitializedNotification Nothing)
                case decode (encode n) :: Maybe ClientNotification of
                    Just (InitializedNotif _) -> pure ()
                    other -> expectationFailure $ "Expected InitializedNotif, got: " <> show other

        describe "ServerNotification (union)" $ do
            it "parses a logging message notification" $ do
                let n = LoggingMessageNotif (LoggingMessageNotification (LoggingMessageParams Debug (String "msg") Nothing))
                case decode (encode n) :: Maybe ServerNotification of
                    Just (LoggingMessageNotif _) -> pure ()
                    other -> expectationFailure $ "Expected LoggingMessageNotif, got: " <> show other

        describe "PrimitiveSchemaDefinition" $ do
            it "round-trips StringSchema" $ do
                let s = StringSchema "string" (Just "Name") Nothing Nothing (Just 100) (Just "email")
                decode (encode s) `shouldBe` Just s
            it "round-trips NumberSchema" $ do
                let s = NumberSchema "number" Nothing (Just "A number") (Just 0) (Just 100)
                decode (encode s) `shouldBe` Just s
            it "round-trips integer NumberSchema" $ do
                let s = NumberSchema "integer" Nothing Nothing Nothing Nothing
                decode (encode s) `shouldBe` Just s
            it "round-trips BooleanSchema" $ do
                let s = BooleanSchema "boolean" Nothing Nothing (Just True)
                decode (encode s) `shouldBe` Just s
            it "round-trips EnumSchema" $ do
                let s = EnumSchema "string" Nothing Nothing ["a", "b", "c"] (Just ["Alpha", "Beta", "Charlie"])
                decode (encode s) `shouldBe` Just s

        describe "ElicitParams" $ do
            it "round-trips" $ do
                let sch = Map.singleton "name" (StringSchema "string" (Just "Name") Nothing Nothing Nothing Nothing)
                    ep = ElicitParams "Enter your name" sch (Just ["name"])
                decode (encode ep) `shouldBe` Just ep
            it "wraps schema in object with type and properties" $ do
                let sch = Map.singleton "age" (NumberSchema "integer" Nothing Nothing Nothing Nothing)
                    ep = ElicitParams "Age?" sch Nothing
                    obj = asObject (toJSON ep)
                case KM.lookup "requestedSchema" obj of
                    Just (Object so) -> do
                        KM.lookup "type" so `shouldBe` Just (String "object")
                        KM.member "properties" so `shouldBe` True
                    other -> expectationFailure $ "Expected requestedSchema object, got: " <> show other

        describe "ElicitResult" $ do
            it "round-trips accept with content" $ do
                let res = ElicitResult "accept" (Just (Map.singleton "name" (String "Alice"))) Nothing
                decode (encode res) `shouldBe` Just res
            it "round-trips decline without content" $ do
                let res = ElicitResult "decline" Nothing Nothing
                decode (encode res) `shouldBe` Just res

    -- -----------------------------------------------------------------
    -- MCP.Aeson
    -- -----------------------------------------------------------------
    describe "MCP.Aeson" $ do
        describe "omitNothingFields" $ do
            it "omits Nothing fields from TH-derived types" $ do
                let impl = Implementation "srv" "1.0" Nothing
                    obj = asObject (toJSON impl)
                KM.member "title" obj `shouldBe` False
            it "includes Just fields" $ do
                let impl = Implementation "srv" "1.0" (Just "Server")
                    obj = asObject (toJSON impl)
                KM.lookup "title" obj `shouldBe` Just (String "Server")

        describe "_meta field" $ do
            it "serialises _meta as \"_meta\" (not modified)" $ do
                let r = Root "file:///a" Nothing (Just (Metadata (Map.singleton "k" (String "v"))))
                    obj = asObject (toJSON r)
                KM.member "_meta" obj `shouldBe` True
            it "omits _meta when Nothing" $ do
                let r = Root "file:///a" Nothing Nothing
                    obj = asObject (toJSON r)
                KM.member "_meta" obj `shouldBe` False
            it "parses _meta from JSON" $ do
                let js = "{\"uri\":\"file:///a\",\"_meta\":{\"k\":\"v\"}}"
                case decode js :: Maybe Root of
                    Just (Root _ _ meta) -> isNothing meta `shouldBe` False
                    Nothing -> expectationFailure "Failed to parse Root"

        describe "$sel: prefix stripping" $ do
            it "strips $sel: prefix for DuplicateRecordFields" $ do
                let sch = InputSchema "object" Nothing Nothing
                    t = Tool "echo" Nothing Nothing sch Nothing Nothing Nothing
                    obj = asObject (toJSON t)
                KM.member "name" obj `shouldBe` True
