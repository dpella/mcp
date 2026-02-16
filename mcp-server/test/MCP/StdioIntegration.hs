{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module:      MCP.StdioIntegration
License:     MPL-2.0
Maintainer:  <matti@dpella.io>, <lobo@dpella.io>

Integration tests for the MCP stdio transport.
Tests serveStdio by creating in-process pipes and exercising
the full protocol over them.
-}
module MCP.StdioIntegration where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception (bracket)
import Data.Aeson (FromJSON, toJSON)
import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Map qualified as Map
import Data.Text (Text)
import MCP.Protocol
import MCP.TestServer (
    availableResourceTemplates,
    completionValues,
    csvTestData,
    initializeTestState,
    mb_handler_finalize,
    processHandlers,
 )
import MCP.TestUtils
import MCP.Types
import MCP.Server (
    MCPServerState (..),
    serveStdio,
 )
import System.IO (Handle, hClose, hFlush, hSetBuffering, BufferMode(..))
import System.Process (createPipe)
import Test.Hspec

-- * Stdio Integration Tests

-- | Complete stdio integration test suite
stdioIntegrationSpec :: Spec
stdioIntegrationSpec = describe "Stdio Transport" $ do
    initializationSpec
    pingSpec
    toolSpec
    resourceSpec
    resourceTemplateStdioSpec
    promptSpec
    completionStdioSpec
    loggingSpec
    errorSpec

-- ** Test Helpers

-- | Create initial server state for stdio tests (no JWT/user)
createStdioServerState :: MCPServerState
createStdioServerState =
    MCPServerState
        { mcp_server_initialized = False
        , mcp_handler_state = initializeTestState
        , mcp_handler_init = Nothing -- No user in stdio mode
        , mcp_handler_finalize = mb_handler_finalize
        , mcp_client_capabilities = Nothing
        , mcp_log_level = Nothing
        , mcp_pending_responses = mempty
        , mcp_pending_responses_next = 1
        , mcp_server_capabilities =
            ServerCapabilities
                { logging = Just LoggingCapability
                , prompts = Just (PromptsCapability{listChanged = Nothing})
                , resources = Just (ResourcesCapability{listChanged = Nothing, subscribe = Nothing})
                , tools = Just (ToolsCapability{listChanged = Just True})
                , completions = Just CompletionsCapability
                , experimental = Nothing
                }
        , mcp_implementation = Implementation "test-server-stdio" "1.0.0" Nothing
        , mcp_instructions = Nothing
        , mcp_process_handlers = processHandlers
        }

-- | Bracket that creates pipes, starts the stdio server in a background
-- thread, and yields the client-side handles for reading/writing.
withStdioServer ::
    ((Handle, Handle) -> IO a) ->
    IO a
withStdioServer f = do
    -- Pipe for server reading (client writes here)
    (client_to_server_read, client_to_server_write) <- createPipe
    -- Pipe for server writing (client reads here)
    (server_to_client_read, server_to_client_write) <- createPipe

    hSetBuffering client_to_server_write LineBuffering
    hSetBuffering server_to_client_read LineBuffering

    bracket
        ( do
            tid <- forkIO $ serveStdio client_to_server_read server_to_client_write createStdioServerState
            -- Small delay to let the server start its loop
            threadDelay 10000
            return tid
        )
        ( \tid -> do
            killThread tid
            hClose client_to_server_write
            hClose client_to_server_read
            hClose server_to_client_write
            hClose server_to_client_read
        )
        (\_ -> f (server_to_client_read, client_to_server_write))

-- | Send a JSON-RPC message to the server via the write handle
sendMsg :: (Aeson.ToJSON a) => Handle -> a -> IO ()
sendMsg h msg = do
    BSL.hPut h (Aeson.encode msg)
    BSL.hPut h "\n"
    hFlush h

-- | Read a JSON-RPC response from the server via the read handle
recvResponse :: Handle -> IO JSONRPCResponse
recvResponse h = do
    line <- BS.hGetLine h
    case Aeson.eitherDecodeStrict' line of
        Right resp -> return resp
        Left err -> fail $ "Failed to parse response: " <> err

-- | Read a JSON-RPC error from the server via the read handle
recvError :: Handle -> IO JSONRPCError
recvError h = do
    line <- BS.hGetLine h
    case Aeson.eitherDecodeStrict' line of
        Right err -> return err
        Left parse_err -> fail $ "Failed to parse error response: " <> parse_err

-- | Read a JSON-RPC response and extract a typed result
recvTypedResponse :: (FromJSON a) => Handle -> IO (RequestId, a)
recvTypedResponse h = do
    JSONRPCResponse _rpc req_id res_val <- recvResponse h
    case Aeson.fromJSON res_val of
        Aeson.Success val -> return (req_id, val)
        Aeson.Error err -> fail $ "Failed to parse typed result: " <> err

-- | Initialize the server (send initialize + initialized notification)
initializeServer :: Handle -> Handle -> IO ()
initializeServer h_read h_write = do
    -- Send initialize request
    sendMsg h_write (toJSON createInitializeRequest)
    (_, InitializeResult{protocolVersion = pv}) <- recvTypedResponse h_read
    pv `shouldBe` pROTOCOL_VERSION

    -- Send initialized notification
    sendMsg h_write (toJSON createInitializedNotification)

-- ** Test Specs

initializationSpec :: Spec
initializationSpec = describe "Initialization" $ do
    it "completes the initialization handshake" $ do
        withStdioServer $ \(h_read, h_write) -> do
            sendMsg h_write (toJSON createInitializeRequest)
            (req_id, InitializeResult{protocolVersion = pv}) <- recvTypedResponse h_read
            req_id `shouldBe` toRequestId (1 :: Int)
            pv `shouldBe` pROTOCOL_VERSION

            -- Send initialized notification (no response expected)
            sendMsg h_write (toJSON createInitializedNotification)

pingSpec :: Spec
pingSpec = describe "Ping" $ do
    it "handles ping after initialization" $ do
        withStdioServer $ \(h_read, h_write) -> do
            initializeServer h_read h_write
            sendMsg h_write (toJSON createPingRequest)
            resp <- recvResponse h_read
            let JSONRPCResponse _ _ res_val = resp
            res_val `shouldBe` Aeson.Object mempty

toolSpec :: Spec
toolSpec = describe "Tools" $ do
    it "lists available tools" $ do
        withStdioServer $ \(h_read, h_write) -> do
            initializeServer h_read h_write
            sendMsg h_write (toJSON $ createListToolsRequest 2)
            (_, ListToolsResult{tools = ts}) <- recvTypedResponse h_read
            let tool_names = fmap (\Tool{name = n} -> n) ts
            tool_names `shouldMatchList` ["addition-tool", "constant-msg-tool"]

    it "calls the addition tool" $ do
        withStdioServer $ \(h_read, h_write) -> do
            initializeServer h_read h_write
            sendMsg h_write (toJSON $ createCallToolRequest 2 "addition-tool" [("arg1", toJSON (5 :: Int)), ("arg2", toJSON (7 :: Int))])
            (_, CallToolResult{structuredContent = structured}) <- recvTypedResponse h_read
            case structured of
                Just m -> Map.lookup "result" m `shouldBe` Just (toJSON (12 :: Int))
                Nothing -> expectationFailure "Expected structured content"

    it "calls the constant message tool" $ do
        withStdioServer $ \(h_read, h_write) -> do
            initializeServer h_read h_write
            sendMsg h_write (toJSON $ createCallToolRequest 2 "constant-msg-tool" [])
            (_, CallToolResult{structuredContent = structured}) <- recvTypedResponse h_read
            case structured of
                Just m -> Map.lookup "result" m `shouldBe` Just (toJSON ("Hello, World!" :: Text))
                Nothing -> expectationFailure "Expected structured content"

    it "returns error for non-existent tool" $ do
        withStdioServer $ \(h_read, h_write) -> do
            initializeServer h_read h_write
            sendMsg h_write (toJSON $ createCallToolRequest 2 "nonexistent-tool" [])
            JSONRPCError _ _ err_info <- recvError h_read
            code err_info `shouldBe` 404

resourceSpec :: Spec
resourceSpec = describe "Resources" $ do
    it "lists available resources" $ do
        withStdioServer $ \(h_read, h_write) -> do
            initializeServer h_read h_write
            sendMsg h_write (toJSON $ createListResourcesRequest 2)
            (_, ListResourcesResult{resources = rs}) <- recvTypedResponse h_read
            let resource_uris = fmap (\Resource{uri = u} -> u) rs
            resource_uris `shouldMatchList` ["resource://example/document", "resource://example/data", "resource://example/image"]

    it "reads a text resource" $ do
        withStdioServer $ \(h_read, h_write) -> do
            initializeServer h_read h_write
            sendMsg h_write (toJSON $ createReadResourceRequest 2 "resource://example/document")
            (_, ReadResourceResult{contents = cs}) <- recvTypedResponse h_read
            case cs of
                [TextResource (TextResourceContents{text = t})] -> t `shouldBe` csvTestData
                _ -> expectationFailure "Expected single text resource"

    it "returns error for non-existent resource" $ do
        withStdioServer $ \(h_read, h_write) -> do
            initializeServer h_read h_write
            sendMsg h_write (toJSON $ createReadResourceRequest 2 "resource://nonexistent")
            JSONRPCError _ _ err_info <- recvError h_read
            code err_info `shouldBe` 404

resourceTemplateStdioSpec :: Spec
resourceTemplateStdioSpec = describe "Resource Templates" $ do
    it "lists available resource templates" $ do
        withStdioServer $ \(h_read, h_write) -> do
            initializeServer h_read h_write
            sendMsg h_write (toJSON $ createListResourceTemplatesRequest 2)
            (_, ListResourceTemplatesResult{resourceTemplates = ts}) <- recvTypedResponse h_read
            length ts `shouldBe` length availableResourceTemplates

promptSpec :: Spec
promptSpec = describe "Prompts" $ do
    it "lists available prompts" $ do
        withStdioServer $ \(h_read, h_write) -> do
            initializeServer h_read h_write
            sendMsg h_write (toJSON $ createPromptListRequest 2)
            (_, ListPromptsResult{prompts = ps}) <- recvTypedResponse h_read
            let prompt_names = fmap (\Prompt{name = n} -> n) ps
            prompt_names `shouldMatchList` ["code-review"]

    it "gets a prompt with arguments" $ do
        withStdioServer $ \(h_read, h_write) -> do
            initializeServer h_read h_write
            let args = Map.fromList [("code", "def foo():\n    return 42")]
            sendMsg h_write (toJSON $ createGetPromptRequest 2 "code-review" (Just args))
            (_, GetPromptResult{description = desc, messages = msgs}) <- recvTypedResponse h_read
            desc `shouldBe` Just "A prompt to request a code review from the LLM"
            length msgs `shouldBe` 2
            fmap (\PromptMessage{role = r} -> r) msgs `shouldBe` [User, Assistant]

    it "returns error for non-existent prompt" $ do
        withStdioServer $ \(h_read, h_write) -> do
            initializeServer h_read h_write
            sendMsg h_write (toJSON $ createGetPromptRequest 2 "nonexistent" Nothing)
            JSONRPCError _ _ err_info <- recvError h_read
            code err_info `shouldBe` 404

completionStdioSpec :: Spec
completionStdioSpec = describe "Completions" $ do
    it "returns completions for known prompt argument" $ do
        withStdioServer $ \(h_read, h_write) -> do
            initializeServer h_read h_write
            sendMsg h_write (toJSON $ createCompleteRequest 2 "code-review" "code" "")
            (_, CompleteResult{completion = CompletionResult{values = vals}}) <- recvTypedResponse h_read
            vals `shouldBe` completionValues

    it "returns filtered completions with prefix" $ do
        withStdioServer $ \(h_read, h_write) -> do
            initializeServer h_read h_write
            sendMsg h_write (toJSON $ createCompleteRequest 2 "code-review" "code" "def")
            (_, CompleteResult{completion = CompletionResult{values = vals}}) <- recvTypedResponse h_read
            vals `shouldBe` ["def foo():"]

    it "returns empty completions for unknown prompt" $ do
        withStdioServer $ \(h_read, h_write) -> do
            initializeServer h_read h_write
            sendMsg h_write (toJSON $ createCompleteRequest 2 "nonexistent" "arg" "")
            (_, CompleteResult{completion = CompletionResult{values = vals}}) <- recvTypedResponse h_read
            vals `shouldBe` []

loggingSpec :: Spec
loggingSpec = describe "Logging" $ do
    it "handles logging/setLevel" $ do
        withStdioServer $ \(h_read, h_write) -> do
            initializeServer h_read h_write
            sendMsg h_write (toJSON $ createSetLevelRequest 2 Debug)
            resp <- recvResponse h_read
            let JSONRPCResponse _ _ res_val = resp
            res_val `shouldBe` Aeson.Null

errorSpec :: Spec
errorSpec = describe "Error Handling" $ do
    it "rejects tools/list before initialization" $ do
        withStdioServer $ \(h_read, h_write) -> do
            sendMsg h_write (toJSON $ createListToolsRequest 1)
            JSONRPCError _ _ err_info <- recvError h_read
            code err_info `shouldBe` sERVER_NOT_INITIALIZED

    it "returns method_not_found for unknown methods" $ do
        withStdioServer $ \(h_read, h_write) -> do
            initializeServer h_read h_write
            sendMsg h_write (toJSON $ createJSONRPCRequest Nothing (2 :: Int) "unknown/method" (Aeson.object []))
            JSONRPCError _ _ err_info <- recvError h_read
            code err_info `shouldBe` mETHOD_NOT_FOUND

    it "returns parse error for malformed JSON" $ do
        withStdioServer $ \(h_read, h_write) -> do
            BS.hPut h_write "this is not json\n"
            hFlush h_write
            JSONRPCError _ _ err_info <- recvError h_read
            code err_info `shouldBe` pARSE_ERROR

    it "handles EOF gracefully" $ do
        withStdioServer $ \(h_read, h_write) -> do
            initializeServer h_read h_write
            sendMsg h_write (toJSON createPingRequest)
            _ <- recvResponse h_read
            -- Closing the write handle signals EOF to the server
            hClose h_write
            -- Server should exit cleanly (no exception)
            -- Wait a bit for the server to process EOF
            threadDelay 50000
            return ()
