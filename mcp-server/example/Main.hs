{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | An example MCP server demonstrating the full feature set of the @mcp@ library.

= Quick start

@
cabal run mcp-example
@

The server starts on @http:\/\/localhost:8080\/mcp@ and prints a JWT bearer token
to stdout. Use this token in the @Authorization@ header for all requests.

= Features demonstrated

This single-file example covers every major capability of the library:

* __JWT authentication__ — a fresh key is generated on startup and a sample
  token is printed so you can test immediately.
* __Tools__ (via 'ToolHandler' \/ 'withToolHandlers') — @echo@, @add@, and
  @current-time@ show text results, structured output, and IO in handlers.
* __Resources__ — static text and JSON resources served by URI.
* __Resource templates__ — a @resource:\/\/example\/users\/{userId}@ URI template
  that dynamically generates user profiles.
* __Prompts__ — a @summarize@ prompt with a required @text@ argument, returning
  a multi-message conversation.
* __Completions__ — auto-complete suggestions for prompt argument values.
* __Logging__ — the server declares @LoggingCapability@ and supports
  @logging\/setLevel@.
* __Lifecycle hooks__ — @handleInit@ and @handleFinalize@ show how to run
  setup\/teardown logic around each session and request.
* __Server instructions__ — a free-text string sent to clients on initialization.

= Architecture overview

1. Define your user type (@ExampleUser@) and per-session state (@ExampleState@).
2. Wire them into the library via type family instances:
   @type instance MCPHandlerState = ExampleState@ and
   @type instance MCPHandlerUser = ExampleUser@.
3. Build a 'ProcessHandlers' record — start from 'defaultProcessHandlers' and
   override the capabilities you need.  Use 'withToolHandlers' for a convenient
   tool-registration API.
4. Create an 'MCPServerState' via 'initMCPServerState', wrap it in an 'MVar',
   and serve it with @serveWithContext (Proxy \@MCPAPI) ctx (mcpAPI stateVar)@.
-}
module Main where

import Control.Concurrent.MVar (newMVar)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, object, toJSON, (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock (getCurrentTime)
import GHC.Generics (Generic)
import MCP.Server
import Network.Wai.Handler.Warp qualified as Warp
import Servant (Context (..), Proxy (..), serveWithContext)
import Servant.Auth.Server qualified as AuthServer
import System.Environment (getArgs, lookupEnv)
import System.IO (hSetBuffering, stderr, stdin, stdout, BufferMode (..))

-- ---------------------------------------------------------------------------
-- User and state types
-- ---------------------------------------------------------------------------

-- | The authenticated user carried inside JWT claims.
--
-- The library requires instances of 'AuthServer.ToJWT' and 'AuthServer.FromJWT'
-- so it can encode\/decode your user in bearer tokens.  Deriving 'Generic' plus
-- 'Aeson.ToJSON' \/ 'Aeson.FromJSON' gives you those for free via the default
-- methods.
data ExampleUser = ExampleUser
    { userId :: Text
    , userName :: Text
    }
    deriving (Eq, Show, Generic)

instance Aeson.ToJSON ExampleUser
instance Aeson.FromJSON ExampleUser
instance AuthServer.ToJWT ExampleUser
instance AuthServer.FromJWT ExampleUser

-- | Per-session handler state.
--
-- This is threaded through every 'MCPServerT' handler via 'StateT'.  You can
-- put anything you need here — database connections, caches, etc.  For this
-- example we just track which user is active.
newtype ExampleState = ExampleState
    { currentUser :: Maybe Text
    }

-- | Wire the type families so the library knows our concrete types.
--
-- Every application using @mcp@ must provide exactly one instance for each.
-- @MCPHandlerState@ is the state available in handlers, and @MCPHandlerUser@
-- is the user type decoded from JWT tokens.
type instance MCPHandlerState = ExampleState
type instance MCPHandlerUser = ExampleUser

-- ---------------------------------------------------------------------------
-- Tools
-- ---------------------------------------------------------------------------

-- | All tools provided by the example server.
--
-- We use the 'toolHandler' smart constructor for convenience — it fills in
-- optional fields (title, output schema, annotations, metadata) with 'Nothing'.
-- Then 'withToolHandlers' wires them into a 'ProcessHandlers' record,
-- automatically generating the @tools\/list@ and @tools\/call@ dispatch logic.
exampleTools :: [ToolHandler]
exampleTools = [echoTool, addTool, currentTimeTool]

-- | A simple tool that echoes its input back.
--
-- Demonstrates 'toolTextResult' — the simplest way to return text from a tool.
echoTool :: ToolHandler
echoTool =
    toolHandler
        "echo"
        (Just "Echoes the input message back to the caller")
        (InputSchema "object" (Just $ Map.fromList [("message", object ["type" .= ("string" :: Text)])]) (Just ["message"]))
        $ \args -> do
            let msg = args >>= Map.lookup "message" >>= asText
            case msg of
                Just txt -> return $ ProcessSuccess $ toolTextResult [txt]
                Nothing -> return $ ProcessSuccess $ toolTextError "Missing or invalid 'message' argument"

-- | A tool that adds two numbers.
--
-- Demonstrates returning structured output via 'CallToolResult' with a
-- 'structuredContent' map alongside the human-readable text content.
addTool :: ToolHandler
addTool =
    toolHandler
        "add"
        (Just "Adds two numbers and returns the sum")
        ( InputSchema
            "object"
            ( Just $
                Map.fromList
                    [ ("a", object ["type" .= ("number" :: Text)])
                    , ("b", object ["type" .= ("number" :: Text)])
                    ]
            )
            (Just ["a", "b"])
        )
        $ \args -> do
            let ma = args >>= Map.lookup "a" >>= asNumber
            let mb = args >>= Map.lookup "b" >>= asNumber
            case (ma, mb) of
                (Just a, Just b) -> do
                    let s = a + b
                    return $
                        ProcessSuccess $
                            CallToolResult
                                { content = [TextBlock $ TextContent "text" ("The sum is: " <> T.pack (show s)) Nothing Nothing]
                                , structuredContent = Just $ Map.fromList [("result", toJSON s)]
                                , isError = Just False
                                , _meta = Nothing
                                }
                _ -> return $ ProcessSuccess $ toolTextError "Arguments 'a' and 'b' must be numbers"

-- | A tool that returns the current UTC time.
--
-- Demonstrates performing IO inside a tool handler via 'liftIO'.
currentTimeTool :: ToolHandler
currentTimeTool =
    toolHandler
        "current-time"
        (Just "Returns the current UTC time")
        (InputSchema "object" Nothing Nothing)
        $ \_ -> do
            now <- liftIO getCurrentTime
            return $ ProcessSuccess $ toolTextResult [T.pack (show now)]

-- ---------------------------------------------------------------------------
-- Resources
-- ---------------------------------------------------------------------------

-- | Static resources advertised by the server.
--
-- Clients discover these via @resources\/list@ and fetch their content with
-- @resources\/read@.  Each resource has a stable URI, a human-readable name,
-- and an optional MIME type.
exampleResources :: [Resource]
exampleResources =
    [ Resource
        { uri = "resource://example/readme"
        , name = "readme"
        , title = Just "Example README"
        , description = Just "A sample text document"
        , mimeType = Just "text/plain"
        , size = Nothing
        , annotations = Nothing
        , _meta = Nothing
        }
    , Resource
        { uri = "resource://example/config"
        , name = "config"
        , title = Just "Example Configuration"
        , description = Just "A sample JSON configuration"
        , mimeType = Just "application/json"
        , size = Nothing
        , annotations = Nothing
        , _meta = Nothing
        }
    ]

-- | Handle @resources\/read@ requests.
--
-- Routes by URI to return the appropriate content.  Unknown URIs get a 404
-- error via 'ProcessRPCError'.  URIs matching the @resource:\/\/example\/users\/{id}@
-- template are handled dynamically.
readResource :: ReadResourceParams -> MCPServerT (ProcessResult ReadResourceResult)
readResource (ReadResourceParams req_uri) =
    case req_uri of
        "resource://example/readme" ->
            ok req_uri "text/plain" "This is the example MCP server README.\nIt demonstrates tools, resources, prompts, and more."
        "resource://example/config" ->
            ok req_uri "application/json" (T.pack $ show $ object ["version" .= ("1.0" :: Text), "debug" .= False])
        _ ->
            -- Handle resource template matches (see exampleResourceTemplates)
            case T.stripPrefix "resource://example/users/" req_uri of
                Just uid | not (T.null uid) ->
                    ok req_uri "application/json" (T.pack $ show $ object ["userId" .= uid, "name" .= ("User " <> uid)])
                _ -> return $ ProcessRPCError 404 "Resource not found"
  where
    ok u mime txt =
        return $
            ProcessSuccess $
                ReadResourceResult
                    { contents = [TextResource $ TextResourceContents u txt (Just mime) Nothing]
                    , _meta = Nothing
                    }

-- ---------------------------------------------------------------------------
-- Resource Templates
-- ---------------------------------------------------------------------------

-- | Resource templates advertised by the server.
--
-- Templates use RFC 6570 URI syntax (e.g. @{userId}@).  Clients can discover
-- them via @resources\/templates\/list@ and then construct concrete URIs to pass
-- to @resources\/read@.
exampleResourceTemplates :: [ResourceTemplate]
exampleResourceTemplates =
    [ ResourceTemplate
        { name = "user-profile"
        , title = Just "User Profile"
        , uriTemplate = "resource://example/users/{userId}"
        , description = Just "Returns a JSON profile for a given user ID"
        , mimeType = Just "application/json"
        , annotations = Nothing
        , _meta = Nothing
        }
    ]

-- ---------------------------------------------------------------------------
-- Prompts
-- ---------------------------------------------------------------------------

-- | Prompt templates advertised by the server.
--
-- Prompts are reusable conversation starters.  Each prompt declares its
-- arguments (name, description, required flag) so clients can present a form
-- and fill in the values before calling @prompts\/get@.
examplePrompts :: [Prompt]
examplePrompts =
    [ Prompt
        { name = "summarize"
        , title = Just "Summarize Text"
        , description = Just "Asks the LLM to produce a concise summary of the provided text"
        , arguments =
            Just
                [ PromptArgument
                    { name = "text"
                    , title = Just "Text to summarize"
                    , description = Just "The text content to summarize"
                    , required = Just True
                    }
                ]
        , _meta = Nothing
        }
    ]

-- | Handle @prompts\/get@ requests.
--
-- Returns a list of 'PromptMessage' values that form a conversation.
-- The 'User' message includes the argument value; the 'Assistant' message
-- primes the model with a helpful starting response.
getPrompt :: GetPromptParams -> MCPServerT (ProcessResult GetPromptResult)
getPrompt = \case
    GetPromptParams{name = "summarize", arguments = Just args} -> do
        let txt = Map.findWithDefault "" "text" args
        let msgs =
                [ PromptMessage
                    { role = User
                    , content = TextBlock $ TextContent "text" ("Please summarize the following text:\n\n" <> txt) Nothing Nothing
                    }
                , PromptMessage
                    { role = Assistant
                    , content = TextBlock $ TextContent "text" "I'll provide a concise summary of the text." Nothing Nothing
                    }
                ]
        return $
            ProcessSuccess $
                GetPromptResult
                    { description = Just "A prompt to summarize text"
                    , messages = msgs
                    , _meta = Nothing
                    }
    _ -> return $ ProcessRPCError 404 "Prompt not found"

-- ---------------------------------------------------------------------------
-- Completions
-- ---------------------------------------------------------------------------

-- | Handle @completion\/complete@ requests.
--
-- Returns auto-complete suggestions for prompt argument values.  The client
-- sends the argument name and a partial value; the server responds with
-- matching completions.  Here we return a fixed set of suggestions for the
-- @text@ argument of the @summarize@ prompt.
handleComplete :: CompleteParams -> MCPServerT (ProcessResult CompleteResult)
handleComplete (CompleteParams _ref (CompletionArgument arg_name _) _ctx) =
    case arg_name of
        "text" ->
            return $
                ProcessSuccess $
                    CompleteResult
                        { completion =
                            CompletionResult
                                { values = ["Hello world", "Lorem ipsum dolor sit amet", "The quick brown fox"]
                                , total = Just 3
                                , hasMore = Just False
                                }
                        , _meta = Nothing
                        }
        _ ->
            return $
                ProcessSuccess $
                    CompleteResult
                        { completion = CompletionResult{values = [], total = Just 0, hasMore = Just False}
                        , _meta = Nothing
                        }

-- ---------------------------------------------------------------------------
-- Process handlers
-- ---------------------------------------------------------------------------

-- | Assemble all handlers into a single 'ProcessHandlers' record.
--
-- Start from 'defaultProcessHandlers' (all fields 'Nothing'), override the
-- capabilities you want, then apply 'withToolHandlers' to wire up tool
-- listing and dispatch automatically.
--
-- Note that 'withToolHandlers' overwrites @listToolsHandler@ and
-- @callToolHandler@, so it should be the outermost wrapper.
exampleHandlers :: ProcessHandlers
exampleHandlers =
    withToolHandlers exampleTools $
        defaultProcessHandlers
            { listResourcesHandler = Just $ \_ ->
                return $
                    ProcessSuccess $
                        ListResourcesResult
                            { resources = exampleResources
                            , nextCursor = Nothing
                            , _meta = Nothing
                            }
            , readResourceHandler = Just readResource
            , listPromptsHandler = Just $ \_ ->
                return $
                    ProcessSuccess $
                        ListPromptsResult
                            { prompts = examplePrompts
                            , nextCursor = Nothing
                            , _meta = Nothing
                            }
            , getPromptHandler = Just getPrompt
            , listResourceTemplatesHandler = Just $ \_ ->
                return $
                    ProcessSuccess $
                        ListResourceTemplatesResult
                            { resourceTemplates = exampleResourceTemplates
                            , nextCursor = Nothing
                            , _meta = Nothing
                            }
            , completeHandler = Just handleComplete
            }

-- ---------------------------------------------------------------------------
-- Server capabilities
-- ---------------------------------------------------------------------------

-- | Declare which MCP capabilities this server supports.
--
-- The client receives these during the @initialize@ handshake so it knows
-- which methods are available.  Setting a capability to 'Nothing' means the
-- server does not support it; setting @listChanged = Just True@ advertises
-- that the server may send @notifications\/tools\/list_changed@ at runtime.
exampleCapabilities :: ServerCapabilities
exampleCapabilities =
    ServerCapabilities
        { logging = Just LoggingCapability
        , prompts = Just PromptsCapability{listChanged = Nothing}
        , resources = Just ResourcesCapability{listChanged = Nothing, subscribe = Nothing}
        , tools = Just ToolsCapability{listChanged = Just True}
        , completions = Just CompletionsCapability
        , experimental = Nothing
        }

-- ---------------------------------------------------------------------------
-- Lifecycle hooks
-- ---------------------------------------------------------------------------

-- | Called once when the client sends @initialize@.
--
-- Use this to set up per-session state — open database connections, load
-- configuration, etc.  The authenticated user is provided so you can
-- personalise the session.
handleInit :: ExampleUser -> ExampleState -> IO ExampleState
handleInit user st = do
    putStrLn $ "Session initialized for user: " <> T.unpack (userId user)
    return st{currentUser = Just (userId user)}

-- | Called after every request.
--
-- Use this for cleanup — close cursors, flush buffers, etc.  In this example
-- we simply clear the current user.
handleFinalize :: ExampleState -> IO ExampleState
handleFinalize st = return st{currentUser = Nothing}

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

-- | Entry point.
--
-- Supports two modes:
--
-- * __HTTP mode__ (default): starts a Warp server with JWT authentication.
-- * __Stdio mode__ (@--stdio@): reads JSON-RPC from stdin, writes to stdout.
--   No authentication is needed.  Debug output goes to stderr.
--
-- @
-- cabal run mcp-example            # HTTP on port 8080
-- cabal run mcp-example -- --stdio # stdio transport
-- @
main :: IO ()
main = do
    args <- getArgs
    if "--stdio" `elem` args
        then mainStdio
        else mainHTTP

-- | Run in stdio transport mode.
--
-- Reads JSON-RPC messages from stdin and writes responses to stdout.
-- There is no JWT authentication — the caller is trusted.
-- Debug/log output goes to stderr so it doesn't interfere with the protocol.
mainStdio :: IO ()
mainStdio = do
    hSetBuffering stderr LineBuffering

    let impl = Implementation "mcp-example" "0.1.0" (Just "Example MCP Server")
    let server_instructions = Just "This is an example MCP server. It provides echo, add, and current-time tools, sample resources, a summarize prompt, and completions."
    let initial_state =
            (initMCPServerState
                (ExampleState Nothing) -- initial handler state
                Nothing -- no handler init in stdio mode (no JWT user)
                (Just handleFinalize) -- lifecycle: after each request
                exampleCapabilities -- what we support
                impl -- server name + version
                server_instructions -- instructions for clients
                exampleHandlers -- all our handlers
            ){mcp_log_level = Just Debug}

    serveStdio stdin stdout initial_state

-- | Run in HTTP mode with JWT authentication.
mainHTTP :: IO ()
mainHTTP = do
    -- Use line buffering so output is visible immediately when piped.
    hSetBuffering stdout LineBuffering

    -- Read port from PORT env var, default to 8080.
    port <- maybe 8080 read <$> lookupEnv "PORT"

    -- Server metadata shown to clients during initialization.
    let impl = Implementation "mcp-example" "0.1.0" (Just "Example MCP Server")

    -- Free-text instructions sent to clients on initialization.
    let server_instructions = Just "This is an example MCP server. It provides echo, add, and current-time tools, sample resources, a summarize prompt, and completions."

    -- Create the initial server state.  initMCPServerState sets sensible
    -- defaults: not yet initialized, log level Warning, empty pending responses.
    -- We override the log level to Debug so all requests/responses are logged.
    let initial_state =
            (initMCPServerState
                (ExampleState Nothing) -- initial handler state
                (Just handleInit) -- lifecycle: on initialize
                (Just handleFinalize) -- lifecycle: after each request
                exampleCapabilities -- what we support
                impl -- server name + version
                server_instructions -- instructions for clients
                exampleHandlers -- all our handlers
            ){mcp_log_level = Just Debug}
    state_var <- newMVar initial_state

    -- Generate a fresh JWK key for signing tokens.
    key <- AuthServer.generateKey
    let jwt_cfg = AuthServer.defaultJWTSettings key
    let cookie_cfg = AuthServer.defaultCookieSettings
    -- The Servant context carries the auth configuration used by servant-auth.
    let ctx = cookie_cfg :. jwt_cfg :. EmptyContext

    -- Mint a JWT for a test user so you can start making requests immediately.
    let test_user = ExampleUser "example-user" "Example User"
    token_result <- AuthServer.makeJWT test_user jwt_cfg Nothing
    case token_result of
        Left err -> putStrLn $ "Failed to generate JWT: " <> show err
        Right token -> do
            let token_str = TE.decodeUtf8 $ BSL.toStrict token
            putStrLn "=== Example MCP Server ==="
            putStrLn $ "Listening on http://localhost:" <> show port <> "/mcp"
            putStrLn ""
            putStrLn "Bearer token for testing:"
            putStrLn $ T.unpack token_str
            putStrLn ""
            putStrLn "Example curl:"
            putStrLn $ "  curl -X POST http://localhost:" <> show port <> "/mcp \\"
            putStrLn "    -H 'Content-Type: application/json' \\"
            putStrLn $ "    -H 'Authorization: Bearer " <> T.unpack token_str <> "' \\"
            putStrLn "    -d '{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{\"protocolVersion\":\"2025-06-18\",\"capabilities\":{},\"clientInfo\":{\"name\":\"curl\",\"version\":\"1.0\"}}}'"

    -- Build the Servant application and start Warp.
    let app = serveWithContext (Proxy @MCPAPI) ctx (mcpAPI state_var)
    Warp.run port app

-- ---------------------------------------------------------------------------
-- JSON value helpers
-- ---------------------------------------------------------------------------

-- | Extract a 'Text' from a JSON 'Value', returning 'Nothing' for non-strings.
asText :: Value -> Maybe Text
asText (Aeson.String t) = Just t
asText _ = Nothing

-- | Extract a 'Double' from a JSON 'Value', returning 'Nothing' for non-numbers.
asNumber :: Value -> Maybe Double
asNumber (Aeson.Number n) = Just (realToFrac n)
asNumber _ = Nothing
