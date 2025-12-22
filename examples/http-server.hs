{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
Module      : Main
Description : Example HTTP MCP Server
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC

This example demonstrates how to run the MCP server over HTTP transport.
The server will expose the MCP API at POST /mcp

To test:

1. Compile: cabal build mcp-http
2. Run: cabal run mcp-http
3. Send JSON-RPC requests to: http://localhost:\<port\>/mcp

Example request:
curl -X POST http://localhost:8080/mcp \
  -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","id":1,"method":"ping"}'

Command line options:
cabal run mcp-http -- --port 8080 --log
cabal run mcp-http -- --oauth --oauth-traces-only  # Filter to show only OAuth events
-}
module Main where

import Control.Concurrent.STM (newTVarIO)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Functor.Contravariant (contramap)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Network.Wai.Handler.Warp (run)
import Options.Applicative
import Plow.Logging (IOTracer (..), Tracer (..), filterTracer)
import Plow.Logging.Async (withAsyncHandleTracer)
import System.IO (hFlush, stdout)

import MCP.Protocol
import MCP.Server
import MCP.Server.Auth
import MCP.Server.HTTP (DemoOAuthBundle (..), HTTPServerConfig (..), mcpAppWithOAuth, mkDemoOAuthBundleFromText, runServerHTTP)
import MCP.Server.HTTP.AppEnv (AppEnv (..), runAppM)
import MCP.Trace.HTTP (HTTPTrace (..))
import MCP.Trace.Types (MCPTrace (..), isOAuthTrace, renderMCPTrace)
import MCP.Types
import Servant.Auth.Server (defaultJWTSettings, generateKey)
import Servant.OAuth2.IDP.Auth.Demo (DemoCredentialEnv (..), defaultDemoCredentialStore)
import Servant.OAuth2.IDP.Store.InMemory (defaultExpiryConfig, newOAuthTVarEnv)

-- | A no-op tracer that discards all trace events
nullIOTracer :: IOTracer a
nullIOTracer = IOTracer (Tracer (\_ -> pure ()))

-- | Command line options
data Options = Options
    { optPort :: Int
    , optEnableLogging :: Bool
    , optEnableOAuth :: Bool
    , optBaseUrl :: Maybe String
    , optOAuthTracesOnly :: Bool
    }
    deriving (Show)

-- | Parser for command line options
optionsParser :: Parser Options
optionsParser =
    Options
        <$> option
            auto
            ( long "port"
                <> short 'p'
                <> metavar "PORT"
                <> Options.Applicative.value 8080
                <> help "Port to run the HTTP server on (default: 8080)"
            )
        <*> switch
            ( long "log"
                <> short 'l'
                <> help "Enable request/response logging"
            )
        <*> switch
            ( long "oauth"
                <> short 'o'
                <> help "Enable OAuth authentication (demo mode)"
            )
        <*> optional
            ( strOption
                ( long "base-url"
                    <> metavar "URL"
                    <> help "Base URL for OAuth endpoints (default: http://localhost:PORT)"
                )
            )
        <*> switch
            ( long "oauth-traces-only"
                <> help "Filter traces to show only OAuth-related events"
            )

-- | Full parser with help
opts :: ParserInfo Options
opts =
    info
        (optionsParser <**> helper)
        ( fullDesc
            <> progDesc "Run an MCP server over HTTP transport"
            <> header "mcp-http - HTTP MCP Server Example"
        )

-- | Example MCP Server implementation (copied from Main.hs)
instance MCPServer MCPServerM where
    -- handleListResources inherits the default implementation

    handleReadResource _params = do
        let textContent = TextResourceContents{uri = "example://hello", text = "Hello from MCP Haskell HTTP server!", mimeType = Just "text/plain", _meta = Nothing}
        let content = TextResource textContent
        return $ ReadResourceResult{contents = [content], _meta = Nothing}

    -- handleListResourceTemplates inherits the default implementation

    -- handleListPrompts inherits the default implementation

    handleGetPrompt _params = do
        let textContent = TextContent{text = "Hello HTTP prompt!", textType = "text", annotations = Nothing, _meta = Nothing}
        let content = TextContentType textContent
        let message = PromptMessage{role = User, content = content}
        return $ GetPromptResult{messages = [message], description = Nothing, _meta = Nothing}

    handleListTools _params = do
        let getCurrentDateTool =
                Tool
                    { name = "getCurrentDate"
                    , title = Nothing
                    , description = Just "Get the current date and time via HTTP"
                    , inputSchema = InputSchema "object" Nothing Nothing
                    , outputSchema = Nothing
                    , annotations = Nothing
                    , _meta = Nothing
                    }
        return $ ListToolsResult{tools = [getCurrentDateTool], nextCursor = Nothing, _meta = Nothing}

    handleCallTool CallToolParams{name = toolName} = do
        case toolName of
            "getCurrentDate" -> do
                currentTime <- liftIO getCurrentTime
                let dateStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S UTC (via HTTP)" currentTime
                let textContent = TextContent{text = T.pack dateStr, textType = "text", annotations = Nothing, _meta = Nothing}
                let content = TextContentType textContent
                return $ CallToolResult{content = [content], structuredContent = Nothing, isError = Nothing, _meta = Nothing}
            _ -> do
                let textContent = TextContent{text = "Tool not found", textType = "text", annotations = Nothing, _meta = Nothing}
                let content = TextContentType textContent
                return $ CallToolResult{content = [content], structuredContent = Nothing, isError = Just True, _meta = Nothing}

    -- handleComplete inherits the default implementation

    handleSetLevel _params = do
        -- handleSetLevel is a no-op in this example
        -- In a real implementation, this would configure the tracer's log level
        return ()

main :: IO ()
main = do
    Options{..} <- execParser opts

    when optEnableLogging $ putStrLn "Request/Response logging: enabled"
    when optOAuthTracesOnly $ putStrLn "Trace filtering: OAuth events only"

    let serverInfo =
            Implementation
                { name = "mcp-haskell-http-example"
                , title = Nothing
                , version = "0.1.0"
                }

    let resourcesCap =
            ResourcesCapability
                { subscribe = Just False
                , listChanged = Just False
                }
    let promptsCap =
            PromptsCapability
                { listChanged = Just False
                }
    let toolsCap =
            ToolsCapability
                { listChanged = Just False
                }

    let capabilities =
            ServerCapabilities
                { resources = Just resourcesCap
                , prompts = Just promptsCap
                , tools = Just toolsCap
                , completions = Nothing
                , logging = Nothing
                , experimental = Nothing
                }

    let baseUrl = T.pack $ fromMaybe ("http://localhost:" ++ show optPort) optBaseUrl
        mcpOAuthConfig =
            if optEnableOAuth
                then
                    let bundle = case mkDemoOAuthBundleFromText baseUrl of
                            Just b -> b
                            Nothing -> Prelude.error $ "Invalid base URL: " ++ T.unpack baseUrl
                        baseMCPConfig = bundleMCPConfig bundle
                     in Just $
                            baseMCPConfig
                                { oauthProviders =
                                    [ OAuthProvider
                                        { providerName = "demo"
                                        , clientId = "demo-client"
                                        , clientSecret = Just "demo-secret"
                                        , authorizationEndpoint = baseUrl <> "/authorize"
                                        , tokenEndpoint = baseUrl <> "/token"
                                        , userInfoEndpoint = Nothing
                                        , scopes = ["mcp:read", "mcp:write"]
                                        , grantTypes = [OAuthAuthorizationCode]
                                        , requiresPKCE = True -- MCP requires PKCE
                                        , metadataEndpoint = Nothing
                                        }
                                    ]
                                , -- Override demo defaults for example
                                  demoUserIdTemplate = Just "demo-user-{clientId}"
                                , demoEmailDomain = "demo.example.com"
                                , demoUserName = "Demo User"
                                , authorizationSuccessTemplate =
                                    "Demo Authorization Successful!\n\n"
                                        <> "Redirect to: {redirectUri}?code={code}{state}\n\n"
                                        <> "This is a demo server. In production, this would redirect automatically."
                                }
                else Nothing

    let config =
            HTTPServerConfig
                { httpPort = optPort
                , httpBaseUrl = baseUrl -- Configurable base URL
                , httpServerInfo = serverInfo
                , httpCapabilities = capabilities
                , httpEnableLogging = optEnableLogging
                , httpMCPOAuthConfig = mcpOAuthConfig
                , httpJWK = Nothing -- Will be auto-generated
                , httpProtocolVersion = mcpProtocolVersion -- Current MCP protocol version
                , httpProtectedResourceMetadata = Nothing -- Will be auto-generated from baseUrl
                }

    putStrLn $ "MCP endpoint available at: POST " ++ T.unpack baseUrl ++ "/mcp"
    putStrLn $ "Protected Resource Metadata: GET " ++ T.unpack baseUrl ++ "/.well-known/oauth-protected-resource"

    if optEnableOAuth
        then do
            putStrLn ""
            putStrLn "Demo Credentials:"
            putStrLn "  Username: demo    Password: demo123"
            putStrLn "  Username: admin   Password: admin456"
            putStrLn ""
            putStrLn "OAuth Demo Flow (Discovery endpoints available at /.well-known/*):"
            putStrLn "1. Generate PKCE code verifier and challenge"
            putStrLn "2. Open authorization URL in browser:"
            putStrLn $ "   " ++ T.unpack baseUrl ++ "/authorize?response_type=code&client_id=demo-client&redirect_uri=http://localhost:3000/callback&code_challenge=YOUR_CHALLENGE&code_challenge_method=S256&scope=mcp:read%20mcp:write"
            putStrLn "3. Exchange authorization code for token:"
            putStrLn $ "   curl -X POST " ++ T.unpack baseUrl ++ "/token \\"
            putStrLn "     -H \"Content-Type: application/x-www-form-urlencoded\" \\"
            putStrLn "     -d \"grant_type=authorization_code&code=AUTH_CODE&code_verifier=YOUR_VERIFIER\""
            putStrLn "4. Use access token for MCP requests:"
            putStrLn $ "   curl -X POST " ++ T.unpack baseUrl ++ "/mcp \\"
            putStrLn "     -H \"Authorization: Bearer ACCESS_TOKEN\" \\"
            putStrLn "     -H \"Content-Type: application/json\" \\"
            putStrLn "     -d '{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\"}'"
        else do
            putStrLn ""
            putStrLn "Example test command:"
            putStrLn $ "curl -X POST " ++ T.unpack baseUrl ++ "/mcp \\"
            putStrLn "  -H \"Content-Type: application/json\" \\"
            putStrLn "  -d '{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\"}'"

    putStrLn ""
    hFlush stdout

    -- Setup async tracing to stdout with 1000-message buffer
    withAsyncHandleTracer stdout 1000 $ \textTracer -> do
        -- Apply OAuth-only filter if requested
        let mcpTracer = contramap renderMCPTrace textTracer
            filteredTracer =
                if optOAuthTracesOnly
                    then
                        let unIOTracer (IOTracer t) = t
                         in IOTracer $ filterTracer isOAuthTrace (unIOTracer mcpTracer)
                    else mcpTracer
            httpTracer = contramap MCPHttp filteredTracer

        if optEnableOAuth
            then do
                -- Use mcpApp pattern for OAuth mode (following demoMcpApp implementation)
                -- Generate JWK for JWT signing
                jwk <- generateKey
                let jwtSettings = defaultJWTSettings jwk

                -- Initialize in-memory OAuth state storage
                oauthEnv <- newOAuthTVarEnv defaultExpiryConfig

                -- Create demo credential environment
                let authEnv = DemoCredentialEnv defaultDemoCredentialStore

                -- Initialize server state
                stateVar <- newTVarIO $ initialServerState (httpCapabilities config)

                -- Create AppEnv with configured settings (including stateVar)
                let bundle = case mkDemoOAuthBundleFromText baseUrl of
                        Just b -> b
                        Nothing -> Prelude.error $ "Invalid base URL: " ++ T.unpack baseUrl
                    oauthCfgEnv = bundleEnv bundle -- Use OAuthEnv from bundle
                    oauthTracer = contramap HTTPOAuth httpTracer -- Route OAuth traces through HTTP tracer
                    appEnv =
                        AppEnv
                            { envOAuth = oauthEnv
                            , envAuth = authEnv
                            , envConfig = config
                            , envTracer = httpTracer
                            , envOAuthEnv = oauthCfgEnv
                            , envOAuthTracer = oauthTracer
                            , envJWT = jwtSettings
                            , envServerState = stateVar
                            , envTimeProvider = Nothing -- Use real IO time
                            }

                -- Use mcpAppWithOAuth for full OAuth support (polymorphic version)
                let app = mcpAppWithOAuth (runAppM appEnv) jwtSettings
                run (httpPort config) app
            else
                -- Use runServerHTTP for non-OAuth mode
                runServerHTTP config httpTracer
