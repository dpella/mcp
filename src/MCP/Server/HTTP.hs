{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module      : MCP.Server.HTTP
Description : MCP server implementation for HTTP communication
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC

This module provides MCP server implementation for HTTP communication.
-}
module MCP.Server.HTTP (
    -- * Server Runner
    runServerHTTP,
    HTTPServerConfig (..),

    -- * Demo Configuration Helpers
    defaultDemoOAuthConfig,
    defaultProtectedResourceMetadata,

    -- * Entry Points
    mcpApp,
    mcpAppWithOAuth,
    demoMcpApp,

    -- * API Types
    MCPAPI,
    FullAPI,
    OAuthAPI,

    -- * HTML Content Type
    HTML,

    -- * Login Types
    PendingAuthorization (..),
    LoginForm,
    LoginError (..),
    LoginResult (..),

    -- * HTML Rendering
    scopeToDescription,
    formatScopeDescriptions,

    -- * Handlers (for testing)
    mcpServerAuth,
) where

import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVarIO, writeTVar)
import Control.Monad (when)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ask, asks)
import Control.Monad.State.Strict (get, put)
import Data.Aeson (encode, fromJSON, object, toJSON, (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Functor.Contravariant (contramap)
import Data.Generics.Product (HasType, getTyped)
import Data.Generics.Sum.Typed (AsType, injectTyped)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.Generics (Generic)
import Network.HTTP.Media ((//), (/:))
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant (Context (..), Handler, Proxy (..), Server, ServerT, hoistServer, hoistServerWithContext, serve, serveWithContext, throwError)
import Servant.API (Accept (..), JSON, MimeRender (..), Post, ReqBody, (:<|>) (..), (:>))
import Servant.Auth.Server (Auth, AuthResult (..), CookieSettings, JWT, JWTSettings, ToJWT, defaultCookieSettings, defaultJWTSettings, generateKey)
import Servant.Server (err400, err401, err500, errBody, errHeaders)
import Servant.Server.Internal.Handler (runHandler)

import MCP.Protocol
import MCP.Server (MCPServer (..), MCPServerM, ServerConfig (..), ServerState (..), initialServerState, runMCPServer)
import MCP.Server.Auth (OAuthConfig (..), OAuthProvider (..), ProtectedResourceMetadata (..))
import MCP.Server.HTTP.AppEnv (AppEnv (..), HTTPServerConfig (..), runAppM)
import MCP.Trace.HTTP (HTTPTrace (..))
import MCP.Trace.Operation (OperationTrace (..))
import MCP.Trace.Server (ServerTrace (..))
import MCP.Types
import Plow.Logging (IOTracer (..), Tracer (..), traceWith)
import Servant.OAuth2.IDP.Auth.Backend (AuthBackend (..))
import Servant.OAuth2.IDP.Auth.Demo (AuthUser (..), DemoCredentialEnv (..), defaultDemoCredentialStore)
import Servant.OAuth2.IDP.LoginFlowError (LoginFlowError)
import Servant.OAuth2.IDP.Server (LoginForm, OAuthAPI, oauthServer)
import Servant.OAuth2.IDP.Store (OAuthStateStore (..))
import Servant.OAuth2.IDP.Store.InMemory (OAuthTVarEnv, defaultExpiryConfig, newOAuthTVarEnv)
import Servant.OAuth2.IDP.Types (AuthorizationError (..), ClientAuthMethod (..), CodeChallengeMethod (..), GrantType (..), OAuthErrorResponse (..), PendingAuthorization (..), RedirectUri (..), ResponseType (..), Scope (..), UserId (..), ValidationError (..), unUserId)

-- | HTML content type for Servant
data HTML

instance Accept HTML where
    contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML Text where
    mimeRender _ = LBS.fromStrict . TE.encodeUtf8

-- | Login error types
data LoginError
    = InvalidOAuthParameters Text
    | ClientNotRegistered Text
    | InvalidRedirectUri Text
    | SessionNotFound Text
    | SessionExpired
    | AuthenticationFailed
    | CookiesRequired
    | MissingFormFields [Text]
    deriving (Show, Eq)

-- | Login result
data LoginResult
    = LoginSuccess
        { resultAuthCode :: Text
        , resultRedirectUri :: Text
        , resultState :: Maybe Text
        }
    | LoginDenied
        { resultRedirectUri :: Text
        , resultState :: Maybe Text
        }
    | LoginFailure LoginError
    deriving (Show)

-- | Client info stored in server
data ClientInfo = ClientInfo
    { clientName :: Text
    , clientRedirectUris :: [RedirectUri]
    , clientGrantTypes :: [GrantType]
    , clientResponseTypes :: [ResponseType]
    , clientAuthMethod :: ClientAuthMethod
    }
    deriving (Show, Generic)

-- | MCP API definition for HTTP server (following the MCP transport spec)
type MCPAPI auths = Auth auths AuthUser :> "mcp" :> ReqBody '[JSON] Aeson.Value :> Post '[JSON] Aeson.Value

-- | Unprotected MCP API without authentication
type UnprotectedMCPAPI = "mcp" :> ReqBody '[JSON] Aeson.Value :> Post '[JSON] Aeson.Value

-- | Complete API with OAuth and configurable auth schemes
type CompleteAPI auths = OAuthAPI :<|> MCPAPI auths

-- | Full API with OAuth using JWT authentication
type FullAPI = OAuthAPI :<|> MCPAPI '[JWT]

-- -----------------------------------------------------------------------------
-- Polymorphic Entry Points (FR-046, FR-048)
-- -----------------------------------------------------------------------------

{- | MCP-only application (no OAuth constraints).

This entry point serves only the MCP API endpoint without OAuth endpoints.
Use this when you don't need OAuth authentication.

This is a simplified entry point that accepts a polymorphic natural transformation
and a polymorphic server implementation.

= Usage Example

@
import MCP.Server.HTTP (mcpApp)
import MCP.Server.HTTP.AppEnv (AppM, AppEnv, runAppM)
import Servant (ServerT)

myServer :: ServerT UnprotectedMCPAPI AppM
myServer = myMcpHandler

myApp :: AppEnv -> Application
myApp env = mcpApp (runAppM env) myServer
@
-}
mcpApp ::
    (forall a. m a -> Handler a) ->
    ServerT UnprotectedMCPAPI m ->
    Application
mcpApp runM serverImpl =
    serve
        (Proxy :: Proxy UnprotectedMCPAPI)
        (hoistServer (Proxy :: Proxy UnprotectedMCPAPI) runM serverImpl)

{- | MCP + OAuth application with full OAuth 2.1 support.

This entry point serves both the MCP API and OAuth endpoints.

Accepts a natural transformation to run the polymorphic monad `m` in Servant's Handler.
The monad `m` must satisfy all constraints needed by the OAuth server and MCP authentication.

= Usage Example

@
import MCP.Server.HTTP (mcpAppWithOAuth)
import MCP.Server.HTTP.AppEnv (AppM, AppEnv, runAppM)

-- Create AppEnv with OAuth state, auth backend, config, tracer, JWT settings
appEnv <- mkAppEnv ...

-- Create WAI Application with natural transformation
let app = mcpAppWithOAuth (runAppM appEnv)
@

= Type Parameter

The function is polymorphic over the monad `m`. The natural transformation
`(forall a. m a -> Handler a)` determines how to execute `m` actions in Servant's Handler.

This enables:

* Production: Use @AppM@ with real PostgreSQL/LDAP backends
* Testing: Use test monads with in-memory/mock backends
* Custom: Use your own monad stack with custom backends
-}
mcpAppWithOAuth ::
    forall m env e.
    ( MCPServer MCPServerM
    , OAuthStateStore m
    , AuthBackend m
    , AuthBackendUser m ~ OAuthUser m
    , ToJWT (OAuthUser m)
    , MonadIO m
    , MonadReader env m
    , MonadError e m
    , AsType ValidationError e
    , AsType AuthorizationError e
    , AsType LoginFlowError e
    , HasType HTTPServerConfig env
    , HasType (IOTracer HTTPTrace) env
    , HasType JWTSettings env
    , HasType (TVar ServerState) env
    ) =>
    (forall a. m a -> Handler a) ->
    JWTSettings ->
    Application
mcpAppWithOAuth runM jwtSettings =
    serveWithContext
        (Proxy :: Proxy FullAPI)
        authContext
        ( hoistServerWithContext
            (Proxy :: Proxy FullAPI)
            (Proxy :: Proxy '[CookieSettings, JWTSettings])
            runM
            fullServer
        )
  where
    authContext :: Context '[CookieSettings, JWTSettings]
    authContext = defaultCookieSettings :. jwtSettings :. EmptyContext

    fullServer :: ServerT FullAPI m
    fullServer = oauthServer :<|> mcpServerAuthPolymorphic

    -- Polymorphic version of mcpServerAuth that works in monad m
    mcpServerAuthPolymorphic :: AuthResult AuthUser -> Aeson.Value -> m Aeson.Value
    mcpServerAuthPolymorphic authResult requestValue = do
        config <- asks (getTyped @HTTPServerConfig)
        tracer <- asks (getTyped @(IOTracer HTTPTrace))
        stateVar <- asks (getTyped @(TVar ServerState))
        -- Call mcpServerAuth by lifting from Handler back to m
        result <- liftIO $ runHandler $ mcpServerAuth config tracer stateVar authResult requestValue
        case result of
            Left err -> throwError (injectTyped @AuthorizationError (InvalidGrant $ T.pack $ show err))
            Right val -> pure val

-- | Create a WAI Application for the MCP HTTP server (internal monomorphic version)
mcpAppInternal :: (MCPServer MCPServerM) => HTTPServerConfig -> IOTracer HTTPTrace -> TVar ServerState -> OAuthTVarEnv -> JWTSettings -> Application
mcpAppInternal config tracer stateVar oauthEnv jwtSettings =
    let cookieSettings = defaultCookieSettings
        authContext = cookieSettings :. jwtSettings :. EmptyContext
        baseApp = case httpOAuthConfig config of
            Just oauthCfg
                | oauthEnabled oauthCfg ->
                    serveWithContext
                        (Proxy :: Proxy (CompleteAPI '[JWT]))
                        authContext
                        (oauthServerNew config tracer oauthEnv jwtSettings :<|> mcpServerAuth config tracer stateVar)
            _ ->
                serve (Proxy :: Proxy UnprotectedMCPAPI) (mcpServerNoAuth config tracer stateVar)
     in if httpEnableLogging config
            then logStdoutDev baseApp
            else baseApp
  where
    -- Use polymorphic server via hoistServerWithContext
    oauthServerNew :: HTTPServerConfig -> IOTracer HTTPTrace -> OAuthTVarEnv -> JWTSettings -> Server OAuthAPI
    oauthServerNew cfg httpTracer oauth jwtSet =
        let authEnv = DemoCredentialEnv defaultDemoCredentialStore
            appEnv =
                AppEnv
                    { envOAuth = oauth
                    , envAuth = authEnv
                    , envConfig = cfg
                    , envTracer = httpTracer
                    , envJWT = jwtSet
                    , envServerState = stateVar
                    , envTimeProvider = Nothing -- Use real IO time for production
                    }
         in hoistServerWithContext
                (Proxy :: Proxy OAuthAPI)
                (Proxy :: Proxy '[CookieSettings, JWTSettings])
                (runAppM appEnv)
                oauthServer

    mcpServerNoAuth :: HTTPServerConfig -> IOTracer HTTPTrace -> TVar ServerState -> Aeson.Value -> Handler Aeson.Value
    mcpServerNoAuth httpConfig httpTracer stateTVar = handleHTTPRequest httpConfig httpTracer stateTVar Nothing

-- | MCP server handler with JWT authentication
mcpServerAuth :: (MCPServer MCPServerM) => HTTPServerConfig -> IOTracer HTTPTrace -> TVar ServerState -> AuthResult AuthUser -> Aeson.Value -> Handler Aeson.Value
mcpServerAuth httpConfig httpTracer stateTVar authResult requestValue =
    case authResult of
        Authenticated user -> do
            liftIO $ traceWith httpTracer $ HTTPAuthSuccess (unUserId $ userUserId user)
            handleHTTPRequest httpConfig httpTracer stateTVar (Just user) requestValue
        BadPassword -> do
            liftIO $ traceWith httpTracer $ HTTPAuthFailure "Bad password"
            liftIO $ traceWith httpTracer $ HTTPAuthRequired "/mcp"
            -- Throw domain error with WWW-Authenticate header
            throwError $
                err401
                    { errHeaders = [("WWW-Authenticate", wwwAuthenticateValue)]
                    , errBody = encode $ OAuthErrorResponse "invalid_client" (Just "Bearer token required")
                    }
        NoSuchUser -> do
            liftIO $ traceWith httpTracer $ HTTPAuthFailure "No such user"
            liftIO $ traceWith httpTracer $ HTTPAuthRequired "/mcp"
            throwError $
                err401
                    { errHeaders = [("WWW-Authenticate", wwwAuthenticateValue)]
                    , errBody = encode $ OAuthErrorResponse "invalid_client" (Just "Bearer token required")
                    }
        Indefinite -> do
            liftIO $ traceWith httpTracer $ HTTPAuthFailure "Authentication indefinite"
            liftIO $ traceWith httpTracer $ HTTPAuthRequired "/mcp"
            throwError $
                err401
                    { errHeaders = [("WWW-Authenticate", wwwAuthenticateValue)]
                    , errBody = encode $ OAuthErrorResponse "invalid_client" (Just "Bearer token required")
                    }
  where
    metadataUrl = httpBaseUrl httpConfig <> "/.well-known/oauth-protected-resource"
    wwwAuthenticateValue = TE.encodeUtf8 $ "Bearer resource_metadata=\"" <> metadataUrl <> "\""

-- | Handle HTTP MCP requests following the MCP transport protocol
handleHTTPRequest :: (MCPServer MCPServerM) => HTTPServerConfig -> IOTracer HTTPTrace -> TVar ServerState -> Maybe AuthUser -> Aeson.Value -> Handler Aeson.Value
handleHTTPRequest httpConfig tracer stateVar mAuthUser requestValue = do
    -- Emit trace for received request
    liftIO $ traceWith tracer $ HTTPRequestReceived "/mcp" "POST" (isJust mAuthUser)

    -- Parse the incoming JSON-RPC message
    case fromJSON requestValue of
        Aeson.Success (msg :: JSONRPCMessage) -> do
            case msg of
                RequestMessage req -> do
                    -- Process the JSON-RPC request
                    result <- liftIO $ processHTTPRequest httpConfig tracer stateVar req
                    case result of
                        Left err -> throwError err500{errBody = encode $ object ["error" .= err]}
                        Right response -> return response
                NotificationMessage notif -> do
                    -- Process notifications (no response expected)
                    _ <- liftIO $ processHTTPNotification httpConfig stateVar notif
                    return $ object [] -- Empty response for notifications
                _ -> throwError err400{errBody = encode $ object ["error" .= ("Invalid JSON-RPC message type" :: Text)]}
        Aeson.Error e -> throwError err400{errBody = encode $ object ["error" .= ("Invalid JSON-RPC message" :: Text), "error_description" .= T.pack e]}

-- | Process an HTTP MCP notification
processHTTPNotification :: HTTPServerConfig -> TVar ServerState -> JSONRPCNotification -> IO ()
processHTTPNotification _ _ _ = do
    -- For now, just ignore notifications since they don't need responses
    -- In a more complete implementation, this would handle logging/setLevel notifications
    return ()

-- | Process an HTTP MCP request
processHTTPRequest :: (MCPServer MCPServerM) => HTTPServerConfig -> IOTracer HTTPTrace -> TVar ServerState -> JSONRPCRequest -> IO (Either Text Aeson.Value)
processHTTPRequest httpConfig tracer stateVar req = do
    -- Read the current state
    currentState <- readTVarIO stateVar
    let dummyConfig =
            ServerConfig
                { configInput = undefined -- Not used in HTTP mode
                , configOutput = undefined -- Not used in HTTP mode
                , configServerInfo = httpServerInfo httpConfig
                , configCapabilities = httpCapabilities httpConfig
                }
        serverTracer = contramap HTTPServer tracer
        opTracer = contramap HTTPOperation tracer

    result <- runMCPServer dummyConfig currentState (handleHTTPRequestInner serverTracer opTracer (httpProtocolVersion httpConfig) req)
    case result of
        Left err -> return $ Left err
        Right (response, newState) -> do
            -- Update the state atomically
            atomically $ writeTVar stateVar newState
            return $ Right response

-- | Handle HTTP request within the MCP monad, returning proper JSON-RPC responses
handleHTTPRequestInner :: (MCPServer MCPServerM) => IOTracer ServerTrace -> IOTracer OperationTrace -> Text -> JSONRPCRequest -> MCPServerM Aeson.Value
handleHTTPRequestInner serverTracer opTracer protocolVersion (JSONRPCRequest _ reqId method params) = do
    config <- ask
    state <- get

    case method of
        "initialize" -> case params of
            Just p -> case fromJSON p of
                Aeson.Success initParams -> do
                    handleInitializeHTTP serverTracer reqId initParams
                    let result =
                            InitializeResult
                                { protocolVersion = protocolVersion
                                , capabilities = configCapabilities config
                                , serverInfo = configServerInfo config
                                , instructions = Nothing
                                , _meta = Nothing
                                }
                    return $ toJSON $ JSONRPCResponse "2.0" reqId (toJSON result)
                Aeson.Error e ->
                    return $
                        toJSON $
                            JSONRPCError "2.0" reqId $
                                JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
            Nothing ->
                return $
                    toJSON $
                        JSONRPCError "2.0" reqId $
                            JSONRPCErrorInfo (-32602) "Missing params" Nothing
        "ping" -> return $ toJSON $ JSONRPCResponse "2.0" reqId (object [])
        "resources/list" -> do
            liftIO $ traceWith opTracer OperationResourcesList
            if not (serverInitialized state)
                then
                    return $
                        toJSON $
                            JSONRPCError "2.0" reqId $
                                JSONRPCErrorInfo (-32002) "Server not initialized" Nothing
                else case params of
                    Just p -> case fromJSON p of
                        Aeson.Success listParams -> do
                            result <- handleListResources listParams
                            return $ toJSON $ JSONRPCResponse "2.0" reqId (toJSON result)
                        Aeson.Error e ->
                            return $
                                toJSON $
                                    JSONRPCError "2.0" reqId $
                                        JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
                    Nothing -> do
                        result <- handleListResources (ListResourcesParams Nothing)
                        return $ toJSON $ JSONRPCResponse "2.0" reqId (toJSON result)
        "resources/read" -> do
            if not (serverInitialized state)
                then
                    return $
                        toJSON $
                            JSONRPCError "2.0" reqId $
                                JSONRPCErrorInfo (-32002) "Server not initialized" Nothing
                else case params of
                    Just p -> case fromJSON p of
                        Aeson.Success readParams@(ReadResourceParams uri) -> do
                            liftIO $ traceWith opTracer $ OperationResourceRead uri
                            result <- handleReadResource readParams
                            return $ toJSON $ JSONRPCResponse "2.0" reqId (toJSON result)
                        Aeson.Error e ->
                            return $
                                toJSON $
                                    JSONRPCError "2.0" reqId $
                                        JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
                    Nothing ->
                        return $
                            toJSON $
                                JSONRPCError "2.0" reqId $
                                    JSONRPCErrorInfo (-32602) "Missing params" Nothing
        "tools/list" -> do
            liftIO $ traceWith opTracer OperationToolsList
            if not (serverInitialized state)
                then
                    return $
                        toJSON $
                            JSONRPCError "2.0" reqId $
                                JSONRPCErrorInfo (-32002) "Server not initialized" Nothing
                else case params of
                    Just p -> case fromJSON p of
                        Aeson.Success listParams -> do
                            result <- handleListTools listParams
                            return $ toJSON $ JSONRPCResponse "2.0" reqId (toJSON result)
                        Aeson.Error e ->
                            return $
                                toJSON $
                                    JSONRPCError "2.0" reqId $
                                        JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
                    Nothing -> do
                        result <- handleListTools (ListToolsParams Nothing)
                        return $ toJSON $ JSONRPCResponse "2.0" reqId (toJSON result)
        "tools/call" -> do
            if not (serverInitialized state)
                then
                    return $
                        toJSON $
                            JSONRPCError "2.0" reqId $
                                JSONRPCErrorInfo (-32002) "Server not initialized" Nothing
                else case params of
                    Just p -> case fromJSON p of
                        Aeson.Success callParams@(CallToolParams name args) -> do
                            result <- handleCallTool callParams
                            let argsJson = maybe (toJSON Aeson.Null) toJSON args
                                resultJson = toJSON result
                                success = maybe True not (MCP.Protocol.isError result)
                            liftIO $ traceWith opTracer $ OperationToolCall name argsJson resultJson Nothing success
                            return $ toJSON $ JSONRPCResponse "2.0" reqId (toJSON result)
                        Aeson.Error e ->
                            return $
                                toJSON $
                                    JSONRPCError "2.0" reqId $
                                        JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
                    Nothing ->
                        return $
                            toJSON $
                                JSONRPCError "2.0" reqId $
                                    JSONRPCErrorInfo (-32602) "Missing params" Nothing
        "prompts/list" -> do
            liftIO $ traceWith opTracer OperationPromptsList
            if not (serverInitialized state)
                then
                    return $
                        toJSON $
                            JSONRPCError "2.0" reqId $
                                JSONRPCErrorInfo (-32002) "Server not initialized" Nothing
                else case params of
                    Just p -> case fromJSON p of
                        Aeson.Success listParams -> do
                            result <- handleListPrompts listParams
                            return $ toJSON $ JSONRPCResponse "2.0" reqId (toJSON result)
                        Aeson.Error e ->
                            return $
                                toJSON $
                                    JSONRPCError "2.0" reqId $
                                        JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
                    Nothing -> do
                        result <- handleListPrompts (ListPromptsParams Nothing)
                        return $ toJSON $ JSONRPCResponse "2.0" reqId (toJSON result)
        "prompts/get" -> do
            if not (serverInitialized state)
                then
                    return $
                        toJSON $
                            JSONRPCError "2.0" reqId $
                                JSONRPCErrorInfo (-32002) "Server not initialized" Nothing
                else case params of
                    Just p -> case fromJSON p of
                        Aeson.Success getParams@(GetPromptParams name args) -> do
                            liftIO $ traceWith opTracer $ OperationPromptGet name (toJSON <$> args)
                            result <- handleGetPrompt getParams
                            return $ toJSON $ JSONRPCResponse "2.0" reqId (toJSON result)
                        Aeson.Error e ->
                            return $
                                toJSON $
                                    JSONRPCError "2.0" reqId $
                                        JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
                    Nothing ->
                        return $
                            toJSON $
                                JSONRPCError "2.0" reqId $
                                    JSONRPCErrorInfo (-32602) "Missing params" Nothing
        "completion/complete" -> do
            if not (serverInitialized state)
                then
                    return $
                        toJSON $
                            JSONRPCError "2.0" reqId $
                                JSONRPCErrorInfo (-32002) "Server not initialized" Nothing
                else case params of
                    Just p -> case fromJSON p of
                        Aeson.Success completeParams@(CompleteParams ref (CompletionArgument argName argValue) _context) -> do
                            let refType = case ref of
                                    PromptRef _ -> "prompt"
                                    ResourceTemplateRef _ -> "resource_template"
                            liftIO $ traceWith opTracer $ OperationComplete refType (argName <> "=" <> argValue)
                            result <- handleComplete completeParams
                            return $ toJSON $ JSONRPCResponse "2.0" reqId (toJSON result)
                        Aeson.Error e ->
                            return $
                                toJSON $
                                    JSONRPCError "2.0" reqId $
                                        JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
                    Nothing ->
                        return $
                            toJSON $
                                JSONRPCError "2.0" reqId $
                                    JSONRPCErrorInfo (-32602) "Missing params" Nothing
        "logging/setLevel" -> case params of
            Just p -> case fromJSON p of
                Aeson.Success setLevelParams -> do
                    _ <- handleSetLevel setLevelParams
                    return $ toJSON $ JSONRPCResponse "2.0" reqId (object [])
                Aeson.Error e ->
                    return $
                        toJSON $
                            JSONRPCError "2.0" reqId $
                                JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
            Nothing ->
                return $
                    toJSON $
                        JSONRPCError "2.0" reqId $
                            JSONRPCErrorInfo (-32602) "Missing params" Nothing
        _ ->
            return $
                toJSON $
                    JSONRPCError "2.0" reqId $
                        JSONRPCErrorInfo (-32601) "Method not found" Nothing

-- | Handle HTTP initialize request
handleInitializeHTTP :: IOTracer ServerTrace -> RequestId -> InitializeParams -> MCPServerM ()
handleInitializeHTTP tracer _ params = do
    config <- ask
    state <- get

    let InitializeParams{capabilities = clientCaps, clientInfo = clientImpl} = params

    put
        state
            { serverInitialized = True
            , clientCapabilities = Just clientCaps
            , serverInfo = Just (configServerInfo config)
            }

    -- Emit ServerInitialized trace
    let Implementation clientName _ _ = clientImpl
    liftIO $ traceWith tracer (ServerInitialized (Just clientName))

    -- Emit ServerCapabilityNegotiation trace
    let caps = extractCapabilityNames (serverCapabilities state)
    liftIO $ traceWith tracer (ServerCapabilityNegotiation caps)

-- | Extract capability names from ServerCapabilities
extractCapabilityNames :: ServerCapabilities -> [Text]
extractCapabilityNames (ServerCapabilities res prpts tls comps logCap _exp) =
    concat
        [ maybe [] (const ["resources"]) res
        , maybe [] (const ["prompts"]) prpts
        , maybe [] (const ["tools"]) tls
        , maybe [] (const ["completions"]) comps
        , maybe [] (const ["logging"]) logCap
        ]

-- | Default demo OAuth configuration for testing purposes
defaultDemoOAuthConfig :: OAuthConfig
defaultDemoOAuthConfig =
    OAuthConfig
        { oauthEnabled = True
        , oauthProviders = []
        , requireHTTPS = False -- For demo only
        -- Default timing parameters
        , authCodeExpirySeconds = 600 -- 10 minutes
        , accessTokenExpirySeconds = 3600 -- 1 hour
        -- Default OAuth parameters
        , supportedScopes = [Scope "mcp:read", Scope "mcp:write"]
        , supportedResponseTypes = [ResponseCode]
        , supportedGrantTypes = [GrantAuthorizationCode, GrantRefreshToken]
        , supportedAuthMethods = [AuthNone]
        , supportedCodeChallengeMethods = [S256]
        , -- Demo mode settings (interactive login required)
          autoApproveAuth = False
        , demoUserIdTemplate = Nothing
        , demoEmailDomain = "example.com"
        , demoUserName = "Test User"
        , publicClientSecret = Just ""
        , -- Default token prefixes
          authCodePrefix = "code_"
        , refreshTokenPrefix = "rt_"
        , clientIdPrefix = "client_"
        , -- Default response template
          authorizationSuccessTemplate = Nothing
        , -- Credential management
          credentialStore = defaultDemoCredentialStore
        , loginSessionExpirySeconds = 600 -- 10 minutes
        }

-- | Default protected resource metadata for a given base URL
defaultProtectedResourceMetadata :: Text -> ProtectedResourceMetadata
defaultProtectedResourceMetadata baseUrl =
    ProtectedResourceMetadata
        { resource = baseUrl
        , authorizationServers = [baseUrl]
        , scopesSupported = Just [Scope "mcp:read", Scope "mcp:write"]
        , bearerMethodsSupported = Just ["header"]
        , resourceName = Nothing
        , resourceDocumentation = Nothing
        }

-- | Map scope to human-readable description
scopeToDescription :: Text -> Text
scopeToDescription "mcp:read" = "Read MCP resources"
scopeToDescription "mcp:write" = "Write MCP resources"
scopeToDescription "mcp:tools" = "Execute MCP tools"
scopeToDescription other = other -- fallback to raw scope

-- | Format scopes as human-readable descriptions
formatScopeDescriptions :: Text -> Text
formatScopeDescriptions scopes =
    let scopeList = T.splitOn " " scopes
        descriptions = map scopeToDescription scopeList
     in T.intercalate ", " descriptions

{- | Demo MCP application entry point.

This is a reference implementation using in-memory TVar storage and demo credentials.
This is what `cabal run mcp-http -- --oauth` uses.

Creates a WAI Application with:

* In-memory OAuth state storage (TVar-based)
* Demo credentials (demo/demo123, admin/admin456)
* Default HTTP server configuration (localhost:8080)
* Null tracer (no trace output)
* Generated JWK for JWT signing

For production use, create a custom AppEnv with appropriate:

* Persistent storage backend (PostgreSQL, Redis, etc.)
* Real credential validation (LDAP, database, etc.)
* Production configuration (proper base URL, HTTPS, etc.)
* Structured tracing to your logging system
-}
demoMcpApp :: (MCPServer MCPServerM) => IO Application
demoMcpApp = do
    -- Generate JWK for JWT signing
    jwk <- generateKey
    let jwtSettings = defaultJWTSettings jwk

    -- Initialize in-memory OAuth state storage
    oauthEnv <- newOAuthTVarEnv defaultExpiryConfig

    -- Create null tracer (discards all traces)
    let tracer = IOTracer (Tracer (\_ -> pure ()))

    -- Create default demo configuration
    let config =
            HTTPServerConfig
                { httpPort = 8080
                , httpBaseUrl = "http://localhost:8080"
                , httpServerInfo = Implementation "mcp-demo" (Just "0.3.0") ""
                , httpCapabilities = ServerCapabilities Nothing Nothing Nothing Nothing Nothing Nothing
                , httpEnableLogging = False
                , httpOAuthConfig = Just defaultDemoOAuthConfig
                , httpJWK = Just jwk
                , httpProtocolVersion = "2025-06-18"
                , httpProtectedResourceMetadata = Just (defaultProtectedResourceMetadata "http://localhost:8080")
                }

    -- Create demo credential environment
    let authEnv = DemoCredentialEnv defaultDemoCredentialStore

    -- Initialize server state
    stateVar <- newTVarIO $ initialServerState (httpCapabilities config)

    -- Create AppEnv with all fields including stateVar
    let appEnv =
            AppEnv
                { envOAuth = oauthEnv
                , envAuth = authEnv
                , envConfig = config
                , envTracer = tracer
                , envJWT = jwtSettings
                , envServerState = stateVar
                , envTimeProvider = Nothing -- Use real IO time
                }

    -- Use polymorphic mcpAppWithOAuth with natural transformation and JWTSettings
    pure $ mcpAppWithOAuth (runAppM appEnv) jwtSettings

-- | Run the MCP server as an HTTP server
runServerHTTP :: (MCPServer MCPServerM) => HTTPServerConfig -> IOTracer HTTPTrace -> IO ()
runServerHTTP config tracer = do
    -- Initialize JWT settings if OAuth is enabled
    jwtSettings <- case httpJWK config of
        Just jwk -> return $ defaultJWTSettings jwk
        Nothing -> defaultJWTSettings <$> generateKey

    -- Initialize the server state
    stateVar <- newTVarIO $ initialServerState (httpCapabilities config)

    -- Initialize OAuth state using typeclass-based OAuthTVarEnv
    oauthEnv <- newOAuthTVarEnv defaultExpiryConfig

    traceWith tracer $ HTTPServerStarting (httpPort config) (httpBaseUrl config)

    when (maybe False oauthEnabled (httpOAuthConfig config)) $ do
        let authEp = httpBaseUrl config <> "/authorize"
            tokenEp = httpBaseUrl config <> "/token"
        traceWith tracer $ HTTPOAuthEnabled authEp tokenEp
        case httpOAuthConfig config >>= \cfg -> if null (oauthProviders cfg) then Nothing else Just (oauthProviders cfg) of
            Just providers -> do
                traceWith tracer $ HTTPOAuthProviders (map providerName providers)
                when (any requiresPKCE providers) $ traceWith tracer HTTPPKCEEnabled
            Nothing -> return ()

    traceWith tracer HTTPServerStarted
    run (httpPort config) (mcpAppInternal config tracer stateVar oauthEnv jwtSettings)
