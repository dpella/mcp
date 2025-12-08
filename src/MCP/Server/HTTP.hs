{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module      : MCP.Server.HTTP
Description : MCP server implementation for HTTP communication
Copyright   : (C) 2025 Matthias Pall Gissurarson
License     : MIT
Maintainer  : mpg@mpg.is
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

    -- * HTML Content Type
    HTML,

    -- * Login Types
    PendingAuthorization (..),
    LoginForm (..),
    LoginError (..),
    LoginResult (..),

    -- * HTML Rendering
    renderLoginPage,
    renderErrorPage,
    scopeToDescription,
    formatScopeDescriptions,
) where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO, writeTVar)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Control.Monad.State.Strict (get, put)
import Crypto.JOSE (JWK)
import Data.Aeson (encode, fromJSON, object, toJSON, (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import GHC.Generics (Generic)
import Network.HTTP.Media ((//), (/:))
import Network.Wai (Application)
import Network.Wai.Handler.Warp (Port, run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant (Context (..), Handler, Proxy (..), Server, serve, serveWithContext, throwError)
import Servant.API (Accept (..), FormUrlEncoded, Get, Header, Headers, JSON, MimeRender (..), NoContent (..), PlainText, Post, QueryParam, QueryParam', ReqBody, Required, addHeader, (:<|>) (..), (:>))
import Servant.Auth.Server (Auth, AuthResult (..), FromJWT, JWT, JWTSettings, ToJWT, defaultCookieSettings, defaultJWTSettings, generateKey, makeJWT)
import Servant.Server (err400, err401, err500, errBody, errHeaders)
import Web.FormUrlEncoded (FromForm (..), parseUnique)

import Control.Monad (unless, when)
import MCP.Protocol
import MCP.Server (MCPServer (..), MCPServerM, ServerConfig (..), ServerState (..), initialServerState, runMCPServer)
import MCP.Server.Auth (CredentialStore (..), OAuthConfig (..), OAuthMetadata (..), OAuthProvider (..), ProtectedResourceMetadata (..), defaultDemoCredentialStore, validateCodeVerifier, validateCredential)
import MCP.Types

-- | HTML content type for Servant
data HTML

instance Accept HTML where
    contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML Text where
    mimeRender _ = LBS.fromStrict . TE.encodeUtf8

-- | Configuration for running an MCP HTTP server
data HTTPServerConfig = HTTPServerConfig
    { httpPort :: Port
    , httpBaseUrl :: Text -- Base URL for OAuth endpoints (e.g., "https://api.example.com")
    , httpServerInfo :: Implementation
    , httpCapabilities :: ServerCapabilities
    , httpEnableLogging :: Bool
    , httpOAuthConfig :: Maybe OAuthConfig
    , httpJWK :: Maybe JWK -- JWT signing key
    , httpProtocolVersion :: Text -- MCP protocol version
    , httpProtectedResourceMetadata :: Maybe ProtectedResourceMetadata
    {- ^ Custom protected resource metadata.
    When Nothing, auto-generated from httpBaseUrl.
    -}
    }
    deriving (Show)

-- | User information from OAuth
data AuthUser = AuthUser
    { userId :: Text
    , userEmail :: Maybe Text
    , userName :: Maybe Text
    }
    deriving (Show, Generic)

-- | Authorization code with PKCE
data AuthorizationCode = AuthorizationCode
    { authCode :: Text
    , authClientId :: Text
    , authRedirectUri :: Text
    , authCodeChallenge :: Text
    , authCodeChallengeMethod :: Text
    , authScopes :: [Text]
    , authUserId :: Text
    , authExpiry :: UTCTime
    }
    deriving (Show, Generic)

-- | OAuth server state
data OAuthState = OAuthState
    { authCodes :: Map Text AuthorizationCode -- code -> AuthorizationCode
    , accessTokens :: Map Text AuthUser -- token -> user
    , refreshTokens :: Map Text (Text, AuthUser) -- refresh_token -> (access_token, user)
    , registeredClients :: Map Text ClientInfo -- client_id -> ClientInfo
    , pendingAuthorizations :: Map Text PendingAuthorization -- session_id -> pending authorization
    }
    deriving (Show, Generic)

-- | Pending authorization awaiting user authentication
data PendingAuthorization = PendingAuthorization
    { pendingClientId :: Text
    , pendingRedirectUri :: Text
    , pendingCodeChallenge :: Text
    , pendingCodeChallengeMethod :: Text
    , pendingScope :: Maybe Text
    , pendingState :: Maybe Text
    , pendingResource :: Maybe Text
    , pendingCreatedAt :: UTCTime
    }
    deriving (Show, Generic)

-- | Login form data
data LoginForm = LoginForm
    { formUsername :: Text
    , formPassword :: Text
    , formSessionId :: Text
    , formAction :: Text
    }
    deriving (Show, Generic)

instance FromForm LoginForm where
    fromForm form =
        LoginForm
            <$> parseUnique "username" form
            <*> parseUnique "password" form
            <*> parseUnique "session_id" form
            <*> parseUnique "action" form

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

-- | Client registration request
data ClientRegistrationRequest = ClientRegistrationRequest
    { client_name :: Text
    , redirect_uris :: [Text]
    , grant_types :: [Text]
    , response_types :: [Text]
    , token_endpoint_auth_method :: Text
    }
    deriving (Show, Generic)

instance Aeson.FromJSON ClientRegistrationRequest where
    parseJSON = Aeson.genericParseJSON Aeson.defaultOptions

-- | Client registration response
data ClientRegistrationResponse = ClientRegistrationResponse
    { client_id :: Text
    , client_secret :: Text -- Empty string for public clients
    , client_name :: Text
    , redirect_uris :: [Text]
    , grant_types :: [Text]
    , response_types :: [Text]
    , token_endpoint_auth_method :: Text
    }
    deriving (Show, Generic)

instance Aeson.ToJSON ClientRegistrationResponse where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions

-- | Client info stored in server
data ClientInfo = ClientInfo
    { clientName :: Text
    , clientRedirectUris :: [Text]
    , clientGrantTypes :: [Text]
    , clientResponseTypes :: [Text]
    , clientAuthMethod :: Text
    }
    deriving (Show, Generic)

-- | Token response
data TokenResponse = TokenResponse
    { access_token :: Text
    , token_type :: Text
    , expires_in :: Maybe Int
    , refresh_token :: Maybe Text
    , scope :: Maybe Text
    }
    deriving (Show, Generic)

instance Aeson.ToJSON TokenResponse where
    toJSON TokenResponse{..} =
        object $
            [ "access_token" .= access_token
            , "token_type" .= token_type
            ]
                ++ maybe [] (\e -> ["expires_in" .= e]) expires_in
                ++ maybe [] (\r -> ["refresh_token" .= r]) refresh_token
                ++ maybe [] (\s -> ["scope" .= s]) scope

instance Aeson.FromJSON AuthUser where
    parseJSON = Aeson.withObject "AuthUser" $ \v ->
        AuthUser
            <$> v Aeson..: "sub"
            <*> v Aeson..:? "email"
            <*> v Aeson..:? "name"

instance Aeson.ToJSON AuthUser where
    toJSON AuthUser{..} =
        object
            [ "sub" .= userId
            , "email" .= userEmail
            , "name" .= userName
            ]

-- Instances for JWT
instance ToJWT AuthUser
instance FromJWT AuthUser

-- | MCP API definition for HTTP server (following the MCP transport spec)
type MCPAPI auths = Auth auths AuthUser :> "mcp" :> ReqBody '[JSON] Aeson.Value :> Post '[JSON] Aeson.Value

-- | Unprotected MCP API (for backward compatibility)
type UnprotectedMCPAPI = "mcp" :> ReqBody '[JSON] Aeson.Value :> Post '[JSON] Aeson.Value

-- | Protected Resource Metadata API (RFC 9728)
type ProtectedResourceAPI = ".well-known" :> "oauth-protected-resource" :> Get '[JSON] ProtectedResourceMetadata

-- | Login API endpoints
type LoginAPI =
    "login"
        :> Header "Cookie" Text
        :> ReqBody '[FormUrlEncoded] LoginForm
        :> Post '[HTML] (Headers '[Header "Location" Text, Header "Set-Cookie" Text] NoContent)

-- | OAuth endpoints
type OAuthAPI =
    ProtectedResourceAPI
        :<|> ".well-known" :> "oauth-authorization-server" :> Get '[JSON] OAuthMetadata
        :<|> "register"
            :> ReqBody '[JSON] ClientRegistrationRequest
            :> Post '[JSON] ClientRegistrationResponse
        :<|> "authorize"
            :> QueryParam' '[Required] "response_type" Text
            :> QueryParam' '[Required] "client_id" Text
            :> QueryParam' '[Required] "redirect_uri" Text
            :> QueryParam' '[Required] "code_challenge" Text
            :> QueryParam' '[Required] "code_challenge_method" Text
            :> QueryParam "scope" Text
            :> QueryParam "state" Text
            :> QueryParam "resource" Text
            :> Get '[HTML] (Headers '[Header "Set-Cookie" Text] Text)
        :<|> LoginAPI
        :<|> "token"
            :> ReqBody '[FormUrlEncoded] [(Text, Text)]
            :> Post '[JSON] TokenResponse

-- | Complete API with OAuth
type CompleteAPI auths = OAuthAPI :<|> MCPAPI auths

-- | Create a WAI Application for the MCP HTTP server
mcpApp :: (MCPServer MCPServerM) => HTTPServerConfig -> TVar ServerState -> TVar OAuthState -> JWTSettings -> Application
mcpApp config stateVar oauthStateVar jwtSettings =
    let cookieSettings = defaultCookieSettings
        authContext = cookieSettings :. jwtSettings :. EmptyContext
        baseApp = case httpOAuthConfig config of
            Just oauthCfg
                | oauthEnabled oauthCfg ->
                    serveWithContext
                        (Proxy :: Proxy (CompleteAPI '[JWT]))
                        authContext
                        (oauthServer config oauthStateVar :<|> mcpServerAuth config stateVar)
            _ ->
                serve (Proxy :: Proxy UnprotectedMCPAPI) (mcpServerNoAuth config stateVar)
     in if httpEnableLogging config
            then logStdoutDev baseApp
            else baseApp
  where
    oauthServer :: HTTPServerConfig -> TVar OAuthState -> Server OAuthAPI
    oauthServer cfg oauthState =
        handleProtectedResourceMetadata cfg
            :<|> handleMetadata cfg
            :<|> handleRegister cfg oauthState
            :<|> handleAuthorize cfg oauthState
            :<|> handleLogin cfg oauthState jwtSettings
            :<|> handleToken jwtSettings cfg oauthState

    mcpServerAuth :: HTTPServerConfig -> TVar ServerState -> AuthResult AuthUser -> Aeson.Value -> Handler Aeson.Value
    mcpServerAuth httpConfig stateTVar authResult requestValue =
        case authResult of
            Authenticated user -> handleHTTPRequest httpConfig stateTVar (Just user) requestValue
            _ ->
                throwError $
                    err401
                        { errHeaders = [("WWW-Authenticate", wwwAuthenticateValue)]
                        , errBody = encode $ object ["error" .= ("Authentication required" :: Text)]
                        }
      where
        metadataUrl = httpBaseUrl httpConfig <> "/.well-known/oauth-protected-resource"
        wwwAuthenticateValue = TE.encodeUtf8 $ "Bearer resource_metadata=\"" <> metadataUrl <> "\""

    mcpServerNoAuth :: HTTPServerConfig -> TVar ServerState -> Aeson.Value -> Handler Aeson.Value
    mcpServerNoAuth httpConfig stateTVar = handleHTTPRequest httpConfig stateTVar Nothing

-- | Handle HTTP MCP requests following the MCP transport protocol
handleHTTPRequest :: (MCPServer MCPServerM) => HTTPServerConfig -> TVar ServerState -> Maybe AuthUser -> Aeson.Value -> Handler Aeson.Value
handleHTTPRequest httpConfig stateVar _mAuthUser requestValue = do
    -- Parse the incoming JSON-RPC message
    case fromJSON requestValue of
        Aeson.Success (msg :: JSONRPCMessage) -> do
            case msg of
                RequestMessage req -> do
                    -- Process the JSON-RPC request
                    result <- liftIO $ processHTTPRequest httpConfig stateVar req
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
processHTTPNotification :: (MCPServer MCPServerM) => HTTPServerConfig -> TVar ServerState -> JSONRPCNotification -> IO ()
processHTTPNotification _ _ _ = do
    -- For now, just ignore notifications since they don't need responses
    -- In a more complete implementation, this would handle logging/setLevel notifications
    return ()

-- | Process an HTTP MCP request
processHTTPRequest :: (MCPServer MCPServerM) => HTTPServerConfig -> TVar ServerState -> JSONRPCRequest -> IO (Either Text Aeson.Value)
processHTTPRequest httpConfig stateVar req = do
    -- Read the current state
    currentState <- readTVarIO stateVar
    let dummyConfig =
            ServerConfig
                { configInput = undefined -- Not used in HTTP mode
                , configOutput = undefined -- Not used in HTTP mode
                , configServerInfo = httpServerInfo httpConfig
                , configCapabilities = httpCapabilities httpConfig
                }

    result <- runMCPServer dummyConfig currentState (handleHTTPRequestInner (httpProtocolVersion httpConfig) req)
    case result of
        Left err -> return $ Left err
        Right (response, newState) -> do
            -- Update the state atomically
            atomically $ writeTVar stateVar newState
            return $ Right response

-- | Handle HTTP request within the MCP monad, returning proper JSON-RPC responses
handleHTTPRequestInner :: (MCPServer MCPServerM) => Text -> JSONRPCRequest -> MCPServerM Aeson.Value
handleHTTPRequestInner protocolVersion (JSONRPCRequest _ reqId method params) = do
    config <- ask
    state <- get

    case method of
        "initialize" -> case params of
            Just p -> case fromJSON p of
                Aeson.Success initParams -> do
                    handleInitializeHTTP reqId initParams
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
                        Aeson.Success readParams -> do
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
                        Aeson.Success callParams -> do
                            result <- handleCallTool callParams
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
                        Aeson.Success getParams -> do
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
                        Aeson.Success completeParams -> do
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
handleInitializeHTTP :: RequestId -> InitializeParams -> MCPServerM ()
handleInitializeHTTP _ params = do
    config <- ask
    state <- get

    let InitializeParams{capabilities = clientCaps} = params

    put
        state
            { serverInitialized = True
            , clientCapabilities = Just clientCaps
            , serverInfo = Just (configServerInfo config)
            }

-- | Handler for /.well-known/oauth-protected-resource endpoint
handleProtectedResourceMetadata :: HTTPServerConfig -> Handler ProtectedResourceMetadata
handleProtectedResourceMetadata config = do
    let metadata = case httpProtectedResourceMetadata config of
            Just m -> m
            Nothing -> defaultProtectedResourceMetadata (httpBaseUrl config)
    return metadata

-- | Handle OAuth metadata discovery endpoint
handleMetadata :: HTTPServerConfig -> Handler OAuthMetadata
handleMetadata config = do
    let baseUrl = httpBaseUrl config
        oauthCfg = httpOAuthConfig config
    return
        OAuthMetadata
            { issuer = baseUrl
            , authorizationEndpoint = baseUrl <> "/authorize"
            , tokenEndpoint = baseUrl <> "/token"
            , registrationEndpoint = Just (baseUrl <> "/register")
            , userInfoEndpoint = Nothing
            , jwksUri = Nothing
            , scopesSupported = fmap supportedScopes oauthCfg
            , responseTypesSupported = maybe ["code"] supportedResponseTypes oauthCfg
            , grantTypesSupported = fmap supportedGrantTypes oauthCfg
            , tokenEndpointAuthMethodsSupported = fmap supportedAuthMethods oauthCfg
            , codeChallengeMethodsSupported = fmap supportedCodeChallengeMethods oauthCfg
            }

-- | Handle dynamic client registration
handleRegister :: HTTPServerConfig -> TVar OAuthState -> ClientRegistrationRequest -> Handler ClientRegistrationResponse
handleRegister config oauthStateVar (ClientRegistrationRequest reqName reqRedirects reqGrants reqResponses reqAuth) = do
    -- Generate client ID
    let prefix = maybe "client_" clientIdPrefix (httpOAuthConfig config)
    clientId <- liftIO $ (prefix <>) <$> generateAuthCode

    -- Store client info
    let clientInfo =
            ClientInfo
                { clientName = reqName
                , clientRedirectUris = reqRedirects
                , clientGrantTypes = reqGrants
                , clientResponseTypes = reqResponses
                , clientAuthMethod = reqAuth
                }

    liftIO $ atomically $ modifyTVar' oauthStateVar $ \s ->
        s{registeredClients = Map.insert clientId clientInfo (registeredClients s)}

    return
        ClientRegistrationResponse
            { client_id = clientId
            , client_secret = "" -- Empty string for public clients
            , client_name = reqName
            , redirect_uris = reqRedirects
            , grant_types = reqGrants
            , response_types = reqResponses
            , token_endpoint_auth_method = reqAuth
            }

-- | Handle OAuth authorize endpoint - now returns login page HTML
handleAuthorize :: HTTPServerConfig -> TVar OAuthState -> Text -> Text -> Text -> Text -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> Handler (Headers '[Header "Set-Cookie" Text] Text)
handleAuthorize config oauthStateVar responseType clientId redirectUri codeChallenge codeChallengeMethod mScope mState mResource = do
    -- Log resource parameter for RFC8707 support
    liftIO $ putStrLn $ "Resource parameter: " <> maybe "not provided" T.unpack mResource

    -- T037: Validate response_type (return HTML error page instead of JSON)
    when (responseType /= "code") $
        throwError err400{errBody = LBS.fromStrict $ TE.encodeUtf8 $ renderErrorPage "Unsupported Response Type" "Only 'code' response type is supported. Please contact the application developer.", errHeaders = [("Content-Type", "text/html; charset=utf-8")]}

    -- T037: Validate code_challenge_method (return HTML error page instead of JSON)
    when (codeChallengeMethod /= "S256") $
        throwError err400{errBody = LBS.fromStrict $ TE.encodeUtf8 $ renderErrorPage "Invalid Request" "Only 'S256' code challenge method is supported. Please contact the application developer.", errHeaders = [("Content-Type", "text/html; charset=utf-8")]}

    -- Look up client to get client name for display
    oauthState <- liftIO $ readTVarIO oauthStateVar

    -- T040: Handle unregistered client_id - render error page (don't redirect - can't trust redirect_uri)
    clientInfo <- case Map.lookup clientId (registeredClients oauthState) of
        Just ci -> return ci
        Nothing ->
            throwError err400{errBody = LBS.fromStrict $ TE.encodeUtf8 $ renderErrorPage "Unregistered Client" ("Client ID '" <> clientId <> "' is not registered. Please contact the application developer."), errHeaders = [("Content-Type", "text/html; charset=utf-8")]}

    -- T041: Handle invalid redirect_uri - render error page (don't redirect)
    unless (redirectUri `elem` clientRedirectUris clientInfo) $
        throwError err400{errBody = LBS.fromStrict $ TE.encodeUtf8 $ renderErrorPage "Invalid Redirect URI" ("The redirect URI '" <> redirectUri <> "' is not registered for this client. Please contact the application developer."), errHeaders = [("Content-Type", "text/html; charset=utf-8")]}

    let displayName = clientName clientInfo

    -- Generate session ID
    sessionId <- liftIO $ UUID.toText <$> UUID.nextRandom
    currentTime <- liftIO getCurrentTime

    -- Create pending authorization
    let pending =
            PendingAuthorization
                { pendingClientId = clientId
                , pendingRedirectUri = redirectUri
                , pendingCodeChallenge = codeChallenge
                , pendingCodeChallengeMethod = codeChallengeMethod
                , pendingScope = mScope
                , pendingState = mState
                , pendingResource = mResource
                , pendingCreatedAt = currentTime
                }

    -- Store pending authorization
    liftIO $ atomically $ modifyTVar' oauthStateVar $ \s ->
        s{pendingAuthorizations = Map.insert sessionId pending (pendingAuthorizations s)}

    -- Build session cookie
    let sessionExpirySeconds = maybe 600 loginSessionExpirySeconds (httpOAuthConfig config)
        cookieValue = "mcp_session=" <> sessionId <> "; HttpOnly; SameSite=Strict; Path=/; Max-Age=" <> T.pack (show sessionExpirySeconds)
        scopes = fromMaybe "default access" mScope
        loginHtml = renderLoginPage displayName scopes mResource sessionId

    return $ addHeader cookieValue loginHtml

-- | Extract session ID from cookie header
extractSessionFromCookie :: Text -> Maybe Text
extractSessionFromCookie cookieHeader =
    let cookies = T.splitOn ";" cookieHeader
        sessionCookies = filter (T.isInfixOf "mcp_session=") cookies
     in case sessionCookies of
            (cookie : _) ->
                let parts = T.splitOn "=" cookie
                 in case parts of
                        [_, value] -> Just (T.strip value)
                        _ -> Nothing
            [] -> Nothing

-- | Handle login form submission
handleLogin :: HTTPServerConfig -> TVar OAuthState -> JWTSettings -> Maybe Text -> LoginForm -> Handler (Headers '[Header "Location" Text, Header "Set-Cookie" Text] NoContent)
handleLogin config oauthStateVar _jwtSettings mCookie loginForm = do
    -- Extract session ID from form
    let sessionId = formSessionId loginForm

    -- T039: Handle cookies disabled - check if cookie matches form session_id
    case mCookie of
        Nothing ->
            throwError err400{errBody = LBS.fromStrict $ TE.encodeUtf8 $ renderErrorPage "Cookies Required" "Your browser must have cookies enabled to sign in. Please enable cookies and try again.", errHeaders = [("Content-Type", "text/html; charset=utf-8")]}
        Just cookie ->
            -- Parse session cookie and verify it matches form session_id
            let cookieSessionId = extractSessionFromCookie cookie
             in unless (cookieSessionId == Just sessionId) $
                    throwError err400{errBody = LBS.fromStrict $ TE.encodeUtf8 $ renderErrorPage "Cookies Required" "Session cookie mismatch. Please enable cookies and try again.", errHeaders = [("Content-Type", "text/html; charset=utf-8")]}

    -- Look up pending authorization
    oauthState <- liftIO $ readTVarIO oauthStateVar
    pending <- case Map.lookup sessionId (pendingAuthorizations oauthState) of
        Just p -> return p
        Nothing ->
            throwError err400{errBody = LBS.fromStrict $ TE.encodeUtf8 $ renderErrorPage "Invalid Session" "Session not found or has expired. Please restart the authorization flow.", errHeaders = [("Content-Type", "text/html; charset=utf-8")]}

    -- T038: Handle expired sessions
    currentTime <- liftIO getCurrentTime
    let sessionExpirySeconds = maybe 600 (fromIntegral . loginSessionExpirySeconds) (httpOAuthConfig config)
        expiryTime = addUTCTime sessionExpirySeconds (pendingCreatedAt pending)
    when (currentTime > expiryTime) $
        throwError err400{errBody = LBS.fromStrict $ TE.encodeUtf8 $ renderErrorPage "Session Expired" "Your login session has expired. Please restart the authorization flow.", errHeaders = [("Content-Type", "text/html; charset=utf-8")]}

    -- Check if user denied access
    if formAction loginForm == "deny"
        then do
            -- Clear session and redirect with error
            let clearCookie = "mcp_session=; Max-Age=0; Path=/"
                errorParams = "error=access_denied&error_description=User%20denied%20access"
                stateParam = case pendingState pending of
                    Just s -> "&state=" <> s
                    Nothing -> ""
                redirectUrl = pendingRedirectUri pending <> "?" <> errorParams <> stateParam

            -- Remove pending authorization
            liftIO $ atomically $ modifyTVar' oauthStateVar $ \s ->
                s{pendingAuthorizations = Map.delete sessionId (pendingAuthorizations s)}

            return $ addHeader redirectUrl $ addHeader clearCookie NoContent
        else do
            -- Validate credentials
            let oauthCfg = httpOAuthConfig config
                store = maybe (CredentialStore Map.empty "") credentialStore oauthCfg
                username = formUsername loginForm
                password = formPassword loginForm

            if validateCredential store username password
                then do
                    -- Generate authorization code
                    code <- liftIO $ generateAuthCodeWithConfig config
                    currentTime <- liftIO getCurrentTime
                    let expirySeconds = maybe 600 (fromIntegral . authCodeExpirySeconds) oauthCfg
                        expiry = addUTCTime expirySeconds currentTime
                        authCode =
                            AuthorizationCode
                                { authCode = code
                                , authClientId = pendingClientId pending
                                , authRedirectUri = pendingRedirectUri pending
                                , authCodeChallenge = pendingCodeChallenge pending
                                , authCodeChallengeMethod = pendingCodeChallengeMethod pending
                                , authScopes = maybe [] (T.splitOn " ") (pendingScope pending)
                                , authUserId = username
                                , authExpiry = expiry
                                }

                    -- Store authorization code and remove pending authorization
                    liftIO $ atomically $ modifyTVar' oauthStateVar $ \s ->
                        s
                            { authCodes = Map.insert code authCode (authCodes s)
                            , pendingAuthorizations = Map.delete sessionId (pendingAuthorizations s)
                            }

                    -- Build redirect URL with code
                    let stateParam = case pendingState pending of
                            Just s -> "&state=" <> s
                            Nothing -> ""
                        redirectUrl = pendingRedirectUri pending <> "?code=" <> code <> stateParam
                        clearCookie = "mcp_session=; Max-Age=0; Path=/"

                    return $ addHeader redirectUrl $ addHeader clearCookie NoContent
                else
                    -- Invalid credentials - return error
                    throwError err401{errBody = encode $ object ["error" .= ("authentication_failed" :: Text), "error_description" .= ("Invalid username or password" :: Text)]}

-- | Handle OAuth token endpoint
handleToken :: JWTSettings -> HTTPServerConfig -> TVar OAuthState -> [(Text, Text)] -> Handler TokenResponse
handleToken jwtSettings config oauthStateVar params = do
    let paramMap = Map.fromList params
    case Map.lookup "grant_type" paramMap of
        Just "authorization_code" -> handleAuthCodeGrant jwtSettings config oauthStateVar paramMap
        Just "refresh_token" -> handleRefreshTokenGrant jwtSettings config oauthStateVar paramMap
        Just _other -> throwError err400{errBody = encode $ object ["error" .= ("unsupported_grant_type" :: Text)]}
        Nothing -> throwError err400{errBody = encode $ object ["error" .= ("invalid_request" :: Text), "error_description" .= ("Missing grant_type" :: Text)]}

-- | Handle authorization code grant
handleAuthCodeGrant :: JWTSettings -> HTTPServerConfig -> TVar OAuthState -> Map Text Text -> Handler TokenResponse
handleAuthCodeGrant jwtSettings config oauthStateVar params = do
    -- Extract and log resource parameter (RFC8707)
    let mResource = Map.lookup "resource" params
    liftIO $ putStrLn $ "Resource parameter in token request: " <> maybe "not provided" T.unpack mResource

    code <- case Map.lookup "code" params of
        Just c -> return c
        Nothing -> throwError err400{errBody = encode $ object ["error" .= ("invalid_request" :: Text)]}

    codeVerifier <- case Map.lookup "code_verifier" params of
        Just v -> return v
        Nothing -> throwError err400{errBody = encode $ object ["error" .= ("invalid_request" :: Text)]}

    -- Look up authorization code
    oauthState <- liftIO $ readTVarIO oauthStateVar
    authCode <- case Map.lookup code (authCodes oauthState) of
        Just ac -> return ac
        Nothing -> throwError err400{errBody = encode $ object ["error" .= ("invalid_grant" :: Text)]}

    -- Verify code hasn't expired
    currentTime <- liftIO getCurrentTime
    when (currentTime > authExpiry authCode) $
        throwError err400{errBody = encode $ object ["error" .= ("invalid_grant" :: Text), "error_description" .= ("Authorization code expired" :: Text)]}

    -- Verify PKCE
    unless (validateCodeVerifier codeVerifier (authCodeChallenge authCode)) $
        throwError err400{errBody = encode $ object ["error" .= ("invalid_grant" :: Text), "error_description" .= ("Invalid code verifier" :: Text)]}

    -- Create user for JWT
    let oauthCfg = httpOAuthConfig config
        emailDomain = maybe "example.com" demoEmailDomain oauthCfg
        userName = maybe "User" demoUserName oauthCfg
        user =
            AuthUser
                { userId = authUserId authCode
                , userEmail = Just $ authUserId authCode <> "@" <> emailDomain
                , userName = Just userName
                }

    -- Generate tokens
    accessTokenText <- generateJWTAccessToken user jwtSettings
    refreshToken <- liftIO $ generateRefreshTokenWithConfig config

    -- Store tokens
    liftIO $ atomically $ modifyTVar' oauthStateVar $ \s ->
        s
            { authCodes = Map.delete code (authCodes s)
            , accessTokens = Map.insert accessTokenText user (accessTokens s)
            , refreshTokens = Map.insert refreshToken (accessTokenText, user) (refreshTokens s)
            }

    return
        TokenResponse
            { access_token = accessTokenText
            , token_type = "Bearer"
            , expires_in = Just $ maybe 3600 accessTokenExpirySeconds (httpOAuthConfig config)
            , refresh_token = Just refreshToken
            , scope = if null (authScopes authCode) then Nothing else Just (T.intercalate " " (authScopes authCode))
            }

-- | Handle refresh token grant
handleRefreshTokenGrant :: JWTSettings -> HTTPServerConfig -> TVar OAuthState -> Map Text Text -> Handler TokenResponse
handleRefreshTokenGrant jwtSettings config oauthStateVar params = do
    -- Extract and log resource parameter (RFC8707)
    let mResource = Map.lookup "resource" params
    liftIO $ putStrLn $ "Resource parameter in token request: " <> maybe "not provided" T.unpack mResource

    refreshToken <- case Map.lookup "refresh_token" params of
        Just t -> return t
        Nothing -> throwError err400{errBody = encode $ object ["error" .= ("invalid_request" :: Text)]}

    -- Look up refresh token
    oauthState <- liftIO $ readTVarIO oauthStateVar
    (oldAccessToken, user) <- case Map.lookup refreshToken (refreshTokens oauthState) of
        Just info -> return info
        Nothing -> throwError err400{errBody = encode $ object ["error" .= ("invalid_grant" :: Text)]}

    -- Generate new JWT access token
    newAccessTokenText <- generateJWTAccessToken user jwtSettings

    -- Update tokens
    liftIO $ atomically $ modifyTVar' oauthStateVar $ \s ->
        s
            { accessTokens = Map.insert newAccessTokenText user $ Map.delete oldAccessToken (accessTokens s)
            , refreshTokens = Map.insert refreshToken (newAccessTokenText, user) (refreshTokens s)
            }

    return
        TokenResponse
            { access_token = newAccessTokenText
            , token_type = "Bearer"
            , expires_in = Just $ maybe 3600 accessTokenExpirySeconds (httpOAuthConfig config)
            , refresh_token = Just refreshToken
            , scope = Nothing
            }

-- | Generate random authorization code
generateAuthCode :: IO Text
generateAuthCode = do
    uuid <- UUID.nextRandom
    return $ "code_" <> UUID.toText uuid

-- | Generate authorization code with configurable prefix
generateAuthCodeWithConfig :: HTTPServerConfig -> IO Text
generateAuthCodeWithConfig config = do
    uuid <- UUID.nextRandom
    let prefix = maybe "code_" authCodePrefix (httpOAuthConfig config)
    return $ prefix <> UUID.toText uuid

-- | Generate JWT access token for user
generateJWTAccessToken :: AuthUser -> JWTSettings -> Handler Text
generateJWTAccessToken user jwtSettings = do
    accessTokenResult <- liftIO $ makeJWT user jwtSettings Nothing
    case accessTokenResult of
        Left _err -> throwError err500{errBody = encode $ object ["error" .= ("Token generation failed" :: Text)]}
        Right accessToken -> return $ TE.decodeUtf8 $ LBS.toStrict accessToken

-- | Generate refresh token with configurable prefix
generateRefreshTokenWithConfig :: HTTPServerConfig -> IO Text
generateRefreshTokenWithConfig config = do
    uuid <- UUID.nextRandom
    let prefix = maybe "rt_" refreshTokenPrefix (httpOAuthConfig config)
    return $ prefix <> UUID.toText uuid

-- | Default demo OAuth configuration for testing purposes
defaultDemoOAuthConfig :: OAuthConfig
defaultDemoOAuthConfig =
    OAuthConfig
        { oauthEnabled = True
        , oauthProviders = []
        , tokenValidationEndpoint = Nothing
        , requireHTTPS = False -- For demo only
        -- Default timing parameters
        , authCodeExpirySeconds = 600 -- 10 minutes
        , accessTokenExpirySeconds = 3600 -- 1 hour
        -- Default OAuth parameters
        , supportedScopes = ["mcp:read", "mcp:write"]
        , supportedResponseTypes = ["code"]
        , supportedGrantTypes = ["authorization_code", "refresh_token"]
        , supportedAuthMethods = ["none"]
        , supportedCodeChallengeMethods = ["S256"]
        , -- Demo mode settings (no longer auto-approve, now requires login)
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
        , scopesSupported = Just ["mcp:read", "mcp:write"]
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

-- | Render login page HTML
renderLoginPage :: Text -> Text -> Maybe Text -> Text -> Text
renderLoginPage clientName scopes mResource sessionId =
    T.unlines
        [ "<!DOCTYPE html>"
        , "<html>"
        , "<head>"
        , "  <meta charset=\"utf-8\">"
        , "  <title>Sign In - MCP Server</title>"
        , "  <style>"
        , "    body { font-family: system-ui, sans-serif; max-width: 500px; margin: 50px auto; padding: 20px; }"
        , "    h1 { color: #333; }"
        , "    form { margin-top: 20px; }"
        , "    label { display: block; margin: 15px 0 5px; }"
        , "    input[type=text], input[type=password] { width: 100%; padding: 8px; box-sizing: border-box; }"
        , "    button { margin-top: 20px; margin-right: 10px; padding: 10px 20px; }"
        , "    .info { background: #f0f0f0; padding: 15px; border-radius: 5px; margin: 20px 0; }"
        , "  </style>"
        , "</head>"
        , "<body>"
        , "  <h1>Sign In</h1>"
        , "  <div class=\"info\">"
        , "    <p>Application <strong>" <> escapeHtml clientName <> "</strong> is requesting access.</p>"
        , "    <p>Permissions requested: " <> escapeHtml (formatScopeDescriptions scopes) <> "</p>"
        , case mResource of
            Just res -> "    <p>Resource: " <> escapeHtml res <> "</p>"
            Nothing -> ""
        , "  </div>"
        , "  <form method=\"POST\" action=\"/login\">"
        , "    <input type=\"hidden\" name=\"session_id\" value=\"" <> escapeHtml sessionId <> "\">"
        , "    <label>Username:"
        , "      <input type=\"text\" name=\"username\" required autofocus>"
        , "    </label>"
        , "    <label>Password:"
        , "      <input type=\"password\" name=\"password\" required>"
        , "    </label>"
        , "    <button type=\"submit\" name=\"action\" value=\"login\">Sign In</button>"
        , "    <button type=\"submit\" name=\"action\" value=\"deny\">Deny</button>"
        , "  </form>"
        , "</body>"
        , "</html>"
        ]

-- | Render error page HTML
renderErrorPage :: Text -> Text -> Text
renderErrorPage errorTitle errorMessage =
    T.unlines
        [ "<!DOCTYPE html>"
        , "<html>"
        , "<head>"
        , "  <meta charset=\"utf-8\">"
        , "  <title>Error - MCP Server</title>"
        , "  <style>"
        , "    body { font-family: system-ui, sans-serif; max-width: 500px; margin: 50px auto; padding: 20px; }"
        , "    h1 { color: #d32f2f; }"
        , "    .error { background: #ffebee; padding: 15px; border-radius: 5px; border-left: 4px solid #d32f2f; }"
        , "  </style>"
        , "</head>"
        , "<body>"
        , "  <h1>" <> escapeHtml errorTitle <> "</h1>"
        , "  <div class=\"error\">"
        , "    <p>" <> escapeHtml errorMessage <> "</p>"
        , "  </div>"
        , "  <p>Please contact the application developer.</p>"
        , "</body>"
        , "</html>"
        ]

-- | Escape HTML special characters
escapeHtml :: Text -> Text
escapeHtml = T.replace "&" "&amp;" . T.replace "<" "&lt;" . T.replace ">" "&gt;" . T.replace "\"" "&quot;" . T.replace "'" "&#39;"

-- | Run the MCP server as an HTTP server
runServerHTTP :: (MCPServer MCPServerM) => HTTPServerConfig -> IO ()
runServerHTTP config = do
    -- Initialize JWT settings if OAuth is enabled
    jwtSettings <- case httpJWK config of
        Just jwk -> return $ defaultJWTSettings jwk
        Nothing -> defaultJWTSettings <$> generateKey

    -- Initialize the server state
    stateVar <- newTVarIO $ initialServerState (httpCapabilities config)

    -- Initialize OAuth state
    oauthStateVar <-
        newTVarIO $
            OAuthState
                { authCodes = Map.empty
                , accessTokens = Map.empty
                , refreshTokens = Map.empty
                , registeredClients = Map.empty
                , pendingAuthorizations = Map.empty
                }

    putStrLn $ "Starting MCP HTTP Server on port " ++ show (httpPort config) ++ "..."

    when (maybe False oauthEnabled (httpOAuthConfig config)) $ do
        putStrLn "OAuth authentication enabled"
        putStrLn $ "Authorization endpoint: " ++ T.unpack (httpBaseUrl config) ++ "/authorize"
        putStrLn $ "Token endpoint: " ++ T.unpack (httpBaseUrl config) ++ "/token"
        case httpOAuthConfig config >>= \cfg -> if null (oauthProviders cfg) then Nothing else Just (oauthProviders cfg) of
            Just providers -> do
                putStrLn $ "OAuth providers: " ++ T.unpack (T.intercalate ", " (map providerName providers))
                when (any requiresPKCE providers) $ putStrLn "PKCE enabled (required by MCP spec)"
            Nothing -> return ()

    run (httpPort config) (mcpApp config stateVar oauthStateVar jwtSettings)
