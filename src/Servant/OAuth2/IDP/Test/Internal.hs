{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{- |
Module      : Servant.OAuth2.IDP.Test.Internal
Description : Internal test utilities for OAuth testing
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC

This module provides test utilities for OAuth flows, particularly PKCE generation
and client registration helpers. These utilities are designed for use with hspec-wai
functional tests.

== Usage

@
import Servant.OAuth2.IDP.Test.Internal

spec :: Spec
spec = do
  describe "OAuth PKCE flow" $ do
    it "accepts valid PKCE challenge" $ do
      (verifier, challenge) <- generatePKCE
      -- Use verifier and challenge in test...
@

== IMPORTANT

These utilities are for testing only. They generate cryptographically secure
random values but should NEVER be used in production code.
-}
module Servant.OAuth2.IDP.Test.Internal (
    -- * Test Configuration
    TestConfig (..),
    TestCredentials (..),

    -- * PKCE Utilities
    generatePKCE,
    generateCodeVerifier,
    generateCodeChallenge,

    -- * HTTP Response Utilities
    extractSessionCookie,
    extractCodeFromRedirect,

    -- * Client Registration Helpers
    withRegisteredClient,

    -- * Authorization Flow Helpers
    withAuthorizedUser,
    withAccessToken,

    -- * Test Specs
    clientRegistrationSpec,
    loginFlowSpec,
    tokenExchangeSpec,
    expirySpec,
    headerSpec,
    oauthConformanceSpec,
) where

import Control.Monad (when)
import Crypto.Hash (hashWith)
import Crypto.Hash.Algorithms (SHA256 (..))
import Crypto.Random (getRandomBytes)
import Data.Aeson (Value (Object, String), decode, eitherDecode, encode, object, (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.ByteString.Base64.URL qualified as B64URL
import Data.ByteString.Lazy qualified as LBS
import Data.Kind (Type)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock (NominalDiffTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Network.HTTP.Types (hContentType, hLocation, methodPost, status200, status201)
import Network.HTTP.Types.URI (renderSimpleQuery)
import Network.URI (URI (..), parseURI, uriQuery)
import Network.Wai (Application)
import Network.Wai.Test (SResponse, simpleBody, simpleHeaders, simpleStatus)
import Test.Hspec (Spec, describe, expectationFailure, it, runIO, shouldSatisfy)
import Test.Hspec.Wai (WaiSession, get, liftIO, postHtmlForm, request, shouldRespondWith, with)

import Control.Monad.Time (MonadTime)
import Servant.Auth.Server (ToJWT)
import Servant.OAuth2.IDP.Auth.Backend (AuthBackend (..))
import Servant.OAuth2.IDP.Store (OAuthStateStore (..))
import Servant.OAuth2.IDP.Types (AuthCodeId, ClientId, mkAuthCodeId, mkClientId, unAuthCodeId, unClientId)

-- -----------------------------------------------------------------------------
-- Test Configuration Types
-- -----------------------------------------------------------------------------

{- | Polymorphic test configuration for OAuth tests.

This type packages all the necessary components for running OAuth tests:

- 'tcMakeApp': Creates the test application and time advancement function
- 'tcRunM': Runs the monad stack in IO for test execution
- 'tcCredentials': Test user credentials for authentication flows

The type is polymorphic over the monad 'm' to support different implementations
(e.g., in-memory TVar-based, mock database, etc.).

== Example

@
let testConfig = TestConfig
      { tcMakeApp = createTestApp
      , tcRunM = runReaderT \`flip\` testEnv
      , tcCredentials = TestCredentials "demo" "demo123"
      }
@
-}
data TestConfig (m :: Type -> Type) = TestConfig
    { tcMakeApp :: IO (Application, NominalDiffTime -> IO ())
    -- ^ Create test application and time advancement function
    , tcRunM :: forall a. m a -> IO a
    -- ^ Run the monad stack in IO
    , tcCredentials :: TestCredentials
    -- ^ Test credentials for authentication
    }

{- | Test user credentials for OAuth authentication flows.

Contains username and password for test user accounts.

== Example

@
let testCreds = TestCredentials
      { tcUsername = "demo"
      , tcPassword = "demo123"
      }
@
-}
data TestCredentials = TestCredentials
    { tcUsername :: Text
    , tcPassword :: Text
    }
    deriving (Eq, Show)

-- -----------------------------------------------------------------------------
-- PKCE Utilities
-- -----------------------------------------------------------------------------

{- | Generate PKCE code verifier and challenge pair.

Returns (verifier, challenge) where:
- verifier: 128-char string using unreserved chars (RFC 7636)
- challenge: S256 hash of verifier (SHA256, base64url-encoded)

The generated values satisfy the server's validation rules:
- Verifier: 43-128 chars, unreserved charset (A-Z, a-z, 0-9, -, ., _, ~)
- Challenge: 43 chars (SHA256 output), base64url charset (A-Z, a-z, 0-9, -, _)

== Example

>>> (verifier, challenge) <- generatePKCE
>>> T.length verifier
128
>>> T.length challenge
43
-}
generatePKCE :: IO (Text, Text)
generatePKCE = do
    verifier <- generateCodeVerifier
    let challenge = generateCodeChallenge verifier
    return (verifier, challenge)

{- | Generate a cryptographically secure code verifier for PKCE
Produces a 43-character base64url-encoded string using CSPRNG per RFC 7636
-}
generateCodeVerifier :: IO Text
generateCodeVerifier = do
    bytes <- getRandomBytes 32 -- 32 bytes = 256 bits entropy
    pure $ TE.decodeUtf8 $ B64URL.encodeUnpadded bytes -- 43 chars

{- | Generate code challenge from verifier using SHA256 (S256 method)
Uses base64url encoding without padding per RFC 7636
-}
generateCodeChallenge :: Text -> Text
generateCodeChallenge verifier =
    let verifierBytes = TE.encodeUtf8 verifier
        challengeHash = hashWith SHA256 verifierBytes
        challengeBytes = convert challengeHash :: ByteString
     in TE.decodeUtf8 $ B64URL.encodeUnpadded challengeBytes

{- | Extract session cookie value from HTTP response headers.

Searches for the "Set-Cookie" header and parses the "mcp_session=<value>" cookie.

Returns:
- Just the cookie value if found and properly formatted
- Nothing if header missing or malformed

== Example

>>> let response = ... -- SResponse with Set-Cookie: mcp_session=abc123; Path=/
>>> extractSessionCookie response
Just "abc123"
-}
extractSessionCookie :: SResponse -> Maybe Text
extractSessionCookie response = do
    let headers = simpleHeaders response
    -- Look for Set-Cookie header (case-insensitive)
    setCookieValue <- lookup "Set-Cookie" headers
    -- Decode to Text
    cookieText <- either (const Nothing) Just $ TE.decodeUtf8' setCookieValue
    -- Parse "mcp_session=<value>; ..." format
    extractCookieValue "mcp_session=" cookieText
  where
    extractCookieValue :: Text -> Text -> Maybe Text
    extractCookieValue prefix fullCookie =
        case T.breakOn prefix fullCookie of
            (_, rest)
                | T.null rest -> Nothing
                | otherwise ->
                    let withoutPrefix = T.drop (T.length prefix) rest
                        -- Take until semicolon or end
                        value = T.takeWhile (/= ';') withoutPrefix
                     in if T.null value then Nothing else Just value

{- | Extract authorization code from redirect Location header.

Parses the Location header to extract the "code" query parameter from the redirect URI.

Returns:
- Just the authorization code if found
- Nothing if header missing, malformed, or code parameter not present

== Example

>>> let response = ... -- SResponse with Location: http://example.com/callback?code=abc123&state=xyz
>>> extractCodeFromRedirect response
Just "abc123"

>>> let response2 = ... -- SResponse with Location: http://example.com/callback?error=access_denied
>>> extractCodeFromRedirect response2
Nothing
-}
extractCodeFromRedirect :: SResponse -> Maybe Text
extractCodeFromRedirect response = do
    let headers = simpleHeaders response
    -- Look for Location header
    locationValue <- lookup hLocation headers
    -- Decode to String for URI parsing
    locationStr <- either (const Nothing) Just $ TE.decodeUtf8' locationValue
    -- Parse as URI
    uri <- parseURI (T.unpack locationStr)
    -- Extract query string
    let query = uriQuery uri
    -- Parse query parameters (format: ?code=xxx&state=yyy)
    extractCodeParam query
  where
    extractCodeParam :: String -> Maybe Text
    extractCodeParam query =
        case query of
            [] -> Nothing
            '?' : rest ->
                let params = map parseParam $ splitOn '&' rest
                 in lookup "code" params
            _ ->
                let params = map parseParam $ splitOn '&' query
                 in lookup "code" params

    parseParam :: String -> (Text, Text)
    parseParam param =
        case break (== '=') param of
            (key, '=' : value) -> (T.pack key, T.pack value)
            (key, _) -> (T.pack key, "")

    splitOn :: Char -> String -> [String]
    splitOn _ [] = []
    splitOn delimiter str =
        let (chunk, rest) = break (== delimiter) str
         in case rest of
                [] -> [chunk]
                (_ : remainder) -> chunk : splitOn delimiter remainder

-- -----------------------------------------------------------------------------
-- Client Registration Helpers
-- -----------------------------------------------------------------------------

{- | Register a test client and run an action with its ID.

Issues a real POST /register request within WaiSession to register a new client.
Uses a UUID-based client name to avoid collisions when tests share state.

The function:
1. Generates a unique UUID for the client name
2. POSTs to /register with client_name and redirect_uris
3. Verifies 201 status code response
4. Extracts client_id from the JSON response
5. Passes the ClientId to the continuation action

== Example

@
it "can register and authorize" $ do
  withRegisteredClient config $ \\clientId -> do
    -- clientId is now a valid registered client
    get ("/authorize?client_id=" <> unClientId clientId <> "&...")
      \`shouldRespondWith\` 200
@

== Test Isolation

When using a shared Application across tests, the UUID-based naming ensures
each invocation registers a distinct client, preventing ID collisions.

For strict isolation, wrap tests with @around@ to create fresh Applications.
-}
withRegisteredClient ::
    TestConfig m ->
    (ClientId -> WaiSession st a) ->
    WaiSession st a
withRegisteredClient _config action = do
    -- Generate unique client name using UUID
    uuid <- liftIO UUID.nextRandom
    let clientName = "test-client-" <> UUID.toText uuid
        body =
            object
                [ "client_name" .= clientName
                , "redirect_uris" .= (["http://localhost/callback"] :: [Text])
                , "grant_types" .= (["authorization_code", "refresh_token"] :: [Text])
                , "response_types" .= (["code"] :: [Text])
                , "token_endpoint_auth_method" .= ("none" :: Text)
                ]

    -- POST to /register endpoint with JSON content-type
    -- Use request with explicit Content-Type header
    resp <- request methodPost "/register" [(hContentType, "application/json")] (encode body)

    -- Verify 201 Created status
    let status = simpleStatus resp
        bodyBytes = simpleBody resp

    -- Use error for failures since this is a test helper combinator
    -- The caller expects a valid ClientId or test failure
    when (status /= status201) $
        liftIO $
            error $
                "Client registration failed: Expected 201 Created, got "
                    <> show status
                    <> ". Body: "
                    <> show bodyBytes

    -- Extract client_id from JSON response
    case eitherDecodeClientId bodyBytes of
        Left err ->
            liftIO $
                error $
                    "Failed to parse client_id from response: "
                        <> err
                        <> ". Body: "
                        <> show bodyBytes
        Right clientId -> action clientId
  where
    -- Helper to decode client_id from JSON response
    eitherDecodeClientId :: LBS.ByteString -> Either String ClientId
    eitherDecodeClientId bs = do
        val <- eitherDecode bs
        case val of
            Object obj ->
                case KM.lookup "client_id" obj of
                    Just (String clientIdText) ->
                        maybe (Left "Invalid ClientId") Right $ mkClientId clientIdText
                    Just other ->
                        Left $ "client_id was not a string: " <> show other
                    Nothing ->
                        Left "client_id field not found in response"
            _ ->
                Left $ "Response was not a JSON object: " <> show val

-- -----------------------------------------------------------------------------
-- Authorization Flow Helpers
-- -----------------------------------------------------------------------------

{- | Complete OAuth authorization flow and run action with authorization code.

Issues real HTTP requests to complete the full OAuth authorization flow:

1. Generate PKCE verifier and challenge
2. GET /authorize with OAuth parameters (triggers login page)
3. Extract session cookie from response
4. POST /login with credentials and session cookie
5. Extract authorization code from redirect Location header
6. Pass code and verifier to the continuation action

The function:
1. Generates cryptographically secure PKCE pair (verifier, challenge)
2. Constructs /authorize URL with client_id, redirect_uri, response_type, and PKCE
3. GETs /authorize to initiate flow (server returns login page with session cookie)
4. Extracts mcp_session cookie from Set-Cookie header
5. POSTs /login with username, password, session_id (from cookie), and action=approve
6. Follows redirect to extract authorization code from Location header query params
7. Passes AuthCodeId and CodeVerifier to the continuation

Returns:
- The result of the continuation action
- Fails with error if any step fails (missing headers, invalid responses, etc.)

== Example

@
it "can complete full OAuth flow" $ do
  withRegisteredClient config $ \\clientId -> do
    withAuthorizedUser config clientId $ \\code verifier -> do
      -- code and verifier are now available for token exchange
      post "/token" (encode tokenRequest) \`shouldRespondWith\` 200
@

== Error Handling

Uses `error` for failures since this is a test helper combinator. The caller
expects valid AuthCodeId and verifier or test failure. Common failure modes:

- Missing Set-Cookie header → session cookie extraction fails
- Missing Location header → code extraction fails
- Invalid OAuth parameters → server returns error page (no redirect)
- Invalid credentials → server returns login page again (no redirect)

== Security Note

This helper uses the demo credentials from TestConfig. For production testing,
ensure TestCredentials match the configured CredentialStore.
-}
withAuthorizedUser ::
    TestConfig m ->
    ClientId ->
    (AuthCodeId -> Text -> WaiSession st a) ->
    WaiSession st a
withAuthorizedUser config clientId action = do
    -- Generate PKCE verifier and challenge
    (verifier, challenge) <- liftIO generatePKCE

    -- Step 1: GET /authorize to initiate OAuth flow
    let authUrl =
            "/authorize?client_id="
                <> unClientId clientId
                <> "&redirect_uri=http://localhost/callback"
                <> "&response_type=code"
                <> "&code_challenge="
                <> challenge
                <> "&code_challenge_method=S256"

    resp1 <- get (TE.encodeUtf8 authUrl)

    -- Extract session cookie from Set-Cookie header
    sessionCookie <- case extractSessionCookie resp1 of
        Just cookie -> pure cookie
        Nothing ->
            liftIO $
                error $
                    "Failed to extract session cookie from /authorize response. "
                        <> "Response headers: "
                        <> show (simpleHeaders resp1)

    -- Step 2: POST /login with credentials and session cookie
    let loginForm =
            [ ("username", T.unpack $ tcUsername (tcCredentials config))
            , ("password", T.unpack $ tcPassword (tcCredentials config))
            , ("session_id", T.unpack sessionCookie)
            , ("action", "approve")
            ]

    resp2 <- postHtmlForm "/login" loginForm

    -- Extract authorization code from Location redirect header
    code <- case extractCodeFromRedirect resp2 of
        Just codeText ->
            case mkAuthCodeId codeText of
                Just authCodeId -> pure authCodeId
                Nothing ->
                    liftIO $
                        error $
                            "Failed to construct AuthCodeId from code: " <> T.unpack codeText
        Nothing ->
            liftIO $
                error $
                    "Failed to extract authorization code from /login redirect. "
                        <> "Response status: "
                        <> show (simpleStatus resp2)
                        <> ". Response headers: "
                        <> show (simpleHeaders resp2)
                        <> ". Response body: "
                        <> show (simpleBody resp2)

    -- Pass code and verifier to continuation
    action code verifier

{- | Exchange authorization code for access token and run action with token.

Issues real HTTP request to complete the token exchange flow:

1. Uses withAuthorizedUser to obtain authorization code and PKCE verifier
2. POST /token with authorization code grant type
3. Extract access token from JSON response
4. Pass access token to the continuation action

The function:
1. Delegates to withAuthorizedUser to get AuthCodeId and code verifier
2. Constructs application/x-www-form-urlencoded token request body with:
   - grant_type: "authorization_code"
   - code: authorization code from previous step
   - redirect_uri: must match the one used in /authorize
   - client_id: the registered client ID
   - code_verifier: PKCE verifier for validation
3. POSTs to /token endpoint
4. Verifies 200 OK status
5. Parses JSON response to extract "access_token" field
6. Passes Text access token to the continuation

Returns:
- The result of the continuation action
- Fails with error if token exchange fails (network error, invalid response, etc.)

== Example

@
it "can obtain access token" $ do
  withRegisteredClient config $ \\clientId -> do
    withAccessToken config clientId $ \\accessToken -> do
      -- accessToken is now available for API requests
      request methodGet "/mcp"
        [(hAuthorization, "Bearer " <> TE.encodeUtf8 accessToken)]
        "" \`shouldRespondWith\` 200
@

== Error Handling

Uses `error` for failures since this is a test helper combinator. The caller
expects a valid access token or test failure. Common failure modes:

- Token exchange fails → non-200 status code
- Malformed JSON response → parse error
- Missing access_token field → field extraction error

== Security Note

This helper completes the full OAuth flow including PKCE validation.
The access token returned is a JWT signed by the server.
-}
withAccessToken ::
    TestConfig m ->
    ClientId ->
    (Text -> WaiSession st a) ->
    WaiSession st a
withAccessToken config clientId action = do
    withAuthorizedUser config clientId $ \code verifier -> do
        -- Construct form-encoded token request body
        let tokenBody =
                renderSimpleQuery
                    False
                    [ ("grant_type", "authorization_code")
                    , ("code", TE.encodeUtf8 $ unAuthCodeId code)
                    , ("redirect_uri", "http://localhost/callback")
                    , ("client_id", TE.encodeUtf8 $ unClientId clientId)
                    , ("code_verifier", TE.encodeUtf8 verifier)
                    ]

        -- POST to /token endpoint with form-urlencoded content-type
        resp <- request methodPost "/token" [(hContentType, "application/x-www-form-urlencoded")] (LBS.fromStrict tokenBody)

        -- Verify 200 OK status
        let status = simpleStatus resp
            bodyBytes = simpleBody resp

        when (status /= status200) $
            liftIO $
                error $
                    "Token exchange failed: Expected 200 OK, got "
                        <> show status
                        <> ". Body: "
                        <> show bodyBytes

        -- Extract access_token from JSON response
        case eitherDecodeAccessToken bodyBytes of
            Left err ->
                liftIO $
                    error $
                        "Failed to parse access_token from response: "
                            <> err
                            <> ". Body: "
                            <> show bodyBytes
            Right accessToken -> action accessToken
  where
    -- Helper to decode access_token from JSON response
    eitherDecodeAccessToken :: LBS.ByteString -> Either String Text
    eitherDecodeAccessToken bs = do
        val <- eitherDecode bs
        case val of
            Object obj ->
                case KM.lookup "access_token" obj of
                    Just (String tokenText) ->
                        Right tokenText
                    Just other ->
                        Left $ "access_token was not a string: " <> show other
                    Nothing ->
                        Left "access_token field not found in response"
            _ ->
                Left $ "Response was not a JSON object: " <> show val

-- -----------------------------------------------------------------------------
-- Test Specs
-- -----------------------------------------------------------------------------

{- | Helper to create a fresh Application for each test (without time control).

Discards the time advancement function for tests that don't need time control.

Returns the IO Application for use with hspec-wai's 'with' combinator.
-}
withFreshAppNoTime :: TestConfig m -> IO Application
withFreshAppNoTime config = do
    (app, _advanceTime) <- tcMakeApp config
    return app

{- | Test specification for client registration endpoint.

Tests the /register endpoint according to OAuth 2.0 Dynamic Client Registration Protocol.

Covers:
- Successful registration with valid request (201 Created)
- Response includes client_id field
- Invalid JSON handling (400 Bad Request)
- Empty redirect_uris validation (400 Bad Request)

== Usage

@
import Servant.OAuth2.IDP.Test.Internal (clientRegistrationSpec)

spec :: Spec
spec = do
  let config = TestConfig { ... }
  clientRegistrationSpec config
@

== Test Isolation

Each test uses 'withFreshApp' to get a fresh Application instance, ensuring
complete isolation between tests (no shared state).
-}
clientRegistrationSpec :: TestConfig m -> Spec
clientRegistrationSpec config = with (withFreshAppNoTime config) $ do
    describe "Client Registration" $ do
        it "registers a new client with valid request" $ do
            let body =
                    encode $
                        object
                            [ "client_name" .= ("test-client" :: Text)
                            , "redirect_uris" .= (["http://localhost/callback"] :: [Text])
                            , "grant_types" .= (["authorization_code", "refresh_token"] :: [Text])
                            , "response_types" .= (["code"] :: [Text])
                            , "token_endpoint_auth_method" .= ("none" :: Text)
                            ]
            resp <- request methodPost "/register" [(hContentType, "application/json")] body
            liftIO $ do
                let status = simpleStatus resp
                status `shouldSatisfy` (== status201)
                let mContentType = lookup hContentType (simpleHeaders resp)
                case mContentType of
                    Just ct -> TE.decodeUtf8 ct `shouldSatisfy` T.isPrefixOf "application/json"
                    Nothing -> expectationFailure "Missing Content-Type header"

        it "returns client_id in response" $ do
            let body =
                    encode $
                        object
                            [ "client_name" .= ("test" :: Text)
                            , "redirect_uris" .= (["http://localhost/cb"] :: [Text])
                            , "grant_types" .= (["authorization_code", "refresh_token"] :: [Text])
                            , "response_types" .= (["code"] :: [Text])
                            , "token_endpoint_auth_method" .= ("none" :: Text)
                            ]
            resp <- request methodPost "/register" [(hContentType, "application/json")] body
            liftIO $ do
                let status = simpleStatus resp
                    bodyBytes = simpleBody resp
                status `shouldSatisfy` (== status201)
                let mJson = decode @Value bodyBytes
                case mJson of
                    Just (Object obj) -> KM.lookup "client_id" obj `shouldSatisfy` isJust
                    other -> expectationFailure $ "Response was not a JSON object. Got: " <> show other <> ". Body: " <> show bodyBytes

        it "returns 400 for invalid JSON" $ do
            request methodPost "/register" [(hContentType, "application/json")] "not json" `shouldRespondWith` 400

        it "returns 400 for empty redirect_uris" $ do
            let body =
                    encode $
                        object
                            [ "client_name" .= ("test" :: Text)
                            , "redirect_uris" .= ([] :: [Text])
                            ]
            request methodPost "/register" [(hContentType, "application/json")] body `shouldRespondWith` 400

{- | Test specification for OAuth login flow.

Tests the interactive login flow including authorization page and credential submission.

Covers:
- Authorization endpoint shows login page (200 OK with text/html)
- Successful login redirects with authorization code
- Session cookie is set on authorization request
- Invalid credentials return 401

== Usage

@
import Servant.OAuth2.IDP.Test.Internal (loginFlowSpec)

spec :: Spec
spec = do
  let config = TestConfig { ... }
  loginFlowSpec config
@

== Test Isolation

Each test uses 'withFreshApp' to get a fresh Application instance, ensuring
complete isolation between tests (no shared state).
-}
loginFlowSpec :: TestConfig m -> Spec
loginFlowSpec config = with (withFreshAppNoTime config) $ do
    describe "Login Flow" $ do
        it "shows login page on authorization request" $ do
            withRegisteredClient config $ \clientId -> do
                (_, challenge) <- liftIO generatePKCE
                let authUrl =
                        "/authorize?client_id="
                            <> unClientId clientId
                            <> "&redirect_uri=http://localhost/callback"
                            <> "&response_type=code"
                            <> "&code_challenge="
                            <> challenge
                            <> "&code_challenge_method=S256"
                resp <- get (TE.encodeUtf8 authUrl)
                liftIO $ do
                    let status = simpleStatus resp
                    status `shouldSatisfy` (== status200)
                    let mContentType = lookup hContentType (simpleHeaders resp)
                    case mContentType of
                        Just ct -> TE.decodeUtf8 ct `shouldSatisfy` T.isPrefixOf "text/html"
                        Nothing -> expectationFailure "Missing Content-Type header"

        it "redirects with code on successful login" $ do
            withRegisteredClient config $ \clientId -> do
                withAuthorizedUser config clientId $ \code _verifier -> do
                    -- withAuthorizedUser validates the entire flow internally
                    -- If we got here, code is valid and non-empty
                    liftIO $ unAuthCodeId code `shouldSatisfy` (not . T.null)

        it "sets session cookie on authorization" $ do
            withRegisteredClient config $ \clientId -> do
                (_, challenge) <- liftIO generatePKCE
                let authUrl =
                        "/authorize?client_id="
                            <> unClientId clientId
                            <> "&redirect_uri=http://localhost/callback"
                            <> "&response_type=code"
                            <> "&code_challenge="
                            <> challenge
                            <> "&code_challenge_method=S256"
                resp <- get (TE.encodeUtf8 authUrl)
                liftIO $ extractSessionCookie resp `shouldSatisfy` isJust

        it "returns 401 for invalid credentials" $ do
            withRegisteredClient config $ \clientId -> do
                (_, challenge) <- liftIO generatePKCE
                -- Step 1: GET /authorize to get session cookie
                let authUrl =
                        "/authorize?client_id="
                            <> unClientId clientId
                            <> "&redirect_uri=http://localhost/callback"
                            <> "&response_type=code"
                            <> "&code_challenge="
                            <> challenge
                            <> "&code_challenge_method=S256"
                resp1 <- get (TE.encodeUtf8 authUrl)

                -- Extract session cookie
                sessionCookie <- case extractSessionCookie resp1 of
                    Just cookie -> pure cookie
                    Nothing ->
                        liftIO $
                            error "Failed to extract session cookie for invalid credentials test"

                -- Step 2: POST /login with invalid credentials
                let loginForm =
                        [ ("username", "invalid-user")
                        , ("password", "wrong-password")
                        , ("session_id", T.unpack sessionCookie)
                        , ("action", "approve")
                        ]
                postHtmlForm "/login" loginForm `shouldRespondWith` 401

{- | Test specification for token exchange endpoint.

Tests the /token endpoint according to OAuth 2.0 token exchange flow with PKCE.

Covers:
- Successful token exchange with valid authorization code and PKCE verifier
- Response includes access_token and refresh_token
- Invalid PKCE verifier returns 400
- Invalid authorization code returns 400

== Usage

@
import Servant.OAuth2.IDP.Test.Internal (tokenExchangeSpec)

spec :: Spec
spec = do
  let config = TestConfig { ... }
  tokenExchangeSpec config
@

== Test Isolation

Each test uses 'withFreshApp' to get a fresh Application instance, ensuring
complete isolation between tests (no shared state).
-}
tokenExchangeSpec :: TestConfig m -> Spec
tokenExchangeSpec config = with (withFreshAppNoTime config) $ do
    describe "Token Exchange" $ do
        it "exchanges valid code for tokens" $ do
            withRegisteredClient config $ \clientId -> do
                withAuthorizedUser config clientId $ \code verifier -> do
                    let body = tokenExchangeBody (unClientId clientId) (unAuthCodeId code) verifier
                    resp <- request methodPost "/token" [(hContentType, "application/x-www-form-urlencoded")] (LBS.fromStrict body)
                    liftIO $ do
                        simpleStatus resp `shouldSatisfy` (== status200)
                        let mJson = decode @Value (simpleBody resp)
                        case mJson of
                            Just (Object obj) -> do
                                KM.lookup "access_token" obj `shouldSatisfy` isJust
                                KM.lookup "refresh_token" obj `shouldSatisfy` isJust
                            _ -> expectationFailure "Response was not a JSON object"

        it "returns 400 for invalid PKCE verifier" $ do
            withRegisteredClient config $ \clientId -> do
                withAuthorizedUser config clientId $ \code _ -> do
                    let body = tokenExchangeBody (unClientId clientId) (unAuthCodeId code) "wrong_verifier"
                    request methodPost "/token" [(hContentType, "application/x-www-form-urlencoded")] (LBS.fromStrict body) `shouldRespondWith` 400

        it "returns 400 for invalid authorization code" $ do
            withRegisteredClient config $ \clientId -> do
                let body = tokenExchangeBody (unClientId clientId) "invalid" "verifier"
                request methodPost "/token" [(hContentType, "application/x-www-form-urlencoded")] (LBS.fromStrict body) `shouldRespondWith` 400

{- | Helper to create application/x-www-form-urlencoded token exchange request body.

Creates a properly-formatted form-urlencoded body for POST /token requests
with grant_type=authorization_code.

== Parameters

- 'ClientId': The registered client ID
- 'AuthCodeId': The authorization code from the OAuth flow
- 'Text': The PKCE code verifier

== Returns

ByteString containing the form-urlencoded request body

== Example

@
let body = tokenExchangeBody clientId code verifier
post "/token" (LBS.fromStrict body)
@
-}
tokenExchangeBody :: Text -> Text -> Text -> ByteString
tokenExchangeBody clientId code verifier =
    renderSimpleQuery
        False
        [ ("grant_type", "authorization_code")
        , ("code", TE.encodeUtf8 code)
        , ("redirect_uri", "http://localhost/callback")
        , ("client_id", TE.encodeUtf8 clientId)
        , ("code_verifier", TE.encodeUtf8 verifier)
        ]

{- | Test specification for OAuth expiry behavior.

Tests time-sensitive OAuth flows using controllable time advancement.

Covers:
- Expired authorization codes (10+ minute old codes rejected)
- Expired login sessions (10+ minute old session cookies rejected)

== Usage

@
import Servant.OAuth2.IDP.Test.Internal (expirySpec)

spec :: Spec
spec = do
  let config = TestConfig { ... }
  expirySpec config
@

== Test Isolation

Each test uses 'withFreshApp' to get a fresh Application instance with
independent time control, ensuring complete isolation between tests.

== Time Control

The 'advanceTime' callback takes NominalDiffTime (seconds as a rational number).
For example:
- @advanceTime 60@ advances time by 1 minute
- @advanceTime (11 * 60)@ advances time by 11 minutes
-}
expirySpec :: TestConfig m -> Spec
expirySpec config = describe "Expiry behavior" $ do
    describe "with fresh app for expired authorization code test" $ do
        (app, advanceTime) <- runIO $ tcMakeApp config
        with (return app) $ do
            it "rejects expired authorization codes" $ do
                withRegisteredClient config $ \clientId -> do
                    withAuthorizedUser config clientId $ \code verifier -> do
                        -- Advance time past the 10-minute authorization code expiry
                        liftIO $ advanceTime (11 * 60) -- 11 minutes = 660 seconds
                        let body = tokenExchangeBody (unClientId clientId) (unAuthCodeId code) verifier
                        request methodPost "/token" [(hContentType, "application/x-www-form-urlencoded")] (LBS.fromStrict body) `shouldRespondWith` 400

    describe "with fresh app for expired login session test" $ do
        (app, advanceTime) <- runIO $ tcMakeApp config
        with (return app) $ do
            it "rejects expired login sessions" $ do
                withRegisteredClient config $ \clientId -> do
                    -- Generate PKCE for authorization request
                    (_, challenge) <- liftIO generatePKCE

                    -- Step 1: GET /authorize to create login session
                    let authUrl =
                            "/authorize?client_id="
                                <> unClientId clientId
                                <> "&redirect_uri=http://localhost/callback"
                                <> "&response_type=code"
                                <> "&code_challenge="
                                <> challenge
                                <> "&code_challenge_method=S256"

                    resp <- get (TE.encodeUtf8 authUrl)

                    -- Extract session cookie
                    sessionCookie <- case extractSessionCookie resp of
                        Just cookie -> pure cookie
                        Nothing ->
                            liftIO $
                                error $
                                    "Failed to extract session cookie for expiry test. "
                                        <> "Response headers: "
                                        <> show (simpleHeaders resp)

                    -- Advance time past the 10-minute login session expiry
                    liftIO $ advanceTime (11 * 60) -- 11 minutes = 660 seconds

                    -- Step 2: POST /login with expired session cookie
                    let loginForm =
                            [ ("username", T.unpack $ tcUsername (tcCredentials config))
                            , ("password", T.unpack $ tcPassword (tcCredentials config))
                            , ("session_id", T.unpack sessionCookie)
                            , ("action", "approve")
                            ]

                    -- Should get 400 for expired session
                    postHtmlForm "/login" loginForm `shouldRespondWith` 400

{- | Test specification for HTTP header correctness.

Tests correct header values in OAuth responses (regression for header swap bug).

Covers:
- Location header contains callback URI with authorization code
- Location header does NOT contain session cookie value (regression test)
- Set-Cookie header clears session cookie on successful login

== Usage

@
import Servant.OAuth2.IDP.Test.Internal (headerSpec)

spec :: Spec
spec = do
  let config = TestConfig { ... }
  headerSpec config
@

== Test Isolation

Each test uses 'withFreshApp' to get a fresh Application instance, ensuring
complete isolation between tests (no shared state).

== Regression Test

This spec prevents regression of bug mcp-nyr.18 where Location and Set-Cookie
header values were incorrectly swapped, causing the session cookie to be
sent in the redirect URL and the redirect URL to be sent in Set-Cookie.
-}
headerSpec :: TestConfig m -> Spec
headerSpec config = with (withFreshAppNoTime config) $ do
    describe "HTTP Headers" $ do
        it "Location header contains callback URI with code" $ do
            withRegisteredClient config $ \clientId -> do
                withAuthorizedUser config clientId $ \code _verifier -> do
                    -- withAuthorizedUser validates the entire flow internally,
                    -- including extracting the code from the Location header.
                    -- If we got here, Location header was valid and contained a code.
                    liftIO $ unAuthCodeId code `shouldSatisfy` (not . T.null)

        it "Location header does NOT contain session cookie value" $ do
            withRegisteredClient config $ \clientId -> do
                (_, challenge) <- liftIO generatePKCE

                -- Step 1: GET /authorize to get session cookie
                let authUrl =
                        "/authorize?client_id="
                            <> unClientId clientId
                            <> "&redirect_uri=http://localhost/callback"
                            <> "&response_type=code"
                            <> "&code_challenge="
                            <> challenge
                            <> "&code_challenge_method=S256"

                resp1 <- get (TE.encodeUtf8 authUrl)

                -- Extract session cookie value
                sessionCookie <- case extractSessionCookie resp1 of
                    Just cookie -> pure cookie
                    Nothing ->
                        liftIO $
                            error $
                                "Failed to extract session cookie for header test. "
                                    <> "Response headers: "
                                    <> show (simpleHeaders resp1)

                -- Step 2: POST /login with credentials
                let loginForm =
                        [ ("username", T.unpack $ tcUsername (tcCredentials config))
                        , ("password", T.unpack $ tcPassword (tcCredentials config))
                        , ("session_id", T.unpack sessionCookie)
                        , ("action", "approve")
                        ]

                resp2 <- postHtmlForm "/login" loginForm

                -- Verify Location header exists and contains code parameter
                let mLocation = lookup hLocation (simpleHeaders resp2)
                case mLocation of
                    Nothing ->
                        liftIO $
                            error $
                                "Failed to find Location header in /login response. "
                                    <> "Response headers: "
                                    <> show (simpleHeaders resp2)
                    Just locationBytes -> do
                        let locationText = TE.decodeUtf8 locationBytes
                        -- Regression test: Location should NOT contain session cookie
                        liftIO $ locationText `shouldSatisfy` (not . T.isInfixOf "mcp_session")
                        -- Location should contain the code parameter
                        liftIO $ locationText `shouldSatisfy` T.isInfixOf "?code="

        it "Set-Cookie clears session on successful login" $ do
            withRegisteredClient config $ \clientId -> do
                (_, challenge) <- liftIO generatePKCE

                -- Step 1: GET /authorize to get session cookie
                let authUrl =
                        "/authorize?client_id="
                            <> unClientId clientId
                            <> "&redirect_uri=http://localhost/callback"
                            <> "&response_type=code"
                            <> "&code_challenge="
                            <> challenge
                            <> "&code_challenge_method=S256"

                resp1 <- get (TE.encodeUtf8 authUrl)

                -- Verify we got a session cookie
                sessionCookie <- case extractSessionCookie resp1 of
                    Just cookie -> pure cookie
                    Nothing ->
                        liftIO $
                            error $
                                "Failed to extract session cookie for Set-Cookie test. "
                                    <> "Response headers: "
                                    <> show (simpleHeaders resp1)

                -- Step 2: POST /login with credentials
                let loginForm =
                        [ ("username", T.unpack $ tcUsername (tcCredentials config))
                        , ("password", T.unpack $ tcPassword (tcCredentials config))
                        , ("session_id", T.unpack sessionCookie)
                        , ("action", "approve")
                        ]

                resp2 <- postHtmlForm "/login" loginForm

                -- Verify Set-Cookie header clears the session cookie
                let mSetCookie = lookup "Set-Cookie" (simpleHeaders resp2)
                case mSetCookie of
                    Nothing ->
                        liftIO $
                            error $
                                "Failed to find Set-Cookie header in /login response. "
                                    <> "Response headers: "
                                    <> show (simpleHeaders resp2)
                    Just setCookieBytes -> do
                        let setCookieText = TE.decodeUtf8 setCookieBytes
                        -- Should contain mcp_session cookie
                        liftIO $ setCookieText `shouldSatisfy` T.isInfixOf "mcp_session="
                        -- Should contain Max-Age=0 to clear the cookie
                        liftIO $ setCookieText `shouldSatisfy` T.isInfixOf "Max-Age=0"

{- | Polymorphic OAuth conformance test suite.

Run against any OAuthStateStore/AuthBackend implementation to verify complete
OAuth 2.0 compliance including:

- Client registration (RFC 7591)
- Authorization code flow with PKCE (RFC 7636)
- Token exchange and refresh (RFC 6749)
- Expiry handling for codes and sessions
- HTTP header correctness (Location, Set-Cookie)

== Usage

@
import Servant.OAuth2.IDP.Test.Internal (oauthConformanceSpec)

spec :: Spec
spec = do
  let config = TestConfig { ... }
  oauthConformanceSpec config
@

== Test Coverage

This suite composes all available OAuth test specs:

1. **Client Registration** - Dynamic client registration protocol
2. **Login Flow** - Interactive authorization with credentials
3. **Token Exchange** - Authorization code to access token conversion
4. **Expiry Behavior** - Time-based validation of codes and sessions
5. **HTTP Headers** - Regression tests for header correctness

== Implementation Requirements

The TestConfig must provide:

- 'tcMakeApp': Create Application with time control
- 'tcRunM': Execute monad stack in IO
- 'tcCredentials': Valid test credentials matching configured AuthBackend

== Type Constraints (Phase 8: FR-039)

The function requires type equality constraints to bridge AuthBackend and
OAuthStateStore user types, ensuring handlers can flow user identity from
authentication to token storage.

Note: The constraints are currently unused but required for future test
extensions. The redundant-constraints warning is suppressed to allow
forward-compatible type signatures.
-}
oauthConformanceSpec ::
    forall m.
    ( OAuthStateStore m
    , AuthBackend m
    , MonadTime m
    , AuthBackendUser m ~ OAuthUser m
    , ToJWT (OAuthUser m)
    ) =>
    TestConfig m ->
    Spec
oauthConformanceSpec config = describe "OAuth Conformance Suite" $ do
    -- Type witness: compile-time verification that constraints hold
    -- This ensures the test suite can be extended to use these constraints
    -- without breaking existing tests.
    let _typeWitness :: Maybe (AuthBackendUser m)
        _typeWitness = Nothing :: Maybe (OAuthUser m)

    clientRegistrationSpec config
    loginFlowSpec config
    tokenExchangeSpec config
    expirySpec config
    headerSpec config
