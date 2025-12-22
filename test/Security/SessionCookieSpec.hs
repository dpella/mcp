{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
Module      : Security.SessionCookieSpec
Description : Tests for session cookie Secure flag behavior
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT

Tests that session cookies include the Secure flag when requireHTTPS is enabled,
and do not include it when requireHTTPS is disabled.
-}
module Security.SessionCookieSpec (spec) where

import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Network.Wai.Test (simpleHeaders)
import Test.Hspec (Spec, describe, expectationFailure, it, runIO, shouldSatisfy)
import Test.Hspec.Wai (get, liftIO, with)

import Control.Concurrent.STM (newTVarIO)
import MCP.Server (MCPServer (..), MCPServerM, initialServerState)
import MCP.Server.HTTP (DemoOAuthBundle (..), defaultDemoOAuthBundle, mcpAppWithOAuth)
import MCP.Server.HTTP.AppEnv (AppEnv (..), HTTPServerConfig (..), runAppM)
import MCP.Server.OAuth.Test.Fixtures (defaultTestTime, mkTestEnv)
import Servant.Auth.Server (defaultJWTSettings, generateKey)
import Servant.OAuth2.IDP.Config (OAuthEnv (..))
import Servant.OAuth2.IDP.Test.Internal (TestConfig (..), TestCredentials (..), generatePKCE, withRegisteredClient)
import Servant.OAuth2.IDP.Types (unClientId)

-- | Minimal MCPServer instance for testing (uses default implementations)
instance MCPServer MCPServerM

{- | Helper to create a test config with custom requireHTTPS setting.

Creates a TestConfig with a modified OAuth configuration, useful for testing
specific OAuth settings like requireHTTPS.
-}
mkTestConfigWith :: Bool -> IO (TestConfig m)
mkTestConfigWith requireHTTPS = do
    -- Create fresh time TVar for this test
    timeTVar <- newTVarIO defaultTestTime

    -- Create base environment
    baseEnv <- mkTestEnv timeTVar

    -- Get default bundle and update requireHTTPS
    let bundle = defaultDemoOAuthBundle
        oauthEnv = (bundleEnv bundle){oauthRequireHTTPS = requireHTTPS}

    -- Update the OAuth config in the environment
    let config = (envConfig baseEnv){httpMCPOAuthConfig = Just (bundleMCPConfig bundle)}
        env = baseEnv{envConfig = config, envOAuthEnv = oauthEnv}

    -- Create fresh server state
    stateVar <- newTVarIO $ initialServerState (httpCapabilities config)
    let envWithState = env{envServerState = stateVar}

    -- Generate JWK if not present
    jwtSettings <- case httpJWK config of
        Just jwk -> return $ defaultJWTSettings jwk
        Nothing -> defaultJWTSettings <$> generateKey

    let finalEnv = envWithState{envJWT = jwtSettings}

    -- Build WAI Application
    let app = mcpAppWithOAuth (runAppM finalEnv) jwtSettings

    return
        TestConfig
            { tcMakeApp = return (app, \_ -> pure ())
            , tcRunM = error "tcRunM not needed for cookie tests"
            , tcCredentials = TestCredentials "demo" "demo123"
            }

{- | Test specification for session cookie Secure flag.

Tests that the session cookie Set-Cookie header includes the Secure flag
when requireHTTPS is True, and excludes it when requireHTTPS is False.

This is a security feature to prevent session hijacking over unencrypted connections.
-}
spec :: Spec
spec = describe "Session cookie Secure flag" $ do
    describe "when requireHTTPS is True" $ do
        config <- runIO $ mkTestConfigWith True
        app <- runIO $ fst <$> tcMakeApp config

        with (return app) $ do
            it "includes Secure flag in session cookie" $ do
                withRegisteredClient config $ \clientId -> do
                    -- Generate valid PKCE challenge
                    (_, challenge) <- liftIO generatePKCE

                    -- GET /authorize to trigger session cookie
                    let authUrl =
                            "/authorize?client_id="
                                <> unClientId clientId
                                <> "&redirect_uri=http://localhost/callback"
                                <> "&response_type=code"
                                <> "&code_challenge="
                                <> challenge
                                <> "&code_challenge_method=S256"

                    resp <- get (TE.encodeUtf8 authUrl)

                    -- Extract Set-Cookie header
                    let mSetCookie = lookup "Set-Cookie" (simpleHeaders resp)
                    case mSetCookie of
                        Nothing ->
                            liftIO $
                                expectationFailure "No Set-Cookie header found in /authorize response"
                        Just setCookieValue ->
                            case TE.decodeUtf8' setCookieValue of
                                Left err ->
                                    liftIO $
                                        expectationFailure $
                                            "Failed to decode Set-Cookie header as UTF-8: " <> show err
                                Right cookieText ->
                                    -- Verify that the cookie contains the Secure flag
                                    liftIO $ cookieText `shouldSatisfy` T.isInfixOf "; Secure"

    describe "when requireHTTPS is False" $ do
        config <- runIO $ mkTestConfigWith False
        app <- runIO $ fst <$> tcMakeApp config

        with (return app) $ do
            it "does NOT include Secure flag in session cookie" $ do
                withRegisteredClient config $ \clientId -> do
                    -- Generate valid PKCE challenge
                    (_, challenge) <- liftIO generatePKCE

                    -- GET /authorize to trigger session cookie
                    let authUrl =
                            "/authorize?client_id="
                                <> unClientId clientId
                                <> "&redirect_uri=http://localhost/callback"
                                <> "&response_type=code"
                                <> "&code_challenge="
                                <> challenge
                                <> "&code_challenge_method=S256"

                    resp <- get (TE.encodeUtf8 authUrl)

                    -- Extract Set-Cookie header
                    let mSetCookie = lookup "Set-Cookie" (simpleHeaders resp)
                    case mSetCookie of
                        Nothing ->
                            liftIO $
                                expectationFailure "No Set-Cookie header found in /authorize response"
                        Just setCookieValue ->
                            case TE.decodeUtf8' setCookieValue of
                                Left err ->
                                    liftIO $
                                        expectationFailure $
                                            "Failed to decode Set-Cookie header as UTF-8: " <> show err
                                Right cookieText ->
                                    -- Verify that the cookie does NOT contain the Secure flag
                                    liftIO $ cookieText `shouldSatisfy` not . T.isInfixOf "; Secure"
