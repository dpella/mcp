{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : MCP.Server.OAuth.Test.Fixtures
Description : Test fixtures for OAuth HTTP-level testing
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC

This module provides test fixtures for OAuth conformance testing using hspec-wai.

= Usage

@
import Test.Hspec
import MCP.Server.OAuth.Test.Fixtures

spec :: Spec
spec = do
  config <- runIO referenceTestConfig
  oauthConformanceSpec config
@

= Design

The fixtures wire up the reference TVar-based implementation with time advancement
capability for expiry testing. This provides:

* Fresh OAuth state per test
* Demo credentials (demo/demo123)
* Controllable time via TVar manipulation
-}
module MCP.Server.OAuth.Test.Fixtures (
    -- * Test Configuration
    referenceTestConfig,
    defaultTestTime,
    mkTestEnv,
) where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask)
import Data.Time.Clock (UTCTime, addUTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeOrError)
import Plow.Logging (IOTracer (..), Tracer (..))
import Servant.Auth.Server (defaultJWTSettings, generateKey)
import Servant.Server.Internal.Handler (runHandler)

import MCP.Server (MCPServer (..), MCPServerM, initialServerState)
import MCP.Server.HTTP (HTTPServerConfig (..), defaultDemoOAuthConfig, defaultProtectedResourceMetadata, mcpAppWithOAuth)
import MCP.Server.HTTP.AppEnv (AppEnv (..), AppM, runAppM)
import MCP.Server.Time (MonadTime (..))
import MCP.Types (Implementation (..), ServerCapabilities (..), ToolsCapability (..))
import Servant.OAuth2.IDP.Auth.Demo (DemoCredentialEnv (..), defaultDemoCredentialStore)
import Servant.OAuth2.IDP.Store.InMemory (defaultExpiryConfig, newOAuthTVarEnv)
import Servant.OAuth2.IDP.Test.Internal (TestConfig (..), TestCredentials (..))

-- -----------------------------------------------------------------------------
-- Default Test Values
-- -----------------------------------------------------------------------------

{- | Default test time for deterministic testing.

Set to 2020-01-01 00:00:00 UTC to match the law test expectations.
-}
defaultTestTime :: UTCTime
defaultTestTime = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M:%S" "2020-01-01 00:00:00"

-- -----------------------------------------------------------------------------
-- Test Environment Construction
-- -----------------------------------------------------------------------------

{- | Create a test environment with controllable time.

The returned AppEnv uses:

* Fresh TVar-based OAuth state (empty)
* Demo credentials (demo/demo123, admin/admin456)
* Minimal HTTP server config
* Null tracer (no output during tests)
* Time from the provided TVar (controllable)

The TVar enables time manipulation for expiry testing via the
advanceTime callback returned by 'referenceTestConfig'.
-}
mkTestEnv :: TVar UTCTime -> IO AppEnv
mkTestEnv timeTVar = do
    -- Create fresh OAuth state with default expiry config
    oauthEnv <- newOAuthTVarEnv defaultExpiryConfig

    -- Demo credentials environment
    let demoEnv = DemoCredentialEnv defaultDemoCredentialStore

    -- Minimal HTTP server config
    let serverConfig =
            HTTPServerConfig
                { httpPort = 8080 -- Not actually used in tests
                , httpBaseUrl = "http://localhost:8080"
                , httpServerInfo = Implementation "test-server" (Just "1.0.0") ""
                , httpCapabilities =
                    ServerCapabilities
                        { resources = Nothing
                        , prompts = Nothing
                        , tools = Just (ToolsCapability Nothing)
                        , completions = Nothing
                        , logging = Nothing
                        , experimental = Nothing
                        }
                , httpEnableLogging = False
                , httpOAuthConfig = Just defaultDemoOAuthConfig
                , httpJWK = Nothing -- Will be generated
                , httpProtocolVersion = "2025-06-18"
                , httpProtectedResourceMetadata = Just (defaultProtectedResourceMetadata "http://localhost:8080")
                }

    -- Null tracer for tests (discards all traces)
    let tracer = IOTracer (Tracer (\_ -> pure ()))

    -- Create JWT settings for tests
    jwtSettings <- case httpJWK serverConfig of
        Just jwk -> return $ defaultJWTSettings jwk
        Nothing -> defaultJWTSettings <$> generateKey

    -- Create placeholder server state (will be replaced in referenceTestConfig)
    placeholderStateVar <- newTVarIO $ initialServerState (httpCapabilities serverConfig)

    return
        AppEnv
            { envOAuth = oauthEnv
            , envAuth = demoEnv
            , envConfig = serverConfig
            , envTracer = tracer
            , envJWT = jwtSettings
            , envServerState = placeholderStateVar
            , envTimeProvider = Just timeTVar -- Use controllable time for tests
            }

-- -----------------------------------------------------------------------------
-- MonadTime Instance for TVar-Based Testing
-- -----------------------------------------------------------------------------

{- | TestTimeM monad that reads time from a TVar.

This allows tests to control time by modifying the TVar, enabling
deterministic testing of time-dependent behavior (expiry, etc.).
-}

{- | NOTE: TestTimeM is currently unused but demonstrates how to wire up
controllable time for the AppM monad. Once HTTP.hs is fully migrated to
AppM, this pattern can replace the TVar-based time control.
-}
newtype TestTimeM a = TestTimeM (ReaderT (TVar UTCTime) IO a)
    deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader (TVar UTCTime))

instance MonadTime TestTimeM where
    currentTime = TestTimeM $ do
        tvar <- ask
        liftIO $ readTVarIO tvar

    monotonicTime = liftIO monotonicTime -- Delegate to IO for monotonic time

-- -----------------------------------------------------------------------------
-- Reference Test Configuration
-- -----------------------------------------------------------------------------

{- | Reference test configuration for the TVar-based implementation.

This wires up the reference in-memory OAuth implementation with time
advancement capability. Use this configuration to run the conformance
suite against the reference implementation.

== Usage

@
import Test.Hspec
import MCP.Server.OAuth.Test.Fixtures
import Servant.OAuth2.IDP.Test.Internal (oauthConformanceSpec)

spec :: Spec
spec = do
  config <- runIO referenceTestConfig
  describe "Reference OAuth Implementation" $
    oauthConformanceSpec config
@

== Implementation Notes

* Creates fresh OAuth state for each test via 'tcMakeApp'
* Uses demo credentials (demo/demo123)
* Provides time advancement via TVar manipulation
* Uses polymorphic mcpApp entry point from MCP.Server.OAuth.App
-}
referenceTestConfig :: (MCPServer MCPServerM) => IO (TestConfig AppM)
referenceTestConfig = do
    let makeApp = do
            -- Create fresh time TVar for this test
            timeTVar <- newTVarIO defaultTestTime

            -- Create fresh environment
            env <- mkTestEnv timeTVar

            -- Create fresh server state
            let config = envConfig env
            stateVar <- newTVarIO $ initialServerState (httpCapabilities config)

            -- Add stateVar to env
            let envWithState = env{envServerState = stateVar}
            let jwtSettings = envJWT env

            -- Build actual WAI Application using mcpAppWithOAuth polymorphic entry point
            let app = mcpAppWithOAuth (runAppM envWithState) jwtSettings

            -- Time advancement callback - modifies the TVar that controls currentTime
            let advanceTime dt = atomically $ modifyTVar' timeTVar (addUTCTime dt)

            return (app, advanceTime)

    return
        TestConfig
            { tcMakeApp = makeApp
            , tcRunM = \action -> do
                -- Create a throwaway environment for law tests
                timeTVar <- newTVarIO defaultTestTime
                env <- mkTestEnv timeTVar
                -- Run AppM action via runAppM (returns Handler a)
                -- Then unwrap Handler using runHandler :: Handler a -> IO (Either ServerError a)
                result <- runHandler $ runAppM env action
                case result of
                    Left err -> fail ("tcRunM: Handler failed with ServerError: " <> show err)
                    Right a -> pure a
            , tcCredentials = TestCredentials "demo" "demo123"
            }
