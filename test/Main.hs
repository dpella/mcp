{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Main
Description : Test suite entry point
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC
-}
module Main (main) where

import Data.ByteArray qualified as BA
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)

-- Auth typeclass modules
import Servant.OAuth2.IDP.Auth.Backend (Salt (..), mkHashedPassword, mkPlaintextPassword, mkUsername)

import Test.Hspec

-- Test monad with controlled time
import TestMonad (TestM, addTestCredential, mkTestEnv, runTestM)

-- Law tests
import Laws.AuthBackendAssociatedTypesSpec qualified as AuthBackendAssociatedTypesSpec
import Laws.AuthBackendSignatureSpec qualified as AuthBackendSignatureSpec
import Laws.AuthBackendSpec (authBackendKnownCredentials, authBackendLaws)
import Laws.AuthCodeFunctorSpec qualified as AuthCodeFunctorSpec
import Laws.BoundarySpec qualified as BoundarySpec
import Laws.ConsumeAuthCodeSpec (consumeAuthCodeConcurrencySpec, consumeAuthCodeSpec)
import Laws.ErrorBoundarySecuritySpec qualified as ErrorBoundarySecuritySpec
import Laws.OAuthStateStoreSpec (oauthStateStoreLaws)
import Laws.OAuthUserTypeSpec qualified as OAuthUserTypeSpec

-- Existing specs
import Trace.FilterSpec qualified as FilterSpec
import Trace.GoldenSpec qualified as GoldenSpec
import Trace.RenderSpec qualified as RenderSpec

-- Unit tests
import MCP.Server.OAuth.TypesSpec qualified as TypesSpec

-- OAuth App tests
import MCP.Server.OAuth.AppSpec qualified as AppSpec

-- HTTP endpoint tests
import MCP.Server.HTTP.AppEnvSpec qualified as AppEnvSpec
import MCP.Server.HTTP.McpAuthSpec qualified as McpAuthSpec

-- MCP.Server.Auth tests
import MCP.Server.AuthSpec qualified as AuthSpec

-- Functional tests
import Functional.OAuthFlowSpec qualified as OAuthFlowSpec

-- Security tests
import Security.SessionCookieSpec qualified as SessionCookieSpec

-- Servant OAuth2 IDP tests

import Servant.OAuth2.IDP.APISpec qualified as APISpec
import Servant.OAuth2.IDP.BrandingSpec qualified as BrandingSpec
import Servant.OAuth2.IDP.ConfigSpec qualified as ConfigSpec
import Servant.OAuth2.IDP.CryptoEntropySpec qualified as CryptoEntropySpec
import Servant.OAuth2.IDP.ErrorsSpec qualified as ErrorsSpec
import Servant.OAuth2.IDP.Handlers.MetadataSpec qualified as HandlersMetadataSpec
import Servant.OAuth2.IDP.LucidRenderingSpec qualified as LucidRenderingSpec
import Servant.OAuth2.IDP.MetadataSpec qualified as MetadataSpec
import Servant.OAuth2.IDP.PKCESpec qualified as PKCESpec
import Servant.OAuth2.IDP.TokenRequestSpec qualified as TokenRequestSpec
import Servant.OAuth2.IDP.TraceSpec qualified as TraceSpec
import Servant.OAuth2.IDP.TypesSpec qualified as IDPTypesSpec

main :: IO ()
main = hspec spec

{- | Fixed base time for all tests (year 2020, start of the test data range)
The Arbitrary instance for UTCTime generates times within 10 years from 2020-01-01,
so times range from 2020 to 2030. We set currentTime to 2020-01-01 so that ALL
generated timestamps appear to be in the future. The law tests then manipulate
timestamps relative to the generated values (e.g., subtract 1 day to make expired).
With currentTime at the minimum and expiry durations measured in hours, this ensures:
- Round-trip tests (subtract 60s) remain valid (not expired)
- Expiry tests (subtract 1 day) become expired
-}
baseTestTime :: UTCTime
baseTestTime = case parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z" "2020-01-01 00:00:00 UTC" of
    Just t -> t
    Nothing -> error "Failed to parse base test time"

-- | Runner for TestM monad (provides both OAuthStateStore and AuthBackend)
runTestM' :: TestM a -> IO a
runTestM' action = do
    env <- mkTestEnv baseTestTime Map.empty
    runTestM env action

-- | Runner for TestM with demo credentials pre-loaded
runTestMWithDemoCreds :: TestM a -> IO a
runTestMWithDemoCreds action = do
    env <- mkTestEnv baseTestTime Map.empty
    runTestM env $ do
        -- Add demo credentials
        let demoUser = case mkUsername "demo" of
                Just u -> u
                Nothing -> error "Test fixture: invalid username 'demo'"
        let adminUser = case mkUsername "admin" of
                Just u -> u
                Nothing -> error "Test fixture: invalid username 'admin'"
        addTestCredential demoUser (mkPlaintextPassword "demo123")
        addTestCredential adminUser (mkPlaintextPassword "admin456")
        action

spec :: Spec
spec = do
    FilterSpec.spec
    GoldenSpec.spec
    RenderSpec.spec

    -- Unit tests
    TypesSpec.spec

    -- OAuth App tests
    AppSpec.spec

    -- HTTP endpoint tests
    AppEnvSpec.spec
    McpAuthSpec.spec

    -- MCP.Server.Auth tests
    AuthSpec.spec

    -- Boundary layer tests (Servant FromHttpApiData/ToHttpApiData)
    BoundarySpec.spec

    -- Error boundary security tests
    ErrorBoundarySecuritySpec.spec

    -- Functional tests
    describe "Functional" OAuthFlowSpec.spec

    -- Security tests
    describe "Security" SessionCookieSpec.spec

    -- Servant OAuth2 IDP tests
    describe "Servant.OAuth2.IDP" $ do
        APISpec.spec
        BrandingSpec.spec
        ConfigSpec.spec
        TokenRequestSpec.spec
        LucidRenderingSpec.spec
        IDPTypesSpec.spec
        CryptoEntropySpec.spec
        ErrorsSpec.spec
        HandlersMetadataSpec.spec
        MetadataSpec.spec
        PKCESpec.spec
        TraceSpec.spec

    -- Typeclass law tests (using TestM with controlled time)
    describe "TestM OAuthStateStore" $ do
        oauthStateStoreLaws runTestM'
        consumeAuthCodeSpec runTestM'
        consumeAuthCodeConcurrencySpec
        OAuthUserTypeSpec.spec

    -- Functor laws
    AuthCodeFunctorSpec.spec

    describe "TestM AuthBackend" $ do
        AuthBackendAssociatedTypesSpec.spec
        AuthBackendSignatureSpec.spec
        authBackendLaws runTestMWithDemoCreds
        let demoUser = case mkUsername "demo" of
                Just u -> u
                Nothing -> error "Test fixture: invalid username 'demo'"
        authBackendKnownCredentials
            runTestMWithDemoCreds
            demoUser
            (mkPlaintextPassword "demo123")
            (mkPlaintextPassword "wrongpassword")

    -- Hash function tests
    describe "Servant.OAuth2.IDP.Auth.Backend" $ do
        describe "mkHashedPassword" $ do
            it "produces consistent hashes for same inputs" $ do
                let saltBytes = BA.convert (TE.encodeUtf8 ("test-salt" :: Text)) :: BA.ScrubbedBytes
                    salt = Salt saltBytes
                    password = mkPlaintextPassword "test-password"
                    hash1 = mkHashedPassword salt password
                    hash2 = mkHashedPassword salt password
                (hash1 == hash2) `shouldBe` True

            it "produces different hashes for different passwords" $ do
                let saltBytes = BA.convert (TE.encodeUtf8 ("test-salt" :: Text)) :: BA.ScrubbedBytes
                    salt = Salt saltBytes
                    hash1 = mkHashedPassword salt (mkPlaintextPassword "test-password")
                    hash2 = mkHashedPassword salt (mkPlaintextPassword "different-password")
                (hash1 /= hash2) `shouldBe` True

            it "produces different hashes for different salts" $ do
                let password = mkPlaintextPassword "test-password"
                    saltBytes1 = BA.convert (TE.encodeUtf8 ("test-salt" :: Text)) :: BA.ScrubbedBytes
                    saltBytes2 = BA.convert (TE.encodeUtf8 ("different-salt" :: Text)) :: BA.ScrubbedBytes
                    hash1 = mkHashedPassword (Salt saltBytes1) password
                    hash2 = mkHashedPassword (Salt saltBytes2) password
                (hash1 /= hash2) `shouldBe` True
