{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Servant.OAuth2.IDP.Handlers.MetadataSpec
Description : Tests for OAuth metadata handler behavior
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC

Tests for metadata handler delegation to OAuthEnv configuration.
-}
module Servant.OAuth2.IDP.Handlers.MetadataSpec (spec) where

import Control.Monad.Reader (runReaderT)
import Data.List.NonEmpty (NonEmpty ((:|)))
import GHC.Generics (Generic)
import Network.URI (URI, parseURI)
import Test.Hspec (Spec, describe, it, shouldBe)

import Servant.OAuth2.IDP.Config (OAuthEnv (..))
import Servant.OAuth2.IDP.Handlers.Metadata (handleMetadata, handleProtectedResourceMetadata)
import Servant.OAuth2.IDP.Metadata (
    mkProtectedResourceMetadata,
    oauthAuthorizationEndpoint,
    oauthCodeChallengeMethodsSupported,
    oauthGrantTypesSupported,
    oauthIssuer,
    oauthResponseTypesSupported,
    oauthScopesSupported,
    oauthTokenEndpoint,
    oauthTokenEndpointAuthMethodsSupported,
    prAuthorizationServers,
    prResource,
 )
import Servant.OAuth2.IDP.Types (
    ClientAuthMethod (..),
    CodeChallengeMethod (..),
    GrantType (..),
    OAuthGrantType (..),
    ResponseType (..),
    Scope,
    mkScope,
 )

-- | Parse a URI from a string, error on invalid (test-only)
unsafeParseURI :: String -> URI
unsafeParseURI s = case parseURI s of
    Just uri -> uri
    Nothing -> error $ "Test URI parse failed: " <> s

-- Helper to get test URI (pattern match safe in test context)
testUri :: URI
testUri = unsafeParseURI "https://example.com"

-- Helper to get test resource server URI
testResourceUri :: URI
testResourceUri = unsafeParseURI "https://resource.example.com"

-- Helper to get test scope (pattern match safe in test context)
testScope :: Scope
testScope = case mkScope "read" of
    Just scope -> scope
    Nothing -> error "Failed to create test scope"

-- Test environment that only includes OAuthEnv
newtype TestEnv = TestEnv
    { testOAuthEnv :: OAuthEnv
    }
    deriving (Generic)

spec :: Spec
spec = do
    describe "handleMetadata" $ do
        it "constructs OAuthMetadata from OAuthEnv without HTTPServerConfig" $ do
            let expectedMetadata = case mkProtectedResourceMetadata
                    (unsafeParseURI "https://resource.example.com")
                    (unsafeParseURI "https://example.com" :| [])
                    Nothing
                    Nothing
                    Nothing
                    Nothing of
                    Just m -> m
                    Nothing -> error "Test fixture: failed to create metadata"

            let oauthEnv =
                    OAuthEnv
                        { oauthRequireHTTPS = True
                        , oauthBaseUrl = testUri
                        , oauthAuthCodeExpiry = 600
                        , oauthAccessTokenExpiry = 3600
                        , oauthLoginSessionExpiry = 600
                        , oauthAuthCodePrefix = "code_"
                        , oauthRefreshTokenPrefix = "rt_"
                        , oauthClientIdPrefix = "client_"
                        , oauthSupportedScopes = [testScope]
                        , oauthSupportedResponseTypes = ResponseCode :| []
                        , oauthSupportedGrantTypes = OAuthAuthorizationCode :| []
                        , oauthSupportedAuthMethods = AuthNone :| []
                        , oauthSupportedCodeChallengeMethods = S256 :| []
                        , resourceServerBaseUrl = testResourceUri
                        , resourceServerMetadata = expectedMetadata
                        , oauthServerName = "OAuth Server"
                        , oauthScopeDescriptions = mempty
                        }

            let env = TestEnv{testOAuthEnv = oauthEnv}

            -- Test that handleMetadata works with ONLY OAuthEnv (no HTTPServerConfig)
            result <- runReaderT handleMetadata env

            -- Verify metadata is constructed from OAuthEnv fields
            oauthIssuer result `shouldBe` "https://example.com"
            oauthAuthorizationEndpoint result `shouldBe` "https://example.com/authorize"
            oauthTokenEndpoint result `shouldBe` "https://example.com/token"
            oauthResponseTypesSupported result `shouldBe` [ResponseCode]
            oauthScopesSupported result `shouldBe` Just [testScope]
            oauthGrantTypesSupported result `shouldBe` Just [GrantAuthorizationCode]
            oauthTokenEndpointAuthMethodsSupported result `shouldBe` Just [AuthNone]
            oauthCodeChallengeMethodsSupported result `shouldBe` Just [S256]

    describe "handleProtectedResourceMetadata" $ do
        it "returns resource server metadata directly from OAuthEnv" $ do
            let expectedMetadata = case mkProtectedResourceMetadata
                    (unsafeParseURI "https://resource.example.com")
                    (unsafeParseURI "https://example.com" :| [])
                    Nothing
                    Nothing
                    Nothing
                    Nothing of
                    Just m -> m
                    Nothing -> error "Test fixture: failed to create metadata"

            let oauthEnv =
                    OAuthEnv
                        { oauthRequireHTTPS = True
                        , oauthBaseUrl = testUri
                        , oauthAuthCodeExpiry = 600
                        , oauthAccessTokenExpiry = 3600
                        , oauthLoginSessionExpiry = 600
                        , oauthAuthCodePrefix = "code_"
                        , oauthRefreshTokenPrefix = "rt_"
                        , oauthClientIdPrefix = "client_"
                        , oauthSupportedScopes = [testScope]
                        , oauthSupportedResponseTypes = ResponseCode :| []
                        , oauthSupportedGrantTypes = OAuthAuthorizationCode :| []
                        , oauthSupportedAuthMethods = AuthNone :| []
                        , oauthSupportedCodeChallengeMethods = S256 :| []
                        , resourceServerBaseUrl = testResourceUri
                        , resourceServerMetadata = expectedMetadata
                        , oauthServerName = "OAuth Server"
                        , oauthScopeDescriptions = mempty
                        }

            let env = TestEnv{testOAuthEnv = oauthEnv}

            -- This should fail until we implement the new fields
            result <- runReaderT handleProtectedResourceMetadata env

            prResource result `shouldBe` unsafeParseURI "https://resource.example.com"
            prAuthorizationServers result `shouldBe` (unsafeParseURI "https://example.com" :| [])
