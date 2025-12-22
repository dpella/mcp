{-# LANGUAGE OverloadedStrings #-}

module Servant.OAuth2.IDP.ConfigSpec (spec) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map.Strict qualified as Map
import Network.URI (URI, parseURI)
import Test.Hspec (Spec, describe, it, shouldBe)

import Servant.OAuth2.IDP.Config (OAuthEnv (..))
import Servant.OAuth2.IDP.Metadata (ProtectedResourceMetadata, mkProtectedResourceMetadata)
import Servant.OAuth2.IDP.Types (
    ClientAuthMethod (..),
    CodeChallengeMethod (..),
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

-- Helper to get test scope (pattern match safe in test context)
testScope :: Scope
testScope = case mkScope "read" of
    Just scope -> scope
    Nothing -> error "Failed to create test scope"

-- Helper for test resource metadata
testResourceMetadata :: ProtectedResourceMetadata
testResourceMetadata = case mkProtectedResourceMetadata
    (unsafeParseURI "https://example.com")
    (unsafeParseURI "https://example.com" :| [])
    Nothing
    Nothing
    Nothing
    Nothing of
    Just m -> m
    Nothing -> error "Failed to create test metadata"

-- Helper to create minimal OAuthEnv for testing
mkTestOAuthEnv :: OAuthEnv
mkTestOAuthEnv =
    OAuthEnv
        { oauthRequireHTTPS = True
        , oauthBaseUrl = testUri
        , oauthAuthCodeExpiry = 600
        , oauthAccessTokenExpiry = 3600
        , oauthLoginSessionExpiry = 600
        , oauthAuthCodePrefix = "code_"
        , oauthRefreshTokenPrefix = "rt_"
        , oauthClientIdPrefix = "client_"
        , oauthSupportedScopes = []
        , oauthSupportedResponseTypes = ResponseCode :| []
        , oauthSupportedGrantTypes = OAuthAuthorizationCode :| []
        , oauthSupportedAuthMethods = AuthNone :| []
        , oauthSupportedCodeChallengeMethods = S256 :| []
        , resourceServerBaseUrl = testUri
        , resourceServerMetadata = testResourceMetadata
        , oauthServerName = "OAuth Server"
        , oauthScopeDescriptions = Map.empty
        }

spec :: Spec
spec = do
    describe "OAuthEnv" $ do
        it "constructs valid configuration with all required fields" $ do
            let
                env =
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
                        , resourceServerBaseUrl = testUri
                        , resourceServerMetadata = testResourceMetadata
                        , oauthServerName = "OAuth Server"
                        , oauthScopeDescriptions = Map.empty
                        }

            oauthRequireHTTPS env `shouldBe` True
            oauthBaseUrl env `shouldBe` testUri
            oauthAuthCodeExpiry env `shouldBe` 600
            oauthAccessTokenExpiry env `shouldBe` 3600
            oauthLoginSessionExpiry env `shouldBe` 600
            oauthAuthCodePrefix env `shouldBe` "code_"
            oauthRefreshTokenPrefix env `shouldBe` "rt_"
            oauthClientIdPrefix env `shouldBe` "client_"
            length (oauthSupportedScopes env) `shouldBe` 1

        it "allows empty scope list (no required scopes)" $ do
            let env = mkTestOAuthEnv

            oauthSupportedScopes env `shouldBe` []

        it "supports multiple response types" $ do
            let env =
                    mkTestOAuthEnv
                        { oauthRequireHTTPS = False
                        , oauthSupportedResponseTypes = ResponseCode :| [ResponseToken]
                        }

            length (oauthSupportedResponseTypes env) `shouldBe` 2

        it "supports multiple grant types" $ do
            let env =
                    mkTestOAuthEnv
                        { oauthSupportedGrantTypes = OAuthAuthorizationCode :| [OAuthClientCredentials]
                        }

            length (oauthSupportedGrantTypes env) `shouldBe` 2

        it "supports multiple auth methods" $ do
            let env =
                    mkTestOAuthEnv
                        { oauthSupportedAuthMethods = AuthNone :| [AuthClientSecretPost, AuthClientSecretBasic]
                        }

            length (oauthSupportedAuthMethods env) `shouldBe` 3

        it "supports multiple code challenge methods" $ do
            let env =
                    mkTestOAuthEnv
                        { oauthSupportedCodeChallengeMethods = S256 :| [Plain]
                        }

            length (oauthSupportedCodeChallengeMethods env) `shouldBe` 2

        it "stores custom server name for branding" $ do
            let env =
                    OAuthEnv
                        { oauthRequireHTTPS = True
                        , oauthBaseUrl = testUri
                        , oauthAuthCodeExpiry = 600
                        , oauthAccessTokenExpiry = 3600
                        , oauthLoginSessionExpiry = 600
                        , oauthAuthCodePrefix = "code_"
                        , oauthRefreshTokenPrefix = "rt_"
                        , oauthClientIdPrefix = "client_"
                        , oauthSupportedScopes = []
                        , oauthSupportedResponseTypes = ResponseCode :| []
                        , oauthSupportedGrantTypes = OAuthAuthorizationCode :| []
                        , oauthSupportedAuthMethods = AuthNone :| []
                        , oauthSupportedCodeChallengeMethods = S256 :| []
                        , resourceServerBaseUrl = testUri
                        , resourceServerMetadata = testResourceMetadata
                        , oauthServerName = "My Custom OAuth Server"
                        , oauthScopeDescriptions = Map.empty
                        }

            oauthServerName env `shouldBe` "My Custom OAuth Server"

        it "stores scope descriptions for consent page" $ do
            let readScope = testScope
                writeScope = case mkScope "write" of
                    Just s -> s
                    Nothing -> error "Failed to create write scope"
                descriptions =
                    Map.fromList
                        [ (readScope, "Read access to your data")
                        , (writeScope, "Write access to your data")
                        ]
                env =
                    OAuthEnv
                        { oauthRequireHTTPS = True
                        , oauthBaseUrl = testUri
                        , oauthAuthCodeExpiry = 600
                        , oauthAccessTokenExpiry = 3600
                        , oauthLoginSessionExpiry = 600
                        , oauthAuthCodePrefix = "code_"
                        , oauthRefreshTokenPrefix = "rt_"
                        , oauthClientIdPrefix = "client_"
                        , oauthSupportedScopes = [readScope, writeScope]
                        , oauthSupportedResponseTypes = ResponseCode :| []
                        , oauthSupportedGrantTypes = OAuthAuthorizationCode :| []
                        , oauthSupportedAuthMethods = AuthNone :| []
                        , oauthSupportedCodeChallengeMethods = S256 :| []
                        , resourceServerBaseUrl = testUri
                        , resourceServerMetadata = testResourceMetadata
                        , oauthServerName = "OAuth Server"
                        , oauthScopeDescriptions = descriptions
                        }

            Map.lookup readScope (oauthScopeDescriptions env) `shouldBe` Just "Read access to your data"
            Map.lookup writeScope (oauthScopeDescriptions env) `shouldBe` Just "Write access to your data"
            Map.size (oauthScopeDescriptions env) `shouldBe` 2
