{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : MCP.Server.AuthSpec
Description : Tests for MCP.Server.Auth module
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC

Tests for MCP-specific OAuth configuration types.
-}
module MCP.Server.AuthSpec (spec) where

import Data.Text (Text)
import Test.Hspec

import MCP.Server.Auth

spec :: Spec
spec = do
    describe "MCPOAuthConfig" $ do
        describe "record construction with unprefixed fields" $ do
            it "creates config with autoApproveAuth disabled (unprefixed)" $ do
                let config =
                        MCPOAuthConfig
                            { autoApproveAuth = False
                            , oauthProviders = []
                            , demoUserIdTemplate = Just "user-{id}"
                            , demoEmailDomain = "example.com"
                            , demoUserName = "Demo User"
                            , publicClientSecret = Nothing
                            , authorizationSuccessTemplate = "<html>Success</html>"
                            }
                autoApproveAuth config `shouldBe` False

            it "creates config with demo user ID template (unprefixed)" $ do
                let template = Just "demo-user-{id}" :: Maybe Text
                    config =
                        MCPOAuthConfig
                            { autoApproveAuth = True
                            , oauthProviders = []
                            , demoUserIdTemplate = template
                            , demoEmailDomain = "test.org"
                            , demoUserName = "Test User"
                            , publicClientSecret = Nothing
                            , authorizationSuccessTemplate = ""
                            }
                demoUserIdTemplate config `shouldBe` template

            it "creates config with demo email domain (unprefixed)" $ do
                let domain = "demo.example.com" :: Text
                    config =
                        MCPOAuthConfig
                            { autoApproveAuth = True
                            , oauthProviders = []
                            , demoUserIdTemplate = Just "user-{id}"
                            , demoEmailDomain = domain
                            , demoUserName = "Demo User"
                            , publicClientSecret = Nothing
                            , authorizationSuccessTemplate = ""
                            }
                demoEmailDomain config `shouldBe` domain

            it "creates config with authorization success template (unprefixed)" $ do
                let template = "<html><body>Authorization successful!</body></html>" :: Text
                    config =
                        MCPOAuthConfig
                            { autoApproveAuth = False
                            , oauthProviders = []
                            , demoUserIdTemplate = Just "user-{id}"
                            , demoEmailDomain = "example.com"
                            , demoUserName = "Demo User"
                            , publicClientSecret = Nothing
                            , authorizationSuccessTemplate = template
                            }
                authorizationSuccessTemplate config `shouldBe` template

            it "creates config with oauth providers list" $ do
                let config =
                        MCPOAuthConfig
                            { autoApproveAuth = False
                            , oauthProviders = [] -- Empty list for simplicity in tests
                            , demoUserIdTemplate = Just "user-{id}"
                            , demoEmailDomain = "example.com"
                            , demoUserName = "Demo User"
                            , publicClientSecret = Nothing
                            , authorizationSuccessTemplate = ""
                            }
                -- Just verify the field exists and can be accessed
                oauthProviders config `shouldBe` []

            it "creates config with demo user name" $ do
                let name = "Custom Demo User" :: Text
                    config =
                        MCPOAuthConfig
                            { autoApproveAuth = True
                            , oauthProviders = []
                            , demoUserIdTemplate = Just "user-{id}"
                            , demoEmailDomain = "example.com"
                            , demoUserName = name
                            , publicClientSecret = Nothing
                            , authorizationSuccessTemplate = ""
                            }
                demoUserName config `shouldBe` name

            it "creates config with public client secret" $ do
                let secret = Just "" :: Maybe Text
                    config =
                        MCPOAuthConfig
                            { autoApproveAuth = True
                            , oauthProviders = []
                            , demoUserIdTemplate = Just "user-{id}"
                            , demoEmailDomain = "example.com"
                            , demoUserName = "Demo User"
                            , publicClientSecret = secret
                            , authorizationSuccessTemplate = ""
                            }
                publicClientSecret config `shouldBe` secret
