{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Servant.OAuth2.IDP.BrandingSpec
Description : Tests for configurable branding in HTML rendering
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC
-}
module Servant.OAuth2.IDP.BrandingSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Lucid (renderText)
import Test.Hspec

import Servant.OAuth2.IDP.Handlers.HTML (
    ErrorPage (..),
    LoginPage (..),
    formatScopeDescriptions,
    renderErrorPage,
    renderLoginPage,
    scopeToDescription,
 )
import Servant.OAuth2.IDP.Types (Scope, mkScope)

-- Helper to create Scope for tests (panics on invalid input - ok for tests)
scope :: T.Text -> Scope
scope t = case mkScope t of
    Just s -> s
    Nothing -> error $ "Test fixture: invalid scope " <> T.unpack t

-- | Test suite for configurable branding
spec :: Spec
spec = do
    describe "renderLoginPage with configurable serverName" $ do
        it "uses provided serverName in title" $ do
            let page =
                    LoginPage
                        { loginClientName = "Test Client"
                        , loginScopes = "test:read"
                        , loginResource = Nothing
                        , loginSessionId = "session-123"
                        , loginServerName = "Custom Server"
                        , loginScopeDescriptions = Map.empty
                        }
            let html = TL.toStrict $ renderText (renderLoginPage "Custom Server" page)

            -- Should use custom server name, not hardcoded "MCP Server"
            html `shouldSatisfy` T.isInfixOf "Sign In - Custom Server"
            html `shouldNotSatisfy` T.isInfixOf "MCP Server"

        it "supports MCP Server branding" $ do
            let page =
                    LoginPage
                        { loginClientName = "MCP Client"
                        , loginScopes = "mcp:read"
                        , loginResource = Nothing
                        , loginSessionId = "session-456"
                        , loginServerName = "MCP Server"
                        , loginScopeDescriptions = Map.empty
                        }
            let html = TL.toStrict $ renderText (renderLoginPage "MCP Server" page)

            html `shouldSatisfy` T.isInfixOf "Sign In - MCP Server"

    describe "renderErrorPage with configurable serverName" $ do
        it "uses provided serverName in title" $ do
            let errorPage = ErrorPage "Invalid Request" "Missing client_id" "My OAuth IDP"
            let html = TL.toStrict $ renderText (renderErrorPage "My OAuth IDP" errorPage)

            html `shouldSatisfy` T.isInfixOf "Error - My OAuth IDP"
            html `shouldNotSatisfy` T.isInfixOf "MCP Server"

        it "supports MCP Server branding in error pages" $ do
            let errorPage = ErrorPage "Session Expired" "Your session has expired" "MCP Server"
            let html = TL.toStrict $ renderText (renderErrorPage "MCP Server" errorPage)

            html `shouldSatisfy` T.isInfixOf "Error - MCP Server"

    describe "scopeToDescription with Map lookup" $ do
        it "looks up scope description from provided map" $ do
            let scopeMap =
                    Map.fromList
                        [ (scope "custom:read", "Read custom data")
                        , (scope "custom:write", "Modify custom data")
                        ]
            scopeToDescription scopeMap (scope "custom:read") `shouldBe` "Read custom data"
            scopeToDescription scopeMap (scope "custom:write") `shouldBe` "Modify custom data"

        it "falls back to generic description for unknown scopes" $ do
            let scopeMap = Map.fromList [(scope "known:scope", "Known scope description")]
            scopeToDescription scopeMap (scope "unknown:scope") `shouldBe` "Access unknown:scope"

        it "supports MCP-specific descriptions when configured" $ do
            let mcpScopeMap =
                    Map.fromList
                        [ (scope "mcp:read", "Read MCP resources")
                        , (scope "mcp:write", "Write MCP resources")
                        , (scope "mcp:tools", "Execute MCP tools")
                        ]
            scopeToDescription mcpScopeMap (scope "mcp:read") `shouldBe` "Read MCP resources"
            scopeToDescription mcpScopeMap (scope "mcp:write") `shouldBe` "Write MCP resources"
            scopeToDescription mcpScopeMap (scope "mcp:tools") `shouldBe` "Execute MCP tools"

        it "generates generic description when map is empty" $ do
            let emptyMap = Map.empty
            scopeToDescription emptyMap (scope "any:scope") `shouldBe` "Access any:scope"

    describe "formatScopeDescriptions integration" $ do
        it "formats multiple scopes using custom descriptions" $ do
            let scopeMap =
                    Map.fromList
                        [ (scope "api:read", "View API data")
                        , (scope "api:write", "Modify API data")
                        ]
            formatScopeDescriptions scopeMap "api:read api:write"
                `shouldBe` "View API data, Modify API data"

        it "handles mix of known and unknown scopes" $ do
            let scopeMap = Map.fromList [(scope "known:scope", "Known description")]
            formatScopeDescriptions scopeMap "known:scope unknown:scope"
                `shouldBe` "Known description, Access unknown:scope"
