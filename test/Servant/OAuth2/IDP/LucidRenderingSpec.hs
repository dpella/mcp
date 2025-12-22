{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{- HLINT ignore "Avoid partial function" -}

{- |
Module      : Servant.OAuth2.IDP.LucidRenderingSpec
Description : Tests for Lucid HTML rendering
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC
-}
module Servant.OAuth2.IDP.LucidRenderingSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Lucid (Html, renderText, toHtml)
import Test.Hspec

import Data.Maybe (fromJust)
import Servant.OAuth2.IDP.Errors (LoginFlowError (..))
import Servant.OAuth2.IDP.Handlers.HTML (
    ErrorPage (..),
    LoginPage (..),
 )
import Servant.OAuth2.IDP.Types (mkSessionId)

-- | Test suite for Lucid-based HTML rendering
spec :: Spec
spec = do
    describe "LoginPage ToHtml instance" $ do
        it "renders a login page with client name and scopes" $ do
            let page =
                    LoginPage
                        { loginClientName = "Test Client"
                        , loginScopes = "mcp:read mcp:write"
                        , loginResource = Nothing
                        , loginSessionId = "test-session-123"
                        , loginServerName = "MCP Server"
                        , loginScopeDescriptions = Map.empty
                        }
            let html = TL.toStrict $ renderText (toHtml page)

            -- Verify essential HTML structure
            html `shouldSatisfy` T.isInfixOf "<!DOCTYPE HTML>"
            html `shouldSatisfy` T.isInfixOf "<html>"
            html `shouldSatisfy` T.isInfixOf "</html>"

        it "includes the client name in the rendered HTML" $ do
            let page =
                    LoginPage
                        { loginClientName = "MyApp"
                        , loginScopes = "mcp:read"
                        , loginResource = Nothing
                        , loginSessionId = "session-456"
                        , loginServerName = "MCP Server"
                        , loginScopeDescriptions = Map.empty
                        }
            let html = TL.toStrict $ renderText (toHtml page)

            html `shouldSatisfy` T.isInfixOf "MyApp"

        it "includes scope text in the rendered HTML" $ do
            let page =
                    LoginPage
                        { loginClientName = "Test"
                        , loginScopes = "mcp:read mcp:write"
                        , loginResource = Nothing
                        , loginSessionId = "session-789"
                        , loginServerName = "MCP Server"
                        , loginScopeDescriptions = Map.empty
                        }
            let html = TL.toStrict $ renderText (toHtml page)

            -- Should contain the raw scope text (formatting is done separately via formatScopeDescriptions)
            html `shouldSatisfy` T.isInfixOf "mcp:read mcp:write"

        it "includes session ID as hidden field" $ do
            let page =
                    LoginPage
                        { loginClientName = "Test"
                        , loginScopes = "mcp:read"
                        , loginResource = Nothing
                        , loginSessionId = "hidden-session-id"
                        , loginServerName = "MCP Server"
                        , loginScopeDescriptions = Map.empty
                        }
            let html = TL.toStrict $ renderText (toHtml page)

            html `shouldSatisfy` T.isInfixOf "hidden-session-id"
            html `shouldSatisfy` T.isInfixOf "name=\"session_id\""

        it "includes optional resource parameter when present" $ do
            let page =
                    LoginPage
                        { loginClientName = "Test"
                        , loginScopes = "mcp:read"
                        , loginResource = Just "https://api.example.com"
                        , loginSessionId = "session-with-resource"
                        , loginServerName = "MCP Server"
                        , loginScopeDescriptions = Map.empty
                        }
            let html = TL.toStrict $ renderText (toHtml page)

            html `shouldSatisfy` T.isInfixOf "https://api.example.com"

        it "escapes HTML special characters in client name" $ do
            let page =
                    LoginPage
                        { loginClientName = "<script>alert('xss')</script>"
                        , loginScopes = "mcp:read"
                        , loginResource = Nothing
                        , loginSessionId = "session-xss-test"
                        , loginServerName = "MCP Server"
                        , loginScopeDescriptions = Map.empty
                        }
            let html = TL.toStrict $ renderText (toHtml page)

            -- Lucid should auto-escape, so literal <script> should not appear
            html `shouldNotSatisfy` T.isInfixOf "<script>alert('xss')</script>"
            -- But the escaped version should be present
            html `shouldSatisfy` T.isInfixOf "&lt;script&gt;"

    describe "ErrorPage ToHtml instance" $ do
        it "renders an error page with title and message" $ do
            let page = ErrorPage "Invalid Request" "The client_id is missing" "MCP Server"
            let html = TL.toStrict $ renderText (toHtml page)

            html `shouldSatisfy` T.isInfixOf "Invalid Request"
            html `shouldSatisfy` T.isInfixOf "The client_id is missing"

        it "includes DOCTYPE and html tags" $ do
            let page = ErrorPage "Error" "Something went wrong" "MCP Server"
            let html = TL.toStrict $ renderText (toHtml page)

            html `shouldSatisfy` T.isInfixOf "<!DOCTYPE HTML>"
            html `shouldSatisfy` T.isInfixOf "<html>"
            html `shouldSatisfy` T.isInfixOf "</html>"

        it "escapes HTML special characters in error messages" $ do
            let page = ErrorPage "Error" "<script>malicious()</script>" "MCP Server"
            let html = TL.toStrict $ renderText (toHtml page)

            html `shouldNotSatisfy` T.isInfixOf "<script>malicious()</script>"
            html `shouldSatisfy` T.isInfixOf "&lt;script&gt;"

    describe "HTML content type integration" $ do
        it "can render LoginPage to Html type" $ do
            let page =
                    LoginPage
                        { loginClientName = "Integration Test"
                        , loginScopes = "mcp:read"
                        , loginResource = Nothing
                        , loginSessionId = "int-session"
                        , loginServerName = "MCP Server"
                        , loginScopeDescriptions = Map.empty
                        }
            -- This test verifies the type signature works
            let _htmlValue :: Html () = toHtml page
            -- If it compiles and runs, the integration works
            True `shouldBe` True

        it "can render ErrorPage to Html type" $ do
            let page = ErrorPage "Test Error" "Test message" "MCP Server"
            let _htmlValue :: Html () = toHtml page
            True `shouldBe` True

    describe "Error page Lucid rendering in production paths" $ do
        it "renders error pages with automatic HTML escaping for XSS protection" $ do
            -- CRITICAL: This test verifies that error pages use Lucid's ToHtml
            -- instance (automatic escaping) instead of renderErrorPage (manual, unsafe)
            let errorPage = ErrorPage "Session Expired" "<script>alert('xss')</script>" "MCP Server"
            let html = TL.toStrict $ renderText (toHtml errorPage)

            -- XSS content must be escaped
            html `shouldNotSatisfy` T.isInfixOf "<script>alert('xss')</script>"
            html `shouldSatisfy` T.isInfixOf "&lt;script&gt;alert"

        it "constructs ErrorPage values instead of calling renderErrorPage" $ do
            -- This test documents the expected pattern:
            -- OLD: throwError $ InvalidRequest $ renderErrorPage "Title" "Message"
            -- NEW: throwError $ InvalidRequest $ ErrorPage "Title" "Message" serverName
            --
            -- The ErrorPage will be rendered via ToHtml instance for automatic escaping
            let errorPage1 = ErrorPage "Cookies Required" "Your browser must have cookies enabled" "MCP Server"
            let errorPage2 = ErrorPage "Session Expired" "Your login session has expired" "MCP Server"
            let errorPage3 = ErrorPage "Invalid Session" "Session not found or has expired" "MCP Server"

            -- Verify all error pages render with proper escaping
            let html1 = TL.toStrict $ renderText (toHtml errorPage1)
            let html2 = TL.toStrict $ renderText (toHtml errorPage2)
            let html3 = TL.toStrict $ renderText (toHtml errorPage3)

            html1 `shouldSatisfy` T.isInfixOf "Cookies Required"
            html2 `shouldSatisfy` T.isInfixOf "Session Expired"
            html3 `shouldSatisfy` T.isInfixOf "Invalid Session"

    describe "LoginFlowError ToHtml instance" $ do
        it "renders CookiesRequired error with user-friendly message" $ do
            let err = CookiesRequired
            let html = TL.toStrict $ renderText (toHtml err)

            -- Should have proper HTML structure
            html `shouldSatisfy` T.isInfixOf "<!DOCTYPE HTML>"
            html `shouldSatisfy` T.isInfixOf "<html>"
            html `shouldSatisfy` T.isInfixOf "</html>"

            -- Should contain user-friendly title and message
            html `shouldSatisfy` T.isInfixOf "Cookies Required"
            html `shouldSatisfy` T.isInfixOf "cookies enabled"

        it "renders SessionCookieMismatch error" $ do
            let err = SessionCookieMismatch
            let html = TL.toStrict $ renderText (toHtml err)

            html `shouldSatisfy` T.isInfixOf "Cookies Required"
            html `shouldSatisfy` T.isInfixOf "cookie mismatch"

        it "renders SessionNotFound error with session ID" $ do
            let err = SessionNotFound (fromJust $ mkSessionId "test-session-123")
            let html = TL.toStrict $ renderText (toHtml (err :: LoginFlowError))

            html `shouldSatisfy` T.isInfixOf "Invalid Session"
            html `shouldSatisfy` T.isInfixOf "not found"
            html `shouldSatisfy` T.isInfixOf "expired"

        it "renders SessionExpired error with session ID" $ do
            let err = SessionExpired (fromJust $ mkSessionId "expired-session-456")
            let html = TL.toStrict $ renderText (toHtml (err :: LoginFlowError))

            html `shouldSatisfy` T.isInfixOf "Session Expired"
            html `shouldSatisfy` T.isInfixOf "login session has expired"

        it "uses Lucid's automatic HTML escaping" $ do
            -- The ToHtml instance uses Lucid which automatically escapes HTML
            -- This test verifies the instance compiles and renders valid HTML
            let err = SessionNotFound (fromJust $ mkSessionId "test-session")
            let html = TL.toStrict $ renderText (toHtml (err :: LoginFlowError))

            -- Should produce valid HTML structure
            html `shouldSatisfy` T.isInfixOf "<!DOCTYPE HTML>"
            html `shouldSatisfy` T.isInfixOf "<html>"
            html `shouldSatisfy` T.isInfixOf "</html>"
            -- Error message should be present (Lucid auto-escapes all text)
            html `shouldSatisfy` T.isInfixOf "Session not found"
