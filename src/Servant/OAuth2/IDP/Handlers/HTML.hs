{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Servant.OAuth2.IDP.Handlers.HTML
Description : HTML rendering functions for OAuth pages
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC

HTML rendering functions for login pages and error pages using Lucid.
-}
module Servant.OAuth2.IDP.Handlers.HTML (
    -- * Data Types
    LoginPage (..),
    ErrorPage (..),

    -- * Helper Functions
    scopeToDescription,
    formatScopeDescriptions,
) where

import Data.Text (Text)
import Data.Text qualified as T
import Lucid (
    ToHtml (..),
    action_,
    body_,
    button_,
    charset_,
    class_,
    div_,
    doctypehtml_,
    form_,
    h1_,
    head_,
    input_,
    label_,
    meta_,
    method_,
    name_,
    p_,
    style_,
    title_,
    toHtmlRaw,
    type_,
    value_,
 )

-- -----------------------------------------------------------------------------
-- Data Types
-- -----------------------------------------------------------------------------

{- | Login page data for rendering.

Contains all information needed to render an OAuth login page:
client name, requested scopes, optional resource, and session ID.
-}
data LoginPage = LoginPage
    { loginClientName :: Text
    , loginScopes :: Text
    , loginResource :: Maybe Text
    , loginSessionId :: Text
    }
    deriving (Show, Eq)

{- | Error page data for rendering.

Contains error title and message to display to the user.
-}
data ErrorPage = ErrorPage
    { errorTitle :: Text
    , errorMessage :: Text
    }
    deriving (Show, Eq)

-- -----------------------------------------------------------------------------
-- ToHtml Instances
-- -----------------------------------------------------------------------------

instance ToHtml LoginPage where
    toHtmlRaw = toHtml
    toHtml LoginPage{..} = doctypehtml_ $ do
        head_ $ do
            meta_ [charset_ "utf-8"]
            title_ "Sign In - MCP Server"
            style_ $
                T.unlines
                    [ "body { font-family: system-ui, sans-serif; max-width: 500px; margin: 50px auto; padding: 20px; }"
                    , "h1 { color: #333; }"
                    , "form { margin-top: 20px; }"
                    , "label { display: block; margin: 15px 0 5px; }"
                    , "input[type=text], input[type=password] { width: 100%; padding: 8px; box-sizing: border-box; }"
                    , "button { margin-top: 20px; margin-right: 10px; padding: 10px 20px; }"
                    , ".info { background: #f0f0f0; padding: 15px; border-radius: 5px; margin: 20px 0; }"
                    ]
        body_ $ do
            h1_ "Sign In"
            div_ [class_ "info"] $ do
                p_ $ do
                    "Application "
                    toHtmlRaw ("<strong>" :: Text)
                    toHtml loginClientName
                    toHtmlRaw ("</strong>" :: Text)
                    " is requesting access."
                p_ $ do
                    "Permissions requested: "
                    toHtml (formatScopeDescriptions loginScopes)
                case loginResource of
                    Just res -> p_ $ do
                        "Resource: "
                        toHtml res
                    Nothing -> pure ()
            form_ [method_ "POST", action_ "/login"] $ do
                input_ [type_ "hidden", name_ "session_id", value_ loginSessionId]
                label_ $ do
                    "Username:"
                    input_ [type_ "text", name_ "username"]
                label_ $ do
                    "Password:"
                    input_ [type_ "password", name_ "password"]
                button_ [type_ "submit", name_ "action", value_ "login"] "Sign In"
                button_ [type_ "submit", name_ "action", value_ "deny"] "Deny"

instance ToHtml ErrorPage where
    toHtmlRaw = toHtml
    toHtml ErrorPage{..} = doctypehtml_ $ do
        head_ $ do
            meta_ [charset_ "utf-8"]
            title_ "Error - MCP Server"
            style_ $
                T.unlines
                    [ "body { font-family: system-ui, sans-serif; max-width: 500px; margin: 50px auto; padding: 20px; }"
                    , "h1 { color: #d32f2f; }"
                    , ".error { background: #ffebee; padding: 15px; border-radius: 5px; border-left: 4px solid #d32f2f; }"
                    ]
        body_ $ do
            h1_ $ toHtml errorTitle
            div_ [class_ "error"] $ do
                p_ $ toHtml errorMessage
            p_ "Please contact the application developer."

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
