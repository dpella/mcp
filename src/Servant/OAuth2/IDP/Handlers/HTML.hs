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

    -- * Rendering Functions
    renderLoginPage,
    renderErrorPage,

    -- * Helper Functions
    scopeToDescription,
    formatScopeDescriptions,
) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Lucid (
    Html,
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

import Servant.OAuth2.IDP.Types (Scope, mkScope, unScope)

-- -----------------------------------------------------------------------------
-- Data Types
-- -----------------------------------------------------------------------------

{- | Login page data for rendering.

Contains all information needed to render an OAuth login page:
client name, requested scopes, optional resource, session ID, and branding configuration.
-}
data LoginPage = LoginPage
    { loginClientName :: Text
    , loginScopes :: Text
    , loginResource :: Maybe Text
    , loginSessionId :: Text
    , loginServerName :: Text
    -- ^ Server name for branding (used in page title)
    , loginScopeDescriptions :: Map Scope Text
    -- ^ Map for scope-to-description lookup (currently unused in rendering, but available for future enhancement)
    }
    deriving (Show, Eq)

{- | Error page data for rendering.

Contains error title, message, and branding configuration.
-}
data ErrorPage = ErrorPage
    { errorTitle :: Text
    , errorMessage :: Text
    , errorServerName :: Text
    -- ^ Server name for branding (used in page title)
    }
    deriving (Show, Eq)

-- -----------------------------------------------------------------------------
-- ToHtml Instances (use configured server name from data types)
-- -----------------------------------------------------------------------------

instance ToHtml LoginPage where
    toHtmlRaw = toHtml
    toHtml page = toHtml (renderLoginPage (loginServerName page) page)

instance ToHtml ErrorPage where
    toHtmlRaw = toHtml
    toHtml page = toHtml (renderErrorPage (errorServerName page) page)

-- -----------------------------------------------------------------------------
-- Rendering Functions
-- -----------------------------------------------------------------------------

-- | Render login page with configurable server name
renderLoginPage :: Text -> LoginPage -> Html ()
renderLoginPage serverName LoginPage{..} = doctypehtml_ $ do
    head_ $ do
        meta_ [charset_ "utf-8"]
        title_ $ toHtml ("Sign In - " <> serverName)
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
                toHtml loginScopes
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
            button_ [type_ "submit", name_ "action", value_ "approve"] "Sign In"
            button_ [type_ "submit", name_ "action", value_ "deny"] "Deny"

-- | Render error page with configurable server name
renderErrorPage :: Text -> ErrorPage -> Html ()
renderErrorPage serverName ErrorPage{..} = doctypehtml_ $ do
    head_ $ do
        meta_ [charset_ "utf-8"]
        title_ $ toHtml ("Error - " <> serverName)
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

-- -----------------------------------------------------------------------------
-- Helper Functions
-- -----------------------------------------------------------------------------

-- | Map scope to human-readable description using configurable map
scopeToDescription :: Map Scope Text -> Scope -> Text
scopeToDescription scopeMap scope =
    case Map.lookup scope scopeMap of
        Just description -> description
        Nothing -> "Access " <> unScope scope -- Generic fallback

-- | Format scopes as human-readable descriptions using configurable map
formatScopeDescriptions :: Map Scope Text -> Text -> Text
formatScopeDescriptions scopeMap scopesText =
    let scopeList = T.splitOn " " scopesText
        -- Parse Text to Scope, filtering out invalid/empty scopes
        parseScope t = if T.null t then Nothing else mkScope t
        scopes = mapMaybe parseScope scopeList
        descriptions = map (scopeToDescription scopeMap) scopes
     in T.intercalate ", " descriptions
