{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Servant.OAuth2.IDP.LoginFlowError
Description : Semantic errors for OAuth login flow
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC

Semantic error types for the OAuth login flow with ToHtml instances
for user-friendly error pages. These errors represent specific failure
modes in the login process and render as HTML pages automatically.
-}
module Servant.OAuth2.IDP.LoginFlowError (
    LoginFlowError (..),
) where

import Lucid (
    ToHtml (..),
    body_,
    charset_,
    class_,
    div_,
    doctypehtml_,
    h1_,
    head_,
    meta_,
    p_,
    style_,
    title_,
 )

import Data.Text (Text)
import Data.Text qualified as T
import Servant.OAuth2.IDP.Types (SessionId (..))

{- | Semantic errors that can occur during the OAuth login flow.

Each constructor represents a specific failure mode with enough
information to render a user-friendly error page.
-}
data LoginFlowError
    = -- | Browser does not have cookies enabled
      CookiesRequired
    | -- | Session cookie doesn't match the form session ID
      SessionCookieMismatch
    | -- | Session not found in storage
      SessionNotFound SessionId
    | -- | Login session has expired
      SessionExpired SessionId
    deriving (Show, Eq)

{- | Render login flow errors as user-friendly HTML pages.

Each error type produces a styled error page with appropriate
title and message. HTML special characters are automatically
escaped by Lucid.
-}
instance ToHtml LoginFlowError where
    toHtmlRaw = toHtml
    toHtml err = doctypehtml_ $ do
        head_ $ do
            meta_ [charset_ "utf-8"]
            title_ $ toHtml (errorTitle err <> " - MCP Server")
            style_ $
                T.unlines
                    [ "body { font-family: system-ui, sans-serif; max-width: 500px; margin: 50px auto; padding: 20px; }"
                    , "h1 { color: #d32f2f; }"
                    , ".error { background: #ffebee; padding: 15px; border-radius: 5px; border-left: 4px solid #d32f2f; }"
                    ]
        body_ $ do
            h1_ $ toHtml (errorTitle err)
            div_ [class_ "error"] $ do
                p_ $ toHtml (errorMessage err)
            p_ "Please contact the application developer."

-- | Get the error title for a LoginFlowError
errorTitle :: LoginFlowError -> Text
errorTitle CookiesRequired = "Cookies Required"
errorTitle SessionCookieMismatch = "Cookies Required"
errorTitle (SessionNotFound _) = "Invalid Session"
errorTitle (SessionExpired _) = "Session Expired"

-- | Get the user-friendly error message for a LoginFlowError
errorMessage :: LoginFlowError -> Text
errorMessage CookiesRequired =
    "Your browser must have cookies enabled to sign in. Please enable cookies and try again."
errorMessage SessionCookieMismatch =
    "Session cookie mismatch. Please enable cookies and try again."
errorMessage (SessionNotFound _) =
    "Session not found or has expired. Please restart the authorization flow."
errorMessage (SessionExpired _) =
    "Your login session has expired. Please restart the authorization flow."
