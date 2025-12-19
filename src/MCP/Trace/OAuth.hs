{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : MCP.Trace.OAuth
Description : OAuth 2.0 flow tracing types
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC

OAuth tracing types for structured logging of OAuth flows.

IMPORTANT: This module is intentionally MCP-independent to enable
future package separation. Do NOT import MCP-specific modules here.
-}
module MCP.Trace.OAuth (
    OAuthTrace (..),
    renderOAuthTrace,
) where

import Data.Text (Text)
import Data.Text qualified as T

{- | OAuth 2.0 flow events.

This type is semantically independent of MCP for future package separation.
-}
data OAuthTrace
    = OAuthClientRegistration
        { clientId :: Text
        , clientName :: Text
        }
    | OAuthAuthorizationRequest
        { clientId :: Text
        , scopes :: [Text]
        , hasState :: Bool
        }
    | OAuthLoginPageServed
        { sessionId :: Text
        }
    | OAuthLoginAttempt
        { username :: Text
        , success :: Bool
        }
    | OAuthPKCEValidation
        { pkceVerifier :: Text
        , pkceChallenge :: Text
        , pkceIsValid :: Bool
        }
    | OAuthAuthorizationGranted
        { clientId :: Text
        , userId :: Text
        }
    | OAuthAuthorizationDenied
        { clientId :: Text
        , reason :: Text
        }
    | OAuthTokenExchange
        { grantType :: Text -- "authorization_code" or "refresh_token"
        , success :: Bool
        }
    | OAuthTokenRefresh
        { success :: Bool
        }
    | OAuthSessionExpired
        { sessionId :: Text
        }
    | OAuthValidationError
        { errorType :: Text
        , validationDetail :: Text
        }
    deriving (Show, Eq)

-- | Render an OAuth trace event to human-readable text.
renderOAuthTrace :: OAuthTrace -> Text
renderOAuthTrace = \case
    OAuthClientRegistration{clientId = cid, clientName = name} ->
        "Client registered: " <> cid <> " (" <> name <> ")"
    OAuthAuthorizationRequest{clientId = cid, scopes = scs, hasState = state} ->
        "Authorization request from " <> cid <> " for scopes " <> renderScopes scs <> stateInfo state
    OAuthLoginPageServed{sessionId = sid} ->
        "Login page served for session " <> sid
    OAuthLoginAttempt{username = user, success = ok} ->
        "Login attempt for user " <> user <> ": " <> if ok then "SUCCESS" else "FAILED"
    OAuthPKCEValidation{pkceVerifier = verifier, pkceChallenge = challenge, pkceIsValid = valid} ->
        "PKCE validation (verifier=" <> T.take 10 verifier <> "..., challenge=" <> T.take 10 challenge <> "...): " <> if valid then "SUCCESS" else "FAILED"
    OAuthAuthorizationGranted{clientId = cid, userId = uid} ->
        "Authorization granted to client " <> cid <> " by user " <> uid
    OAuthAuthorizationDenied{clientId = cid, reason = rsn} ->
        "Authorization denied for client " <> cid <> ": " <> rsn
    OAuthTokenExchange{grantType = gt, success = ok} ->
        "Token exchange (" <> gt <> "): " <> if ok then "SUCCESS" else "FAILED"
    OAuthTokenRefresh{success = ok} ->
        "Token refresh: " <> if ok then "SUCCESS" else "FAILED"
    OAuthSessionExpired{sessionId = sid} ->
        "Session expired: " <> sid
    OAuthValidationError{errorType = typ, validationDetail = detail} ->
        "Validation error [" <> typ <> "]: " <> detail
  where
    renderScopes :: [Text] -> Text
    renderScopes [] = "(none)"
    renderScopes xs = "[" <> mconcat (punctuate ", " xs) <> "]"

    stateInfo :: Bool -> Text
    stateInfo True = " (with state)"
    stateInfo False = ""

    punctuate :: Text -> [Text] -> [Text]
    punctuate _ [] = []
    punctuate _ [x] = [x]
    punctuate sep (x : xs) = (x <> sep) : punctuate sep xs
