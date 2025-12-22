{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Servant.OAuth2.IDP.Trace
Description : OAuth trace events with domain newtypes
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC

OAuth-specific trace events using domain newtypes from Servant.OAuth2.IDP.Types
and minimal trace-specific ADTs for operation results and denial reasons.

Per FR-005 requirements:
- OperationResult: type-safe boolean alternative (Success | Failure)
- DenialReason: authorization denial reason ADT
- OAuthTrace: main trace ADT using domain newtypes
-}
module Servant.OAuth2.IDP.Trace (
    -- * Supporting Types
    OperationResult (..),
    DenialReason (..),

    -- * Main Trace ADT
    OAuthTrace (..),

    -- * Rendering
    renderOAuthTrace,
) where

import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Network.URI (uriToString)
import Servant.OAuth2.IDP.Auth.Backend (Username, usernameText)
import Servant.OAuth2.IDP.Errors (ValidationError (..))
import Servant.OAuth2.IDP.Types (
    ClientId,
    OAuthGrantType (..),
    RedirectUri,
    Scope,
    SessionId,
    unClientId,
    unRedirectUri,
    unScope,
    unSessionId,
 )

-- -----------------------------------------------------------------------------
-- Supporting Types
-- -----------------------------------------------------------------------------

-- | Operation result (type-safe boolean alternative)
data OperationResult
    = Success
    | Failure
    deriving stock (Show, Eq, Generic)

-- | Authorization denial reason ADT
data DenialReason
    = UserDenied
    | InvalidRequest
    | UnauthorizedClient
    | ServerError Text
    deriving stock (Show, Eq, Generic)

-- -----------------------------------------------------------------------------
-- Main Trace ADT
-- -----------------------------------------------------------------------------

{- | OAuth trace events using domain newtypes.

All constructors use typed domain values instead of primitives:
- ClientId, SessionId, Username, etc. instead of Text
- OperationResult instead of Bool
- DenialReason ADT instead of Text
- ValidationError domain type instead of Text

This enables type-safe trace construction and exhaustive pattern matching.
-}
data OAuthTrace
    = -- | Client registration with redirect URI
      TraceClientRegistration ClientId RedirectUri
    | -- | Authorization request with scopes
      TraceAuthorizationRequest ClientId [Scope] OperationResult
    | -- | Login page served with session
      TraceLoginPageServed SessionId
    | -- | Login attempt by user
      TraceLoginAttempt Username OperationResult
    | -- | PKCE code challenge validation
      TracePKCEValidation OperationResult
    | -- | Authorization granted to client for user
      TraceAuthorizationGranted ClientId Username
    | -- | Authorization denied with reason
      TraceAuthorizationDenied ClientId DenialReason
    | -- | Token exchange by grant type
      TraceTokenExchange OAuthGrantType OperationResult
    | -- | Refresh token operation
      TraceTokenRefresh OperationResult
    | -- | Login session expired
      TraceSessionExpired SessionId
    | -- | Validation error occurred
      TraceValidationError ValidationError
    deriving stock (Show, Eq, Generic)

-- -----------------------------------------------------------------------------
-- Rendering
-- -----------------------------------------------------------------------------

{- | Render an OAuth trace event to human-readable text.

Unwraps domain newtypes (ClientId, SessionId, Username, etc.) and renders
ADTs (OperationResult, DenialReason, ValidationError) to human-readable text.

Per FR-005 requirements:
- Unwrap domain newtypes using un* functions or show
- Render OperationResult as "SUCCESS"/"FAILED"
- Render DenialReason constructors to human-readable text
- Render OAuthGrantType and ValidationError appropriately
-}
renderOAuthTrace :: OAuthTrace -> Text
renderOAuthTrace = \case
    TraceClientRegistration cid redirectUri ->
        "Client registered: " <> unClientId cid <> " (" <> renderRedirectUri redirectUri <> ")"
    TraceAuthorizationRequest cid scopes result ->
        "Authorization request from "
            <> unClientId cid
            <> " for scopes "
            <> renderScopes scopes
            <> ": "
            <> renderResult result
    TraceLoginPageServed sid ->
        "Login page served for session " <> unSessionId sid
    TraceLoginAttempt user result ->
        "Login attempt for user " <> usernameText user <> ": " <> renderResult result
    TracePKCEValidation result ->
        "PKCE validation: " <> renderResult result
    TraceAuthorizationGranted cid user ->
        "Authorization granted to client " <> unClientId cid <> " by user " <> usernameText user
    TraceAuthorizationDenied cid reason ->
        "Authorization denied for client " <> unClientId cid <> ": " <> renderDenialReason reason
    TraceTokenExchange grantType result ->
        "Token exchange (" <> renderGrantType grantType <> "): " <> renderResult result
    TraceTokenRefresh result ->
        "Token refresh: " <> renderResult result
    TraceSessionExpired sid ->
        "Session expired: " <> unSessionId sid
    TraceValidationError err ->
        "Validation error: " <> renderValidationError err
  where
    -- Render OperationResult as SUCCESS/FAILED
    renderResult :: OperationResult -> Text
    renderResult Success = "SUCCESS"
    renderResult Failure = "FAILED"

    -- Render DenialReason to human-readable text
    renderDenialReason :: DenialReason -> Text
    renderDenialReason UserDenied = "User denied"
    renderDenialReason InvalidRequest = "Invalid request"
    renderDenialReason UnauthorizedClient = "Unauthorized client"
    renderDenialReason (ServerError msg) = "Server error: " <> msg

    -- Render OAuthGrantType to protocol string
    renderGrantType :: OAuthGrantType -> Text
    renderGrantType OAuthAuthorizationCode = "authorization_code"
    renderGrantType OAuthClientCredentials = "client_credentials"

    -- Render RedirectUri (URI) to Text
    renderRedirectUri :: RedirectUri -> Text
    renderRedirectUri = T.pack . (\uri -> uriToString id uri "") . unRedirectUri

    -- Render list of scopes
    renderScopes :: [Scope] -> Text
    renderScopes [] = "(none)"
    renderScopes xs = "[" <> T.intercalate ", " (map unScope xs) <> "]"

    -- Render ValidationError to human-readable text
    renderValidationError :: ValidationError -> Text
    renderValidationError (RedirectUriMismatch cid uri) =
        "Redirect URI mismatch for client " <> unClientId cid <> ": " <> renderRedirectUri uri
    renderValidationError (UnsupportedResponseType rt) =
        "Unsupported response_type: " <> rt
    renderValidationError (ClientNotRegistered cid) =
        "Client not registered: " <> unClientId cid
    renderValidationError (MissingRequiredScope scope) =
        "Missing required scope: " <> unScope scope
    renderValidationError (InvalidStateParameter state) =
        "Invalid state parameter: " <> state
    renderValidationError (UnsupportedCodeChallengeMethod method) =
        "Unsupported code_challenge_method: " <> T.pack (show method) <> " (only S256 supported)"
    renderValidationError (MissingTokenParameter param) =
        "Missing token parameter: " <> T.pack (show param)
    renderValidationError (InvalidTokenParameterFormat param detail) =
        "Invalid token parameter format (" <> T.pack (show param) <> "): " <> detail
    renderValidationError EmptyRedirectUris =
        "Client registration with no redirect URIs"
