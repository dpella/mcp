{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : Servant.OAuth2.IDP.Boundary
Description : Boundary conversion functions between Text and typed OAuth newtypes
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC

This module provides unsafe constructors and extractors for converting between
Text (used by HTTP.hs and Servant) and type-safe OAuth newtypes. These functions
bypass validation and should ONLY be used at the HTTP boundary where data has
already been validated by the HTTP layer.

= Usage Pattern

HTTP.hs receives Text from Servant endpoints → convert to typed newtypes using
unsafe constructors → call typeclass methods → convert results back to Text for
JSON responses.

= Safety Notes

These functions are marked "unsafe" because they bypass the smart constructors
that perform validation. They should ONLY be used when:

1. The Text value comes from a validated HTTP request
2. You're at the boundary between HTTP.hs and the typeclass layer
3. You need to convert between representations without re-validating

For all other uses, prefer the smart constructors from "Servant.OAuth2.IDP.Types".
-}
module Servant.OAuth2.IDP.Boundary (
    -- * Unsafe Constructors (HTTP boundary only)

    -- | WARNING: These bypass validation. Use only for data from validated HTTP requests.
    unsafeAuthCodeId,
    unsafeClientId,
    unsafeSessionId,
    unsafeAccessTokenId,
    unsafeRefreshTokenId,
    unsafeUserId,
    unsafeScope,
    unsafeCodeChallenge,

    -- * Extractors (typed → Text)
    authCodeIdToText,
    clientIdToText,
    sessionIdToText,
    accessTokenIdToText,
    refreshTokenIdToText,
    userIdToText,
    redirectUriToText,
    scopeToText,
    codeChallengeToText,

    -- * Error Boundary Translation
    OAuthBoundaryTrace (..),
    domainErrorToServerError,

    -- * Re-exports from Servant.OAuth2.IDP.Types
    module Servant.OAuth2.IDP.Types,
) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as BL
import Data.Functor.Const (Const (..))
import Data.Generics.Sum (AsType (..))
import Data.Monoid (First (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as TL
import Lucid (renderText, toHtml)
import Network.HTTP.Types.Status (Status, statusCode)
import Plow.Logging (IOTracer (..), traceWith)
import Servant.OAuth2.IDP.Auth.Backend (AuthBackend (..))
import Servant.OAuth2.IDP.LoginFlowError (LoginFlowError)
import Servant.OAuth2.IDP.Store (OAuthStateStore (..))
import Servant.OAuth2.IDP.Types
import Servant.Server (ServerError (..), err401, err500)

-- -----------------------------------------------------------------------------
-- Unsafe Constructors
-- -----------------------------------------------------------------------------

{- | Unsafe constructor for AuthCodeId.

WARNING: Bypasses validation. Use only for data from validated HTTP requests.
-}
unsafeAuthCodeId :: Text -> AuthCodeId
unsafeAuthCodeId = AuthCodeId

{- | Unsafe constructor for ClientId.

WARNING: Bypasses validation. Use only for data from validated HTTP requests.
-}
unsafeClientId :: Text -> ClientId
unsafeClientId = ClientId

{- | Unsafe constructor for SessionId.

WARNING: Bypasses validation (does NOT check UUID format).
Use only for data from validated HTTP requests.
-}
unsafeSessionId :: Text -> SessionId
unsafeSessionId = SessionId

{- | Unsafe constructor for AccessTokenId.

WARNING: Bypasses validation. Use only for JWT tokens from validated sources.
-}
unsafeAccessTokenId :: Text -> AccessTokenId
unsafeAccessTokenId = AccessTokenId

{- | Unsafe constructor for RefreshTokenId.

WARNING: Bypasses validation. Use only for data from validated HTTP requests.
-}
unsafeRefreshTokenId :: Text -> RefreshTokenId
unsafeRefreshTokenId = RefreshTokenId

{- | Unsafe constructor for UserId.

WARNING: Bypasses validation. Use only for data from validated HTTP requests.
-}
unsafeUserId :: Text -> UserId
unsafeUserId = UserId

{- | Unsafe constructor for Scope.

WARNING: Bypasses validation (does NOT check for whitespace).
Use only for data from validated HTTP requests.
-}
unsafeScope :: Text -> Scope
unsafeScope = Scope

{- | Unsafe constructor for CodeChallenge.

WARNING: Bypasses validation (does NOT check base64url charset or length).
Use only for data from validated HTTP requests.
-}
unsafeCodeChallenge :: Text -> CodeChallenge
unsafeCodeChallenge = CodeChallenge

-- -----------------------------------------------------------------------------
-- Extractors
-- -----------------------------------------------------------------------------

-- | Extract Text from AuthCodeId
authCodeIdToText :: AuthCodeId -> Text
authCodeIdToText = unAuthCodeId

-- | Extract Text from ClientId
clientIdToText :: ClientId -> Text
clientIdToText = unClientId

-- | Extract Text from SessionId
sessionIdToText :: SessionId -> Text
sessionIdToText = unSessionId

-- | Extract Text from AccessTokenId
accessTokenIdToText :: AccessTokenId -> Text
accessTokenIdToText = unAccessTokenId

-- | Extract Text from RefreshTokenId
refreshTokenIdToText :: RefreshTokenId -> Text
refreshTokenIdToText = unRefreshTokenId

-- | Extract Text from UserId
userIdToText :: UserId -> Text
userIdToText = unUserId

-- | Extract Text from RedirectUri
redirectUriToText :: RedirectUri -> Text
redirectUriToText (RedirectUri uri) = T.pack (show uri)

-- | Extract Text from Scope
scopeToText :: Scope -> Text
scopeToText = unScope

-- | Extract Text from CodeChallenge
codeChallengeToText :: CodeChallenge -> Text
codeChallengeToText = unCodeChallenge

-- -----------------------------------------------------------------------------
-- Error Boundary Translation
-- -----------------------------------------------------------------------------

{- | Trace events for boundary error translation.

These events are logged for observability but never exposed to clients.
-}
data OAuthBoundaryTrace
    = -- | OAuth state storage error (details hidden from client)
      BoundaryStoreError Text
    | -- | Authentication backend error (details hidden from client)
      BoundaryAuthError Text
    | -- | Validation error (safe to expose)
      BoundaryValidationError ValidationError
    | -- | Authorization error (safe to expose)
      BoundaryAuthorizationError AuthorizationError
    | -- | Login flow error (safe to expose, renders as HTML)
      BoundaryLoginFlowError LoginFlowError
    deriving (Eq, Show)

{- | Translate domain errors to Servant ServerError with security-conscious handling.

This function uses generic-lens 'AsType' constraints to pattern match on
different error types and translate them appropriately:

* **OAuthStateError**: Logs details, returns generic 500 (no details exposed)
* **AuthBackendError**: Logs details, returns generic 401 (no details exposed)
* **ValidationError**: Returns descriptive 400 (safe to expose)
* **AuthorizationError**: Returns OAuth error response (safe to expose)

== Returns

* @Just ServerError@: If the error matches one of the prisms
* @Nothing@: If the error doesn't match any prism (caller handles it)
-}
domainErrorToServerError ::
    forall m m' e trace.
    ( MonadIO m
    , AsType (OAuthStateError m') e
    , AsType (AuthBackendError m') e
    , AsType ValidationError e
    , AsType AuthorizationError e
    , AsType LoginFlowError e
    , Show (OAuthStateError m')
    , Show (AuthBackendError m')
    ) =>
    IOTracer trace ->
    (OAuthBoundaryTrace -> trace) ->
    e ->
    m (Maybe ServerError)
domainErrorToServerError tracer inject err =
    -- Try each prism in order
    case tryOAuthStateError err of
        Just storeErr -> do
            -- Log details but return generic 500
            liftIO $ traceWith tracer $ inject $ BoundaryStoreError $ T.pack $ show storeErr
            pure $ Just $ err500{errBody = "Internal server error"}
        Nothing -> case tryAuthBackendError err of
            Just authErr -> do
                -- Log details but return generic 401
                liftIO $ traceWith tracer $ inject $ BoundaryAuthError $ T.pack $ show authErr
                pure $ Just $ err401{errBody = "Unauthorized"}
            Nothing -> case tryValidationError err of
                Just validationErr -> do
                    -- Validation errors are safe to expose
                    let (status, message) = validationErrorToResponse validationErr
                    pure $ Just $ toServerError status message
                Nothing -> case tryLoginFlowError err of
                    Just loginErr -> do
                        -- Login flow errors are safe to expose, render as HTML via ToHtml instance
                        liftIO $ traceWith tracer $ inject $ BoundaryLoginFlowError loginErr
                        pure $ Just $ toServerErrorLoginFlow loginErr
                    Nothing -> case tryAuthorizationError err of
                        Just authzErr -> do
                            -- Authorization errors use OAuth JSON format
                            let (status, oauthResp) = authorizationErrorToResponse authzErr
                            pure $ Just $ toServerErrorOAuth status oauthResp
                        Nothing ->
                            -- No prism matched
                            pure Nothing
  where
    -- Try to extract OAuthStateError using AsType prism
    -- Using Const (First a) to properly extract 'a' from sum type 'e'
    tryOAuthStateError :: e -> Maybe (OAuthStateError m')
    tryOAuthStateError = getFirst . getConst . (_Typed @(OAuthStateError m') @e) (Const . First . Just)

    -- Try to extract AuthBackendError using AsType prism
    tryAuthBackendError :: e -> Maybe (AuthBackendError m')
    tryAuthBackendError = getFirst . getConst . (_Typed @(AuthBackendError m') @e) (Const . First . Just)

    -- Try to extract ValidationError using AsType prism
    tryValidationError :: e -> Maybe ValidationError
    tryValidationError = getFirst . getConst . (_Typed @ValidationError @e) (Const . First . Just)

    -- Try to extract LoginFlowError using AsType prism
    tryLoginFlowError :: e -> Maybe LoginFlowError
    tryLoginFlowError = getFirst . getConst . (_Typed @LoginFlowError @e) (Const . First . Just)

    -- Try to extract AuthorizationError using AsType prism
    tryAuthorizationError :: e -> Maybe AuthorizationError
    tryAuthorizationError = getFirst . getConst . (_Typed @AuthorizationError @e) (Const . First . Just)

    -- Convert Status + Text to ServerError
    toServerError :: Network.HTTP.Types.Status.Status -> Text -> ServerError
    toServerError status message =
        ServerError
            { errHTTPCode = fromIntegral $ statusCode status
            , errReasonPhrase = ""
            , errBody = BL.fromStrict $ TE.encodeUtf8 message
            , errHeaders = [("Content-Type", "text/plain; charset=utf-8")]
            }

    -- Convert Status + OAuthErrorResponse to ServerError
    toServerErrorOAuth :: Network.HTTP.Types.Status.Status -> OAuthErrorResponse -> ServerError
    toServerErrorOAuth status oauthResp =
        ServerError
            { errHTTPCode = fromIntegral $ statusCode status
            , errReasonPhrase = ""
            , errBody = encode oauthResp
            , errHeaders = [("Content-Type", "application/json; charset=utf-8")]
            }

    -- Convert LoginFlowError to HTML ServerError using ToHtml instance
    toServerErrorLoginFlow :: LoginFlowError -> ServerError
    toServerErrorLoginFlow loginErr =
        let htmlBytes = BL.fromStrict $ TE.encodeUtf8 $ TL.toStrict $ renderText $ toHtml loginErr
         in ServerError
                { errHTTPCode = 400
                , errReasonPhrase = ""
                , errBody = htmlBytes
                , errHeaders = [("Content-Type", "text/html; charset=utf-8")]
                }
