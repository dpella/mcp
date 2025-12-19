{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Servant.OAuth2.IDP.Handlers.Helpers
Description : Helper functions for OAuth handlers
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC

Helper functions used across OAuth handler implementations.
-}
module Servant.OAuth2.IDP.Handlers.Helpers (
    extractSessionFromCookie,
    generateAuthCode,
    generateJWTAccessToken,
    generateRefreshTokenWithConfig,
) where

import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString.Lazy qualified as LBS
import Data.Generics.Sum.Typed (AsType, injectTyped)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.UUID.V4 qualified as UUID
import Servant.Auth.Server (JWTSettings, ToJWT, makeJWT)

import Data.UUID qualified as UUID
import MCP.Server.Auth (OAuthConfig (..), authCodePrefix, refreshTokenPrefix)
import MCP.Server.HTTP.AppEnv (HTTPServerConfig (..))
import Servant.OAuth2.IDP.Store (OAuthStateStore (..))
import Servant.OAuth2.IDP.Types (
    AuthorizationError (..),
    SessionId (..),
    mkSessionId,
 )

-- | Extract session ID from cookie header
extractSessionFromCookie :: Text -> Maybe SessionId
extractSessionFromCookie cookieHeader =
    let cookies = T.splitOn ";" cookieHeader
        sessionCookies = filter (T.isInfixOf "mcp_session=") cookies
     in case sessionCookies of
            (cookie : _) ->
                let parts = T.splitOn "=" cookie
                 in case parts of
                        [_, value] -> mkSessionId (T.strip value)
                        _ -> Nothing
            [] -> Nothing

-- | Generate authorization code with config prefix
generateAuthCode :: HTTPServerConfig -> IO Text
generateAuthCode config = do
    uuid <- UUID.nextRandom
    let prefix = maybe "code_" authCodePrefix (httpOAuthConfig config)
    return $ prefix <> UUID.toText uuid

-- | Generate JWT access token for user
generateJWTAccessToken :: (OAuthStateStore m, ToJWT (OAuthUser m), MonadIO m, MonadError e m, AsType AuthorizationError e) => OAuthUser m -> JWTSettings -> m Text
generateJWTAccessToken user jwtSettings = do
    accessTokenResult <- liftIO $ makeJWT user jwtSettings Nothing
    case accessTokenResult of
        Left _err -> throwError $ injectTyped @AuthorizationError $ InvalidRequest "Token generation failed"
        Right accessToken -> case TE.decodeUtf8' $ LBS.toStrict accessToken of
            Left _decodeErr -> throwError $ injectTyped @AuthorizationError $ InvalidRequest "Token encoding failed"
            Right tokenText -> return tokenText

-- | Generate refresh token with configurable prefix
generateRefreshTokenWithConfig :: HTTPServerConfig -> IO Text
generateRefreshTokenWithConfig config = do
    uuid <- UUID.nextRandom
    let prefix = maybe "rt_" refreshTokenPrefix (httpOAuthConfig config)
    return $ prefix <> UUID.toText uuid
