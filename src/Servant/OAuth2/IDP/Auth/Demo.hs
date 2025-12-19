{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Servant.OAuth2.IDP.Auth.Demo
Description : Demo credential AuthBackend implementation
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC

This module provides a demo implementation of the AuthBackend typeclass
using an in-memory credential store with hardcoded demo accounts.

== Security Notice

This implementation is for DEMONSTRATION purposes only:

* Uses hardcoded credentials (demo/demo123, admin/admin456)
* Uses simple SHA256 hashing (not suitable for production)
* No rate limiting or account lockout
* No audit logging

Production implementations should:

* Use secure password hashing (Argon2id, bcrypt, PBKDF2)
* Store credentials in a secure database
* Implement rate limiting and account lockout
* Log all authentication attempts
* Support password rotation policies

== Usage

@
import Servant.OAuth2.IDP.Auth.Demo
import Control.Monad.Reader

-- Create environment with demo credentials
let env = DemoCredentialEnv defaultDemoCredentialStore

-- Validate credentials
let username = Username "demo"
let password = mkPlaintextPassword "demo123"
result <- runReaderT (validateCredentials username password) env
-- result == True
@
-}
module Servant.OAuth2.IDP.Auth.Demo (
    -- * Environment
    DemoCredentialEnv (..),

    -- * Error Type
    DemoAuthError (..),

    -- * User Types
    DemoUserId,
    AuthUser (..),

    -- * Default Credentials
    defaultDemoCredentialStore,
) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT, asks)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.:?), (.=))
import Data.ByteArray qualified as BA
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import GHC.Generics (Generic)
import Servant.Auth.Server (FromJWT, ToJWT)
import Servant.OAuth2.IDP.Auth.Backend (
    AuthBackend (..),
    CredentialStore (..),
    Salt (..),
    Username (..),
    mkHashedPassword,
    mkPlaintextPassword,
 )
import Servant.OAuth2.IDP.Types (UserId (..))

-- -----------------------------------------------------------------------------
-- User Types
-- -----------------------------------------------------------------------------

-- | Demo user ID type (alias for UserId)
type DemoUserId = UserId

-- | Authenticated user information
data AuthUser = AuthUser
    { userUserId :: UserId
    , userUserEmail :: Maybe Text
    , userUserName :: Maybe Text
    }
    deriving stock (Eq, Show, Generic)

instance FromJSON AuthUser where
    parseJSON = withObject "AuthUser" $ \v ->
        AuthUser
            <$> v .: "user_id"
            <*> v .:? "user_email"
            <*> v .:? "user_name"

instance ToJSON AuthUser where
    toJSON AuthUser{..} =
        object
            [ "user_id" .= userUserId
            , "user_email" .= userUserEmail
            , "user_name" .= userUserName
            ]

-- | JWT instances for AuthUser (rely on JSON instances above)
instance ToJWT AuthUser

instance FromJWT AuthUser

-- -----------------------------------------------------------------------------
-- Environment
-- -----------------------------------------------------------------------------

{- | Environment for demo credential authentication.

Contains the in-memory credential store with demo accounts.
-}
newtype DemoCredentialEnv = DemoCredentialEnv
    { credentialStore :: CredentialStore
    -- ^ In-memory map of username -> hashed password
    }
    deriving (Generic)

-- -----------------------------------------------------------------------------
-- Error Type
-- -----------------------------------------------------------------------------

{- | Authentication errors for demo implementation.

Intentionally minimal to avoid leaking information about which step failed.
-}
data DemoAuthError
    = -- | Username/password combination is invalid (deliberately vague)
      InvalidCredentials
    | -- | User does not exist (only used internally, not exposed to clients)
      UserNotFound Username
    deriving (Eq, Show, Generic)

-- -----------------------------------------------------------------------------
-- AuthBackend Instance
-- -----------------------------------------------------------------------------

instance (MonadIO m) => AuthBackend (ReaderT DemoCredentialEnv m) where
    type AuthBackendError (ReaderT DemoCredentialEnv m) = DemoAuthError
    type AuthBackendEnv (ReaderT DemoCredentialEnv m) = DemoCredentialEnv
    type AuthBackendUser (ReaderT DemoCredentialEnv m) = AuthUser

    validateCredentials username password = do
        store <- asks credentialStore
        let storedHash = Map.lookup username (storeCredentials store)
        case storedHash of
            Nothing -> pure Nothing -- User not found (same as invalid password)
            Just hash -> do
                let candidateHash = mkHashedPassword (storeSalt store) password
                -- ScrubbedBytes Eq is constant-time
                if hash == candidateHash
                    then do
                        let userId = UserId (unUsername username)
                        let authUser =
                                AuthUser
                                    { userUserId = userId
                                    , userUserEmail = Just (unUsername username <> "@demo.local")
                                    , userUserName = Just (unUsername username)
                                    }
                        pure $ Just authUser
                    else pure Nothing

-- -----------------------------------------------------------------------------
-- Default Credentials
-- -----------------------------------------------------------------------------

{- | Default demo credential store with hardcoded test accounts.

Credentials:

* Username: @demo@, Password: @demo123@
* Username: @admin@, Password: @admin456@

== Example

@
import Servant.OAuth2.IDP.Auth.Demo
import Control.Monad.Reader

let env = DemoCredentialEnv defaultDemoCredentialStore
let username = Username "demo"
let password = mkPlaintextPassword "demo123"
result <- runReaderT (validateCredentials username password) env
-- result == True
@
-}
defaultDemoCredentialStore :: CredentialStore
defaultDemoCredentialStore =
    let saltText = "mcp-demo-salt" :: Text
        saltBytes = BA.convert (TE.encodeUtf8 saltText) :: BA.ScrubbedBytes
        salt = Salt saltBytes
        demoHash = mkHashedPassword salt (mkPlaintextPassword "demo123")
        adminHash = mkHashedPassword salt (mkPlaintextPassword "admin456")
     in CredentialStore
            { storeCredentials =
                Map.fromList
                    [ (Username "demo", demoHash)
                    , (Username "admin", adminHash)
                    ]
            , storeSalt = salt
            }
