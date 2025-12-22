{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : MCP.Server.OAuth.BoundarySpec
Description : Tests for OAuth domain-to-ServerError boundary translation
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC

This module tests the boundary layer that translates domain errors into
Servant ServerError responses via appErrorToServerError.
-}
module MCP.Server.OAuth.BoundarySpec (spec) where

import MCP.Server.HTTP.AppEnv (AppError (..), appErrorToServerError)
import Servant (ServerError (..))
import Servant.OAuth2.IDP.Auth.Demo (DemoAuthError (..))
import Servant.OAuth2.IDP.Errors (AuthorizationError (..), ValidationError (..))
import Servant.OAuth2.IDP.Store.InMemory (OAuthStoreError (..))
import Test.Hspec

spec :: Spec
spec = describe "OAuth.Boundary" $ do
    describe "appErrorToServerError" $ do
        it "translates OAuthStoreErr to 500" $ do
            let err = OAuthStoreErr (StoreUnavailable "test")
                serverErr = appErrorToServerError err
            errHTTPCode serverErr `shouldBe` 500

        it "translates AuthBackendErr to 401" $ do
            let err = AuthBackendErr InvalidCredentials
                serverErr = appErrorToServerError err
            errHTTPCode serverErr `shouldBe` 401

        it "translates ValidationErr to 400" $ do
            let err = ValidationErr (UnsupportedResponseType "implicit")
                serverErr = appErrorToServerError err
            errHTTPCode serverErr `shouldBe` 400

        it "translates AuthorizationErr ExpiredCode to 400" $ do
            let err = AuthorizationErr ExpiredCode
                serverErr = appErrorToServerError err
            errHTTPCode serverErr `shouldBe` 400
