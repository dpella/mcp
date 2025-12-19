{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Laws.OAuthUserTypeSpec
Description : Tests for OAuthUser/OAuthUserId associated types
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC

This module tests that the OAuthStateStore typeclass has the required
OAuthUser and OAuthUserId associated types with correct signatures.
-}
module Laws.OAuthUserTypeSpec (spec) where

import Data.Proxy (Proxy (..))
import Data.Typeable (typeRep)
import Test.Hspec (Spec, describe, it, shouldBe)
import TestMonad (TestM)

import Servant.OAuth2.IDP.Auth.Demo (AuthUser)
import Servant.OAuth2.IDP.Store (OAuthStateStore (OAuthUser))

{- | Test that OAuthUser associated type exists.

This test verifies that:
1. OAuthStateStore has OAuthUser associated type
2. For TestM, OAuthUser TestM is AuthUser
-}
spec :: Spec
spec = describe "OAuthStateStore associated types" $ do
    it "has OAuthUser associated type for TestM" $ do
        let userType = typeRep (Proxy @(OAuthUser TestM))
            expectedType = typeRep (Proxy @AuthUser)
        userType `shouldBe` expectedType
