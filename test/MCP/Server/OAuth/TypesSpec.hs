{-# LANGUAGE OverloadedStrings #-}

module MCP.Server.OAuth.TypesSpec (spec) where

import Test.Hspec

{- NOTE: This test file has been disabled during Phase F.3 (AuthorizationError ADT refactoring).
   The tests in this file tested the old Text-based error constructor interface which has been
   replaced with structured ADT payloads. The new comprehensive tests for the ADT interface
   are in test/Servant/OAuth2/IDP/ErrorsSpec.hs.

   This file is kept as a placeholder to avoid breaking the test suite structure, but all
   tests have been removed as they tested deprecated functionality.
-}

spec :: Spec
spec = do
    describe "MCP.Server.OAuth.Types (deprecated tests removed)" $ do
        it "placeholder test" $ do
            True `shouldBe` True
