{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Servant.OAuth2.IDP.BearerMethodSpec
Description : Tests for BearerMethod type and JSON serialization
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC

Tests for BearerMethod ADT and its JSON serialization per RFC 6750.
-}
module Servant.OAuth2.IDP.BearerMethodSpec (spec) where

import Data.Aeson (decode, encode)
import Data.Aeson qualified as Aeson
import Servant.OAuth2.IDP.Metadata (BearerMethod (..))
import Test.Hspec

spec :: Spec
spec = do
    describe "BearerMethod" $ do
        context "ToJSON instance" $ do
            it "serializes BearerHeader to \"header\"" $ do
                let encoded = encode BearerHeader
                decode encoded `shouldBe` Just (Aeson.String "header")

            it "serializes BearerBody to \"body\"" $ do
                let encoded = encode BearerBody
                decode encoded `shouldBe` Just (Aeson.String "body")

            it "serializes BearerUri to \"query\"" $ do
                let encoded = encode BearerUri
                decode encoded `shouldBe` Just (Aeson.String "query")

        context "FromJSON instance" $ do
            it "deserializes \"header\" to BearerHeader" $ do
                let json = Aeson.String "header"
                decode (encode json) `shouldBe` Just BearerHeader

            it "deserializes \"body\" to BearerBody" $ do
                let json = Aeson.String "body"
                decode (encode json) `shouldBe` Just BearerBody

            it "deserializes \"query\" to BearerUri" $ do
                let json = Aeson.String "query"
                decode (encode json) `shouldBe` Just BearerUri

            it "rejects unknown bearer method strings" $ do
                let json = Aeson.String "invalid"
                (decode (encode json) :: Maybe BearerMethod) `shouldBe` Nothing

        context "Round-trip" $ do
            it "round-trips BearerHeader through JSON" $ do
                let original = BearerHeader
                decode (encode original) `shouldBe` Just original

            it "round-trips BearerBody through JSON" $ do
                let original = BearerBody
                decode (encode original) `shouldBe` Just original

            it "round-trips BearerUri through JSON" $ do
                let original = BearerUri
                decode (encode original) `shouldBe` Just original
