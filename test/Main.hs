{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module:      Main
Copyright:   (c) DPella AB 2025
License:     LicenseRef-AllRightsReserved
Maintainer:  <matti@dpella.io>, <lobo@dpella.io>

Main test suite for the MCP library
-}
module Main where

import MCP.Integration qualified as Integration
import Test.Hspec (describe, hspec)

-- | Main function to run all tests
main :: IO ()
main = hspec $ do
    describe "MCP Library" $ do
        Integration.integrationSpec
