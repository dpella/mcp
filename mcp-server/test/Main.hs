{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module:      Main
License:     MPL-2.0
Maintainer:  <matti@dpella.io>, <lobo@dpella.io>

Main test suite for the MCP library
-}
module Main where

import MCP.Integration qualified as Integration
import MCP.StdioIntegration qualified as StdioIntegration
import Test.Hspec (describe, hspec)

-- | Main function to run all tests
main :: IO ()
main = hspec $ do
    describe "MCP Library" $ do
        Integration.integrationSpec
        StdioIntegration.stdioIntegrationSpec
