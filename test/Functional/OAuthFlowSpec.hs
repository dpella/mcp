{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
Module      : Functional.OAuthFlowSpec
Description : Functional tests for OAuth flow
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC
-}
module Functional.OAuthFlowSpec (spec) where

import Test.Hspec

import MCP.Server (MCPServer (..), MCPServerM)
import MCP.Server.OAuth.Test.Fixtures (referenceTestConfig)
import Servant.OAuth2.IDP.Test.Internal (oauthConformanceSpec)

-- | Minimal MCPServer instance for testing (uses default implementations)
instance MCPServer MCPServerM

spec :: Spec
spec = do
    config <- runIO referenceTestConfig
    oauthConformanceSpec config
