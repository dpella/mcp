{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Main
Description : Test suite entry point
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC
-}
module Main (main) where

import Test.Hspec

-- Law tests (now in servant-oauth2-idp-test)

-- Existing specs
import Trace.FilterSpec qualified as FilterSpec
import Trace.GoldenSpec qualified as GoldenSpec
import Trace.RenderSpec qualified as RenderSpec

-- Unit tests
import MCP.Server.OAuth.TypesSpec qualified as TypesSpec

-- OAuth App tests
import MCP.Server.OAuth.AppSpec qualified as AppSpec

-- HTTP endpoint tests
import MCP.Server.HTTP.AppEnvSpec qualified as AppEnvSpec
import MCP.Server.HTTP.McpAuthSpec qualified as McpAuthSpec

-- MCP.Server.Auth tests
import MCP.Server.AuthSpec qualified as AuthSpec

-- Functional tests
import Functional.OAuthFlowSpec qualified as OAuthFlowSpec

-- Security tests
import Security.SessionCookieSpec qualified as SessionCookieSpec

-- Servant OAuth2 IDP tests (now in servant-oauth2-idp-test)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    FilterSpec.spec
    GoldenSpec.spec
    RenderSpec.spec

    -- Unit tests
    TypesSpec.spec

    -- OAuth App tests
    AppSpec.spec

    -- HTTP endpoint tests
    AppEnvSpec.spec
    McpAuthSpec.spec

    -- MCP.Server.Auth tests
    AuthSpec.spec

    -- Functional tests
    describe "Functional" OAuthFlowSpec.spec

    -- Security tests
    describe "Security" SessionCookieSpec.spec
