{- |
Module:      MCP.Aeson
Copyright:   (c) DPella AB 2025
License:     MPL-2.0
Maintainer:  <matti@dpella.io>, <lobo@dpella.io>
-}
module MCP.Aeson where

import Data.Aeson
import Data.List (stripPrefix)

-- | Custom Aeson parsing options for MCP types.
mcpParseOpts :: Options
mcpParseOpts =
    defaultOptions
        { omitNothingFields = True
        , fieldLabelModifier = flm
        }
  where
    -- Custom field label modifier to handle "_meta" field
    flm :: String -> String
    flm "_meta" = "_meta"
    -- Handle duplicate record fields
    flm fl
        | Just with_sel <- stripPrefix "$sel:" fl =
            takeWhile (/= ':') with_sel
    flm fl = fl
