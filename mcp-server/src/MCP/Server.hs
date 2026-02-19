{- |
Module:      MCP.Server
License:     MPL-2.0
Maintainer:  <matti@dpella.io>, <lobo@dpella.io>

Model Context Protocol (MCP) server implementation.

This module re-exports everything from the transport-specific modules
for backwards compatibility.  For finer-grained imports use:

* "MCP.Server.Common"     — types, state, request routing, tool helpers
* "MCP.Server.HTTP"       — HTTP transports (JWT-authenticated and unauthenticated)
* "MCP.Server.Stdio"      — stdio transport
-}
module MCP.Server (
    module MCP.Server.Common,
    module MCP.Server.HTTP,
    module MCP.Server.Stdio,
) where

import MCP.Server.Common
import MCP.Server.HTTP
import MCP.Server.Stdio
