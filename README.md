![image](https://github.com/user-attachments/assets/69dc29d5-5228-467c-8288-16c18192f986)

# MCP-Haskell

A complete implementation of the [Model Context Protocol (MCP)](https://modelcontextprotocol.io/) for Haskell.

Available on [Hackage](https://hackage.haskell.org/package/mcp)

## Overview

This project provides a type-safe, comprehensive implementation of the Model Context Protocol in Haskell. MCP is an open protocol that standardizes how applications provide context to Large Language Models (LLMs), enabling AI models to securely connect to data sources and tools.

## Features

- **Latest Protocol Support**: Implements MCP protocol version 2025-06-18 with full compatibility
- **Complete MCP Protocol Implementation**: All MCP message types, requests, responses, and notifications
- **Type-Safe Design**: Full Haskell type system integration with automatic JSON serialization
- **Multiple Transport Options**: Both StdIO and HTTP transport support
- **MCP Transport Compliance**: HTTP implementation follows the official MCP transport specification
- **Production-Ready HTTP Server**: Configurable OAuth, timing, and security parameters
- **Extensible Server Interface**: Clean typeclass-based API for implementing custom servers
- **Working Example Server**: Demonstrates basic MCP functionality

## Architecture

The implementation is organized into five main modules:

### `MCP.Types`

- Core MCP data types (Content, Resource, Tool, Prompt, etc.)
- Automatic JSON serialization/deserialization via Aeson
- Type-safe mapping of the complete MCP schema

### `MCP.Protocol`

- JSON-RPC message wrappers
- All client and server request/response types
- Notification types for bidirectional communication
- Union types for organizing related messages

### `MCP.Server`

- Core server infrastructure with `MCPServerM` monad stack
- `MCPServer` typeclass for implementing custom servers
- Shared types and utilities for both transport methods

### `MCP.Server.StdIO`

- StdIO transport implementation
- JSON-RPC communication over stdin/stdout
- Suitable for process-based MCP clients

### `MCP.Server.HTTP`

- HTTP transport implementation following MCP specification
- RESTful JSON-RPC API at `/mcp` endpoint
- Built with Servant and Warp for production use
- OAuth 2.0 authentication support (optional)

## Quick Start

### Building

```bash
cabal build
```

### Running the Example Server

**StdIO Mode (default):**

```bash
cabal run mcp
```

The server will start and listen for MCP messages on stdin, responding on stdout.

**HTTP Mode:**
Create a simple HTTP server runner:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import MCP.Server.HTTP
import MCP.Types

main :: IO ()
main = do
  let serverInfo = Implementation "mcp-haskell-http" "0.1.0"
  let capabilities = ServerCapabilities
        { resources = Just $ ResourcesCapability Nothing Nothing
        , tools = Just $ ToolsCapability Nothing
        , prompts = Just $ PromptsCapability Nothing
        , completions = Nothing
        , logging = Nothing
        , experimental = Nothing
        }
  let config = HTTPServerConfig
        { httpPort = 8080
        , httpBaseUrl = "http://localhost:8080"  -- Configure base URL
        , httpServerInfo = serverInfo
        , httpCapabilities = capabilities
        , httpEnableLogging = False
        , httpMCPOAuthConfig = Nothing  -- No OAuth
        , httpJWK = Nothing  -- Auto-generated
        , httpProtocolVersion = "2025-06-18"  -- MCP protocol version
        , httpProtectedResourceMetadata = Nothing
        }
  runServerHTTP config
```

Then compile and run:

```bash
cabal build mcp-http
cabal run mcp-http
```

Or compile directly:

```bash
ghc -o mcp-http MyHTTPServer.hs
./mcp-http
```

The HTTP server will start on port 8080 with the MCP endpoint available at `POST /mcp`.

### OAuth Authentication (HTTP Transport)

The HTTP transport supports MCP-compliant OAuth 2.1 authentication with mandatory PKCE:

**MCP OAuth Requirements:**

- **PKCE Required**: All OAuth flows MUST use PKCE (Proof Key for Code Exchange)
- **HTTPS Required**: OAuth endpoints must use HTTPS in production
- **Grant Types**: Supports Authorization Code (user flows) and Client Credentials (app-to-app)
- **Token Format**: Bearer tokens in Authorization header only (never in query strings)

**OAuth Endpoints:**

- **Metadata Discovery**: `/.well-known/oauth-authorization-server` - Returns OAuth server metadata
- **Dynamic Registration**: `/register` - Allows clients to register dynamically
- **Authorization**: `/authorize` - Initiates authorization flow with PKCE
- **Token Exchange**: `/token` - Exchanges authorization code for access token

**Token Format**: The server generates proper JWT tokens using servant-auth-server, preventing authentication loops that can occur with simple UUID-based tokens.

**Complete OAuth Flow:**

1. **Discovery**: Client discovers OAuth metadata

   ```bash
   curl http://localhost:8080/.well-known/oauth-authorization-server
   ```

2. **Registration**: Client registers dynamically

   ```bash
   curl -X POST http://localhost:8080/register \
     -H "Content-Type: application/json" \
     -d '{
       "client_name": "My MCP Client",
       "redirect_uris": ["http://localhost:3000/callback"],
       "grant_types": ["authorization_code", "refresh_token"],
       "response_types": ["code"],
       "token_endpoint_auth_method": "none"
     }'
   ```

3. **Authorization**: User authorizes with PKCE

   ```text
   http://localhost:8080/authorize?
     response_type=code&
     client_id=CLIENT_ID&
     redirect_uri=http://localhost:3000/callback&
     code_challenge=CHALLENGE&
     code_challenge_method=S256&
     scope=mcp:read%20mcp:write
   ```

4. **Token Exchange**: Exchange code for token

   ```bash
   curl -X POST http://localhost:8080/token \
     -H "Content-Type: application/x-www-form-urlencoded" \
     -d "grant_type=authorization_code&code=AUTH_CODE&code_verifier=VERIFIER"
   ```

5. **API Access**: Use token for authenticated requests

   ```bash
   curl -X POST http://localhost:8080/mcp \
     -H "Authorization: Bearer ACCESS_TOKEN" \
     -H "Content-Type: application/json" \
     -d '{"jsonrpc":"2.0","id":1,"method":"ping"}'
   ```

**Error Responses (MCP-compliant):**

- `401 Unauthorized`: Invalid or expired token
- `403 Forbidden`: Insufficient permissions
- `400 Bad Request`: Malformed request

### Implementing a Custom Server

The same server implementation works for both transport methods:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import MCP.Server
import MCP.Server.StdIO  -- For StdIO transport
import MCP.Server.HTTP   -- For HTTP transport
import MCP.Types
import MCP.Protocol

instance MCPServer MCPServerM where
  handleListTools _params = do
    let tool = Tool
          { name = "calculator"
          , description = Just "A simple calculator"
          , inputSchema = -- JSON schema for tool parameters
          , annotations = Nothing
          }
    return $ ListToolsResult [tool] Nothing
    
  handleCallTool params = do
    -- Implement your tool logic here
    let result = TextContentType $ TextContent "Result: 42"
    return $ CallToolResult [result] Nothing
    
  -- Implement other required methods...

-- StdIO version
runStdIO :: IO ()
runStdIO = do
  let config = ServerConfig
        { configInput = stdin
        , configOutput = stdout
        , configServerInfo = Implementation "my-server" "1.0.0"
        , configCapabilities = serverCapabilities
        }
  MCP.Server.StdIO.runServer config

-- HTTP version  
runHTTP :: IO ()
runHTTP = do
  let config = HTTPServerConfig
        { httpPort = 8080
        , httpBaseUrl = "http://localhost:8080"
        , httpServerInfo = Implementation "my-server" "1.0.0"
        , httpCapabilities = serverCapabilities
        , httpEnableLogging = False
        , httpMCPOAuthConfig = Nothing  -- or Just mcpOAuthConfig for OAuth
        , httpJWK = Nothing
        , httpProtocolVersion = "2025-06-18"
        , httpProtectedResourceMetadata = Nothing
        }
  runServerHTTP config
```

## MCP Protocol Support

This implementation provides complete support for MCP protocol version **2025-06-18** with full schema compliance:

### ✅ Implemented Features

- **Initialization**: Protocol version negotiation and capability exchange with server instructions
- **Resources**: Expose data and content that can be read by LLMs with enhanced metadata
- **Resource Links**: Reference resources in prompts and tool results without embedding content
- **Tools**: Functions that LLMs can execute with input/output schema validation
- **Prompts**: Pre-written prompt templates with arguments and rich content blocks
- **Completion**: Auto-completion for resource URIs, prompt arguments, etc. with context support
- **Sampling**: LLM sampling requests with model preferences and content restrictions
- **Elicitation**: Interactive user input forms with primitive schema validation
- **Logging**: Configurable logging levels with structured data
- **Notifications**: Bidirectional event notifications for real-time updates

### 🆕 New in 2025-06-18

- **Enhanced Content System**: ContentBlock type supporting text, image, audio, embedded resources, and resource links
- **Schema Validation**: Tool input/output schemas and elicitation form schemas
- **Metadata Fields**: Comprehensive _meta field support with lastModified timestamps
- **Base Metadata**: Consistent name/title distinction across all types
- **Sampling Restrictions**: SamplingMessage limited to text, image, and audio content only
- **Context-Aware Completions**: Additional context parameters for better autocompletion

### Core Operations

| Operation | Description | Status |
|-----------|-------------|--------|
| `initialize` | Start session and negotiate capabilities | ✅ |
| `ping` | Health check | ✅ |
| `resources/list` | List available resources | ✅ |
| `resources/templates/list` | List available resource templates | ✅ |
| `resources/read` | Read resource contents | ✅ |
| `resources/subscribe` | Subscribe to resource updates | ✅ |
| `resources/unsubscribe` | Unsubscribe from resource updates | ✅ |
| `prompts/list` | List available prompts | ✅ |
| `prompts/get` | Get prompt with arguments | ✅ |
| `tools/list` | List available tools | ✅ |
| `tools/call` | Execute a tool | ✅ |
| `completion/complete` | Auto-completion with context | ✅ |
| `logging/setLevel` | Set logging level | ✅ |
| `sampling/createMessage` | Request LLM sampling | ✅ |
| `roots/list` | List client root directories | ✅ |
| `elicitation/create` | Request user input via forms | ✅ |

## Project Structure

```text
src/
├── MCP/
│   ├── Types.hs          # Core MCP data types
│   ├── Protocol.hs       # JSON-RPC protocol messages
│   ├── Server.hs         # Core server infrastructure
│   ├── Server/
│   │   ├── StdIO.hs      # StdIO transport implementation
│   │   ├── HTTP.hs       # HTTP transport implementation
│   │   ├── HTTP/
│   │   │   └── AppEnv.hs # Composite environment and error types
│   │   └── Auth.hs       # MCP-specific OAuth configuration
│   └── Trace/
│       └── HTTP.hs       # HTTP tracing types
├── Servant/
│   └── OAuth2/
│       └── IDP/          # Reusable OAuth 2.1 library (MCP-independent)
│           ├── Types.hs      # Core domain newtypes
│           ├── Config.hs     # OAuthEnv configuration
│           ├── Errors.hs     # Error types and conversions
│           ├── PKCE.hs       # RFC 7636 PKCE implementation
│           ├── Metadata.hs   # RFC 8414/9728 metadata types
│           ├── Store.hs      # OAuthStateStore typeclass
│           ├── Trace.hs      # OAuthTrace ADT
│           ├── Server.hs     # OAuth API composition
│           └── Handlers/     # OAuth endpoint handlers

app/
└── Main.hs               # Example MCP server (StdIO mode)

examples/
└── http-server.hs        # HTTP server example with OAuth
```

## Development

### Dependencies

**Core Dependencies:**

- **aeson**: JSON serialization/deserialization
- **text**: Text processing
- **containers**: Map and other container types
- **bytestring**: Binary data handling
- **mtl/transformers**: Monad transformers for MCPServerM

**HTTP Server Dependencies:**

- **warp**: High-performance HTTP server
- **servant-server**: Type-safe web API framework  
- **wai**: Web application interface
- **http-types**: HTTP status codes and headers
- **servant-auth**: JWT authentication for Servant
- **servant-auth-server**: Server-side JWT implementation
- **jose**: JSON Object Signing and Encryption
- **cryptonite**: Cryptographic primitives for PKCE
- **base64-bytestring**: Base64 encoding for PKCE challenges

### Testing with MCP Clients

**StdIO Transport (for process-based clients):**

1. Build the server: `cabal build`
2. Configure your MCP client to use: `cabal run mcp`
3. The server communicates via JSON-RPC over stdin/stdout

**HTTP Transport (for web-based clients):**

1. Build and run HTTP server: `runServerHTTP config`
2. Client connects to `POST http://localhost:8080/mcp`
3. Send JSON-RPC requests with `Content-Type: application/json`
4. Supports both single responses and future SSE streaming

### Example MCP Client Configuration

**Claude Desktop:**

For Claude Desktop, add to your config file (location varies by OS):

```json
{
  "mcpServers": {
    "haskell-mcp": {
      "command": "cabal",
      "args": ["run", "mcp"],
      "cwd": "/absolute/path/to/mcp-haskell"
    }
  }
}
```

*Configuration file locations:*

- **macOS**: `~/Library/Application Support/Claude/claude_desktop_config.json`
- **Windows**: `%APPDATA%\Claude\claude_desktop_config.json`
- **Linux**: `~/.config/claude/claude_desktop_config.json`

**Cursor:**

In Cursor's menu, navigate to the "Tools and Integrations" section of Cursor's settings. The "MCP Tools" subsection has a "New MCP Server" button. Add your config, with a few more args than for Clude Desktop:

```json
{
  "mcpServers": {
    "haskell-stdio-mcp": {
      "command": "cabal",
      "args": ["run", "--project-dir", "/absolute/path/to/mcp-haskell", "-v0", "mcp-stdio", "--", "--log"],
      "cwd": "/absolute/path/to/mcp-haskell",
      "env": {
        "GHC_ENVIRONMENT": "-"
      }
    }
  }
}
```

Note that we have to specify the cabal project directory, despite it being the same as the `cwd`; at least, we couldn't get Cursor to `cabal run` the stdio server otherwise.

If this works you should see the tools for your MCP server listed under the "MCP Tools" subsection, as pictured below:
![Screenshot of the MCP Tools subsection of the Cursor Settings page, showing tools listed for two example Haskell MCP Servers](.assets/cursor-mcp-tools-configured.jpeg)

At the time of writing, Cursor supports text-based MCP servers only, but http-based server support is anticipated.

----
See the [`examples/`](examples/) directory for more detailed configuration examples and setup instructions.

## Implementation Notes

### Type Safety

The implementation leverages Haskell's type system to ensure protocol compliance:

- All MCP message types are statically typed
- JSON schema validation through type structure
- Compile-time guarantees for message format correctness

### Content Types

MCP 2025-06-18 supports rich content blocks with proper type wrappers:

```haskell
-- Text content
TextContentType $ TextContent { textType = "text", text = "Hello world!", annotations = Nothing, _meta = Nothing }

-- Image content  
ImageContentType $ ImageContent { imageType = "image", data' = "data:image/png;base64,iVBOR...", mimeType = "image/png", annotations = Nothing, _meta = Nothing }

-- Resource link (new in 2025-06-18)
ResourceLinkType $ ResourceLink { resourceLinkType = "resource_link", uri = "file:///example.txt", name = "example", title = Just "Example File", description = Just "Sample text file", mimeType = Just "text/plain", size = Just 1024, annotations = Nothing, _meta = Nothing }

-- Embedded resource content
EmbeddedResourceType $ EmbeddedResource { resourceType = "resource", resource = TextResource $ TextResourceContents { uri = "file:///example.txt", text = "content", mimeType = Just "text/plain", _meta = Nothing }, annotations = Nothing, _meta = Nothing }

-- Sampling content (restricted for LLM APIs)
SamplingTextContent $ TextContent { textType = "text", text = "Hello LLM!", annotations = Nothing, _meta = Nothing }
```

### Error Handling

The server includes comprehensive error handling:

- JSON parsing errors
- Invalid method names
- Missing or malformed parameters
- Server initialization requirements

## Contributing

This is the first known implementation of MCP for Haskell, now fully compliant with the latest MCP protocol version 2025-06-18. Contributions are welcome!

Areas for improvement:

- Server-Sent Events (SSE) support for HTTP transport
- WebSocket transport implementation
- More comprehensive example servers demonstrating new features (elicitation, resource links, etc.)
- Performance optimizations for large-scale deployments
- Enhanced error messages with better debugging information
- Additional documentation and tutorials covering new 2025-06-18 features
- Integration examples with popular Haskell web frameworks
- Benchmarking and profiling tools

## License

MIT License - see LICENSE file for details.

## References

- [Model Context Protocol Specification](https://modelcontextprotocol.io/)
- [MCP TypeScript SDK](https://github.com/modelcontextprotocol/typescript-sdk)
- [Official MCP Documentation](https://modelcontextprotocol.io/introduction)
