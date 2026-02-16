# Example MCP Server — Build & Test Skill

This skill explains how to build, run, and test the example MCP server that
demonstrates the `mcp` Haskell library.

## Prerequisites

- GHC 9.12.2 and cabal-install
- The project root is at the repository root (where `cabal.project` lives)

## Build

```bash
cabal build mcp-example
```

This compiles the example server along with its dependencies (`mcp-types` and
`mcp`).

## Run

```bash
cabal run mcp-example
```

The server starts on **http://localhost:8080/mcp** and prints a JWT bearer token
to stdout. Copy this token — you need it for all requests.

## Test with curl

Replace `$TOKEN` with the token printed by the server.

### 1. Initialize the session

```bash
curl -s -X POST http://localhost:8080/mcp \
  -H 'Content-Type: application/json' \
  -H "Authorization: Bearer $TOKEN" \
  -d '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{},"clientInfo":{"name":"curl","version":"1.0"}}}'
```

### 2. Send initialized notification

```bash
curl -s -X POST http://localhost:8080/mcp \
  -H 'Content-Type: application/json' \
  -H "Authorization: Bearer $TOKEN" \
  -d '{"jsonrpc":"2.0","method":"notifications/initialized","params":null}'
```

### 3. List tools

```bash
curl -s -X POST http://localhost:8080/mcp \
  -H 'Content-Type: application/json' \
  -H "Authorization: Bearer $TOKEN" \
  -d '{"jsonrpc":"2.0","id":2,"method":"tools/list","params":{}}'
```

### 4. Call the echo tool

```bash
curl -s -X POST http://localhost:8080/mcp \
  -H 'Content-Type: application/json' \
  -H "Authorization: Bearer $TOKEN" \
  -d '{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"echo","arguments":{"message":"Hello from MCP!"}}}'
```

### 5. Call the add tool

```bash
curl -s -X POST http://localhost:8080/mcp \
  -H 'Content-Type: application/json' \
  -H "Authorization: Bearer $TOKEN" \
  -d '{"jsonrpc":"2.0","id":4,"method":"tools/call","params":{"name":"add","arguments":{"a":17,"b":25}}}'
```

### 6. Call the current-time tool

```bash
curl -s -X POST http://localhost:8080/mcp \
  -H 'Content-Type: application/json' \
  -H "Authorization: Bearer $TOKEN" \
  -d '{"jsonrpc":"2.0","id":5,"method":"tools/call","params":{"name":"current-time","arguments":{}}}'
```

### 7. List resources

```bash
curl -s -X POST http://localhost:8080/mcp \
  -H 'Content-Type: application/json' \
  -H "Authorization: Bearer $TOKEN" \
  -d '{"jsonrpc":"2.0","id":6,"method":"resources/list","params":{}}'
```

### 8. Read a resource

```bash
curl -s -X POST http://localhost:8080/mcp \
  -H 'Content-Type: application/json' \
  -H "Authorization: Bearer $TOKEN" \
  -d '{"jsonrpc":"2.0","id":7,"method":"resources/read","params":{"uri":"resource://example/readme"}}'
```

### 9. List resource templates

```bash
curl -s -X POST http://localhost:8080/mcp \
  -H 'Content-Type: application/json' \
  -H "Authorization: Bearer $TOKEN" \
  -d '{"jsonrpc":"2.0","id":8,"method":"resources/templates/list","params":{}}'
```

### 10. Read a templated resource

```bash
curl -s -X POST http://localhost:8080/mcp \
  -H 'Content-Type: application/json' \
  -H "Authorization: Bearer $TOKEN" \
  -d '{"jsonrpc":"2.0","id":9,"method":"resources/read","params":{"uri":"resource://example/users/42"}}'
```

### 11. List prompts

```bash
curl -s -X POST http://localhost:8080/mcp \
  -H 'Content-Type: application/json' \
  -H "Authorization: Bearer $TOKEN" \
  -d '{"jsonrpc":"2.0","id":10,"method":"prompts/list","params":{}}'
```

### 12. Get a prompt

```bash
curl -s -X POST http://localhost:8080/mcp \
  -H 'Content-Type: application/json' \
  -H "Authorization: Bearer $TOKEN" \
  -d '{"jsonrpc":"2.0","id":11,"method":"prompts/get","params":{"name":"summarize","arguments":{"text":"The MCP protocol enables AI models to interact with external tools."}}}'
```

### 13. Request completions

```bash
curl -s -X POST http://localhost:8080/mcp \
  -H 'Content-Type: application/json' \
  -H "Authorization: Bearer $TOKEN" \
  -d '{"jsonrpc":"2.0","id":12,"method":"completion/complete","params":{"ref":{"type":"ref/prompt","name":"summarize"},"argument":{"name":"text","value":"Hel"}}}'
```

### 14. Set log level

```bash
curl -s -X POST http://localhost:8080/mcp \
  -H 'Content-Type: application/json' \
  -H "Authorization: Bearer $TOKEN" \
  -d '{"jsonrpc":"2.0","id":13,"method":"logging/setLevel","params":{"level":"debug"}}'
```

### 15. Ping

```bash
curl -s -X POST http://localhost:8080/mcp \
  -H 'Content-Type: application/json' \
  -H "Authorization: Bearer $TOKEN" \
  -d '{"jsonrpc":"2.0","id":14,"method":"ping","params":{}}'
```

## Available Features

| Feature              | Methods                                      |
|----------------------|----------------------------------------------|
| Tools                | `tools/list`, `tools/call`                   |
| Resources            | `resources/list`, `resources/read`           |
| Resource Templates   | `resources/templates/list`                   |
| Prompts              | `prompts/list`, `prompts/get`               |
| Completions          | `completion/complete`                        |
| Logging              | `logging/setLevel`                           |
| Lifecycle            | `initialize`, `notifications/initialized`    |
| Health               | `ping`                                       |

## Run the agent test (Claude Agent SDK)

A Node.js test in `test/` uses the Claude Agent SDK to start the server and
exercise every tool endpoint through a Claude agent.

```bash
cd mcp-server/example/test
pnpm install
pnpm test   # requires ANTHROPIC_API_KEY
```

The script starts the server, captures its JWT token, connects a Claude agent,
and verifies that all 3 tool checks pass (echo, add, current-time).
See `test/README.md` for details.

## Run the library tests

```bash
cabal test all
```

This runs the full integration test suite (42 tests) for the `mcp` library.
