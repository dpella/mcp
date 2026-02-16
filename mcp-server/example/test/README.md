# Agent Test for Example MCP Server

Uses the [Claude Agent SDK](https://www.npmjs.com/package/@anthropic-ai/claude-agent-sdk)
to spin up the example MCP server and exercise every tool endpoint through a Claude agent.

## Prerequisites

- Node.js 18+
- pnpm
- `ANTHROPIC_API_KEY` environment variable set
- GHC 9.12 and cabal (to build the server)

## Setup

```bash
# From this directory (mcp-server/example/test/)
pnpm install
```

## Run

```bash
pnpm test
```

The script will:

1. Build and start the example MCP server
2. Capture the JWT token from server output
3. Connect a Claude agent to the running server via HTTP
4. Ask the agent to call every tool
5. Check the agent's output for expected results
6. Print pass/fail for each check and stop the server

## What Gets Tested

| # | Endpoint | Check |
|---|----------|-------|
| 1 | `tools/call` (echo) | Response contains "Hello MCP!" |
| 2 | `tools/call` (add) | Response contains "42" |
| 3 | `tools/call` (current-time) | Response contains a date |
