#!/usr/bin/env node
/**
 * Test the example MCP server using the Claude Agent SDK.
 *
 * Builds the example server once, then exercises every tool endpoint
 * over HTTP (JWT), simple HTTP (unauthenticated), and stdio transports.
 *
 * Usage:
 *   # From the repository root:
 *   cabal build mcp-example
 *   cd mcp-server/example/test
 *   npm install
 *   npm test
 *
 * Requires:
 *   - ANTHROPIC_API_KEY environment variable set
 *   - GHC 9.12 + cabal (to build the server)
 */

import { query } from "@anthropic-ai/claude-agent-sdk";
import { execSync, spawn } from "node:child_process";
import { createServer } from "node:net";
import { existsSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import http from "node:http";

const __dirname = dirname(fileURLToPath(import.meta.url));

// ---------------------------------------------------------------------------
// Server management
// ---------------------------------------------------------------------------

function findRepoRoot() {
  let d = __dirname;
  for (let i = 0; i < 10; i++) {
    if (existsSync(join(d, "cabal.project"))) return d;
    d = dirname(d);
  }
  return null;
}

function buildServer(repoRoot) {
  console.log("  Building mcp-example (this may take a while)...");
  execSync("cabal build mcp-example", { cwd: repoRoot, stdio: "inherit" });
  console.log("  Build succeeded.");
}

function getServerBin(repoRoot) {
  return execSync("cabal list-bin mcp-example", {
    cwd: repoRoot,
    encoding: "utf-8",
  }).trim();
}

function getFreePort() {
  return new Promise((resolve, reject) => {
    const srv = createServer();
    srv.listen(0, () => {
      const { port } = srv.address();
      srv.close(() => resolve(port));
    });
    srv.on("error", reject);
  });
}

function startServer(repoRoot, port) {
  return new Promise((resolve, reject) => {
    console.log(`  repo root: ${repoRoot}`);
    console.log(`  port:      ${port}`);
    const serverBin = getServerBin(repoRoot);
    console.log(`  binary:    ${serverBin}`);

    const proc = spawn(serverBin, [], {
      cwd: repoRoot,
      stdio: ["ignore", "pipe", "pipe"],
      env: { ...process.env, PORT: String(port) },
    });

    proc.stderr.on("data", (chunk) => {
      for (const line of chunk.toString().split("\n").filter(Boolean)) {
        console.log(`  [server:stderr] ${line}`);
      }
    });

    let token = null;
    const deadline = Date.now() + 30_000;
    console.log("  Waiting for server to print JWT token...");

    let buf = "";
    proc.stdout.on("data", (chunk) => {
      buf += chunk.toString();
      const lines = buf.split("\n");
      buf = lines.pop(); // keep incomplete trailing line
      for (const raw of lines) {
        const line = raw.trim();
        console.log(`  [server:stdout] ${line}`);
        if (!token && line.startsWith("eyJ")) {
          token = line;
        }
      }
    });

    const check = setInterval(() => {
      if (token) {
        clearInterval(check);
        // Give the server a moment to finish binding.
        setTimeout(() => verifyAndResolve(), 1000);
      } else if (Date.now() > deadline) {
        clearInterval(check);
        proc.kill();
        reject(new Error("Timed out waiting for JWT token"));
      }
    }, 200);

    function verifyAndResolve() {
      console.log("  Verifying server is reachable...");
      const body = JSON.stringify({
        jsonrpc: "2.0",
        id: 1,
        method: "ping",
      });
      const req = http.request(
        {
          hostname: "localhost",
          port,
          path: "/mcp",
          method: "POST",
          headers: {
            "Content-Type": "application/json",
            Authorization: `Bearer ${token}`,
          },
        },
        (res) => {
          let data = "";
          res.on("data", (c) => (data += c));
          res.on("end", () => {
            console.log(`  Server responded: ${data.slice(0, 200)}`);
            resolve({ proc, token });
          });
        },
      );
      req.on("error", (e) => {
        proc.kill();
        reject(new Error(`Server not reachable on port ${port}: ${e.message}`));
      });
      req.end(body);
    }

    proc.on("error", (err) => {
      clearInterval(check);
      reject(err);
    });
    proc.on("exit", (code) => {
      if (!token) {
        clearInterval(check);
        reject(new Error(`Server exited with code ${code} before printing token`));
      }
    });
  });
}

function startSimpleHTTPServer(repoRoot, port) {
  return new Promise((resolve, reject) => {
    console.log(`  repo root: ${repoRoot}`);
    console.log(`  port:      ${port}`);
    const serverBin = getServerBin(repoRoot);
    console.log(`  binary:    ${serverBin}`);

    const proc = spawn(serverBin, ["--simple-http"], {
      cwd: repoRoot,
      stdio: ["ignore", "pipe", "pipe"],
      env: { ...process.env, PORT: String(port) },
    });

    proc.stderr.on("data", (chunk) => {
      for (const line of chunk.toString().split("\n").filter(Boolean)) {
        console.log(`  [server:stderr] ${line}`);
      }
    });

    let ready = false;
    const deadline = Date.now() + 30_000;
    console.log("  Waiting for simple HTTP server to start...");

    let buf = "";
    proc.stdout.on("data", (chunk) => {
      buf += chunk.toString();
      const lines = buf.split("\n");
      buf = lines.pop(); // keep incomplete trailing line
      for (const raw of lines) {
        const line = raw.trim();
        console.log(`  [server:stdout] ${line}`);
        if (!ready && line.includes("Listening on")) {
          ready = true;
        }
      }
    });

    const check = setInterval(() => {
      if (ready) {
        clearInterval(check);
        // Give the server a moment to finish binding.
        setTimeout(() => verifyAndResolve(), 1000);
      } else if (Date.now() > deadline) {
        clearInterval(check);
        proc.kill();
        reject(new Error("Timed out waiting for simple HTTP server"));
      }
    }, 200);

    function verifyAndResolve() {
      console.log("  Verifying server is reachable...");
      const body = JSON.stringify({
        jsonrpc: "2.0",
        id: 1,
        method: "ping",
      });
      const req = http.request(
        {
          hostname: "localhost",
          port,
          path: "/mcp",
          method: "POST",
          headers: {
            "Content-Type": "application/json",
          },
        },
        (res) => {
          let data = "";
          res.on("data", (c) => (data += c));
          res.on("end", () => {
            console.log(`  Server responded: ${data.slice(0, 200)}`);
            resolve({ proc });
          });
        },
      );
      req.on("error", (e) => {
        proc.kill();
        reject(new Error(`Server not reachable on port ${port}: ${e.message}`));
      });
      req.end(body);
    }

    proc.on("error", (err) => {
      clearInterval(check);
      reject(err);
    });
    proc.on("exit", (code) => {
      if (!ready) {
        clearInterval(check);
        reject(new Error(`Server exited with code ${code} before becoming ready`));
      }
    });
  });
}

function stopServer(proc) {
  proc.kill("SIGTERM");
  setTimeout(() => proc.kill("SIGKILL"), 5000);
}

// ---------------------------------------------------------------------------
// Agent runner
// ---------------------------------------------------------------------------

const TEST_PROMPT = `\
You MUST call these MCP tools directly as tool calls (do NOT use Bash or any other tool):

1. Call mcp__mcp-example__echo with {"message": "Hello MCP!"} — report the returned text.
2. Call mcp__mcp-example__add with {"a": 17, "b": 25} — report the numeric result.
3. Call mcp__mcp-example__current-time with {} — report the returned time string.

After all three tool calls, print "ALL TESTS DONE".
`;

const AGENT_TIMEOUT_MS = 120_000;

async function runAgent(mcpServers) {
  const fullText = [];

  console.log("  Starting agent (connecting to MCP server + API)...");

  const controller = new AbortController();
  const timer = setTimeout(() => {
    console.log(`  ERROR: Agent timed out after ${AGENT_TIMEOUT_MS / 1000}s`);
    controller.abort();
  }, AGENT_TIMEOUT_MS);

  try {
    for await (const message of query({
      prompt: TEST_PROMPT,
      options: {
        mcpServers,
        allowedTools: ["mcp__mcp-example__*"],
        disallowedTools: [
          "Bash",
          "Read",
          "Edit",
          "Write",
          "Glob",
          "Grep",
          "WebFetch",
          "WebSearch",
          "NotebookEdit",
          "TodoWrite",
          "Task",
        ],
        permissionMode: "bypassPermissions",
        allowDangerouslySkipPermissions: true,
        maxTurns: 10,
        settingSources: [],
        abortController: controller,
      },
    })) {
      if (message.type === "system" && message.subtype === "init") {
        console.log(
          `  [system:init] model=${message.model} tools=${JSON.stringify(message.tools)} mcp=${JSON.stringify(message.mcp_servers)}`,
        );
      } else if (message.type === "assistant") {
        for (const block of message.message.content) {
          if ("text" in block) {
            console.log(`  [assistant] ${block.text.slice(0, 200)}`);
            fullText.push(block.text);
          } else if ("name" in block) {
            console.log(
              `  [tool_use] ${block.name}(${JSON.stringify(block.input).slice(0, 200)})`,
            );
          }
        }
      } else if (message.type === "user") {
        const content = message.message?.content;
        if (Array.isArray(content)) {
          for (const block of content) {
            if (block.type === "tool_result") {
              const text =
                typeof block.content === "string"
                  ? block.content
                  : JSON.stringify(block.content);
              console.log(`  [tool_result] ${text?.slice(0, 200)}`);
              if (text) fullText.push(text);
            }
          }
        }
      } else if (message.type === "result") {
        console.log(
          `  [result] subtype=${message.subtype} turns=${message.num_turns} cost=$${message.total_cost_usd}`,
        );
        if (message.subtype !== "success") {
          console.log(`  [result] errors: ${message.errors}`);
        }
      }
    }
  } catch (err) {
    if (err.name === "AbortError") {
      console.log("  Agent aborted (timeout).");
    } else {
      console.log(`  ERROR in query(): ${err.message}`);
    }
  } finally {
    clearTimeout(timer);
  }

  return fullText.join("\n");
}

// ---------------------------------------------------------------------------
// Result checking
// ---------------------------------------------------------------------------

const CHECKS = [
  ["echo tool", (t) => t.includes("Hello MCP!")],
  ["add tool", (t) => t.includes("42")],
  ["current-time tool", (t) => /\d{4}-\d{2}-\d{2}/.test(t)],
];

function evaluate(output) {
  let passed = 0;
  let failed = 0;
  for (const [name, check] of CHECKS) {
    if (check(output)) {
      console.log(`  PASS: ${name}`);
      passed++;
    } else {
      console.log(`  FAIL: ${name}`);
      failed++;
    }
  }
  return { passed, failed };
}

// ---------------------------------------------------------------------------
// Transport tests
// ---------------------------------------------------------------------------

async function testHTTP(repoRoot) {
  const port = await getFreePort();
  console.log(`Starting MCP example server on port ${port}...`);
  const { proc, token } = await startServer(repoRoot, port);
  console.log(`Server started (token: ${token.slice(0, 20)}...)`);

  try {
    console.log("\nRunning HTTP agent tests...");
    const output = await runAgent({
      "mcp-example": {
        type: "http",
        url: `http://localhost:${port}/mcp`,
        headers: {
          Authorization: `Bearer ${token}`,
        },
      },
    });

    console.log("\n--- HTTP agent output ---");
    console.log(output);
    console.log("--- End HTTP agent output ---\n");

    console.log("HTTP results:");
    return evaluate(output);
  } finally {
    console.log("\nStopping HTTP server...");
    stopServer(proc);
  }
}

async function testSimpleHTTP(repoRoot) {
  const port = await getFreePort();
  console.log(`Starting simple HTTP MCP server on port ${port}...`);
  const { proc } = await startSimpleHTTPServer(repoRoot, port);
  console.log("Server started (no auth).");

  try {
    console.log("\nRunning simple HTTP agent tests...");
    const output = await runAgent({
      "mcp-example": {
        type: "http",
        url: `http://localhost:${port}/mcp`,
      },
    });

    console.log("\n--- Simple HTTP agent output ---");
    console.log(output);
    console.log("--- End simple HTTP agent output ---\n");

    console.log("Simple HTTP results:");
    return evaluate(output);
  } finally {
    console.log("\nStopping simple HTTP server...");
    stopServer(proc);
  }
}

async function testStdio(serverBin) {
  console.log("\nRunning stdio agent tests...");
  console.log(`  binary: ${serverBin}`);

  const output = await runAgent({
    "mcp-example": {
      type: "stdio",
      command: serverBin,
      args: ["--stdio"],
    },
  });

  console.log("\n--- Stdio agent output ---");
  console.log(output);
  console.log("--- End stdio agent output ---\n");

  console.log("Stdio results:");
  return evaluate(output);
}

// ---------------------------------------------------------------------------
// Main
// ---------------------------------------------------------------------------

async function main() {
  if (!process.env.ANTHROPIC_API_KEY) {
    console.error("Error: ANTHROPIC_API_KEY environment variable not set");
    process.exit(1);
  }

  const repoRoot = findRepoRoot();
  if (!repoRoot) {
    console.error("Error: could not find repository root (cabal.project)");
    process.exit(1);
  }

  // Build once, reuse for all transports.
  buildServer(repoRoot);
  const serverBin = getServerBin(repoRoot);

  let totalPassed = 0;
  let totalFailed = 0;

  // --- HTTP ---
  {
    const { passed, failed } = await testHTTP(repoRoot);
    totalPassed += passed;
    totalFailed += failed;
  }

  // --- Simple HTTP ---
  {
    const { passed, failed } = await testSimpleHTTP(repoRoot);
    totalPassed += passed;
    totalFailed += failed;
  }

  // --- Stdio ---
  {
    const { passed, failed } = await testStdio(serverBin);
    totalPassed += passed;
    totalFailed += failed;
  }

  console.log(`\nOverall: ${totalPassed}/${totalPassed + totalFailed} checks passed`);
  if (totalFailed > 0) process.exit(1);
  console.log("Done.");
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
