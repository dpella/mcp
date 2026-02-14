# Repository Guidelines

This repository implements the Model Context Protocol (MCP) as two Haskell library packages using Servant. Follow these concise guidelines to contribute effectively.

## Project Structure & Module Organization
- `mcp-protocol/src/`: Pure protocol types (`MCP.Types`, `MCP.Protocol`, `MCP.Aeson`).
- `mcp/src/`: Server implementation (`MCP.Server`).
- `mcp/test/`: Tests (hspec + hspec-wai integration tests).
- Build files: `mcp-protocol/mcp-protocol.cabal`, `mcp/mcp.cabal`, `cabal.project`.
- Expose or list new modules in the appropriate `.cabal` file (`exposed-modules` or `other-modules`).

## Build, Test, and Development Commands
- `cabal build all` — build both packages and test suites.
- `cabal test all` — run all tests (integration tests included).
- `cabal repl mcp` — open a REPL for the server library.
- `cabal repl mcp-protocol` — open a REPL for the protocol library.
- Optional coverage: `cabal test --enable-coverage`.

## Coding Style & Naming Conventions
- Haskell2010 with project defaults; prefer existing extensions already enabled in the `.cabal` files.
- Indentation: 2 spaces, no tabs; keep lines ≤ 100 columns.
- Modules: `MCP.*` (e.g., `MCP.Protocol`). One top-level export list per module.
- Names: Types in `TitleCase`; functions in `camelCase`; record fields follow existing `snake_case` pattern (e.g., `mcp_server_initialized`).
- Imports: qualified where helpful; group stdlib/external/local; avoid unused imports.

## Testing Guidelines
- Frameworks: `hspec`, `hspec-wai`.
- Place tests under `mcp/test/MCP/*`; add to `other-modules` in `mcp.cabal` if needed.
- Aim to cover new endpoints/branches; prefer integration tests for protocol flows.
- Run `cabal test all` before submitting; keep tests deterministic and hermetic (no network).

## Commit & Pull Request Guidelines
- Use Conventional Commits (recommended): `feat:`, `fix:`, `refactor:`, `test:`, `docs:`.
- PRs must include: clear description, rationale, linked issues, and tests for behavior changes.
- Update the appropriate `.cabal` file when adding/renaming modules; update README/examples if API changes.
- Keep diffs focused and minimal; avoid unrelated formatting churn.

## Security & Configuration Tips
- JWT tokens are used for authentication; keep signing keys out of the repo.
- Never log secrets or tokens.

## Agent-Specific Instructions
- Keep changes surgical; do not add license headers; follow the style above for any files you touch.
- Do not introduce new dependencies or language extensions without justification and consensus.
