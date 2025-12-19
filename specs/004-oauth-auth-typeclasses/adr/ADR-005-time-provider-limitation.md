# ADR-005: Time Provider Architecture Limitation

## Status
Accepted

## Context
AppM's OAuthStateStore instance duplicates expiry logic from Store/InMemory.hs. This appears to violate the three-layer cake pattern (deep modules should hide implementation).

The duplicated methods are:
- `lookupAuthCode` (lines 365-377 in AppEnv.hs)
- `consumeAuthCode` (lines 383-399 in AppEnv.hs)
- `lookupPendingAuth` (lines 441-455 in AppEnv.hs)

Each method contains identical expiry-checking logic to the InMemory implementation, but runs it in the AppM context instead of delegating via `runReaderT`.

## Problem
The duplication exists due to a MonadTime context resolution issue:

- AppM implements MonadTime via `envTimeProvider :: Maybe (TVar UTCTime)` (line 341-346 in AppEnv.hs)
- This allows tests to control time by advancing the TVar
- InMemory methods use `currentTime` from MonadTime for expiry checks
- If AppM delegated to InMemory via `runReaderT ... oauthEnv`, the ReaderT context would be `ReaderT OAuthTVarEnv IO`
- In that context, MonadTime resolves to IO's instance (real system time), NOT AppM's test time provider
- This would break test time control completely - tests could not advance time to test expiry

The issue is fundamental to how ReaderT stacks compose: each layer has its own MonadTime instance, and `runReaderT` changes the context to the inner monad.

## Decision
Accept the code duplication as intentional and document it clearly.

Options considered:

1. **Pass time explicitly to all lookup operations**
   - Rejected: Would require changing OAuthStateStore API signatures
   - Breaking change for all implementers
   - Makes API less composable (forces caller to track time)

2. **Document the limitation** (chosen)
   - Pragmatic solution that preserves test time control
   - Clear documentation prevents future confusion
   - Explicit maintenance burden (must sync changes manually)

3. **Remove MonadTime from OAuthStateStore entirely**
   - Rejected: Too radical, loses composability
   - Would force all implementations to use real time
   - Defeats the purpose of typeclass abstraction

4. **Custom ReaderT wrapper that preserves MonadTime**
   - Rejected: Complex, obscures the actual issue
   - Adds another layer of indirection
   - Still requires manual synchronization of logic

## Consequences

### Positive
- Test time control works correctly
- Tests can advance time via TVar to test expiry scenarios
- MonadTime abstraction preserved
- No breaking changes to OAuthStateStore API

### Negative
- Code duplication exists between AppEnv.hs and InMemory.hs
- Manual synchronization required when InMemory logic changes
- Maintenance burden: must update both implementations

### Mitigations
- Comprehensive documentation in AppEnv.hs (lines 355-392)
- Reference to this ADR from code comments
- Clear explanation of why duplication is necessary
- Future implementers of OAuthStateStore are warned about MonadTime context

## Related
- **Bead**: mcp-2z6.9 (AppM OAuthStateStore duplication is intentional)
- **Affected files**:
  - `src/MCP/Server/HTTP/AppEnv.hs` (lines 349-459)
  - `src/Servant/OAuth2/IDP/Store/InMemory.hs` (original implementation)

## Implementation Notes

The duplicated expiry logic must remain synchronized with InMemory.hs:

```haskell
-- Pattern: Check expiry before returning
now <- currentTime
if now >= expiryTime
  then pure Nothing
  else pure (Just value)
```

When changing expiry logic in either file, update both:
1. InMemory.hs (canonical implementation)
2. AppEnv.hs (AppM instance methods)

Tests verify this synchronization by running the same test suite against both implementations.
