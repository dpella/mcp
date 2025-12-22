<!--
SYNC IMPACT REPORT
===================
Version: 1.0.2 (smart constructor hygiene)
Changes:
- Initial constitution creation from typed functional design skills
- 6 core principles distilled from: typed-domain-modeling, haskell-design,
  architecture-patterns, testing-strategies, code-review-standards, error-handling-strategies
- 1.0.1: Added domain-centric type naming convention (types denote what they ARE, not field names)
- 1.0.2: Added smart constructor export rule (MUST NOT export type constructors, use pattern synonyms for matching)
Added sections:
- Core Principles (6 principles)
- Development Standards
- Quality Gates
- Governance
Templates requiring updates:
- .specify/templates/plan-template.md - ✅ UPDATED (Constitution Check section now references all 6 principles)
- .specify/templates/spec-template.md - ✅ (no principle references needed)
- .specify/templates/tasks-template.md - ✅ (compatible with principle-driven task types)
Follow-up TODOs: None
-->

# MCP-Haskell Constitution

## Core Principles

### I. Type-Driven Design

Every design decision MUST be reflected in the type system. Types are not documentation—they
are compile-time proofs of correctness.

**Requirements:**
- Types MUST be designed before implementation begins
- Domain concepts MUST have explicit types (no primitive obsession)
- Illegal states MUST be unrepresentable through algebraic data types
- Smart constructors MUST enforce invariants at construction time
- Parse at boundaries, don't validate repeatedly

**Rationale:** Well-typed programs eliminate entire categories of runtime errors. The type
system is the primary tool for encoding business rules and design constraints.

### II. Deep Module Architecture

Modules MUST hide complexity behind simple interfaces. The ratio of interface simplicity to
implementation complexity determines module quality.

**Requirements:**
- Public interfaces MUST be minimal—export only what users need
- Implementation details MUST be hidden through module exports and opaque types
- Smart constructors MUST be the only way to create validated domain types
- Type constructors for smart-constructor types MUST NOT be exported—not even for tests
  - Export pattern: `module Foo (FooType, mkFooType, unFooType)` — never `FooType(..)`
  - If pattern matching is needed, export pattern synonyms instead of raw constructors
  - This allows proving correct construction by construction
- Common cases MUST be simple; rare cases MAY be harder
- Complexity MUST be pulled downward into implementations, not pushed to callers

**Rationale:** Deep modules reduce cognitive load across the codebase. One module handling
complexity beats forcing all consumers to understand it.

### III. Denotational Semantics

Every function SHOULD have a clear mathematical meaning. Laws and properties MUST be
documented and tested.

**Requirements:**
- Core operations SHOULD have documented semantic specifications
- Mathematical laws (identity, associativity, etc.) MUST be tested via properties
- Type class instances MUST satisfy their laws
- Semantics MUST be defined before implementation when designing new abstractions

**Rationale:** Mathematical precision eliminates ambiguity and enables property-based
testing. Clear semantics make code easier to reason about and compose.

### IV. Total Functions and Railway Programming

Functions MUST handle all possible inputs explicitly. Partial functions are prohibited in
public APIs.

**Requirements:**
- Public functions MUST NOT use partial operations (head, tail, fromJust, etc.)
- Expected errors MUST be encoded in return types (Either, Maybe, Result)
- Error handling MUST compose via monadic/applicative operations
- Exceptions SHOULD be reserved for truly exceptional circumstances
- Error types MUST be specific and actionable, not stringly-typed

**Rationale:** Total functions make all outcomes explicit. Railway-oriented programming
provides composable error handling without hidden control flow.

### V. Pure Core, Impure Shell

Business logic MUST be pure. Effects MUST be pushed to system boundaries.

**Requirements:**
- Domain logic MUST be implementable as pure functions
- IO and effects MUST live at application edges, not in domain code
- Pure functions MUST be testable without mocking
- Effect boundaries MUST be explicit in type signatures
- State mutations MUST be isolated and controlled

**Rationale:** Purity enables testing, reasoning, and safe concurrent execution. Separating
concerns makes code more maintainable and reusable.

### VI. Property-Based Testing

Test strategies MUST derive from types and mathematical properties, not just example cases.

**Requirements:**
- Algebraic laws MUST be tested via property-based tests
- Serialization round-trips MUST be verified (parse . serialize = id)
- Golden tests MUST protect API contracts and persisted data formats
- Test coverage SHOULD follow the testing pyramid (unit > integration > e2e)
- Tests MUST be independent and deterministic

**Rationale:** Property-based tests explore edge cases that example-based tests miss. Laws
provide a specification that tests verify.

## Development Standards

### Code Organization

**Module Structure:**
- Maximum 500 lines per module (split if exceeded)
- Explicit export lists required
- Types in dedicated modules when shared across features
- Internal modules for implementation details

**Naming Conventions:**
- Types: PascalCase
- Functions: camelCase
- Smart constructors: mk prefix (mkEmail, mkUserId)
- Predicates: is/has prefix
- Conversions: to/from (emailToText, textToEmail)
- Type names MUST denote what they ARE (domain concept), not what field they populate:
  - Good: `TokenValidity`, `ClientName` (describes the concept)
  - Bad: `ExpiresIn`, `NameField` (mirrors field/wire format names)

### Error Handling

**Error Type Hierarchy:**
- Domain errors as sum types (OrderError, ValidationError)
- Specific constructors with relevant context
- Error messages MUST suggest remediation when possible

**Error Boundaries:**
- Parse external input at system boundaries
- Aggregate errors at handler/controller level
- Crash fast for unrecoverable errors (config missing, migration failed)

## Quality Gates

*GATE: All code MUST pass before merge.*

- [ ] **Types designed first**: Type signatures written before implementations
- [ ] **No illegal states**: Sum types used for state machines and alternatives
- [ ] **Smart constructors**: Validated types constructed only through smart constructors
- [ ] **Total functions**: No partial functions in public APIs
- [ ] **Pure domain logic**: Business rules testable without IO
- [ ] **Property tests**: Laws and invariants tested via properties
- [ ] **Build passes**: `cabal build` succeeds without warnings
- [ ] **Tests pass**: `cabal test` succeeds
- [ ] **Linting clean**: `hlint` reports no issues

## Governance

This constitution supersedes all other coding practices in this project. Amendments require:

1. Written proposal documenting the change and rationale
2. Update to this file with version increment
3. Review of dependent templates for consistency
4. Migration plan for existing code if principle changes

**Compliance:**
- All code reviews MUST verify adherence to these principles
- Violations MUST be justified in the Complexity Tracking section of plan.md
- Unjustified complexity SHOULD be refactored before merge

**Version Policy:**
- MAJOR: Principle removed or fundamentally redefined
- MINOR: New principle added or existing principle materially expanded
- PATCH: Clarifications, wording improvements, non-semantic changes

**Version**: 1.0.2 | **Ratified**: 2025-12-08 | **Last Amended**: 2025-12-19
