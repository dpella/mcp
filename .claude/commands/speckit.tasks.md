---
description: Generate actionable, dependency-ordered beads (tasks) for the feature based on available design artifacts. All tasks are children of a feature epic.
handoffs:
  - label: Analyze For Consistency
    agent: speckit.analyze
    prompt: Run a project analysis for consistency
    send: true
  - label: Implement Project
    agent: speckit.implement
    prompt: Start the implementation in phases
    send: true
---

## Tools

MUST use the beads-project-tracking skill for effective beads usage

## User Input

```text
$ARGUMENTS
```

You **MUST** consider the user input before proceeding (if not empty).

## Outline

1. **Setup**: Run `.specify/scripts/bash/check-prerequisites.sh --json` from repo root and parse FEATURE_DIR and AVAILABLE_DOCS list. All paths must be absolute. For single quotes in args like "I'm Groot", use escape syntax: e.g 'I'\''m Groot' (or double-quote if possible: "I'm Groot").

2. **Get feature branch name**: Run `git branch --show-current` to get the current branch name. This will be used as the epic title.

3. **Load design documents**: Read from FEATURE_DIR:
   - **Required**: plan.md (tech stack, libraries, structure), spec.md (user stories with priorities)
   - **Optional**: data-model.md (entities), contracts/ (API endpoints), research.md (decisions), quickstart.md (test scenarios)
   - Note: Not all projects have all documents. Generate tasks based on what's available.

4. **Check for existing epic**: Run `bd list --type epic --label "feature:<branch-name>"` to check if an epic already exists for this feature.
   - If epic exists: Use the existing epic ID as the parent for new tasks
   - If no epic exists: Create one in step 5

5. **Create feature epic** (if needed):
   ```bash
   bd create "Epic: <Feature Name from plan.md>" -t epic -p 1 \
     -l "feature:<branch-name>" \
     -d "Feature branch: <branch-name>. <Brief description from spec.md overview>. See FEATURE_DIR for full specification."
   ```
   - The epic ID (e.g., `bd-a1b2`) will be the parent for all tasks
   - Store this EPIC_ID for use in subsequent task creation

6. **Execute task generation workflow**:
   - Load plan.md and extract tech stack, libraries, project structure
   - Load spec.md and extract user stories with their priorities (P1, P2, P3, etc.)
   - If data-model.md exists: Extract entities and map to user stories
   - If contracts/ exists: Map endpoints to user stories
   - If research.md exists: Extract decisions for setup tasks
   - Generate tasks organized by user story (see Task Generation Rules below)
   - Create tasks as children of the feature epic using beads CLI

7. **Create tasks using beads CLI**: For each task, run:
   ```bash
   bd create "<Task description>" -t task -p <priority> \
     --parent <EPIC_ID> \
     -l "<phase-label>" \
     -l "<story-label>" \
     -d "<Detailed description with file paths and context>"
   ```

   **Label conventions**:
   - Phase labels: `phase:setup`, `phase:foundational`, `phase:us1`, `phase:us2`, `phase:polish`
   - Story labels: `story:us1`, `story:us2`, etc. (for user story tasks only)
   - Parallel marker: `parallel:true` (for tasks that can run in parallel)

8. **Create dependencies between tasks**: For tasks that block others:
   ```bash
   bd dep add <CHILD_ID> <BLOCKING_ID> --type blocks
   ```

   Use `blocks` sparingly - only for true prerequisites where work cannot begin until the blocker is complete.

9. **Report**: Output summary:
   - Epic ID and title
   - Total task count
   - Task count per phase
   - Task count per user story
   - Parallel opportunities identified (tasks with `parallel:true` label)
   - Independent test criteria for each story
   - Suggested MVP scope (typically just User Story 1)
   - Command to view task tree: `bd dep tree <EPIC_ID>`

Context for task generation: $ARGUMENTS

The beads should be immediately executable - each task must be specific enough that an LLM can complete it without additional context.

## Task Generation Rules

**CRITICAL**: Tasks MUST be organized by user story to enable independent implementation and testing.

**Tests are OPTIONAL**: Only generate test tasks if explicitly requested in the feature specification or if user requests TDD approach.

### Task Description Format

Every task description MUST include:
1. **Clear action**: What needs to be done
2. **File path(s)**: Exact location(s) where work happens
3. **Context**: Why this task exists and what it enables

**Examples**:
- "Create User model with email, password_hash, created_at fields in src/models/user.py"
- "Implement JWT token generation and validation in src/auth/tokens.ts - RS256, 1h expiry"
- "Add authentication middleware to protect /api/users/* routes in src/middleware/auth.py"

### Task Organization

1. **From User Stories (spec.md)** - PRIMARY ORGANIZATION:
   - Each user story (P1, P2, P3...) gets its own phase
   - Map all related components to their story:
     - Models needed for that story
     - Services needed for that story
     - Endpoints/UI needed for that story
     - If tests requested: Tests specific to that story
   - Mark story dependencies (most stories should be independent)

2. **From Contracts**:
   - Map each contract/endpoint → to the user story it serves
   - If tests requested: Each contract → contract test task before implementation in that story's phase

3. **From Data Model**:
   - Map each entity to the user story(ies) that need it
   - If entity serves multiple stories: Put in earliest story or Setup phase
   - Relationships → service layer tasks in appropriate story phase

4. **From Setup/Infrastructure**:
   - Shared infrastructure → Setup phase
   - Foundational/blocking tasks → Foundational phase
   - Story-specific setup → within that story's phase

### Phase Structure

- **Phase: Setup** (`phase:setup`): Project initialization, dependencies, configuration
- **Phase: Foundational** (`phase:foundational`): Blocking prerequisites - MUST complete before user stories
- **Phase: US1, US2, US3...** (`phase:us1`, `phase:us2`, etc.): User Stories in priority order (P1, P2, P3...)
  - Within each story: Tests (if requested) → Models → Services → Endpoints → Integration
  - Each phase should be a complete, independently testable increment
- **Phase: Polish** (`phase:polish`): Cross-cutting concerns, documentation, cleanup

### Priority Mapping

| Priority | Use For | bd priority |
|----------|---------|-------------|
| P0 | Security, data loss, blocking issues | 0 |
| P1 | Core functionality, MVP features | 1 |
| P2 | Standard features, normal priority | 2 |
| P3 | Nice-to-have, optimization | 3 |
| P4 | Future work, backlog | 4 |

### Hierarchical ID Structure

Beads automatically generate hierarchical IDs based on parent-child relationships:

```
bd-a1b2                    # Epic: Feature
├── bd-a1b2.1              # Task: Setup (child of epic)
├── bd-a1b2.2              # Task: User model (child of epic)
│   ├── bd-a1b2.2.1        # Subtask: discovered during implementation
│   └── bd-a1b2.2.2        # Subtask: another discovery
└── bd-a1b2.3              # Task: API endpoint
    └── bd-a1b2.3.1        # Subtask: validation edge case
```

**Creating discovered work during implementation**:
```bash
# Create discovered issue and link to parent task
bd create "Discovered bug in validation" -t bug -p 0 --json
bd dep add <new-id> <parent-task-id> --type discovered-from
```

The `discovered-from` dependency type links discovered work back to the task where it was found, preserving context for future sessions.

## Example Output

After running this command, you should see output like:

```
Feature Epic Created
━━━━━━━━━━━━━━━━━━━━
Epic ID: bd-a1b2
Title: Epic: User Authentication
Branch: 42-user-auth
Labels: feature:42-user-auth

Tasks Created (12 total)
━━━━━━━━━━━━━━━━━━━━━━━━
Phase: Setup (2 tasks)
  - bd-a1b2.1 [P2] Initialize project structure
  - bd-a1b2.2 [P2] Configure dependencies and environment

Phase: Foundational (2 tasks)
  - bd-a1b2.3 [P1] Set up database connection
  - bd-a1b2.4 [P1] Create base model classes

Phase: US1 - User Registration (4 tasks)
  - bd-a1b2.5 [P1] [story:us1] Create User model
  - bd-a1b2.6 [P1] [story:us1] Implement UserService
  - bd-a1b2.7 [P1] [story:us1] Add POST /users endpoint
  - bd-a1b2.8 [P1] [story:us1] Create registration form

Phase: US2 - User Login (3 tasks)
  - bd-a1b2.9  [P1] [story:us2] Implement JWT tokens
  - bd-a1b2.10 [P1] [story:us2] Add POST /auth/login endpoint
  - bd-a1b2.11 [P1] [story:us2] Create login form

Phase: Polish (1 task)
  - bd-a1b2.12 [P3] Add API documentation

Parallel Opportunities
━━━━━━━━━━━━━━━━━━━━━
- Setup tasks bd-a1b2.1 and bd-a1b2.2 can run in parallel
- US1 tasks bd-a1b2.5, bd-a1b2.6 affect different files

Dependencies
━━━━━━━━━━━━
- bd-a1b2.3 blocks bd-a1b2.5 (User model needs DB)
- bd-a1b2.9 blocks bd-a1b2.10 (Login needs JWT)

View full tree: bd dep tree bd-a1b2
Find ready work: bd ready --sort hybrid
```
