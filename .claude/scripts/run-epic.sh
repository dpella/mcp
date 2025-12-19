#!/usr/bin/env bash
# Run Claude in a loop until no beads are ready for a given epic
#
# Usage: run-epic.sh <epic-id>
#
# Environment variables:
#   CLAUDE_MODEL    - Model to use (default: opus)
#   SESSION_LOG     - Log file path (default: ../session-log.jsonl)
#   CLAUDE_PROMPT   - Override the default prompt
#   DRY_RUN         - If set, print prompt and exit without running Claude

set -euo pipefail

EPIC_ID="${1:-}"

if [[ -z "$EPIC_ID" ]]; then
    echo "Usage: $0 <epic-id>" >&2
    echo "Environment: CLAUDE_MODEL, SESSION_LOG, CLAUDE_PROMPT" >&2
    exit 1
fi

# Configuration (edit these or set via environment)
CLAUDE_MODEL="${CLAUDE_MODEL:-opus}"
SESSION_LOG="${SESSION_LOG:-../session-log.jsonl}"

# Base prompt template (beads list appended dynamically in main loop)
# Note: We use regular heredoc (not <<'EOF') to allow variable expansion for EPIC_ID
read -r -d '' BASE_PROMPT <<PROMPT_EOF || true
/speckit.implement claim ONE bead from epic $EPIC_ID, complete it, use commit skill, then STOP. Ensure by running tests that no regressions have been introduced BEFORE closing. Use the efficient-subagent-orchestration skill for main agent context-management. Use the beads-project-tracking skill to track progress.

EPIC: $EPIC_ID
Claim exactly ONE bead from the candidates below, complete it, then STOP.

CONTEXT MONITORING (CRITICAL): Continuously monitor your context usage. When you reach 50% context capacity, you MUST:
1. Leave the current bead as in_progress (do NOT close it)
2. Update the bead description to include: work completed, work remaining, and why you are handing off
3. Use the commit skill to commit any completed work
4. STOP immediately to allow a fresh context to continue

TECHNICAL DEBT TRACKING (MANDATORY):
- When you discover or create technical debt, create a 'discovered-from' bead to track it (unless already tracked)
- ALL TODO, FIXME, XXX, and HACK comments MUST include a bead ID that tracks the work. Format: TODO(bead-id): description
- Never leave implicit work—make it explicit in beads

BEAD SELECTION PRIORITY:
1. FIRST: Resume any in_progress bead (handoff continuation)
2. THEN: Claim the highest-priority ready bead from candidates below

ONE BEAD ONLY:
- Complete exactly ONE bead per session
- After bd close: commit, then STOP IMMEDIATELY
- Do NOT continue to the next bead - the wrapper script handles iteration
PROMPT_EOF

# Function to get sorted candidate beads for this epic
get_candidate_beads() {
    bd ready --json 2>/dev/null | jq -r --arg epic "$EPIC_ID" '
        [.[] | select(.id == $epic or (.id | startswith($epic + ".")))]
        | sort_by(.priority // 99)
        | .[]
        | "  \(.priority // "-")  \(.id)  \(.title)"
    '
}

# Function to get in_progress beads (handoff continuations)
get_in_progress_beads() {
    bd list --status in_progress --json 2>/dev/null | jq -r --arg epic "$EPIC_ID" '
        [.[] | select(.id != $epic and (.id | startswith($epic + ".")))]
        | .[]
        | "  \(.priority // "-")  \(.id)  \(.title)"
    '
}

# Build full prompt with current bead state
build_prompt() {
    local in_progress candidates full_prompt

    in_progress=$(get_in_progress_beads)
    candidates=$(get_candidate_beads)

    full_prompt="$BASE_PROMPT"

    if [[ -n "$in_progress" ]]; then
        full_prompt+=$'\n\nIN_PROGRESS (resume first):\n  P#  ID  TITLE\n'"$in_progress"
    fi

    if [[ -n "$candidates" ]]; then
        full_prompt+=$'\n\nCANDIDATE BEADS (sorted by priority):\n  P#  ID  TITLE\n'"$candidates"
    fi

    echo "$full_prompt"
}

# Function to count ready beads for epic (includes epic itself and all children)
count_ready_beads() {
    bd ready --json 2>/dev/null | jq --arg epic "$EPIC_ID" '
        [.[] | select(.id == $epic or (.id | startswith($epic + ".")))] | length
    '
}

# Claim the epic if not already claimed
claim_epic_if_needed() {
    local status output

    if ! output=$(bd show "$EPIC_ID" --json 2>&1); then
        echo "[$(date -Iseconds)] ERROR: Failed to query epic $EPIC_ID: $output" >&2
        exit 1
    fi

    status=$(echo "$output" | jq -r '.[0].status // "unknown"')

    case "$status" in
        ready|open)
            echo "[$(date -Iseconds)] Claiming epic $EPIC_ID..."
            if ! bd update "$EPIC_ID" --status in_progress; then
                echo "[$(date -Iseconds)] ERROR: Failed to claim epic $EPIC_ID" >&2
                exit 1
            fi
            echo "[$(date -Iseconds)] Epic $EPIC_ID claimed successfully"
            ;;
        in_progress)
            echo "[$(date -Iseconds)] Epic $EPIC_ID already in_progress"
            ;;
        completed)
            echo "[$(date -Iseconds)] Epic $EPIC_ID is already completed. Nothing to do."
            exit 0
            ;;
        unknown)
            echo "[$(date -Iseconds)] ERROR: Epic $EPIC_ID not found or has invalid status" >&2
            exit 1
            ;;
        *)
            echo "[$(date -Iseconds)] Epic $EPIC_ID has status: $status (cannot proceed)"
            exit 1
            ;;
    esac
}

# Ensure epic is claimed before starting (skip in dry run mode)
if [[ -z "${DRY_RUN:-}" ]]; then
    claim_epic_if_needed
fi

# Main loop
iteration=0
while true; do
    ready_count=$(count_ready_beads)
    iteration=$((iteration + 1))

    if [[ "$ready_count" -eq 0 ]]; then
        echo "[$(date -Iseconds)] No ready beads for epic $EPIC_ID. Done after $iteration iteration(s)!"
        exit 0
    fi

    echo "[$(date -Iseconds)] Iteration $iteration: $ready_count ready bead(s) for epic $EPIC_ID"

    # Build prompt with current bead state (or use override if set)
    if [[ -n "${CLAUDE_PROMPT:-}" ]]; then
        current_prompt="$CLAUDE_PROMPT"
    else
        current_prompt=$(build_prompt)
    fi

    # Dry run mode: print prompt and exit
    if [[ -n "${DRY_RUN:-}" ]]; then
        echo "=== DRY RUN MODE ==="
        echo "Epic: $EPIC_ID"
        echo "Model: $CLAUDE_MODEL"
        echo "Log: $SESSION_LOG"
        echo ""
        echo "=== PROMPT ==="
        echo "$current_prompt"
        echo ""
        echo "=== END DRY RUN ==="
        exit 0
    fi

    echo "[$(date -Iseconds)] Running Claude (model=$CLAUDE_MODEL, log=$SESSION_LOG)..."

    # Run Claude with streaming JSON output, log to file
    claude -p "$current_prompt" \
        --dangerously-skip-permissions \
        --model "$CLAUDE_MODEL" \
        --output-format stream-json \
        --verbose \
        | tee -a "$SESSION_LOG"

    echo ""  # Newline after Claude output
    sleep 2
done
