#!/usr/bin/env bash
# Post-edit hook: run cabal build and run hlint on modified/new files
# Rejects edits that introduce errors or warnings

# Read JSON input from stdin
input=$(cat)
file_path=$(echo "$input" | jq -r '.tool_input.file_path // empty')

# Only check .hs files
if [[ ! "$file_path" == *.hs ]]; then
  exit 0
fi

cd "$CLAUDE_PROJECT_DIR" || exit 1

output=$(cabal build 2>&1)
result=$?

if [[ $result -ne 0 ]]; then
  echo "cabal build failed:" >&2
  echo "$output" >&2
  exit 2  # Exit 2 = blocking error, rejects the edit
fi

output=$(hlint "$file_path")
result=$?

if [[ $result -ne 0 ]]; then
  echo "hlint failed:" >&2
  echo "$output" >&2
  exit 2  # Exit 2 = blocking error, rejects the edit
fi

# Format the edited file with fourmolu
fourmolu -i "$file_path" 2>&1

exit 0
