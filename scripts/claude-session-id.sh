#!/bin/bash
# Derive the current Claude Code session ID from filesystem
# Usage: claude-session-id.sh [project-path] [--by-title TITLE]

PROJECT_PATH="${1:-$(pwd)}"
BY_TITLE=""

# Parse args
while [[ $# -gt 0 ]]; do
    case $1 in
        --by-title) BY_TITLE="$2"; shift 2 ;;
        *) PROJECT_PATH="$1"; shift ;;
    esac
done

# Convert path to Claude's directory format
CLAUDE_DIR="$HOME/.claude/projects/$(echo "$PROJECT_PATH" | sed 's|/|-|g')"

if [[ ! -d "$CLAUDE_DIR" ]]; then
    echo "Error: No Claude project directory for $PROJECT_PATH" >&2
    exit 1
fi

if [[ -n "$BY_TITLE" ]]; then
    # Find by custom title in sessions-index.json
    INDEX_FILE="$CLAUDE_DIR/sessions-index.json"
    if [[ -f "$INDEX_FILE" ]]; then
        SESSION_ID=$(jq -r ".entries[] | select(.customTitle == \"$BY_TITLE\") | .sessionId" "$INDEX_FILE" | head -1)
        if [[ -n "$SESSION_ID" && "$SESSION_ID" != "null" ]]; then
            echo "$SESSION_ID"
            exit 0
        fi
    fi
    echo "Error: No session with title '$BY_TITLE'" >&2
    exit 1
fi

# Find most recently modified .jsonl file
NEWEST=$(ls -t "$CLAUDE_DIR"/*.jsonl 2>/dev/null | head -1)
if [[ -z "$NEWEST" ]]; then
    echo "Error: No session files found" >&2
    exit 1
fi

# Extract session ID from filename
basename "$NEWEST" .jsonl
