#!/bin/bash
# Claude Code hook to POST activity to MUSN
#
# Configured via ~/.claude/settings.local.json:
#   "hooks": {
#     "UserPromptSubmit": [{
#       "hooks": [{"type": "command", "command": "/home/joe/code/futon3/scripts/claude-activity-hook.sh"}]
#     }]
#   }
#
# Input (JSON via stdin):
#   session_id, cwd, prompt, etc.

MUSN_HOST="${MUSN_HOST:-127.0.0.1}"
MUSN_PORT="${MUSN_PORT:-6065}"

# Read the hook payload from stdin
PAYLOAD=$(cat)

# Extract fields from the hook payload
SESSION_ID=$(echo "$PAYLOAD" | jq -r '.session_id // empty' 2>/dev/null)
CWD=$(echo "$PAYLOAD" | jq -r '.cwd // empty' 2>/dev/null)

# POST to MUSN activity log (fire and forget, backgrounded)
{
  curl -s -X POST "http://${MUSN_HOST}:${MUSN_PORT}/musn/activity/log" \
    -H "Content-Type: application/json" \
    -d "$(jq -n \
      --arg session_id "$SESSION_ID" \
      --arg cwd "$CWD" \
      '{
        agent: "claude",
        source: "claude-code",
        "session/id": $session_id,
        "event/type": "agent/prompt-submit",
        metadata: {cwd: $cwd, hook: "UserPromptSubmit"}
      }')" >/dev/null 2>&1
} &

# Output nothing (no additional context needed)
# Exit 0 to allow the prompt to proceed
exit 0
