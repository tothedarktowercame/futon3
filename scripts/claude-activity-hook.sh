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

# Debug: log payload to temp file
echo "$(date): PAYLOAD=$PAYLOAD" >> /tmp/claude-hook-debug.log

# Extract fields from the hook payload
SESSION_ID=$(echo "$PAYLOAD" | jq -r '.session_id // empty' 2>/dev/null)
CWD=$(echo "$PAYLOAD" | jq -r '.cwd // empty' 2>/dev/null)
PROMPT=$(echo "$PAYLOAD" | jq -r '.prompt // empty' 2>/dev/null)

# Debug: log extracted fields
echo "$(date): SESSION_ID=$SESSION_ID PROMPT_LEN=${#PROMPT}" >> /tmp/claude-hook-debug.log

# POST to MUSN for Labs integration + affect processing (fire and forget)
{
  if [ -n "$PROMPT" ] && [ -n "$SESSION_ID" ]; then
    # Ensure session exists (create if needed)
    SESSION_RESP=$(curl -s -X POST "http://${MUSN_HOST}:${MUSN_PORT}/musn/session/create" \
      -H "Content-Type: application/json" \
      -d "$(jq -n \
        --arg session_id "$SESSION_ID" \
        '{
          "session/id": $session_id
        }')" 2>&1)
    echo "$(date): session/create resp=$SESSION_RESP" >> /tmp/claude-hook-debug.log

    # Log the turn (triggers affect processing)
    TURN_RESP=$(curl -s -X POST "http://${MUSN_HOST}:${MUSN_PORT}/musn/scribe/turn" \
      -H "Content-Type: application/json" \
      -d "$(jq -n \
        --arg session_id "$SESSION_ID" \
        --arg content "$PROMPT" \
        '{
          "session/id": $session_id,
          "role": "user",
          "content": $content
        }')" 2>&1)
    echo "$(date): scribe/turn resp=$TURN_RESP" >> /tmp/claude-hook-debug.log
  fi

  # Also log to activity stream for vitality tracking
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
