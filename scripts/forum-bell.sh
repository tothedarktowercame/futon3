#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'USAGE'
Usage: ./scripts/forum-bell.sh --thread <thread-id> [--duration <seconds>] [--par]

Summon agents into a Forum thread for timeboxed coordination.
Optionally start a CRDT PAR session for collaborative reflection.

Environment:
  FORUM_SERVER           Forum HTTP base (default: http://localhost:5050)
  FORUM_WS_SERVER        Forum WS base (default: ws://localhost:5055)
  FORUM_THREAD           Thread id (required if not passed)
  BELL_DURATION_SECONDS  Duration in seconds (default: 300)
  BELL_CODEX             1 to include fucodex (default: 1)
  BELL_CLAUDE            1 to include fuclaude (default: 1)
  BELL_AUTHOR            Author name for bell post (default: bell)
  BELL_POST              1 to post a bell message (default: 1)
  AGENCY_SERVER          Agency base URL (default: http://localhost:7070)
  CRDT_SERVER            CRDT server host (default: localhost)
  CRDT_PORT              CRDT server port (default: 6530)
  MUSN_SERVER            MUSN HTTP base for PAR submission (default: http://localhost:6065)

Flags:
  --thread <id>          Forum thread id
  --duration <seconds>   Duration (seconds)
  --codex-only           Include fucodex, exclude fuclaude
  --claude-only          Include fuclaude, exclude fucodex
  --no-post              Do not post a bell message
  --par                  Start CRDT PAR session for collaborative reflection
  --par-title <title>    Title for the PAR session (default: "Joint Standup")
USAGE
}

thread="${FORUM_THREAD:-}"
duration="${BELL_DURATION_SECONDS:-300}"
include_codex="${BELL_CODEX:-1}"
include_claude="${BELL_CLAUDE:-1}"
bell_author="${BELL_AUTHOR:-bell}"
bell_post="${BELL_POST:-1}"
par_mode=0
par_title="Joint Standup"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --thread)
      thread="$2"
      shift 2
      ;;
    --duration|--seconds)
      duration="$2"
      shift 2
      ;;
    --codex-only)
      include_codex=1
      include_claude=0
      shift
      ;;
    --claude-only)
      include_codex=0
      include_claude=1
      shift
      ;;
    --no-post)
      bell_post=0
      shift
      ;;
    --par)
      par_mode=1
      shift
      ;;
    --par-title)
      par_title="$2"
      shift 2
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "Unknown arg: $1" >&2
      usage
      exit 2
      ;;
  esac
done

# Thread is required for Forum standup, optional for PAR-only
if [[ -z "$thread" && "$par_mode" == "0" ]]; then
  echo "FORUM_THREAD is required (or use --par for PAR-only mode)" >&2
  usage
  exit 2
fi

if ! [[ "$duration" =~ ^[0-9]+$ ]]; then
  echo "Duration must be an integer (seconds)." >&2
  exit 2
fi

forum_server="${FORUM_SERVER:-http://localhost:5050}"
forum_ws_server="${FORUM_WS_SERVER:-ws://localhost:5055}"
agency_server="${AGENCY_SERVER:-http://localhost:7070}"
crdt_server="${CRDT_SERVER:-localhost}"
crdt_port="${CRDT_PORT:-6530}"
musn_server="${MUSN_SERVER:-http://localhost:6065}"

pids=()
cleanup() {
  for pid in "${pids[@]:-}"; do
    kill "$pid" >/dev/null 2>&1 || true
  done
}
trap cleanup EXIT

start_bridge() {
  timeout "$duration" "$@" &
  pids+=("$!")
}

# Build participant list
participants=()
if [[ "$include_codex" == "1" ]]; then
  participants+=("fucodex")
fi
if [[ "$include_claude" == "1" ]]; then
  participants+=("fuclaude")
fi
participants+=("joe")  # Human always included

# Start Forum bridges if thread specified
if [[ -n "$thread" ]]; then
  if [[ "$include_codex" == "1" ]]; then
    FORUM_SERVER="$forum_server" \
    FORUM_WS_SERVER="$forum_ws_server" \
    FORUM_THREAD="$thread" \
    FORUM_MENTION="@fucodex" \
    AGENCY_SERVER="$agency_server" \
    AGENCY_AGENT_ID="fucodex" \
    AGENCY_PERIPHERAL="chat" \
    start_bridge ./scripts/forum-bridge.clj
  fi

  if [[ "$include_claude" == "1" ]]; then
    FORUM_SERVER="$forum_server" \
    FORUM_WS_SERVER="$forum_ws_server" \
    FORUM_THREAD="$thread" \
    FORUM_MENTION="@fuclaude" \
    AGENCY_SERVER="$agency_server" \
    AGENCY_AGENT_ID="fuclaude" \
    AGENCY_PERIPHERAL="chat" \
    start_bridge ./scripts/forum-bridge-fuclaude.clj
  fi

  # Give bridges time to connect before ringing the bell
  sleep 2
fi

# Post bell message
if [[ "$bell_post" == "1" && -n "$thread" ]]; then
  mentions=()
  if [[ "$include_codex" == "1" ]]; then
    mentions+=("@fucodex")
  fi
  if [[ "$include_claude" == "1" ]]; then
    mentions+=("@fuclaude")
  fi
  mention_text="${mentions[*]}"

  if [[ "$par_mode" == "1" ]]; then
    bell_body="[bell] ${mention_text} standup + PAR for ${duration}s.

**PAR Session:** ${par_title}
**CRDT:** ${crdt_server}:${crdt_port}
**Participants:** ${participants[*]}

After standup, connect to CRDT for collaborative PAR. (bridges auto-exit)"
  else
    bell_body="[bell] ${mention_text} standup for ${duration}s (bridges auto-exit)."
  fi

  curl -s \
    -X POST "${forum_server%/}/forum/thread/${thread}/reply" \
    -H 'Content-Type: application/json' \
    -d "{\"author\":\"${bell_author}\",\"body\":$(echo "$bell_body" | jq -Rs .)}" \
    >/dev/null || true
fi

# PAR mode: print instructions for CRDT connection
if [[ "$par_mode" == "1" ]]; then
  echo ""
  echo "=== PAR SESSION: ${par_title} ==="
  echo "CRDT Server: ${crdt_server}:${crdt_port}"
  echo "Participants: ${participants[*]}"
  echo "Duration: ${duration}s"
  echo ""
  echo "In Emacs, run:"
  echo "  (setq futon-crdt-server-host \"${crdt_server}\")"
  echo "  (futon-start-joint-par \"${par_title}\" ${participants[*]/#/:})"
  echo ""
  echo "When done, submit the PAR:"
  echo "  M-x futon-par-submit-multi"
  echo ""
  echo "Or submit via curl:"
  echo "  curl -X POST ${musn_server}/musn/par/multi \\"
  echo "    -H 'Content-Type: application/json' \\"
  echo "    -d '{\"par/title\": \"${par_title}\", \"par/sessions\": [...], \"par/participants\": [${participants[*]/#/\":\"}], \"par/questions\": {...}}'"
  echo ""
  echo "=== Waiting for standup to complete (${duration}s) ==="
fi

wait

if [[ "$par_mode" == "1" ]]; then
  echo ""
  echo "=== Bell session complete ==="
  echo "Don't forget to submit the PAR if you haven't already!"
fi
