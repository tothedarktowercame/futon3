#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'USAGE'
Usage: ./scripts/forum-bell.sh --thread <thread-id> [--duration <seconds>]

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

Flags:
  --thread <id>          Forum thread id
  --duration <seconds>   Duration (seconds)
  --codex-only           Include fucodex, exclude fuclaude
  --claude-only          Include fuclaude, exclude fucodex
  --no-post              Do not post a bell message
USAGE
}

thread="${FORUM_THREAD:-}"
duration="${BELL_DURATION_SECONDS:-300}"
include_codex="${BELL_CODEX:-1}"
include_claude="${BELL_CLAUDE:-1}"
bell_author="${BELL_AUTHOR:-bell}"
bell_post="${BELL_POST:-1}"

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

if [[ -z "$thread" ]]; then
  echo "FORUM_THREAD is required" >&2
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

if [[ "$bell_post" == "1" ]]; then
  mentions=()
  if [[ "$include_codex" == "1" ]]; then
    mentions+=("@fucodex")
  fi
  if [[ "$include_claude" == "1" ]]; then
    mentions+=("@fuclaude")
  fi
  mention_text="${mentions[*]}"
  bell_body="[bell] ${mention_text} standup for ${duration}s (bridges auto-exit)."

  curl -s \
    -X POST "${forum_server%/}/forum/thread/${thread}/reply" \
    -H 'Content-Type: application/json' \
    -d "{\"author\":\"${bell_author}\",\"body\":\"${bell_body}\"}" \
    >/dev/null || true
fi

wait
