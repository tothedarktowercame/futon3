#!/usr/bin/env bash
# Run the Claude Opus forum bridge with sensible defaults
#
# Usage:
#   ./scripts/forum-bridge.sh t-123bcc8e
#   ./scripts/forum-bridge.sh --thread t-123bcc8e

set -euo pipefail
cd "$(dirname "$0")/.."

# Load environment defaults
if [[ -f .env.local ]]; then
  set -a; source .env.local; set +a
elif [[ -f .env.dev ]]; then
  set -a; source .env.dev; set +a
fi

# Parse thread argument
THREAD="${1:-}"
if [[ "$THREAD" == "--thread" ]]; then
  THREAD="${2:-}"
fi

if [[ -z "$THREAD" ]]; then
  echo "Usage: $0 <thread-id>"
  echo "Example: $0 t-123bcc8e"
  exit 1
fi

export FORUM_THREAD="$THREAD"
export FORUM_SERVER="${FORUM_SERVER:-http://localhost:5050}"
export FORUM_WS_SERVER="${FORUM_WS_SERVER:-ws://localhost:5055}"

echo "[forum-bridge] Thread: $FORUM_THREAD"
echo "[forum-bridge] Server: $FORUM_SERVER"
echo "[forum-bridge] WS: $FORUM_WS_SERVER"

exec npx ts-node scripts/claude-opus-forum-bridge.ts
