#!/usr/bin/env bash
set -euo pipefail

# Restart Claude drawbridge with an explicit session id (resume).
#
# Usage:
#   scripts/claude-drawbridge-restart.sh --session-id <id>
#   FUTON3_CLAUDE_SESSION_ID=<id> scripts/claude-drawbridge-restart.sh
#
# The Claude drawbridge runs in the MUSN JVM, so this uses repl-eval
# to hot-reload rather than spawning a separate process.

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

SESSION_ID="${FUTON3_CLAUDE_SESSION_ID:-}"
AGENCY_URL="${FUTON3_CLAUDE_AGENCY_URL:-http://localhost:7070}"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --session-id)
      SESSION_ID="${2:-}"
      shift 2
      ;;
    --agency-url)
      AGENCY_URL="${2:-}"
      shift 2
      ;;
    *)
      echo "Unknown arg: $1" >&2
      exit 1
      ;;
  esac
done

if [[ -z "${SESSION_ID}" ]]; then
  echo "Missing session id. Use --session-id or set FUTON3_CLAUDE_SESSION_ID." >&2
  exit 1
fi

cd "${ROOT}"

# Restart via REPL (runs in MUSN JVM)
cat <<EOF | ./scripts/repl-eval
(do
  (require '[futon3.drawbridge.claude :as claude])
  (claude/stop!)
  (claude/start! {:agent-id "claude"
                  :agency-http-url "${AGENCY_URL}"
                  :resume-id "${SESSION_ID}"})
  (println "[claude-drawbridge] resumed session: ${SESSION_ID}"))
EOF

echo ""
echo "[claude-drawbridge] resumed session: ${SESSION_ID}"
curl -s "http://127.0.0.1:7070/agency/connected" | jq -r '.local // []' || true
