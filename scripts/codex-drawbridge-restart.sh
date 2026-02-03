#!/usr/bin/env bash
set -euo pipefail

# Restart Codex drawbridge with an explicit session id (resume).
#
# Usage:
#   scripts/codex-drawbridge-restart.sh --session-id <id>
#   FUTON3_CODEX_SESSION_ID=<id> scripts/codex-drawbridge-restart.sh
#
# Optional:
#   FUTON3_CODEX_AGENCY_WS=ws://host:7070/agency/ws?agent-id=codex
#   FUTON3_CODEX_AGENT_ID=codex

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

SESSION_ID="${FUTON3_CODEX_SESSION_ID:-}"
AGENT_ID="${FUTON3_CODEX_AGENT_ID:-codex}"
AGENCY_WS="${FUTON3_CODEX_AGENCY_WS:-}"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --session-id)
      SESSION_ID="${2:-}"
      shift 2
      ;;
    --agent-id)
      AGENT_ID="${2:-}"
      shift 2
      ;;
    *)
      echo "Unknown arg: $1" >&2
      exit 1
      ;;
  esac
done

if [[ -z "${SESSION_ID}" ]]; then
  echo "Missing session id. Use --session-id or set FUTON3_CODEX_SESSION_ID." >&2
  exit 1
fi

if [[ -z "${AGENCY_WS}" ]]; then
  AGENCY_WS="ws://localhost:7070/agency/ws?agent-id=${AGENT_ID}"
fi

pgrep -f "drawbridge.codex" | xargs -r kill || true

cat > /tmp/codex-drawbridge.sh <<EOF
#!/usr/bin/env bash
set -euo pipefail
cd "${ROOT}"
expr='(require (quote futon3.drawbridge.codex)) (futon3.drawbridge.codex/start! {:http-port 6769 :ws-port 6771 :agent-id "'"${AGENT_ID}"'" :resume-id "'"${SESSION_ID}"'" :agency-ws-url "'"${AGENCY_WS}"'" :register-local? false}) (Thread/sleep 1000000000)'
exec clojure -M -e "\${expr}"
EOF
chmod +x /tmp/codex-drawbridge.sh

setsid /tmp/codex-drawbridge.sh >/tmp/codex-drawbridge.log 2>&1 </dev/null &

sleep 1
echo "[codex-drawbridge] resumed session: ${SESSION_ID}"
curl -s "http://127.0.0.1:6769/codex/status?token=InParallelWithHyperlinkQualityTesting" || true
