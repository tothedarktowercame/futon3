#!/usr/bin/env bash
set -euo pipefail

if [[ -t 1 && -z "${NO_COLOR:-}" ]]; then
  RED="\033[0;31m"
  YELLOW="\033[0;33m"
  GREEN="\033[0;32m"
  BLUE="\033[0;34m"
  RESET="\033[0m"
else
  RED=""
  YELLOW=""
  GREEN=""
  BLUE=""
  RESET=""
fi

info() { printf "%b\n" "${BLUE}$*${RESET}"; }
warn() { printf "%b\n" "${YELLOW}$*${RESET}" >&2; }
err()  { printf "%b\n" "${RED}$*${RESET}" >&2; }

# Futon3 consolidated dev server
#
# All services now run in a single JVM via f2.musn:
#   - Transport (HUD) on port 5050
#   - UI on port 6060
#   - MUSN HTTP on port 6065
#   - IRC bridge on port 6667
#   - Forum WebSocket on port 5055
#   - Lab WebSocket on port 5056
#   - Drawbridge REPL on port 6767
#   - Chat supervisor (polling)
#   - Agency HTTP on port 7070 (separate process)
#
# Environment variables to disable services:
#   FUTON3_MUSN_HTTP=0       - disable MUSN HTTP service
#   FUTON3_IRC_BRIDGE=0      - disable IRC bridge
#   FUTON3_CHAT_SUPERVISOR=0 - disable chat supervisor
#   FUTON3_AGENCY=0          - disable Agency HTTP service
#   FUTON3_DRAWBRIDGE=0      - disable Drawbridge REPL (enabled by default)
#   FUTON3_CODEX_DRAWBRIDGE=0 - disable Codex drawbridge (enabled by default on laptop)
#   FUTON3_CODEX_SESSION_ID   - optional Codex resume id for drawbridge
#
# Drawbridge runs on port 6767 for hot-reloading code:
#   ./scripts/repl-eval '(require '\''f2.transport :reload)'
#
# Example: run without chat supervisor (for manual fuclaude testing)
#   FUTON3_CHAT_SUPERVISOR=0 ./scripts/dev.sh

cd "$(dirname "$0")/.."

# Load environment defaults
# Priority: existing env vars > .env.local > .env.dev
if [[ -f .env.local ]]; then
  info "[dev] Loading .env.local"
  set -a; source .env.local; set +a
elif [[ -f .env.dev ]]; then
  info "[dev] Loading .env.dev"
  set -a; source .env.dev; set +a
fi

touch /tmp/musn_stream.log

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

# Load .env if present (avoid nounset errors while sourcing).
if [[ -f "${ROOT}/.env" ]]; then
  set +u
  set -a
  # shellcheck disable=SC1091
  source "${ROOT}/.env"
  set +a
  set -u
fi

# Legacy env var support
if [[ "${SKIP_CHAT_SUPERVISOR:-}" == "1" ]]; then
  export FUTON3_CHAT_SUPERVISOR=0
fi

# Enable Drawbridge by default for dev (hot-reloading on port 6767)
export FUTON3_DRAWBRIDGE="${FUTON3_DRAWBRIDGE:-1}"
export FUTON3_CODEX_DRAWBRIDGE="${FUTON3_CODEX_DRAWBRIDGE:-1}"

# Load ADMIN_TOKEN from .admintoken if not already set (required for Drawbridge)
if [[ -z "${ADMIN_TOKEN:-}" && -f .admintoken ]]; then
  export ADMIN_TOKEN="$(cat .admintoken | tr -d '\n')"
fi

# Bootstrap penholder registry (needed for fresh data dirs).
if [[ "${FUTON1_API:-1}" != "0" && "${FUTON1_BOOTSTRAP_PENHOLDER:-1}" != "0" ]]; then
  FUTON1_ROOT="${FUTON1_ROOT:-${ROOT}/../futon1}"
  DATA_DIR="${BASIC_CHAT_DATA_DIR:-${ROOT}/data/default}"
  BOOTSTRAP_MARKER="${DATA_DIR}/.penholder_bootstrap"
  if [[ -d "${FUTON1_ROOT}" && ! -f "${BOOTSTRAP_MARKER}" ]]; then
    echo "[dev] Bootstrapping penholder registry in ${DATA_DIR}..."
    mkdir -p "${DATA_DIR}"
    (
      cd "${FUTON1_ROOT}"
      BASIC_CHAT_DATA_DIR="${DATA_DIR}" \
      ALPHA_PROFILE="${ALPHA_PROFILE:-default}" \
      MODEL_PENHOLDER="${MODEL_PENHOLDER:-${USER:-cli}}" \
      MODEL_VERIFY_ON_WRITE=0 \
      clojure -M:scripts/graph-memory -m scripts.charon-penholder-bootstrap
    )
    touch "${BOOTSTRAP_MARKER}"
  fi
fi

# Pass through any JVM opts for memory limits if desired
# Example: JAVA_OPTS="-Xmx1g" ./scripts/dev.sh
port_open() {
  local port="$1"
  (echo > "/dev/tcp/127.0.0.1/${port}") >/dev/null 2>&1
}

agency_pid=""
codex_drawbridge_pid=""
if [[ "${FUTON3_AGENCY:-1}" != "0" ]]; then
  if port_open 7070; then
    warn "[dev] Agency already running on port 7070."
  else
    info "[dev] Starting Agency on port 7070..."
    (cd "${ROOT}" && ./scripts/agency) &
    agency_pid="$!"
  fi
fi

cleanup() {
  if [[ -n "${agency_pid}" ]]; then
    kill "${agency_pid}" >/dev/null 2>&1 || true
  fi
  if [[ -n "${codex_drawbridge_pid}" ]]; then
    kill "${codex_drawbridge_pid}" >/dev/null 2>&1 || true
  fi
}
trap cleanup EXIT

if [[ "${FUTON3_CODEX_DRAWBRIDGE:-1}" != "0" ]]; then
  if port_open 6769; then
    warn "[dev] Codex drawbridge already running on port 6769."
  else
    info "[dev] Starting Codex drawbridge on ports 6769/6771..."
    cat > /tmp/codex-drawbridge.sh <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
cd /home/joe/code/futon3
resume_id="${FUTON3_CODEX_SESSION_ID:-}"
if [[ -n "${resume_id}" ]]; then
  resume_form=":resume-id \"${resume_id}\""
else
  resume_form=""
fi
exec clojure -M -e "(require (quote futon3.drawbridge.codex)) (futon3.drawbridge.codex/start! {:http-port 6769 :ws-port 6771 :agent-id \"codex\" ${resume_form} :agency-ws-url \"ws://localhost:7070/agency/ws?agent-id=codex\" :register-local? false}) (Thread/sleep 1000000000)"
EOF
    chmod +x /tmp/codex-drawbridge.sh
    setsid /tmp/codex-drawbridge.sh >/tmp/codex-drawbridge.log 2>&1 </dev/null &
    codex_drawbridge_pid="$!"
  fi
fi

if [[ -n "${JAVA_OPTS:-}" ]]; then
  clojure $JAVA_OPTS -M:dev "$@"
else
  clojure -M:dev "$@"
fi
