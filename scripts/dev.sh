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
#   - Agency HTTP on port 7070
#
# Environment variables to disable services:
#   FUTON3_MUSN_HTTP=0       - disable MUSN HTTP service
#   FUTON3_IRC_BRIDGE=0      - disable IRC bridge
#   FUTON3_CHAT_SUPERVISOR=0 - disable chat supervisor
#   FUTON3_AGENCY=0          - disable Agency HTTP service
#   FUTON3_DRAWBRIDGE=0      - disable Drawbridge REPL (enabled by default)
#   FUTON3_CODEX_DRAWBRIDGE=0 - disable Codex drawbridge (enabled by default on laptop)
#   FUTON3_CODEX_SESSION_ID   - optional Codex resume id for drawbridge
#   FUTON3_CODEX_AGENT_ID     - agent id for Codex drawbridge (default: codex)
#   FUTON3_CLAUDE_DRAWBRIDGE=0 - disable Claude drawbridge (enabled by default)
#   FUTON3_CLAUDE_SESSION_ID  - optional Claude resume id for drawbridge
#   FUTON3_CLAUDE_AGENT_ID    - agent id for Claude drawbridge (default: claude)
#   FUTON3_MUSN_PAGE=0        - disable MUSN chat -> Agency page bridge (enabled by default)
#   --musn-page-room ROOM     - override MUSN_PAGE_ROOM
#   --musn-page-agent ID      - override MUSN_PAGE_AGENT
#   --musn-page-ignore CSV    - override MUSN_PAGE_IGNORE
#   --musn-page-poll SECS     - override MUSN_PAGE_POLL
#   --musn-page-timeout MS    - override MUSN_PAGE_TIMEOUT
#   --musn-page-disable       - set FUTON3_MUSN_PAGE=0
#   --musn-page-enable        - set FUTON3_MUSN_PAGE=1
#
# Drawbridge runs on port 6767 for hot-reloading code:
#   ./scripts/repl-eval '(require '\''f2.transport :reload)'
#
# Example: run without chat supervisor (for manual fuclaude testing)
#   FUTON3_CHAT_SUPERVISOR=0 ./scripts/dev.sh

cd "$(dirname "$0")/.."

touch /tmp/musn_stream.log

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

# Load env defaults.
# Priority: existing env vars > .env.local > .env.dev > .env
#
# Note: `.env.local` must override `.env`. (Previously `.env` was sourced last
# and clobbered `.env.local`.)
if [[ -f "${ROOT}/.env" ]]; then
  set +u
  set -a
  # shellcheck disable=SC1091
  source "${ROOT}/.env"
  set +a
  set -u
fi
if [[ -f .env.dev ]]; then
  info "[dev] Loading .env.dev"
  set -a; source .env.dev; set +a
fi
if [[ -f .env.local ]]; then
  info "[dev] Loading .env.local"
  set -a; source .env.local; set +a
fi

# Legacy env var support
if [[ "${SKIP_CHAT_SUPERVISOR:-}" == "1" ]]; then
  export FUTON3_CHAT_SUPERVISOR=0
fi

# If requested, run Futon1a as the Futon1 replacement API and disable Futon1's embedded API.
# This keeps the Futon3 runtime pointed at an API on port 8080, but backed by Futon1a storage.
if [[ "${FUTON3_USE_FUTON1A:-0}" == "1" ]]; then
  export FUTON1_API=0
  export FUTON1A_PORT="${FUTON1A_PORT:-8080}"
  export FUTON1_API_BASE="http://localhost:${FUTON1A_PORT}"
fi

# CLI overrides for MUSN page bridge
dev_args=()
while [[ $# -gt 0 ]]; do
  case "$1" in
    --musn-page-room) export MUSN_PAGE_ROOM="$2"; shift 2 ;;
    --musn-page-agent) export MUSN_PAGE_AGENT="$2"; shift 2 ;;
    --musn-page-ignore) export MUSN_PAGE_IGNORE="$2"; shift 2 ;;
    --musn-page-poll) export MUSN_PAGE_POLL="$2"; shift 2 ;;
    --musn-page-timeout) export MUSN_PAGE_TIMEOUT="$2"; shift 2 ;;
    --musn-page-disable) export FUTON3_MUSN_PAGE=0; shift ;;
    --musn-page-enable) export FUTON3_MUSN_PAGE=1; shift ;;
    *) dev_args+=("$1"); shift ;;
  esac
done
set -- "${dev_args[@]}"

# Enable Drawbridge by default for dev (hot-reloading on port 6767)
export FUTON3_DRAWBRIDGE="${FUTON3_DRAWBRIDGE:-1}"
export FUTON3_CODEX_DRAWBRIDGE="${FUTON3_CODEX_DRAWBRIDGE:-1}"
export FUTON3_CLAUDE_DRAWBRIDGE="${FUTON3_CLAUDE_DRAWBRIDGE:-1}"

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

codex_drawbridge_pid=""
claude_drawbridge_pid=""
musn_page_pid=""
futon1a_pid=""
# Agency now runs inside MUSN JVM (see f2.musn/start-agency!)
# Set FUTON3_AGENCY=0 to disable it

cleanup() {
  if [[ -n "${codex_drawbridge_pid}" ]]; then
    kill "${codex_drawbridge_pid}" >/dev/null 2>&1 || true
  fi
  if [[ -n "${claude_drawbridge_pid}" ]]; then
    kill "${claude_drawbridge_pid}" >/dev/null 2>&1 || true
  fi
  if [[ -n "${musn_page_pid}" ]]; then
    kill "${musn_page_pid}" >/dev/null 2>&1 || true
  fi
  if [[ -n "${futon1a_pid}" ]]; then
    kill "${futon1a_pid}" >/dev/null 2>&1 || true
  fi
}
trap cleanup EXIT

if [[ "${FUTON3_USE_FUTON1A:-0}" == "1" ]]; then
  if port_open "${FUTON1A_PORT}"; then
    warn "[dev] Futon1a already running on port ${FUTON1A_PORT}."
  else
    info "[dev] Starting Futon1a (replacement API) on port ${FUTON1A_PORT}..."
    cat > /tmp/futon1a-dev.sh <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
cd /home/joe/code/futon1a
export FUTON1A_DATA_DIR="${FUTON1A_DATA_DIR:-/home/joe/code/storage/futon1a/default}"
export FUTON1A_PORT="${FUTON1A_PORT:-8080}"
export FUTON1A_ALLOWED_PENHOLDERS="${FUTON1A_ALLOWED_PENHOLDERS:-${USER:-cli},futon3}"
export FUTON1A_COMPAT_PENHOLDER="${FUTON1A_COMPAT_PENHOLDER:-${MODEL_PENHOLDER:-${USER:-cli}}}"
exec clojure -M -m futon1a.system
EOF
    chmod +x /tmp/futon1a-dev.sh
    setsid /tmp/futon1a-dev.sh >/tmp/futon1a-dev.log 2>&1 </dev/null &
    futon1a_pid="$!"
  fi
fi

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
agent_id="${FUTON3_CODEX_AGENT_ID:-codex}"
if [[ -n "${resume_id}" ]]; then
  resume_form=":resume-id \"${resume_id}\""
else
  resume_form=""
fi
expr='(require (quote futon3.drawbridge.codex)) (futon3.drawbridge.codex/start! {:http-port 6769 :ws-port 6771 :agent-id "'"${agent_id}"'" '"${resume_form}"' :agency-ws-url "ws://localhost:7070/agency/ws?agent-id='"${agent_id}"'" :register-local? false}) (Thread/sleep 1000000000)'
exec clojure -M -e "${expr}"
EOF
    chmod +x /tmp/codex-drawbridge.sh
    setsid /tmp/codex-drawbridge.sh >/tmp/codex-drawbridge.log 2>&1 </dev/null &
    codex_drawbridge_pid="$!"
  fi
fi

if [[ "${FUTON3_CLAUDE_DRAWBRIDGE:-1}" != "0" ]]; then
  if port_open 6768; then
    warn "[dev] Claude drawbridge already running on port 6768."
  else
    info "[dev] Starting Claude drawbridge on ports 6768/6770..."
    cat > /tmp/claude-drawbridge.sh <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
cd /home/joe/code/futon3
resume_id="${FUTON3_CLAUDE_SESSION_ID:-}"
agent_id="${FUTON3_CLAUDE_AGENT_ID:-claude}"
if [[ -n "${resume_id}" ]]; then
  resume_form=":resume-id \"${resume_id}\""
else
  resume_form=""
fi
expr='(require (quote futon3.drawbridge.claude)) (futon3.drawbridge.claude/start! {:http-port 6768 :ws-port 6770 :agent-id "'"${agent_id}"'" '"${resume_form}"' :agency-ws-url "ws://localhost:7070/agency/ws?agent-id='"${agent_id}"'" :register-local? false}) (Thread/sleep 1000000000)'
exec clojure -M -e "${expr}"
EOF
    chmod +x /tmp/claude-drawbridge.sh
    setsid /tmp/claude-drawbridge.sh >/tmp/claude-drawbridge.log 2>&1 </dev/null &
    claude_drawbridge_pid="$!"
  fi
fi

# Bridge MUSN chat -> Agency pages (enabled by default)
if [[ "${FUTON3_MUSN_PAGE:-1}" == "1" ]]; then
  page_agents="${MUSN_PAGE_AGENTS:-${MUSN_PAGE_AGENT:-${FUTON3_CODEX_AGENT_ID:-}}}"
  if [[ -z "${page_agents}" ]]; then
    warn "[dev] FUTON3_MUSN_PAGE=1 but MUSN_PAGE_AGENTS/MUSN_PAGE_AGENT not set; skipping."
  else
    for page_agent in ${page_agents//,/ }; do
      if [[ -z "${page_agent}" ]]; then
        continue
      fi
      info "[dev] Starting MUSN chat pager for agent=${page_agent}..."
      cat > "/tmp/musn-chat-page.${page_agent}.sh" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
cd /home/joe/code/futon3
musn_url="${MUSN_URL:-http://localhost:6065}"
agency_url="${AGENCY_URL:-http://localhost:7070}"
room="${MUSN_PAGE_ROOM:-lab}"
agent="${MUSN_PAGE_AGENT}"
poll="${MUSN_PAGE_POLL:-2.0}"
timeout_ms="${MUSN_PAGE_TIMEOUT:-30000}"
ignore="${MUSN_PAGE_IGNORE:-}"

until curl -fsS "${musn_url}/health" >/dev/null 2>&1; do
  sleep 1
done
until curl -fsS "${agency_url}/health" >/dev/null 2>&1; do
  sleep 1
done

exec ./scripts/musn-chat-page \
  --musn-url "${musn_url}" \
  --agency-url "${agency_url}" \
  --room "${room}" \
  --agent-id "${agent}" \
  --poll-interval "${poll}" \
  --timeout-ms "${timeout_ms}" \
  --ignore-nicks "${ignore}"
EOF
      chmod +x "/tmp/musn-chat-page.${page_agent}.sh"
      MUSN_PAGE_AGENT="${page_agent}" setsid "/tmp/musn-chat-page.${page_agent}.sh" >/tmp/musn-chat-page."${page_agent}".log 2>&1 </dev/null &
    done
  fi
fi

# Lint recently changed Clojure files before starting JVM
if command -v clj-kondo >/dev/null 2>&1; then
  changed_files=$(git diff --name-only HEAD~5 -- '*.clj' '*.cljs' '*.cljc' 2>/dev/null | head -20)
  if [[ -n "$changed_files" ]]; then
    info "[dev] Linting recently changed files..."
    # shellcheck disable=SC2086
    if ! clj-kondo --lint $changed_files 2>&1 | grep -E "^.*(error|warning):"; then
      info "[dev] Lint OK"
    else
      if clj-kondo --lint $changed_files 2>&1 | grep -q "error:"; then
        err "[dev] clj-kondo found errors in changed files. Fix before starting."
        exit 1
      fi
    fi
  fi
fi

if [[ -n "${JAVA_OPTS:-}" ]]; then
  clojure $JAVA_OPTS -M:dev "$@"
else
  clojure -M:dev "$@"
fi
