#!/usr/bin/env bash
set -euo pipefail

# Futon3 consolidated dev server
#
# All services now run in a single JVM via f2.musn:
#   - Transport (HUD) on port 5050
#   - UI on port 6060
#   - MUSN HTTP on port 6065
#   - IRC bridge on port 6667
#   - Chat supervisor (polling)
#
# Environment variables to disable services:
#   FUTON3_MUSN_HTTP=0       - disable MUSN HTTP service
#   FUTON3_IRC_BRIDGE=0      - disable IRC bridge
#   FUTON3_CHAT_SUPERVISOR=0 - disable chat supervisor
#
# Example: run without chat supervisor (for manual fuclaude testing)
#   FUTON3_CHAT_SUPERVISOR=0 ./scripts/dev.sh

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
if [[ -n "${JAVA_OPTS:-}" ]]; then
  exec clojure $JAVA_OPTS -M:dev "$@"
else
  exec clojure -M:dev "$@"
fi
