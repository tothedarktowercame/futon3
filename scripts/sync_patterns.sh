#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

# Load .env if present (avoid nounset errors while sourcing).
if [[ -f "${ROOT}/.env" ]]; then
  set +u
  # shellcheck disable=SC1091
  source "${ROOT}/.env"
  set -u
fi

API_BASE="${FUTON1_API_BASE:-${FUTON1_API:-http://localhost:8080}}"
if [[ "${API_BASE}" =~ /api/ ]]; then
  API_ROOT="${API_BASE%/}"
else
  API_ROOT="${API_BASE%/}/api/alpha"
fi

if [[ -z "${FUTON1_SYNC_ALLOW_RUNNING:-}" && -x "$(command -v curl)" ]]; then
  if curl -fsS --max-time 1 "${API_ROOT}/meta/model" >/dev/null 2>&1; then
    echo "[sync-patterns] Futon1 API appears to be running at ${API_ROOT}."
    echo "[sync-patterns] Stop dev (or set FUTON1_SYNC_ALLOW_RUNNING=1) before syncing to avoid XTDB lock issues."
    exit 1
  fi
fi

FUTON1_ROOT="${FUTON1_ROOT:-${ROOT}/../futon1}"
DATA_DIR="${BASIC_CHAT_DATA_DIR:-${ROOT}/data/default}"
PROFILE="${ALPHA_PROFILE:-default}"

if [[ ! -d "${FUTON1_ROOT}" ]]; then
  echo "[sync-patterns] FUTON1_ROOT not found: ${FUTON1_ROOT}"
  exit 1
fi

mkdir -p "${DATA_DIR}"

echo "[sync-patterns] Building pattern index..."
( cd "${ROOT}" && clojure -M -m scripts.build-pattern-index )

echo "[sync-patterns] Bootstrapping penholder registry in ${DATA_DIR}..."
(
  cd "${FUTON1_ROOT}"
  BASIC_CHAT_DATA_DIR="${DATA_DIR}" \
  ALPHA_PROFILE="${PROFILE}" \
  MODEL_PENHOLDER="${MODEL_PENHOLDER:-${USER:-cli}}" \
  MODEL_VERIFY_ON_WRITE=0 \
  clojure -M:scripts/graph-memory -m scripts.charon-penholder-bootstrap
)

echo "[sync-patterns] Ingesting Futon3 patterns into Futon1 store..."
(
  cd "${FUTON1_ROOT}"
  BASIC_CHAT_DATA_DIR="${DATA_DIR}" \
  ALPHA_PROFILE="${PROFILE}" \
  FUTON3_ROOT="${ROOT}" \
  MODEL_VERIFY_ON_WRITE=0 \
  clojure -M:scripts/ingest-futon3 --profile "${PROFILE}"
)

echo "[sync-patterns] Done. Restart the Futon1 API (make dev) to load the new patterns."
