#!/usr/bin/env bash
set -euo pipefail

ROOT="${HOME}/code/futon3"
LOG_DIR="${HOME}/code/storage/futon3/phase-5-signatures"
LOG_FILE="${LOG_DIR}/weekly.log"

mkdir -p "${LOG_DIR}"

{
  printf '=== %s phase_5_signatures --all-labels start ===\n' "$(date -Is)"
  cd "${ROOT}"
  bb scripts/phase_5_signatures.clj --all-labels
  printf '=== %s phase_5_signatures --all-labels done ===\n' "$(date -Is)"
} >> "${LOG_FILE}" 2>&1
