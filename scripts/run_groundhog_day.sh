#!/usr/bin/env bash
set -euo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
INPUT=${1:-"$ROOT_DIR/dev/groundhog_day.ndjson"}
HOST=${MUSN_HOST:-"http://localhost:5050"}
if [ ! -f "$INPUT" ]; then
  echo "Groundhog Day NDJSON not found: $INPUT" >&2
  exit 1
fi
curl -s -X POST "$HOST/musn/ingest" --data-binary "@$INPUT"
echo
