#!/usr/bin/env bash
set -euo pipefail

# lab_ingest: POST lab/raw JSON to Futon1.
# Usage:
#   scripts/lab_ingest.sh --file lab/raw/SESSION.json
#   scripts/lab_ingest.sh --dir lab/raw --recent 3
# Options:
#   --api-base URL   (default: http://localhost:8080/api/alpha)
#   --profile NAME  (optional X-Profile header)

API_BASE="http://localhost:8080/api/alpha"
PROFILE=""
RAW_DIR=""
RAW_FILE=""
RECENT_COUNT=""

usage() {
  echo "Usage: lab_ingest --file PATH | --dir DIR [--recent N]"
  echo ""
  echo "Options:"
  echo "  --api-base URL   API base (default: $API_BASE)"
  echo "  --profile NAME  X-Profile header"
  echo "  --file PATH     Single lab/raw JSON file"
  echo "  --dir DIR       Directory of lab/raw JSON files"
  echo "  --recent N      Only ingest N most recent files from DIR"
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --api-base)
      API_BASE="$2"
      shift 2
      ;;
    --profile)
      PROFILE="$2"
      shift 2
      ;;
    --file)
      RAW_FILE="$2"
      shift 2
      ;;
    --dir)
      RAW_DIR="$2"
      shift 2
      ;;
    --recent)
      RECENT_COUNT="$2"
      shift 2
      ;;
    --help|-h)
      usage
      exit 0
      ;;
    *)
      echo "[lab_ingest] Unknown option: $1" >&2
      usage
      exit 1
      ;;
  esac
done

header_args=()
if [[ -n "$PROFILE" ]]; then
  header_args+=(-H "X-Profile: $PROFILE")
fi

post_file() {
  local file="$1"
  if [[ ! -f "$file" ]]; then
    echo "[lab_ingest] Missing file: $file" >&2
    return 1
  fi
  curl -sS -H "Content-Type: application/json" "${header_args[@]}" \
    --data-binary @"$file" \
    "$API_BASE/lab/session"
  echo ""
}

if [[ -n "$RAW_FILE" ]]; then
  post_file "$RAW_FILE"
  exit 0
fi

if [[ -z "$RAW_DIR" ]]; then
  usage
  exit 1
fi

if [[ -n "$RECENT_COUNT" ]]; then
  mapfile -t files < <(ls -t "$RAW_DIR"/*.json 2>/dev/null | head -n "$RECENT_COUNT")
else
  mapfile -t files < <(ls -t "$RAW_DIR"/*.json 2>/dev/null)
fi

for f in "${files[@]}"; do
  post_file "$f"
done
