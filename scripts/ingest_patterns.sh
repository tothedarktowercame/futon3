#!/usr/bin/env bash
# Ingest patterns from patterns-index.tsv into futon1
set -euo pipefail

API_URL="${FUTON1_API:-http://localhost:8080}"
TSV_FILE="${1:-resources/sigils/patterns-index.tsv}"

echo "Ingesting patterns from $TSV_FILE into $API_URL"

count=0
# Skip header line, read TSV
tail -n +2 "$TSV_FILE" | while IFS=$'\t' read -r pattern_id okipona truth rationale hotwords; do
  # Skip empty lines
  [ -z "$pattern_id" ] && continue

  # Create entity payload
  payload=$(jq -n \
    --arg name "$pattern_id" \
    --arg type "pattern" \
    --arg rationale "$rationale" \
    --arg sigil "$truth" \
    --arg hotwords "$hotwords" \
    '{
      name: $name,
      type: $type,
      props: {
        "pattern/rationale": $rationale,
        "pattern/sigil": $sigil,
        "pattern/hotwords": $hotwords
      }
    }')

  # POST to futon1
  result=$(curl -s -X POST "$API_URL/api/alpha/entity" \
    -H 'Content-Type: application/json' \
    -d "$payload")

  # Check result
  if echo "$result" | jq -e '.id' > /dev/null 2>&1; then
    count=$((count + 1))
    echo "[$count] Ingested: $pattern_id"
  else
    echo "WARN: Failed to ingest $pattern_id: $result"
  fi
done

echo "Done. Ingested $count patterns."
