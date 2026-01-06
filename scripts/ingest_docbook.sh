#!/usr/bin/env bash
# Ingest docbook entries from futon4/docs/docbook into futon1
set -euo pipefail

API_URL="${FUTON1_API:-http://localhost:8080}"
DOCBOOK_ROOT="${1:-/home/joe/code/futon4/docs/docbook}"

echo "Ingesting docbook entries from $DOCBOOK_ROOT into $API_URL"

count=0

# Process each futon book
for book in futon0 futon1 futon2 futon3 futon4; do
  book_dir="$DOCBOOK_ROOT/$book"
  [ -d "$book_dir" ] || continue

  echo "Processing book: $book"

  for org_file in "$book_dir"/*.org; do
    [ -f "$org_file" ] || continue

    # Extract metadata from org file
    filename=$(basename "$org_file")
    doc_id=$(grep -m1 ':DOC_ID:' "$org_file" 2>/dev/null | sed 's/.*:DOC_ID: *//' || echo "$filename")
    entry_id=$(grep -m1 ':ENTRY_ID:' "$org_file" 2>/dev/null | sed 's/.*:ENTRY_ID: *//' || echo "${doc_id}::org")
    title=$(grep -m1 '#+TITLE:' "$org_file" 2>/dev/null | sed 's/.*#+TITLE: *//' || echo "$doc_id")
    outline_path=$(grep -m1 ':OUTLINE_PATH:' "$org_file" 2>/dev/null | sed 's/.*:OUTLINE_PATH: *//' || echo "$book / $title")
    path_string=$(grep -m1 ':PATH_STRING:' "$org_file" 2>/dev/null | sed 's/.*:PATH_STRING: *//' || echo "$outline_path")

    # Calculate level from outline path depth (count " / " separators + 1)
    level=$(echo "$outline_path" | awk -F' / ' '{print NF}')

    # Read body content (skip property drawer)
    body=$(awk '/^:END:/{found=1; next} found{print}' "$org_file" | head -100)

    # Create entry payload
    payload=$(jq -n \
      --arg doc_id "$doc_id" \
      --arg entry_id "$entry_id" \
      --arg title "$title" \
      --arg outline_path "$outline_path" \
      --arg path_string "$path_string" \
      --argjson level "$level" \
      --arg body "$body" \
      --arg version "org" \
      '{
        "doc/id": $doc_id,
        "doc/entry-id": $entry_id,
        "doc/title": $title,
        "doc/outline_path": $outline_path,
        "doc/path_string": $path_string,
        "doc/level": $level,
        "doc/version": $version,
        "doc/body": $body
      }')

    # POST to futon1 docbook endpoint
    result=$(curl -s -X POST "$API_URL/api/alpha/docs/$book/entry" \
      -H 'Content-Type: application/json' \
      -d "$payload")

    # Check result
    if echo "$result" | jq -e '.ok' > /dev/null 2>&1; then
      count=$((count + 1))
      echo "  [$count] Ingested: $doc_id ($title)"
    else
      echo "  WARN: Failed to ingest $doc_id: $result"
    fi
  done
done

echo "Done. Ingested $count docbook entries."
