#!/usr/bin/env bash
# Ingest patterns from patterns-index.tsv into futon1
set -euo pipefail

API_URL="${FUTON1_API:-http://localhost:8080}"
TSV_FILE="${1:-resources/sigils/patterns-index.tsv}"
CACHE_FILE="${PATTERN_INGEST_CACHE:-data/pattern-ingest-cache.edn}"

echo "Ingesting patterns from $TSV_FILE into $API_URL"
echo "Using ingest cache: $CACHE_FILE"

declare -A cache
tmp_cache=$(mktemp)
if [[ -f "$CACHE_FILE" ]]; then
  clojure -e "(require '[clojure.edn :as edn] '[clojure.string :as str]) (try (let [m (edn/read-string (slurp \"$CACHE_FILE\"))] (doseq [[k v] m] (println k \"\t\" v))) (catch Exception _ nil))" > "$tmp_cache" || true
fi
if [[ -s "$tmp_cache" ]]; then
  while IFS=$'\t' read -r key val; do
    if [[ -n "$key" && -n "$val" ]]; then
      cache["$key"]="$val"
    fi
  done < "$tmp_cache"
fi
rm -f "$tmp_cache"

count=0
skipped=0
# Skip header line, read TSV
while IFS=$'\t' read -r pattern_id okipona truth rationale hotwords; do
  # Skip empty lines
  [ -z "$pattern_id" ] && continue

  row_hash=$(printf "%s\t%s\t%s\t%s\t%s" "$pattern_id" "$okipona" "$truth" "$rationale" "$hotwords" | sha256sum | awk '{print $1}')
  if [[ "${cache[$pattern_id]-}" == "$row_hash" ]]; then
    skipped=$((skipped + 1))
    continue
  fi

  lang="${pattern_id%%/*}"
  lang_name="pattern-language/${lang}"

  # Ensure language -> pattern include relation so invariants do not fail.
  rel_payload=$(jq -n \
    --arg rel_type ":arxana/scholium" \
    --arg rel_note ":pattern-language/includes" \
    --arg src_name "$lang_name" \
    --arg dst_name "$pattern_id" \
    '{
      type: $rel_type,
      provenance: {note: $rel_note},
      src: {name: $src_name, type: ":pattern/language"},
      dst: {name: $dst_name, type: ":pattern/library"}
    }')

  rel_result=$(curl -s -X POST "$API_URL/api/alpha/relation" \
    -H 'Content-Type: application/json' \
    -d "$rel_payload")

  if ! echo "$rel_result" | jq -e '.relation.id // .result.relation.id // .relation // .result.relation' > /dev/null 2>&1; then
    echo "WARN: Failed to ensure relation $lang_name -> $pattern_id: $rel_result"
  fi

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
  if echo "$result" | jq -e '.id // .entity.id' > /dev/null 2>&1; then
    count=$((count + 1))
    cache["$pattern_id"]="$row_hash"
    echo "[$count] Ingested: $pattern_id"
  else
    echo "WARN: Failed to ingest $pattern_id: $result"
  fi
done < <(tail -n +2 "$TSV_FILE")

tmp_out=$(mktemp)
for key in "${!cache[@]}"; do
  printf "%s\t%s\n" "$key" "${cache[$key]}" >> "$tmp_out"
done
sort "$tmp_out" -o "$tmp_out"
mkdir -p "$(dirname "$CACHE_FILE")"
clojure -M -e "(require '[clojure.string :as str]) (let [lines (str/split-lines (slurp \"$tmp_out\")) m (into {} (keep (fn [line] (when (seq line) (let [[k v] (str/split line #\"\\t\" 2)] [k v])))) lines)] (spit \"$CACHE_FILE\" (pr-str m)))" || true
rm -f "$tmp_out"

echo "Done. Ingested $count patterns. Skipped $skipped cached patterns."
