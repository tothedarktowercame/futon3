#!/usr/bin/env bash
# Incremental pattern pipeline: index -> embeddings -> ingest
set -euo pipefail

ROOT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
cd "$ROOT_DIR"

INDEX_CACHE="${PATTERN_INDEX_CACHE:-data/pattern-index-cache.edn}"
MINILM_MODEL="${MINILM_MODEL:-sentence-transformers/all-MiniLM-L6-v2}"
MINILM_CACHE="${MINILM_CACHE:-data/pattern-embedding-minilm-cache.json}"
GLOVE_CACHE="${GLOVE_CACHE:-data/pattern-embedding-glove-cache.json}"
FASTTEXT_CACHE="${FASTTEXT_CACHE:-data/pattern-embedding-fasttext-cache.json}"

echo "[pattern] incremental index build"
clj -M -m scripts.build-pattern-index-incremental --cache "$INDEX_CACHE"

if command -v python3 >/dev/null 2>&1; then
  echo "[pattern] MiniLM embeddings (incremental)"
  python3 scripts/embed_patterns_minilm.py --incremental --model "$MINILM_MODEL" --cache "$MINILM_CACHE"

  if [[ -n "${GLOVE_PATH:-}" ]]; then
    echo "[pattern] GloVe embeddings (incremental)"
    python3 scripts/embed_patterns_glove.py --incremental --glove "$GLOVE_PATH" --cache "$GLOVE_CACHE"
  else
    echo "[pattern] GloVe embeddings skipped (set GLOVE_PATH to enable)"
  fi

  if [[ -n "${FASTTEXT_PATH:-}" ]]; then
    echo "[pattern] fastText embeddings (incremental)"
    python3 scripts/embed_patterns_fasttext.py --incremental --fasttext "$FASTTEXT_PATH" --cache "$FASTTEXT_CACHE"
  else
    echo "[pattern] fastText embeddings skipped (set FASTTEXT_PATH to enable)"
  fi
else
  echo "[pattern] python3 not available; embeddings skipped"
fi

echo "[pattern] ingest patterns"
./scripts/ingest_patterns.sh
