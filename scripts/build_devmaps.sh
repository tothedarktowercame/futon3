#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
MMD_CONF="$ROOT/scripts/mmdc-puppeteer.json"

PYTHON_BIN="${PYTHON_BIN:-python3}"
if ! "$PYTHON_BIN" "$ROOT/scripts/make_devmaps_tex.py"; then
  echo "Failed to regenerate devmaps.tex" >&2
  exit 1
fi

PREFERRED_DIR="$ROOT/artifacts"
FALLBACK_DIR="$ROOT/holes/artifacts"
if [[ -f "$PREFERRED_DIR/devmaps.tex" ]]; then
  ARTIFACT_DIR="$PREFERRED_DIR"
elif [[ -f "$FALLBACK_DIR/devmaps.tex" ]]; then
  ARTIFACT_DIR="$FALLBACK_DIR"
else
  echo "Error: Could not find artifacts/devmaps.tex in $PREFERRED_DIR or $FALLBACK_DIR." >&2
  exit 1
fi

TEX_SOURCE="$ARTIFACT_DIR/devmaps.tex"
WORK_DIR="$(dirname "$ARTIFACT_DIR")"

mkdir -p "$ARTIFACT_DIR"

: "${TEXMFVAR:=$WORK_DIR/.texlive2025/texmf-var}"
: "${TEXMFCACHE:=.}"
mkdir -p "$TEXMFVAR"

HOME_VALUE="${DEVMAPS_HOME:-$WORK_DIR}"

(
  cd "$WORK_DIR"
  mkdir -p "$TEXMFCACHE"
  export TEXMFVAR TEXMFCACHE HOME="$HOME_VALUE"
  exec lualatex -interaction=nonstopmode -halt-on-error \
    -output-directory "$ARTIFACT_DIR" \
    "$TEX_SOURCE"
)

MERMAID_SRC="$ARTIFACT_DIR/devmap-deps.mmd"
MERMAID_OUT="$ARTIFACT_DIR/devmap-deps.png"
if [[ -f "$MERMAID_SRC" ]]; then
  MMD_ARGS=(-i "$MERMAID_SRC" -o "$MERMAID_OUT")
  if [[ -f "$MMD_CONF" ]]; then
    MMD_ARGS+=(-p "$MMD_CONF")
  fi
  if command -v mmdc >/dev/null 2>&1; then
    if ! mmdc "${MMD_ARGS[@]}"; then
      echo "Warning: mmdc failed to render devmap-deps.png; see $MERMAID_SRC for the Mermaid source" >&2
    fi
  else
    echo "Warning: Mermaid CLI (mmdc) not found; skipping graph rendering" >&2
  fi
fi
