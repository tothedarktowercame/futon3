#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
MMD_CONF="$ROOT/scripts/mmdc-puppeteer.json"
PYTHON_BIN="${PYTHON_BIN:-python3}"

if ! "$ROOT/scripts/build_devmaps.sh"; then
  echo "Failed to build baseline devmaps artifact" >&2
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

WORK_DIR="$(dirname "$ARTIFACT_DIR")"
: "${TEXMFVAR:=$WORK_DIR/.texlive2025/texmf-var}"
: "${TEXMFCACHE:=.}"
mkdir -p "$TEXMFVAR"
HOME_VALUE="${DEVMAPS_HOME:-$WORK_DIR}"

normalize_futon_id() {
  local raw="${1,,}"
  raw="${raw#futon}"
  raw="${raw#f}"
  if [[ ! "$raw" =~ ^[0-7]$ ]]; then
    echo "Invalid futon id: $1 (expected 0-7, f0-f7, or futon0-futon7)" >&2
    return 1
  fi
  printf '%s\n' "$raw"
}

compile_tex() {
  local tex_source="$1"
  (
    cd "$WORK_DIR"
    mkdir -p "$TEXMFCACHE"
    export TEXMFVAR TEXMFCACHE HOME="$HOME_VALUE"
    exec lualatex -interaction=nonstopmode -halt-on-error \
      -output-directory "$ARTIFACT_DIR" \
      "$tex_source"
  )
}

render_mermaid_if_present() {
  local mmd_source="$1"
  local out_png="$2"
  if [[ ! -f "$mmd_source" ]]; then
    return 0
  fi
  local args=(-i "$mmd_source" -o "$out_png")
  if [[ -f "$MMD_CONF" ]]; then
    args+=(-p "$MMD_CONF")
  fi
  if command -v mmdc >/dev/null 2>&1; then
    if ! mmdc "${args[@]}"; then
      echo "Warning: mmdc failed for $mmd_source" >&2
    fi
  fi
}

if [[ $# -gt 0 ]]; then
  FUTONS=("$@")
else
  FUTONS=(0 1 2 3 4 5 6 7)
fi

for requested in "${FUTONS[@]}"; do
  fid="$(normalize_futon_id "$requested")"
  stem="devmaps-futon${fid}"
  tex_path="$ARTIFACT_DIR/${stem}.tex"
  mmd_path="$ARTIFACT_DIR/${stem}-deps.mmd"
  png_path="$ARTIFACT_DIR/${stem}-deps.png"

  "$PYTHON_BIN" "$ROOT/scripts/make_devmaps_tex.py" \
    --futons "$fid" \
    --output "$tex_path" \
    --mermaid-output "$mmd_path"

  compile_tex "$tex_path"
  render_mermaid_if_present "$mmd_path" "$png_path"
  echo "Built $ARTIFACT_DIR/${stem}.pdf"
done
