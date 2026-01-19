#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

prompt=$(cat <<'PROMPT'
Read:
- library/iching/hexagram-11-tai.flexiarg
- library/iching/hexagram-12-pi.flexiarg

Then write a short demo note:
1) One-paragraph contrast of Peace vs Obstruction.
2) Three bullet "pattern applications" for a software team.
3) One suggested next step in this repo (no code changes).
PROMPT
)

./fucodex --live --no-hud --no-pause-on-no-write \
  --clock-in iching/hexagram-11-tai \
  --clock-in iching/hexagram-12-pi \
  --intent "I Ching demo: Peace vs Obstruction" \
  exec --prompt "$prompt"
