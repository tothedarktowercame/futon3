#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
LAB_URL="${LAB_URL:-http://localhost:5050}"
MUSN_URL="${MUSN_URL:-http://localhost:6065}"
SESSION_TS="$(date -u +%Y%m%dT%H%M%SZ)"
SESSION_ID="${SESSION_ID:-codex-parity-${SESSION_TS}}"

failures=0

request() {
  local method="$1"
  local url="$2"
  local data="${3:-}"
  local tmp
  tmp="$(mktemp)"

  if [[ -n "$data" ]]; then
    code="$(curl -sS -o "$tmp" -w "%{http_code}" -X "$method" \
      -H "Content-Type: application/json" \
      -d "$data" \
      "$url" || true)"
  else
    code="$(curl -sS -o "$tmp" -w "%{http_code}" -X "$method" "$url" || true)"
  fi

  echo "$code" "$tmp"
}

check_status() {
  local name="$1"
  local expect="$2"
  local code="$3"
  local body="$4"

  if [[ "$code" != "$expect" ]]; then
    echo "FAIL $name: expected $expect, got $code"
    echo "Body: $(cat "$body")"
    failures=$((failures + 1))
    return 1
  fi

  echo "OK   $name ($code)"
}

check_ok_field() {
  local name="$1"
  local body="$2"
  python3 - "$body" "$name" <<'PY'
import json
import sys

path = sys.argv[1]
name = sys.argv[2]
try:
    with open(path, "r", encoding="utf-8") as f:
        data = json.load(f)
except Exception as exc:
    print(f"FAIL {name}: invalid json ({exc})")
    sys.exit(1)
if isinstance(data, dict):
    ok = data.get("ok")
else:
    ok = None
if ok is True:
    print(f"OK   {name} ok=true")
    sys.exit(0)
print(f"FAIL {name}: ok={ok!r}")
sys.exit(1)
PY
}

echo "Parity check: LAB_URL=$LAB_URL MUSN_URL=$MUSN_URL"

read -r code body < <(request GET "$LAB_URL/lab/anchors/qa-arxana-test-2026-02-02")
check_status "arxana anchors (existing)" 200 "$code" "$body" || true

read -r code body < <(request POST "$MUSN_URL/musn/session/create" \
  "{\"session/id\":\"$SESSION_ID\",\"session/source\":\"codex-parity\",\"session/project\":\"futon3\"}")
check_status "musn session create" 200 "$code" "$body" || true
check_ok_field "musn session create" "$body" || failures=$((failures + 1))

read -r code body < <(request POST "$MUSN_URL/musn/par" \
  "{\"session/id\":\"$SESSION_ID\",\"par/questions\":{\"intention\":\"reflect parity test\",\"happening\":\"par endpoint\",\"learned\":\"ok\"},\"par/tags\":[\"test\",\"par\"]}")
check_status "musn par" 200 "$code" "$body" || true
check_ok_field "musn par" "$body" || failures=$((failures + 1))
SESSION_FILE="${ROOT}/lab/sessions/${SESSION_ID}.edn"
if [[ -f "$SESSION_FILE" ]]; then
  if rg -q "session/par" "$SESSION_FILE"; then
    echo "OK   musn par appended"
  else
    echo "FAIL musn par appended: no session/par event"
    failures=$((failures + 1))
  fi
else
  echo "FAIL musn par appended: session file not found at ${SESSION_FILE}"
  failures=$((failures + 1))
fi

read -r code body < <(request POST "$LAB_URL/lab/anchor/create" \
  "{\"session-id\":\"$SESSION_ID\",\"turn\":1,\"type\":\"artifact\",\"content\":\"codex parity anchor\",\"author\":\"codex\",\"note\":\"parity-check\"}")
check_status "arxana anchor create" 200 "$code" "$body" || true
check_ok_field "arxana anchor create" "$body" || failures=$((failures + 1))

read -r code body < <(request GET "$LAB_URL/lab/anchors/$SESSION_ID")
check_status "arxana anchors (new)" 200 "$code" "$body" || true

read -r code body < <(request GET "$MUSN_URL/rap")
check_status "musn rap" 200 "$code" "$body" || true

read -r code body < <(request POST "$MUSN_URL/musn/patterns/search" \
  "{\"intent\":\"coherence\",\"limit\":3}")
check_status "musn pattern search" 200 "$code" "$body" || true
check_ok_field "musn pattern search" "$body" || failures=$((failures + 1))

read -r code body < <(request GET "$MUSN_URL/musn/dashboard")
if [[ "$code" == "200" ]]; then
  echo "OK   musn dashboard ($code)"
else
  echo "FAIL musn dashboard: expected 200, got $code"
  echo "Body: $(cat "$body")"
  failures=$((failures + 1))
fi

if [[ "$failures" -gt 0 ]]; then
  echo "Parity check failed: $failures issue(s)"
  exit 1
fi

echo "Parity check passed"
