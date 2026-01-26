#!/usr/bin/env bash
set -euo pipefail

touch /tmp/musn_stream.log

clojure -M -m futon3.musn.http &
musn_http_pid=$!

MUSN_URL="${FUTON3_MUSN_URL:-http://localhost:6065}"
clojure -M -m scripts.musn-irc-bridge --musn-url "$MUSN_URL" &
irc_bridge_pid=$!

# Chat supervisor can use fuclaude or fucodex; skip if SKIP_CHAT_SUPERVISOR=1
chat_supervisor_pid=""
if [[ "${SKIP_CHAT_SUPERVISOR:-}" != "1" ]]; then
  # Determine which agent to use (prefer fuclaude, fall back to fucodex)
  AGENT_CMD=""
  AGENT_MODE=""
  if [[ -x "./fuclaude" ]] && command -v claude &>/dev/null; then
    AGENT_CMD="./fuclaude"
    AGENT_MODE="claude"
  elif [[ -x "./fucodex" ]] && command -v codex &>/dev/null; then
    AGENT_CMD="./fucodex"
    AGENT_MODE="codex"
  fi

  if [[ -n "$AGENT_CMD" ]]; then
    echo "[dev.sh] Starting chat supervisor with $AGENT_CMD (mode=$AGENT_MODE)"
    clojure -M -m scripts.musn-chat-supervisor --musn-url "$MUSN_URL" --agent "$AGENT_CMD" --mode "$AGENT_MODE" --no-sandbox --approval-policy never &
    chat_supervisor_pid=$!
  else
    echo "[dev.sh] Skipping chat supervisor (neither claude nor codex available)"
  fi
else
  echo "[dev.sh] Skipping chat supervisor (SKIP_CHAT_SUPERVISOR=1)"
fi

cleanup() {
  if [[ -n "${musn_http_pid:-}" ]]; then
    kill "${musn_http_pid}" 2>/dev/null || true
  fi
  if [[ -n "${irc_bridge_pid:-}" ]]; then
    kill "${irc_bridge_pid}" 2>/dev/null || true
  fi
  if [[ -n "${chat_supervisor_pid:-}" ]]; then
    kill "${chat_supervisor_pid}" 2>/dev/null || true
  fi
}
trap cleanup EXIT

clojure -M:dev "$@"
