#!/usr/bin/env bash
set -euo pipefail

touch /tmp/musn_stream.log

clojure -M -m futon3.musn.http &
musn_http_pid=$!

MUSN_URL="${FUTON3_MUSN_URL:-http://localhost:6065}"
clojure -M -m scripts.musn-irc-bridge --musn-url "$MUSN_URL" &
irc_bridge_pid=$!

clojure -M -m scripts.musn-chat-supervisor --musn-url "$MUSN_URL" --no-sandbox --approval-policy never &
chat_supervisor_pid=$!

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
