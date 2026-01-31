#!/bin/bash
# Test WSS connection to lab-ws through nginx
# Run from laptop to diagnose 400 errors

SERVER="${1:-172-236-28-208.ip.linodeusercontent.com}"
PORT="${2:-5057}"
PATH_PARAM="${3:-/tmp/test.jsonl}"

echo "Testing WSS connection to $SERVER:$PORT"
echo "Path: $PATH_PARAM"
echo "---"

curl -v --no-buffer \
  -H "Connection: Upgrade" \
  -H "Upgrade: websocket" \
  -H "Sec-WebSocket-Version: 13" \
  -H "Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==" \
  "https://${SERVER}:${PORT}?path=$(python3 -c "import urllib.parse; print(urllib.parse.quote('$PATH_PARAM'))")" \
  2>&1 | head -60
