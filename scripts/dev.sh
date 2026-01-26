#!/usr/bin/env bash
set -euo pipefail

# Futon3 consolidated dev server
#
# All services now run in a single JVM via f2.musn:
#   - Transport (HUD) on port 5050
#   - UI on port 6060
#   - MUSN HTTP on port 6065
#   - IRC bridge on port 6667
#   - Chat supervisor (polling)
#
# Environment variables to disable services:
#   FUTON3_MUSN_HTTP=0       - disable MUSN HTTP service
#   FUTON3_IRC_BRIDGE=0      - disable IRC bridge
#   FUTON3_CHAT_SUPERVISOR=0 - disable chat supervisor
#
# Example: run without chat supervisor (for manual fuclaude testing)
#   FUTON3_CHAT_SUPERVISOR=0 ./scripts/dev.sh

touch /tmp/musn_stream.log

# Legacy env var support
if [[ "${SKIP_CHAT_SUPERVISOR:-}" == "1" ]]; then
  export FUTON3_CHAT_SUPERVISOR=0
fi

# Pass through any JVM opts for memory limits if desired
# Example: JAVA_OPTS="-Xmx1g" ./scripts/dev.sh
if [[ -n "${JAVA_OPTS:-}" ]]; then
  exec clojure $JAVA_OPTS -M:dev "$@"
else
  exec clojure -M:dev "$@"
fi
