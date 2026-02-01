#!/usr/bin/env bash
# Ring the bell for a collaborative PAR session
#
# Usage:
#   ./scripts/par-bell.sh "Lab Upload Debrief"
#   ./scripts/par-bell.sh --title "Lab Upload Debrief" --agents fucodex,fuclaude
#
# Environment:
#   CRDT_HOST       CRDT server host (default: localhost)
#   CRDT_PORT       CRDT server port (default: 6530)
#   AGENCY_URL      Agency server URL (default: http://localhost:7070)
#   PAR_AGENTS      Comma-separated agent IDs (default: fucodex,fuclaude)

set -euo pipefail

usage() {
  cat <<'USAGE'
Usage: ./scripts/par-bell.sh [options] [title]

Ring the bell for a collaborative PAR session.

Options:
  --title <title>     PAR session title (or pass as first positional arg)
  --agents <list>     Comma-separated agent IDs (default: fucodex,fuclaude)
  --no-agents         Don't start agent peripherals (human-only PAR)
  --emacs-socket <s>  Emacs server socket name (default: server)
  -h, --help          Show this help

Environment:
  CRDT_HOST           CRDT server host (default: localhost)
  CRDT_PORT           CRDT server port (default: 6530)
  AGENCY_URL          Agency server (default: http://localhost:7070)

Examples:
  ./scripts/par-bell.sh "Lab Upload Debrief"
  ./scripts/par-bell.sh --title "Sprint Review" --agents fucodex
  PAR_AGENTS=fuclaude ./scripts/par-bell.sh "Quick Reflection"
USAGE
}

# Defaults
title=""
agents="${PAR_AGENTS:-fucodex,fuclaude}"
start_agents=1
emacs_socket="${EMACS_SOCKET:-server}"
crdt_host="${CRDT_HOST:-localhost}"
crdt_port="${CRDT_PORT:-6530}"
agency_url="${AGENCY_URL:-http://localhost:7070}"

# Parse args
while [[ $# -gt 0 ]]; do
  case "$1" in
    --title)
      title="$2"
      shift 2
      ;;
    --agents)
      agents="$2"
      shift 2
      ;;
    --no-agents)
      start_agents=0
      shift
      ;;
    --emacs-socket)
      emacs_socket="$2"
      shift 2
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    -*)
      echo "Unknown option: $1" >&2
      usage
      exit 2
      ;;
    *)
      # Positional arg = title
      title="$1"
      shift
      ;;
  esac
done

if [[ -z "$title" ]]; then
  echo "Error: PAR title is required" >&2
  usage
  exit 2
fi

# Convert agents to array
IFS=',' read -ra agent_list <<< "$agents"

echo "=== PAR Bell ==="
echo "Title: $title"
echo "CRDT: $crdt_host:$crdt_port"
echo "Agents: ${agent_list[*]}"
echo ""

# Step 1: Create PAR buffer on server Emacs via emacsclient
echo "[bell] Creating PAR buffer..."
emacsclient -s "$emacs_socket" -e "
(progn
  (require 'futon-crdt-par)
  (futon-start-joint-par \"$title\" $(printf "':%s " "${agent_list[@]}") :joe))" \
  || { echo "Error: Could not create PAR buffer. Is Emacs running with server?"; exit 1; }

echo "[bell] PAR buffer created: *PAR: $title*"
echo ""

# Step 2: Start agent peripherals
if [[ "$start_agents" == "1" ]]; then
  pids=()

  for agent in "${agent_list[@]}"; do
    echo "[bell] Starting peripheral for $agent..."

    CRDT_HOST="$crdt_host" \
    CRDT_PORT="$crdt_port" \
    AGENT_ID="$agent" \
    PAR_TITLE="$title" \
    AGENCY_URL="$agency_url" \
    emacs --batch -Q \
      -l ~/.emacs.d/elpa/crdt-*/crdt.el \
      -l ~/code/futon0/contrib/futon-par-peripheral.el \
      2>&1 | sed "s/^/[$agent] /" &

    pids+=($!)
    sleep 2  # Stagger agent starts
  done

  echo ""
  echo "[bell] Agent peripherals started. PIDs: ${pids[*]}"
  echo "[bell] Waiting for agents to contribute..."
  echo ""

  # Wait for all agents to finish
  for pid in "${pids[@]}"; do
    wait "$pid" 2>/dev/null || true
  done

  echo ""
  echo "[bell] All agents have contributed."
fi

echo ""
echo "=== PAR Session Ready ==="
echo "Buffer: *PAR: $title*"
echo ""
echo "Next steps:"
echo "  1. Review and edit the PAR in Emacs"
echo "  2. Add your own perspective to each section"
echo "  3. When done, submit: M-x futon-par-submit-multi"
echo ""
