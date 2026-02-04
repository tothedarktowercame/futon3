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
#   PAR_AGENTS      Comma-separated agent IDs (default: query Agency)

set -euo pipefail

usage() {
  cat <<'USAGE'
Usage: ./scripts/par-bell.sh [options] [title]

Ring the bell for a collaborative PAR session.

Options:
  --title <title>     PAR session title (or pass as first positional arg)
  --agents <list>     Comma-separated agent IDs (default: all from Agency)
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
agents=""  # Will be populated from Agency if not specified
start_agents=1
emacs_socket="${EMACS_SOCKET:-server}"
crdt_port="${CRDT_PORT:-6530}"
agency_url="${AGENCY_URL:-http://localhost:7070}"
agent_timeout="${PAR_AGENT_TIMEOUT:-240}"
crdt_el_path="${CRDT_EL_PATH:-}"
lang="${PAR_LANG:-${LANG:-en_US.UTF-8}}"
lc_all="${PAR_LC_ALL:-${LC_ALL:-en_US.UTF-8}}"

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

if [[ -z "$crdt_el_path" ]]; then
  crdt_el_path="$(ls -d ~/.emacs.d/elpa/crdt-*/crdt.el 2>/dev/null | head -n 1 || true)"
fi

if [[ -z "$crdt_el_path" || ! -f "$crdt_el_path" ]]; then
  echo "Error: crdt.el not found. Set CRDT_EL_PATH or install crdt.el in ~/.emacs.d/elpa/." >&2
  exit 1
fi

# If no agents specified, query Agency for connected agents
if [[ -z "$agents" ]]; then
  echo "[bell] Querying Agency for connected agents..."
  agents=$(curl -s "$agency_url/agency/connected" | jq -r '.agents // [] | join(",")')
  if [[ -z "$agents" || "$agents" == "null" ]]; then
    echo "Warning: No agents connected to Agency" >&2
    agents=""
  else
    echo "[bell] Found agents: $agents"
  fi
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

# Step 2: Ring bell for each agent via Agency
# Each agent's Drawbridge will spawn a local peripheral
if [[ "$start_agents" == "1" ]]; then
  echo "[bell] Ringing PAR bell for agents via Agency..."
  echo ""

  # Build the PAR payload - CRDT host is resolved by each agent locally
  par_payload=$(cat <<EOF
{
  "par-title": "$title",
  "crdt-port": $crdt_port,
  "agency-url": "$agency_url"
}
EOF
)

  for agent in "${agent_list[@]}"; do
    echo "[bell] === Ringing bell for $agent ==="

    # Send PAR bell to this specific agent
    bell_response=$(curl -s -X POST "$agency_url/agency/bell" \
      -H "Content-Type: application/json" \
      -d "{\"agent-id\": \"$agent\", \"type\": \"par\", \"payload\": $par_payload}")

    if echo "$bell_response" | jq -e '.ok' >/dev/null 2>&1; then
      echo "[bell] Bell sent to $agent"
    else
      echo "[bell] Failed to ring bell for $agent: $bell_response"
    fi

    # Stagger bells slightly
    sleep 1
  done

  echo ""
  echo "[bell] PAR bells sent to all agents."
  echo "[bell] Agents will spawn local peripherals and contribute via CRDT."
  echo "[bell] Watch the PAR buffer for contributions..."
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
