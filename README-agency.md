# Futon3 Agency

Agency is a thin HTTP service that turns Forum/bridge requests into
agent runs. It manages:
- **Peripherals** (prompt templates and runners)
- **Agent state** (rolling summary, recent messages, current thread id)
- **CLI execution** of a model runner (default `codex` CLI)
- **Optional Forum and MUSN callbacks**

It is not a scheduler or a daemon manager — it simply accepts a request,
executes an agent run, updates state, and returns a response.

## Start the service

```bash
./scripts/agency
```

By default it listens on port `7070`. Override with:

```bash
AGENCY_PORT=7070 ./scripts/agency
```

## HTTP API

Base URL: `http://localhost:7070`

### POST /agency/run
Run an agent with a peripheral.

Request body:
```json
{
  "agent-id": "fuclaude",
  "peripheral": "chat",
  "prompt": "Summarize the last 10 posts.",
  "forum": {
    "server": "http://localhost:5050",
    "thread-id": "t-123bcc8e",
    "author": "fuclaude",
    "token": "<optional>"
  },
  "musn": {
    "url": "http://localhost:6065",
    "session-id": "s-abc123"
  },
  "resume-id": "<optional>"
}
```

Response (success):
```json
{
  "ok": true,
  "agent-id": "fuclaude",
  "thread-id": "<resume-id or new thread>",
  "response": "...",
  "usage": {"input_tokens": 123, "output_tokens": 456, "total_tokens": 579}
}
```

Notes:
- If `forum` is provided, Agency will post the response back to the thread.
- If `resume-id` (or `thread-id`) is provided, the agent continues its prior thread.
- `musn` is optional; if provided, rollover events are emitted to MUSN.

### POST /agency/rollover
Force a context rollover for an agent.

```json
{ "agent-id": "fuclaude", "reason": "manual" }
```

### GET /agency/state?agent-id=fuclaude
Returns the current agent state (summary, recent messages, current thread id).

### GET /agency/threads
Returns the known agent list with current thread ids and last-active timestamps.

### GET /health
Simple health probe.

## Peripherals

Peripherals are prompt templates and behavior configs.
They live in `resources/agency/peripherals.edn` and can be overridden via
`AGENCY_PERIPHERALS_PATH`.

Built-ins:
- `chat` – normal assistant replies
- `pattern-search` – uses futon3a portal scripts to choose a pattern
- `forum-agent` – tool-using agent that is expected to post back to the forum

## State and rollover

State is stored under `lab/agency/<agent-id>.edn` by default and includes:
- Rolling summary
- Recent messages
- Current thread id
- Token usage
- Rollover flags

Rollover happens when usage exceeds the configured threshold, or manually via
`/agency/rollover`. When rolling over, Agency updates the summary and clears
recent messages, then continues in a fresh thread.

## Environment variables

Core:
- `AGENCY_PORT` (default 7070)
- `AGENCY_CODEX_BIN` (default `codex`) – CLI used to run the model
- `AGENCY_WORKDIR` (default current directory)
- `AGENCY_STATE_DIR` (default `lab/agency`)
- `AGENCY_LAB_ROOT` (default `lab`)

Context controls:
- `AGENCY_CONTEXT_MAX_TOKENS` (default 12000)
- `AGENCY_CONTEXT_ROLLOVER_RATIO` (default 0.85)
- `AGENCY_SUMMARY_WORDS` (default 250)
- `AGENCY_CONTEXT_RECENT_MESSAGES` (default 12)
- `AGENCY_RETRY_ON_OVERFLOW` (default true)

Execution controls:
- `AGENCY_APPROVAL_POLICY` (passed as `--permission-mode`)
- `AGENCY_NO_SANDBOX` (adds `--dangerously-skip-permissions`)
- `AGENCY_SKIP_GIT_CHECK` (default true; used by higher-level tooling)

Forum integration:
- `AGENCY_FORUM_SERVER` or `FORUM_SERVER`
- `AGENCY_FORUM_TOKEN` or `FORUM_TOKEN`

Peripherals:
- `AGENCY_PERIPHERALS_PATH` (override `resources/agency/peripherals.edn`)
- `AGENCY_FUTON3A_ROOT` (used by the `pattern-search` peripheral)

## Typical flows

### Forum → Agency → Forum (on-demand)
The forum can dispatch tasks to Agency directly:

```bash
curl -s -X POST \
  http://localhost:5050/forum/thread/t-123bcc8e/dispatch \
  -H 'Content-Type: application/json' \
  -d '{"peripheral":"chat","agent-id":"fuclaude","prompt":"Give status."}'
```

### Forum bridge → Agency (real-time)
Use a forum bridge to stream posts into Agency:

```bash
FORUM_THREAD=t-123bcc8e \
FORUM_MENTION=@fuclaude \
AGENCY_SERVER=http://localhost:7070 \
AGENCY_AGENT_ID=fuclaude \
./scripts/forum-bridge-fuclaude.clj
```

## Agent WebSocket (Walkie-Talkie)

Agency provides a WebSocket endpoint for real-time agent-to-agent communication.

### Connecting

```
ws://localhost:7070/agency/ws?agent-id=<your-agent-id>
```

Example with websocat:
```bash
websocat "ws://localhost:7070/agency/ws?agent-id=codex2"
```

### Message Protocol

All messages are JSON. You'll receive:

| Type | Description | Response |
|------|-------------|----------|
| `ping` | Keepalive | Send `{"type":"pong"}` |
| `bell` | Async notification from another agent | None required |
| `page` | Sync request expecting response | Send `{"type":"page-response",...}` |

### HTTP Endpoints for Agent Communication

**Check connected agents:**
```bash
curl -s http://localhost:7070/agency/connected | jq '.agents'
```

**Ring bell (async notification):**
```bash
curl -s -X POST http://localhost:7070/agency/bell \
  -H "Content-Type: application/json" \
  -d '{"agent-id":"codex","type":"handoff","payload":{"from":"claude","message":"Ready for review"}}'
```

**Page agent (sync, waits for response):**
```bash
curl -s -X POST http://localhost:7070/agency/page \
  -H "Content-Type: application/json" \
  -d '{"agent-id":"codex","prompt":"Status?","timeout-ms":30000}'
```

**Kick agent (disconnect):**
```bash
curl -s -X POST http://localhost:7070/agency/kick/codex
```

### Using the Chat-Page Bridge

For agents that can't maintain a WebSocket, use the polling bridge:

```bash
cd /home/joe/code/futon3
./scripts/musn-chat-page \
  --agent-id codex2 \
  --room futon \
  --musn-url http://localhost:6065 \
  --agency-url http://localhost:7070
```

### Codex Drawbridge Auto-Connection

When running `make dev`, a Codex Drawbridge automatically connects as `codex`.
To disable this (freeing the slot for a real Codex instance):

```bash
FUTON3_CODEX_DRAWBRIDGE=0 make dev
```

Or use a different agent-id for your second Codex (e.g., `codex2`).

## Agent Registry (Unified Routing)

Agency supports registering multiple Claude and Codex agents in a single JVM
via the unified agent registry. This eliminates the need for separate Drawbridge
processes per agent.

### Registering Agents (REPL)

```clojure
(require '[futon3.agency.agents :as agents])

;; Register a Codex agent (one-shot exec)
(agents/register-codex! "codex-1")

;; Register a Claude agent (persistent subprocess)
(agents/register-claude! "claude-1")

;; Register with session resume
(agents/register-codex! "codex-2" {:session-id "thread_abc123"})

;; Register a mock agent for testing
(agents/register-mock! "test-echo")
```

### Invoking Registered Agents

Registered agents can be invoked via HTTP:

```bash
curl -s -X POST http://localhost:7070/agency/page \
  -H "Content-Type: application/json" \
  -d '{"agent-id":"codex-1","prompt":"Say hello in 5 words"}'
```

Response includes the source:
```json
{"ok":true,"response":"Hello there, nice to meet!","session-id":"thread_xyz","source":"registry"}
```

### Registry Status

```bash
# List all connected agents (registry + WebSocket)
curl -s http://localhost:7070/agency/connected | jq

# Registry agents only
curl -s http://localhost:7070/agency/connected?type=registry | jq

# Full registry status
curl -s http://localhost:7070/agency/registry | jq
```

### Priority Order

When paging an agent, Agency checks in order:
1. **Registry** - Direct invocation via `invoke-agent!` (fastest)
2. **Local handlers** - Legacy Drawbridge handlers
3. **WebSocket** - Remote agents connected via WS

### Codex Quickstart

To register multiple Codex agents in Agency:

```clojure
;; In Agency REPL (scripts/agency-repl or nrepl)
(require '[futon3.agency.agents :as agents])

;; Register multiple Codex instances
(agents/register-codex! "codex-review")
(agents/register-codex! "codex-build")
(agents/register-codex! "codex-test")

;; Check they're registered
(agents/list-agents)
;; => ("codex-review" "codex-build" "codex-test")

;; Page one of them
(agents/invoke! "codex-review" "Review the latest commit")
```

From the command line:
```bash
# Page a specific Codex
curl -s -X POST http://localhost:7070/agency/page \
  -H "Content-Type: application/json" \
  -d '{"agent-id":"codex-review","prompt":"Review src/foo.clj for bugs"}'
```

### Cleanup

```clojure
;; Unregister a single agent
(agents/unregister! "codex-review")

;; Shutdown all registered agents
(agents/shutdown-all!)
```

## Security

Agency does not enforce auth by default. If you expose it beyond localhost,
add a reverse proxy with auth or a network ACL. The forum bridge will pass
`X-Agency-Token` if you choose to add validation on top.
