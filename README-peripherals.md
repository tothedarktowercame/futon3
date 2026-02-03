# Peripherals: Minimal Contract + Routing Notes

This document defines a **minimal, machine-readable contract** for Futon3
peripherals so Agency can route summons across machines reliably (server ↔ laptop).

## What Is a Peripheral?

A **peripheral** is a *constrained capability envelope* for an agent.
It represents a specific task mode with explicit inputs, tools, and expected output,
so it can be invoked locally or routed remotely with predictable behavior.

Key properties:

- **Task-scoped**: designed for a specific intent (e.g., PAR contribution).
- **Explicit I/O**: declared inputs and normalized outputs.
- **Capability-bound**: tool access and context are constrained by spec.
- **Routable**: the same peripheral can run on server or laptop via Agency.
- **Composable**: peripherals can be chained or swapped without changing callers.

## Goals

- **Portable**: same peripheral can run locally or via remote Agency.
- **Predictable I/O**: clear inputs, outputs, and expected response shape.
- **Safe routing**: server Agency can delegate without ad-hoc prompt glue.

## Peripheral Spec (EDN)

Peripheral specs should live in `resources/agency/peripherals.edn`.

```clojure
{:id :par-participant
 :entry "Join PAR and contribute a concise perspective."
 :inputs [:par/title :par/crdt-host :par/crdt-port :session/id]
 :prompt-template "PAR: {{par/title}}\nCRDT: {{par/crdt-host}}:{{par/crdt-port}}\nSession: {{session/id}}\n\nInstructions: Provide 2-4 sentences per section you can answer."
 :tools {:fs false :shell false :http false}
 :response {:type :text :field :response}}
```

### Required Keys

- `:id` — unique peripheral identifier (keyword)
- `:entry` — short description of intent
- `:inputs` — list of expected input keys (namespaced keywords)
- `:prompt-template` — template used to build prompt at runtime
- `:response` — describes output shape (minimum is `{:type :text :field :response}`)

### Optional Keys

- `:tools` — capability envelope for the peripheral
- `:context` — list of context blocks to include (e.g., `:last-turns`, `:files`)
- `:timeout-ms` — override default timeout

## Agency Run Payload

Agency is invoked with a uniform payload so routing is transparent.

```json
{
  "agent-id": "fucodex",
  "peripheral": "par-participant",
  "inputs": {
    "par/title": "Lab Upload Debrief",
    "par/crdt-host": "172-236-28-208.ip.linodeusercontent.com",
    "par/crdt-port": 6530,
    "session/id": "019c16ac-..."
  }
}
```

### Inputs vs Prompt

`/agency/run` accepts either:

- `inputs` (preferred): values used to render `:prompt-template`
- `prompt`: raw user prompt (used when no structured inputs are available)

If both are provided, `inputs` populate the template and `prompt` is appended as
the "User request" block.

### Expected Response

```json
{
  "ok": true,
  "response": "...",
  "meta": {
    "agent-id": "fucodex",
    "duration-ms": 3214
  }
}
```

## Routing: Server ↔ Laptop

The server Agency owns **routing decisions**. A simple registry (EDN) maps agent
IDs to their location.

Example `resources/agency/agents.edn`:

```clojure
{:fucodex {:location :remote :url "http://LAPTOP_HOST:7070"}
 :fuclaude {:location :local}}
```

Routing rule:

- `:local` → handle via local `/agency/run`
- `:remote` → POST the same payload to remote Agency `/agency/run`

Responses are normalized so callers don’t care where execution happened.

## par-bell Integration (Agency-only)

`par-bell.sh` should call server Agency for each agent with:

- `peripheral: "par-participant"`
- inputs: PAR title, CRDT host/port, session id

No forum snapshot/merge is required for the minimal viable path.

## Implementation: Claude Drawbridge (In-JVM)

The **Claude Drawbridge** (`src/futon3/drawbridge/claude.clj`) is an in-JVM peripheral
infrastructure that runs inside MUSN. It provides a multi-headed interface to Claude:

**Architecture:**
- **HTTP API** (port 6768): POST `/claude` to send messages
- **Java-WebSocket** (port 6770): streaming output (http-kit WebSockets are unreliable)
- **Session persistence**: `--resume <session-id>` for thread-safe continuity
- **Agency integration**: local handler registration (no WebSocket needed, same JVM)
- **IRC client**: connect to MUSN IRC bridge for chat hops

**Key Insight:** Uses `claude --print --output-format json` for one-shot invocations,
capturing `session_id` from the response for subsequent `--resume` calls. This is
thread-safe (unlike `--continue` which is directory-based).

**Usage:**
```clojure
;; Start Drawbridge with Agency registration
(futon3.drawbridge.claude/start!
  {:http-port 6768
   :ws-port 6770
   :agent-id "drawbridge"})

;; Send a message (via REPL, HTTP, or Agency bell)
(futon3.drawbridge.claude/send-input! "Hello Claude")

;; Connect to IRC for chat hop
(futon3.drawbridge.claude/connect-irc! {:room "lab" :nick "claude"})
;; ... chat happens, messages flow both ways ...
(futon3.drawbridge.claude/disconnect-irc!)
;; => transcript
```

**Chat Hop Flow:**
1. `connect-irc!` joins IRC room
2. IRC messages → Claude via `send-input!` (prefixed with `[IRC #room] <nick>`)
3. Claude responses → IRC via `send-to-irc!` (automatic)
4. `disconnect-irc!` returns transcript
5. Session continues with full memory of the conversation

**Why Java-WebSocket?** http-kit's WebSocket implementation has reliability issues
(masking problems, connection drops). Java-WebSocket (org.java-websocket) on a
separate port provides reliable streaming.

## Implementation: fuclaude-peripheral.ts (Demo)

A **demo implementation** of a multiplexed peripheral is at `scripts/fuclaude-peripheral.ts`.
This is a working proof-of-concept that demonstrates the peripheral architecture with
a "backpack" metaphor - the agent carries items that provide capabilities:

**Backpack Items:**
- **Walkie-talkie** (Agency WebSocket) - receives bells, summons from other agents
- **ID card** (session persistence) - memory across restarts via `--resume`
- **Forum notebook** (Forum WebSocket) - joins threads, auto-replies to posts
- **PAR notebook** (PAR bells) - contributes to Post-Action Reviews
- **Pattern card** (PSR/PUR) - selects patterns to guide work, records outcomes

**Architecture:**
- Connects to Agency via WebSocket at `/agency/ws`
- Multiplexes human input (readline) with Agency events (bells, summons)
- Invokes `claude` CLI for each input, preserving session via `--resume`
- All inputs feed into one conversation thread
- PSR/PUR records are logged to MUSN activity stream

**Usage:**
```bash
./scripts/fuclaude-peripheral.ts                    # fresh session
./scripts/fuclaude-peripheral.ts --resume <id>      # continue session
./scripts/fuclaude-peripheral.ts --no-agency        # human-only mode
./scripts/fuclaude-peripheral.ts --forum-thread t-abc123  # join forum thread
```

**Backpack Commands:**
```bash
/psr <query>      # Pattern Selection Record - search, select, carry
/pur [outcome]    # Pattern Use Record - record outcome, clear pattern
/pattern          # Show active pattern in backpack
```

**Demo Limitations:**
- Single-threaded blocking (execSync) - one input at a time
- Pattern selection uses Claude to pick best match (could be local scoring)
- No streaming output during Claude invocation
- Session detection relies on file modification times

This demo validates the peripheral concept. Production implementations might use:
- Async subprocess handling with proper signal management
- Direct pattern scoring without LLM round-trip for PSR
- Streaming responses for better UX
- More robust session tracking

## Implementation: fucodex-peripheral.ts

Codex peripheral wrapper is at `scripts/fucodex-peripheral.ts`.

**Usage:**
```bash
./scripts/fucodex-peripheral.ts                    # fresh session
./scripts/fucodex-peripheral.ts --resume <id>      # continue session
./scripts/fucodex-peripheral.ts --no-agency        # human-only mode
./scripts/fucodex-peripheral.ts --simple           # codex exec (no lab stream)
```

**Notes:**
- Uses `fucodex --live` so lab streams are persisted.
- `--approval-policy` defaults to `never` unless overridden.
- Avoids `--prompt` to preserve `--resume` behavior in `fucodex`.
 - `--simple` uses `codex exec` directly (no FULAB summaries / lab stream).

## Plugin Demonstrators (Claude)

Claude has a split plugin demonstrator in `plugins/` that interoperates with the
peripheral hop workflow:

- **Interactive** plugin: `plugins/futon`
  - Commands: `/futon:psr`, `/futon:pur`
  - Uses Claude Code UI (AskUserQuestion) for selection
- **Hop** plugin: `plugins/peripherals`
  - Commands: `/peripherals:par`, `/peripherals:hop`
  - Detaches to a peripheral (e.g., Emacs) and returns with the same session
- **Combined** plugin (legacy): `plugins/futon-peripherals`

**Session continuity:** The same `--resume` ID works before and after a hop, so
context is preserved across the round trip.

## Implementation Gotchas (IMPORTANT for Codex)

These are hard-won lessons from implementing `fuclaude-peripheral.ts`:

### 1. Node spawn()/exec() vs execSync for Claude CLI

**GOTCHA:** Both `spawn()` and async `exec()` hang indefinitely when piping
stdout/stderr, even though running the same command in a terminal works fine.

**Cause:** Claude CLI does something with piped stdio that prevents the normal
completion signals from firing. The callback/events never trigger.

**Solution:** Use `execSync()` with shell redirection to a temp file:
```typescript
// BAD - hangs forever (callback never fires)
exec(`claude -p '${input}'`, (err, stdout) => { ... });

// BAD - also hangs
spawn("claude", ["-p", input], { stdio: ["pipe", "pipe", "pipe"] });

// GOOD - works with file redirection
const tmpFile = `/tmp/claude-out-${Date.now()}.txt`;
execSync(`claude --permission-mode bypassPermissions -p '${input}' > ${tmpFile} 2>&1`);
const output = fs.readFileSync(tmpFile, "utf-8");
```

Note: `spawn()` with `stdio: "inherit"` also works but you can't capture output.

### 2. Permission mode for non-interactive use

**GOTCHA:** `--dangerously-skip-permissions` doesn't work reliably in subprocess.

**Solution:** Use `--permission-mode bypassPermissions` instead:
```bash
claude --permission-mode bypassPermissions -p "prompt"
```

### 3. WebSocket endpoint path

**GOTCHA:** Easy to mismatch the URL path between server and client.

Agency WebSocket is at `/agency/ws`, not `/ws`:
```typescript
// BAD
ws://localhost:7070/ws

// GOOD
ws://localhost:7070/agency/ws
```

### 4. Clojure forward declarations

**GOTCHA:** Functions used in the request handler but defined later in the file
cause "Unresolved symbol" errors.

**Solution:** Add `(declare ...)` near the top of the file:
```clojure
(declare handle-agency-ws connected-agent-ids handle-get-secret)
```

### 5. http-kit WebSocket with clj-kondo

**GOTCHA:** `http/with-channel` macro introduces bindings that clj-kondo doesn't
recognize, causing false "Unresolved symbol: channel" errors.

**Solution:** Add ignore comment:
```clojure
#_{:clj-kondo/ignore [:unresolved-symbol]}
(http/with-channel req channel ...)
```

### 6. Shebang for npx ts-node

**GOTCHA:** `#!/usr/bin/env npx ts-node` fails because env treats it as one arg.

**Solution:** Use `-S` flag to split:
```bash
#!/usr/bin/env -S npx ts-node
```

### 7. Shell escaping for prompts

**GOTCHA:** Prompts with quotes or special chars break when passed to shell.

**Solution:** Escape single quotes properly:
```typescript
const escapedInput = input.replace(/'/g, "'\\''");
const cmd = `claude -p '${escapedInput}'`;
```

### 8. Agency server must be restarted for new routes

**GOTCHA:** Adding new HTTP/WebSocket routes requires server restart - the JVM
doesn't hot-reload the handler.

### 9. Claude --print requires stdin, not CLI argument

**GOTCHA:** Passing the prompt as a CLI argument to `claude --print` causes the
process to hang. Using stdin works reliably.

**Solution:** Pipe the prompt via stdin:
```clojure
;; BAD - hangs
(ProcessBuilder. ["claude" "--print" "Hello"])

;; GOOD - works
(let [pb (ProcessBuilder. ["claude" "--print"])
      proc (.start pb)
      stdin (io/writer (.getOutputStream proc))]
  (doto stdin (.write "Hello") (.flush) (.close))
  ;; read stdout...
  )
```

### 10. --resume vs --continue for session persistence

**GOTCHA:** `--continue` uses the most recent conversation *in the current directory*,
which is not thread-safe if multiple processes share a directory.

**Solution:** Use `--resume <session-id>` with explicit session IDs:
```clojure
;; First call - no resume, capture session_id from JSON output
["claude" "--print" "--output-format" "json"]
;; Response includes: {"session_id": "abc-123", ...}

;; Subsequent calls - use --resume
["claude" "--print" "--output-format" "json" "--resume" "abc-123"]
```

### 11. http-kit WebSockets are unreliable

**GOTCHA:** http-kit's WebSocket implementation has issues with masking, frame
handling, and connection stability. Connections drop unexpectedly.

**Solution:** Use Java-WebSocket library on a separate port:
```clojure
(:import (org.java_websocket.server WebSocketServer))
;; Run on dedicated port (e.g., 6770 for streaming, separate from HTTP on 6768)
```

## Future Extensions

### Infrastructure
- **Fanout**: dispatch multiple agents in parallel with aggregate responses.
- **Routing overrides**: allow per-request `:route` for ad-hoc targeting.
- **Context fetchers**: e.g. auto-inject last N turns or lab session context.
- **Timeout envelopes**: per-peripheral timeouts and retry policies.

### Exciting Peripheral Explorations

**1. CLJ Ant in futon1**
Put Claude into an ant agent in the Active Inference ant simulation. The ant would:
- Receive sensory observations (food gradients, pheromones, nest direction)
- Use Claude to decide actions based on beliefs and goals
- Update beliefs via AIF free energy minimization
- Communicate with other ants through pheromone signals

This would test whether Claude can operate as a "brain" for an embodied AIF agent,
making decisions under uncertainty with continuous sensory streams.

**2. Smart Cursor in Emacs**
A Claude peripheral that lives in Emacs as an intelligent cursor companion:
- Watches buffer context (current file, cursor position, recent edits)
- Offers contextual suggestions without being asked
- Can be summoned with a keybinding for inline assistance
- Maintains session continuity across Emacs restarts
- Uses eldoc-style hints or overlay suggestions

Implementation: Emacs Lisp client → Drawbridge HTTP API, with buffer state
sent as context.

**3. Claude Driving Codex as "User"**
Invert the typical relationship: Claude acts as the human operator for Codex:
- Claude formulates high-level goals
- Codex executes with its tool-use capabilities
- Claude reviews Codex output and provides feedback
- Creates an interesting dynamic of LLM-to-LLM collaboration

Use case: Claude explores a problem space by directing Codex to investigate,
then synthesizes findings across multiple Codex sessions.

**4. Pattern Space Explorer (futon3a)**
A peripheral for navigating the pattern library through conversation:
- Claude browses `resources/sigils/patterns-index.tsv`
- Explores pattern relationships (hotwords, namespaces, connections)
- Proposes new patterns based on gaps discovered in practice
- Records PSR/PUR evidence as exploration progresses
- Uses futon3a's pattern engine for embedding-based similarity

This would help grow the pattern library organically through use, with Claude
as an active participant in pattern discovery and refinement.
