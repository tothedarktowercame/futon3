# Agent Reference Guide

## Quick Reference

| Task | Tool | Example |
|------|------|---------|
| Validate sigil | `futon3.chops` | `(chops/validate-sigil "🐜/予")` |
| Stamp pattern | `futon3.chops` | `(chops/stamp {:id "x"} ["🐜/予"])` |
| Audit all sigils | `scripts/audit-sigils` | `./scripts/audit-sigils` |
| Lint Clojure | `clj-kondo` | `clj-kondo --lint src/ test/` |
| Hot reload | Drawbridge | See "Drawbridge hot reloading" below |
| Pattern check | `futon3.pattern-check.integration` | See "Realtime pattern checking" below |

---

## Sigil validation with 印章 (chops)

Before creating or modifying patterns, validate sigils against canonical sets using `futon3.chops`.

### Canonical sets

- **Emojis** (~124): from `resources/tokizh/tokizh.org` — each maps to a toki pona word
- **Hanzi** (256): from `resources/truth-table-8/truth-table-8.el` — each maps to an 8-bit value

Sigils encode two-word concepts: `emoji/hanzi` = `word₁/word₂`. Valid sigils use any canonical emoji + any canonical hanzi (combinatorial).

### Validating sigils

```clojure
(require '[futon3.chops :as chops])

;; Validate a single sigil
(chops/validate-sigil "🐜/予")
;; => {:valid? true, :decoded {:word1 "lili", :word2 "e", :reading "lili e"}}

;; Validate multiple
(chops/validate-sigils ["🐜/予" "🐺/内"])

;; Check if components are canonical
(chops/valid-emoji? "🐜")   ; => true
(chops/valid-hanzi? "予")   ; => true
```

### Finding `futon3.chops`

- **Code**: `src/futon3/chops.clj` (namespace `futon3.chops`)
- **Canonical sources**: `resources/tokizh/tokizh.org`, `resources/truth-table-8/truth-table-8.el`
- **Quick check** (from repo root):
  ```bash
  clj -M -e '(require (quote futon3.chops)) (println (futon3.chops/valid-emoji? "🐜"))'
  ```
- If the namespace won’t load in a bare REPL, you can force-load it:
  ```bash
  clj -M -e '(load-file "src/futon3/chops.clj") (require (quote futon3.chops)) (println (futon3.chops/valid-hanzi? "予"))'
  ```

### Stamping patterns

Before committing new patterns, stamp them to record validation:

```clojure
(chops/stamp {:pattern-id "my-pattern"} ["🐜/予" "🐺/内"])
;; => {:pattern-id "my-pattern"
;;     :chop/status :valid
;;     :chop/timestamp "2025-01-26T..."
;;     :chop/validation {...}}
```

### Auditing devmaps

Run `scripts/audit-sigils` to check all devmaps for invalid sigils:

```bash
./scripts/audit-sigils
# Sigil Audit Report
# Files checked: 9
# Invalid sigils: 0
# ✓ All sigils are canonical!
```

### Migrating invalid sigils

If you encounter invalid sigils, use `scripts/migrate-sigils`:

```bash
./scripts/migrate-sigils --dry-run  # preview changes
./scripts/migrate-sigils            # apply migrations
```

Migrations are defined in `resources/sigils/sigil-migrations.edn` with semantic rationales.

### Core vs enlarged pattern sets

- **Core patterns**: sigils where hanzi is in tokizh.org (124 chars) — fully toki pona readable
- **Enlarged patterns**: sigils using other truth-table-8 hanzi (132 chars) — emoji readable, hanzi as pure symbol

Both are valid; core patterns have richer semantic decoding.

## Maintaining devmap + pattern coherence

- Keep each futon devmap in sync with reality: when a clause is finished, cite the evidence (README section, test, tag); when it is blocked, note the dependency. Use the stack-coherence patterns (e.g. `stack-blocker-detection`) to spot empty dirs or missing artifacts and record `blocked-by[...]` until the prerequisite is done.
- Watch for repeated sigil pairs in devmaps. The pattern library (e.g. `pattern-differentiation-alarms`) treats duplicates as alarms: either split/merge the underlying patterns, add a more specific devmap-coherence flexiarg, or retire stale clauses. After adjustments, rerun the TF–IDF embedding + sigil matrices so the fake embedding stays meaningful.
- Contribute new stack-coherence patterns whenever the project needs cross-futon checks (e.g. comparing READMEs vs git logs). These global patterns should inspect hotspots—git history, README timestamps, file trees—to classify each devmap item as “done / blocked / drifting” and help the informal proof engine keep the whole stack honest.

## MUSN stuff

F2 (MUSN) — One-Page Build Brief

Scope (thin waist): Transport + routing only. F1 (graph/memory) and F3 (ants) are external via adapters.

Non-goals: No storage, no UI, no inference, no arbitrary eval.

Interfaces you must call (adapters supplied elsewhere)
;; F1
(put-event!      f1 run-id event)            ;; -> {:eid ...}
(close-session!  f1 sid)                     ;; -> {:sid ...}
(export-scenario! f1 sid)                    ;; -> {:scenario-path ...}

;; F3
(run-scenario!   f3 scenario-path policy)    ;; -> {:job-id ...}
(job-status      f3 job-id)                  ;; -> {:state :metrics?}

Message protocol (v1) — JSON lines over WS; HTTP POST fallback

hello {client,caps[]} → ack {rev,run-id}

event {msg-id,t,actor,verb,object,prov} → ack {eid,run-id}

session-close {sid} → ack {sid,run-id}

export {sid} → ack {scenario-path,run-id}

run {scenario-path|sid,policy} → ack {job-id,run-id}

status {job-id} → {state,metrics?}

bye → ack

Rules:

Idempotent on msg-id.

200 ms cap on status, 5 s on run submission.

Per-client supervision: errors never crash the server; return {ok:false,err,...}.

Every ack echoes run-id.

Handlers to implement
handle-hello(ctx,msg)         ; mint run-id, negotiate rev
handle-event(ctx,msg)         ; validate → F1.put-event!
handle-session-close(ctx,msg) ; → F1.close-session!
handle-export(ctx,msg)        ; → F1.export-scenario!
handle-run(ctx,msg)           ; resolve sid→scenario if needed → F3.run-scenario!
handle-status(ctx,msg)        ; → F3.job-status

Validation (malli/spec)

Schemas for event, session-close, export, run, status. Hard-fail on unknown fields; include detail in error.

Back-pressure

Queue or drop with {ok:false,err:"overload"} when >1k events/min locally.

Acceptance tests

Replaying 100 identical events (same msg-id) yields the same eids.

export {sid} returns a path; F2 does not touch the file.

run → job-id; status eventually done with metrics.

Killing a client doesn’t affect others; server logs a clean drop.

### MUSN guidance hygiene

- Avoid prompt injection outside MUSN-HELP. If new guidance is needed, add or update a pattern in `library/musn` instead of appending raw instructions to the HUD/prompt.
- Keep any HUD help text short; prefer pointing to MUSN patterns that agents and humans can read.
- When guidance is added, ensure it is represented as a pattern with context/IF/HOWEVER/THEN/BECAUSE/next-steps.



Repo layout (tiny)
/docs/protocol.md
/src/f2/transport.clj   ; WS/HTTP + timeouts
/src/f2/router.clj      ; decode→validate→dispatch
/src/f2/schemas.edn
/src/f2/adapters/mock.clj
/test/f2/{router_test.clj, transport_test.clj}
/dev/demo.ndjson
/scripts/dev.sh
/Makefile

Make targets
make dev    # start with mock adapters
make demo   # hello → events → session-close → export → run → status
make test   # unit + replay tests

### Emacs HUD tests
`scripts/test-elisp.sh` drives the chatgpt-shell HUD tests. Keep them gentle by supplying
`ERT_SELECTOR` (e.g., `ERT_SELECTOR='futon3-prototype-*'`) and, when running everything, consider
`ulimit -v 2097152` to avoid GUI Emacs OOMs.

## Drawbridge hot reloading

**IMPORTANT**: Always use hot-reloading instead of restarting the server. Drawbridge is enabled by default in `make dev` and runs on port 6767.

### Prerequisites

Drawbridge requires `ADMIN_TOKEN` to be set. The token is stored in `.admintoken` in the futon3 root:

```bash
# The token file location (already exists)
cat ~/code/futon3/.admintoken

# If missing, create it (one-time setup)
echo "your-secret-token" > .admintoken
```

The `make dev` script automatically loads `.admintoken` and sets `ADMIN_TOKEN`.

### Using the repl-eval helper

The `scripts/repl-eval` script is the preferred way to hot-reload code:

```bash
# Reload a file after editing (preferred - guarantees re-read from disk)
./scripts/repl-eval '(load-file "src/f2/transport.clj")'

# Alternative: require with :reload (may not pick up all changes)
./scripts/repl-eval '(require '\''f2.transport :reload)'

# Evaluate any Clojure expression
./scripts/repl-eval '(+ 1 2)'

# Check current state
./scripts/repl-eval '(count @f2.transport/claude-stream-watchers)'
```

### Common reload targets

| File | Reload command |
|------|----------------|
| Transport (port 5050) | `(load-file "src/f2/transport.clj")` |
| MUSN HTTP (port 6065) | `(load-file "src/futon3/musn/http.clj")` |
| MUSN service | `(load-file "src/futon3/musn/service.clj")` |
| Lab WebSocket (port 5056) | `(load-file "src/futon3/lab/ws.clj")` |
| Futon1 invariants | `(load-file "src/app/invariants.clj")` |
| Futon1 graph handlers | `(load-file "src/api/handlers/graph.clj")` |

**Note**: Futon1 namespaces run in the same JVM when using `make dev`, so they can be hot-reloaded via Drawbridge.

### Verifying Drawbridge is running

```bash
# Should return "6", not empty or "connection refused"
./scripts/repl-eval '(+ 1 2 3)'

# Check listening ports
ss -tlnp | grep 6767
```

If Drawbridge isn't running, check:
1. `.admintoken` file exists in futon3 root
2. `FUTON3_DRAWBRIDGE` is not set to `0`
3. Restart `make dev` to pick up changes

### Troubleshooting

**Empty response `[]` from repl-eval:**
The first nREPL call creates a session (returns empty). The `repl-eval` script handles this automatically by making multiple calls. If you're using curl directly, you need to:
1. First call with `op=clone` to get a session cookie
2. Second call with `op=clone` returns `new-session` ID
3. Third call with `op=eval` + session ID to evaluate code
4. Fourth call polls for the result

**Direct curl for debugging:**
```bash
TOKEN=$(cat .admintoken)
COOKIES=$(mktemp)

# Setup session
curl -s -c "$COOKIES" "http://localhost:6767/repl?token=$TOKEN" \
  -d "op=clone" >/dev/null

# Get session ID
SESSION=$(curl -s -c "$COOKIES" -b "$COOKIES" \
  "http://localhost:6767/repl?token=$TOKEN" \
  -d "op=clone" | jq -r '.[0]["new-session"]')

# Eval
curl -s -c "$COOKIES" -b "$COOKIES" \
  "http://localhost:6767/repl?token=$TOKEN" \
  --data-urlencode "op=eval" \
  --data-urlencode "code=(+ 1 2)" \
  --data-urlencode "session=$SESSION" >/dev/null

# Get result
curl -s -b "$COOKIES" "http://localhost:6767/repl?token=$TOKEN" \
  -d "op=clone" | jq '.[].value'

rm "$COOKIES"
```

**"forbidden" response:**
Token mismatch. Check that `.admintoken` matches what the server loaded at startup.

### When to restart instead

Some changes still require a full server restart:
- Adding new dependencies to `deps.edn`
- Changes to startup/initialization code in `f2.musn`
- Modifications to atoms/state that are initialized with `defonce`
- Adding new Java-WebSocket servers (forum-ws, lab-ws)
- **Ring middleware changes** - middleware is compiled into handler chains at startup; reloading the namespace doesn't re-wire the middleware stack

## Realtime pattern checking

The pattern-check loop validates sigils and PSR/PUR events from live IRC chat without blocking the relay. It writes read-only JSONL logs.

### Running the pattern checker

```bash
# Connect to IRC bridge and write JSONL output
clj -M -m futon3.pattern-check.integration \
  --host localhost \
  --port 6680 \
  --room lab \
  --password $MUSN_IRC_PASSWORD \
  --output /tmp/musn_pattern_checks.jsonl
```

### REPL usage

```clojure
(require '[futon3.pattern-check.integration :as pci])

;; Start standalone checker (no IRC)
(def checker (pci/start! {:jsonl-path "/tmp/test_checks.jsonl"}))

;; Manually ingest lines
(pci/ingest! checker (pci/normalize-line "Message with 🐜/予" :source :irc))

;; Stop
(pci/stop! checker)

;; Or start with IRC connection
(def listener (pci/start-irc-listener!
               {:host "localhost" :port 6680 :room "lab"
                :password "secret" :jsonl-path "/tmp/checks.jsonl"}))
(pci/stop-irc-listener! listener)
```

### What it checks

- **Sigils**: Extracts `emoji/hanzi` and `tokipona/hanzi` patterns, validates via `futon3.chops`
- **PSR events**: Detects `:pattern/selection-claimed` and `:turn/select` structured events
- **PUR events**: Detects `:pattern/use-claimed` and `:turn/use` structured events
- **JSONL output**: One record per batch with sigil counts, PSR/PUR ids, and errors

### Spec

See `docs/realtime-pattern-check.md` for the full specification including buffer behavior, batch timing, and schema details.
