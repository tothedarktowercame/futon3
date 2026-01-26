# M1: Smoke Test + H1-Lite

**Week**: 1-2
**Type**: Stabilization (S1) + Hub-lite (H1)
**Depends**: P0 (Portal), P1 (Sidecar)
**Gate**: futon3a/P11

---

## Objective

Verify that portal and sidecar work in isolation:
1. Can create a session via CLI
2. Can query patterns from futon3 library
3. Session logs are valid EDN with complete PSR/PUR chains

---

## Prerequisites

Before running this mission, ensure the following are in place:

### 1. ADMIN_TOKEN (required for code eval)

```bash
# Generate a token and save it
cd /home/joe/code/futon3a
echo "$(uuidgen)" > .admintoken
chmod 600 .admintoken
```

### 2. Start Drawbridge Server

```bash
# In a separate terminal (keep running)
cd /home/joe/code/futon3a
./scripts/musn-repl
# Expected: "Drawbridge ready on http://127.0.0.1:6767/repl"
```

### 3. Futon1 API (optional for pattern queries)

Pattern queries via `portal patterns` subcommands need Futon1 running:
```bash
# Default: http://localhost:8080/api/alpha
# Override: export PORTAL_FUTON1_URL=...
```

---

## Steps

### S1: Portal Smoke Test (requires Drawbridge running)

```bash
cd /home/joe/code/futon3a

# 1. Ping via code eval
./scripts/portal "(+ 1 2)"
# Expected: 3

# 2. Check MUSN core loaded
./scripts/portal "(require 'musn.core) :ok"
# Expected: :ok
```

### S1: Pattern Query Test (requires Futon1 API)

```bash
cd /home/joe/code/futon3a

# 1. List patterns (queries Futon1 registry)
./scripts/portal patterns list --limit 5
# Expected: vector of pattern maps with :id and :name

# 2. Search patterns
./scripts/portal patterns search "coherence" --limit 5
# Expected: ranked results with :score

# 3. Get specific pattern
./scripts/portal patterns get "stack-coherence/evidence-ledger"
# Expected: full pattern entity or nil
```

### S1: Sidecar Logging Test (requires Drawbridge running)

```bash
cd /home/joe/code/futon3a

# 1. Create session and start turn
./scripts/musn-session "M1 smoke test"
# Expected: Session: musn-XXXXXXXX

# Save the session ID
sid="musn-XXXXXXXX"  # replace with actual

# 2. Select a pattern
./scripts/musn-select "$sid" "stack-coherence/evidence-ledger" "testing selection"
# Expected: selection entry printed

# 3. Log action
./scripts/musn-action "$sid" "stack-coherence/evidence-ledger" "implement" "Verified pattern exists"
# Expected: action entry printed

# 4. Add evidence
./scripts/musn-evidence "$sid" "stack-coherence/evidence-ledger" "/tmp/test.txt" "Test evidence note"
# Expected: evidence entry printed

# 5. End turn
./scripts/musn-end "$sid" "Completed smoke test"
# Expected: turn end entry printed
```

### S1: Log Validation

```bash
cd /home/joe/code/futon3a

# Find the log file (default: log/<session>.edn)
ls log/musn-*.edn

# Verify EDN is parseable
clojure -M -e "(require '[clojure.edn :as edn]) (doseq [line (line-seq (clojure.java.io/reader \"log/$sid.edn\"))] (edn/read-string line))"
# Expected: No parse errors

# Check event sequence
cat log/$sid.edn
# Expected: :session/create → :turn/start → :turn/select → :turn/action → :evidence/add → :turn/end
```

### H1-Lite: Pattern Query from futon3 Library

```bash
cd /home/joe/code/futon3a

# Query patterns by namespace (if Futon1 has library indexed)
./scripts/portal patterns list --namespace "library-coherence" --limit 10
./scripts/portal patterns list --namespace "code-coherence" --limit 10

# Search by terms
./scripts/portal patterns search "pattern maturity" --limit 8
./scripts/portal suggest "devmap coherence" --limit 8
```

---

## Success Criteria

- [ ] Portal responds to ping
- [ ] Portal returns patterns for namespace queries
- [ ] Portal returns patterns for hotword queries
- [ ] Session creates successfully
- [ ] All turn operations log without error
- [ ] Log file is valid EDN (every line parses)
- [ ] Log contains :turn/select and corresponding :turn/action entries
- [ ] Query latency <500ms for each call

---

## Failure Modes to Watch

| Failure | Indicates |
|---------|-----------|
| Portal doesn't start | deps.edn / classpath issue |
| Empty pattern results | Library not synced / path wrong |
| Log parse error | EDN serialization bug |
| Missing log entries | persist! not called |
| Slow queries (>1s) | Index missing or cold start |

---

## Artifacts

- `/tmp/m1-portal-smoke.log` - captured portal output
- `~/.futon3a/lab/sessions/<sid>/log.edn` - session log
- `/tmp/m1-summary.edn` - mission summary (pass/fail per criterion)

---

## Next

If M1 passes → M2 (verify logs, iterate on issues)
If M1 fails → Fix issues, re-run M1

---

## Notes

This is a manual mission for Week 1. Once stable, automate via mission queue (P4).
