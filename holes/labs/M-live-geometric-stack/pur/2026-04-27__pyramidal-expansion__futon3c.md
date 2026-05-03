# PUR: Pyramidal Expansion — futon3c (largest substrate yet)

pattern (re-confirmed):
- All four substrate-2 patterns held without revision: deterministic
  ingest, canonical interface, counter-ratchet, durability-first.
  No new patterns needed; no script changes needed.

actions taken (single session, 2026-04-27):
- Ran phase-1 ingest on futon3c (label `futon3c-d`).
- Ran phase-1 invariant tests.
- Ran phase-2 geometric layer.
- Ran phase-2 regression tests.
- Ran phase-3 commit ingest.
- Ran phase-3 invariant tests.

outcome: success. **65/65 invariants pass**.

### futon3c is by far the largest active substrate

| Quantity | futon3c-d | futon4-elisp-d | futon1a-d | futon2-d |
|---|---:|---:|---:|---:|
| .clj files | 249 | 0 | 85 | 46 |
| .el files | 7 | 147 | 0 | 0 |
| Commits | 598 | 280 | 77 | 37 |
| Vars (XTDB distinct) | **3,378** | 2,140 | 365 | 562 |
| Tests | **1,333** | 311 | 107 | 46 |
| Coverage edges | **2,212** | 531 | 147 | 50 |
| Coverage % | **15.7** | 11.0 | 14.0 | 4.4 |
| Call edges | **5,610** | 2,837 | 508 | 766 |
| Vocab uses | 458 | 174 | 87 | 63 |
| Components | **401** | 420 | 42 | 34 |
| Phase-1 hyperedges | 16,931 | 8,579 | 1,954 | 2,403 |
| Phase-3 :edits | 14,136 | 17,104 | 2,241 | 1,398 |
| Total hyperedges | **~32,200** | ~26,300 | ~4,350 | ~3,900 |

futon3c is also the **highest coverage discipline** of any
codebase ingested so far — 15.7% paired vars (vs 14% for
futon1a, the next highest). Joe's framing of futon3c as a
"buzzing hive of activity" is reflected in the substrate:
not just biggest, but most disciplined.

### Top -ΔT (load-bearing utilities)

The top-K maps directly onto futon3c's documented architecture:

| Var | ΔT | Role |
|---|---:|---|
| `futon3c.evidence.store/append*` | -33 | evidence-store write primitive |
| `futon3c.dev/make-codex-invoke-fn` | -28 | Codex agent invocation |
| `futon3c.dev.config/env` | -24 | config loader |
| `futon3c.dev.apm-conductor-v2/handle-solve-return!` | -21 | APM solve handler |
| `futon3c.agency.registry/registry-status` | -20 | agent registry status |
| `futon3c.agency.registry/invoke-agent!` | -19 | agent invocation |
| `futon3c.evidence.store/query*` | -18 | evidence-store read primitive |
| `futon3c.peripheral.runner/runner-error` | -17 | peripheral error path |

`evidence.store/append*` and `query*` as the two heaviest
load-bearing primitives: futon3c's central data layer.
`agency.registry` and `apm-conductor-v2` as the agent
orchestration spine. `peripheral.runner` for the gauntlet.
The substrate sees the architecture without being told —
same pattern across all 5 codebases now.

### Top +ΔT (orchestrators)

| Var | ΔT | Role |
|---|---:|---|
| `futon3c.transport.ws/make-ws-callbacks` | +17 | websocket setup |
| `futon3c.dev.bootstrap/run-main!` | +16 | system bootstrap |
| `futon3c.dev/start-tickle!` | +12 | periodic-task starter |
| `futon3c.social.shapes/Timestamp` | +12 | shared shape ctor |
| `futon3c.dev.fm/start-fm-conductor!` | +10 | Fast-Mid conductor starter |

### Findings

1. **No script changes were needed.** Phase 1+2+3 all
   ran clean against futon3c with the existing toolchain.
   This is the first codebase where the substrate-2 ingest
   ran without surfacing a new bug — a confidence signal
   that the pyramid has stabilised. Schema and discipline
   generalise.

2. **futon3c.dev/make-codex-invoke-fn at -28** — the
   second-most load-bearing utility. Codex (the agent
   collaborating with us this session) shows up as a
   first-class architectural element of futon3c, recursively.
   The substrate is observing the system observing itself.

3. **9 duplicate-defn smells caught** (bb 3,387; XTDB
   distinct 3,378). futon3c has more of these than any
   other codebase per-var-count (proportionally same as
   futon4 and futon2). Worth surfacing as candidate SORRYs
   when phase 5 (futonic-zapper) emits signatures.

4. **5 unique authors** (substrate's first multi-author
   reading): Joe + Codex + 3 others I haven't catalogued.

### Substrate state at end of session

| Label | Phase-1 | Phase-3 | Total |
|---|---:|---:|---:|
| futon2-d | 2,403 | 1,508 | 3,911 |
| futon4-d (deprecated) | 586 | 1,478 | 2,064 |
| futon4-elisp-d | 8,579 | 17,683 | 26,262 |
| futon1a-d | 1,954 | 2,395 | 4,349 |
| **futon3c-d** | **16,931** | **15,330** | **32,261** |
| (historical pre-fix) | ~5,000 | – | ~5,000 |
| **GRAND TOTAL** | **~35,500** | **~38,400** | **~73,800** |

next: phase 4 (live watcher) is now well-justified — the
substrate is meaningfully large enough that per-event
incremental update is the right architecture rather than
"re-run the script". Or phase 5 (futonic-zapper signatures)
to convert this substrate into operator-visible discipline
without yet committing to the watcher architecture. Joe's
call.
