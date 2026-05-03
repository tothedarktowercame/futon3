# PUR: B-1 — Vocab-Use Term-Set Whitespace Mismatch

pattern (re-confirmed):
- `storage/canonical-interface` ✓ — fix is purely projector-side;
  no API or schema change.
- `storage/deterministic-ingest-pipeline` ✓ — the captured-group
  normalisation is deterministic; same source → same output.

actions taken (2026-04-27):
- Edited `futon3/scripts/ingest_v05_to_futon1a.clj:271` —
  added `normalise-term-hit` helper that lower-cases + collapses
  internal whitespace + trims; threaded through
  `scan-file-for-terms`.
- Edited `futon3/scripts/v0_codebase_hypergraph.clj:124` — same
  fix in the v0 projection (it has its own copy of the function).
- Re-ingested futon5 under fresh label `futon5-d2` to verify.

outcome: success.

| Metric | Before fix (futon5-d, partial) | After fix (futon5-d2, clean) |
|---|---|---|
| Phase-1 ingest | crashed at L2 throw mid-vocab-loop | **completed: 81,817 hyperedges in 2m22s, 0 failures** |
| Phase-3 ingest | 11,176 edits (ran independently; was unaffected) | 11,176 edits (consistent) |
| Phase-1 invariant on the failing assertion | `:error/reason :unresolved-target` | (no longer thrown) |

### Root cause

The regex compiled by `term-pattern` allows internal `\s+` between
tokens of multi-word terms (e.g. for `"free energy"` the regex
matches `free\s+energy`). When the captured group hits a source
text occurrence with multiple whitespace characters between tokens
(e.g. `"free  energy"` — double space, or `"free\nenergy"` — line
break), the captured group preserves that whitespace verbatim.

The previous `scan-file-for-terms` did:

```clojure
(comp str/lower-case #(if (vector? %) (second %) %))
```

…which lower-cased but did NOT collapse whitespace. So the result
included strings like `"free  energy"` that the canonical term-set
(populated by `extract-vocab-terms`, which collapses whitespace
during cleaning) never contains.

The downstream L2 invariant in
`ingest_v05_to_futon1a.clj:418-424` then threw
`:error/reason :unresolved-target` because the `:vocabulary-use`
edge's `:dst` (the messy term string) wasn't in `term-set`.

### Fix

```clojure
(defn- normalise-term-hit [s]
  (-> s str/lower-case (str/replace #"\s+" " ") str/trim))
```

…applied to the captured group before the set-membership check.
3 lines total per file (× 2 files for the parallel script copy).

### What's now unblocked

- futon5 ingest is clean end-to-end.
- futon5-d2 (post-fix) is the canonical futon5 substrate from now
  on; futon5-d (partial pre-fix) is historical.
- The multi-watcher (`multi_watcher.clj`) can now safely watch
  futon5 .md saves without crashing — was the original blocking
  concern from the cleanup checkpoint.

### Substrate state at end of fix

futon5-d2: 81,817 phase-1 + ~12k phase-3 = **~94,000 hyperedges**
under one label. **futon5 is now the largest single-label
substrate** in the system (futon3-d was previously the leader at
~62k).

Total substrate-2 in futon1a: ≈190,000 + ~94,000 = **≈285,000
hyperedges** across 15 active labels.

### Next from the cleanup checkpoint

- B-2 (cross-codebase var-vertex prop pollution) — schema-level
  decision; defer to phase-5 prep.
- B-3 (watcher move/delete handling) — design done; implementation
  next time we touch the watcher.
- A-1 (futon2 War Machine wraps futon0.report.war-machine) —
  unblocked but no urgency.
- P-1 (phase 5 futonic-zapper) — substrate fully ready.
- P-2 (multi-watcher launch) — now ALSO unblocked since B-1 was
  the only known crash trigger.

The cleanest next move is either P-2 (launch the watcher and
*live* the operating-system framing) or P-1 (phase 5 signature
emission). Both are operator-visible payoffs.
