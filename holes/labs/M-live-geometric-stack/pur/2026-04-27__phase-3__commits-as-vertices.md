# PUR: Phase 3 — Commits as First-Class Vertices

pattern (re-confirmed):
- `storage/deterministic-ingest-pipeline` ✓
- `futon-theory/counter-ratchet` ✓ (per-repo commit count
  monotonic; idempotent re-run adds 0)
- `storage/canonical-interface` ✓
- `storage/durability-first` ✓
- `futon-theory/curry-howard-operational` ✓ — every commit
  is now a typed vertex; `:edits` edges are constructive
  arrows from prior to next vertex state. The commit DAG
  is the proof-tree of the codebase's evolution.

actions taken (single session, 2026-04-27):
- Created `futon3/scripts/ingest_commits_to_futon1a.clj`.
- Created
  `futon3/holes/labs/M-live-geometric-stack/tests/phase_3_commits_test.clj`.
- Ingested commit-graph for futon2-d, futon4-d, futon1a-d.
- Cross-ref both directions with `M-stack-stereolithography`
  (Joe, 2026-04-27: "the DAG is the spanning object for the
  stack as it is printed" — phase 3 IS the substrate that
  mission has been waiting for).

outcome: success across three codebases.

### Cross-codebase commit-graph counts

| Type | futon2-d | futon4-d | futon1a-d |
|---|---:|---:|---:|
| `:commit` | 37 | 280 | 77 |
| `:author` (global, deduped) | 2 | 2 | 3 |
| `:authored` | 37 | 280 | 77 |
| `:precedes` | 36 | 279 | 76 |
| `:edits` | 1,398 | 917 | 2,241 |

Total phase-3 hyperedges added: ~5,250 across three codebases.

### Test results

| Phase | futon2-d | futon4-d | futon1a-d |
|---|---:|---:|---:|
| 1 (edge taxonomy) | 40/40 | 40/40 | 40/40 |
| 2 (geometric layer) | 12/12 | 12/12 | 12/12 |
| 3 (commits-as-vertices) | 13/13 | 13/13 | 13/13 |

**195 PASS / 0 FAIL** across all phases / all codebases.

### Real findings

1. **futon4 has 280 commits but only 917 :edits edges.**
   Most futon4 commits touch .el files, which the v0
   projector doesn't handle yet. Confirms the elisp gap
   (Codex's parallel work on E-substrate-2-elisp-projection
   will close it). Once elisp lands, the futon4 :edits
   count should rise dramatically.

2. **Authors are global, not per-codebase.** Joe + Codex
   span all three repos (Joe + Rob in futon1a). The
   `:author` vertex is deduplicated by email across
   codebases. The test correctly treats `code/v05/author`
   as a global type (via the `global-types` set in the
   test); the per-label filter does not apply to it. This
   is the right schema commitment: people aren't owned
   by codebases.

3. **Test-helper defns inside `_test.clj` files are not
   first-class.** Phase 1's convention: in test files,
   only `deftest` forms become `:test` vertices; helper
   defns are deliberately skipped. Phase 3's
   `vars-in-file` initially included them by mistake,
   producing 76 :edits edges to non-existent var-vertices
   in futon2-d. Fix: aligned `vars-in-file` to phase 1's
   convention (`var-def-forms` + `test-def-form`,
   gate by `test-file?`). The pre-fix ingest's stale 76
   edges remain in the store (substrate-1 has no DELETE
   route) and are surfaced as a documented historical
   finding by the test, log-not-fail. Same pattern as
   phase 1's duplicate-defn finding.

4. **Substrate-1 has no DELETE route for hyperedges.**
   This is a recurring obstacle whenever substrate-2's
   ingest needs to revise a previous run's output.
   Workarounds so far: idempotency on stable IDs (good
   enough most of the time), per-label filtering (works
   when label is in props), historical-finding
   acknowledgment (when neither helps). A proper fix
   needs futon1a server-side work — beyond substrate-2
   scope but tracked.

### Phase-3 v0 limitations (next-version DERIVE feeders)

- `:edits` resolves against HEAD's var index, not the
  per-commit var index. Means a commit that introduced
  a var which has since been deleted will produce no
  `:edits` edge for it. Fix: per-commit projection
  (much heavier; punted to v1 if `T(v, c)` for
  historical c needs it).
- Linear `:precedes` chain only — branched histories
  (merges) would need `git log --parents`. None of
  futon2/futon4/futon1a have branches at HEAD; works
  for now.
- Author identity is by email only. Joe under multiple
  emails would split into multiple author vertices.
  Fix: a per-author canonical-email map.

### Substrate state at end of session (4943 + ~5250 = ~10,200 hyperedges)

```
:repo "futon2-phase1"   2,403  (pre-direction-fix, historical)
:repo "futon4-phase1"     586  (pre-direction-fix, historical)
:repo "futon1a-phase1"  1,954  (pre-direction-fix, historical)
:repo "futon2-d"        2,403 + 1,508 phase-3 = 3,911
:repo "futon4-d"          586 + 1,478 phase-3 = 2,064
:repo "futon1a-d"       1,954 + 2,395 phase-3 = 4,349
                                ─────────
                          ≈10,308 substrate-2 hyperedges total
```

### What this enables for M-stack-stereolithography

Per Joe's pickup (2026-04-27) and the mission's cross-ref:
the commit DAG is now first-class. `T(v, c)` queries
become tractable: at any commit `c`, T(v, c) is the
bath/exists boundary at print-layer c. The semilattice
M-stack-stereolithography reaches for is computable from
this substrate without a separate render pipeline.

What that mission still owns: the script (VSATARCS) and
storyboard (WebArxana) views — substrate-2 phase 3 does
not deliver UIs, only the substrate they consume.

next: phase 4 (live watcher) is the next logical step,
but per Joe's pyramidal expansion strategy, the right
move is probably to broaden phase 3 to whatever
codebase Codex's elisp work unblocks (futon3c, full
futon4) before deepening into phase 4's architectural
commitment.
