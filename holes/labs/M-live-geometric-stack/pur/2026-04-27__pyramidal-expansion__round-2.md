# PUR: Pyramidal Expansion — Round 2 (8 new repos to phases 1+2+3)

pattern (re-confirmed):
- `storage/deterministic-ingest-pipeline` ✓ — same algorithm
  ran across all 8 new repos; structure unchanged.
- `futon-theory/counter-ratchet` ✓ on phase-3 (commits never decrease).

actions taken (single session, 2026-04-27):
- Pyramidal phase-1 + phase-3 ingest of:
  futon7-d, futon3a-d, futon3b-d, futon5a-d, futon0-d, futon3-d,
  futon1-d (full); futon5-d (partial; phase-1 crashed mid-vocab,
  phase-3 ran clean).

outcome: 7 of 8 fully landed; 1 partial; 2 real bugs surfaced.

### Round-2 ingest counts

| Label | Phase-1 hyperedges | Phase-3 hyperedges | Notes |
|---|---:|---:|---|
| futon7-d | ~720 | ~184 | 40/40 phase-1 invariants (after retry); cross-codebase artifact noted |
| futon3a-d | 1,860 | ~927 | clean |
| futon3b-d | 817 | ~671 | clean |
| futon5a-d | 2,320 | ~223 | 0 tests in repo; coverage% = 0 |
| futon0-d | 4,528 | ~4,194 | 111 commits |
| **futon3-d** | **15,598** | **~46,250** | **815 commits, 45,435 edits — biggest by phase-3** |
| futon5-d | partial (~7,500) | ~11,452 | phase-1 vocab-loop crashed; see bug-1 |
| futon1-d | 7,836 | ~9,711 | 158 commits |

Round-2 total: ~115,000 new hyperedges. Cumulative across all
labels in futon1a: **≈170,000 hyperedges.** 13 active labels
across 11 distinct codebases.

### Real bugs surfaced

**Bug-1 — vocab-use L2 term-set throw on futon5.** During phase-1
ingest of futon5, `ingest_v05_to_futon1a.clj:424` threw
`:error/reason :unresolved-target` because a `:vocabulary-use`
edge had a `:dst` not in the `term-set`. Root cause hypothesis:
the regex compiled by `term-pattern` collapses whitespace in
multi-word terms via `\s+`, so a source-file occurrence like
`"free  energy"` (double space) is matched but the captured
group is `"free  energy"`, which after lower-casing is NOT
literally in the term-set (which has `"free energy"`). futon5
appears to have at least one such whitespace-variant.

**Fix path:** normalise multi-whitespace in the captured group
before set membership check. ~3-line change. Non-blocking; phase-3
runs independently and was unaffected. Phase 4.5 watcher would hit
the same crash on .md saves under futon5; needs fix before live
watching futon5.

**Bug-2 — cross-codebase counter-ratchet violation on futon7.**
Phase-1 invariant test on futon7-d showed
`code/v05/calls: before=128 after=100 (Δ=-28)`. Counter-ratchet
violated by re-ingest. Root cause hypothesis: when a futon7 var
shares a qname with a var in some other ingested codebase (e.g.,
`clojure.core` symbols, or shared utility namespaces), the
substrate's prop on that var-vertex gets overwritten by the
*other* repo's ingest, and the futon7 label-filter then misses
the calls edges that touch those vertices. Cross-codebase
namespace pollution we'd seen on `:author` (handled via
`global-types`) but apparently also exists for `:var` when
namespaces collide. futon7 with only 9 .clj files seems to
have higher exposure to this than the bigger repos — possibly
because more of its calls reach into shared third-party
namespaces.

**Fix path:** for cross-codebase robustness, every var should
carry a `:repo` *list* (not single value) in props, OR the
stable-ID should include the repo as a discriminator (cf. the
directed-edge stable-ID fix). The latter is correct but
requires a schema migration. Defer to phase 5 prep.

### Substrate state at end of session

```
Active labels in futon1a: 13
  futon2-d futon4-d futon4-elisp-d futon1a-d futon3c-d
  futon7-d futon3a-d futon3b-d futon5a-d futon0-d
  futon3-d futon5-d (partial) futon1-d
Estimated total hyperedges: ~170,000
Skipped: futon3c-origin/p7 (legacy snapshots),
         futon6 (waiting for Codex's Python projector),
         futon7a (1 elisp file, trivial)
```

### Pyramid status

| Phase | Repos at this phase | Notes |
|---|---|---|
| 1 (edge taxonomy) | 13 | futon5-d partial; rest clean |
| 2 (geometric layer) | 13 (queryable) | derived; no per-repo state |
| 3 (commits-as-vertices) | 12 (all but futon5-d's possibly-incomplete state) | clean across all |
| 4 (live watcher) | 0 | scripts ready; not yet launched |
| 4.5 (per-file + multi-watcher) | 0 launched | tested on futon2+futon7 |
| 5 (futonic-zapper signatures) | not started | substrate ready |

### Suggested next moves (not yet executed)

1. **Fix bug-1** (vocab-use whitespace normalisation) so the
   watcher doesn't crash on futon5 .md saves.
2. **Re-ingest futon5-d** under a fresh label after the fix.
3. **Pause and decide on bug-2** (cross-codebase var collision).
   Phase-5 signatures may want this fixed first.
4. **Launch multi-watcher on the full 11-repo set** in
   background. Joe's "operating system territory" framing
   becomes lived experience.
5. **Or move to phase 5** as originally planned, accepting
   the two known bugs as documented.
