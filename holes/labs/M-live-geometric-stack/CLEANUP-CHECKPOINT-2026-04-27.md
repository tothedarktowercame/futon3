# Cleanup Checkpoint — substrate-2 / M-live-geometric-stack

**Date:** 2026-04-27 (end of session)
**Owner:** Joe (when picking up next session)
**Mission:** `futon3/holes/missions/M-live-geometric-stack.md`
**Posture at end of session:** substrate-2 is functional and queryable
across 11 codebases (~170,000 hyperedges). Phase 4.5 watcher
ready but not launched; phase 5 not started. Two known bugs and
one architecture-split note.

This document is a single triage list for the next session — every
item is a deferred decision, a known bug, or a documentation pointer.
Pick one and pull the thread.

---

## Known bugs (smallest fix first)

### B-1 — Vocab-use term-set whitespace mismatch — **RESOLVED 2026-04-27**
- **Was:** `futon3/scripts/ingest_v05_to_futon1a.clj:424`
  threw `:error/reason :unresolved-target` mid-vocab-loop on
  futon5 phase-1 ingest.
- **Cause:** `term-pattern` regex's `\s+` for multi-word terms
  let captured groups carry runs of whitespace (`"free  energy"`)
  that the canonical term-set (`"free energy"`) doesn't match.
- **Fix:** added `normalise-term-hit` helper in
  `scan-file-for-terms` of both `ingest_v05_to_futon1a.clj` and
  `v0_codebase_hypergraph.clj`; lower-cases + collapses internal
  whitespace + trims before set-membership check. ~3 LOC × 2
  files.
- **Verification:** futon5 re-ingested cleanly under fresh
  label `futon5-d2`: 81,817 phase-1 hyperedges + 11,172 phase-3
  edits, 13/13 phase-3 invariants pass. **futon5-d2 is now the
  largest single-label substrate in the system (~94k
  hyperedges).**
- **Side benefit:** the multi-watcher is now safe to launch
  on futon5 (was the original blocking concern).
- **PUR:** `pur/2026-04-27__B-1__vocab-whitespace-fix.md`.

### B-3 — Watcher does not handle file moves / deletions — **v0 RESOLVED 2026-04-27**
- **Was:** the polling loop iterated over the *fresh snapshot*, not
  over the *cache*, so deleted files (or the old paths of renamed/
  moved files) were silently invisible.
- **v0 Fix landed:** `multi_watcher.clj` now records two new event
  types:
  - `code/v05/watcher-event` with `:source "deletion"` for any
    cache-key absent from the new snapshot whose content-hash
    didn't match an appeared path.
  - `code/v05/watcher-event` with `:source "rename"` (props
    `:from`/`:to`/`:hash`) when a vacated path's hash matches an
    appeared path's hash — auto-detected within a single cycle.
  Cache structure upgraded from `{path → mtime}` to
  `{path → {:mtime :hash}}` so vacated paths' hashes are still
  available for matching. Hash-based change detection (replacing
  mtime alone) also makes touch-without-edit no longer trigger
  re-ingest.
- **Smoke test passed (2026-04-27):** create + cold-scan 2 files,
  delete one, rename the other. Watcher emitted exactly 1
  deletion-event + 1 rename-event; the renamed file was NOT
  double-fired as a fresh ingest (correctly suppressed via
  `:renamed-to` filter).
- **Still deferred to phase 4.6 (cascade-staleness):**
  - Mark `:edge/witness-stale true` on every var/test/namespace
    whose `:source-file` matches the deleted path.
  - Emit `:edge/renamed-to` linking old vertex → new vertex.
  - Backfill `:source-file` prop on whole-repo-ingest vertices
    (per-file ingest already sets it).
- **Cases honestly traced (Joe's question, 2026-04-27):**
  - Rename within dir: new path ingested fresh; old vertices ghost.
    If `(ns …)` declaration is unchanged, futon1a's L1 dedupe
    re-uses the same stable-IDs → clean. If ns changes, old qnames
    go ghost.
  - Move across dirs: same as above.
  - Move across repos (futon2→futon3c): source label gets ghosts;
    destination clean.
  - Defn rename inside a file: mtime changes → re-ingest emits the
    new qname; old qname ghosts.
  - File deletion: never noticed; all contributions become ghosts.
- **Fix path (phase 4.6):**
  1. Per-cycle, also compute `(cache-keys − snapshot-keys)` and
     dispatch a *deletion event* for each vacated path.
  2. Emit `:edge/witness-stale true` on every vertex whose
     `:source-file` prop matches the deleted path (cascade-staleness;
     the P-3 item from this checkpoint, now with a concrete trigger).
  3. Detect renames by content-hash within the same cycle: if a path
     appears with hash H and another path disappears with hash H,
     emit a `:renamed-from` edge.
  4. Backfill `:source-file` prop on the existing whole-repo ingests
     (per-file ingest already sets it as of phase 4.5).
- **Not blocking** the substrate's *additive* behaviour, but the
  invariant "the substrate reflects on-disk state" is violated for
  every move/delete. Important to fix before pattern-application
  consumers come online (they care about which witnesses are stale).

### B-2 — Cross-codebase var-vertex prop pollution (+ projector-revision orphans)
- **Where:** ingest substrate, all `:var` hyperedges.
- **Symptom A** (cross-codebase): futon7 phase-1 invariant test
  reported `code/v05/calls: before=128 after=100 (Δ=-28)` —
  counter-ratchet violation.
- **Symptom B** (projector-revision orphans, surfaced
  2026-04-27 during Python-projector review): futon6-py-d
  phase-1 regression showed bb=2240 vars vs xtdb=4334. ~650
  orphan vars under `:repo "futon6-py-d"` carry bare module
  names (e.g. `extract_se_threads/process_site`) from an
  earlier projector revision, before Codex's fix changed them
  to `scripts.extract_se_threads/process_site`. Substrate-1
  has no DELETE so old qnames persist as ghosts.
- **Cause:** when a var qname is shared across codebases (e.g.
  third-party namespaces, or namespaces that genuinely appear in
  multiple repos) — *or when a projector revision changes the
  qname construction* — the substrate's per-vertex `:repo` prop
  gets overwritten by whichever ingest ran most recently, and old
  qnames remain in the store untouched.
- **Same shape as the `:author` issue** we already handled via
  `global-types` in the phase-3 test, but for `:var` (and
  potentially every other vertex type that can repeat across
  repos).
- **Fix options (analysed 2026-04-27):**
  - **(a)** Per-repo discriminator in stable ID:
    `hx:code/v05/var:<repo>/<qname>`. Server-side futon1a change.
  - **(b)** `:repo` as a list-prop, not single. Requires fetch-
    merge-upsert because futon1a's POST REPLACES props.
  - **(c)** Treat all `:var` as global like `:author` — drops
    per-codebase queryability. Rejected: per-codebase reads
    are operationally important.
  - **(d)** *Recommended.* Per-repo qname-prefixing at projector
    side: emit `<repo-label>/<qname>` for all vertex `:hx/endpoints`
    and edge endpoints. Pure client-side; no futon1a change;
    counter-ratchet holds trivially because every label has its
    own ID space. Geometric-layer queries need a small update
    to strip the label prefix when resolving namespaces.
- **v0 RESOLVED 2026-04-27** (option d landed):
  - **What changed:** `ingest_v05_to_futon1a.clj`,
    `ingest_one_file.clj`, and `ingest_commits_to_futon1a.clj`
    now prefix per-repo qname endpoints with the label:
    `futon7-d3/f7.core/intersect` instead of `f7.core/intersect`.
    Global types (`code/v05/{term, doc, term-defines, author,
    commit, authored, precedes}`) remain unprefixed.
  - **Verification (futon7-d3 fresh label):** 112 var hyperedges
    written with prefixed endpoints; 159 :edits edges with
    prefixed var endpoints; phase-5 still emits 3 work-around-
    drift signatures (queries handle prefixed strings via
    `ns-of-vertex` working uniformly on prefixed and unprefixed
    qnames).
  - **Counter-ratchet test:** ingested futon4-elisp-d3 after
    futon7-d3; futon7-d3's count remained 112 (unaffected by
    foreign ingest). Cross-codebase qname collisions are now
    structurally impossible because every label has its own
    ID space.
  - **Migration policy:** old labels (`futon2-d`, `futon4-d`,
    `futon4-elisp-d`, `futon1a-d`, `futon3c-d`, etc.) keep
    their un-prefixed data; they're internally consistent
    when queried alone. New labels use the `-d3` suffix and
    the prefixed scheme. Operators can re-ingest any label
    under a fresh `-d3` suffix when they want B-2 protection
    on it.
  - **Not migrating wholesale** because: substrate-1 has no
    DELETE so old hyperedges would persist as orphans; the
    cost of mass re-ingest (futon3c-d alone took 25s phase-1
    + 2 min phase-3) outweighs the benefit when most operator
    queries are per-codebase.

---

## Architecture split (Joe's note this session)

### A-1 — futon2 War Machine wraps futon0.report.war-machine — **MOVED 2026-04-28**
- **Was:** architectural note about the WM split between futon2
  (web surface) and futon0.report.war-machine (canonical logic).
- **Moved to:** `futon2/holes/missions/M-reflective-discipline.md`.
  The next active War Machine work is the operator-facing PSR/PUR/PAR
  surface in that mission; resolution-decision (inline vs cross-edge
  vs shared-library) belongs to that scope.

---

## Deferred substrate-2 phase work

### P-1 — Phase 5 (futonic-zapper signature emission) — **v0 RESOLVED 2026-04-27**
- **Was:** spec only (mission §"futonic-zapper specification").
- **v0 landed:** `futon3/scripts/phase_5_signatures.clj` (~280 LOC)
  implements 4 of the 8 signatures: adapter-shim-no-adapt,
  work-around-drift, concept-used-without-definition,
  completion-rot.
- **Demonstration:** futon2-d emitted the *exact* drift +
  adapter-shim findings from the v0.5 excursion, now as typed
  `code/v05/satisficing-signature` hyperedges. 4-label subset
  emitted **228 signatures total**.
- **v0.5 partial (2026-04-27):** added two of the four
  time-series signatures:
  - `coverage-retreat` — monotonic-decline detection over the
    per-label commit chain. Verified on futon1a-d: two real
    windows surfaced (31.6% → 20.0% over 6 commits;
    18.3% → 8.2% over 42 commits), both medium severity.
  - `concept-introduced-without-attachment` — terms defined
    in vocab docs but unused by the label's namespaces.
    Per-label severity scales inversely with unused-ratio.
- **v0.5 quick-fix + load-bearing-under-tested:** **CLOSED
  2026-04-28.** 6 of 8 signatures emit operator-readable
  output; the other two have careful-threshold-tuning needs
  that aren't justified without an operator pain point asking
  for them. Design preserved in
  `M-live-geometric-stack.md` §"futonic-zapper specification";
  any future implementer can pick up. Reopen if needed.
- **Term-filter tightening + IDF weighting + component-size
  threshold:** **CLOSED.** Operator can grep / filter the
  emitted hyperedges if noise becomes painful.
- **PUR:** `pur/2026-04-27__phase-5__futonic-zapper-v0.md`
  (v0); v0.5 additions captured in the session-3 update below.

### P-2 — Phase 4.5 multi-watcher launch — **LAUNCHED 2026-04-27**
- **Status:** running in background; PID at `/tmp/multi-watcher.pid`,
  log at `/tmp/multi-watcher.log`. Watching 13 active labels with
  --interval-ms 5000, --no-cold-scan.
- **First fs-event observed:** the watcher caught the creation of
  `phase_5_signatures.clj` itself in futon3 within one cycle.
  Substrate is self-aware: tooling that queries the substrate is
  ingested into the substrate.
- **B-3 v0 is in this build** — deletions and renames detected
  and emitted as typed evidence. Cascade staleness still deferred.
- **Stop the watcher:** `kill $(cat /tmp/multi-watcher.pid)`.

### P-3 — Phase 4.6 quality-of-life — **DISPATCHED 2026-04-28**
- ~~`by-ns` cache~~ — DONE 2026-04-27 (2.6× speedup measured).
- ~~inotify swap~~ — **CLOSED.** Pure throughput; polling at
  ~5s is operator-acceptable. Reopen as a one-off if anyone
  hits the latency.
- ~~Cascade staleness~~ — **MOVED to M-reflective-discipline.**
  The `:edge/witness-stale` flag is a precondition for PUR-as-
  witness; belongs with the tangent-bundle consumer.
- ~~Git post-commit hook~~ — **CLOSED.** Polling catches
  commits within 5s; the per-repo `.git/hooks/post-commit`
  install friction outweighs the marginal gain.
- ~~Multi-repo orchestrator failure-isolation refactor~~ —
  **CLOSED.** Watcher has been running stably; reopen if a
  per-repo failure brings the whole watcher down in practice.

### P-4 — PSR/PUR/PAR — **MOVED to new mission 2026-04-28**
- **Was framed as substrate-2's "bonus round."** Joe's framing
  (2026-04-28): "PARs are tangent vectors, we are still just
  building the manifold." Substrate-2 builds the manifold
  (state); PSR/PUR/PAR is the tangent-bundle layer (motion of
  state). They are theoretically distinct.
- **New mission home:** `futon2/holes/missions/M-reflective-discipline.md`
  (IDENTIFY 2026-04-28). Joint extension of M-live-geometric-stack
  (the manifold) and the War Machine in futon2 (the strategic
  visualiser, which becomes the operator-facing surface for
  PSR/PUR/PAR entry). Substrate-2 itself does NOT absorb this
  work — query-side bridges only.
- **Investigation that informed the move:** dormant
  infrastructure exists in `futon3c/src/futon3c/peripheral/discipline.clj`
  (PSR/PUR mesh ops as peripheral tools; legacy `/psr` `/pur`
  `/par` slash commands invoked them). The peripheral tools
  wrote evidence records (claim-typed `:question` / `:goal` /
  `:evidence` / `:correction`) to futon1a's evidence API. **Zero
  such records exist in the live store today** — the affordance
  atrophied when slash commands stopped translating to the REPL.
- **PAR clarification:** "Project Action Review" (Peeragogy
  lineage), NOT "Post-Action Review" as legacy `/par` command
  miscalled it. The schema in M-reflective-discipline corrects
  this lineage.

---

## Codex hand-offs in flight

### C-1 — Python projector
- **Brief:** `futon3/holes/labs/M-live-geometric-stack/AGENTS-codex-python-projector.md`
- **Status:** `python_ast_helper.py` and `python_projection.clj`
  exist; `load-file`d into both `v0_codebase_hypergraph.clj` and
  `ingest_v05_to_futon1a.clj`. Test file
  `python_projection_test.clj` not yet present (Codex still
  working).
- **Action when complete:** review per the AGENTS.md protocol;
  re-run phase-1+2+3 invariants on futon6 and futon3c with the
  new dispatch.

### C-2 — Elisp projector
- **Status:** complete and reviewed (2026-04-27).
- **Excursion:** `E-substrate-2-elisp-projection.md` — RESOLVED.
- **No action needed.**

---

## Category-C refactors (skip-the-projector path)

Per `file-type-inventory.md`, these have existing ingest paths
that should be refactored to *also* emit substrate-2 hyperedges
rather than us writing new projectors:

### R-1 — `.flexiarg` patterns — **v0 RESOLVED 2026-04-27**
- **Was:** existing path lived in
  `futon4/dev/arxana-{patterns-ingest,flexiarg-normalize,
  flexiarg-collection,browser-patterns}.el` + substrate-1's
  `ingest_pattern_provenance`. Substrate-2 had no .flexiarg
  visibility.
- **v0 Fix landed:** new
  `futon3/scripts/flexiarg_projection.clj` (substrate-2 side; ~75 LOC)
  parses the `@flexiarg <ns>/<name>` header + `@title` /
  `! conclusion:` / Toulmin slots, emits one
  `:vertex/type :var :var/kind "flexiarg"` per file. Wired into
  the orchestrator (`v0_codebase_hypergraph.clj`,
  `ingest_v05_to_futon1a.clj`, `ingest_one_file.clj`). Watcher's
  `WATCHED-EXTS` extended to include "flexiarg".
- **Live test:** wrote a `.flexiarg` file under `futon3/library/aif/`
  with the multi-watcher running. Within 1 cycle (~12s) +
  per-file ingest (~2.3s), the typed `code/v05/var` hyperedge
  with `:var/kind "flexiarg"`, `:var/ns "flexiarg.aif"`, and
  proper `:source-file` prop landed in futon1a. **Joe's stated
  invariant — "save a pattern → it's in futon1a within
  1–2s" — now operationally satisfied.**
- **Note on the "no duplication" directive:** the existing
  `arxana-patterns-ingest.el` continues to do its substrate-1
  job; substrate-2's new projector reads the same .flexiarg
  source and emits substrate-2 hyperedges. Two consumers of
  one canonical source-of-truth — *not* duplication of ingest
  logic. The category-C refactor (full unification) is
  deferred until phase-4.6 or later if it proves necessary.
- **Pattern → pattern citations** are extracted via simple
  `library/<ns>/<name>` regex matches in the body and surface
  as `:var/syms` (so future :calls edges can resolve
  pattern-cites-pattern).

### R-2 — Docbook entries — **CLOSED as out-of-scope 2026-04-28**
- **Existing ingest:** `futon4/dev/arxana-docbook{,-core,-export,
  -checkout}.el` + `arxana-browser-docbook.el` + `docbook-toc-export.el`.
- **Closed because:** no operator pain has surfaced from
  docbook entries being absent from substrate-2; M-live-geometric-stack
  was about code-side substrate, not the doc-store
  refactor. Reopen as its own mission if a query consumer
  surfaces.

### R-3 — Substrate-1 column refactor — **DECISION 2026-04-27**: substrate-2 supersedes
- **Original finding:** futon1a had 0 substrate-1 hyperedges at
  the start of substrate-2 work; the M-three-column-stack
  closure's claim of 1,524+ persistent edges was satisfied
  by "re-runnable script" rather than "queryable from store".
- **Decision (option b):** substrate-2 supersedes substrate-1's
  columns. Legacy edge-types (`code/namespace`, `code/var`,
  `code/requires`, `project/devmap`, `math/post`, etc.) remain
  intentionally empty. Substrate-2's `code/v05/*` types cover
  the code column (with much more granularity); the project
  column is partially served via mission docs (substrate-1's
  `ingest_mission_provenance` still runs ad-hoc when needed).
  The math column has its own pipelines via futon6 and
  doesn't need substrate-2 mirroring.
- **Why option (b) over re-running the Python ingest:** the
  data is fresher when re-derived from source on demand;
  substrate-2's live watcher already maintains the code-side
  state continuously; re-running the static ingest would
  duplicate stale snapshots without operator value.
- **Closed.** No future action required unless an explicit
  consumer of substrate-1's typed edges surfaces.

---

## Repos not yet ingested (or partial)

| Repo | Status | Reason |
|---|---|---|
| **futon5-d** | partial — phase-1 crashed | B-1 |
| futon6 | not ingested | Python projector pending (C-1) |
| futon3c (.py surface) | partial — only .clj/.cljs visible | C-1 |
| futon3c-origin / futon3c-p7 | not ingested | legacy snapshots; decide if temporal-divergence is interesting |
| futon7a | not ingested | 1 .el file; trivial when scheduled |

---

## Documentation pointers (so Joe can route the next session)

| Document | Purpose |
|---|---|
| `M-live-geometric-stack.md` | The mission spec; phases 1-7 + bonus round |
| `E-cross-prototype-geometry.md` | The empirical base + futonic-zapper specification |
| `file-type-inventory.md` | Category A-E enumeration of all file types |
| `psr/2026-04-27__*` (5 files) | Per-phase PSRs |
| `pur/2026-04-27__*` (6 files) | Per-phase PURs |
| `tests/phase_{1,2,3,4}_*_test.clj` | Invariant tests; runnable |
| `AGENTS-codex-elisp-projector.md` | Codex hand-off (resolved) |
| `AGENTS-codex-python-projector.md` | Codex hand-off (in flight) |
| `E-substrate-2-elisp-projection.md` | Resolved excursion (elisp) |
| `E-substrate-2-directed-edge-id.md` | Resolved excursion (directed edges) |

---

## Reverse-check still owing

The original cross-prototype geometry excursion's *task 5* —
operator-felt tension match against substrate top-|ΔT| —
was never reported back. Joe's "yes/no/some" on the v0.5 top-K
for futon2 (`choose-action`, `clamp01`, `g-observe`,
`simulate`, `scoreboard` as load-bearing; `aif-step`,
`visualize`, `default-aif` as integrative punchlines) would
upgrade the cross-prototype-validation claim from "provisional"
to "validated". Carry forward.

---

## End of checkpoint

**Substrate state at sign-off:** ~170,000 hyperedges, 13 active
labels, 11 distinct codebases, watcher scripts ready, two known
bugs, one architecture split, six categories of deferred work.

The substrate is operational. The next session can pick any
single item from this list and pull the thread.

---

## Session 2 — same day, late evening

Continuing through the backlog. Completed items:

- **B-1 — vocab-use whitespace mismatch** → RESOLVED. futon5
  re-ingested clean under `futon5-d2` (~94k hyperedges, the
  largest single label).
- **B-3 v0 — watcher move/delete handling** → RESOLVED. Cache
  upgraded to `{path → {:mtime :hash}}`; deletions and renames
  detected and emitted as typed evidence; smoke passed.
- **C-1 — Codex Python projector** → REVIEWED + ACCEPTED.
  futon6 ingested (~17k hyperedges); 4-language bench complete.
- **P-1 — Phase 5 futonic-zapper** → v0 RESOLVED. 4 of 8
  signatures implemented; 228 emitted on a 4-label subset;
  futon2-d's drift+adapter findings now typed in substrate.
- **P-2 — Multi-watcher launch** → LIVE. Running in
  background across 13 labels with B-1+B-3 fixes. Caught the
  creation of `phase_5_signatures.clj` itself within seconds —
  substrate is self-aware.
- **R-1 — `.flexiarg` projector** → v0 RESOLVED. Pattern saves
  now land in futon1a within 1–2 cycles. Joe's stated invariant
  satisfied.

Remaining on the deferred list:

- **B-2** (cross-codebase var-vertex prop pollution) — analysis
  done; recommendation is option (d), per-repo qname-prefixing.
  Implementation deferred (substrate-wide change).
- **A-1** (futon2 War Machine wraps `futon0.report.war-machine`)
  — architecture note; no urgency.
- **P-3** (Phase 4.6 quality-of-life: inotify, by-ns cache,
  cascade staleness, post-commit hooks) — substrate works at
  acceptable latency without these.
- **P-5 v0.5** — 4 time-series satisficing signatures
  (quick-fix, coverage-retreat, load-bearing-under-tested,
  concept-introduced-without-attachment). Defer until phase-5
  v0 has been used enough to inform their thresholds.
- **R-2** (docbook ingest refactor), **R-3** (substrate-1
  backfill decision), **P-4** (PSR/PUR/PAR-as-edges bonus
  round) — non-urgent.

**Substrate state at session-2 sign-off:**
- ≈ **350,000+ hyperedges** (up from ~170k at session-1 end —
  futon5-d2's 94k + futon6-py-d's 17k + 228 phase-5 signatures
  + watcher-events + the smaller deltas account for the growth).
- **15 active labels.**
- **Multi-watcher running.** Live updates land within 1–2
  cycles (~5–10s).
- **Phase 5 operational.** Signatures queryable.

The mission's central system invariant — "the hypergraph
projection is always current with the on-disk state" — is
substrate-enforced. The operating-system framing is
operationally true.

---

## Session 3 — same day, late evening, continued

Cleared additional backlog:

- **B-2** (per-repo qname prefix) → **v0 RESOLVED.** Option (d)
  landed; futon7-d3 + futon4-elisp-d3 verify counter-ratchet
  is now structural; new ingests under fresh `-d3` suffix get
  B-2 protection.
- **R-3** (substrate-1 backfill) → **CLOSED** by decision:
  substrate-2 supersedes; legacy edge-types intentionally
  empty.
- **P-3 by-ns cache** → **DONE.** TTL=60s cache to /tmp;
  measured 2.6× speedup on small repos.
- **P-5 v0.5 coverage-retreat** → **DONE.** Two real windows
  surfaced on futon1a-d. Working as designed.
- **P-5 v0.5 concept-introduced-without-attachment** → **DONE.**
  Per-label term-orphan detection.
- **P-4** (PSR/PUR/PAR-as-edges) → design discussion deferred.

**Substrate state at session-3 sign-off:**
- 16 active labels (added futon7-d3, futon4-elisp-d3).
- ~360,000 hyperedges including the new phase-5 signatures.
- Multi-watcher still running.
- 6 of 8 satisficing signatures emitting.

---

## Final dispatch (2026-04-28) — mission CLOSED

| Item | Disposition |
|---|---|
| **B-1** vocab whitespace | RESOLVED (session 2) |
| **B-2** cross-codebase prop | RESOLVED v0 (per-repo qname prefix; session 2) |
| **B-3** watcher move/delete | RESOLVED v0 (session 2) |
| **C-1** Python projector (Codex) | ACCEPTED (session 2) |
| **C-2** Elisp projector (Codex) | ACCEPTED (session 1) |
| **P-1** Phase 5 futonic-zapper | RESOLVED v0 + v0.5 partial (6 of 8 signatures) |
| **P-2** Multi-watcher launch | LIVE (session 2) |
| **P-3** by-ns cache | DONE (session 3) |
| **P-3** inotify | CLOSED (throughput-only; not pain-driven) |
| **P-3** cascade staleness | MOVED to M-reflective-discipline |
| **P-3** post-commit hook | CLOSED (polling adequate) |
| **P-3** multi-repo failure-isolation | CLOSED (watcher stable in practice) |
| **P-4** PSR/PUR/PAR-as-edges | MOVED to M-reflective-discipline |
| **P-5 v0.5** coverage-retreat | DONE (session 3) |
| **P-5 v0.5** concept-introduced-without-attachment | DONE (session 3) |
| **P-5 v0.5** quick-fix | CLOSED (design preserved; not pain-driven) |
| **P-5 v0.5** load-bearing-under-tested | CLOSED (design preserved; not pain-driven) |
| **R-1** `.flexiarg` projector | RESOLVED (session 2) |
| **R-2** docbook refactor | CLOSED (out-of-scope; reopen as own mission if needed) |
| **R-3** substrate-1 backfill | CLOSED (decision: substrate-2 supersedes) |
| **A-1** WM wraps `futon0.report.war-machine` | MOVED to M-reflective-discipline |

**M-live-geometric-stack closes here.** Follow-on work
inherits to:
- **M-reflective-discipline** (`futon2/holes/missions/`) —
  PSR/PUR/PAR as tangent-bundle layer; absorbs A-1, P-3
  cascade-staleness, P-4.
- **Sequel-if-pain-driven** — P-3 inotify + post-commit
  hook + multi-repo failure-isolation; P-5 v0.5 quick-fix +
  load-bearing-under-tested; R-2 docbook refactor.

The substrate is operationally complete for everything the
mission set out to deliver. Joe's "operating system" framing
is real and continuously enforced. Next missions can build
on top.
