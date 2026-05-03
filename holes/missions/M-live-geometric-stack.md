# Mission: Live Geometric Stack (substrate-2)

**Status:** COMPLETE (2026-04-28). All seven phases delivered;
known bugs resolved (B-1, B-2 v0, B-3 v0); cleanup-checkpoint
items dispatched (closed or moved to follow-on missions).
Substrate is operationally live: ~360,000 hyperedges across 16
labels covering 12 distinct codebases; multi-watcher running;
phase-5 satisficing-zapper emits 6 of 8 specified signatures
(remaining 2 closed as not-pain-driven).
**See `holes/labs/M-live-geometric-stack/CLEANUP-CHECKPOINT-2026-04-27.md`
§"End of checkpoint" for the dispatch summary, and
`futon2/holes/missions/M-reflective-discipline.md` for the
follow-on mission absorbing the tangent-bundle layer
(PSR/PUR/PAR-as-edges).**

**Status (historical):** IDENTIFY (2026-04-27)
**Owner:** Joe
**Parent:** This is the **substrate-2** sibling of
`futon4/holes/missions/M-self-representing-stack.md` (substrate-1)
and `futon4/holes/missions/M-three-column-stack.md` (substrate-1
closure). Substrate-1 produced a working three-column hyperedge
store with 1,524+ static edges, 11 ingestion pipelines, and
4 cross-column invariants — *but the empirical work in
`E-cross-prototype-geometry.md` exposed that the existing
substrate is the wrong shape* for the geometric closure of futon
theory the parent mission of this excursion targets. Substrate-2
absorbs substrate-1's plumbing and adds the missing edge classes,
geometric layer, liveness invariant, and cross-prototype
schema-sharing.

**Cross-refs:**
- `futon3/holes/missions/M-pattern-application-diagnostic.md` —
  consumer mission. Pattern-application records sit on the
  substrate this mission delivers. **Tighter coupling than
  "consumer":** pattern applications and SORRY mining /
  promotion are how the *boundary* of the stack grows — high-T
  vertices are mined as SORRYs, SORRYs become typed
  pattern-applications when a constructive move is found,
  and applied patterns reduce T at their target region while
  potentially raising T elsewhere (the "tension is conserved
  locally; the boundary moves" intuition). The substrate-2
  delivers the queryable T-field that makes this dynamic
  observable; M-pattern-application-diagnostic owns the
  question of *how* boundary growth is specified, evaluated,
  and disciplined. Both missions are underspecified on this
  exact interface today; sorting it out — naming the
  precise edge / vertex classes that mark a SORRY, a mining
  event, a promotion, and a pattern-application witness — is
  scoped to M-pattern-application-diagnostic's DERIVE step
  and feeds back to substrate-2's edge-taxonomy lock.
- `futon3/holes/excursions/E-cross-prototype-geometry.md` —
  empirical base. v0.5 + diachronic findings on futon2
  validate the substrate-2 shape and motivate the seven
  satisficing-signature outputs.
- `futon3/holes/excursions/E-math-prototype-pilot.md` — math-side
  validation that the geometric `(T, ∇, Δ)` machinery is
  computable on a typed hypergraph.
- `futon4/holes/missions/M-self-representing-stack.md` —
  substrate-1 source mission. Substrate-2 inherits its
  AIF+ stack-as-organism framing; the stack subsystems
  (futon0..7) remain the addressing schema.
- `futon4/holes/missions/M-three-column-stack.md` — substrate-1
  closure. The 1,524+ existing hyperedges are the bootstrap
  state; substrate-2 does not require them to be re-created.
- `futon4/scripts/ingest-three-columns.py` — existing pipeline
  to extend, not replace.
- `futon5a/holes/missions/M-stack-stereolithography.md` —
  **rationalised by phase 3.** That mission's central
  formulation is: "the stack is accretive; we have many
  capability projections but no single printout that says
  'exists already' vs 'coming out of the bath' with
  provenance." Phase 3 (commits-as-vertices) is exactly that
  substrate: every vertex carries the commit at which it
  appeared; the DAG IS the print schedule; T(v, c) at a
  commit `c` is the bath/exists boundary at that print
  layer; the semilattice the stereolithography mission
  reaches for is the natural structure on the commit-DAG-
  factored hypergraph. This mission does not absorb
  M-stack-stereolithography — that mission owns the
  *render* side (script + storyboard + viewer); substrate-2
  delivers the *substrate* the renderer needs.
- `futon1a/README-best-practice.md` — discipline scaffold.
  The futon1a rebuild produced a reusable best-practice
  document covering layered gate architecture, error shape
  contract, proof-path protocol, counter-ratchet invariant,
  PSR/PUR discipline, and mission-diagram validation. **All
  of these apply to substrate-2 directly** — the watcher
  daemon is structurally a write-pipeline, ingestion failure
  modes have the same five-layer shape, and per-phase
  invariants are best authored *from inside out* using the
  same scaffold. See §"Inside-out invariant scaffolding"
  below.

## IFR (inherited and sharpened)

> **The futon stack is one living object whose geometric
> structure is computable, queryable, and continuously
> current. Any mission, pattern application, code edit, or
> doc revision can be evaluated against the substrate's
> geometry in O(query) time, with results that withstand
> dispute because the substrate is the authoritative source.**

This is the M-pattern-application-diagnostic system invariant
("the hypergraph projection is always current with on-disk
state") promoted to a top-line deliverable, plus the geometric
layer that makes the substrate not merely *reflective* but
*reflexive*: the stack can compute its own `(T, ∇, Δ)` and
identify its own satisficing-failure-mode signatures without
operator narration.

Five sub-properties:
1. **Coverage** (every code/doc/mission unit ingested),
2. **Geometric** (T, ∇T, ΔT, drift derived from typed edges),
3. **Live** (filesystem events update the substrate within
   debounce window),
4. **Cross-codebase** (one schema across futon0..7 + math
   corpus, no per-codebase exceptions),
5. **Self-zapping** (satisficing-signature events surface as
   first-class records).

## Diagnosis — why substrate-1 is not enough

Substrate-1 (M-self-representing-stack +
M-three-column-stack) delivered:

- **Three-column schema:** math/code/project hyperedges.
- **102 namespaces, 396 vars, 268 namespace-level requires
  edges, 9 project tensions, 79 components.**
- **11 ingestion functions** (math, code, project, file
  churn 90-day, indentation complexity, mission provenance,
  pattern provenance, evidence bindings, tension surface,
  cross-futon deps).
- **4 cross-column invariants** (existence-checks like
  "every namespace has docstring", "every scope is
  IATC-referenced").
- **Browser views** in `arxana-browser-*.el`.

The empirical work in `E-cross-prototype-geometry.md`
identified five gaps that substrate-1 leaves unaddressed:

### Gap A — Edge taxonomy is at the wrong granularity

Substrate-1 has `code/requires` (namespace-level imports)
but no var-level `:calls` edges; no body-symbol-resolution
`:coverage` edges; no `:vocabulary-use` edges from code to
vocabulary terms. The v0.5 work showed that **gradient
signal lives on these specific edge classes**: coverage-only
edges have ∇T ≡ 0 by construction; gradient flow requires at
least two edge classes whose endpoint-T distributions can
disagree. Substrate-1 has only the coarsest of these.

### Gap B — Geometric quantities are ad-hoc, not derived

Substrate-1's `ingest_tension_surface` is its own pipeline
rather than a discrete-differential-geometry computation
over the typed edges. T, ∇T, ΔT, drift are not first-class
derived properties on the hypergraph. The seven satisficing-
failure-mode signatures from the futonic-zapper specification
(`E-cross-prototype-geometry.md` §"futonic-zapper specification")
are not computable from substrate-1 today even with all its
data.

### Gap C — Liveness invariant absent

Substrate-1 ingestion runs as `python3 ingest-three-columns.py
--all` on demand. The parent mission's "the hypergraph is
always current" requirement is aspirational, not enforced.
File edits do not propagate; commit events do not propagate;
vocab-doc edits do not propagate. This is the single most
important gap because the geometric layer is only as
trustworthy as the liveness it sits on top of.

**Empirically demonstrated 2026-04-27**, in the most
acute possible form: substrate-1's claimed 1,524+ edges
from M-three-column-stack closure (2026-03-04) are
currently *zero* in the live futon1a store. The mission
satisficed "persistence" with a re-runnable script;
liveness requires the script's output to remain present
without manual re-invocation. This is the **completion
rot** signature listed in §"Surfaces" and the inaugural
real-world detection of it. Every phase of substrate-2
must withstand the same test: at any commit `c`, the
substrate's deliverables must be queryable, not merely
re-derivable.

### Gap D — Time is partly external, partly internal, never first-class

Substrate-1 has `ingest_file_churn` that reads `git log` for
the last 90 days and emits churn-edges. That is one
diachronic signal, but it is not the **commits-as-vertices**
schema that the cross-prototype duality argument requires.
Math's `citation-grounding` edges are first-class temporal
edges in the math column today; the code column has no
analogue. The codebase manifold is asymmetrically poorer
than the math manifold on time, and substrate-2 closes
that asymmetry.

### Gap E — Schema is futon-specific, not domain-agnostic

The parent mission's prototype-1-as-instance-of-a-series
framing requires that the substrate work for math corpora,
legal-text corpora, scientific literature graphs — anything
typed. Substrate-1's column types (`code/namespace`,
`code/var`, `project/devmap`, `project/component`) bake
codebase assumptions into the schema. The math column
escapes this only by having its own
`math/post`, `math/iatc`, `math/scope` types — i.e. the
substrate is a *union* of per-domain schemas rather than
a single abstract typed schema with per-domain projections.

## The formalism

### Vertex types

| Type | Source | Persistent? |
|---|---|---|
| `:entity` | abstract; specialised below | yes |
| `:var` | code: `defn`/`def`/`defmulti` form | yes |
| `:test` | code: `deftest` form | yes |
| `:namespace` | code: `(ns ...)` form | yes |
| `:term` | doc: vocabulary heading / bold / italic | yes |
| `:pattern` | library: `.flexiarg` lift | yes |
| `:commit` | git: SHA + author + timestamp | yes (immutable) |
| `:author` | git: committer email | yes |
| `:doc` | filesystem: any tracked `.md`/`.org` | yes |
| `:mission` | doc: mission lifecycle record | yes |
| `:evidence` | futon1a: PSR/PUR/PAR record | yes |
| `:claim` | math doc: typed claim node (already in math hg) | yes |
| `:proof` | math doc: typed proof node (already in math hg) | yes |

### Edge types (typed, with directionality and provenance)

The full edge taxonomy is the union of v0.5's findings,
the math hypergraph's existing edges, the substrate-1
edges, and the time-axis additions:

| Edge | Endpoints | Purpose | Status |
|---|---|---|---|
| `:calls` | var → var | call-graph (body-symbol resolution) | **new** |
| `:coverage` | test → var | test exercises var | **new** |
| `:requires` | namespace → namespace | ns import (substrate-1) | inherited |
| `:contains` | namespace → var | ns membership (substrate-1) | inherited |
| `:vocabulary-use` | namespace/file → term | text mentions term | **new** |
| `:term-defines` | doc → term | doc defines term | **new** |
| `:edits` | commit → vertex | a commit changed a vertex | **new** |
| `:precedes` | commit → commit | parent commit relation | **new** |
| `:authored` | author → commit | author of commit | **new** |
| `:derivation` | claim → proof | math BHK arrow (math hg) | inherited |
| `:citation-grounding` | doc → doc | external citation | inherited |
| `:pattern-application` | pattern → witness | a pattern was applied here | **new** |
| `:tension-on` | tension → vertex | substrate-1 tension binding | inherited |
| `:trace-path` | mission → vertex* | substrate-1 trace | inherited |

`:edits` plus `:precedes` together promote the commit DAG
to a hypergraph spanning structure (per Joe's
spanning-tree framing); other spanning structures over
the same temporal manifold (vocabulary co-evolution,
component-merge, coverage-frontier) are derived views
computed from `:edits`-tagged vertex changes.

### Geometric layer (derived, not stored)

Computed over any `(vertex-class, edge-class)` selection of
the substrate. Definitions transferred from the math-side
pilot, validated on futon2 in v0.5:

- `T(v, c)` — scalar field at vertex `v` at commit `c`.
  Default for code domain: `T(var, c) = 1` if `var` has no
  incident `:coverage` edge in commit `c`, else `0`.
  Domain-overridable.
- `∇T(e, c) = T(target(e), c) − T(source(e), c)` for any
  edge `e` present at `c`.
- `ΔT(v, c) = Σ_{e: target=v} ∇T(e, c) − Σ_{e: source=v} ∇T(e, c)`.
- `drift(C₁, C₂, c) = Jaccard(V(C₁, c), V(C₂, c))` over
  vocabulary fingerprints of components `C₁, C₂`.
- `∂T/∂t (v, c) = T(v, c) − T(v, prev(c))` along
  `:precedes` edges.
- `∂(drift)/∂t = drift(C₁, C₂, c) − drift(C₁, C₂, prev(c))`.

Geometric quantities are **not** stored as edges — they are
**derived properties**, recomputed on query or maintained
incrementally by the watcher when the underlying edges
change.

### Live ingestion discipline

A watcher daemon per repo (or one per stack subsystem):

- Subscribes to filesystem events on configured roots
  (whole repo minus exclusions: `.git`, `node_modules`,
  build artifacts, large binaries).
- Per-file-type routing: `.flexiarg` → pattern; `.clj/.cljs/.cljc`
  → AST + var/test/namespace edges; `.md` → doc + vocab-term
  edges + cross-ref edges; `.edn` → typed-data record.
- 5 s debounce per file; SHA-256 content hash to skip
  no-op re-ingestions; per-file incremental edge update.
- Cascade: when a file's hash changes, vertices whose
  source the file is get marked re-ingest; their incoming
  hyperedges (the records that *depend* on them) get
  marked `:edge/witness-stale true`.
- Commit-time events: `git post-commit` hook (or polling)
  emits `:commit`, `:authored`, `:edits`, `:precedes`
  edges atomically.
- Self-instrumentation: every ingestion emits
  `event "hypergraph-update"` evidence with
  `{:repo :file :hash :edges-touched :stale-cascade-count
    :latency-ms}` so the pipeline's own health is observable.

This is the parent mission's §"Implementation discipline" —
hardened from sketch to specification.

### Cross-codebase aggregation

One canonical futon1a store. All repos write into it. Each
hyperedge carries a `:repo` label (futon0, futon3a, futon4,
math-corpus, …). The schema is **abstract** (`:entity`,
`:relation`, `:evidence`, `:role`); domain-specific
projections layer on top via labels, not via per-domain
edge types. Math's `:claim`/`:proof`/`:derivation` and
code's `:var`/`:calls` are projections of `:entity` /
`:relation`; the geometric layer reads only the abstract
shape and does not need to know which domain a given
hyperedge comes from.

### Surfaces

Two existing surfaces are re-pointed at the substrate-2
output, no rewrite:

- **Stack HUD** (futon3c) — gains widgets for current `T`,
  current top-K `|ΔT|`, current drift hotspots, and the
  satisficing-failure-mode signature counts.
- **Arxana Browser** (futon4) — gains a "geometry view"
  pane alongside the existing trace / activation /
  evidence views: query subgraph at `(commit, edge-class
  filter)` and visualise.

A new surface, **the futonic zapper**, subscribes to the
satisficing-signature event stream:

- Quick-fix: `ΔT(v) drops at v in c`, `ΔT(neighbours) rises ≥`
- Work-around: new component, vocab-overlap > θ with
  existing component, zero structural edges across.
- Adapter shim that doesn't adapt: component named
  `*adapter*`/`*bridge*`/`*shim*`, `:requires`/`:calls`
  count to claimed adaptee = 0.
- Coverage retreat: paired-vars stable while vars grow
  monotonically across N commits.
- Load-bearing under-tested: `|ΔT|` negative top-K stable
  across N commits without coverage growth on those vars.
- Concept introduced without attachment: term defined in
  vocab-doc commit, no `:vocabulary-use` edges within N
  subsequent commits.
- Concept used without definition: `:vocabulary-use` edge
  to a term not present in any vocab doc.
- Completion rot: a mission marked COMPLETE whose claimed
  `:deliverable` edges are no longer present in the
  current substrate. Discovered live during this
  mission's phase-1 pre-flight (2026-04-27) on
  M-three-column-stack: claimed 1,524+ persistent
  hyperedges; current count is zero across all six
  declared types. The signature is an existence-check
  query against substrate state at HEAD commit.

Each signature is a hyperedge of type
`:diagnostic/satisficing-signature` with
`{:signature :evidence-vertices :evidence-commit-range
:severity}`. The futonic zapper reads these and surfaces
them with operator-acknowledgement (which itself becomes
an evidence record, closing the loop).

### Note: War Machine as keystone finishing surface

Reflection (2026-04-27): `M-war-machine` looks like a
plausible **keystone finishing touch** for substrate-2,
especially once phases 5-6 exist in working form. The fit
is strong: War Machine is hungry for live, directional,
strategic numerics, and substrate-2's `T`, `∇T`, `ΔT`,
drift, and satisficing-signature stream are exactly that
kind of signal.

The architectural rule, however, should be explicit:
**War Machine is a consumer of substrate-2, not the owner
of its geometry.** The canonical source remains the live
typed substrate in futon1a/futon3c. War Machine may
project, rank, and render; it should not be the place
where geometric quantities or signature detectors are
re-implemented.

Likely bridge points:

- a canonical live geometry feed (freshness, head commit,
  top-`|ΔT|`, drift hotspots, signature counts, signature
  records with evidence range and acknowledgement state);
- a join layer from geometry outputs to War Machine's
  strategic objects (repo / mission / AIF spine /
  conflict), analogous to the existing repo→leaf→spine
  join on the AIF side;
- geometry-derived observation channels in the War
  Machine's backend, so the strategic recommendation
  surface actually *uses* substrate-2 rather than merely
  decorating itself with it;
- acknowledgement / promotion paths that write back to the
  canonical evidence landscape instead of terminating in
  the surface.

Important caveat: if this lands as "more counts in more
tiles", the bridge will weaken both missions. The good
version is a one-way strategic projection: substrate-2
computes the pressure field; War Machine turns that field
into operator-legible strategic synthesis with witnesses.
Concrete interface design can wait until the next session.

## Sequencing

Phased so each phase is operator-checkable before the next:

1. **Phase 1 — Edge taxonomy lift.** Add `:calls`,
   `:coverage`, `:vocabulary-use`, `:term-defines`,
   `:term` to the substrate-1 ingestion pipeline (extend
   `ingest-three-columns.py` or fork). Verify the v0.5
   geometry reproduces from XTDB queries the same answers
   the bb scripts produce now. **Empirically grounded;
   v0.5 outputs are the regression test.**

2. **Phase 2 — Geometry as derived layer.** Implement
   T, ∇T, ΔT, drift as XTDB-query-backed functions (not
   ad-hoc Python ingestion). Verify they match the v0.5
   numbers on futon2.

3. **Phase 3 — Commits as first-class.** Add `:commit`,
   `:authored`, `:edits`, `:precedes` ingestion. Walk
   `git log` per repo on first ingest; subsequent ingests
   are incremental. Verify diachronic queries on futon2
   match the time-series in
   `E-cross-prototype-geometry.md` §"Diachronic results".

4. **Phase 4 — Liveness.** Watcher daemon, one repo at a
   time. futon2 first (small, recent
   pattern-application-diagnostic excursion already
   uses it). Then futon4. Then the rest of the stack.
   Each repo onboarded is a checkpoint with self-
   instrumentation evidence.

5. **Phase 5 — Satisficing-signature emission.**
   Implement the seven signature detectors over the
   geometric layer; each emits
   `:diagnostic/satisficing-signature` hyperedges.
   Verify on the v0.5 futon2 baseline (three signatures
   are detectable today; the rest unlock with phase 3).

6. **Phase 6 — Surface re-pointing.** Stack HUD widgets;
   Arxana Browser geometry view; futonic-zapper console.
   Cosmetic, but the value to the operator is the
   visibility.

7. **Phase 7 — Cross-codebase.** All repos onboarded.
   The math column is re-ingested under the abstract
   schema (`:entity`/`:relation`) so the geometric layer
   reads it the same way it reads the code column.

Each phase is independent enough that a parallel mission
or excursion can start while another is in DERIVE; the
ordering above is dependency-ordered, not exclusive.

### Pyramidal expansion strategy

Per Joe (2026-04-27): rather than driving phases 1→7 to
completion on a single codebase before touching the next,
**expand pyramidally**: do phase 1 on futon2 (smallest
substrate), then phase 2 on futon2 (validate the geometric
layer against the v0.5 numbers we already have), **then
return to phase 1 at broader scope** — futon3, futon4,
futon3c, futon3a — refining the schema and the L1/L2
invariants under the pressure of more diverse source.
Then phase 2 broadens. Then phase 3 (commits-as-vertices)
on futon2, then broadens, and so on.

Why pyramidal: each phase's invariants are tested at small
scope first; the schema stays alive and revisable until any
given phase has been done across the stack. A wide-scope
phase-1 run will surface schema problems the futon2-only
run cannot — and we'd rather discover those *before* phases
3-7 build atop frozen-in choices.

Phase ordering within a codebase remains as listed; the
*expansion* is across codebases at each phase, not across
phases within a codebase.

### Bonus round — moved out 2026-04-28

The earlier framing of "PSR / PUR / PAR as first-class
hyperedges" as a substrate-2 bonus round has been **moved
to a separate mission** at
`futon2/holes/missions/M-reflective-discipline.md`
(IDENTIFY 2026-04-28).

The reason for the move is theoretical, not just
organisational. Joe's framing (2026-04-28):

> "PARs are tangent vectors, we are still just building
> the manifold."

Substrate-2 builds the manifold (state on the futon stack —
vertices/edges/T-field). PSR/PUR/PAR captures *motion* of that
state — directional derivatives at points on the manifold.
They belong to a tangent-bundle layer over substrate-2, not
inside it.

Also: PAR is **Project Action Review** (Peeragogy lineage),
not Post-Action Review or Pattern Application Record. It is
linked to but not identified with patterns; substrate-2's
schema must keep the layers separate.

Substrate-2 retains query-side bridges (any `:psr` / `:pur`
/ `:par` record can cross-ref substrate-2 vertices it bears
on) but does not absorb the writer responsibility. The
canonical writer for the REPL era will be a War Machine UI
(per the new mission); the legacy `discipline.clj`
peripheral and `/psr` `/pur` `/par` slash commands inform
the schema but do not return as-is.

Sequencing: this bonus round is **after phase 5** at the
earliest (because PSR/PUR-as-edges depends on commits
being first-class, phase 3), but **before phase 7** so the
cross-codebase aggregation can absorb the discipline.

Mark in this mission's open-questions list as a candidate
sub-mission once phase 5 is done.

## Inside-out invariant scaffolding (per phase)

`futon1a/README-best-practice.md` provides the authoring
discipline. Each phase of substrate-2 should land its
invariants *from inside out* — innermost (durability,
identity) before outermost (validation, model) — using the
same five-layer error shape:

```
{:error/layer N
 :error/status HTTP-status
 :error/reason keyword
 :error/context map}
```

### Watcher daemon as a write-pipeline

The watcher daemon is structurally identical to futon1a's
write pipeline. File events flow L4 → L3 → L2 → L1 → L0:

| Layer | Watcher gate | Failure example |
|---:|---|---|
| 4 | **Validation:** file-type recognised; parser succeeds | `.clj` reads as forms; `.md` term-extraction returns ≥1 candidate |
| 3 | **Authorization:** repo on the configured root list; not in exclusion set | Reject events for `.git/`, `node_modules/`, build artifacts |
| 2 | **Integrity:** extracted vertices have stable IDs; symbol resolution is deterministic | `:var/qname` consistent across rebuilds for same source state |
| 1 | **Identity:** edge stable IDs (e.g. `hx:calls:src/dst`) match substrate-1's scheme | No duplicate edges from re-ingest; merge / dedupe correct |
| 0 | **Durability:** XTDB write succeeds; bitemporal `valid-time` set to file-event timestamp | Write retries on transient failure; failure cascades up |

Each layer carries the futon1a error shape. A failed
ingest produces an evidence record, not a silent drop.

### Per-phase invariants (sketches)

These are the invariants substrate-2 should enforce
*from the inside* of each phase, not as audit-after work:

| Phase | Invariant | Layer |
|---:|---|---:|
| 1 | Every `.clj/.cljs/.cljc` source file with an `(ns …)` form produces ≥1 `:var` or `:test` vertex (no silent skip). | I-2 (integrity) |
| 1 | Every `:calls` edge has both endpoints resolved to existing `:var` vertices in the same revision. | I-1 (identity) |
| 1 | Every `:vocabulary-use` edge points to a `:term` vertex defined by some `:term-defines` edge. | I-1 |
| 2 | `T(v, c)`, `ΔT(v, c)`, `drift(C₁, C₂, c)` queries are **deterministic** given the substrate at commit `c`. | I-2 |
| 2 | Geometric quantities never disagree between an XTDB query and a fresh-projection bb script over the same source state (regression invariant). | I-2 |
| 3 | The commit DAG is acyclic; every `:precedes` edge is consistent with `git log --topo-order`. | I-1 |
| 3 | Counter-ratchet on commits: the count of `:commit` vertices is monotonic per repo (commits are immutable; ingestion is idempotent). | I-2 |
| 4 | Every filesystem event produces either a hyperedge update or an evidence record explaining why not (no silent drops). | I-2 |
| 4 | Watcher latency from file event to hyperedge update is bounded; SLA is a public health metric. | I-0 |
| 5 | Every `:diagnostic/satisficing-signature` carries `:evidence-vertices` and `:evidence-commit-range` such that the signature can be re-derived from those alone (auditable). | I-2 |
| 5 | Operator acknowledgement of a signature produces a `:diagnostic/acknowledgement` evidence record citing the signature's hyperedge ID. | I-1 |
| 6 | Surface re-pointing is config-only; no surface code path queries the old `tension-surface` pipeline once the geometric layer is live. | I-3 |
| 7 | Cross-codebase abstract schema preserves all existing math/code/project query shapes (no regression on substrate-1 consumers). | I-2 |

### Proof-path discipline for ingestion writes

The futon1a 8-phase proof-path applies one-to-one to a
watcher ingest of one file:

```
clock-in → observe (file event)
  → propose-claim (parsed vertices/edges)
    → apply-change (XTDB write)
      → verify (read-back-equal)
        → invariant-check (per-phase invariants above)
          → proof-commit (commit message: file-hash + edge-count)
            → clock-out (latency + edge-count emitted as evidence)
```

This is *not* paperwork — it is the audit trail that lets
us answer "why does the substrate currently believe X" by
walking back through the proof-paths. For an agent-driven
build, this is the substrate's analogue of the *witness*
property the IFR demands of pattern applications.

### PSR/PUR per phase

Each of the seven phases gets:
- **PSR before:** which patterns from `futon3/library/`
  ground the design choices (e.g. `code-coherence/dead-
  code-hygiene`, `aif/structured-observation-vector`,
  `realtime/liveness-heartbeats`).
- **PUR after:** what worked, what didn't, prediction
  errors. PURs feed back into pattern-application records
  in M-pattern-application-diagnostic — the build of this
  mission *is* an exercise of that mission's diagnostic.
  Reflexive in the right way.

PSR/PUR pairs live in `futon3/holes/labs/M-live-geometric-
stack/psr/` and `pur/`.

### Mission-diagram validation

Per `futon1a/README-best-practice.md` §6: ship a machine-
checkable `mission-diagram.edn` describing the substrate's
component graph, with `:accepts` and `:produces` for each
component (watcher route, ingester, geometric-layer query,
signature emitter, surface). Validate with futon5's
`m/validate` before each phase gate. The diagram is the
single source of truth for boundary types; if it doesn't
validate, the phase isn't done.

The watcher routes are the components most likely to
expose type-mismatch bugs (input `.clj` text vs output
`:var` hyperedges; input `.md` text vs output `:term`
hyperedges). Naming `:accepts`/`:produces` precisely is
exactly what futon1a's "components are morphisms"
insight pays off here too.

### Boundary-growth interface (joint with M-pattern-application-diagnostic)

The substrate-2 must publish enough of its T-field for
M-pattern-application-diagnostic to *act* on the boundary,
not merely observe it. Concretely:

- **Mining:** the futonic zapper's high-`|ΔT|` and
  drift-hotspot outputs are SORRY candidates. They are
  the substrate's nomination of "places that need a
  pattern application." A SORRY is then a typed vertex
  with an open `:tension-on` edge.
- **Promotion:** when an operator (or agent) chooses to
  address a SORRY, a `:pattern-application` edge is
  written from a candidate pattern to the SORRY's
  affected vertices. Substrate-2 stores the edge; M-
  pattern-application-diagnostic specifies the schema
  and validates the application.
- **Witness:** the resulting code edits or doc revisions
  produce normal `:edits` events that update T at the
  affected vertices. The pattern-application is closed
  if T drops at the targeted region without a
  compensating rise outside it (the line-integral
  property from §"Geometric commitment" of the parent
  excursion's parent mission).
- **Boundary motion:** the set of vertices with `T = 1`
  shifts as applications close and new mining events
  open. The "stack boundary" is exactly this set, and
  it is queryable in O(query) time once substrate-2 is
  live.

This is the dynamic Joe named ("pattern applications and
SORRY mining and promotion are how the boundary of the
stack grows"). It is underspecified at the schema level
today; the schema landing is M-pattern-application-
diagnostic's DERIVE feeder #5 ("witness format for the
futon-stack domain"). Substrate-2 should *not* freeze its
edge taxonomy until that DERIVE is at least ARGUE-quality.

## Open questions (DERIVE-feeders)

1. **Storage choice.** Stay with futon1a XTDB or migrate
   to a different bitemporal store? XTDB has bitemporal
   queries, which is the natural home for `T(v, c)`-style
   queries; substrate-1 is already on it. Default: stay.
   But verify XTDB query performance on the expected
   edge volume (10⁵–10⁶ edges across the stack).

2. **Watcher implementation language.** Python (extend
   substrate-1) vs Clojure (futon3c JVM, sub-millisecond
   latency, can call into XTDB directly). Default:
   Clojure on the JVM, with a Python adapter for any
   futon-stack subdir whose authoring tools are
   Python-native.

3. **Schema versioning.** Substrate-1 hyperedges have
   stable IDs (`hx:{type}:{sorted-endpoints}`); adding
   new edge classes is additive, but renaming or
   re-shaping existing types is not. The transition
   plan needs a schema-version property and a migration
   pass.

4. **Cross-prototype generalisation.** The math column
   currently has its own `math/post`/`math/iatc` types;
   migrating to abstract `:entity`/`:relation` is
   technically additive but conceptually a re-think of
   how math content is queried. Coordinate with futon6
   ownership before this lands.

5. **Watcher event volume.** Heavy editing sessions
   (especially during agent-driven development) can
   produce 100s of file events / second. Pause / resume
   discipline + evidence emission rate-limit needed.
   Spike on a typical agent session before phase 4.

## What this mission does NOT scope

- **Pattern authoring.** This mission builds the substrate;
  M-pattern-application-diagnostic builds the pattern-
  application records that sit on the substrate; pattern
  *authoring* (deciding which patterns the futon stack
  needs) lives elsewhere.
- **The futonic zapper UI.** Phase 6 ships the surface;
  the *interaction discipline* (when to show, how to
  acknowledge, how to roll up) is its own mission.
- **Persistent homology, Forman-Ricci.** The parent
  mission's §"Notes on prior art and feasibility" lists
  these as cheap-on-this-substrate but not-yet-load-bearing.
  This mission ships the substrate they'd run on, not the
  computations themselves.

## Notes

- The motivation is Joe's framing (2026-04-27): "M-self-
  representing-stack was not enough... having done a little
  bit of exploration now, maybe we're starting to be able
  to specify the next formalism." The exploration is
  `E-cross-prototype-geometry.md` v0 → v0.5 → diachronic;
  this mission is that exploration becoming a buildable
  spec.
- The **central value-add** of the geometric closure is
  not "more diagnostics" — it is converting prose-form
  futon theory ("don't satisfice short-term") into
  substrate-enforced discipline. The v0.5 work shows the
  mechanism: signatures of failure are computable from
  the substrate alone, with no operator narration. This
  removes the need for theory to be *read and remembered*;
  the substrate enforces it.
- The phasing is deliberately incremental against
  substrate-1: every phase delivers operator-visible
  value, every phase is checkable against the v0.5
  empirical baseline, and no phase requires re-creating
  what substrate-1 already produced.
- The inside-out invariant scaffolding (§ above) means
  substrate-2 *cannot regress* relative to substrate-1
  by accident: each phase gate has explicit invariants
  that an XTDB query can verify, and the watcher's own
  evidence stream is the live audit trail.
- Building the substrate is itself a pattern-application
  exercise. Each phase's PSR/PUR records become inputs to
  M-pattern-application-diagnostic, and any signatures
  the watcher emits during the build (e.g. quick-fix
  detected during phase 4) directly feed the operator's
  oversight loop. The substrate is its own first user.

## Checkpoint - 2026-05-01: live predicate audit, E-live-means-live opened

Discovered while running a stepper trial for M-war-machine-wiring's
VERIFY Checkpoint 1 (`futon5a/holes/missions/M-war-machine-wiring.md`):
the COMPLETE status this mission landed on 2026-04-28 holds at the
schema / watcher / query level, but the **"live" predicate is
partial**.

Specifically: var/test/namespace coverage is real-time (~5s, multi-
watcher). Commit coverage is **manual-batch** — populated only when
someone runs `bb scripts/ingest_commits_to_futon1a.clj`. Sample
freshness probe (2026-05-01): latest-indexed commit per repo ranges
from 4 to 19 days behind `git log -1`. Recent commits exist as
files (which the watcher tracks for var-level changes) but do not
yet exist as commit-vertices in substrate-2.

This is the same anti-pattern `M-reachable-from-boot` opened a
STOP-THE-LINE family for in `futon3c` 2026-05-01 (long-lived state
populated only at operator-driven activation time; "loaded guns on
Chekhov's desk"). On reflection, substrate-2's commit ingestion is
a sibling of that family that wasn't in M-reachable-from-boot's
INSTANTIATE scope.

The fix path is logged in a dedicated excursion:
`futon3/holes/excursions/E-live-means-live.md`. That excursion
covers (a) closing the freshness gap (cron the script, or
watcher-integrate, or both), (b) auditing the "live" claim per
vertex class, and (c) proposing an invariant sibling under
`reachable-from-boot/<container>` so the freshness gap can't drift
silently again.

Status of THIS mission stays COMPLETE for what was actually built;
the excursion holds the "live-means-live" follow-up.
