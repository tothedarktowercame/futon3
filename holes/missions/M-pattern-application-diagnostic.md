# Mission: Pattern Application Diagnostic

**Status:** IDENTIFY (2026-04-27)
**Owner:** Joe
**Parent:** `holes/missions/M-pattern-mining.md` (the IFR was articulated
there during the activation-browser verdict on 2026-04-27; this mission
is the **build-the-new** sibling, with M-pattern-mining as the
**retire-the-old** counterpart).
**Cross-refs:**
- `library/futon-theory/task-as-arrow.flexiarg` — the BHK substrate.
  This mission generalises that pattern from "tasks" to "pattern
  applications" by treating each `context → tension → move` invocation
  as a BHK arrow whose witness is the resulting codebase / corpus
  state.
- `library/futon-theory/structural-tension-as-observation.flexiarg` —
  the upstream pattern that frames tension as the observable signal
  of mission progress.
- `futon4/holes/missions/M-futon-enrichment.md` — the existing
  enrichment-layers plan (var→mission, var→pattern, var→evidence,
  var→tension; subsumes Tornhill's hotspot / temporal-coupling
  analysis via reflection + cross-column invariants). Today's mission
  is **not** a replacement for that plan; it is the **substrate
  upgrade** that makes its static-analysis layers cheap and
  continuous instead of one-shot reconstruction passes. See the
  System-invariant section's "What this means for M-futon-enrichment"
  subsection for the concrete reframe.
- `futon5a/holes/excursions/E-btoa-candidate-queue-reweighting-v0.md`
  — the B→A reweighting design, which is the same problem from the
  operator side.
- `futon5a/holes/excursions/E-candidate-queue-xtdb-shape-v0.md` —
  hybrid XTDB shape (canonical / run / snapshot triple). The A-side
  of pattern-application records should adopt the same triple.
- `futon5a/holes/missions/M-trip-journal.md` — installs the
  hinge-log register; will consume A-side typed records once they
  exist.
- `futon3/holes/missions/M-pattern-mining.md` — sibling; carries the
  retirement of the futon3a MiniLM substrate.

## IFR (inherited from M-pattern-mining)

> **Each turn A→B is coded as an activation of a pattern that is
> well-specified, repeatable, and of demonstrable value in the
> codebase; this means we have a good understanding of our own
> workflow and it has a high chance of being replicable, even under
> full automation.**

Five properties unpacked: **coverage** (every turn coded),
**well-specified** (typed slots), **repeatable** (deterministic),
**demonstrable value** (verifiable witness), **replicable under
automation** (self-explaining).

## Prototype-1 framing — self-application is one instance, not the only frame

This mission scopes to **prototype 1: the futon stack codebase**
**and prototype 2: the math corpus, running in parallel** (see the
"Prototype 2 — math corpus, substrate-half-built" subsection
below for why the math side is materially further along than was
anticipated when the mission was first scoped).

The architecture is being designed so that *the same machinery
projects onto multiple domains* — explicitly:

- **Prototype 1 (active):** the futon stack codebase. Self-application:
  the system applies its own pattern-machinery to itself.
- **Prototype 2 (active, ahead):** mathematical content as a
  hypergraph. Patterns become inference rules; pattern applications
  become proof steps; witnesses become theorem fragments. ~40k
  arxiv-math papers already processed by Rob's superpod-mark2
  pipeline (`futon6/scripts/superpod-job.py`). See the dedicated
  subsection below.
- **Prototype N (anticipated):** other typed knowledge corpora —
  legal-text reasoning, scientific literature graphs, etc.

The futon stack is **prototype 1 because it is self-application**:
the system applies its own pattern-machinery to itself. That is the
strongest possible test bed because (a) we author the corpus and
the patterns, so we can co-evolve, (b) the witnesses are already
under version control, and (c) any failure of the diagnostic shows
up immediately in our own working life. Self-application also
prevents the architecture from leaning on domain accidents we'd
miss in a third-party corpus.

**Design implication:** abstractions that work *only* for code are
disqualified. Edge types like `(file, function, import)` won't
generalise; the schema needs domain-agnostic primitives like
`(entity, relation, evidence, role)`. Pattern slots like "code
units changed" must be projections of more abstract slots like
"witness tokens." The hypergraph schema is a *typed* schema, not a
code-specific one.

The futon stack benefits from being prototype 1 even if no other
prototype ever ships: getting it right gives us the diagnostic for
our own workflow. But the prototype-1 framing protects us from
narrowing the design prematurely.

## Prototype 2 — math corpus, substrate-half-built

### What's already in hand (2026-04-27 inventory)

Rob's superpod-mark2 pipeline has been running for weeks. Pulling
the latest `~/mark2/outbox` into local storage as of 2026-04-27:

| Lineage              | Papers | Era             | Per-paper density (avg) |
|---------------------|--------|-----------------|-------------------------|
| `results-001..006`  | 40,000 | 2001-era arxiv-math | n_nodes ≈ 9, n_edges ≈ 1 |
| `results-mfuton-001..002` | ≥10,000 | 2026 modern arxiv | **n_nodes ≈ 222, n_edges ≈ 17, theorem/proof blocks parsed** |

Pulled to `~/code/storage/mark2/outbox/`. Old abstract-only
versions of results-001..003 (pre-fix, ~30 % less data per file)
archived to `~/code/storage/mark2/outbox.abstract-only/`.

### The substrate already produces what the IFR specifies

This is the surprising find. Reading
`output/reverse-morphogenesis.json` from any results batch shows
each paper's analysis already has the BHK-arrow-shaped slots the
IFR called for:

```json
{
  "entity_id": "arxiv-2604.20840v1",
  "analysis": {
    "xiang_form":      "...",   // form / type of the problem
    "xiang_salience":  "...",   // what's salient — TENSION
    "arrow_constraint":"...",   // the constructive MOVE (literal "arrow")
    "situation_S":     "...",   // the CONTEXT
    "quality":         {form: "good", salience: "good", arrow: "good"},
    "roundtrip_check": "Yes, ..."
  }
}
```

Slot mapping onto §Scope's typed pattern shape:

| §Scope slot       | Already populated as     |
|-------------------|--------------------------|
| `:context`        | `situation_S`            |
| `:tension`        | `xiang_salience`         |
| `:move`           | `arrow_constraint` (literal BHK arrow) |
| `:witness-shape`  | `quality` + `roundtrip_check` (partial) |
| `:domain`         | implicit `:math-corpus`  |

The hypergraph projection is also already there. Each paper has
typed `nodes` (concept, technique, equation, term-use, section,
claim, proof, definition) and typed hyperedges (`type:
"derivation"` with `roles: {claim: target, proof: proof_of_target}`,
`type: "definition-use"`) carrying `provenance: classical|llm|...`
and natural-language `rationale` strings. **That is a typed
hypergraph with provenance — exactly what the codebase side has
zero of.**

### One real gap and two technical issues

1. **`pattern-tags.json` is empty for ≈99 % of papers.** Spot-check
   confirms: `[]` on most, occasional non-empty tags like
   `["work-examples-first", "reduce-to-known-result",
   "split-into-cases"]`. **Root cause:** the Stage 3 prompt
   (`superpod-job.py:1568`) is the math.SE-derived
   *"analyse this Q&A"* prompt with the futon6/P0 25-pattern Q&A
   list. arxiv papers aren't Q&As; the LLM correctly returns `[]`
   because the prompt asks about a structure that isn't there.
   Fix is runner-side; tracked in
   [GH issue: superpod-mark3 prompt + eprint defaults]
   (see subsection "Runner changes filed for Rob" below).

2. **Regular results-001..006 ran without eprint mode**, hence
   `n_blocks: 0, has_theorem_blocks: false`. The parser
   (`futon6/src/futon6/paper_hypergraph.py`) supports theorem/proof
   blocks; the runner (`superpod-job.py`) supports passing
   `--discover-terms-eprint-dir`. The mfuton variant exercises
   both. The fix is invocation-level, not code-level. Same GH
   issue.

3. **Pattern set is wrong for arxiv content.** The 25-pattern
   list is curated for math.SE answer reasoning ("work examples
   first," "reduce to known result"). A theorem/proof corpus
   needs a different pattern vocabulary — proof-strategy
   patterns (induction, contradiction, construction, reduction,
   counterexample, generalisation, specialisation,
   diagonalisation, …). Curation is a deliberate exercise; not
   the runner's job to invent the list.

### Cross-pollination — math's slots inform prototype 1

Math has done the IDENTIFY-step DERIVE work without naming it as
such. Specifically, the `(situation_S, xiang_salience,
arrow_constraint)` triple is a working example of the typed
pattern shape M-pattern-application-diagnostic specified for
prototype 1. **First DERIVE move on the codebase side becomes:
test whether the same triple can describe a code-edit turn.**

Concretely:

- `situation_S` → "what was the codebase state when the agent
  arrived at this turn?" — a region of the codebase hypergraph
  the agent's context-window touches.
- `xiang_salience` → "what tension was salient?" — measurable
  via the geometric commitment (T at the agent's region of
  interest).
- `arrow_constraint` → "what move was the agent constrained to
  make?" — the typed direction in the codebase hypergraph.
- `roundtrip_check` → "given the result of the move, does
  re-running the analysis from the witness reproduce the
  triple?" — the IFR's *demonstrable value* property in
  operational form.

If the triple describes a code-edit turn cleanly, **the
prototype-1-as-instance-of-a-series claim is empirically
validated, not just argued.** If it doesn't, we learn which
slot the codebase domain demands that the math domain doesn't,
and the schema gets refined. Either outcome is informative.

### Runner changes filed for Rob

Rather than land prompt + parser changes locally on a branch
(which would still need superpod redeploy), filed as a single
GitHub issue covering:

- (R-1) Stage 3 prompt fork: arxiv-aware prompt builder, taking
  `paper_id`, `title`, optional abstract, optional theorem/proof
  block excerpts. Default to math.SE prompt when input shape is
  Q&A; fork to arxiv-prompt when input shape is paper.
- (R-2) Pattern-set curation: a separate
  `arxiv-proof-patterns.edn` (≈15-25 entries: induction,
  construction, contradiction, reduction, …) referenced by the
  arxiv-prompt builder. Authoring the list is operator work, not
  Rob's; the runner fix is the swap mechanism.
- (R-3) eprint-mode as default for arxiv batches:
  `--discover-terms-eprint-dir` enabled by default for any batch
  whose source is arxiv (vs math.SE). The mfuton lineage proves
  the pipeline supports it; just turn it on.
- (R-4) Re-run results-001..006 with the patched runner once
  R-1..R-3 land. The substrate is large enough that the re-run
  cost is non-trivial but produces 40k full-text papers with
  proper pattern tags — strict win over the current state.
- (R-5) Suggest renaming the runner to **superpod-mark3** if
  R-1..R-4 land together; this is enough of a regime change to
  warrant a version cut, and "mark3" gives the new pipeline a
  clean handle distinct from the existing mark2 manifest /
  outbox state.

Issue URL:
[futon6 #45](https://github.com/tothedarktowercame/futon6/issues/45)
(filed 2026-04-27).

### What this means for sequencing

Sequencing in §Sequencing-within-this-mission was structured for
prototype 1 with prototype 2 as anticipated. Revised under the
parallel-running framing:

1. **Pilot lift** of ≤20 patterns into the typed schema —
   prototype 1, blocked on no one. Run independently while Rob's
   patches land.
2. **Math-side slot validation** — read 100 random
   `reverse-morphogenesis.json` analyses and check whether the
   `(situation_S, xiang_salience, arrow_constraint)` triple is
   well-populated for arxiv papers; this is the substrate-quality
   check before we commit to the slot vocabulary. Doable now.
3. **Witness-check spike** — pick 1 codebase pattern + 1
   arxiv-paper pattern, attempt to re-derive their witnesses
   under the geometric commitment. If both work, schema is
   sound across prototypes.
4. **(After R-1..R-4 ship)** Math-side pattern-tag pass: with
   proper prompt + pattern set, re-run on the existing 40k
   papers and use the populated tags to seed the codebase
   side's pattern-discovery process.
5. **Watcher invariant** — last, as before.

If at DERIVE the math-side substrate validation reveals
problems we can't anticipate from a 100-paper read, spawn
`E-math-corpus-fix1.md` as an excursion alongside this mission
rather than a sibling mission, per Joe's "don't proliferate
missions" preference.

### First pilot result (2026-04-27)

`holes/excursions/E-math-prototype-pilot.md` ran the §What this
means for sequencing items 2 + 3 ahead of Rob's R-1..R-4
patches: hand-tagging on 8 mfuton papers + first geometric-
quantity demo (tension scalar `T = unpaired-claim-indicator` on
paper #5 Zarankiewicz). Findings:

- Triple is well-populated 7/8; one silent-fail (≈12 %, mirrors
  codebase side's pre-fix Codex coverage gap).
- One *collapsed* triple — slots indistinguishable. Schema
  should accept collapsed cases as informative, not malformed.
- The `derivation` edge type (`claim → proof_of_claim`) is
  literally a BHK arrow at the edge level. **The math substrate
  encodes BHK arrows in its raw output**, before any of our
  pattern-application machinery.
- Hand-tagged 7 distinct patterns across 7 papers (sharp-
  dichotomy, cross-domain-bridge, structural-equivalence,
  foundational-clarification, failure-mode-characterization,
  construction-from-known-structure, algorithm-with-error-
  bounds). None overlap with the futon6/P0 math.SE pattern
  list — confirms R-2 (curation of `arxiv-proof-patterns.edn`)
  is necessary, and gives us 7 seed entries.
- Tension scalar with the trivial definition correctly surfaces
  the *punchline* claims of the paper (high-T = stated-but-not-
  proven-here = active research direction). The Laplacian over
  this T identifies the load-bearing concepts the punchline
  depends on.

Cross-prototype takeaway: **the geometric commitment is
already operational on the math side.** Tension, gradient, and
Laplacian are computable with naive definitions on the existing
hypergraph, no further substrate work needed. Six concrete
transfer items to prototype 1 listed in the excursion's
"Transfer to prototype 1" section.

### Phase-2 result (2026-04-27): 25-paper extension

`E-math-prototype-pilot.md §"Phase 2 — 25-paper extension"`
expanded the sample across mfuton-001, mfuton-002, and
results-005 (older lineage). 24/25 papers tagged cleanly,
producing **15 distinct leaf patterns that cluster into 5
families**: existence, characterization, structural-relation,
property-of-object, clarification (meta).

Findings:
- Pilot's flat 7-pattern vocabulary did not saturate. The
  5-family hierarchy did (each family ≥ 2 papers).
- 58 % of papers fell into "characterization" or
  "structural-relation" — suggesting most math research is
  *describing what something is* or *relating two things*,
  with existence-from-scratch a smaller slice.
- Triple quality is largely independent of hypergraph
  richness. Hand-tagging works equally well on the abstract-
  only `results-005` lineage as on the full-text mfuton
  papers. R-1 (prompt) and R-3 (eprint) are independent
  patches that can land independently.
- Direct deliverable for R-2: a draft hierarchical
  `arxiv-proof-patterns.edn` (5 family parents + 15 leaves
  = 20 entries) is sized for authoring in one focused
  session, not a curation deadlock.

Two stub excursions parked for the deferred next-moves so
they don't get lost:
- `holes/excursions/E-Ttotal.md` — paper-level T_total
  scalar across a batch (open-question item 1).
- `holes/excursions/E-cross-prototype-geometry.md` —
  codebase-side `(T, ∇, Δ)` validation (open-question item 4).

### R-2 deliverable — 2026-04-27

The hierarchical arxiv-proof pattern set landed in
`futon3/library/`:
- 5 family parents in `library/math-strategy/`
  (existence, characterization, structural-relation,
  property-of-object, clarification-meta).
- 5 genuinely new leaves in `library/math-informal/`
  (failure-mode-characterization, structural-characterization,
  structural-inclusion, complexity-classification,
  structural-equivalence).
- 1 operator-readable index at
  `library/math-strategy/PAPER-SHAPES-INDEX.md` mapping families
  to existing + new leaves with exemplar arxiv paper-ids per
  family.

The taxonomy is the choice space the arxiv-aware Stage 3 prompt
in `M-superpod-mark3` consumes (see PAPER-SHAPES-INDEX.md §"Use
as a prompt choice space"). It validates the parent mission's
slot schema empirically: 24/25 papers in the pilot fit the
five-family hierarchy without forcing.

## Geometric commitment — tension as differential geometry, not vibes

If the codebase has a geometric projection (the System Invariant's
core claim), then every term in the pattern shape `context →
tension → move` lifts to a precise object on that projection.
**Tension is not a label or a feeling; it is a scalar field on
the hypergraph.** Patterns are vector fields. Applying a pattern
is following the negative gradient locally. The "demonstrable
value" property of the IFR is a line integral. Without this
commitment we are back to vibes — and the entire mission collapses
into another decoration layer.

The realisation is **discrete differential geometry**, not smooth
manifolds. The hypergraph (vertices = code-units / docs / mission
records; edges = typed relations among them) is the carrier; the
operative calculus is Forman-style combinatorial calculus and
discrete exterior calculus. Tractable; computable; no smooth-
structure infrastructure required. The smooth analogue is
mathematically illuminating but not load-bearing.

### Correspondences

| Operational term                    | Geometric realisation                                                  |
|-------------------------------------|------------------------------------------------------------------------|
| "tension" at a location             | Scalar field `T : V → ℝ` on hypergraph vertices/edges                 |
| "applying a pattern reduces tension" | Following `-∇T` along an outgoing edge                                |
| "the pattern's move"                | Vector field (assignment of an out-edge at each vertex in the pattern's region) |
| "demonstrable value" (IFR)          | Line integral `∫_path -∇T` along the move's witness-path is positive  |
| "sorry" / "unresolvable"            | Critical point (∇T vanishes locally) or singularity (T diverges)      |
| "repeatable" (IFR)                  | Vector field is single-valued at every point in the applicable region |
| "hotspot" (Tornhill, M-futon-enrichment) | High-Laplacian region (`ΔT = div ∇T` large) — local curvature      |
| "drift between code and docs"       | Parallel-transport failure across the code-projection and doc-projection |
| "prototype-2 generalises"           | Same calculus, different manifold (codebase ↔ math corpus)            |

### What this gives us operationally

- **Tension becomes measurable.** "Is there tension at vertex v?"
  has a numerical answer. "Is this a tension hotspot?" is a
  Laplacian computation. "Did the move reduce tension?" is an
  integral. None of these require human judgement; they require
  the field T to be defined and computable.

- **The witness-check becomes a positivity check.** A pattern
  application's claim is "applying this move reduces tension by
  Δ"; the witness check is "compute T before and after along the
  witness path; the integral matches the claim within tolerance."
  Either it does or it does not. This is the **demonstrable
  value** property in operational form.

- **Pattern conflicts become geometric.** Two patterns with
  overlapping applicable regions whose vector fields disagree at
  a vertex produce an ambiguous direction. The diagnostic flags
  this rather than silently picking one. **Repeatability** in
  the IFR is exactly the well-definedness of the field.

- **Sorrys get a topological home.** The futon stack already
  treats sorrys as first-class. Geometrically, a sorry is a
  critical point of T where no local move can reduce tension —
  resolving it requires a non-local intervention (a structural
  change to the manifold itself, e.g. a new pattern or a
  refactor). This matches the existing intuition that a sorry
  is "stuck" in a way a normal task is not.

- **Tornhill's hotspots become free.** Change-frequency × code-
  complexity is one estimator of local curvature; the watcher's
  evidence stream gives this continuously. M-futon-enrichment's
  hotspot layer becomes "the Laplacian of T sampled at edit
  events."

- **Cross-prototype generalisation is mathematical, not
  rhetorical.** A different corpus (math, arxiv) gives a
  different manifold; the calculus is the same. "Prototype 1 of
  a series" stops being a hopeful gesture and becomes the claim
  "the same `(T, ∇, Δ)` machinery runs on a different graph."

### What this requires of the typed pattern shape

The pattern slots from §Scope (`{:context :tension :move
:witness-shape :domain}`) need geometric refinement:

- `:tension` — not a string description, but a *typed
  contribution to T*: which vertices/edges this pattern's
  presence increases tension at, and by how much (could be
  symbolic / parametrised).
- `:move` — not a prose recipe, but a *typed direction*: the
  outgoing edge type to follow, plus a precondition on the
  current vertex's neighbourhood.
- `:witness-shape` — not just "what does the witness look like,"
  but *what tension reduction does the witness demonstrate*:
  before/after T values along the path, with tolerance.
- `:domain` — names which manifold this pattern lives on
  (`:futon-stack`, `:math-corpus`, …) so the calculus knows which
  vertex/edge schema to use.

These are first-DERIVE-step refinements of the §Scope shape;
they don't change the IDENTIFY-level commitment but they
sharpen what "well-specified" means.

### Notes on prior art and feasibility

- Discrete exterior calculus (Hirani 2003, Crane et al.) is the
  canonical reference for `∇`, `Δ`, etc. on simplicial complexes
  and graphs. Computable in linear time per vertex.
- Forman-Ricci curvature on graphs (Forman 2003; Sreejith et al.
  2016) gives a hotspot detector that has already been used in
  network-of-papers analyses — directly relevant to prototype 2.
- Persistent homology (Edelsbrunner-Harer; gudhi, ripser) gives
  multi-scale tension structure: which tension features survive
  across scales, which are noise. Probably overkill for v0 but
  worth knowing exists.
- We do NOT need to build any of this from scratch. The
  computations are simple enough on the hypergraph sizes we
  have (≤10⁵ vertices for a long time) that a vanilla Clojure
  implementation suffices; we adopt the *vocabulary* and the
  *invariants*, not the heavy mathematical machinery.

### Sequencing

The geometric commitment is **not** a probe to schedule; it is a
**design constraint** on every probe in the mission:

- The pilot lift (≤20 patterns into the typed schema) must
  populate the geometric slots, not stop at prose `:tension`.
- The witness-check spike must compute T-before / T-after, not
  merely "did anything change."
- The watcher's edge-update output must support `T` updates
  alongside structural updates (the field is a derived view of
  the structure).

If a probe can't be done while honouring the geometric
commitment, that's evidence the probe is wrongly shaped, not
evidence the commitment is wrong.

## BHK substrate — why this generalises

Per `library/futon-theory/task-as-arrow.flexiarg`, a task in the
futon stack is a **BHK arrow**: a proof of `A → B` is a constructive
transformation that takes a proof of `A` as input and produces a
proof of `B` as output. Tasks are not work descriptions; they are
arrows whose validity is proved by the proof-path record.

A pattern is the *type* of such an arrow. `context → tension →
move` is exactly the BHK shape:

- **context** = the proposition `A` (the start state, evidenced by
  the codebase / corpus configuration)
- **tension** = the *direction* `→` (the structural pressure that
  this arrow exists to relieve)
- **move** = the constructive transformation
- **witness** = the proof-of-`B` (the resulting code change /
  proof step / theorem; without it the arrow is unrealised)

Every pattern application is therefore an instance of a typed
arrow with a constructive witness. The hypergraph of pattern
applications across a corpus is the **proof tree of the activity
in that corpus** — not a metaphor; a structural identity, per
task-as-arrow.

This is what makes the architecture domain-agnostic. BHK doesn't
care whether the witnesses are code edits or proof steps. Whatever
the corpus is, "pattern applies here, witness is X" is checkable
against the corpus state — and once checkable, the diagnostic IS
the proof object.

This is also why the IFR's **demonstrable value** property is the
hard one. Without a witness, a pattern application is a claim with
no proof — exactly the mode the futon3a MiniLM pipeline operated
in (a similarity score isn't a witness). Hypergraph-first lands
witnesses naturally because every typed edge points at the
specific codebase / corpus locations that bear it.

## System invariant — the live hypergraph projection

The IFR's **demonstrable value** and **replicable under automation**
properties cannot hold against a stale projection. So before
hypergraph-match retrieval becomes worth talking about, an
upstream invariant has to be in force:

> **The hypergraph projection of the codebase is always
> current with the on-disk state. The codebase is one living
> object; files are an authoring view, the hypergraph is the
> truth.**

This is not a property the diagnostic *uses*; it is a property the
diagnostic *requires*. Without it, every pattern-application record
risks pointing at a witness that no longer exists, and every
"is this pattern still applying?" answer is conditioned on stale
inputs. Lift it from "sounds like a good idea Joe" aspiration to
**system invariant** — the system enforces it, the same way it
enforces other structural laws.

### What the invariant binds together

This same machinery is the answer to a half-dozen "should be
automatic but isn't" properties Joe has flagged over time:

- **No imports.** "Importing" is "tell the system this file
  exists so retrieval can see it." If the projection is always
  current, importing is the empty operation. The pattern-import
  problem (F-6 in M-pattern-mining) was a special case; the
  general case is *every kind of file*.
- **Code → docs correspondence.** When an API changes, the docs
  that reference it know — typed cross-ref edges in the
  hypergraph mark referencing records stale, surfaces flag
  the drift instead of "best-effort search later."
- **Code → tests correspondence.** When implementation changes,
  the tests whose witnesses pointed at it become candidate
  re-runs.
- **Pattern → application currency.** When code under a pattern
  application changes, the application's witness is re-checked;
  if the witness no longer holds, the application is marked for
  diagnostic re-evaluation.
- **Doc → doc cross-ref currency.** When a mission doc renames
  its sections or a pattern is renamed, dependent docs flag the
  broken reference instead of silently rotting.
- **Mission → code currency.** When a module a mission claims
  ownership of moves or splits, the mission's claim is flagged.

These are not separate features; they are projections of one
invariant. The same watcher + typed-routing pipeline serves all of
them.

### Implementation discipline (sketch only — full design in DERIVE)

- **Watcher** subscribes to filesystem events on the configured
  roots; not just `library/`, the whole stack (with explicit
  exclusions for noise — `.git`, `node_modules`, build artifacts,
  large binaries).
- **Per-file-type routing.** `.flexiarg` → pattern record;
  `.clj` → AST + call-graph deltas; `.md` → doc record +
  cross-ref edges; `.edn` → typed-data record; `.py` / others
  by extension. Each route is a small, auditable function.
- **Debounce + content-hash.** 5 s after last write per file;
  hash to skip no-op re-ingestions; per-file incremental update
  to the hypergraph.
- **Cascade.** When a file's hash changes, its incoming
  hypergraph edges (the records that *depend* on it) get marked
  `:edge/witness-stale true`. Stale records don't disappear —
  they're flagged for re-check, with timestamp, by the next
  pass over their owning pattern application or doc record.
- **Pause / resume.** Heavy editing sessions can pause the
  pipeline; resume with a single replay over the affected hash
  set.
- **Self-instrumentation.** The pipeline emits typed evidence
  records (`event "hypergraph-update"`, `body {:file :hash
  :edges-touched :stale-cascade-count :latency-ms}`) so its own
  health is observable and can be a Stack HUD widget.

### What this means for M-futon-enrichment

`futon4/holes/missions/M-futon-enrichment.md` already plans
**enrichment layers** — `var→mission`, `var→pattern`,
`var→evidence`, `var→tension` — as timestamped batches of
hyperedges, explicitly subsuming Tornhill's `code-maat`
hotspot / temporal-coupling analysis. That plan was written
under the assumption that enrichment is a periodic discipline:
"each pass produces a layer." Reasonable when a pass is a
manual or scheduled reconstruction event.

Under the live-hypergraph invariant, the architectural
classification flips:

- **Static analysis becomes par for the course.** A `.clj`
  edit triggers AST + call-graph + reflection-envelope updates
  on the affected vars; that *is* a `var→mission` /
  `var→pattern` re-derivation. Enrichment "layers" are no
  longer batches; they are continuous deltas, with the same
  bitemporal queryability XTDB already gives us.
- **Tornhill's signals become free.** Hotspot detection
  (change-frequency × complexity) and temporal coupling
  (which files always change together) fall out of the
  watcher's own evidence stream — the `event "hypergraph-update"`
  records this mission proposes are exactly the signal
  `code-maat` extracts from VCS history, but live and typed.
- **Dynamic analysis becomes plausible at the same surface.**
  futon3c already emits invoke-event evidence, runtime traces,
  and reflection envelopes from the running JVM. If those
  records are typed-edges in the same hypergraph (alongside
  the file-watcher's static edges), then "what does this
  function actually call at runtime?" is the same query
  shape as "what does it import?" The static / dynamic
  distinction collapses into "edge provenance is `:static`
  or `:runtime`" — same hypergraph, different sources.
- **"Rational reconstruction" stops being a methodology
  question and becomes a query.** M-futon-enrichment's
  central rhetorical move (replay history as if we'd
  maintained discipline all along) becomes a bitemporal
  XTDB query: "show me the projection as of layer N." The
  *replay* is the projection itself, queried at past
  times, because the watcher has been writing it
  continuously.

**Net effect:** M-futon-enrichment's *plan* survives, its
*phase ordering* mostly survives, but its *cost model*
shifts dramatically. Enrichment is no longer a thing one
schedules; it is a thing one *receives* as a side effect of
authoring. M-futon-enrichment retains ownership of which
edge types matter and what each layer means; this mission
owns the substrate that makes those edges cheap.

### Sequencing within this mission

The invariant is a **precondition**, not the first deliverable.
Sequencing:

1. **Pilot lift** of ≤20 patterns into the typed schema *without*
   the watcher (manual ingestion). Establishes whether the
   schema works.
2. **Witness-check spike** on 1 pattern *without* the watcher
   (manual re-derive). Establishes whether the witness format
   is right.
3. **Then** the watcher: build it once we know what the
   hypergraph needs to receive. Building it earlier risks
   shipping an ingestion pipeline shaped wrong because we hadn't
   yet specified the receiver.
4. **Cascade rules** are last: only meaningful once enough
   records exist that staleness propagation has something to
   propagate through.

If the invariant feels too heavy to live inside this mission once
DERIVE starts, spawn `M-living-hypergraph` (or similar) as a
sibling and let this mission depend on it. The decision lives at
DERIVE-exit, not now.

## Path verdict — hypergraph-first

(Inherited from M-pattern-mining's path comparison; restated for
self-containment.) Hypergraph-first dominates against the IFR:

| Property              | Hypergraph-first                      |
|-----------------------|---------------------------------------|
| Coverage              | every turn writes a record            |
| Well-specified        | typed slots are the substrate         |
| Repeatable            | subgraph-match is deterministic       |
| Demonstrable value    | claim references concrete witnesses   |
| Replicable / explainable | provenance is constructive         |

Embedding-first fails on three of the five — exactly the
properties the activation-browser probe exposed as missing.

A **layered** architecture (hypergraph + embedding for novelty
candidate generation) is reserved as phase-2 if subgraph-match
recall proves too narrow once the typed-slot work is in.

## Scope

### In scope

- **Typed pattern shape.** `{:pattern/id, :pattern/title,
  :pattern/context, :pattern/tension, :pattern/move,
  :pattern/witness-shape, :pattern/domain}` minimum. The
  `:pattern/domain` slot is what makes prototype 1 fit alongside
  later prototypes without architectural change.
- **Hypergraph schema for pattern applications** in futon1a's
  hyperedge API. Edges are typed over abstract primitives
  (`entity`, `relation`, `evidence`, `role`) so the schema is
  domain-agnostic. A code-domain projection layers on top.
- **Diagnostic retrieval** — given a turn, identify which
  hypergraph subgraph it touches and which pattern hyperedges
  match that subgraph shape. Returns: `{pattern, witness-pointers,
  confidence, why}`.
- **Coverage discipline** — every turn produces an evidence
  record, including "no pattern matched." (Closes M-pattern-mining
  F-4 silent-fail at the new substrate.)
- **Witness check** — given a claimed pattern application,
  re-derive the witness against current codebase / corpus state
  and report whether the claim still holds. This is the
  **demonstrable value** property in operational form.
- **Pilot lift** of a small set of existing futon3/library
  patterns (≤20) into the typed schema as the first DERIVE
  exercise. Don't lift all 955; pick patterns that cover the slot
  variability and prove the schema holds.
- **Surfaces.** Stack HUD widget + Arxana Browser activation
  view (already shipped against the old substrate) re-pointed at
  the new hypergraph-backed retrieval once it lands. Same
  shape, different origin.

### Out of scope

- **Prototype 2 (math / arxiv).** Will become its own mission
  once prototype 1 demonstrates the schema holds. The design
  must *anticipate* prototype 2 — that's why we're insisting on
  domain-agnostic primitives — but we are not building it here.
- **Lifting all 955 existing patterns** into the typed schema.
  The pilot lift establishes whether the schema is right; bulk
  re-authoring is post-VERIFY work.
- **Replacing the embedding entirely.** The retired MiniLM substrate
  still has a possible future as a candidate-generator in a layered
  architecture; that decision lives in phase-2 if recall demands it.
- **Authoring new patterns.** This mission builds the diagnostic;
  pattern *authoring* (deciding which patterns the futon stack
  needs) lives elsewhere.

## Open questions (DERIVE-feeders)

1. **What are the abstract primitives of the typed hypergraph?**
   Candidate set: `entity`, `relation`, `evidence`, `role`,
   `tension`, `witness`. Minimal sufficient set TBD; this is the
   first DERIVE step because the rest of the design hangs off it.

2. **What's the smallest pattern that exercises the schema fully?**
   Target a pattern from `library/futon-theory/` that already has
   prose for context / tension / move (e.g. `task-as-arrow`
   itself), lift it, and check the schema doesn't lose anything.
   If it does, the schema is incomplete.

3. **How does subgraph-match handle approximate hits?** Exact
   subgraph-match is deterministic but may have low recall. Two
   knobs: (a) accept partial subgraph matches with a deficit
   measure, (b) layer in a continuous index (path-2). Choose
   after the pilot lift exposes the recall problem concretely.

4. **What does "the codebase as a hypergraph" actually contain?**
   The hypergraph already partly exists (futon1a hyperedges,
   stack stereolithography, B→A typed graph). Need to enumerate
   what's there and what's missing before pretending we're
   starting from zero. The lineage section names the four
   partial projections; this question maps which slots each one
   already populates.

5. **What's the witness format for the futon-stack domain?** Code
   edit ranges + commit hashes? Evidence record IDs + a check
   procedure? This needs to be concrete enough that an agent can
   run the witness check unsupervised, per the IFR's automation
   property.

6. **How does this interact with codex-5's B→A reweighting?**
   The B→A learner currently plans to derive A-side family
   mappings from MiniLM context-retrieval evidence. Under this
   mission's substrate, A-side typed records become the natural
   input. Coordinate with codex-5 so we don't ship two
   incompatible producer schemas.

## MAP pointers (TBD)

When promoted to MAP:
- Lift `task-as-arrow.flexiarg` and 4 sibling patterns into the
  typed schema; record what fits, what's awkward, what's missing.
- Map the four partial projections (stereolithography, B→A typed
  graph, Arxana hypergraph, futon3a embedding) onto the proposed
  schema; identify which slots each populates.
- Specify the witness-check procedure for the code domain; spike
  it on one pattern.
- Spec the JSON / HTTP shape for the diagnostic retrieval endpoint
  so the existing surfaces (Stack HUD, Arxana Browser activation
  view) can re-point with a config change rather than a rewrite.

## Notes

- The activation-browser verdict on 2026-04-27 was the catalyst:
  reading actual MiniLM activations made it concrete that the
  pipeline returns *artifact* and not signal. The IFR was already
  implicit in the futon-theory library; what was missing was the
  recognition that the substrate had to match it. This mission is
  the recognition becoming a build plan.
- The phrasing "a curlicue on top of the A→B turn, when in fact we
  wanted a diagnostic" (Joe, 2026-04-27) is worth holding as the
  central operational test for any candidate substrate: if the
  diagnostic doesn't tell you something you couldn't have
  confidently said yourself, the substrate is curlicue.
