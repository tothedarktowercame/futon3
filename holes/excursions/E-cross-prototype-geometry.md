# Excursion: Cross-Prototype Geometry — same calculus, two manifolds

**Status:** ACTIVE — v0 first-pass run on futon2 (2026-04-27)
**Owner mission:** `holes/missions/M-pattern-application-diagnostic.md`
**Parent excursion:** `holes/excursions/E-math-prototype-pilot.md`
  (this excursion is open-question item 4 from there).
**Implementation:** `futon3/scripts/v0_codebase_hypergraph.clj` (babashka).

## Goal

Test the prototype-1-as-instance-of-a-series claim
empirically: define the codebase analogue of T (the tension
scalar) on a small futon-stack region, compute it, and see
whether high-T sits where the operator already feels active
tension. If yes, the geometric commitment is validated cross-
prototype; if no, we learn what the codebase domain demands
that the math domain doesn't.

## Mapping

| Math-side definition (validated, E-math-prototype-pilot.md) | Codebase analogue (this excursion) |
|--------------------------------------------------------------|------------------------------------|
| Vertex: `claim`                                              | Vertex: `function-spec` (docstring claim, type signature, mission-claim, invariant claim) |
| Vertex: `proof`                                              | Vertex: `test`, `runtime-evidence`, `invariant-check-result` |
| Edge: `derivation` (claim → proof_of_target)                 | Edge: `coverage` (function-spec → test_proving_it) |
| `T(claim) = 1 if no incident derivation, else 0`             | `T(function-spec) = 1 if no incident coverage, else 0` |
| `ΔT` ≈ Tornhill hotspot (load-bearing concept)               | `ΔT` ≈ Tornhill hotspot (load-bearing function) |

## Tasks

1. Pick a futon-stack region (e.g. `futon3c/src/futon3c/aif/`
   — small, recently-active, has both code and tests).
2. Build a v0 hypergraph projection: parse `.clj` files into
   `(ns, var, defn-spec)` vertices; parse test files into
   `(test, asserted-spec)` vertices; emit `coverage` edges
   where a test asserts a defn's behaviour (heuristic: test
   name string-matches defn name).
3. Compute `T(spec) = 1 if no incident coverage else 0`.
4. Compute `(ΔT)(v) = sum of (∇T) over incident edges` per
   vertex. Read the top-10 vertices by `|ΔT|`. Are these the
   functions / specs that intuitively feel like the "active
   tension" of the region?
5. Reverse check: ask Joe (or the operator working in that
   region) to point at the 5 functions that feel most "owed
   work" — where TODOs live mentally even if uncoded. Does the
   computed top-K `|ΔT|` overlap?

## Pitfalls to look for

- **Naive coverage detection (string match) misses indirect
  coverage.** If `function-foo` is tested only via `function-
  bar` calling it, the v0 won't see that. Refinement: walk
  call-graph for transitive coverage. v0 deliberately uses the
  cheaper definition to see how much it gets right anyway.
- **Codebase has no analogue of "claim".** Function defns
  rarely have explicit specs. Use docstring presence as a
  proxy; functions with `nil` docstring contribute T=1
  trivially. This may dominate the signal — flag if so.
- **Mission docs introduce another vertex class.** Mission-
  claims should also be in the hypergraph; their coverage is
  by code that demonstrably implements them. Probably want
  `mission-claim → impl` edges by hand-curation for v0.

## Expected payoff

If high-T regions of the codebase hypergraph correspond to the
operator's intuitive tension map, we have:
- (a) Empirical validation of the geometric commitment cross-
  prototype.
- (b) The base case for an automated "where's the active
  tension" surface in the Stack HUD.
- (c) Strong evidence that the same `(T, ∇, Δ)` machinery runs
  on different graphs — the prototype-N generalisation claim.

If low-T regions feel intensely active to the operator, or
high-T regions feel inert, we learn what's missing from the
codebase hypergraph (probably: the "specs" the codebase
actually has aren't well-encoded as `function-spec` vertices;
specs live in mission docs, comments, conversation history, or
nowhere explicit).

## v0 run — futon2 baseline + War Machine import (2026-04-27)

Joe chose futon2 as the substrate (small, clojure-native, has both
`src/` and `test/`). The probe was structured as a two-point diff:

- **Baseline:** futon2 at HEAD `ce7a816` (pre-WM import).
- **Treatment:** futon2 at HEAD `489e75f` after importing the
  untracked `futon0/web/war-machine/` working tree as a single
  commit under `web/war-machine/`. WM had no recorded git history
  to replay; single-commit import was the honest path.

### Schema realised

The script extracts:

- `:var` vertices from `defn / def / defmulti / defmethod /
  defprotocol / defrecord / deftype` forms (with `:var/has-doc`
  flag).
- `:test` vertices from `deftest` forms.
- `:coverage` edges (test→var) by scanning each `deftest` body for
  symbol references that resolve to known vars (via same-ns,
  `:require :as` aliases, or fully-qualified namespace).
- `:calls` edges (var→var) by the same body-symbol resolution.

Tension definition (transferred verbatim from the math side):

- `T(var)  = 1` if no incident `:coverage` edge else `0`.
- `T(test) = 0`.
- `∇T(e)   = T(target) − T(source)` per edge.
- `ΔT(v)   = Σ_{e: target=v} ∇T(e)  −  Σ_{e: source=v} ∇T(e)`
  (net inflow of gradient at `v`).

### Findings (futon2 baseline)

| | Value |
|---|---:|
| Files / namespaces / vars / tests | 46 / 46 / 384 / 46 |
| Coverage edges | 50 |
| Call edges | 511 |
| Paired vars (T=0) | 25 |
| Unpaired vars (T=1) | 359 |
| Coverage by var-count | 6.5 % |
| Vars with docstring | 113 / 384 |
| Vertices with nonzero ΔT | **114** |

**First v0 attempt** used a name-only matching heuristic (test name
≈ var name, after `-test` strip). It produced 3 paired vars / 0
nonzero ΔT — useless, because the math-side observation that
"derivation edges have zero gradient" applies to coverage edges
too: both endpoints are at T=0 by construction, so ΔT vanishes
along coverage alone. **A second edge type is required for
gradient flow**, exactly as the math side found with
`structural-cooccurrence`. `:calls` plays that role on the
codebase manifold.

**Top −ΔT (load-bearing utilities, math analogue: high
negative-Laplacian "concepts adjacent to many open claims"):**

| Var | ΔT |
|---|---:|
| `ants.aif.policy/choose-action` | −21 |
| `ants.aif.observe/clamp01` | −16 |
| `ants.aif.observe/g-observe` | −11 |
| `ants.war/simulate` | −11 |
| `ants.ui/scoreboard` | −11 |
| `ants.war/new-world` | −10 |
| `ants.aif.perceive/perceive` | −10 |
| `ants.war/step` | −8 |

**Top +ΔT (orchestrators / "punchlines": uncovered vars that
integrate covered library code):**

| Var | ΔT |
|---|---:|
| `ants.aif.core/aif-step` | +6 |
| `ants.visual/visualize` | +3 |
| `ants.war/default-aif`, `tick-hunger`, `expected-free-energy`, `ants.main/-main`, `step-ant`, `update-sensory`, `run`, `default-config`, `attach-config*`, `merge-deep`, `clamp`, `act-forage`, `modulate-precisions` | +2 each |

The top-K list is operator-readable: `choose-action` and
`clamp01` are exactly the kind of vars where uncovered callers
pile up. `aif-step` is the canonical "this orchestrator pulls
covered library together but isn't itself tested."

**Reverse check pending Joe.** The geometric commitment passes
v0 if these names overlap with operator-felt tension; it
informs schema refinement if they don't.

### Findings (post-WM import)

The WM import was structured as a single commit because
`futon0/web/war-machine/` had no recorded git history (`git log`
empty for that path; no nested `.git`). 12 .clj/.cljs source
files, 11 .spec.ts playwright tests, no Clojure unit tests.

| | Pre-WM | Post-WM | Δ |
|---|---:|---:|---:|
| Vars | 384 | 565 | +181 |
| Coverage edges | 50 | 50 | **0** |
| Call edges | 511 | 766 | +255 |
| Paired vars | 25 | 25 | **0** |
| Nonzero-ΔT vertices | 114 | 114 | **0** |

**Top-K is identical pre/post.** Verified by inspection: no WM
namespace `:require`s anything from `ants.*` or `futon2.*`; all
WM tests are TypeScript playwright specs, invisible to
`deftest` extraction. Result: WM is a 181-vertex disconnected
manifold attached to the ants/AIF core only by shared root
directory, and uniformly at T=1 with ΔT=0 across the whole
lobe. The field is *flat* on WM.

**This is itself a structural finding the geometry surfaced
cleanly.** The math-side analogue is a paper-cluster where
every claim is unpaired and no `derivation` or
`structural-cooccurrence` edges link the claims to each other
or to concepts — a region containing *only tension*, nothing
to relieve it. WM, geometrically, is exactly that: a tension
reservoir disconnected from any witness machinery.

The diff also shows what the codebase *needs* for the geometry
to engage:

1. **At least one cross-boundary edge.** Any WM namespace
   `:require`-ing an ants/AIF namespace introduces a
   `:calls` edge with non-zero ∇T (T=1 source on WM, T=0
   target on a covered ants utility). ΔT lights up at both
   endpoints.
2. **At least one Clojure-side test on WM.** Even a single
   `deftest` exercising one WM var creates a coverage edge,
   pulls one WM var to T=0, and creates non-zero gradient
   through the WM-internal calls graph.
3. **A TypeScript-test-aware coverage extractor.** Currently
   `.spec.ts` files are detected but unparsed. A v0.5 could
   regex-extract the WM var qnames referenced from inside
   each spec (e.g. `state/cells`, `tick/decay`) and emit
   coverage edges from a synthetic `:test/playwright/...`
   vertex. Cheap upgrade; large potential signal.

### Pitfalls confirmed in v0

- **Coverage-only edges have ∇T = 0 by construction.** Worth
  promoting to a §"Geometric commitment" caveat in the parent
  mission: any single edge type whose endpoints share a T-value
  by construction will produce no gradient signal. The
  diagnostic needs at least *two* edge classes whose endpoint-T
  distributions can disagree.
- **String-matching coverage misses the actual coverage.**
  futon2's deftest names are behavior phrases
  (`preference-risk-favors-return-when-hungry`), not var names.
  Body-symbol scanning is the right v0 — caught 50 edges where
  name-matching caught 3.
- **Disconnected components are real and the geometry says so.**
  The WM import created a non-trivial second connected component
  in the call-graph. ΔT being identically 0 on a component is
  not a bug; it is the diagnostic surfacing that the component
  is not yet integrated with any covered-witness substrate.

### v0.5 — vocabulary edges + drift signal (2026-04-27)

The v0 surfaced a sharp failure mode: WM and ants/AIF are
fully structurally disconnected (zero `:calls` / `:coverage`
edges across the boundary), even though *thematically* they
were supposed to share the AIF concept space. The v0.5
extension makes the would-be-thematic edges first-class.

#### Design

A third edge class:

- `:term` — vertex parsed from a vocabulary markdown doc.
  Sources tried: section headings (`## Foo`), bolded
  phrases (`**bar**`), italic phrases (`*baz*`).
- `:vocabulary-use` — namespace → term, where the namespace's
  source file contains the term (case-folded whole-token
  match).

Term-set size: **140 terms** retained from the union of
`futon5/docs/{core,cyberants,metaca-v2,ct-wiring,nonstarter}-
terminal-vocabulary.md` after filtering with a stopword list
+ length / token-count caps. (Pre-filter: 379 candidate terms
across the 5 docs; post-filter ratio ≈37 %.)

Drift between two connected components is the Jaccard overlap
of their term-fingerprints:
`drift(C₁,C₂) = |V(C₁) ∩ V(C₂)| / |V(C₁) ∪ V(C₂)|`.

This re-frames theory as a structural object: prose terminology
becomes typed edges; "thematic coherence" becomes an
overlap measure; "drift" becomes the temporal derivative of
that overlap (next subsection).

#### Synchronic results — futon2 post-WM at HEAD `489e75f`

34 connected components in the call/coverage subgraph. Three
non-trivial:

| Component | Size | NS count | Sample namespaces | Top terms |
|---|---:|---:|---|---|
| 0 (core) | 368 | 31 | `ants.*`, `ants.aif.*`, `ants.war`, `ants.cyber` | actions, beliefs, energy, **free energy**, hexagram, **integration**, **pattern constraints**, policies |
| 2 (WM client) | 169 | 10 | `war-machine.client.*` | adapt, both, current, energy, **mana**, selects, sequential |
| 3 (adapters) | 18 | 4 | `futon2.aif.adapter`, `futon2.aif.adapters.{ants,fulab,futon5-mca}` | beliefs, current, integration |

**The drift hotspot finding:**

| Pair | Structural edges | Vocabulary overlap (on samples) |
|---|---:|---|
| Core ↔ WM | 0 | `{energy, adapt}` (+ `current` noise) |
| Core ↔ Adapters | 0 | `{beliefs, integration}` (+ `current`) |
| WM ↔ Adapters | 0 | `{current}` only |

**This is the failure mode the diagnostic was built to surface.**
The adapters component is named "adapter" yet has *zero* call
edges to the namespaces it claims to adapt. The WM client
shares `energy` / `adapt` with core (genuine AIF concepts) yet
has zero call edges. The geometry has now made visible what
was previously hidden in prose ("WM was supposed to be
developed with reference to `core-terminal-vocabulary.md`").

Caveat: the term filter still leaks generic English words
(`current`, `actions`, `policies`) — these dilute the drift
signal. A v0.6 should either (a) hand-curate the term-set
from the vocabulary docs, or (b) raise the cohesion bar by
requiring multi-word matches.

#### Diachronic results — 13-commit series across futon2 history

Joe's framing: synchronic drift is metaphorical; only when
the codebase is treated as a discrete manifold *evolving over
time* does drift become observable. Implemented via
`futon3/scripts/diachronic_v0.clj`: walks every Nth commit,
checks each into a `git worktree` (non-destructive), runs the
v0.5 projection, captures per-rev counts and component
summary.

Time-series (every 3rd commit, 13 points spanning the full
futon2 history):

```
idx  sha       vars  tests  cov  call  vocab  comp  paired  cov%
 0   0577ef1     0     0    0     0      0     0     0     0.0    (root)
 1   84d7fad   270    39   38   374     28    19    20     7.4
 2   4ff9b01   320    46   50   426     36    25    25     7.8
 3   e410cb1   336    46   50   437     45    32    25     7.4
 4   cacbdf5   344    46   50   456     44    27    25     7.3
 5   55acac6   349    46   50   465     44    26    25     7.2
 6   48911fa   361    46   50   472     46    32    25     6.9
 7   ad066b3   382    46   50   509     48    26    25     6.5
 8   2f46259   382    46   50   509     48    26    25     6.5
 9   7388684   384    46   50   511     50    26    25     6.5
10   d5e4b7f   384    46   50   511     50    26    25     6.5
11   5aae352   384    46   50   511     50    26    25     6.5
12   489e75f   565    46   50   766     63    34    25     4.4    (WM import)
```

Three observable diachronic phenomena:

1. **Coverage discipline is in steady retreat.** Paired-vars
   plateaued at 25 from idx 2 onward; vars grew from 320 to
   565 (a 76 % increase). Coverage % declined monotonically
   from 7.8 → 4.4. **The codebase is ageing the wrong way on
   the geometry — T-mass is accumulating, not relieving.**
   This is the synchronic "load-bearing utility" finding
   (`choose-action`, `clamp01`) extended in time: those
   utilities are picking up new uncovered callers faster than
   they pick up new tests.

2. **The WM import shows up as a step-function discontinuity**
   at idx 12: vars +47 %, call-edges +50 %, components +8,
   vocab-edges +26 %. The smooth growth curve breaks visibly.
   The geometry quantifies what would otherwise be a "feels
   like a big change" intuition. **The discontinuity is itself
   the diagnostic signal of a drift event.** A daemon reading
   this signal could flag any commit whose ratio
   (Δvocab / Δstructure) deviates from the baseline as
   "thematic-without-structural" or "structural-without-thematic"
   imports.

3. **Component count grows roughly linearly with codebase
   size** but is *not monotonic*. Drops from idx 3 (32 comps)
   to idx 4-5 (27, 26): some structural change merged
   components. Idx 7 → 6 → 7 oscillates. Mergers and splits
   are visible. Each oscillation deserves a label-readable
   reason — that's a v0.6 job (per-commit *which* components
   appeared / disappeared).

#### Time as duality, not asymmetry

Initial framing in this excursion claimed code has time and
math doesn't. Joe's correction (2026-04-27): math papers
*do* carry time — every paper has a publication date, and
`citation-grounding` edges are time-asymmetric (papers cite
predecessors, almost never successors). The asymmetry is
not in *whether* time exists but in *how* it is encoded:

| | Code manifold | Math manifold |
|---|---|---|
| Time-bearing entity | Same artifact at multiple revs | Distinct papers on a timeline |
| Temporal edges | Implicit commit-graph (currently *outside* the hypergraph) | Explicit `citation-grounding` edges *inside* the hypergraph, naturally time-asymmetric |
| Drift quantity | Single-artifact-over-time divergence | Cross-paper pattern-frequency shift over publication years |
| Time as a hypergraph object | Not yet | **Already promoted** |

**Math is structurally ahead on time.** Every
`citation-grounding` edge in the existing math hypergraph is
already a typed temporal edge. Codebase parity requires
promoting commits to first-class hypergraph vertices —
`:commit` nodes with `:edits` (commit → var-version),
`:authored` (author → commit), `:precedes`
(commit_t → commit_{t+1}) — instead of treating revs as
loop-iteration variables in a driver script. Then `T(v, t)`
becomes `T(v, c)` for commit-vertex `c`, and `∂T/∂t` is a
discrete derivative along `:precedes`.

Joe's framing of the commit DAG as **a "spanning tree" for
the topology** sharpens this further. The full possibility
space `P` (every conceivable codebase configuration) is
unbounded; the realised history `H` is one chosen walk on
`P` — a spanning structure that covers part of the temporal
manifold but does not exhaust the alternatives. Crucially,
**multiple spanning structures can be defined over the same
realised history**:

- The commit DAG itself (`:precedes` edges).
- The vocabulary-co-evolution graph (which terms appear
  together at each revision; an edge whenever two terms
  first co-occur).
- The component-merge graph (when did two previously
  disjoint components first share a call edge).
- The coverage-frontier graph (which vars first crossed
  T=1→T=0 in which commit).

Each is a different *projection* of the time axis. The
diagnostic should support many; the signal varies non-
trivially between them, and discrepancies are themselves
informative.

#### The futonic-zapper specification — geometric signatures of satisficing

Joe's third pickup (2026-04-27) names the central pathology
this whole substrate addresses: **short-term satisficing
versus long-term structural validity** is the core agentic-
coding bug, and prose-form futon theory does not always
prevent it because operators (and agents) can ignore prose.
The geometric closure of futon theory makes the failure modes
*visible*, not just *describable* — and once visible, they
become queryable. The "big futonic zapper" is an automated
diagnostic that lights up against geometric signatures of
the named failure modes:

| Failure mode | Geometric signature | Already detectable in v0.5? |
|---|---|---|
| **Quick fix** — local T-reduction that pushes mass elsewhere | `ΔT(v)` drops at `v` in commit `c`; `ΔT(neighbours)` rises by ≥ same magnitude. Net T-mass conserved on the local neighbourhood. | Yes, with v1 (commits-as-vertices). v0.5 has all the inputs. |
| **Work-around** — new code routes around a problem instead of fixing it | New component appears with high vocabulary overlap with existing component but zero structural edges across. | **Yes, today.** This is the WM-import finding generalised. |
| **Adapter shim that doesn't adapt** | Component whose namespaces are named `adapter`/`bridge`/`shim`/`port` but has zero call edges to the components it claims to bridge. | **Yes, today.** This is the futon2 adapters-component finding. |
| **Coverage retreat** — uncovered code accumulating faster than tests | Coverage % monotonically decreasing across commits; var-count growing without paired-var growth. | **Yes, today.** Diachronic series shows futon2 in this regime: 7.8 % → 4.4 %. |
| **Load-bearing utility under-tested** | High `|ΔT|` negative on a few utility vars; their incoming call-edge count grows commit-over-commit without coverage-edge growth. | Yes, with v1. |
| **Concept introduced without structural attachment** | New term appears in vocab-doc commit; no `:vocabulary-use` edges from any code component to that term within N subsequent commits. | Requires extending the diachronic driver to also walk vocab-doc commits. Cheap. |
| **Concept use without definition** | `:vocabulary-use` edge from code component to a term not present in any vocab doc. | **Yes, today.** Inverse of the above. |
| **Completion rot** — mission marked COMPLETE but its claimed deliverables are no longer present in the substrate they were supposed to land in | `mission/status = COMPLETE` + count of edges in the mission's claimed `:deliverable` set falls below threshold (or = 0) at current commit. | **Yes, today** — discovered live during substrate-2 phase-1 pre-flight (2026-04-27): M-three-column-stack closure (2026-03-04) claimed 1,524+ hyperedges; current futon1a store has 0 of all 6 claimed edge types. The mission was satisficed by interpreting "persistence" as "re-runnable" rather than "queryable when downstream consumers need it." |

These are the v1 outputs of the diagnostic, named and
specified. The parent mission's §"Diagnostic retrieval" can
adopt them directly: alongside `{pattern, witness-pointers,
confidence, why}`, the diagnostic emits a list of
`{failure-mode, evidence-vertices, evidence-commit-range,
severity}` records. The "futonic zapper" is the surface that
consumes this list and prompts the operator (or agent) to
address each.

This is also why prose-form theory is insufficient:
operators can ignore prose. The geometric output is
unignorable — either the signature is present or it isn't,
and the substrate computes the answer.

#### Theoretical implication — theory IS the weak-link layer

Joe's framing during this excursion: "the role of theory is
exactly to provide such weak links, but for now theory has
been hidden in things like the core-terminal-vocabulary.md
and the negative validates our proposal to make everything
more coherently geometric." The v0.5 + diachronic results
make this concrete:

- The `:vocabulary-use` edge class is the *promotion of theory
  from prose to geometry*. Without it, the diagnostic has
  no way to even ask "is this region coherent with that
  one?"
- Drift requires time. The synchronic Jaccard overlap is a
  static quantity; whether two components are *coherent*,
  *drifting*, or *appropriately decomposed* is a
  diachronic question. A high-overlap pair that has been
  stable for 30 commits is coherent; a high-overlap pair
  whose overlap is decaying is drifting; a pair that has
  always had low overlap is appropriately decomposed.
- Simon's near-decomposability says weak inter-block ties
  *exist* in good systems — they are not accidental.
  Hiding them in prose docs makes them invisible to the
  diagnostic and hence invisible to discipline. Promoting
  them to typed edges is the prerequisite for noticing
  when they fail.

This is also the relationship to the parent mission's
**system invariant** (live hypergraph projection):
`:vocabulary-use` edges only stay current if the hypergraph
is rebuilt as docs and code co-evolve. A static snapshot
gives one drift reading; a watcher gives drift as a live
signal.

### Transfer back to the parent mission

These v0 + v0.5 + diachronic findings sharpen items in
`M-pattern-application-diagnostic.md`:

1. §"Geometric commitment" §"What this requires of the typed
   pattern shape" — `:tension` slot's "typed contribution to T"
   needs at least one *other* edge type to make ∇T informative.
   The tension-shape spec should include an edge class hint, not
   just a vertex T-contribution.
2. §"Sequencing within this mission" item 1 (pilot lift) can
   now lift each pattern with two slots populated: the
   coverage-style `:witness-shape` *and* a `:related-vertices`
   class for cooccurrence-style edges (the analogue of
   structural-cooccurrence on the codebase manifold).
3. **Cross-prototype validation: passes provisionally.** Same
   `(T, ∇, Δ)` machinery, same naive coverage-style T
   definition, runs on a second manifold and produces a
   readable top-K. The "prototype-N generalisation" claim has
   one further empirical confirmation. Pending Joe's reverse
   check on the operator-tension overlap to upgrade
   "provisionally" to "validated."

4. **Time is constitutive of *both* manifolds, dually.**
   Math has multiple-papers-on-a-timeline encoded as
   `citation-grounding` edges (already promoted to first-
   class). Code has same-artifact-over-time encoded as the
   commit DAG (currently *outside* the hypergraph). **v1
   should promote the commit DAG to hypergraph vertices
   and edges**, not loop-iteration variables. `T(v, t)` is
   then a per-commit-vertex query. The commit DAG is one
   spanning structure of the temporal manifold; vocabulary-
   co-evolution, component-merge, and coverage-frontier
   graphs are alternative spanning structures over the same
   realised history. The diagnostic should support several
   and surface discrepancies between them.

5. **The vocabulary docs are not yet typed structures.** The
   `:term` extraction in v0.5 is a screen-scraper over
   markdown headings + bold + italic. A `library/vocabulary/`
   directory of `.flexiarg`-style typed term records would
   make terms first-class hypergraph vertices with their own
   `:context` / `:tension` / `:move` fields, eliminating the
   filter-noise problem and giving each term a versionable
   home. Sequencing: the diagnostic's pilot lift (§Sequencing
   item 1) should include lifting at least one vocabulary
   doc — `core-terminal-vocabulary.md` is the obvious
   first — into typed records.

6. **Drift hotspots are a queryable diagnostic.** "Component
   pairs with `overlap > θ` and zero call-graph edges between
   them, sorted by overlap descending" is the named output for
   the failure mode that Joe surfaced: *thematically linked
   but structurally disconnected work*. This belongs in the
   parent mission's §Scope §"Diagnostic retrieval" output
   shape: alongside `{pattern, witness-pointers, confidence,
   why}`, the diagnostic should also emit
   `{drift-pairs [{c1 c2 overlap shared-terms}]}`.

7. **The futonic-zapper specification.** v1 of the diagnostic
   should emit a list of `{failure-mode, evidence-vertices,
   evidence-commit-range, severity}` records covering the
   seven satisficing signatures listed in the §"futonic-
   zapper specification" subsection above. Three of these are
   already detectable in v0.5 today; the rest unlock with the
   commits-as-vertices upgrade. This is the central value-
   add of the geometric closure: it converts prose-form theory
   ("don't satisfice short-term") into substrate-enforced
   discipline.

## References

- Pilot validating the math side:
  `holes/excursions/E-math-prototype-pilot.md` §"Tension
  scalar demo on paper #5".
- §"Geometric commitment" of the parent mission for the
  calculus and the why.
- §"What this means for M-futon-enrichment" of the parent
  mission — Tornhill's hotspot is the same `ΔT`.
- futon4 `M-futon-enrichment.md` already proposes
  `var → mission`, `var → evidence`, `var → tension`
  enrichment layers; this excursion's hypergraph projection
  is a v0 implementation of those edges restricted to a
  single subdirectory.
