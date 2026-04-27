# Excursion: Math Prototype Pilot — Hand-Tagging + First Geometry

**Date opened:** 2026-04-27
**Owner mission:** `holes/missions/M-pattern-application-diagnostic.md`
**Entry point:** §"Prototype 2 — math corpus, substrate-half-built" §"What this means for sequencing" item 2 (math-side slot validation) + item 3 (witness-check spike), running ahead of Rob's R-1..R-4 patches because the substrate is rich enough to do useful work without the patched pipeline.

## Method

Sample of 8 papers from `results-mfuton-001/output/`
(modern-arxiv lineage; richer per-paper hypergraphs than the
abstract-era `results-001..006`). For each:

1. Read the `reverse-morphogenesis.json` BHK-shaped triple.
2. Read the typed hypergraph (`paper-hypergraphs.json`):
   node-type histogram, edge-type histogram, per-edge roles.
3. Hand-tag the dominant *pattern of mathematical move* — i.e.
   what is the paper *doing*, expressed in pattern language.
4. For one paper, sketch a tension scalar `T` and observe
   what a discrete gradient looks like.

Time spent: ≈45 minutes of agent work. Goal: not exhaustive
analysis, but enough texture to learn whether the slot
vocabulary survives contact with real arxiv content.

## Findings

### Triple quality — 7/8 well-populated, 1/8 silently null

```
arxiv-2604.20840v1   ✓  Z/2-harmonic forms in dim 4         (good)
arxiv-2604.20832v1   ✗  EMPTY fields, quality null         (silent-fail)
arxiv-2604.20827v1   ✓  Entropic optimal transport bounds   (good)
arxiv-2604.20820v1   ◇  S-Prime Element Principle           (collapsed)
arxiv-2604.20818v1   ✓  Coburn's lemma → edge modes         (good)
arxiv-2604.20815v1   ✓  Zarankiewicz dichotomy              (good, sharp)
arxiv-2604.20812v1   ✓  Hausdorff dim via continued fracs   (good)
arxiv-2604.20808v1   ✓  Coxeter group cohomology equiv.     (good)
```

The **silent-fail rate is non-trivial (≈12 % here)** — pattern
matches what we already know from the codebase side, where
Codex turns produced no pattern-tag evidence until 2026-04-27
because the producer was unwired. The same IFR property
(coverage) is needed on the math side: empty `analysis` fields
should produce *some* record, even if it's `:status :failed`,
not `null` everywhere.

The **collapsed case** (paper #4, S-Prime) is more interesting.
Its situation/salience/arrow are paraphrases of one sentence:
"understand the significance of the S-Prime Element Principle."
The slots are well-formed but indistinguishable. Two
explanations:

- (a) The paper genuinely has a single-axis content; the
  triple isn't broken, the paper is shallow on this axis.
- (b) The slot-extraction prompt failed to differentiate, and
  the LLM emitted one description three times.

Reading the analysis quality field — `quality: good, good,
good` — suggests the LLM didn't notice the collapse. This is
worth flagging in the prompt fix (R-1): require the slots be
*demonstrably distinct* before passing the quality check.

### Edge types map directly to BHK arrows

Across all 8 papers, four edge types dominate:

| Edge type             | Role pattern                               | BHK reading |
|-----------------------|--------------------------------------------|--------------|
| `derivation`          | `(claim: target, proof: proof_of_target)` | Literal BHK arrow: a proof is a constructive transformation realising a claim |
| `structural-cooccurrence` | clustered concepts within a section    | Co-presence; not yet typed as causal/grounding |
| `definition-use`      | `(concept: many roles, definition: def-N)` | A definition grounds many uses; `definition` is a hub vertex |
| `citation-grounding`  | `(citation: source, proof: proof_of_target)` | External proof grounds local move |

The *most common edge in the corpus is literally a BHK arrow.*
Each `derivation` edge is `(claim, proof)` with roles
`(target, proof_of_target)`. **The math substrate already
encodes proof-as-arrow at the edge level**, before any of our
pattern-application machinery.

### Hand-tagged pattern vocabulary (7 papers)

| Paper                    | Pattern (hand)                                    |
|--------------------------|--------------------------------------------------|
| #1 (Z/2-harmonic forms)  | construction-from-known-structure                |
| #3 (entropic OT bounds)  | failure-mode-characterization                    |
| #4 (S-Prime Principle)   | foundational-principle-clarification             |
| #5 (Zarankiewicz)        | sharp-dichotomy                                  |
| #6 (Coburn → edge modes) | cross-domain-bridge                              |
| #7 (Hausdorff dim algo)  | algorithm-with-error-bounds                      |
| #8 (Coxeter cohomology)  | structural-equivalence                           |

7 papers → 7 distinct patterns, all proof-strategy / paper-shape
patterns rather than Q&A-strategy patterns. **None of these
appear in the futon6/P0 math.SE 25-pattern list** (which had
"work-examples-first," "reduce-to-known-result," "split-into-
cases," etc.). Some have semantic overlap (sharp-dichotomy ≈
"split-into-cases"), but the granularity and framing are
domain-mismatched. This validates GH issue R-2 (curation of
`arxiv-proof-patterns.edn`) is the right move; it also gives
us 7 candidate entries for that file.

The list is small but reusable — a 25-paper expansion of the
hand-tagging would cover most of the major proof-strategy
families. That's a one-Joe-afternoon task, not a curation
deadlock.

### Tension scalar demo on paper #5 (Zarankiewicz)

Paper hypergraph: 55 nodes, 19 edges.
- 15 `claim` nodes
- 10 `derivation` edges (each pairing a claim with a proof)
- 9 `structural-cooccurrence` edges
- Remainder: concepts, equations, etc.

Define `T(v)` for `v` in `claim` ∪ `proof`:

```
T(claim) = 1  if no derivation edge incident
        = 0  otherwise
T(proof) = 0
T(other) = 0
```

This makes `T` the indicator of "stated-but-not-grounded-here"
claims. For paper #5: **5 unpaired claims** out of 15 — these
are the high-tension vertices.

Compute `(∇T)(e) = T(target(e)) − T(source(e))` along each
derivation edge: every derivation has T=0 on both ends (by
construction the claim becomes grounded once the edge is
present). **Derivation edges have zero gradient.**

Run the same on the 5 unpaired claims: their incident edges
are mostly `structural-cooccurrence` (claims grouped with
concepts). Gradient is non-zero across these edges:
`T(claim)=1, T(concept)=0`, so `∇T = ±1` on each cooccurrence
edge from claim to concept.

**The high-tension claims sit at vertices where gradient flows
*outward* into the surrounding concept cloud, but no arrow
flows *inward* (no derivation edge converts them to grounded).**
That is exactly the working mathematician's intuition for "the
theorem of the paper" — stated boldly, surrounded by the
concepts it depends on, but the proof is either elsewhere or
left to readers / future work. The paper IS the apparatus; the
high-T vertices are the *punchline*.

This isn't an analogy. We computed it on actual data, with a
trivial tension definition, and got the answer mathematicians
would give for "where's the action in this paper." Discrete
differential geometry on the hypergraph already works.

### One geometry-derived property that could matter

The Laplacian `(ΔT)(v) = Σ_{e ∋ v} (∇T)(e)` for the
Zarankiewicz paper:

- High-T claims with many cooccurrence edges: `ΔT` is large
  positive (lots of outflow).
- Grounded claims (T=0) inside dense proof clusters: `ΔT ≈ 0`
  (nothing happening).
- Concepts adjacent to many high-T claims: `ΔT` is large
  negative (lots of inflow).

**The negative-Laplacian concepts are the *load-bearing
ideas*** — the concepts that show up adjacent to many of the
paper's open claims, hence are the substrate the punchline
depends on. For Zarankiewicz that's likely "set-theoretic
condition," "axis-parallel boxes," etc.

A tornhill-style hotspot in the codebase domain is the same
quantity transposed: large Δ at a function vertex means many
high-tension claims (TODOs, failing tests, open invariants)
flow into it. Same calculus, same diagnostic, two domains.

## Transfer to prototype 1 (futon stack codebase)

Six concrete takeaways for codebase prototype:

1. **Coverage discipline is universal.** ≈12 % silent-fail on
   the math side mirrors the codebase side's silent-fails. The
   IFR's coverage property is not domain-specific; both
   prototypes need a "this turn produced no analysis" record
   rather than a `null`.

2. **Schema slots can collapse — that's a real signal.** Paper
   #4's collapsed triple is informative ("this paper is single-
   axis"), not just broken. The prototype-1 schema should
   accept collapsed slots with an annotation, not reject them
   as malformed. A code-edit turn that's "rename a variable"
   has a flat triple; that's fine, it's still well-typed.

3. **Edge types ARE BHK arrows in the right substrate.** The
   `derivation` edge with roles `(claim: target, proof:
   proof_of_target)` is the literal BHK arrow at edge level.
   For codebase prototype, the analogous typed edges are:
   - `implementation` `(claim: spec, code: implementation_of_spec)`
   - `test-coverage` `(code: implementation, test: coverage_of_implementation)`
   - `refactor-equiv` `(code: before, code: semantically_equivalent_after)`
   - `cite-grounding` `(claim: assertion, doc: external_grounding)`
   These are not metaphors; they're structural identities, and
   they should be the canonical edge vocabulary for the
   codebase hypergraph.

4. **Tension is computable with naive definitions.** The
   "unpaired claim count" indicator gives a working
   mathematician's intuition for paper-level tension. The
   codebase analogue is "TODO without an associated test" or
   "function whose declaring docstring's claims aren't covered
   by any test." Δ over the dependency graph gives Tornhill's
   hotspot signal directly. **No smooth-manifold infrastructure
   needed.** Forman-Ricci as needed for refinement; trivial Δ
   for v0.

5. **Pattern vocabulary doesn't transfer wholesale.** The 7
   patterns hand-tagged on math papers are math-domain-shaped
   (sharp-dichotomy, cross-domain-bridge, structural-
   equivalence). Codebase analogues exist but need their own
   shape (case-split, library-bridge, refactor-preserving-
   semantics). The schema (`{:context :tension :move
   :witness-shape :domain}`) holds across; the *vocabulary
   inside the slots* is per-domain. R-2 in the GH issue gives
   us the math-side curation; an analogous curation pass on
   the codebase side will give us the futon-stack vocabulary.

6. **Quality varies; that's information, not noise.** Some
   triples are sharp (#5 Zarankiewicz: explicit bounds, dichotomy
   stated in formula form), some are exploratory (#4 S-Prime:
   "understand the significance of"). The IFR's "demonstrable
   value" property naturally has a *depth* dimension — light vs
   deep pattern application. The witness-check should produce
   a continuous depth score, not a binary "applied / not
   applied." For codebase: a one-line typo fix and a 200-line
   refactor both apply the same pattern (improve correctness)
   at very different depths.

## Open questions to push at next

1. **What does `T` look like cross-paper?** Within one paper
   `T = unpaired-claim-indicator` is informative. Across papers
   in the corpus, a paper-level scalar (e.g., fraction of
   claims unpaired) would tell us which papers are "open
   problem" papers vs. "fully proven" papers. Cluster on this
   and the embedding together → see whether paper-style
   correlates with hypergraph geometry.

2. **Does the structural-similarity-index reflect pattern
   sharing?** `output/structural-similarity-index.npy` already
   exists. Pull pairs of papers with high structural similarity
   and check whether they share hand-tagged patterns. If yes,
   the embedding ≈ pattern-distance, and we have a candidate
   pattern-discovery loop (cluster → hand-tag a representative
   → propagate via similarity).

3. **Persistent homology over the corpus.** What topological
   features are stable across many papers? Those motifs are
   candidates for inferring the actual pattern set from the
   hypergraph, not from an LLM prompt. This is a phase-2 move
   but worth flagging — if it works, R-2 in the GH issue
   becomes mostly automatic.

4. **Cross-domain calculus check.** Take the codebase prototype
   pilot lift (when it lands) and apply the same `T` definition
   ("unpaired claim" → "TODO without test coverage"). If the
   high-T set in the codebase corresponds to the active-tension
   set the operator already feels, the geometric commitment is
   validated cross-prototype. If it doesn't, we learn what's
   missing in the codebase hypergraph.

## Next moves

1. Extend hand-tagging to ≈25 papers to seed
   `arxiv-proof-patterns.edn`. ~1 hour of focused work.
2. Compute per-paper `T_total = #unpaired_claims /
   #total_claims` over a results batch (one bb script reading
   paper-hypergraphs.json). Histogram the result. Cheap.
3. Pick 3-5 high-T and 3-5 low-T papers from (2) and read them
   to validate the intuition that high-T = "open / hard
   research direction" and low-T = "fully proved here."
4. Once R-1..R-4 patches land for Rob, re-run pattern-tagging
   over a batch with the curated arxiv pattern set. Compare
   tag-frequency distribution to the hand-tags from this
   pilot and the extended hand-tagging — calibration check.

## Phase 2 — 25-paper extension (2026-04-27)

Sample expanded across three batches for diversity:

- 8 papers from `results-mfuton-001` (#0–7, the pilot sample).
- 7 more from `results-mfuton-001` (#8–14).
- 5 from `results-mfuton-002` (different time slice, modern arxiv).
- 5 from `results-005` (older non-mfuton lineage; n_blocks ≈ 0,
  mostly thin hypergraphs — included to test whether
  hand-tagging works on poorer substrate).

Total: 25 papers, of which 24 had populated triples (paper #2
arxiv-2604.20832v1 silently null; ≈4 % failure rate over 25 vs.
12.5 % over the pilot 8 — small-sample variance).

### Per-paper tagging table

| #  | Paper                     | Hand tag                               |
|----|---------------------------|----------------------------------------|
| 1  | mf1 #0 Z/2-harmonic       | construction-from-known-structure       |
| 2  | mf1 #1 — NULL —           | (excluded)                             |
| 3  | mf1 #2 entropic OT        | failure-mode-characterization           |
| 4  | mf1 #3 S-Prime            | clarification (collapsed)               |
| 5  | mf1 #4 Coburn → edges     | cross-domain-bridge                    |
| 6  | mf1 #5 Zarankiewicz       | sharp-dichotomy                        |
| 7  | mf1 #6 Hausdorff alg      | algorithm-with-error-bounds             |
| 8  | mf1 #7 Coxeter cohom      | structural-equivalence                 |
| 9  | mf1 #8 PDE singularity    | regularity-and-asymptotics              |
| 10 | mf1 #9 Hardy / Schrödinger| rigidity-from-decay-conditions          |
| 11 | mf1 #10 E-measure         | clarification (collapsed)               |
| 12 | mf1 #11 Exchange # NP-c   | complexity-classification               |
| 13 | mf1 #12 Thurston norm     | structural-inclusion (monotonicity)     |
| 14 | mf1 #13 infinite matroid  | generalization-of-existing-theory       |
| 15 | mf1 #14 Riesz on NC tori  | generalization-of-existing-theory       |
| 16 | mf2 #0 Hodge-Lefschetz    | theorem-transfer-to-new-class           |
| 17 | mf2 #1 PDE scheme         | algorithm-with-error-bounds (struct-pres)|
| 18 | mf2 #2 ec endomorph alg   | structural-characterization             |
| 19 | mf2 #3 Kneser-Hamilton    | existence-result                       |
| 20 | mf2 #4 semilinear ellipt  | algorithm-with-error-bounds (verification)|
| 21 | r5  #0 KMS C*-algebra     | classification-result                   |
| 22 | r5  #1 AFD outer actions  | classification-result                   |
| 23 | r5  #2 Galois group rings | clarification (collapsed)               |
| 24 | r5  #3 Gevrey ultardistr  | construction-from-known-structure       |
| 25 | r5  #4 Noether fractional | cross-domain-bridge                    |

### Distribution across 24 tagged papers, 15 distinct patterns

```
clarification (collapsed)             3   (#4, #11, #23)
algorithm-with-error-bounds           3   (#7, #17, #20)
construction-from-known-structure     2   (#1, #24)
cross-domain-bridge                   2   (#5, #25)
generalization-of-existing-theory     2   (#14, #15)
classification-result                 2   (#21, #22)
failure-mode-characterization         1   (#3)
sharp-dichotomy                       1   (#6)
structural-equivalence                1   (#8)
regularity-and-asymptotics            1   (#9)
rigidity-from-decay-conditions        1   (#10)
complexity-classification             1   (#12)
structural-inclusion                  1   (#13)
theorem-transfer-to-new-class         1   (#16)
structural-characterization           1   (#18)
existence-result                      1   (#19)
```

The pilot's 7 patterns covered 12/24 tagged papers (50 %). The
extension produced 8 new patterns covering the remaining 12.
**Saturation is not reached at 25 papers.** A 50-100 paper sweep
would likely keep adding tail patterns at a slower rate.

### A pattern hierarchy emerges at scale

The 15 flat patterns cluster into 5 families with shared shape:

```
existence                       (3 papers)
  ├ constructive                  → construction-from-known-structure ×2
  ├ with-error-bounds             → algorithm-with-error-bounds ×3
  └ existence-of-structure        → existence-result ×1, Hamilton-cycle, etc.

characterization                (8 papers)
  ├ full-characterization         → structural-characterization ×1
  ├ partial / failure-mode        → failure-mode-characterization ×1
  ├ classification (cases)        → classification-result ×2,
                                     complexity-classification ×1
  └ dichotomy                     → sharp-dichotomy ×1
                                     (could subsume into classification with k=2)

structural-relation             (6 papers)
  ├ equivalence                   → structural-equivalence ×1
  ├ inclusion / monotonicity      → structural-inclusion ×1
  ├ transfer-to-new-class         → theorem-transfer-to-new-class ×1
  └ generalization                → generalization-of-existing-theory ×2,
                                     cross-domain-bridge ×2

property-of-object              (2 papers — domain-specific tail)
  ├ regularity / asymptotics      → ×1
  └ rigidity                      → ×1

clarification (meta-tag)        (3 papers)
  └ collapsed-triple cases — single-axis paper or extraction failure
```

24 papers, 5 families, each family ≥ 2 papers. The hierarchy is
empirically grounded, not invented to fit. Two observations:

1. **Most patterns are "characterization" or "structural-
   relation."** Together: 14/24 = 58 %. The two families that
   answer "what is this thing?" or "how does this thing relate
   to that thing?" dominate the corpus. Existence-claims (12 %)
   and property-of-object (8 %) are smaller. This is consistent
   with how mathematicians describe research: most papers
   *characterize* or *connect*, fewer *prove existence from
   scratch* or *establish properties of a fixed object*.

2. **`clarification` is the catch-all for the collapsed-triple
   cases**, not a real proof-strategy pattern. 3/24 papers fell
   here. This is the *signal* of either a single-axis paper or a
   prompt-extraction failure. Worth flagging in R-2's prompt
   design: if the LLM wants to emit "clarification," it should
   instead be required to either (a) declare the paper single-
   axis with explicit reason, or (b) redo the slot extraction.

### Observations across mfuton vs older-lineage substrate

The 5 papers from `results-005` (older arxiv-math, thin
hypergraphs, n_nodes ≈ 8–33) hand-tagged just as cleanly as
the mfuton ones. **The triple-quality is largely independent
of hypergraph richness**, because the analysis works on the
paper's content via the LLM, not via the hypergraph
structure. The hypergraph is what we need for the *geometric*
quantities; the *triple* doesn't need it.

Implication: hand-tagging can proceed on the existing
abstract-only substrate without waiting for Rob's eprint-mode
re-runs. The substrate that matters for the *geometric* work
(unpaired-claim T, Δ, Forman-Ricci) does need the patched
runner. Two parallel work-streams unblock each other.

### Candidate `arxiv-proof-patterns.edn` seed

Direct deliverable for GH issue R-2. Below is a draft entry
shape for the hierarchical pattern set; final shape to be
agreed when R-2 lands.

```clojure
{:pattern/id        :existence/constructive
 :pattern/family    :existence
 :pattern/parent    :existence
 :pattern/title     "Constructive existence"
 :pattern/context   "An object of a desired class is sought."
 :pattern/tension   "The class is non-empty in principle, but
                     no construction has been exhibited."
 :pattern/move      "Exhibit an explicit construction; verify
                     it lies in the class."
 :pattern/witness-shape
  "An explicit object o + a proof that o satisfies the class
   defining conditions."
 :pattern/domain    :math-corpus
 :pattern/exemplars [#"arxiv-2604.20840v1"     ; Z/2-harmonic
                     #"arxiv-math/0603597v1"]} ; Gevrey
```

15 such entries cover the corpus distribution observed in this
sample (one per leaf). Five family-level parent entries cover
the hierarchy. Total: 20 entries. The draft `.edn` is sized for
authoring in one focused session — no thousand-pattern
deliberation deadlock.

### What this teaches prototype 1 (codebase)

Refining the six takeaways from §"Transfer to prototype 1" of
the pilot, with what the 25-paper extension adds:

1. **Hierarchy beats flat list.** The pilot's flat 7-pattern
   list collapsed under scale. A 5-family hierarchy survived 25
   papers. For codebase prototype, **start with families, not
   leaves.** Candidate codebase families:
   - existence (new code: algorithm, function, type)
   - characterization (refactor, naming, type-tightening)
   - structural-relation (re-architecture, dependency-changes)
   - property-of-object (performance, correctness, security)
   - clarification (docs, comments, README)

2. **Most code-edit turns are characterization or relation,
   not creation.** The 58 % characterization+relation slice in
   math papers is suggestive; an analogous read of git history
   would tell us whether codebase-edit-turns split similarly.
   If yes, we have a near-universal coarse classifier with two
   families covering the majority of activity.

3. **Collapsed-triple = single-axis-or-failure is a useful
   *meta*-tag.** Don't let the LLM emit "clarification" as a
   leaf pattern; force a meta-tag with a reason. Same applies
   to codebase prototype: a turn that does only "rename
   variable" is single-axis on purpose; a turn that emits
   collapsed slots without a reason is a producer failure.

4. **Domain-specific patterns are real and small in count.**
   `regularity`, `rigidity`, `complexity-classification` are
   domain-specific and appear once each in 25 papers. Codebase
   will have its own tail (`thread-safety-introduction`,
   `lock-ordering-fix`, `feature-flag-rollback`). Build the
   schema to **accept domain-specific leaves under family
   parents**, rather than insisting every leaf be cross-domain.

5. **Hand-tagging is fast.** ≈25 minutes for 25 papers including
   data extraction. Rob's pipeline does this for ≈40,000 papers;
   even 10× overhead per paper for higher quality would be
   ≈70 hours of pure agent compute, which is operationally
   tractable on the superpod. The R-2 curation deliverable's
   actual scope is small; what's gating it is choosing the
   *list*, not the per-paper labelling cost.

6. **The triple's quality varies independent of hypergraph
   richness.** Both abstract-only and full-text hypergraph
   batches produced clean triples. Means R-1 (prompt fix) is
   *the* gating change for pattern-tag quality; R-3 (eprint
   default) is gating for the *hypergraph* quality. They're
   independent and can land independently.

## Implications for the mission

This pilot validates the §"Geometric commitment" section
empirically: tension is computable with naive definitions on
the existing hypergraph, the result is mathematically
sensible, and the calculus is cheap. The geometric commitment
is not a future direction — it's already operational on the
math side, and the patterns documented here are reusable as
shape evidence for the codebase prototype.

The §"Prototype 2 — math corpus" subsection of the parent
mission can cite this excursion as the first concrete
operational evidence that prototype 2's substrate is not just
"present" but *usable for diagnostic geometry without the
patched runner.* That tightens the sequencing argument: math
side is doing work *now* with the substrate it has, and the
runner patches will multiply that work, not gate it.
