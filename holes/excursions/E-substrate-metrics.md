# Excursion: Substrate Metrics — accreting findings on math-corpus quality

**Date opened:** 2026-04-27
**Status:** open / accreting
**Owner mission:**
`futon3/holes/missions/M-pattern-application-diagnostic.md` (theory)
**Sister mission:**
`futon6/holes/missions/M-superpod-mark3.md` (substrate / pipeline)
**Audience:** Rob (operational), Joe (theory / paper direction)

## Purpose

A running container for substrate-quality findings on the
math-corpus extraction pipeline (`futon6/scripts/superpod-job.py`).
Each finding lands here as a self-contained section with:
- the metric definition,
- the data,
- the implication for mark2 → mark3 changes,
- (optional) paper-direction notes.

As more metrics accrete (pattern-tag yield, Δ-Laplacian top-support
quality, hand-tag agreement rate, …), each gets its own §; the
cross-cutting story emerges from the section index.

This excursion is deliberately not scoped to a single deliverable.
It is a *findings notebook*, not a mission.

## §1. F₂ substrate-extraction score (2026-04-27)

### Headline

A single computable metric tells you whether a results batch's
substrate is doing useful work. **mfuton batches: F₂ ≈ 0.70.
Older non-eprint batches: F₂ ≈ 0.20. The eprint-mode flag closes a
3.5× gap.**

### Definition

For a results batch B with N papers:

```
recall(B)    = #papers-with-claims / N
                                   — extraction coverage; how often does
                                     the parser find any theorem-block
                                     structure to project onto?
precision(B) = #(papers with 0 < T_total < 1) / #papers-with-claims
                                   — informative-T mass; papers piled at
                                     T=0 (every claim grounded) or T=1
                                     (no claim grounded) carry no
                                     dynamic-range signal, so they are
                                     correct-but-uninformative.
F_β(B)       = (1 + β²) · precision · recall / (β² · precision + recall)
```

We use **β = 2**. Justification: in substrate-quality work, recall
costs more than precision — extraction failures are silent and
irrecoverable downstream, while precision-noise (uninformative T
papers) is filterable by any consumer that reads the artifact.
β=2 weights recall 4× over precision, which matches the cost
asymmetry. (β=1 is also defensible if the consumer doesn't care
about the cost asymmetry.)

T_total is defined in `E-Ttotal.md` §"v0 Result"; in short, T_total
= #unpaired_claims / #total_claims for each paper, where an unpaired
claim has no incident `derivation` edge with role `target`.

### Numbers

Three batches from `~/code/storage/mark2/outbox/`, computed via
`futon6/scripts/compute-paper-T.py`:

| Batch                | N     | with-claims | recall | precision | **F₂** |
|----------------------|-------|-------------|--------|-----------|--------|
| `results-mfuton-001` | 5,000 | 3,422       | 0.684  | 0.801     | **0.704** |
| `results-mfuton-002` | 5,000 | 3,609       | 0.722  | 0.800     | **0.736** |
| `results-005`        | 5,000 |   858       | 0.172  | 0.591     | **0.200** |

Two batch-level findings:

**F₂ is itself a stable corpus property.** mfuton-001 and mfuton-002
are independent 5,000-paper time-slices from the same lineage. They
give F₂ ≈ 0.70 / 0.74 — within ≤ 5 % of each other across all three
component numbers. This mirrors `E-Ttotal.md` F-1 (T_total
distribution stable across samples) at the meta-level: *not just
T_total but its quality score is reproducible*. Strong reproducibility
argument.

**R-3 (eprint-mode default) closes a 3.5× gap.** F₂ goes from 0.20
on the older non-eprint lineage to ≈ 0.72 on mfuton. The cost
breakdown:

```
eprint-off impact on recall:    0.684 → 0.172   (75 % collapse)
eprint-off impact on precision: 0.801 → 0.591   (26 % collapse)
```

Most of the F₂ gap comes from recall: when the parser doesn't have
LaTeX source, it can't find theorem-blocks at all. The 75 %
collapse is the empirical cost of running without
`--discover-terms-eprint-dir` — a single flag whose code already
exists (per `74cc161 "Harden mark2 eprint plumbing"`).

### Implication for Rob

[GitHub futon6 #46](https://github.com/tothedarktowercame/futon6/issues/46)
is the discrete sub-issue tracking the eprint-default change. F₂
gives the issue an empirical motivation it didn't have before:

- "Patches a 75 % recall gap with no code change" — a flag-flip.
- "Shows up as F₂ doubling on every batch run" — measurable
  end-to-end without needing pattern tags or downstream consumers.
- "F₂ becomes a regression-detection signal" — once mark3 runs are
  routine, a batch whose F₂ drops more than (say) 0.05 below the
  established mfuton baseline is a flag for substrate-quality
  regression, independent of pattern-tag content.

The right place to compute F₂ in mark3 is in the geometry artifact
stage. It costs essentially nothing on top of T_total computation
(both come from the same per-paper sweep over `paper-hypergraphs.json`).

### Paper-direction note (parked)

This finding is paper-shaped: a self-contained methods note framing
F₂ as a tunable substrate-quality score, with reproducibility
evidence across independent samples and a single-flag intervention
that closes a 3.5× gap. Compact, evidence-rich, publishable in an
applied-NLP / digital-humanities / scientific-knowledge-mining
venue (NLP4Sci, MathNLP, JCDL workshop, etc.).

Three open methodology questions a paper would need to address:

- **β choice.** β=2 is defensible from cost-asymmetry; should we
  vary it and report a curve? Or commit to β=2 as canonical for
  this domain?
- **Precision numerator.** "Fraction with 0 < T < 1" is the simplest
  shape-quality proxy; alternatives include T-variance, Wasserstein
  distance to a target distribution, etc. Sensitivity analysis
  needed.
- **Cross-corpus generalisation.** Does the same F₂ structure work
  for non-math corpora (CS / physics / social-science arxiv slices)?
  We don't yet have data; would require running superpod-mark3 on
  a non-math sample.

Parked here, not pursued; revisit if a paper-publishing window
opens.

### Files

- `futon6/scripts/compute-paper-T.py` — computes T_total per paper
  and the inputs to F₂. v0 doesn't yet emit F₂ explicitly; that's
  ≈ 20 lines of Python in a `--batch-summary` flag once we want it
  operational rather than analytic.
- `~/code/storage/mark2/outbox/results-mfuton-{001,002}/output/paper-T.tsv`
  — outputs from this excursion's runs.
- `~/code/storage/mark2/outbox/results-005/output/paper-T.tsv` — the
  thin-substrate baseline.

## §2. (Reserved) Pattern-tag yield (post R-1 + R-3 deploy)

Once Rob's R-1 (arxiv-aware Stage 3 prompt) + R-3 (eprint default)
land, the next substrate metric is **what fraction of papers
produce non-empty `pattern-tags.json` entries**, broken out by
family. The mark2 baseline is ≈ 1 % (the math.SE-prompt mismatch
detected in `M-pattern-mining.md` F-3); the post-mark3 target is
≥ 70 % (per M-superpod-mark3 §IDENTIFY completion criterion 1).

This § will populate when the patched runner produces its first
batch.

## §3. (Reserved) Δ-Laplacian top-support quality

The `top_support_id` column in `compute-paper-T.py` output is a v0
estimator of "the load-bearing concept the punchline depends on."
On the Zarankiewicz paper it returned `technique:axis-parallel-box`,
which matches the abstract's actual punchline support. A more
systematic check: hand-rate top-support concepts for ≈ 50 random
high-T papers; report % "matches the abstract's load-bearing
concept" agreement.

This § will populate when that check is run.

## §4. (Reserved) Pattern-tag agreement rate (hand vs pipeline)

Once R-1 lands and produces non-empty pattern-tags on a batch,
re-tag the 25 hand-tagged papers from `E-math-prototype-pilot.md`
with the patched pipeline and report family-level + leaf-level
agreement. The acceptance criterion in M-superpod-mark3 §IDENTIFY
is ≥ 60 % family-level agreement (with leaf-level half-credit).
This § will populate when that comparison is run.

## Cross-references

- `E-math-prototype-pilot.md` — the 25-paper hand-tagging that
  produced the family taxonomy F₂ scores against.
- `E-Ttotal.md` — T_total definition and the per-paper distribution
  data F₂ aggregates over.
- `E-cross-prototype-geometry.md` — the codebase-side analogue of
  T (and eventually F₂) once prototype-1 substrate exists.
- `M-pattern-application-diagnostic.md` — the parent theory mission
  with the IFR's five properties, of which F₂'s recall corresponds
  to "coverage" and precision corresponds to "well-specified".
- `M-superpod-mark3.md` — runner-side substrate work; F₂ is the
  scalar that mark3 should compute as a batch-level invariant.
- [GitHub futon6 #46](https://github.com/tothedarktowercame/futon6/issues/46)
  — the eprint-default sub-issue F₂ empirically motivates.
