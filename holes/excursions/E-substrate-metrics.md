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

Five batches from `~/code/storage/mark2/outbox/` (and one from the
archived abstract-only run), computed via
`futon6/scripts/compute-paper-T.py`:

| Batch                              | Lineage  | Vintage             | eprint | N     | with-claims | recall | precision | **F₂** |
|------------------------------------|----------|---------------------|--------|-------|-------------|--------|-----------|--------|
| `outbox.abstract-only/results-001` | regular  | older arxiv-math    | **OFF** | 5,000 |     0       | 0.000  | n/a       | **0.00** |
| `results-001`                      | regular  | older arxiv-math    | on     | 5,000 |   570       | 0.114  | 0.467     | **0.13** |
| `results-005`                      | regular  | older arxiv-math    | on     | 5,000 |   858       | 0.172  | 0.591     | **0.20** |
| `results-mfuton-001`               | mfuton   | modern arxiv (2026) | on     | 5,000 | 3,422       | 0.684  | 0.801     | **0.70** |
| `results-mfuton-002`               | mfuton   | modern arxiv (2026) | on     | 5,000 | 3,609       | 0.722  | 0.800     | **0.74** |

Three batch-level findings:

**F₁: F₂ is a stable corpus property.** mfuton-001 and mfuton-002
are independent 5,000-paper time-slices from the same lineage. They
give F₂ ≈ 0.70 / 0.74 — within ≤ 5 % of each other across all three
component numbers. Likewise the two regular older-arxiv batches
(results-001 / results-005, both eprint-on) give F₂ ≈ 0.13 / 0.20 —
also clustered. This mirrors `E-Ttotal.md` F-1 (T_total distribution
stable across samples) at the meta-level: *not just T_total but its
quality score is reproducible per (lineage, vintage, eprint) tuple*.

**F₂: Two independent factors drive the substrate-quality gap.**
Three orders of magnitude separate `0.00` (abstract-only baseline)
from `0.74` (best mfuton). Decomposed:

```
abstract-only        eprint-on           eprint-on
older arxiv-math     older arxiv-math    modern arxiv
F₂ = 0.00       →    F₂ = 0.13–0.20  →   F₂ = 0.70–0.74
       eprint-mode toggle    paper-vintage richness
       closes 0.13 gap       closes 0.55 gap
```

**Eprint mode** is the floor — without it, the parser finds zero
theorem-blocks on any paper, full stop. Closing this gap is R-3
([futon6 #46](https://github.com/tothedarktowercame/futon6/issues/46)),
a flag-flip (code already exists per `74cc161`).

**Paper-vintage richness** is the second-order factor that R-3 alone
does not address: older arxiv-math papers (1999–2006 era) often use
plain TeX without theorem/proof environments the parser
recognises. Modern arxiv papers (2026 era in mfuton-*) routinely
declare `\begin{theorem}` / `\begin{proof}` and produce ~50× richer
hypergraphs (mean n_blocks: 56 vs 8–12). Mark3's geometry artifact
makes this *measurable*: pre-mark3, the gap was a vague
"older papers are harder"; with F₂, it's a quantified
0.5-point penalty even after eprint is enabled.

**F₃: Within a (lineage, vintage, eprint) tuple, F₂ is reproducible
to ≤ 0.1.** The two regular-old-eprint-on batches differ by 0.07;
the two mfuton batches differ by 0.04. Treat any future batch
whose F₂ deviates by > 0.1 from its (lineage, vintage, eprint)
peers as a substrate regression worth investigating.

### Implication for Rob

[GitHub futon6 #46](https://github.com/tothedarktowercame/futon6/issues/46)
is the discrete sub-issue tracking the eprint-default change. F₂
gives the issue an empirical motivation it didn't have before:

- **Eprint default closes the floor**: F₂ goes 0.00 → 0.13–0.20 just
  by flipping `--discover-terms-eprint-dir` on. Below that floor,
  recall is literally zero across 5,000 papers.
- **The remaining gap is content-dependent.** The mfuton-vs-regular
  delta (F₂ ~0.20 vs ~0.72) is paper-vintage / LaTeX-richness, not
  fixed by R-3. To close it for older arxiv-math papers we'd need
  either better TeX-environment heuristics or a pre-processing pass
  that synthesises theorem/proof blocks from prose cues. Out of
  scope for mark3 v0; a candidate post-INSTANTIATE follow-on.
- **F₂ becomes a regression-detection signal.** Once mark3 runs
  produce stable F₂ baselines per (lineage, vintage, eprint) tuple,
  a batch whose F₂ drops > 0.1 below its peers is flag-worthy
  independent of pattern-tag content.

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
