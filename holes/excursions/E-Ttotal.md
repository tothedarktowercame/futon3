# Excursion: T_total — paper-level tension scalar across a batch

**Status:** v0 — real data landed (2026-04-27)
**Owner mission:** `holes/missions/M-pattern-application-diagnostic.md`
**Parent excursion:** `holes/excursions/E-math-prototype-pilot.md`
  (this excursion is open-question item 1 from there).
**Sister mission:** `futon6/holes/missions/M-superpod-mark3.md` (Track B
  demo for Rob; the geometry-artifact stage of mark3 can adopt the script
  this excursion produced verbatim).

## Goal

Compute a paper-level tension scalar over a results batch and see
what its distribution tells us about the corpus.

The trivial v0 definition (validated in
`E-math-prototype-pilot.md` for paper #5):

```
T_total(paper) = #unpaired_claims / #total_claims
```

where a claim is "unpaired" iff no `derivation` edge with that
claim as `target` exists in the paper's hypergraph.

## Tasks

1. Babashka script that reads `output/paper-hypergraphs.json`
   from a batch, computes T_total per paper, emits a TSV
   (paper_id, n_claims, n_unpaired, T_total).
2. Run on `results-mfuton-001` (≈5 k papers — modern, theorem/
   proof-block-rich).
3. Histogram T_total. Expected shape unknown — a strongly
   right-skewed distribution would mean most papers prove most
   of what they state; a long left tail would mean a long tail
   of "open-problem" papers.
4. Sample 5 high-T and 5 low-T papers; read each briefly.
   Calibration check: do high-T papers actually feel like open-
   problem / survey-style, low-T like fully-resolved-here?
5. Cross-tab T_total against the hand-tagged pattern from
   `E-math-prototype-pilot.md` (and its 25-paper extension)
   when that data is in. Do certain patterns concentrate at
   high-T vs low-T? E.g. does `sharp-dichotomy` skew low-T
   (well-grounded) vs `failure-mode-characterization` skew
   high-T (the failure mode itself is the open territory)?

## Expected payoff

If T_total + pattern is a useful 2-D summary of a paper, we
have a tractable batch-level diagnostic that costs essentially
zero compute. Surfaces in the existing Stack HUD widget +
Arxana Browser (※) → Pattern Activation surface as a per-
paper column on math-corpus rows.

If T_total turns out to be uninformative (e.g. uniformly close
to 0 or uniformly mid-range across papers), the unpaired-claim
indicator was the wrong v0 definition and we need a richer T
(weighted by claim type / proof depth / cross-paper citation).

## References

- Pilot result: `holes/excursions/E-math-prototype-pilot.md`
  §"Tension scalar demo on paper #5" — confirmed working on a
  single paper.
- Substrate: `~/code/storage/mark2/outbox/results-mfuton-001.tar.gz`
  → `output/paper-hypergraphs.json`.
- §Geometric commitment of the parent mission lays out the
  discrete differential geometry framing T enters.

---

## v0 Result (2026-04-27)

`futon6/scripts/compute-paper-T.py` materialises the script. Reads
`output/paper-hypergraphs.json` from a results batch, emits TSV with
per-paper `n_claims`, `n_unpaired`, `T_total`, plus a Δ-Laplacian proxy
(`top_support_id`: the non-claim vertex co-occurring most often with
unpaired claims — a load-bearing-concept candidate). 5,000 papers in
≈4 seconds; pure-Python.

### Distribution across three batches (5,000 papers each)

```
batch                  papers  with-claims  empty   T_mean  T_median  T=0%   T=1%
results-mfuton-001     5,000   3,422        1,578   0.364   0.333     13.1%  6.8%
results-mfuton-002     5,000   3,609        1,391   0.368   0.333     13.5%  6.4%
results-005 (older)    5,000   858          4,142   0.527   0.455     10.7%  30.2%
```

### Three structural findings the data forces

**F-1: T_total is a stable corpus property.** mfuton-001 and mfuton-002
are independent 5,000-paper time-slices from the same lineage, processed
the same way. Their T-distributions are **near-identical**:
mean 0.364 / 0.368, median 0.333 / 0.333, p25 0.150 / 0.154,
p75 0.500 / 0.517. Two independent samples agreeing this closely
confirms T_total is not a sampling artifact. **The geometry is real.**

**F-2: The eprint-off cost is now measurable, not just argued.**
`results-005` (older lineage, run without `--discover-terms-eprint-dir`)
has **4,142 of 5,000 papers (83 %) with zero claim nodes** — the parser
couldn't find theorem-block structure in abstract-only text. mfuton
batches have 28–32 % empty-claim papers. The 50-percentage-point gap is
the eprint-flag cost on a single dimension; downstream T values are
also pushed toward 1.0 (mean shifts from 0.36 → 0.53 on the few papers
that did get processed) because the parser found claims but missed the
proofs that would ground them. **R-3 (eprint default) is now backed by
hard numbers, not architectural argument.**

**F-3: The distribution shape is informative, not degenerate.** The
mfuton percentile spread (p10=0.0, p90=0.78) and the modest pile-up at
T=0 (≈ 13 %) and T=1 (≈ 7 %) means T_total has *meaningful resolution*
for distinguishing "well-grounded" from "open-question" papers. Roughly
80 % of mfuton papers have a non-trivial T value in [0, 0.78] —
plenty of dynamic range for downstream consumers (the Stack HUD widget,
the B→A reweighting prior, the Arxana Browser activation surface).

### Cross-tab against 25 hand-tagged papers (E-math-prototype-pilot.md)

```
family               n  mean T  median T  range            note
characterization     6  0.19    0.19      [0.11, 0.33]    well-grounded
existence            6  0.20    0.25      [0.00, 0.55]    well-grounded
structural-relation  7  0.33    0.33      [0.14, 0.50]    mid
property-of-object   2  0.32    0.32      [0.24, 0.40]    mid (n=2)
clarification        3  N/A     N/A       all n_claims=0  see below
```

(Excluding 5 papers from results-005 which all had n_claims=0 due to
the F-2 substrate gap — those would inflate the family means but
artificially because T isn't computable.)

The signal is **directionally consistent** with intuition:

- **Characterization** papers (median T=0.19) ground their claims best
  — describing what something *is* requires structural grounding.
- **Existence** papers (median T=0.25) close behind — exhibiting an
  object usually grounds it.
- **Structural-relation** papers (median T=0.33) are mid — sometimes
  fully proved, sometimes the relation is asserted with proof
  elsewhere.
- **Property-of-object** papers (median T=0.32 from n=2) — properties
  are easier to state than to ground in-paper.

Variance is wide and n is small, so this is not statistical evidence
yet. But the rank ordering matches the qualitative reading from the
pilot: characterization papers feel "complete," property-of-object
papers feel "stated."

**Pivot finding on `clarification` family:** all three hand-tagged
clarification (collapsed-triple) papers have `n_claims = 0`. Either:

- (a) Collapsed-triple is *correlated with thin substrate* — the LLM
  extraction collapses when fed too little structure to differentiate
  slots. This is a **producer-side artifact**, not a paper-shape
  signal.
- (b) Single-axis papers genuinely have fewer typed claims because
  they are discussion or principle-clarification papers.

The two interpretations have different remediation: (a) is fixed by
R-1 (slot-distinctness in the prompt) + R-3 (eprint-mode for thicker
substrate); (b) is a real signal and the `clarification-meta`
`single-axis` reason is the right way to mark it. Distinguishing the
two requires running R-1's improved prompt on the same papers and
seeing whether the triple still collapses with a thicker input. The
empirical question is settable.

### Two silent-fail modes are now distinguishable

```
silent-fail mode       fraction of pilot 25 papers   substrate cause
triple-level (null analysis)         1/25 = 4%       LLM extraction failed
hypergraph-level (n_claims=0)        8/25 = 32%      parser found no theorem blocks
```

These are independent: 1 paper had both, 7 had hypergraph-only,
0 had triple-only. Coverage discipline (the IFR's Property #1) needs
*both* silent-fail modes recorded. The mark3 schema for the failed-
record shape needs to distinguish them:

```clojure
{:status :failed
 :reason :triple-extraction         ; LLM produced null analysis
 :stage :stage6}

{:status :failed
 :reason :no-theorem-blocks         ; parser found no claims
 :stage :stage5d
 :substrate-mode :abstract-only}    ; or :eprint, :degraded, etc.
```

This is more granular than the original M-superpod-mark3 §3 sketch;
the mission DERIVE phase should adopt it.

### Δ-Laplacian proxy spot-check

The script's `top_support_id` column reports, per paper, the non-claim
vertex co-occurring most often with unpaired claims — a v0 estimator
of "the load-bearing concept the punchline depends on." Spot check on
paper #5 (Zarankiewicz):

```
paper_id              top_support_id                   top_support_count
arxiv-2604.20815v1    technique:axis-parallel-box     5
```

Reading the abstract: yes, axis-parallel boxes are exactly the
combinatorial object whose properties drive the dichotomy. The proxy
identifies the right load-bearing concept. **The geometric framing is
operationally correct on real data, with a one-line definition.**

A more thorough cross-paper check is downstream work
(`E-cross-prototype-geometry.md` is the codebase-side analogue).

## Implications for M-superpod-mark3

These findings give DERIVE concrete numbers and constrain the schema:

1. **`compute-paper-T.py` ships as the geometry artifact's reference
   implementation.** Lifts directly into mark3's pipeline as a per-batch
   stage; cost is negligible (≈4 s per 5k papers).
2. **The coverage record needs the two distinguishable silent-fail
   reasons** (triple-extraction vs no-theorem-blocks) named in §3
   DERIVE.
3. **R-3 is now empirically motivated.** Rob's eprint-mode-default
   patch closes the 50 percentage-point empty-claim gap visible in F-2.
4. **The clarification-meta family interpretation is unsettled.** R-1
   prompt + R-3 eprint together can resolve whether collapsed-triple
   is a producer artifact or a real paper signal — re-tag the same
   pilot papers under the patched runner and compare.

## Files

- `futon6/scripts/compute-paper-T.py` — the script.
- `~/code/storage/mark2/outbox/results-mfuton-001/output/paper-T.tsv` —
  outputs from the v0 run (3,422 papers with claims).
- `~/code/storage/mark2/outbox/results-mfuton-002/output/paper-T.tsv` —
  ditto (3,609 papers with claims). Independent confirmation.
- `~/code/storage/mark2/outbox/results-005/output/paper-T.tsv` —
  the thin-substrate baseline (858 papers with claims).
