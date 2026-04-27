# Excursion: T_total — paper-level tension scalar across a batch

**Status:** STUB / parked (2026-04-27)
**Owner mission:** `holes/missions/M-pattern-application-diagnostic.md`
**Parent excursion:** `holes/excursions/E-math-prototype-pilot.md`
  (this excursion is open-question item 1 from there).

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
