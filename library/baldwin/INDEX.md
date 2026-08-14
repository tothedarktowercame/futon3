# Baldwin mechanism argument

This library separates the high-level engineering metaphor in
`futon-theory/baldwin-cycle` from the causal claims an evolutionary experiment
must support.

The argument begins at [mechanism](ARGUMENT.flexiarg): plasticity can guide
evolution when it exposes inherited differences to selection; complete genetic
assimilation is a stronger, conditional outcome.

## Argument map

1. [Two claims, not one](two-claims-not-one.flexiarg) distinguishes
   learning-guided evolution from reduction of plasticity.
2. [A selective neighbourhood](plasticity-builds-a-selective-neighbourhood.flexiarg)
   explains the central Hinton--Nowlan mechanism.
3. [Charge for realized work](charge-for-realized-work-not-for-capacity.flexiarg)
   states which kind of cost can drive assimilation and which cannot.
4. [A heritable shadow](assimilable-traits-need-heritable-shadows.flexiarg)
   states what a plastic contribution needs before it can be fixed.
5. [A resolved path](assimilation-requires-a-resolved-path.flexiarg) separates
   endpoint existence, genetic accessibility, selectability, and measurement resolution.
6. [Stationarity](stationarity-decides-what-can-be-fixed.flexiarg) determines
   whether selection should favor specialization or retained plasticity.
7. [The heritable unit](choose-the-heritable-unit-where-invariance-lives.flexiarg)
   separates "no assimilable structure" from "none in these coordinates".
8. [Ablation and instrument](ablation-axes-must-not-disable-the-instrument.flexiarg)
   guards learning-curve and truncated-budget designs.
9. [The MetaCA evidence](metaca-negative-evidence-localizes-the-bottleneck.flexiarg)
   localizes the current negative without generalizing beyond its representation.
10. [The next claim](test-evolution-of-learnability-before-static-assimilation.flexiarg)
    proposes evolution of learnability as the next preregistered target, with strong
    assimilation retained as a secondary endpoint.

## Revision, 2026-08-01

Revised after the review appended to
`futon5/holes/tech-notes/TN-baldwin-experiments-status.md` (sections R1--R10).
Patterns 3, 7 and 8 are new; 2, 4, 6, 9 and 10 were sharpened; the root argument
gained four obligations and four failure modes.

The revision was prompted by an uncomfortable finding, recorded here because it is
the most reusable thing in it. **This library was not mainly wrong; it was mainly
unenforced.** Two prescriptions that would have caught the central defect were
already written down before the pilots ran and were not implemented:

- `plasticity-builds-a-selective-neighbourhood` asked for "a preregistered
  inherited-perturbation response curve" in preflight. Preflight instead checks that
  `:field` mutates (I4 reachability) and that the *gamma* axis is navigable (I6). The
  field axis — the one that has to carry assimilation — is never tested for response.
- The same pattern asked to "replace the band-only scalar". `fitness` remained
  `band-score(reach) - c * plasticDependence` and guidance preparedness remained
  `:population-mean-band-score`.
- `test-evolution-of-learnability-before-static-assimilation` asked to score
  "time-to-threshold, cumulative realized rewrites, and held-out-task performance".
  Only the third was registered; the run set `plasticityCost := 0`.

A pattern whose prescription lives only in prose can be satisfied in name by a
registration that drops its load-bearing clause, and no apparatus check will notice.
The root argument's `CHECK` section now states four obligations in the form of
registration fields that fail closed. Future work on this library should prefer
adding an obligation over adding a paragraph.

## Evidence boundary

The local numerical claims refer to:

- `mmca-clj/data/baldwin/indifference_map_fixedgeno.tsv` (corrected per-cell
  indifference; `mmca-clj` commit `1081503`)
- `mmca-clj/data/baldwin-runs/baldwin-hold-only-20260731-084131/`
- `mmca-clj/data/baldwin-runs/baldwin-search-pilot-20260731-200346-r2/`
- `mmca-clj/data/baldwin-runs/baldwin-guidance-pilot-20260801-135800-r2.reanalysis/`
- `mmca-clj/src/mmca/hinton_nowlan.clj` (the planted positive control)
- `futon5/holes/tech-notes/TN-baldwin-selection-rewriters.md`
- `futon5/holes/tech-notes/TN-baldwin-experiments-status.md`
- `mathlib4/DarkTower/BaldwinDesign.lean`
- `mathlib4/DarkTower/BaldwinSearchPreregistration.lean`
- `mathlib4/DarkTower/BaldwinGuidancePreregistration.lean`

Both pilots used one evolution seed and their registered confirmation seeds are
unspent. They are mechanistic evidence and a source of new falsifiable hypotheses,
not replicated estimates of a general effect. The guidance pilot additionally has the
axis defect described in pattern 8, so it should not be counted as an independent
negative about assimilation.

## Indexing

All eleven patterns are in `resources/sigils/patterns-index.tsv` and are surfaced by PSR
hotword search.

They were *not* indexed when this file was first written on 2026-08-01, which was
recorded here as a gap. It closed on its own: the TSV is a symlink into
`storage/futon3/resources/sigils/` rebuilt nightly at 04:30 by the `index_patterns.sh`
cron, and the patterns landed in the 2026-08-02 run. The indexing lag is therefore up to
24h after a pattern is written, and no manual `build_pattern_index.clj` invocation is
needed. Note that the *committed* path is the symlink, not the content — `git show
HEAD:resources/sigils/patterns-index.tsv` returns the link target, so a stale-looking
git history for this file says nothing about the live index.
