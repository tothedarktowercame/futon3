# self-heal/ — how repairs have actually been done

Joe, 2026-10-28: self-heal is to be refactored "in pattern terms like we did
selection", and the self-heal cascade "should presumably include a
pattern-based intake/analysis/extension for any new findings". The statement
these patterns serve is `cascades/heal-by-repair-cascades`. A repair policy is
a design-pattern cascade, evidenced by how repairs were actually done; these
patterns are the pieces. None was brainstormed. The evidence catalogues are:

- `~/notes/repair-patterns/witnesses-machine-repairs-2026-10-28.md`
  (codex-15, 55 witnesses, SHA-256 `5068a461…`): injury ledger, medic
  dispositions, repair receipts, wm-contract discoveries and packets,
  A-self-heal receipts, repair commits. Cited below as "machine Kn.m".
- `~/notes/repair-patterns/witnesses-intake-and-extension-2026-10-28.md`
  (codex-10, 51 witnesses, SHA-256 `081a44f0…`): tickets, technotes, the
  apparatus patterns' own evidence, library promotion history, Joe's repair
  rulings. Cited below as "intake Kn.m".

Each witness is marked worked, partial, did not work, parked or promotion
only. Twelve sampled quotes were checked against their sources (claude-11,
2026-10-28).

## The patterns, by role

Each pattern has `@guard` (what it reads), `@effect` (what it changes) and
`@predicts` (what should be observed afterwards), so that order follows from
what each needs, and so that a step whose effect is missing can be seen.

| role | pattern | tension | witnesses | apparatus it applies |
|---|---|---|---|---|
| intake | record-the-finding-with-its-target | record something vs record enough to repair | intake K1; machine K11 | open-conditions-live-in-a-ledger, replayable-not-precious |
| locate | locate-the-producer-first | edit where it surfaced vs find where it was made | machine K1 | repairs-name-defects-not-neighborhoods, model-upstream-and-coupled |
| locate | reproduce-on-retained-inputs | build a fixture vs replay the incident | machine K2 | recorded-failures-before-live-runs |
| decide | a-correct-refusal-is-not-the-defect | make the refusal go away vs repair its input, or stop | machine K3 | one-authority-per-question |
| witness | fix-the-red-before-the-repair | a green test vs a test that was red | machine K4; intake K2.4 | success-must-not-resemble-failure |
| fix | repair-the-producer-not-the-checker | fix the neighbourhood vs one commit at the producer | machine K5 | repairs-name-defects-not-neighborhoods |
| prove | witness-the-path-not-the-helper | helper green vs path green, on loaded code | machine K6; intake K2 | done-is-observed-running, bind-promotion-to-post-repair-replay |
| review | a-different-seat-reviews | the author's log vs another seat's judgment | machine K7; intake K6 | bind-promotion-to-post-repair-replay |
| dispose | retire-by-appending-to-the-ledger | say "repaired" vs append through the ledger's grammar | machine K8 | evidence-to-disposition-once, open-conditions-live-in-a-ledger |
| continue | retry-only-the-repaired-phase | retry / fresh click vs abort or stop | machine K9; intake K5 | the-system-stops-on-schedule, every-wait-has-a-deadline |
| check | a-step-that-did-nothing-is-an-observation | no error vs the effect happened | machine K10 | success-must-not-resemble-failure, loudness-is-conserved |
| intake (closing) | take-in-what-no-step-predicted | report the outcome vs record the prediction errors | machine K11; intake K3.2 | — |
| analysis (closing) | match-the-finding-against-the-library | add a class vs match, mark, or ask one layer down | intake K3, K4 | new-failure-class-is-a-design-defect |
| extension (closing) | introduce-the-pattern-from-the-repair | promote on argument or scan later vs formal introduction citing the repair | intake K3, K6.6 | bind-promotion-to-post-repair-replay |

Ordering against other work is not a self-heal pattern: it is
`task-selection/repair-goes-to-the-front`, with the correction in intake
K5.2-K5.4 (a front ticket attempted and not resolved stops the line and is
repaired from outside), which `retry-only-the-repaired-phase` guards on.

`a-step-that-did-nothing-is-an-observation` is not a position in the order: it
is applied after every step, comparing the step's `@effect` with the state.

## Repair cascades in the record

A repair policy is an ordered composition of these patterns. The record holds
these; each is a candidate repair policy, not a proposal.

1. **Genuine repair of a code defect** (T3 a59c, 2026-10-28; worked, retired):
   record → locate (runner catch) → reproduce (T3 red on retained trip) →
   decide (T3 correct, runner wrong) → red-before boundary (three arms) → fix
   producer (`cc050c0a7`) → path witness (`9b1cbba12`) → review → retire
   (`transact-medic-disposition!`).
2. **Repair at the source, history left for disposition** (2aef requeue
   identity `5cdaf1c75`; 2aef silent phase skip `a5dce0c19`): locate → decide
   (refusal correct) → red-before at the real check → fix producer; the
   existing contradictory record is then a separate disposition (cascade 1's
   last steps, or cascade 3).
3. **Historical misclassification** (a9b900a4, 9c622038): reproduce (replay
   says non-injury) → decide (no defect) → retire as a correction. No code.
4. **The debugger loop** (A-self-heal, 2026-10-02): record (keep the stop as
   the patient) → locate → one bounded fix → review → retry the repaired
   phase or abort → the next stop is a new observation (back to locate).
5. **Stop without repair** (repair iterations 1 and 3; readiness refusal):
   locate → decide (premise not established, or boundary unchanged) → stop,
   ledger bytes unchanged → repair taken outside.
6. **Repair with library extension** (APM principle-scoring trial, September
   2026, outside the machine): record → repair → score each candidate
   principle against the repair (worked, misled, uncovered, unscored) →
   promote only what the field evidence supports.

Cascades 1 and 2 share most patterns and differ in whether the existing
record is retired or left for a separate disposition. Cascade 3 skips the
fix entirely. Given the same injury, they predict different things, which is
what lets G choose among them.

## The present fixed pipeline, against these patterns (first reading)

`meta_self_heal_production` runs diagnose → acceptance test → claim → author →
review → enact → retry for every injury.

- diagnose ≈ locate-the-producer-first, without reproduce-on-retained-inputs
  as a separate step;
- acceptance test ≈ fix-the-red-before-the-repair, but written by the
  machine from the diagnosis; the red-before check
  (`:meta-self-heal/acceptance-test-passes-before-repair`) catches a vacuous
  test and then has no step to go to (machine K4.1);
- author / review / enact ≈ repair-the-producer-not-the-checker,
  a-different-seat-reviews, and activation;
- retry ≈ retry-only-the-repaired-phase;
- disposition (medic) sits outside the pipeline and was assembled by hand for
  a59c (machine K8.1).

Not present as steps: a-correct-refusal-is-not-the-defect (there is no
"stop, the premise is unproved" or "this is a misclassification" branch, so
cascades 3 and 5 can only happen by hand); witness-the-path-not-the-helper;
a-step-that-did-nothing-is-an-observation (silent skips existed until
`a5dce0c19`); and all three closing steps. So the pipeline is roughly
cascade 1 or 2 with its order hard-coded, and the other cascades are done
outside it. This reading is claude-11's from the code and the record; a
discovery should check it line by line before anything is built.

## Scoring a repair cascade (proposed, not built)

Joe, 2026-10-28: "we should exercise our normal G-over-cascades mechanism
rather than set up a fixed pipeline". So there is no repair-level model or
scorer of its own. Each repair cascade fills the `:self-heal` arm of
`meta/meta-outer-policy-cascade` as one candidate policy, and the G that
scores filled META policies (`:meta/minimise-g-over-filled-meta-policies`)
chooses among them; a cascade's prediction is the conjunction of its
patterns' `@predicts` for this injury. The closing patterns are part of the
cascades G scores, not a tail added by code. Criteria:
`futon2/holes/labs/wm-contract/E-self-heal-cascade.md`.

## Open

- No witness yet of the machine doing the closing intake / analysis /
  extension itself (machine catalogue, thin kinds); those patterns are
  evidenced by how it was done by hand.
- The clock-busy repair has a diagnosis and a non-recurrence but no ledger
  retirement (machine catalogue, thin kinds).
- Neither catalogue read all ~140 generated `T-repair-occ-*` tickets, nor
  the full META ledger entry by entry.
- Guards and effects are prose; nothing here is executable yet.

## Status

Drafts, 2026-10-28 (claude-11, from Joe's statements of the same day). For
Joe to read and correct before anything is built on them.
