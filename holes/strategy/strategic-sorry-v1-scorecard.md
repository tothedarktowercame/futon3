# Strategic SORRY v1 Scorecard

This note scores the current `🌐N` Strategic SORRY set against the
candidate invariant `strategic-sorry-next-step-sufficiently-specified`
in [structural-law-inventory.sexp](/home/joe/code/futon3c/docs/structural-law-inventory.sexp:193).

The comparison point is not "does this sorry exist?" but "is this sorry
specified enough to become an honest live recommendation in the War
Machine?"

## Rubric

Each handle is scored on seven fields:

1. `canonical target` — the closure object is named unambiguously
2. `concrete action` — there is a specific next move, not only a theme
3. `step witness` — the system could tell whether the move was attempted
4. `effect witness` — the system could tell whether the move changed a
   downstream counter or condition
5. `successor witness` — the system could tell what counts as rolling
   forward once the first move passes
6. `backing surface` — there is a stable story/devmap/mission surface
   behind the handle
7. `failure interpretation` — the note says what non-success would mean

Scoring:

- `1.0` = present
- `0.5` = partial / implied but not yet preregistered cleanly
- `0.0` = absent

Readout bands:

- `7.0 / 7` = recommendation-grade
- `5.0-6.5 / 7` = near-grade
- `3.0-4.5 / 7` = exploratory
- `0.0-2.5 / 7` = named-only

Severity buckets used here:

- `Critical` = `:critical`
- `Warning` = `:medium`
- `Low` = `:low` or `:info`

Important comparison:

- `wm.close-s6.v1` is already an explicit preregistered agenda object
  with step/effect/successor witnesses.
- None of the Strategic SORRY handles have reached that same level yet.
- So this scorecard is intentionally strict: a good devmap can still be
  under-specified relative to recommendation-grade WM execution.

## Scorecard

| Handle | Severity bucket | Score | Band | Main gap |
|---|---|---:|---|---|
| `🌐1` `SORRY-market-interface` | Critical | `6.5` | near-grade | No explicit WM-grade successor witness yet |
| `🌐2` `SORRY-mode-violation` | Critical | `6.5` | near-grade | No explicit WM-grade successor witness yet |
| `🌐8` `SORRY-policy-transition` | Critical | `6.5` | near-grade | No explicit WM-grade successor witness yet |
| `🌐4` `SORRY-paragogy-revenue` | Warning | `4.0` | exploratory | Action and counters still probe-shaped |
| `🌐5` `SORRY-vsat-revenue` | Warning | `4.0` | exploratory | Revenue path sharper than `🌐4`, but still not preregistered as a v1 agenda |
| `🌐3` `SORRY-peer-eval-artifact` | Warning | `3.5` | exploratory | Still a packaging note, not a closure program |
| `🌐6` `SORRY-governance-interface` | Low | `3.5` | exploratory | Deferred and not yet load-bearing |
| `🌐7` `SORRY-novelty-floor` | Low | `3.0` | exploratory | Diagnostic tick, not yet a standalone closure program |

## Breakdown By Bucket

### Critical

`🌐1`, `🌐2`, and `🌐8` are all strategically important and all remain
under-specified by the new invariant, but only narrowly. They already
have:

- explicit canonical targets
- concrete next actions
- plausible step witnesses
- plausible effect witnesses
- stable backing surfaces
- explicit failure interpretations

What they still lack is the same thing `S6v1` now has:

- a preregistered agenda object
- an explicit successor witness
- a machine-readable rule for "this v1 step is consumed; now open v2"

So the focus signal is not "invent more critical devmaps." It is:

- promote `🌐1v1`, `🌐2v1`, and `🌐8v1` into explicit agenda objects
- preregister their successor conditions before execution
- only then let the War Machine recommend them as crisp executable moves

### Warning

The warning-tier items are real but still looser:

- `🌐4` has live probes, but the offer/counter pair is still fuzzy
- `🌐5` has the sharpest medium-tier commercial route, but it is still
  not expressed as an executable v1 closure agenda
- `🌐3` is honest and important, but it is still more "artifact needed"
  than "closure loop ready"

This means:

- `🌐5` is the best candidate for the next warning-tier promotion
- `🌐4` should probably wait until one payment/uptake path is named more
  tightly
- `🌐3` should probably remain a bounded packaging problem until it
  starts projecting into counters or buyer classes

### Low

The low bucket does not currently look like the focus frontier:

- `🌐6` is conceptually real but explicitly deferred
- `🌐7` is useful as a symptom, but still better treated as a tick than
  as a recommendation program

So if operator time is scarce, low-bucket under-specification is not the
place to spend it first.

## Field Notes

### `🌐1` Market Interface

Present:

- canonical target in alignment and devmap
- concrete action in packet + ask + delivery path
- step/effect logic in the devmap prototypes
- stable backing surface
- clear failure interpretation

Missing:

- explicit `🌐1v1` agenda id and action surface
- explicit successor witness for the first successful ask

### `🌐2` Mode Violation

Present:

- clean canonical target (`cargo-implies-depositing`)
- explicit finite closure path
- strong step/effect framing
- stable backing surface
- honest failure story

Missing:

- explicit `🌐2v1` agenda object
- explicit successor rule after first violation-clearance event

### `🌐8` Policy Transition

Present:

- explicit regime target
- concrete Ji-point and balance-shift path
- clear effect framing around ratios and reselection
- stable backing surface
- honest failure interpretation

Missing:

- explicit `🌐8v1` agenda object
- explicit successor rule after the first passed transition witness

### `🌐3`, `🌐4`, `🌐5`, `🌐6`, `🌐7`

These all have enough substance to remain named Strategic SORRYs, but
they are not yet recommendation-grade. The main failure mode is not
"false object" but "not yet specified enough to execute and detect
honestly."

## Operational Conclusion

The current focus frontier is clear:

1. The critical class is where the under-specification matters most.
2. The next move is to mint `🌐1v1`, `🌐2v1`, and `🌐8v1` at roughly the
   same preregistration level as `wm.close-s6.v1`.
3. The warning and low classes should mostly stay candidate pressure
   until their v1 closure paths sharpen.
