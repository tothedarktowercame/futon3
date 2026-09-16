# translation/ — carrying work and meaning across a boundary

Some things in this stack are cleared away; others have to cross a boundary
and still count as the same thing on the other side. A buffer is killed. A
commit is pushed. A test run is handed from author to reviewer. A Lean
declaration becomes a Clojure function. A prose pattern becomes guards and
effects the War Machine can run.

Translating C++ into Python preserves what the program computes but not its
memory layout or timing, and that is fine as long as everyone knows which was
meant. These patterns write that down for the translations we do.

## What happens to the unit

| crossing | example | how to know it went right |
|---|---|---|
| destroy | buffer cleaner kills a buffer | later truth: was it needed? (currently unavailable) |
| move unchanged | inbox zero pushes a commit; test registry hands a run to the reviewer | hash identity, including environment |
| translate | Lean → Clojure contracts; prose pattern → token guards | source binding by hash, then a declared relation tested by behaviour |

The `hygiene/` patterns cover the first two rows for automated upkeep. This
directory covers crossings where the unit must stay equivalent, from the
identity case (test registry) to real translations (contracts, pattern
interpretation).

## The shared patterns and their instances

| translation pattern | test registry (identity) | contracts (Lean → Clojure) | pattern interpretation (prose → tokens) |
|---|---|---|---|
| declare-what-is-preserved | register-the-run (what the warrant records) | holder-states-the-claim | declare-the-universe |
| choose-the-equivalence-check | bind-warrant-to-the-diff (hash) | every-entry-has-a-falsifier | compile-guards-exactly |
| bind-to-the-source | bind-warrant-to-the-diff | resolve-pointers-at-head, rebase-by-name | cite-the-source-bytes |
| test-by-reproducing-behaviour | spot-check-at-the-declared-rate | every-entry-has-a-falsifier | reproduce-the-recorded-run |
| declare-what-is-lost | — (identity loses nothing it claims) | declare-the-reductions | put-uncertainty-in-theta |
| route-the-untranslatable | warrant-rides-the-handoff (:unwarranted) | holder-states-the-claim (lagging) | route-the-untranslatable |

## Reading one row: test-by-reproducing-behaviour

- **Test registry.** Nothing is translated, so reproduction is a spot-check:
  run one recorded test and see the record reproduce. zai-8 showed it is not a
  correctness verdict (one passing draw moves P(clean) from 0.5 to 0.526).
- **Contracts.** The WM-11 selection posterior added F where the Lean subtracts
  it. A falsifier test at equal G and habit caught it (futon2 `74a118c5`).
- **Pattern interpretation.** The interpreted buffer-cleaner cascade reproduced
  the recorded run. The interpreted inbox-zero cascade did not match the
  implementation, and the mismatch named three implementation defects rather
  than an interpretation error.

## Gaps kept visible

- The identity case has nothing to declare as lost, so its cell is empty.
- Contracts reuse `holder-states-the-claim` in two rows: the holder both states
  the preserved claim and marks a lagging, partly untranslatable one.
- Pattern interpretation is still a proposal (claude-4's D1–D8, not approved);
  its patterns rest on three worked instances
  (`futon2/holes/labs/wm-contract/WORKED-INSTANCES-pattern-interpretation-2026-09-16.md`).

## Status

Drafts, 2026-09-16 (claude-7), written so the working knowledge from the
day's contract emissions and worked interpretations is in the library rather
than in one session.
