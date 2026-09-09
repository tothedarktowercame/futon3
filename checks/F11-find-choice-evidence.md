# F11 experimental choice evidence

2026-09-09, codex-17. Packet 3; reviewer manifest at `4182d32`.
This is NEXT-STEPS input for `:find-f2-receipt-carrier`, `:find-f4-reading`,
and `:find-f3-citation-field`. All remain `:observed-not-decided`.

The fresh primary runs passed on six scenarios and 34 visited queries:
816 candidate records, with 96 selected receipts, 278 evaluated exclusions,
272 other-grain records, 34 P2 exclusions, and 136 records without an
executable interpretation. Each query accounts for the same 24 library
members. The g4 round-1/2 input, acknowledgment, and exclusion expectations
matched without adjustment. F1, receipt content/citation/provenance, and
A-at-input plus the recorded B exclusion passed at every query. C passed in
its reviewer-pinned scope; outside that scope it is explicitly vacuous.

| Comparison | Verdict | Observed difference |
|---|---|---|
| F2 full vs presence | distinguished | Clause-only mutation preserves presence but fails attribution/full checks. |
| F2 cross-round as-of | distinguished | Round-1 provenance substituted into the same pattern's round-2 receipt fails query binding alone. |
| F3 two-check matrix | distinguished | Genuine `[true true]`; clause-only `[false true]`; citation-only `[true false]`. |
| F4 A vs B | distinguished | Admitting B diagnostically leaves an evaluated omission, so A stays true while B fails. |
| F4 B vs C | distinguished | Admitting the first pinned C member, re-enter-after-observed-repair, leaves B true while C fails. |

The observations demonstrate that query-clause content and query provenance
can detect faults that receipt presence preserves, and that attribution and
citation ownership are independently checkable. Diagnostic admissions separate
the three exclusion evaluations on this finite record. They neither adopt a
receipt-carrier arm nor settle the universal F4 meanings. All mutations are
labelled controls; the F4 controls are selection projections, explicitly invalid
as primary runs, and do not manufacture receipts. The uninterpreted members
supply no semantic-zero-mass claim.

## Reproduction

From futon3 in a standalone process:

```sh
clojure -Sdeps '{:paths ["checks" "."]}' -M -m find-snatch-choices
```

This comparison driver calls the unchanged, pinned
`find-snatch-evidence/build-evidence`; it contains no additional finder.
It validates the primary record and declared controls, rechecks the real source
pins and manifest after computation, and writes only the two new artifacts.
Its own source SHA256 is recorded in the artifacts as awaiting independent
review; it does not claim to have issued a reviewer pin for itself. The four
reviewer-pinned executable files and the manifest are unchanged.

Two fresh executions produced byte-identical artifacts:

- `find-snatch-evidence.edn`: SHA256 `e8e9bf808f5fbdb9cf126c70fa3760f198834d529fe502df6dbbec74ce3cc3fe`
- `find-snatch-choice-evidence.edn`: SHA256 `424beee25e97a58073cf7cd41252c0db560da28be0de53f31245a90dffbb7684`

Validation: 16 focused/shared tests, 71 assertions, 0 failures/errors;
clj-kondo 0 errors/0 warnings and check-parens OK on both changed Clojure files
and both generated EDN artifacts. Whitespace checks pass. Legacy artifacts,
registries, and mathlib4 were not edited. The initial new-test load caught an
extra closing parenthesis; it was corrected before the passing test run.
