# F11: plan for an evidence-producing find

2026-09-09, codex-17, discovery-only packet for claude-1 review.
Agency packet: `invoke-1788926335898-15962-d9ef0d29`.
Read basis: futon3 `04f5ee1721a58811ecc9f6a1d738557dca846087`;
choices file last changed in futon2 `b085c7ba7766947a1204a1dd63e96f9d6ceb13f2`.
This note changes no code, registry, pattern, or Lean source.

## Proposed implementation and its limit

Extend the existing `find-snatch/find -> find-organise/find` call path to
produce query-bound receipt evidence and a complete candidate evaluation
record. Exercise it through fresh deterministic Snatch playouts over all
24 committed patterns. A new evidence runner consumes those results and
compares the open receipt and F4 readings; it is not a second finder and must
not replay saved selections. The comparison report is the next-step input to
the existing choices, not a ruling or a claim that the opaque Lean function
has become executable.

There is already useful machinery, so the implementation work is primarily
instrumenting its inputs and receipts and enforcing the documented receipt
ownership boundary. The important new evidence is what each clause evaluation
actually did, including exclusions, bound to the state and source it read.
Another run emitting only receipt-presence booleans would not satisfy the
[refusal-becomes-pattern warrant](../library/process-coherence/refusal-becomes-pattern.flexiarg).

## Current state and sources

The [F11 worklist](../../futon2/holes/labs/wm-contract/worklist.edn) records:

- Slice 10 reconciled the old 18-pattern fixture with the committed 24-pattern
  library, including independent name round-trip and refusal controls. Do not
  reopen that repair or overwrite its pinned fixture casually.
- Slice 11 supplied the written warrant: implement, gather discriminating
  evidence, then revisit the dependent choices.
- Slice 12b records application of the revised signature in `ae9dc5b58e` and
  the reviewed compatibility repair `65d15a8d`; the evidence-producing
  implementation is still the next slice.

[C590](../../futon2/holes/labs/wm-contract/C590-F11-find-signature-draft.md)
describes the applied interface, subject to slice 12b's recorded naming and
`List.IsChain` adaptations. Its route is `structuredAntecedent`; its query
carries a tension and `fires`; its receipts expose clause kind, acknowledged
clause, route, as-of, and text-or-authored-descent citation. These are carrier
and law declarations, not evidence that arbitrary runtime receipt contents
are true. F4 remains exclusion of a repository member, not antecedent soundness.

The three currently undecided dependent entries in
[aif-equations.edn](../../futon2/holes/labs/wm-contract/aif-equations.edn) are
`:find-f2-receipt-carrier`, `:find-f4-reading`, and `:find-f3-citation-field`.
All remain `:observed-not-decided`. `:find-sorry` is already `:decided` under
Joe's refusal-becomes-pattern ruling. Preserve these actual states, including
the existing ruling; do not copy historical prose saying all entries remain
undecided. No registry write belongs to this packet or its proposed experiment.

Other sources: [C562, receipt carrier](../../futon2/holes/labs/wm-contract/C562-F11-receipt-carrier.md),
[C563, F4 readings](../../futon2/holes/labs/wm-contract/C563-F11-f4-reading.md),
[C567, citation field](../../futon2/holes/labs/wm-contract/C567-F11-citation-field.md),
and [P-validated-R5 laws](../../futon2/holes/problems/P-validated-R5.md), lines 484-488.

## Reuse, additions, and concrete consumer

| Component | Reuse | Proposed addition |
|---|---|---|
| `checks/find_organise.clj` | `read-repository`, authored clause extraction, `warrant`, repository-contained selection in `find` | Reject caller attempts to overwrite core receipt fields; support query-bound evidence as explicit extension data. |
| `checks/find_snatch.clj` | Qualified-ID adapter, six scenario order, declared zero-mass table, `find`, existing drift/law checks | An explicit evidence-enabled call returning the same selected set plus per-pattern evaluation data and query-bound receipts. Existing caller delegates to this implementation; no copied finder. |
| `checks/playout_snatch.clj` | Existing executable interpretations, `fires?`, `play`, and `pi-patterns` | No semantic change. The evidence collector observes the policy callback's actual state and returns the existing policy result. |
| Proposed `checks/find_snatch_evidence.clj` | Calls the instrumented Snatch entry at each `play` callback | Pins inputs, writes a new evidence artifact, checks receipt content, emits the choice comparison sheet and negative-control results. |

The concrete consumer is the new evidence runner's callback passed to
`playout-snatch/play` (existing producer at lines 310-354). It calls the
instrumented `find-snatch/find` path (current call at lines 56-66), validates
and records its output, then returns `playout-snatch/pi-patterns state patterns`.
The existing scenario observer at `find_snatch.clj:89-121` supplies the pattern
to reuse. The run is fresh computation from treatment/disposition/round-count
inputs; saved `:find` results, actions, and outcomes are comparison targets,
never inputs to selection. This is an executable experimental consumer, not
yet a live War Machine deployment.

Proposed new outputs: `checks/find-snatch-evidence.edn` and
`checks/find-snatch-choice-evidence.edn`. Keep `checks/find-snatch.edn` and its
closed Lean row certificates unchanged unless an independently reviewed
fixture migration is required. New artifacts carry their own schema and basis.

## Concrete FindQuery and declared domain

Use `P =` the 24 qualified `:snatch/...` IDs read from the committed Snatch
library; do not reduce the repository to the runner's IDs. Before running,
compare disk paths and hashes against Git's committed files, pin the sorted
24-name population and library blob hashes, and refuse unexplained additions,
missing files, or modified source. Read it once for the run and check the
basis again afterward. Namespace-level cached repositories must be refreshed
by starting the evidence runner in its own process, not by loading into a
shared JVM.

`State` is the complete state passed by `play` to its policy callback. The
query's tension describes the next P1 move at the recorded treatment and
round, subject to that treatment's permitted actions and the current observed
state. Name the proposed query clauses independently of pattern IDs, e.g.
available resources, observed prior response, repair opportunity, and terminal
round. Before running, supply a reviewed interpretation table mapping these
query clauses to the existing predicates and authored IF/HOWEVER spans; do not
derive the expected acknowledged clause from the receipt being tested.

The executable `q.fires` is the existing P1 antecedent interpretation of each
qualified ID, reflected as a Boolean. Do not introduce similarity thresholds,
new routing alternatives, or new pattern meanings. Record `want`/`however`
as the experiment's declared propositions with source-backed interpretations;
do not claim these labels themselves prove a Lean proposition.

Measured discovery: there are **24 library patterns and 20 runner entries**.
The four without a runner entry are `have-a-temperament`,
`widen-the-cascade-only-on-evidence`, `promote-the-remedy-before-the-exit`, and
`grim-cuts-the-cascade-and-never-widens-it`. The existing finder omits them.
The new record must distinguish `no-executable-interpretation` from
`if-false`, `however-false`, and the P2/other-grain exclusions. Do not label
missing interpretations as proved zero mass or silently claim all 24 have
executable meanings. Interpreting those four is separate authoring work if
the acceptance requires it; the proposed run quantifies over all 24 candidates
but witnesses only the declared executable P1 domain.

Run all six existing scenarios, with `g4/snatcher` rounds 1 and 2 as the
first inspected pair. A read-only fresh playout during this discovery showed:

| Query | Observations | Selected patterns (local IDs) |
|---|---|---|
| g4/snatcher round 1 | tokens 10; not snatched; no known disposition | ask-for-surplus-not-surrender, exchange-when-both-sides-gain, probe-before-committing |
| g4/snatcher round 2 | tokens 9; snatched; disposition known; seized 1 | a-free-mark-is-always-worth-assigning, ask-for-surplus-not-surrender, consult-the-remedy-before-exiting, exchange-when-both-sides-gain |

At round 2 `re-enter-after-observed-repair` has false IF and false HOWEVER;
`probe-before-committing` has false IF and true HOWEVER. The declared
scenario zero-mass member is `forced-play-needs-a-loss-floor`, a different
exclusion. These are concrete sources of richer exclusion evidence, without
inventing new outcomes or changing the policy. Nil predicate results must be
recorded as such alongside their Boolean interpretation, not described as a
measured missing-world-value diagnosis without further evidence.

## Receipt and evaluation evidence

Produce one candidate evaluation per repository member, partitioned exactly
into selected, evaluated-excluded, and uninterpreted/out-of-domain. Each
evaluation names the query, full input-state digest, executable interpretation
source, IF/HOWEVER result or explicit absence, and applicable authored spans.
The selected set must equal the instrumented evaluation's firing set and the
existing caller's selection on the same state. Predicates are pure in this
runner; capture their actual evaluations once rather than execute a second,
potentially divergent interpretation merely for the receipt.

Each selected member has a receipt with:

- Acknowledged **query clause**, clause kind, and matching authored clause
  citation. Where both IF and HOWEVER contribute, preserve both evaluations;
  one typed receipt may carry a nonempty clause collection in the parameterized
  Clause carrier, with a declared primary kind. Review this concrete encoding
  before implementation; it is an experimental serialization, not a ruling
  that selects a canonical carrier arm.
- Route `:structured-antecedent` mapped explicitly to `structuredAntecedent`.
  Route diversity is **not** an aim: the applied interface has only this route.
- As-of containing pinned library/runner identity and query identity
  (scenario, round, state digest). Store run wall-clock time separately, once;
  reproducibility compares against the same captured run inputs, not a newly
  invented timestamp. Temporal provenance must not be inferred from a pattern
  ID or attached only at fixture level.
- A `patternText` citation with file, blob/content digest, span, and exact text,
  validated against the selected pattern's committed source. Reuse `warrant`;
  do not make up an authored descent or use a numeric match as its warrant.

Receipt content checks re-read the pinned source and independently supplied
query expectations. Score-only citations, other-pattern citations, absent
provenance, and clause-attribution mismatches must be distinguishable failures.
`Receipt.nonSelfCertifying := True` at the typed carrier does not perform this
runtime truth check. No claim of universal Lean conformance follows from this
finite experimental serialization.

## The generic merge overwrite defect must be repaired first

`find_organise.clj:245-251` merges callback data after its core receipt map, so
`:receipt` can replace `:route`, `:warrant`, or `:if`. Its docstring promises
otherwise. `find_snatch.clj:62-65` happens to add only `:however` and
`:state-fields`, so its current use is benign; fixing the caller alone would
leave the generic defect intact.

Propose a small generic repair: reject any extension that supplies a reserved
core key, with a typed collision naming the pattern and keys. Do not silently
drop the callback fields, reverse merge precedence, or special-case Snatch.
Declare ownership of new evidence fields explicitly as they are introduced.
Preserve legitimate query-supplied route values in the generic API; other
consumers use `:cue-citation` and are not instances of this restricted Snatch
FindRoute experiment.

The call-site survey found consumers in `construct_cascade`,
`construct_open_cascade`, `construct_retrodiction_cascade`,
`construct_alfworld_cascade`, `construct_zaif_cascade`,
`construct_ants_cascade`, and `learn_edge_weights`, plus Snatch.
The constructor family uses `construct-cascade/receipt-for`, which adds
`:match` and `:acknowledges`. Re-enumerate these consumers during implementation
and test their actual extension shapes before changing the shared boundary.
Add targeted collision tests for each reserved key and a valid-extension case.

## How the run informs the choices without deciding them

| Choice / question | Evidence to emit | What must remain unclaimed |
|---|---|---|
| F2 presence/Prop fields versus data | On actual selected receipts, compare the full record with its presence-only projection. A controlled copy with the acknowledged query clause changed retains receipt presence but must fail independent content checks. A separate cross-round as-of substitution must fail query binding while retaining the right pattern citation. | That every data field pays its storage or modeling cost, or that one serialization is canonically required. Route remains constant. |
| F2 row-carrier versus receipt data / external checker | Record what each projection preserves and loses, especially per-query provenance and multiple clause acknowledgments. Have the evidence validator consume those fields, so their usefulness is measured at a real consumer. | Editing Lean's receipt carrier, adopting a choice arm, or treating existing shallow checks as complete content checks. |
| F3 citation versus acknowledged clause | Change only the clause attribution in one diagnostic copy and only the cited pattern in another. Emit a two-check matrix demonstrating whether the independent expectation and citation checks actually separate them. | A synthetic fault is not an observed live misattribution. Label all mutated copies as controls. |
| F4 A: some exclusion; B: scenario-designated exclusion; C: external designation | Per query, record all evaluated omissions and their reasons separately from the pre-existing scenario designation. Evaluate A-at-this-input, B-at-recorded-scenario, and C-at-a-declared-external-set separately. Keep uninterpreted members out of semantic zero-mass claims. | Finite observations prove neither A nor C's universal quantifiers, and omitted members are not automatically a normative designation. |
| F4 distinguishing example | On the same full 24-pattern g4 record, a controlled copy that adds the B-designated member but still excludes another evaluated member leaves A-at-input true and makes B false. For C, a reviewer pins a nonempty extra designation from independently evaluated IF/HOWEVER exclusions **before** selection; a separate control admits that extra member while retaining B's exclusion. | No production selection is changed for this comparison. A data-supported candidate designation is evidence for review, not the chosen universal meaning of zero mass. An empty C designation is reported vacuous and cannot earn discrimination. |

The primary fresh runs must satisfy F1, receipt-content/citation checks, and
the applied F4 exclusion law at each tested query. The diagnostic mutations
are refused examples, never accepted runs. If the same 24-pattern experiment
does not yield a difference in a proposed comparison, report it as
`not-distinguished-on-this-record`. Do not expand or corrupt the run until it
produces the desired verdict. No arbitrary extra member is dropped merely to
make A hold; if the genuine selection exhausts the repository, stop and report
the law conflict. A singleton or other changed repository would be an explicit
off-domain challenge, not a way to weaken the accepted 24-pattern experiment.

Before dispatch, the reviewer should approve a small comparison manifest with
the expected distinguishing cases, their independent bases, and exact
refutation conditions. At least one comparison must distinguish on a
nonempty selected run and its explicitly labeled controls. If none can, the
warrant's counterfactual refuses the experiment. The plan anticipates clause
and query-provenance discrimination and the located F4 controls above; it does
not promise that they settle the operator's preferred interpretation.

## Suggested implementation packets and acceptance

1. Repair reserved-field collisions in the shared finder. Tests must reject
   overwrite attempts while preserving current successful callers. Review
   this separately from the richer experimental interface.
2. Review and pin the query/interpretation/comparison manifest, then add the
   evidence-enabled Snatch adapter and evidence runner. Require complete
   24-member accounting, unchanged genuine selections, current citations,
   explicit unknowns, typed absence, and independent content validation.
3. Run the six scenarios and the declared controls; emit the comparison sheet
   as the choices' NEXT STEPS. Author and reviewer differ. The reviewer decides
   whether the evidence supports, modifies, or refuses the warrant; only a
   separately authorized choice update changes any registry state.

Use clj-kondo and futon4 check-parens on changed Clojure/EDN, focused tests for
the changed behavior, and the existing find drift/F1-F4 negative modes. Run
relevant caller checks without rewriting their pinned outputs unintentionally.
Show artifact reproducibility against frozen inputs and preserve the committed
legacy fixture. No live JVM load, outward action, mathlib4 edit, or new global
qualification is part of these proposed runtime packets. Any required Lean
transcription/qualification is a subsequent reviewed handoff, respecting U87
ownership and source-receipt freshness.

## Discovery validation

Read the full F11 trail and the cited warrant/interface/choice records; checked
the existing generic and Snatch call sites. Counted 24 committed files with
`git ls-files`, and independently measured the runtime reader's 24 patterns,
20 runner entries, and four missing interpretations. A read-only g4 playout
using the existing policy produced the two-round findings above, exit 0. An
initial diagnostic command had an unmatched delimiter; the corrected command
exited 0. No runner output file was regenerated. Local note links and staged
whitespace are checked; no executable change or implementation test is claimed.
