# task-selection/ — how tasks have actually been selected

Joe, 2026-10-28: the outer loop selects tasks, and a policy for selecting
tasks is a design-pattern cascade, evidenced by how work was actually selected
(`cascades/select-tasks-from-cascade-live`). These patterns are the pieces
those cascades are made from. Each one is a reason for selecting work that the
record shows being used, with the tension it resolves and quoted witnesses.
None was brainstormed: the evidence catalogue they were written from is
`~/notes/selection-patterns/witnesses-2026-10-28.md` (about 100 witnesses from
`futon*/holes` and Cascade Live, `GET /api/alpha/cascade-real/graph`).

## The patterns, by what they do to the field

A selection cascade starts from the field of candidate tasks. Each pattern
does one of five things to it, and carries an `@guard` (what it needs to
read) and an `@effect` (what it changes), so that order between patterns
follows from what each needs, as in
`cascade-construction/order-by-what-each-step-needs`.

| role | pattern | tension it resolves | witnesses (catalogue kind) |
|---|---|---|---|
| source | the-operator-asked | do every ask now vs pass it through the gates | K10 |
| source | work-that-pays | revenue vs internal leverage | K11 |
| source | hold-with-a-rearm-condition | forget held work vs re-ask it every report | K3, K15 |
| gate | repair-goes-to-the-front | throughput vs building on a defect | K2, K17 |
| gate | spend-what-the-seats-have | the planned author vs the usage available | K1 |
| gate | only-what-can-start-now | most wanted vs can start | K4 |
| gate | end-in-something-shown | long productive-looking runs vs verifiable results | K12 |
| order | what-others-wait-on-first | the plan's order vs the predecessor many need | K4, K5 |
| order | prepare-the-ground | frees nothing now vs shapes a later build | K13 |
| order | take-the-cheap-win | cheap and safe vs leverage | K6 |
| order | finish-what-is-nearly-done | importance scores vs resolvedness | K14 |
| order | take-the-hard-one-to-learn | cheap pieces vs interdependent whole | K8 |
| order | the-date-decides | leverage vs an external date | K16 |
| reshape | halfway-house | want it vs too hard now | K7 |
| reshape | find-out-before-building | build now vs design depends on the unobserved | K9 |
| reshape | bound-the-bet | uncertain value vs unbounded exploration | K19 |
| reshape | write-it-down-instead | do it now (scope) vs lose it | K18 |
| compose | a-portfolio-not-a-winner | one score vs mixed real picks | cross-cutting |
| compose | offer-a-menu-with-a-default | hand back the judgment vs hide alternatives | procedure |

`budgeted-action-selection/mana-gated-work` is an older gate of the same kind
as `spend-what-the-seats-have` (budget, not seat quota).

What the record did not show: work chosen because it is challenging for its
own sake (none), and work chosen to free an idle agent (only parallelism and
seat accounting). These are left out.

## Selection cascades in the record

A selection policy is an ordered composition: sources, then gates, then
orderings, with reshapes applied to particular candidates. The record holds
these compositions; each is a candidate selection policy, not a proposal.

1. **Giraffe order** (built, `futon3c/src/futon3c/giraffe/pass.clj:300-322`):
   only-what-can-start-now → what-others-wait-on-first → prepare-the-ground
   (cone) → source order. Witness: M-giraffe ARGUE, "prefer tasks that can be
   done now and tasks that unblock others".
2. **Closure under scarce seats** (Joe 2026-09-16, memory
   `wm-closure-strategy-zai-owners`): spend-what-the-seats-have (Codex at 3%,
   Zai owners) → only-what-can-start-now (FUNDAMENTALS.edn roots first) →
   what-others-wait-on-first. "Don't grow the DAG."
3. **The repair click** (Joe 2026-09-22 and 2026-09-30): repair-goes-to-the-front
   → only-what-can-start-now → end-in-something-shown.
4. **The operator's portfolio** (M-action-vocabulary:24-27, E-live-loop-3:1598):
   the-operator-asked → a-portfolio-not-a-winner over take-the-cheap-win,
   take-the-hard-one-to-learn and finish-what-is-nearly-done.
5. **Uncertain design** (M-G-over-cascades:762-769, M-apm-capability-ratchet:395,
   M-xtdb-22x-benchmarking:252): find-out-before-building → bound-the-bet →
   halfway-house.
6. **A dated demonstration** (E-futon-workshop-demo, M-apm-demonstration I.20):
   the-date-decides → end-in-something-shown → halfway-house.

Several cascades share patterns; they differ in which patterns they include
and in which order. The same field gives different tasks under different
cascades, which is why G over them can choose something.

## Scoring a selection cascade (proposed, not built)

Each pattern has an `@predicts` line: what should be observed after a task it
selected is done (the next task does not repeat the failure; ready items rise
by the predicted count; the item closes; the stop rule fires or the bet
succeeds). A cascade's prediction is the conjunction of its patterns'
predictions for the task it picks. That gives the pieces of G for a selection
cascade in the current circumstances:

- **circumstances**, read from Cascade Live and Giraffe: open repair tickets,
  seat usage, dated events in mission documents, held items whose re-arm
  condition now holds, readiness and unblocks per item;
- **pragmatic value**: how far the predicted outcome meets the preferences
  (closures, items unblocked, a demonstrated feature, money);
- **epistemic value**: cascades containing find-out-before-building or
  bound-the-bet are worth more when the design is uncertain, because their
  outcome is informative either way;
- **cost**: seat usage of the task the cascade picks;
- **habit**: how often this cascade's predictions have held in similar
  circumstances, counted from its uses (the existing shadow scorer,
  `strategic_policies.clj:210`, already has a Dirichlet over policy selection
  counts and an outcome model over witnessed transitions,
  `strategic_outcomes.clj:40`, but its policies are lists of missions, not
  cascades).

Different cascades will score differently in the same circumstances: with an
open repair ticket the repair click dominates; with Codex exhausted, closure
under scarce seats; with a date near, the dated demonstration. That is the
claim to test, by scoring each cascade on past circumstances in Cascade Live
and checking which one's pick matches what was actually started next.

## Open

- Every witness has a quote and a place, but most come from mission prose;
  Cascade Live's lineage records the selection act (who clocked on to what)
  and not its reason. Linking each lineage record to the pattern that selected
  it is what would let use counts be taken.
- Giraffe's stage 1 reads holes/tickets only; clusters and needs between
  clusters (stage 2) are not built.
- Giraffe's item record lacks dated transitions, `dropped` with a reason, and
  who must decide an external need; hold-with-a-rearm-condition and
  the-operator-asked need those fields.
- The cone has not been tested against what was started next.
- Nothing here is executable yet: guards and effects are written in prose.
- The catalogue was a keyword search plus reading around each hit, not an
  exhaustive read of all ~300 Motivation / Why now sections. A second pass
  may add witnesses or new kinds.

## Status

Drafts, 2026-10-28 (claude-11, from Joe's statement of the same day). For Joe
to read and correct before anything is built on them.
