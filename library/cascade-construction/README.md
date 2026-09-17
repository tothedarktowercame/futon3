# cascade-construction/ — how a cascade for a problem gets built

Joe, 2026-09-17: constructing cascades is currently done well by an agent and
invisibly to the model; a library of patterns describing that operation makes
it inspectable and improvable. These patterns record the moves used to build
the inbox-zero and test-registry cascades and their shared layers on
2026-09-16.

`cascades/` (existing) covers deriving a relevant slice of the library at
query time and receipting its use. This directory covers the step before and
after: building a new cascade for a problem, testing it and generalising it.

| move | pattern | where it was used |
|---|---|---|
| ground | read-what-exists-first | inbox-zero gate journal and escalation code; unused registry CLI |
| propose | mine-patterns-from-incidents | one incident per inbox-zero and test-registry pattern |
| propose | borrow-a-sibling-cascade | buffer cleaner → inbox zero → test registry rows |
| model | choose-the-grain-where-state-lives | cycle, repo, handoff × namespace |
| model | order-by-what-each-step-needs | guards carry observe → classify → act → route → settle |
| test | run-it-on-a-real-case | reproduction, implementation divergence, 2-of-64 saving |
| extend | add-a-pattern-when-an-item-fits-no-class | generated-by-role from p4ng + Joe's .aux/.pdf distinction |
| review | separate-construction-from-meaning-review | claude-4's four corrections |
| generalise | lift-when-three-align | hygiene/, translation/ |
| stop | hand-over-when-acting-is-worth-more | inbox zero stopped after one real case |

## These moves as construction-level policies

The present War Machine has no model of construction: retrieval proposes, an
agent interprets, and `organise` builds one up-closure extension that is then
scored. Stage 0's rule "admit an extension iff ΔG < 0" filters proposals but
does not generate them. Written as patterns, the moves above can become the
actions of a construction-level policy:

- **state:** the partial cascade plus beliefs about each candidate pattern,
  interpretation and grain;
- **actions:** the moves in the table (read, propose from an incident, check a
  sibling row, choose grain, order, run on a case, add a pattern, request
  review, lift);
- **what makes a move worth taking:** its pragmatic value (expected improvement
  in the lower-level cascade's G) plus its epistemic value (what it would teach
  about uncertain interpretations or parameters). Running on a real case and
  requesting review are mainly epistemic moves; they are how exploration enters
  without a temperature.

If uses of these moves are receipted (which move, in what context, with what
outcome), the receipts can become counts for a habit prior over construction
moves per domain, which is where learned, context-dependent confidence would
come from. None of this is implemented; the patterns name the actions so the
model can be written against them.

## Status

Drafts, 2026-09-17 (claude-7). Evidence is from one day's construction by one
agent; the moves should be checked against other agents' constructions before
being treated as general.

## Execution kind and confidence

Each pattern carries an `@execution` line (Joe, 2026-09-17: confidence relates
to habit; deterministic code admits high confidence in principle, an LLM query
is more aleatoric). Two quantities are kept apart:

- **how noisy a step is** (θ): deterministic steps can approach θ = 1; LLM
  steps stay below 1 however often they run;
- **how sure we are of that noise level** (the concentration around θ): this
  grows with receipted use for both kinds, and is what habit over moves is
  built from.

| pattern | execution |
|---|---|
| read-what-exists-first | deterministic search + LLM judgement |
| mine-patterns-from-incidents | LLM |
| borrow-a-sibling-cascade | LLM (row walk mechanical once chosen) |
| choose-the-grain-where-state-lives | LLM, human on conflict |
| order-by-what-each-step-needs | deterministic given guards |
| run-it-on-a-real-case | deterministic given state and interpretation |
| add-a-pattern-when-an-item-fits-no-class | deterministic detection, human distinction |
| separate-construction-from-meaning-review | human or independent agent |
| lift-when-three-align | LLM, deterministic citation check |

Turning a step from the second kind into the first (e.g. guard compilation,
once an agent judgement, now D4's rule) is one way construction improves.


## Epistemic scoring and the definition of done (2026-09-17)

Joe: construction is a policy for creating policies; it must be epistemic and it
must have a definition of done, or it becomes a costlier "unknown → halt".

**What a move is worth.** Every pattern now carries `@epistemic-value` (what the
move reveals) and `@done` (when that move has nothing left to reveal). There are
two kinds of information:
- **state information:** facts about the problem found out by observation
  (reading the implementation, running a real case). Lean:
  `mathlib4 DarkTower/WarMachine/EpistemicValue.lean` (3b19f6225e). The acting
  step selects the observation channel, and risk plus ambiguity then includes the
  mutual information the observation gains. Its fixture: checking an unknown fact
  scores G = 0 against ln 2 for doing nothing. P7's form, with one observation
  channel for all actions, gives the check no value (`horizonEFE_eq_of_B`).
- **interpretation and parameter information:** which reading of a pattern is
  right, what grain, what a pattern's θ is (mining, borrowing a sibling, meaning
  review). This is novelty, not state information. It is not in risk plus
  ambiguity and is not yet formalised (it needs a Dirichlet novelty term over
  DirichletLearning). Until it is, these moves' epistemic values are estimates,
  recorded as such.

**When construction stops.** `hand-over-when-acting-is-worth-more`:
- Stop when the best move's expected value (pragmatic plus epistemic, minus
  cost) is no more than the value of acting on the current best family.
- Or stop at a declared budget, or at a stopping observation.
- Unknown facts become check candidates in the family, so they are found out by
  acting, not by constructing further.
- The construction receipt records moves, family, coverage, stop reason and
  budget used.
- Stopping is not target success.

| pattern | epistemic value | done when |
|---|---|---|
| read-what-exists-first | facts found by observation | four sources read or unavailable; disagreements listed |
| mine-patterns-from-incidents | which requirements and guard meanings | new incidents add no requirement |
| borrow-a-sibling-cascade | fit of uncertain rows | every row has a counterpart, gap or reversal |
| choose-the-grain-where-state-lives | whether facts are observable at a grain | one grain, every fact has a method |
| order-by-what-each-step-needs | exposes uncheckable guards as check candidates | every need is produced, observed or a check |
| run-it-on-a-real-case | reproduction; interpretation or defect | one case per distinct predicted outcome |
| add-a-pattern-when-an-item-fits-no-class | whether a class is covered | every item acted on or listed unclassifiable |
| separate-construction-from-meaning-review | semantic claims | each named claim confirmed or corrected |
| lift-when-three-align | whether a reason is shared | runs after targets; never delays enactment |
| hand-over-when-acting-is-worth-more | none of its own | definition of done for construction |
