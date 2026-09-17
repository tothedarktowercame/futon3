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
