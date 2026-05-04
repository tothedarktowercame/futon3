# TN-tensegrity

**Date:** 2026-05-04
**Status:** theoretical, deferred — no current library pattern requires this.
**Cross-refs:**
- `futon3/holes/excursions/E-clause-vocabulary-reshape.sexp` — the rule
  that motivates this note: `:tension → :if + :however`. This note
  records the **one structural case** that rule does not cover.
- `futon3/holes/missions/M-pattern-application-diagnostic.md`
  §"Geometric commitment — tension as differential geometry, not vibes"
  — the live-hypergraph view where tension is a scalar field
  `T : V → ℝ` over hypergraph vertices/edges. Tensegrity is what a
  *vector-valued* tension would look like on the same substrate.
- `futon3/library/futon-theory/structural-tension-as-observation.flexiarg`
  — the upstream pattern that frames tension as the observable signal
  of mission progress; this note generalises the framing to multi-axis
  cases.

## Why this note exists

The clause-vocabulary reshape rule says `:tension → :if + :however` — a
pattern's tension is exactly the gap between what should hold and what
does not. That is **two-pole, scalar**: one IF, one HOWEVER, and the
"tension" is the prose name for their gap.

That works for every pattern in the current library (survey 2026-05-04:
63 `:tension` clauses, all reflowable to a single IF/HOWEVER pair). But
the linear two-pole frame is genuinely a special case. There is a
recognisable structural shape — drawn from Buckminster Fuller's
*tensional integrity* — where a pattern's equilibrium arises from
**three or more simultaneous, mutually-balancing constraints** that
no IF/HOWEVER pair can capture without smuggling the tensegrity into
prose.

This note (a) names the case so we recognise it when it appears,
(b) gives a hypothetical example, and (c) sketches what the pattern
format would extend to if the case becomes load-bearing.

## The structural shape

A tensegrity field is the multi-axis generalisation of the IF/HOWEVER
pair:

| Form | Constraint topology | Equilibrium | Observable |
|---|---|---|---|
| **IF / HOWEVER** | one IF (push), one HOWEVER (counter-push) | scalar gap | scalar field `T : V → ℝ` |
| **Tensegrity field** | three+ constraints, each holding the others in place | vector / tensor gap | vector field `T⃗ : V → ℝⁿ` |

Two diagnostic features distinguish a genuine tensegrity from a pattern
that's merely "complicated":

1. **Removal of any one constraint collapses the structure.** In Fuller's
   physical tensegrities, removing one cable or strut is not a degradation
   — the form ceases to exist. In a tensegrity-shaped pattern, removing
   any one axis would make the others under-constrained: they no longer
   have a unique equilibrium to settle into.
2. **No two-pole projection preserves the equilibrium.** You can describe
   any single axis as an IF/HOWEVER pair, but the projection loses
   information: the other axes' contribution to that axis's equilibrium
   is hidden. A round trip *axis → IF/HOWEVER → axis* does not reproduce
   the original constraint.

A pattern fails diagnostic-1 if the constraints can be partitioned into
independent IF/HOWEVER pairs (then it's a *bundle* of patterns, not a
tensegrity). A pattern fails diagnostic-2 if one axis is dominant and
the others are derived (then it's a single IF/HOWEVER plus refinements,
which the reshape rule already supports as substructure).

## Hypothetical example

A coordination pattern *Live-CAP-equilibrium*, governing how a
distributed system maintains availability under network partition:

> **Conclusion.** Hold a working tradeoff between consistency,
> availability, and partition-tolerance such that operator workload
> stays bounded across normal operation, partition events, and
> consistency-violation events.

Three axes, each pulling on the others:

- `:tensegrity/consistency` — strong-consistency moves degrade
  availability under partition.
- `:tensegrity/availability` — availability-under-partition moves
  degrade consistency.
- `:tensegrity/partition-tolerance` — partition-tolerance moves
  (heart-beating, lease renewal, gossip) consume both consistency
  and availability budget.

No IF/HOWEVER pair captures this honestly. *"IF strong consistency
THEN availability degrades"* is true but doesn't say what
partition-tolerance is doing. Each axis is in tension with **both**
others simultaneously, and the operator's job is to **inhabit the
equilibrium**, not pick one axis and accept loss on another.

The pattern's shape would be:

```
+ TENSEGRITY:
  + axis: consistency
    + pulls-against: availability
    + pulls-against: partition-tolerance
    + slack: <prose for what gives when this axis tightens>
  + axis: availability
    + pulls-against: consistency
    + pulls-against: partition-tolerance
    + slack: <prose>
  + axis: partition-tolerance
    + pulls-against: consistency
    + pulls-against: availability
    + slack: <prose>
  + equilibrium: <prose for the working tradeoff this pattern names>
```

Note the recursive form: each `:axis` plays the IF role for itself;
its `:pulls-against` lines are HOWEVER terms; its `:slack` is what
relaxes when other axes pull. The IF/HOWEVER scalar pair is the
**1-axis special case** of this 3-axis form.

## Mathematical reading

In the live-hypergraph view (M-pattern-application-diagnostic
§"Geometric commitment"), today's tension is a scalar field
`T : V → ℝ`. The tensegrity-shaped pattern would lift that to a
vector field `T⃗ : V → ℝⁿ` where `n` is the number of axes.
Equilibrium is `∇·T⃗ = 0` at the operative vertex (zero divergence:
the vector forces balance). A pattern application would follow the
projection of `-∇T⃗` onto the operator's currently-tracked axis,
*while the other axes continue to contribute to the vertex's local
geometry.*

This is a strict generalisation: setting `n = 1` recovers exactly the
scalar IF/HOWEVER frame. A reshape from tensegrity to IF/HOWEVER is
**lossy** — it picks a single axis and projects — whereas the inverse
embedding is faithful (a 1-axis tensegrity *is* an IF/HOWEVER pair).

The reshape rulebook's `:tension → :if + :however` is therefore
correct for `n = 1` (which today's library is); the rule deliberately
does not cover `n > 1` because no current pattern needs it. Forcing
a hypothetical 3-axis pattern through the rule would silently project
to one axis and lose the other two.

## When this becomes load-bearing

Watch for the diagnostic features above. Likely candidates:

- **Coordination patterns** where three or more roles maintain mutual
  authority (e.g., a triple-key protocol — none of the three holders
  alone can act, and none can be removed without collapsing the form).
- **Active-inference / homeostat patterns** where multiple drives
  compete (`explore / exploit / conserve`, or
  `proprioception / exteroception / interoception`). The
  homeostat-as-agent thesis in M-the-futon-stack is a likely entry
  point.
- **Material/structural patterns** if the futon stack ever encodes
  literal tensegrities (architecture, robotics).
- **Patterns about pattern composition** at the meta level —
  composition-with-siblings already names a 1-axis case; a
  3-cycle of co-constraining patterns would be tensegrity-shaped.

## What this note is not

Not a recommendation to extend the pattern format **now**. The
seven-component canonical frame plus the reshape rulebook covers every
pattern currently in the library. Adding a tensegrity component
prematurely would (a) create a tempting clause name that authors would
reach for whenever a pattern has more than one HOWEVER axis (most of
which are bundled IF/HOWEVER pairs, not genuine tensegrities), and
(b) commit the parser, retrieval, and HIT loop to a vector-valued
slot the substrate isn't yet ready to consume.

When the first genuinely-tensegrity-shaped pattern appears (if it
does), this note is the place to come back to. The reshape rulebook
gets a new rule; the seven-component frame gets either an eighth
component or — more likely — a `:tensegrity` cluster under
`:however` that the parser learns to project onto a vector tension
field. Until then: deferred.

---

*Tech note — companion to the clause-vocabulary reshape rulebook;
records one theoretical case the rulebook does not yet need to handle.*
