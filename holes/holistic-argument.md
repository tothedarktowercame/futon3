# The Holistic Argument

*What the futon stack is, why it works, and what would prove it wrong.*

**Supersedes**: `holistic-argument-sketch.md` (pre-split,
pre-three-pillars). The original five support claims (S1–S5) and four
attack relations (A1–A4) are preserved and reorganized around the
three-pillar structure that emerged from M-aif-head (March 2026).
See also: [Three Pillars](https://github.com/tothedarktowercame/futon3c/blob/master/docs/three-pillars.md)
(human-readable summary for non-specialists).

---

## The Claim

Software systems are built by people who hold the design in their
heads. The futon stack is an experiment in building software that
holds its own design — not in documentation that drifts, but in three
interconnected structures that the system itself consults, enforces,
and updates:

1. **The Argument** — a written, falsifiable model of what the system
   is and why it should continue to exist.
2. **The Invariants** — a machine-readable catalogue of structural
   constraints, classified by confidence (operational, candidate,
   liquid).
3. **The Missions** — a seven-phase process that turns tasks into
   evidence, with every decision recorded and every outcome feeding
   back into the Argument and the Invariants.

These three structures correspond to three components of Active
Inference (the mathematical framework behind the design): the
Argument is the generative model, the Invariants are precision-weighted
priors, and the Missions are the action policy. But the ideas are
older than the mathematics — they are beliefs, confidence, and method.

The claim is not "we built an AI." The claim is: **a software system
can participate in its own maintenance if it keeps track of what it
believes, what it's sure of, and what it's doing about it.**

---

## The Evidential Base

### By the numbers (March 2026)

| Measure | Count |
|---------|-------|
| Repositories | 10 (futon0–futon7, plus futon3a/3b/3c) |
| Missions tracked | ~84 |
| Missions complete | ~30 |
| Missions in progress | ~10 |
| Pattern library | 853+ patterns across 49 namespaces |
| Structural law families | 9 operational, 8 candidate |
| Test assertions | 4,300+ across futon3c alone |
| AIF head implementations | 1 (Mission Peripheral), protocol ready for all |

### What runs daily

- **Real-time coordination** (futon3c): multi-agent registry,
  peripheral dispatch, IRC bridging, session management.
- **Deterministic storage** (futon1a): Datascript + XTDB, invariant
  enforcement active for 90+ days.
- **Active Inference engine** (futon2): ant demonstrators, FulabAdapter,
  440+ property tests, now extended with AifHead protocol for peripherals.
- **Evidence landscape**: every mission phase, gate traversal, pattern
  selection, and portfolio inference step emits durable evidence.

---

## The Three Pillars as Support Structure

The original argument sketch had five support claims (S1–S5). They
map naturally onto the three pillars:

### Pillar 1: The Argument (generative model)

**S1: The evidence discipline works.**
853 patterns, PSR/PUR records, evidence replication, gate enforcement.
The M-futon3x-e2e demo proved end-to-end integration: pattern-backed
concept graph → federated search → gate traversal → commercial probes
in one pipeline.

*What's new*: The evidence discipline is no longer just a practice —
it's structurally enforced. The gate pipeline (G5→G0) requires
evidence at each stage. Portfolio Inference reads evidence counts to
recommend actions. AIF heads consult the evidence landscape as part of
their observation loop.

**S5: The reflexive architecture is rare.**
Systems that observe their own development, produce evidence about
what works, and feed that evidence back into decisions are uncommon.
The three-pillar structure makes this reflexivity *explicit and
queryable*, not just an emergent property.

*What's new*: The three-pillars document (`docs/three-pillars.md`) is
the first artifact that explains the reflexive architecture without
requiring Active Inference theory. The M-aif-head mission demonstrated
that the system can verify its own design against its own framework
(the mission's VERIFY phase applied the AIF+ invariants to the
mission's own structure).

### Pillar 2: The Invariants (precision)

**S6: Structural constraints crystallize into enforceable law.**
(New claim — not in the original sketch.)

The structural law inventory contains 9 operational families (always
enforced) and 8 candidate families (observed as pressures, not yet
promoted). This is not static documentation — it's a machine-readable
sexp that the AIF head loads as core.logic pldb facts and consults
before every phase transition.

The crystallization metaphor captures the dynamics: code starts liquid
(it works but structure is implicit), conventions harden into patterns,
patterns harden into tests, tests harden into structural laws. The
inventory tracks where each family sits on this continuum.

Evidence:
- 9 operational families with continuous enforcement
- 8 candidate families with documented pressures
- `check-law` gate in `validate-phase-advance` — the system can refuse
  a transition that violates a structural law, including transitions
  initiated by the human operator ("I'm sorry, Joe")
- The "every peripheral has an AIF head" invariant is the first law
  whose enforcement mechanism was produced by the mission that
  identified it (M-aif-head, C9)

### Pillar 3: The Missions (policy)

**S2: The Active Inference framing is generative.**
The same formalism operates at four scales now (was three):
- Ant agents (futon2) — 200-tick grid world
- Portfolio management (futon3c) — 15-channel observation, EFE policy
- Mission peripherals (futon3c) — 10-channel observation, three-tier
  architecture, AIF head
- FutonZero (futon0) — capability observation model (in progress)

The AifHead protocol makes this composable: any peripheral can
implement `observe` + `default-mode` + `check-law` alongside
`select-pattern` + `update-beliefs`. The Mission Peripheral is the
first; the remaining peripherals are mechanical follow-ons.

**S3: Pattern transfer is real.**
MetaCA→ants proved wiring diagram patterns transfer between domains.
Math-informal patterns apply to software reasoning. 13 real-time
coordination patterns apply to any multi-agent system. The mission
methodology itself transfers: the same seven phases apply whether the
task is building an AIF head, proving a Ramsey theory result
(FM-001), or writing documentation.

Evidence: M-aif-head used 19 patterns from 6 library subdirectories
(aif/, realtime/, futon-theory/, agent/, gauntlet/, social/) to
ground its 8 ARGUE decisions. The patterns weren't decorative — they
determined the design.

---

## The Generative Cycle

The original sketch described five steps where the fifth was missing:

1. Work produces proof.
2. Proof produces patterns.
3. Patterns produce coordination.
4. Coordination produces understanding.
5. Understanding produces argument. ← *was missing*

**Step 5 is no longer missing.** Three things close it:

- **The three-pillars document** is a human-readable self-argument —
  the system explaining itself without requiring domain expertise.
- **AIF heads** make the argument *computable* — each peripheral
  observes its environment, checks constraints, and selects actions
  against the structural law inventory (which IS the argument's
  precision landscape).
- **The VERIFY phase of M-aif-head** demonstrated self-application:
  the mission verified its own design against its own AIF+ framework.
  The system argued for its own next steps using its own tools.

The cycle is now:

```
work → evidence → patterns → coordination → self-representation
  ↑                                                    │
  └────────── argument (three pillars) ◄───────────────┘
```

---

## Attack Relations (why it might not work)

**A1: Complexity cost.**
Ten repos, ~84 missions, 853 patterns. The system may be too complex
for one person to maintain.

*Counter (updated)*: The three-pillar structure is itself a complexity
management tool. The Argument says what matters. The Invariants say
what's settled. The Missions say what to do next. Portfolio Inference
ranks by expected free energy, not gut feeling. CYDER (M-cyder, in
progress) will make operational complexity observable. And the
completed mission count (~30) demonstrates that the process produces
finished work, not just complexity.

**A2: The solo-developer bottleneck.**
Everything flows through Joe. The self-improvement loop is designed to
reduce this but isn't yet autonomous enough.

*Counter (updated)*: The M-aif-head mission was executed by Joe +
claude-1 in a two-hour pairing session. The futonic mission
methodology makes agent contribution *structurally possible* — agents
can execute scoped handoffs (H-1 through H-9) with clear inputs,
outputs, and verification criteria. The bottleneck is real but
narrowing: from "only Joe can do anything" to "Joe sets direction,
agents execute structure."

**A3: The commercialisation gap.**
No revenue flows yet. The landscape intelligence shows where demand
exists but hasn't converted.

*Counter (unchanged)*: The landscape intelligence itself is commercially
viable. The pattern discipline + evidence architecture is the kind of
infrastructure enterprise developer tooling pays for. The three-pillars
document is the first artifact that could explain the value proposition
to a non-technical audience.

**A4: The explanation problem.**
The system is hard to explain.

*Counter (updated)*: This was the hardest attack relation and it's the
one that has moved the most. The three-pillars document explains the
core ideas without AIF theory, computer science, or jargon. The
mission landscape appendix shows it's not theory. The M-aif-head
mission link provides a concrete, public example. The explanation
problem is not solved, but it's no longer the blank wall it was.

---

## Falsifiability Conditions

The Argument is not a marketing document. Here is what would prove it
wrong:

**F1: Evidence discipline produces no measurable benefit.**
If missions with full PSR/PUR/PAR discipline take longer and produce
worse code than ad-hoc development, the discipline is overhead, not
infrastructure.

**F2: Structural laws don't prevent real failures.**
If the invariant catalogue grows but violations still cause production
incidents at the same rate, the precision metaphor is decorative.

**F3: The cycle doesn't close.**
If evidence never feeds back into better pattern selection — if
portfolio inference recommends random actions, if AIF heads ignore
their observations — the reflexive architecture is a Rube Goldberg
machine.

**F4: No one else can use it.**
If the system is legible only to its creator and the agents trained on
its conventions, the "rare reflexive architecture" claim is really
"idiosyncratic personal tool."

**F5: Complexity grows faster than capability.**
If each new mission increases maintenance burden more than it increases
the system's ability to maintain itself, the organism metaphor fails —
it's growing tumours, not organs.

---

## What's Next

The adjacent possible, ordered by what enables what:

1. **Structural law enforcement** (M-structural-law, in progress) —
   promoting candidate invariants to operational status with
   evidence-gated crystallization.

2. **AIF head rollout** (Phase 2 of M-aif-head) — equipping remaining
   peripherals (chat, explore, edit, deploy, reflect, mentor, proof,
   discipline) with AIF heads. Mechanical given the protocol and
   reference implementation.

3. **FutonZero** (M-futonzero, planned) — the capability observation
   model that reads longitudinal trends in the Joe peripheral's
   behavioral traces as learning evidence. This is where the system
   starts to model its own improvement over time.

4. **The queryable argument** — this document as a living AIF+ graph
   in Arxana, where claims are nodes, evidence entries are edges, and
   the graph updates as new evidence arrives. M-self-representing-stack
   built the infrastructure; what's needed is the argument schema.
