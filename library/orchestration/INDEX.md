# Orchestration Pattern Library

Patterns that govern **how components are arranged and coordinated** to achieve a goal, distinct from the message protocols themselves (`library/realtime/`) and the capability envelopes that constrain individual components (`library/peripherals/`).

## Scope

An orchestration pattern names a recurring shape in *how multiple parts coordinate over time* — what state they share, how they signal, when they pause, what triggers refinement. The component-level mechanism (a whistle, a flexiarg, a hop) is usually instance-of an existing pattern in `realtime/` or `peripherals/`; the orchestration pattern abstracts over the specific mechanism and names the coordination shape.

Examples of orchestration-shaped questions:
- Where does state live during a multi-turn coordination?
- How does a rule evolve from its own failures?
- When does an excursion close back into its parent mission?
- What signal marks the end of a bounded collaboration?

Examples of NON-orchestration questions (these belong elsewhere):
- What's the message format? → `realtime/`
- What tools does this peripheral grant? → `peripherals/`
- What invariants must this code uphold? → `code-coherence/` or `stack-coherence/`

## Current patterns

- [state-in-substrate-deltas-in-messages](state-in-substrate-deltas-in-messages.flexiarg) — for fast bounded coordination between two components, lift state into a shared substrate and keep messages as deltas
- [rule-evolves-from-its-deferrals](rule-evolves-from-its-deferrals.flexiarg) — when a classifier rule defers as well as accepts, route the defers into an explicit typed channel and refine the rule from cluster shape in that channel

## Provenance

Namespace established 2026-05-25 after the E-pilot-hop-trigger-wiring collab between claude-2 and claude-1 surfaced two coordination shapes that didn't fit cleanly under `realtime/` or `peripherals/`. The session also produced several candidate patterns that were judged too instance-specific to abstract usefully (`scratch-file-collab-rapid-whistle`, `bidirectional-occupancy-binding`); those remain as instances of the abstract patterns above rather than entries in their own right.

## When to add a pattern here

If you find yourself describing a coordination shape using nouns like *substrate*, *channel*, *cadence*, *closure*, *evolution*, *handoff*, *occupancy*, *quorum*, *retry-policy*, *backpressure* — and the existing namespaces don't carry it — this is the right place.

Resist the urge to name an instance as a pattern. Test by asking: "would this pattern apply if the message transport were different, the agents were humans not Claudes, the domain were not software?" If yes to all three, it's an orchestration pattern. If no to any, it's an instance.
