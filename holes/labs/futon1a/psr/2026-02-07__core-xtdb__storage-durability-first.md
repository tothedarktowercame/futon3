# PSR: Layer 0 Durability Design

## Metadata
- **Date**: 2026-02-07
- **Agent**: claude
- **Module**: core/xtdb.clj
- **Cycle**: 1

## Context

Designing the Layer 0 durability module for futon1a. This is the foundation
layer - all higher invariants depend on reliable persistence. Evidence from
futon1 history (commits 0e2b3a5, 5c51506, 5227d75) shows durability drift
was a recurring bug pattern.

## Pattern Search

Query: "durability persistence write confirm"

### Candidates Considered

1. **futon-theory/durability-first** [I0]
   Persistence is the zeroth invariant: what you save is what you get back.
   Writes return success ONLY after durable storage confirms.

2. **storage/durability-first**
   Concrete storage pattern with XTDB specifics.
   References tension: durability-throughput-gate.

3. **storage/durability-throughput-gate**
   Tension pattern: If you want throughput but also durable truth, gate
   responses on durable commit and treat watchdog ordering as invariant.

4. **futon-theory/proof-path**
   Every change follows auditable chain. PROOF_COMMIT phase requires
   durable confirmation.

## Pattern Chosen

**futon-theory/durability-first** as primary theory constraint, instantiated
via **storage/durability-first** for concrete implementation.

## Rationale

- Theory pattern provides abstract invariant (I0) that constrains design
- Storage pattern provides concrete implementation guidance
- Tension pattern (durability-throughput-gate) informs the tradeoff resolution
- Proof-path ensures durability check is part of the event protocol

## Decision

Design core/xtdb.clj with:
1. Single write chokepoint (`tx-sync!`)
2. Synchronous confirmation before success return
3. Return tx-id as durable proof token
4. Throw on failure, never return false
5. Integrate with proof-path PROOF_COMMIT phase

## Alternatives Rejected

- **Async writes with callback**: Violates "no fire-and-forget" requirement
- **Optimistic return with background sync**: Creates async leak risk
- **Multiple write paths**: Makes debugging harder (violates I4)

## Confidence

**High** - This is well-supported by evidence and aligns with both theory
and storage patterns. The tension resolution (gate on commit) is clear.

## Evidence Links

- Theory: `library/futon-theory/durability-first.flexiarg`
- Storage: `library/storage/durability-first.flexiarg`
- Tension: `library/storage/durability-throughput-gate.flexiarg`
- Git evidence: `holes/missions/M-futon1a-evidence.md#L43`

## Next

Implement core/xtdb.clj, then record PUR with outcome.
