# PUR: Layer 0 Durability Design (Exemplar)

## Metadata
- **Date**: 2026-02-07
- **Agent**: claude
- **Module**: core/xtdb.clj
- **Pattern**: futon-theory/durability-first + storage/durability-first
- **PSR Reference**: psr/2026-02-07__core-xtdb__storage-durability-first.md

## Actions Taken

1. Analyzed pattern requirements from futon-theory/durability-first
2. Mapped to concrete implementation via storage/durability-first
3. Identified tension resolution from storage/durability-throughput-gate
4. Designed module interface:
   - `tx-sync!` as single write chokepoint
   - Returns `{:tx-id <id>}` on success
   - Throws with layer-0 error context on failure
5. Documented in module header with pattern references

## Outcome

**Success** - Design complete, ready for implementation.

## Expected vs Actual

- **Expected**: Pattern would provide clear design constraints
- **Actual**: Pattern + tension pattern together gave complete design
- **Surprise**: Tension pattern was more useful than expected for
  understanding the tradeoff (throughput vs durability)

## Prediction Error

**Low** - Design followed pattern guidance closely. The main learning was
that tension patterns are as valuable as enforcement patterns for design.

## Notes

- The combination of theory pattern (abstract) + storage pattern (concrete)
  + tension pattern (tradeoff) provides a complete design framework
- This exemplar demonstrates the PSR→PUR cycle for future sessions
- Module implementation is next milestone (Codex primary)

## Pattern Quality Feedback

- **futon-theory/durability-first**: Useful for establishing invariant,
  but needed storage pattern for implementation specifics
- **storage/durability-first**: Good concrete guidance
- **storage/durability-throughput-gate**: Key for understanding the
  tradeoff and why we gate on commit

## Evidence Links

- PSR: `holes/labs/futon1a/psr/2026-02-07__core-xtdb__storage-durability-first.md`
- Design decision: This document
- Implementation: TODO (core/xtdb.clj)
