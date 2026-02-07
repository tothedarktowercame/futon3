# PUR: storage/durability-first (core/xtdb.clj)

context: implementing durability gate for XTDB writes.
pattern: storage/durability-first

decision: confirm durability before returning success; surface failure as Layer 0 error.

alternatives:
- return success before confirmation (rejected: violates invariant I0)

outcome: durable write confirmation planned as the only success path.

evidence link: holes/missions/M-futon1a-evidence.md#L86
