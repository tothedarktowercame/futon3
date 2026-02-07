# PSR: storage/durability-first (core/xtdb.clj)

context: futon1a needs a hard durability gate for all writes at Layer 0.
pattern: storage/durability-first

decision: use a synchronous durability confirmation before acknowledging write success.

alternatives:
- async write with later verification (rejected: violates invariant I0)
- best-effort durability with warnings (rejected: not deterministic)

outcome: chosen pattern enforces durable confirmation as a precondition for success.

evidence link: holes/missions/M-futon1a-evidence.md#L86
