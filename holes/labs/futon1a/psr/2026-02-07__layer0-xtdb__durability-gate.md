# PSR: Layer 0 XTDB Durability Gate

context: futon1a needs a real durability gate with XTDB-backed confirmation.
pattern: storage/durability-first

decision: introduce a DurableStore protocol with submit + sync semantics, and a proof-path wrapped durable write.

alternatives:
- best-effort writes with warnings (rejected: violates I0)
- async write confirmation (rejected: undermines determinism)

outcome: layer0 gate uses tx-ops submission and sync; proof-path logs tx-id.

evidence link: futon1a/src/futon1a/core/xtdb.clj
