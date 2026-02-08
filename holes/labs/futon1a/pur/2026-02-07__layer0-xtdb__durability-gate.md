# PUR: Layer 0 XTDB Durability Gate

context: implemented durability gate with XTDB adapter + integration test.
pattern: storage/durability-first

decision: enforce submit-tx! + tx-sync! and validate proof-path completion.

outcome: real XTDB integration test passes and durable write emits tx-id.

evidence link: futon1a/test/futon1a/integration/xtdb_integration_test.clj
