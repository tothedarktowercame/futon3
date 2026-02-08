# PSR: Counter-Ratchet Invariant

context: integrity must detect unexpected count drops.
pattern: futon-theory/counter-ratchet

decision: add counter-ratchet helper and tests; wire errors to Layer 2.

outcome: counter-ratchet throws on non-numeric or dropping counts.

evidence link: futon1a/src/futon1a/core/invariants.clj
