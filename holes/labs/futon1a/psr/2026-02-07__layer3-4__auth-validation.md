# PSR: Layers 3–4 Auth + Validation

context: authorization and model validation are required before writes.
pattern: storage/error-layer-hierarchy

decision: add penholder gate (403) and model validation gate (400).

alternatives:
- validate after write (rejected: violates hierarchy)

outcome: auth + validation gates implemented as Layer 3 and Layer 4.

evidence link: futon1a/src/futon1a/auth/penholder.clj
