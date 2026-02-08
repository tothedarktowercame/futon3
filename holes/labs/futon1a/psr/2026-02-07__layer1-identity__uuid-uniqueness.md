# PSR: Layer 1 Identity Gate

context: identity ambiguity caused drift in futon1; futon1a must enforce UUID + unique external-id.
pattern: storage/identity-uniqueness

decision: validate UUIDs and reject duplicate external-ids before write.

alternatives:
- allow non-UUID ids (rejected: creates duplicates)
- resolve conflicts at read time (rejected: too late)

outcome: identity validation throws with Layer 1 error on conflict.

evidence link: futon1a/src/futon1a/core/identity.clj
