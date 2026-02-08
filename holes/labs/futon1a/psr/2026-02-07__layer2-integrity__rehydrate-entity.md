# PSR: Layer 2 Integrity Gate

context: rehydration must be all-or-nothing; entity/relation integrity must hold.
pattern: storage/all-or-nothing-startup

decision: require non-empty entities, validate relation endpoints, and throw on any integrity failure.

alternatives:
- partial startup with warnings (rejected: violates I2)

outcome: rehydrate + entity validators enforce Layer 2 integrity.

evidence link: futon1a/src/futon1a/core/rehydrate.clj
