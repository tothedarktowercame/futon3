# PUR: Layer 2 Integrity Gate

context: implemented rehydration and entity/relation integrity checks.
pattern: storage/all-or-nothing-startup

decision: gate startup on validation; relation endpoints must exist.

outcome: tests cover empty entities, invalid relations, and success path.

evidence link: futon1a/test/futon1a/layer2/rehydrate_test.clj
