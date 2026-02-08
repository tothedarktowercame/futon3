# PUR: Layer 1 Identity Gate

context: implemented UUID enforcement and external-id uniqueness.
pattern: storage/identity-uniqueness

decision: normalize external-id and reject conflicts at validation.

outcome: identity tests cover UUID acceptance, rejection, and conflict.

evidence link: futon1a/test/futon1a/layer1/identity_test.clj
