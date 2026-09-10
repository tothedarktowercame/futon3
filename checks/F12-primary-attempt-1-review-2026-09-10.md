# F12 primary attempt 1: executed, serialization rejected

Codex-17, 2026-09-10. Independently reviewed a6c869b and ran its tests:
Clojure 2 tests/12 assertions and Python 4 tests passed. Manifest SHA256 was
ff3d9873b76ea406b0df0096e43472eb92148a64ccd1f6a07d58acc0209770b5.
The accompanying review authorized one pair, which has now been consumed.

The single reviewed-pair invocation returned completed, instance-o4 true,
parent-o4 false, before/after scores 1/1, and both dispositions both-match.
This is the predicted FSD/SFD contextual order change with unchanged parent
projection. No benefit, general conformance, closure or RUN4 claim follows.

Acceptance STOPPED during artifact readback. command! parses receipt JSON with
recursive keyword conversion, including absolute filesystem paths used as keys
in mapping. pprint emits those as invalid EDN namespaced maps. Independent
clojure.edn/read-string fails with 'Namespaced map must specify a valid namespace';
clj-kondo reports nine errors. check-parens alone passes, illustrating its limit.

Preserved the exact output bytes as
fixtures/f12-preparation/primary-attempt-1-2026-09-10.edn.txt, SHA256
7e00351de0433994904c33b7355f5bf13b2873c98ff735c10b5450dab0e3f536.
Only its extension changed to identify rejected serialization; content was not
repaired or regenerated. No second primary pair ran. The in-memory summary is
an observation, not an accepted machine-readable deposit.

Required repair: retain data-valued mapping keys as strings and test the complete
real receipt/episode serialization round trip. Existing diagnostic output has
the same exposure. Any successor source/manifest needs a new independent review;
this consumed approval is not permission to rerun. If lossless recovery of the
retained bytes is proposed, preserve this raw original and prove the recovery
transformation separately rather than silently replacing the attempt.
