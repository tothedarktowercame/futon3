# F12 receipt serialization repair — independent review requested

Fixes the failed deposit reported in d84eedf. No primary execution or recovery
deposit was performed. The original primary bytes, diagnostic bytes, original
manifest and consumed approval remain unchanged.

`parse-receipt` now converts only top-level JSON schema fields into keywords.
Nested mapping keys (absolute paths) and environment keys remain strings.
`deposit-text` checks a full pprint/EDN equality round trip and EOF; reviewed-pair
now checks this serialization before returning a successful record. This catches
invalid serialization at the producer boundary, though callers still must retain
and read back their actual deposited bytes.

Tests now cover the complete real checksum receipt and full diagnostic episode,
including both state and transcript copies, JSON/EDN equality and path-key types.
They also distinguish arbitrary relative/absolute mapping strings from schema
keys. Tests execute diagnostic checksum actions only, never `reviewed-pair`.

## Explicit proposed successor; no automatic re-freeze

`fixtures/f12-preparation/manifest-serialization-successor-proposed.json` is a
new proposed manifest, SHA256:
`2d2c2cc157e2f0b9b9d28877ca6ed089ce00ef4fb6099ec03398d0d23f5d2ef8`.

It retains the old input copies, expected hashes, runtime pins, population,
guards, ranks, score and budgets exactly. Only the adapter source pin changes,
plus explicit supersedes/status/consumed-approval metadata. The original
`manifest.json` is not rewritten and `freeze` was not called. The old approval
is consumed and cannot approve this successor. This packet proposes a source
repair, not a rerun of the observed attempt.

## Separate recovery proposal, qualified without writing recovered evidence

The primary raw file still has SHA256
`7e00351de0433994904c33b7355f5bf13b2873c98ff735c10b5450dab0e3f536`.
The original diagnostic file has SHA256
`2dff3708000161a7d7ca2217739980ea5feb2043ce41a2ba587131170a910dcc`.
Both fail EDN parsing and contain eight damaged mapping headers (four physical
receipts, each repeated in state and transcript). The diagnostic exposure is
confirmed; its original is preserved rather than silently corrected.

`f12_serialization_recovery_test.clj` qualifies this exact proposal in memory:
for only the two named canonical paths, replace the malformed `#:{:home/joe/...`
header with `{"/home/joe/..."`. Leave all value bytes, whitespace, timestamps,
stdout/stderr, ranks, scores, hashes and other fields unchanged. Before touching
anything, assert the exact raw-file hash and eight damaged occurrences. The
inverse replacement must reconstruct the entire original text exactly. Read
one EDN form and require EOF, then verify serialization round trip.

The checks additionally require state/transcript receipt equality, one mapping
entry, a known logical path, physical path equal to cwd/target, logical-to-actual
stdin agreement, stdout naming the physical target, exit 0 and original
stdin/stdout/stderr SHA256s. The primary's outer `:status/:result` envelope is
preserved; scores 1/1 and recorded instance true/parent false are checked.
The first test draft missed that outer envelope and failed; it was corrected
against the retained bytes, not by changing the evidence.

This transformation is byte-reversible and its proposed path interpretation is
corroborated by the independently retained logical stdin fields. It does not
recover arbitrary malformed EDN, validate the old execution universally, or turn
the failed deposit into a new run. No candidate replacement file is published
here. Independent review may authorize a separately named derived recovery
artifact plus transformation provenance; the raw original must remain the
source evidence. Acceptance of that derived artifact is a distinct decision.

## Checks

From futon3:

```sh
bb -cp checks checks/f12_preparation_test.clj
bb -cp checks checks/f12_serialization_recovery_test.clj
/usr/bin/python3 checks/f12_preparation_io_test.py
clj-kondo --lint checks/f12_preparation.clj checks/f12_preparation_test.clj checks/f12_serialization_recovery_test.clj
emacs -Q --batch -l /home/joe/code/futon4/dev/check-parens.el --eval '(arxana-check-parens-cli)' -- checks/f12_preparation.clj checks/f12_preparation_test.clj checks/f12_serialization_recovery_test.clj
```

Pass: 3 Clojure adapter tests/29 assertions; 1 recovery-qualification test/99
assertions; 4 Python tests. Zero lint errors/warnings; parentheses and scoped
diff checks pass. Raw-file hashes and unchanged original manifest verified.
No Lean, registry, worklist, service, or live-data edits.
