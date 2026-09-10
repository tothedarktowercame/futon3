# F12 attempt 1: accepted derived serialization recovery

Codex-17, 2026-09-10. Independently reviewed c1052e7: adapter tests 3/29,
recovery qualification 1/99, Python tests 4, all pass. The key-preservation
repair is accepted. The proposed successor manifest does not authorize another
primary pair; the original approval remains consumed.

Executed f12_recover_attempt.py twice, obtaining identical derived bytes. It
checks the exact original SHA, the eight occurrences, and byte-exact inverse
reconstruction before writing a separately named artifact. It refuses conflicting
existing output. The original primary and diagnostic deposits remain unchanged.
No checksum action or primary episode is executed by recovery.

Derived artifact: fixtures/f12-preparation/primary-attempt-1-2026-09-10.recovered.edn
SHA256: 54a48cb77be26e5eb7896193e2eb76c4b90c6ed0355da980093e7fa6cb03f075
Source: primary-attempt-1-2026-09-10.edn.txt
SHA256: 7e00351de0433994904c33b7355f5bf13b2873c98ff735c10b5450dab0e3f536

Independent readback parses exactly one form, joins acting orders to actual
transcripts and the comparison row, then recomputes O4 with find-organise:
FSD versus SFD, scores 1/1, contextual-instance O4 true, parent-projected O4
false. Both byte checks matched. Recovered path keys agree with the independently
retained logical/physical stdin and stdout; recovery qualification verifies their
recorded hashes and state/transcript duplication.

The derived record is accepted as recoverable evidence of the one previously
executed pair, not a clean original deposit or new execution. The original
serialization failure stays recorded in F12-primary-attempt-1-review-2026-09-10.md.
This establishes a finite contextual-instance precedence effect only. It does
not establish improved preparation quality, parent-level O4, general organise
conformance, retirement of the implementation refusal, or a compliant RUN4.

The recovered EDN passes clj-kondo (0/0) and check-parens. A first ad-hoc review
command had an unmatched parenthesis and failed before reading the artifact;
corrected review code passed. No evidence was changed to resolve that command
error. Scoped git diff checks passed before commit.
