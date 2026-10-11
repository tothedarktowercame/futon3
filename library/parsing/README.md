# parsing/ — reading an artifact, or a request, for what it really requires

From a postmortem (2026-10-11, Joe and claude-11) on a private project: porting
a programming-language implementation into another host language, for a
collaborator whose goal was a lossless round trip from source to typed objects
and back. The project turned out to involve parsing two things: the language, and
the request. These patterns record the shape of the false start and its
correction; the project's details stay private.

## The arc they come from

| stage | what stood in for the real thing |
|---|---|
| map | a causality map of the implementation's compiler passes, built to decide what a port must reproduce |
| comparator | the native implementation as oracle; an inherited source-to-source compiler repaired toward it, fix by fix |
| the reference itself | the implementation's own compiled code executed in the new host, with the native binary kept as the check |

Joe's reading: a precession of simulacra for the real solution. Each stage shed a
layer of representation until the copy collapsed into the original. The same arc
recurred in printing (grammar models, then the language's own pretty-printer,
then the real parser as the check) and in method: we began by adapting the
collaborator's method rather than asking how the language implementation works.
The collaborator's goal was clear throughout.

## Patterns and the translation/ patterns they rest on

| pattern | @why |
|---|---|
| run-the-reference-before-modelling-it | translation/choose-the-equivalence-check (a move is judged by identity, a translation needs a tested relation) |
| a-comparator-points-at-its-own-rehosting | translation/test-by-reproducing-behaviour |
| check-output-with-the-real-parser | translation/test-by-reproducing-behaviour |
| adopt-the-goal-not-the-method | translation/declare-what-is-preserved (the goal is what must be preserved; the method is free) |
| audit-the-route-not-only-the-repairs | translation/revise-the-source-under-pressure |

## What the false start did contribute

The repair period left a native trace of exactly what a minimal case executes,
the habit of comparing every result with native output, much of the runtime
transcription, and fluency in the implementation's intermediate representation.
Those gave the working approach its shape. The switch itself needed none of
them, only the question in run-the-reference-before-modelling-it.

## Gaps kept visible

- Each pattern has one generation; the library's rule wants three before any of
  these becomes shared.
- adopt-the-goal-not-the-method is in tension with
  coordination/intent-to-mission-binding where a mission text states a method;
  the binding should be to the goal and its acceptance test.
- The project's own record is private; evidence here is stated at the level of
  shape and counts, plus public Lean behaviour (leanprover/lean4#15614).
