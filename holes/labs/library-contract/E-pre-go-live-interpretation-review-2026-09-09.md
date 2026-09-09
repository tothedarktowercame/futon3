# E-pre-go-live: independent library interpretation review

2026-09-09 · codex-10, independent reviewer of codex-17's proposal.
Packet: `invoke-1788979071216-16801-67f90589`.

## Verdict and exact scope

Four candidate roles AGREE; the pin-maintenance boundary DISAGREES as
currently undifferentiated. All five fingerprints and all five cited spans
match. This is a review of the proposal at futon2 `f9433625`, not an
execution-manifest adoption, experiment result, or permission to run.
Disagreement is an actionable finding for the author, not a veto or a new
ruling. No experiment, generator, live load, or Lean build was performed.

Reviewed [brief](/home/joe/code/futon2/holes/labs/wm-contract/runs/E-pre-go-live-experiment-2026-09-09.md),
section “Candidate pattern interpretation, not yet adopted”, against futon3
`8199b9b88d96bc82ac62faf256371e566d3c41d1`. The working pattern files
match the pinned Git objects. The brief contains candidate-role paraphrases,
**not verbatim quotations** of the clauses. Consequently no verbatim-quote
claim can be certified for its table; its line references are correct.
The appendix below supplies the actual text directly from those Git objects.

Each file has IF marker/content at 17/18, HOWEVER at 20/21 and THEN at
23/24. Thus 17–24 is the correct combined span, while a future
`:then-source` citation should name 23–24, not mistake IF at 17 for THEN.
Full-file SHA-256 is not a separate clause hash.

## Per-pattern findings

| Pattern (apparatus/) | Verdict | Interpretation finding |
| --- | --- | --- |
| one-authority-per-question | AGREE | Resolving the owner/gate and refusing competing authorities is a faithful task-local reading. Disagreement must **stop dependent publication**, not merely be mentioned in a published readiness verdict. |
| pin-moves-with-the-population | DISAGREE | The boundary “same slice may span repos/commits when source-Git-SHA emission requires it” does not distinguish the THEN's two timing obligations. It cannot be applied to population re-pins without weakening the explicit same-commit requirement. |
| evidence-to-disposition-once | AGREE | One preparation disposition with consumer-specific answers preserves the distinction between constructing the evidence join and unifying answers. This is agreement with the candidate role, not certification that its full constructor/consumer contract exists. |
| replayable-not-precious | AGREE | Naming preparation as replayable and separating sacred live-run evidence is faithful to the design-time choice. Isolated replay alone does not implement its failure-attribution obligation. |
| done-is-observed-running | AGREE, supporting only | The explicit refusal to call a local probe a standing comparator is correct. Separately reporting activation is an outstanding obligation, not execution of the complete THEN. |

### Required pin interpretation correction

The authored THEN (line 24) says:

> The mutation and its maintenance are one change: move the population and re-pin in the same commit; edit the source and re-emit the attestation in the same slice.

The brief's source-Git-SHA rationale can justify a multi-commit **source
attestation slice** without contradicting that second clause. It does not
justify moving a **pinned population** and its re-pin in different commits.
Before adoption, classify each proposed maintenance action as population
re-pin or source re-attestation and scope the multi-commit wording only to
the latter. Keep the same mutating owner responsible, preserve review and
owning-generator gates, and block the dependent publish while stale.
If an actual population pin also cannot be updated in its mutation commit,
name the conflict for resolution; this review grants no exemption.
The candidate's “no early self-repinning” constraint is sound and remains.

### Obligations to carry into the executable interpretation

These are not additional authored rules or assertions that the proposal
already claims implementation. They prevent a faithful short role from being
mistaken for complete execution of its THEN:

- **Authority:** name the authoritative reader per fact, the permitted
  mutation gate and the comparator for each deliberate second copy. A
  preparation checklist cannot become a competing readiness authority.
- **Disposition:** line 24 requires a small closed enumeration constructed
  once at collection, total consumer dispatch and an exhaustiveness lint.
  A versioned free-form summary is insufficient. Consumers do not re-join
  raw evidence. If blame is represented, retain the typed opaque mechanism
  envelope and one pure versioned blame function; transport codes are not
  blame inputs. State non-applicability explicitly if the task has no blame
  consumer, rather than claiming that branch executed.
- **Replay:** line 24 also requires apparatus-caused marking of failure
  reruns so they do not contaminate measurements. Pin attempt identity,
  restart/partial-state handling, which rounds are primary and how reruns
  are attributed before execution. A changed or missing source refuses the
  frozen comparison; it is not an opportunity to quietly update one arm.
- **Observed-running:** the full THEN requires reviewer-observed live action,
  a manifest and a timed comparator of running/loaded state. None is
  supplied by this read-only preparation experiment. Keep the member
  supporting/unexecuted and record unknown runtime evidence honestly.

The baseline, precise command allowlist, executable guards and effects,
primary-round score definition and reviewed manifest are still future
deliverables in the brief's “Next bounded packet”. This note does not pin
those unspecified choices or replace their independent review.

## Authored relation boundary: an actual reader check

Read-only repository parsing with
[find-organise/read-repository](/home/joe/code/futon3/checks/find_organise.clj:166)
over `library`, `[:apparatus]`, default `#{:why}`, finds **no outgoing
stands-on edges from any of the five selected members**. Therefore their
authored why-based fast-forward graph cannot supply the proposed precedence
change. A reviewed task-local precedence intervention must remain explicitly
an interpretation, not be relabelled an authored dependency edge.

The parser nevertheless extracts three ID-shaped tokens from line-10 prose:
`S1/S2` (authority), `futon3c/holes/T-apm-recurring-failure-end-to-end.md`
(disposition), and `a1-vs-a2/a3` (replay). These are dangling in this
repository, **not authored pattern-id relations**. Do not turn them into
nodes, edges or “unresolved required rules”. Their lexical shape is not
semantic authorship. The brief correctly warns about this exact category.

The files also contain explicit `@see-also` references and `@references`
lists. Under this reader's default, neither supplies `:stands-on`;
`@references` is not an edge directive at all. If a later manifest proposes
another relation basis it must name and review it, not silently count those
references as authored why edges. Preserve exact O3 fast-forward equality
as well as O2 reachability and selected/authored/admission bindings.

## LA2, organise and disposition (f)

The [reconciliation](/home/joe/code/futon3/holes/labs/library-contract/LA2-organise-closure-reconciliation-2026-09-09.md)
distinguishes the running policy-grain mechanism from universal runtime
conformance and from exercise of the ruled carrier's O4. The
[LA1c weakening clause](/home/joe/code/futon3/holes/labs/library-contract/LA1c-restatement.md:434)
requires a running policy-grain rule from LA2; the
[LA2 acceptance/review](/home/joe/code/futon3/holes/labs/library-contract/worklist.edn:1503)
records a broader historical suite. Neither two repaired Snatch sites nor
this new proposal alone releases the later exemplar proviso.
[F12 discovery](/home/joe/code/futon3/checks/F12-O4-witness-discovery.md)
assigns semantic interpretation to independent library authors/reviewers and
explicitly rejects treating a constructed graph as observed member execution.

The brief respects these boundaries: it calls itself no witness, retains
organise/config as external dependencies, forbids self-certification and
live-run authorization, requires genuine command traces and all six O4
fields, and allows unchanged acting order to leave O4 unexercised. It keeps
acting-order measurement on transcript rounds and score on declared primary
rounds, requires the flat-effect control and does not call its proxy canonical G.
A successful finite exemplar would not prove universal implementation
correspondence or select a concrete implementation in place of the ruled
existence statement.

[Disposition (f)](/home/joe/code/futon2/holes/labs/wm-contract/RULINGS-walkthrough-2026-09-08.md:66)
and its [registry ruling](/home/joe/code/futon2/holes/labs/wm-contract/aif-equations.edn:983)
require a new real reusable naturalistic example, with Snatch reproduction
as warm-up/bridge, and a paper account. Today's manually selected
pre-go-live task fits the commissioned purpose; that is **selection**, not
discharge. Do not claim the future outer-loop selector is implemented.

The eventual receipt should say what task and choice were exercised, what
actually fired, which members remained supporting/uninterpreted, which
comparisons were unexercised, and whether the ruled fields reproduce the
observed record. Carry unresolved correspondence/divergence explicitly;
do not convert “seven clauses pass” or “reviewed interpretation” into
“organise implementation validated”. Disposition (f) is the adopted ruling,
not a blanket adoption of the older conditional-release arm (e).
This review supplies no run evidence and no release receipt.

## Verification

- Read futon2 `f9433625` with `git show`; extracted every cited IF/HOWEVER/THEN
  from the pinned futon3 Git objects and checked marker positions.
- Recomputed all five full-file SHA-256 values with `sha256sum`: exact
  matches to the brief, reproduced below. The two supporting fingerprints
  for `checks/find_organise.clj` and `checks/F12-O4-witness-discovery.md`
  also match. Other evolving readiness inputs are not frozen by this review.
- Ran only a read-only `bb -cp checks -e` repository parse: verified the five
  IF/HOWEVER content spans, inspected raw edges, asserted empty outgoing
  stands-on for each. Exit 0. No firing or construction experiment called.
- Checked this note's local links resolve and source-line pointers exist;
  `git diff --check` passes. Markdown only: clj-kondo, check-parens and
  Lean tests are not applicable.
- Note-only commit. Existing sigil-index changes and untracked spider
  receipt are untouched; no pattern, Lean, registry, worklist or data edit.

## Appendix: verbatim authored spans

### one-authority-per-question

Source: [authored pattern](/home/joe/code/futon3/library/apparatus/one-authority-per-question.flexiarg:17).
Full-file SHA-256: `42371c5db7fa2cfbd82688b63aac9ab85518cc0baad7eeb69ccca94aca9bcda1`.

```text
  + IF:
    Two or more places can answer the same question.

  + HOWEVER:
    The second copy agrees most of the time, so it is read as authoritative by whoever finds it first — a seat type is not the sort of thing a reader expects to cross-check. When it finally disagrees, the disagreement propagates as a correction of the correct value.

  + THEN:
    Designate one owner per fact. Route all mutations through the owner's gate (never hand-edit a merged registry; a board writes only through its gated transcription). Keep second copies only as deliberate pins: stale by design, read only by a comparator, disagreement surfacing as an alarm that stops the dependent publish.
```

### pin-moves-with-the-population

Source: [authored pattern](/home/joe/code/futon3/library/apparatus/pin-moves-with-the-population.flexiarg:17).
Full-file SHA-256: `259e32df9a71a6360bc7129cb48680329d9275bfd7c25b5c67c211c25ceb9b79`.

```text
  + IF:
    You are making a change that legitimately moves a pinned population or edits an attested source.

  + HOWEVER:
    The re-pin feels like separate bookkeeping, so it is deferred — and the red then lands on the NEXT lane to publish, who did not cause it, must diagnose it, and learns to distrust the control. Deferred at scale, the debt becomes a stale-attestation backlog whose obvious remedy is a steward role — which is a catch layer for a failure shape (see apparatus/new-failure-class-is-a-design-defect); at-source re-attestation is the one-layer-down fix.

  + THEN:
    The mutation and its maintenance are one change: move the population and re-pin in the same commit; edit the source and re-emit the attestation in the same slice. A gate may enforce it — refuse a change that moves a pinned population without touching its pin.
```

### evidence-to-disposition-once

Source: [authored pattern](/home/joe/code/futon3/library/apparatus/evidence-to-disposition-once.flexiarg:17).
Full-file SHA-256: `9b07bae8e672031979881d6bacbff4926453b949691ffa99751e226b43e6033e`.

```text
  + IF:
    More than one consumer must judge the same event from the same underlying evidence.

  + HOWEVER:
    Each consumer joins the raw authorities itself, ad hoc, in the shape of whatever incident prompted it — and a "single canonical answer" is genuinely impossible, because the consumers' questions differ. (The wrong fix is unifying the answers; the missing thing is unifying the construction.)

  + THEN:
    Reify the join: one constructor builds a disposition value — a small closed enumeration of the authorities' product space (delivered-clean, delivered-then-wrapper-cancelled, process-died-no-submission, submission-without-process-record, …) — at collection time. Consumers pattern-match totally; a lint checks every consumer covers the enum, so a new evidence surface changes one constructor and every consumer either handles the new case or fails loudly. For blame in particular: the fault ORIGIN records what physically happened as a typed, namespaced envelope passed through opaquely (intermediaries may wrap, never translate); blame is one pure, versioned function of the mechanism; transport status codes are never an input to blame.
```

### replayable-not-precious

Source: [authored pattern](/home/joe/code/futon3/library/apparatus/replayable-not-precious.flexiarg:17).
Full-file SHA-256: `44a87d59413bd24b5a86cab526b751b953b846bca9519a0500b926fe4a543546`.

```text
  + IF:
    A unit of work can fail partway, and the question is what to do with its partial state.

  + HOWEVER:
    Re-running feels wasteful (tokens spent) or invalid (the attempt is a datum), so each incident's author preserves the partial state with one more bespoke mechanism — and the sum of those mechanisms becomes most of the apparatus.

  + THEN:
    Decide the tension once, at design time, and say it out loud: which units are replayable (then failures are handled by kill-and-rerun from pinned inputs, marked apparatus-caused so the rerun does not contaminate the measurement) and which are genuinely precious (then the preservation machinery is a costed commitment, not an accident). Where the experiment makes attempts sacred, that sentence belongs in the preregistration.
```

### done-is-observed-running

Source: [authored pattern](/home/joe/code/futon3/library/apparatus/done-is-observed-running.flexiarg:17).
Full-file SHA-256: `301a19b722382997e8e1d455be1c40605ebc987c998aedd5ce5302bcb4a70653`.

```text
  + IF:
    Correctness depends on something being switched on — a loaded namespace, a running supervisor, a wired production caller.

  + HOWEVER:
    The definition of done in use is "committed and reviewed", so the repo and the live system silently diverge; and the occasional audit that catches a gap is a one-off scan, which FEELS like compliance but is true exactly once.

  + THEN:
    Make activation part of done: a packet delivering a mechanism is complete when the reviewer has observed the mechanism act in the live system. Structurally, maintain a manifest of what should be running and one comparator on a timer diffing it against what is running and what is loaded; mismatches are apparatus faults like any other.
```

## Bounded re-review: corrected pin interpretation — 2026-09-09

**AGREE.** codex-10, independent reviewer; packet
`invoke-1788979627150-16822-4bacde13`. Reviewed futon2 commit
`88bb27dc5117bef842724b622cf2920251a8f4ed`, both its corrected candidate-table
row and its [dated response](/home/joe/code/futon2/holes/labs/wm-contract/runs/E-pre-go-live-experiment-2026-09-09.md:119),
against the authored pin THEN reproduced above (lines 23–24 of the pattern).
This supersedes only the pin-timing DISAGREE in this review, not its historical
record or the other four verdicts.

The revision distinctly requires population mutation and membership re-pin in
the **same commit, same mutating owner, with no source-SHA exception**.
Source edit and re-attestation remain in that owner's **same slice**; only
that class may span commits when the attestation requires the source commit's
SHA. The mixed-report row requires both obligations after per-field
classification and explicitly stops on conflict. It cannot relabel a whole
mixed report as source re-attestation. This removes the weakening identified
in the initial review.

The fence is recorded, not merely implied: actual output fields must be
enumerated in the manifest; existing unreviewed changes gain no retrospective
license; stale pins and authority conflicts stop dependent publication rather
than becoming warnings in a published verdict. The addendum also explicitly
excludes the three prose-derived tokens and forbids promoting references or
see-also links into why edges. It retains exact O2/O3 and fast-forward checks.

The interpretation objection is cleared. The addendum itself still requires
the baseline, allowlist, guards/effects, primary score and execution manifest
to be specified and reviewed before execution. This bounded AGREE does not
assert those separate prerequisites are complete, discharge the organise
proviso, or authorize a live run. No experiment was run for this review.

Validation: read the recorded revision with `git show`; checked the cited
response pointer and unchanged authored pattern SHA-256; staged
`git diff --cached --check` passes. Review-note-only change; no code gates
apply and no patterns, registries, worklists or run inputs were edited.
