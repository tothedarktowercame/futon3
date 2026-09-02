# `checks/` — derived behavioural witnesses over library edges

## LA7 blind retrodiction constructor

`construct_retrodiction_cascade.clj` constructs five independent cascades over
the whole library from the statement-only packet `retrodiction-items.edn` and
writes `retrodiction-cascade.edn`. Run it from `futon3` with:

    clojure -Sdeps '{:paths ["checks" "."] :deps {babashka/fs {:mvn/version "0.5.25"}}}' \
      -M -m construct-retrodiction-cascade

Its ledger-row cue licence recomputes each statement digest and requires every
cue to occur in that same statement. The packet contains no resolutions, so a
resolution-derived cue cannot pass the licence. The artefact is for a different
agent to compare with completed outcomes; this constructor enacts nothing.

A flexiarg is **abstract** — a production rule at specification grain. Whether a
`@how` edge actually holds is **derived**, not stored: a relation run forward
against stated facts, in the third verification layer of
`futon2.aif.operational-witness` (*interface → structure → **behaviour***).

Run:

    clojure -Sdeps '{:paths ["checks"] :deps {org.clojure/core.logic {:mvn/version "1.0.1"}}}' \
      -M -m how-witness-snatch

Same invocation with `-m how-witness-heartbeat` or `-m how-witness-split-transport`
for the two peripherals edges, `-m how-witness-declare-conditioning`,
`-m how-witness-no-self-certification`, `-m how-witness-scheduled-observer`,
`-m how-witness-status-gated-belief`, `-m how-witness-two-layer-calibration` or
`-m how-witness-off-continuity-null` for the six `aif` ones, and
`-m how-witness-delivery-vs-practice` for the one `ukrns` edge. That is all nine
attested `@how` edges in the library; every one is priced.

`.clj-kondo/config.edn` teaches clj-kondo that core.logic's `fresh`, `run*` and
`project` bind like `fn`; without it every logic variable in these files reads
as an unresolved symbol and the gate cannot tell a defect from core.logic
syntax. Added 2026-09-01 with worklist item `:L4`.

**Every witness must ship with a mirror** — the same relation asked of a case
where it must *not* hold. A witness that cannot fail is not a witness.

| file | what it does | result |
|---|---|---|
| `how_witness_snatch.clj` | the edge as a **relation** — does it hold? | **ATTESTED**; mirror silent |
| `how_kernel_snatch.clj` | the same edge as a **kernel** — with what spread? | entropy 1.3863 → 1.3121 nats under attestation; mirror zero mass |
| `how_witness_heartbeat.clj` | the **price test** (worklist `:L4`) — the same relational shape over an *attested* edge in `library/peripherals/`, run against futon3c source rather than a game diagram | **NOT ATTESTED** — 0 of 5 obligations carried out by the method; positive control finds 3; mirror silent. Cost written up at `p4ng/empirics-futon/NOTE-the-one-edge-price.md` |
| `how_witness_split_transport.clj` | the price test **generalised** (worklist `:L10` slice 1) — the second attested `@how` edge in `library/peripherals/`, run against the War Machine's markdown / hex-web / VSATARCS renderings | **NOT ATTESTED** — 1 of 5 obligations carried out by the method; positive control finds 3; mirror silent. Cost written up at `p4ng/empirics-futon/NOTE-the-one-edge-price.md` |
| `how_witness_declare_conditioning.clj` | the price test in a **second section** (worklist `:L10` slice 2) — the first attested `@how` edge in `library/aif/`, run against futon2's `M-evaluate-policies` ARGUE exhibit and the E5 conditioned-cascade probe | **NOT ATTESTED** — 0 of 5; positive control finds 5; mirror (a real typed observation vector in futon3c) silent. The method's output *is* read at the site and is flattened to a 400-char sentence before any valuation consumes it — **de-typing at the boundary**, not absence |
| `how_witness_no_self_certification.clj` | the price test over the edge whose two patterns **share a site** (worklist `:L10` slice 3) — the G-SIM calibration arc, run against futon3c's WM-pilot tagger, its two verdict surfaces, and futon0's reward-red-team fixture | **NOT ATTESTED** — 2 of 6; positive control finds 6 of 6 (the widest yet — every obligation is discharged by *something* here); strictness control 3 of 6; mirror (`:scan-as-of`, stored twice and read by nothing) silent. Three obligations belong to the anti-laundering lane, not to window hygiene; the fourth, *untagged never counts*, is enforced by `validity-mask` and **contradicted** by `calibration-report` reading the same tag |
| `how_witness_scheduled_observer.clj` | the price test over an edge whose source pattern **names the verdict it wants gated** (worklist `:L10` slice 4) — R10 live-operation, run against futon2's scheduler lane, grounded-actuation lane, R10 criterion surfaces, recorded cron state and APM tripwire | **NOT ATTESTED** — 0 of 5; positive control finds 4; addressee control (I9 dropped) 1 of 5; mirror (the A5 substrate dial, armed since 2026-07-14 and never once sampled) silent. The dial-move gate the clause asks for **is built and executable** (`grounded-close-errors`) and is installed on the *attempt outcome label*; R10's own operational check reads "Find the schedule + the trace store" |
| `how_witness_status_gated_belief.clj` | the price test over the first edge whose **site is outside the futon stack** (worklist `:L10` slice 5) — the v3 AIF runner's status gate, run against `ukrn-services-simulation` ba27028 | **NOT ATTESTED** — 2 of 6; positive control finds 5 of 6; non-invocation control (I10 dropped) 3 of 6; mirror (the EFE planner's forward model, which fires the same gate on an imagined next state and discards the result) silent. `update-mu` has exactly **two** production callers and both are gate branches, so the `:cited` NEXT-STEPS line is literally satisfied — and the clause it does not reach, *variance grows by sensor-variance per tick*, is **contradicted** by the method's own EWMA, whose zero-residual fixed point is σ²/α |
| `how_witness_two_layer_calibration.clj` | the price test over the edge whose **method is scoped to one of the source's two layers** (worklist `:L10` slice 6) — the FutonZero G-SIM calibration split, run across futon2's two forward models, futon3c's pilot/flight-record/calibration reader, and futon0's reward gate and charter ledger | **NOT ATTESTED** — 1 of 6; positive control finds 6 of 6 (every obligation discharged by *something*, so the overstatement is ownership); plumbing control (I11 dropped) 2 of 6; mirror (`flight-stratification`, the one constant-vs-scaled comparison in code, over a corpus of zero `*.flight.edn` records and with no caller) silent. The dual prediction is computed on every candidate every tick and plumbed through five files; **no verdict anywhere compares it**. The L2 gate is complete, declares itself synthetic, and cannot read the records the canonical reader emits |
| `how_witness_off_continuity_null.clj` | the price test over the edge that **closes a two-hop chain** (worklist `:L10` slice 7) — its source was slice 6's method and its method was slice 3's, so both endpoints arrive with their site already read; run against futon3c's flight-record mask and the two flights of 2026-06-12 | **NOT ATTESTED** — 1 of 6; positive control finds 6 of 6; strictness control (I7 dropped) 2 of 6, tightness control (I13 dropped) 1 of 6, both dropped 3 of 6; mirror (`:begin-at`, the field the mission says the confound is "checkable from", read by one pretty-printer and nothing else) silent. The attestation cites a **gate condition** — *mask-in still requires the null be CLEAN* — and the gate is real: `validity-mask` admitted the one off-continuity null ever flown, whose baseline bracket was **201.5 s against the contrast flight's 18 s**. `settled-window?` tests `begin <= commit` as an ordering and never as a duration, so the half of cleanliness that failed is the half the gate does not read |
| `how_witness_delivery_vs_practice.clj` | the price test over the last of the nine and the only `ukrns` edge (worklist `:L10` slice 8) — the first whose two patterns name **no path at all**, only "the current paper's *training* track" and "the current paper's *indicators* track"; the site is the one document that defines both, futon4's `M-or-training-as-learning-system.md`, plus the simulation that consumes the delivery counts | **NOT ATTESTED** — 1 of 4; positive control finds 4 of 4; qualifier control (I14 dropped) 1 of 3, then-only control (I15 dropped) 1 of 3, so the verdict rests on neither invention; mirror (the MVSG deferral, a conditional claim about an architectural label) silent. The site keeps delivery and impact apart — but by naming the gap as an unmet proof obligation, not by the indicator-capacity partition. Meanwhile the simulation thresholds `t1-to-d` of the delivery count into cells named **Multiplied / Absorbed**, so an institution acquires a practice-outcome label at 100 trainees with no practice observed |

## Why both

`Rel` is copy–discard but **not Markov**: a relation may be partial and lose
mass. A kernel is total. So `run*` answers *which outcomes are possible* and the
kernel answers *how much mass each carries* — the relation supplies the
**support**, the Beta posterior supplies the **mass**.

An unattested edge is Beta(1,1): uniform, maximum entropy, **maximum epistemic
value**. You learn most by testing the method nobody has attested.

`Beta(1,1)` is a **prior**, therefore a stipulation, and is declared in the
source rather than absorbed — per `S-G3` in
`p4ng/empirics-futon/NOTE-a-standard-for-G.md`.

---

## The generic `find` / `organise` path

`find_organise.clj` — worklist `:L6`, P-validated-R5 §3e. The two functions the
War Machine's Lean holes declare (`DarkTower/WarMachine/Holes.lean:264`, `:308`),
over **any** library section rather than over `library/snatch`.

    clojure -Sdeps '{:paths ["checks" "."] :deps {babashka/fs {:mvn/version "0.5.25"}}}' \
      -M -m find-organise

`--negative-f1` … `--negative-o4` mutate one law on a real recorded row and must
be rejected; the exit convention is `0-pass / 1-fail / 2-mutation-slipped`.

The laws are checked in the **narrowed** form the 2026-08-31 scope amendment gave
them: each is a predicate on a recorded row, not a universal over an
implementation that is deliberately refused. F1–F4 run over 40 rows of the pinned
`find-snatch.edn` (the check refuses to proceed if that file's sha256 has moved
off the one the four `findF*` declarations pin). O1–O4 run over six `CascadeDiff`s
built from `snatch-cascade.edn` by `organise` itself, so a generic path that did
not reproduce the recorded cascades would fail rather than sit beside them.

`organise` takes **three** arguments, `Cascade policy → Set P → Repository P →
Cascade P`. The third is the temperament, and it is not decoration: the library
already records two organise policies that the two-argument type cannot tell
apart — `playout_snatch.clj` takes the up-closure under `@why`, while
`wmCascadeDiffFixture` keeps `nodes = selected` and fast-forwards through the
unselected bridge. Both satisfy O1's narrowed form, so which one runs is data.
Over `library/war-room` the difference is visible: dropping its four bridge
patterns turns nine authored edges into five fast-forwarded ones, all of them
landing on `wr-0-organise-without-apparatus` — Joe's *"fast-forward the edges
that didn't fit our current problem"*, off Snatch.

| what it reads | measure |
|---|---|
| `library/snatch` | 24 patterns, 26 authored `@why` edges, acyclic, 1 dangling cross-section target |
| `library/cycle-machine` | 7 patterns, 5 edges, `@why` depth 1 — no bridges, so the two temperaments agree |
| `library/war-room` | 28 patterns, 9 edges, 4 bridges — the temperaments differ |
| `library/ants` | 5 patterns, **0** authored edges — the unorganised control (P-validated-R5 §1b addendum 3) |

The reader is checked against `library_graph_lint.clj`'s own `scan-library` on
every run and the check fails on any disagreement: two readers of one graph is
the defect `find_snatch.clj`'s `representation-mismatches` refuses one level up.

---

## Items — the unit of "one new good thing"

Joe, 2026-08-27: *"the reason the 82 historical items were not good is that they
had no Q, no core logic, no Markov categories… so we need to start with 1 new
good item."*

An **item** is the smallest complete unit. It carries, **before the outcome is
known**:

| part | in `item-s001.edn` |
|---|---|
| **π** — a rule from history to action | `:probe-one-token` — offer 1, and on a snatch never offer again |
| **the hidden state and its prior** | P2's disposition, Beta(1,1), *declared* as a stipulation per S-G3 |
| **Q(o∣π)** — a predicted distribution over a real outcome space | `{O1 0.0, O2 0.5, O3 0.0, O4 0.5}` over the G1 flowchart leaves |
| **a spread** | entropy 0.6931 nats = log 2 |
| **a falsifier** | `O3` — *in* the space, *zero* predicted mass |
| **a derived check** | `score_item.clj`, core.logic over the item's own support |
| **a receipt contract** | `:realised-outcome :posterior :refuted?` |

    clojure -Sdeps '{:paths ["checks" "."] :deps {org.clojure/core.logic {:mvn/version "1.0.1"}}}' \
      -M -m score-item

### What makes an item good

**Some outcome in the space must carry zero predicted mass.** Otherwise no
outcome could refute the prediction — which is the mirror discipline in
distribution form. `S-001` has two such outcomes of four.

**And the other outcomes must move the spread.** `O2` and `O4` each collapse
entropy 0.6931 → 0. A prediction that leaves the posterior where it found it has
taught nothing, whatever it scored.

By that test the 82 War Machine flights are not items: they carry an outcome and
no prediction, so there was never anything they could have refuted. They remain
useful for **fixture design** — which shapes are real — and for nothing else.

---

## The playout, and what it found about the collection

`playout_snatch.clj` — five rounds, three P2 dispositions, two treatments.

    clojure -Sdeps '{:paths ["checks"]}' -M -m playout-snatch

**Finding 1 — the collection is a *design* language, not a *play* language.**
Encoded from the flexiargs' own `IF` clauses, **all six patterns are
design-grain**: *"a rule is proposed…"*, *"you want a defection to have
consequences…"*, *"you must design, compare, or extend a family…"*. None has a
game state as its antecedent. So a playout matches **zero patterns in every
round** — total coverage failure at play grain, and full coverage at design
grain.

That is not a defect in the patterns; it is a fact about what they are for. But
it means **a pattern collection cannot guide play unless someone writes
play-grain patterns**, and it says concretely what R6's "candidate proposer"
would need at library grain: antecedents over states, not over design questions.

**Finding 2 — a first encoding hid this.** The initial run reported *zero*
coverage gaps, because `preserve-the-right-to-abstain` was encoded as
`(contains? p1-actions :abstain)` — true in every round of every treatment. **A
guard true in every situation carries no information**: a dimension with no
singularity on it, inside the very artefact written to study them. The zero was
an artefact of the guard, not a property of the collection.

### Play-grain patterns added (2026-08-27), and what the re-run shows

Five written from the findings above: `probe-before-committing`,
`escalate-only-as-far-as-you-can-lose`, `consult-the-remedy-before-exiting`,
`a-free-mark-is-always-worth-assigning`, `an-unmodelled-response-stops-the-line`.
Two of the five were written **from the playout itself** — the last from the
falsifier firing, the third from remedies going unused.

    treatment / disposition      coverage gaps (of 5 rounds)
    G1 snatcher                  4   ← correct: see below
    G1 sharer                    0
    G1 cautious                  0
    G4 snatcher                  0

**The surviving gap is right, not a hole.** In G1 after a snatch there *is* no
remedy to consult, no acceptance to escalate on, and the disposition is known —
so the collection honestly has nothing to say. A state of nature offers a player
who has been robbed no move but the one the design layer already named. **A
collection that produced advice here would be inventing it.**

**And the trace shows a policy ignoring an applicable pattern.** In G4 rounds
2–5, `consult-the-remedy-before-exiting` and `a-free-mark-is-always-worth-assigning`
are applicable in every round, and the grim-trigger policy abstains anyway. That
is the pattern's own `HOWEVER` — *"a policy written for the state of nature exits
on the first defection"* — **exhibited by the playout that motivated writing it**.
Availability is not use, at play grain.

**Finding 3 — the falsifier fires.** Item `S-001` predicts `{O2 0.5, O4 0.5}`
with `O3` at zero mass. A *cautious* P2 refuses, producing `O3`, and **the
two-disposition model is refuted** — a stated prediction failing, which is the
whole reason for stating it.

### The policy grain — the temperament fired, not written down (`:LA2`)

Before this row `playout_snatch.clj` held its two temperaments as map literals —
`patterns-overrides {}` and `exchange-first-overrides {:exchange-when-both-sides-gain 0}`
— and `library/snatch` held six policy-grain patterns that nothing executed.
Six library files and the behaviour still in the Clojure is the facade
`LA1c-restatement.md` §11 named. What the row builds:

| piece | where |
|---|---|
| `:policy`, a third value of the grain conjunct | the two entries at the end of `collection` |
| `CascadeEdit` — `admit`, `drop-node`, `promote-above`, `set-flag`, `halt` | `admit` … `halt`, `cascade-edit?` |
| the one grain-polymorphic firing loop | `fire`, called by `pattern-policy` (play) and `construct` (policy) — moved to `find_organise.clj` by `:LA3`, which fires the same loop over the library |
| the construction loop of `LA1c` §4.1 | `construct`, over a `CascadeState` from `initial-cascade-state` |

The two maps are now **derived**: `temperament-overrides` fires a temperament
and reports the entries of the resulting precedence field that differ from the
authored one. `snatch/play-the-authored-order-first` emits `halt` and yields
`{}`; `snatch/lead-with-the-exchange-rule` emits one `promote-above` and yields
`{:exchange-when-both-sides-gain 0}`. The equality against the pre-row literals
is checked when the namespace loads, because `derive_q_snatch.clj`,
`ablate_g_snatch.clj` and `find_snatch.clj` all take those maps and a drift would
move their artefacts without anyone running this report. `snatch-cascade.edn`
regenerates byte-identical, so all six scenarios score exactly as before.

Three controls run on every `-main`, and each fails the check rather than
printing a note:

- **grain separation** — no rule of one grain fires in a situation of another,
  both directions.
- **the conjunct deleted** — the grain is forged on a foreign situation, so the
  rest of the guard runs without its conjunct. **2 of 9 rules then throw**
  (`probe-before-committing`, `exchange-when-both-sides-gain`, both on `:tokens`
  being nil); the other seven are silent because the two situation types happen
  to use disjoint keys. So the conjunct carries the separation at play grain and
  is currently redundant at policy grain — worth knowing, and the control fails
  only if *every* rule is silent.
- **library correspondence** — each policy-grain runner entry names a pattern
  `library/snatch` holds, and its `:then-source` line still begins `+ THEN:`.

**Two of the six are executed.** `unexecuted-policy-patterns` names the other
four and the `CascadeEdit` each would need; between them every constructor of the
type has a named claimant. What blocks the four is stated there and is not
effort: nothing carries a cascade between rounds, so grim's monotone flag has
nothing to be monotone over, and the term `widen-the-cascade-only-on-evidence`
would order its admissions by needed a repository to read authored edges off,
which this seven-rule harness does not have. `:LA3` supplies it: see *The
constructor, over the whole library* below.

---

## The constructor, over the whole library (`:LA3`)

`construct_cascade.clj` — worklist `:LA3`, LA1c-restatement.md §4. `:LA2` built
the policy grain and ran it on a seven-rule cascade; what it could not build is
§4.1's loop — *admit one pattern at a time, ordered by a score, and stop on a
rule stated in advance* — because the score had no substrate the laws admit.

    clojure -Sdeps '{:paths ["checks" "."] :deps {babashka/fs {:mvn/version "0.5.25"}}}' \
      -M -m construct-cascade

`decisions.edn :constructor-relevance-substrate` records the arm this row builds:
keep `cascade_construct.py`'s functional form and swap both of its substrates.

| term | refused substrate | what runs here |
|---|---|---|
| relevance | MiniLM cosine (`cascade_construct.py:48-58`) — F3 refuses a receipt that cites the finder's score | `match`: how many clauses of the tension the pattern's IF ∧ HOWEVER acknowledge, each acknowledgement citing a file and a line span |
| connectivity | a phylogeny whose descent relation is substring containment (`pattern_phylogeny.py:22`) — O2 refuses it | `degree`: `ReachOutside` (`Holes.lean:297-300`) restricted to the chosen set, over the authored `@why`/`@how` directives |

**`@why` ∪ `@how` is cyclic** — `@why` points at what a pattern stands on and
`@how` at what carries it out, so as a directed relation the union runs both ways
and fifteen patterns lie on a cycle (`:union-is-cyclic` in the report). It is
therefore never used as a `standsOn`: the prior reads it *undirected*, which is
what "an authored edge **between** p and q" means, and the constructed cascade's
edges come from `fast-forward` over `@why` alone, exactly as `organise`'s do.
`:prior-separation` in the report is the check that the two relations stay apart:
over the budgeted cascade's twenty nodes the `@why` relation gives 5 edges and
the prior's relation would give a different set.

**Two temperaments that differ only in where they stop.** Both carry
`widen-the-cascade-only-on-evidence`; one adds `halt-on-budget`, the other
`halt-at-the-marginal-gain-floor`. All three rules encode one authored THEN,
`library/snatch/widen-the-cascade-only-on-evidence.flexiarg:29-33`, re-read from
disk on every run. `differ-only-in-the-stop` checks that the node lists are equal
once the stop rules are removed, and that they resolve to the same rule maps —
otherwise "different stopping rules, different cascades" would be a claim about
two different constructors.

    widen-to-a-budget                 11 admissions, stop [:budget 20], 20 members
    widen-to-the-marginal-gain-floor    4 admissions, stop [:marginal-gain-floor 1.0], 13 members

This is also the first cascade in the repository with a non-empty `admittedBy`:
`organise`'s two temperaments declare a closure policy and emit no edits, so
O1's three-way union has always been checked with its third term empty.

### The measurement — pre-registered, and it is the row's third clause

*At 8.3% rationale-layer coverage, is the degree term distinguishable from
uniform on a real tension?* The arm is the same construction with `degree` forced
to zero, run on every `-main` so it cannot be dropped when inconvenient. If it
came out negative, the delivery would be the finding that L5/L9 coverage blocks
the constructor.

**It came out positive, and thinly.** Under the budget temperament two of eleven
admissions change when the degree term is removed; under the floor temperament
the real arm admits four patterns and the uniform arm admits **none**, because
with no authored edge no candidate clears ε. The budget arm settles it without
the floor, which matters because ε is a threshold this file chose and the budget
is one the operator ruled (`:distinguishable-without-the-floor?`).

**But 4 of 50 scored candidates have any degree at all**, every one of them 1,
and all four sit in `library/cascades` and `library/snatch` — the two sections a
person organised by hand. So the term discriminates where the library is
organised and is flat everywhere else, which is the cost
`:constructor-relevance-substrate` states, now measured rather than predicted:
the constructor does not become good by being written.

### Controls, each failing the check rather than printing a note

- **citations re-read** — every acknowledgement's cited line span is re-read from
  disk and must contain the cited cue. Two mutations must be rejected: a cue that
  occurs nowhere, and a span off the end of the file.
- **library correspondence** — each rule's `:then-source` names a pattern the
  repository holds and points at a line that still begins `+ THEN:`.
- **grain separation and the conjunct forged** — the `:bare` forge (a plain play
  situation with `:grain` set to `:policy`) leaves every rule silent, so at this
  grain the separation would also hold on key-disjointness alone; the
  `:enriched` forge hands the rules a situation carrying *both* kinds of field,
  where the conjunct is the only thing left to tell them apart, and there the
  rules fire. The control fails if every rule is silent under `:enriched`.
- **O1–O3 on both constructed cascades**, with three mutations that must be
  rejected: an edge the prior's relation carries and `@why` reachability does not
  (O2), a dropped edge (O3), and a node in none of the three origins (O1).
- **determinism** — two constructions of one temperament must agree edit for
  edit, so the tie-break by pattern id is checked rather than documented.
- **F1–F4 on the find that produces the seed**, with `ants/pheromone-trail-tuner`
  named in advance as the pattern this tension must not select.

`:as-of :read-digest` is a sha256 over exactly what the constructor reads.
`library/math-formalization` takes files from a live scribe with no baseline step
of its own (`decisions.edn :math-formalization-untracked-flexiargs`), so a report
from this file is a timestamp and not a standing property, and the digest is what
makes that visible.

---

## Ants — the constructor's control domain (`:LA4`)

`construct_ants_cascade.clj` — worklist `:LA4`, LA1c-restatement.md §6. The same
constructor, on the second of the four domains the abstract interface names, and
Ants is there because it is the **control**: its authority gate refused every
real pattern on held-out yield in 2026-07-16 with sound controls
(`futon2/holes/cascade-ants.edn:21-23`), so a constructor that only ever confirms
is caught here. LA1c §6 states the expected result in advance — a refusal — and
this pair of files does not soften it.

    clojure -Sdeps '{:paths ["checks" "."] :deps {babashka/fs {:mvn/version "0.5.25"}}}' \
      -M -m construct-ants-cascade      # writes checks/ants-cascade.edn
    cd ../futon2 && clojure -M -m cascade-authority-gate 20 300
    cd ../futon3 && clojure ... -M -m construct-ants-cascade   # second pass: folds in the O4 row

Nothing tension-independent is copied: `acknowledgements`, `match`,
`antecedent-holds?`, `score`, `ranked`, the three policy-grain rules, the two
temperaments, `initial-state`, `run` and `cascade-of` all come from
`construct_cascade.clj`, whose `tension` is `^:dynamic` for exactly this. A
second copy would be the facade LA1c §11 names, one level below the firing loop
`:LA3` moved into `find_organise.clj` for the same reason. `construct-cascade.edn`
regenerates byte-identically after the change (sha256 `fa644053…`).

**The split between the two files is the point.** `construct_ants_cascade.clj`
writes the cascade as **data** — members, precedence, provenance, stop — and
writes nothing about `@aif-delta`; `futon2/scripts/cascade_authority_gate.clj`
reads that artefact and folds the deltas from the same flexiarg files. So the
cascade can be reviewed *before* the run (LA1c §7) and the two sides never share
a fold.

### What the ants domain gives the constructor, and what it does not

`library/ants` holds five patterns and **zero** authored `@why`/`@how`
directives, so the degree term is identically zero before anything is scored.
That is the second tension `decisions.edn :constructor-degree-term-measured
:what-would-change-this` asked for, and it comes out as that entry predicted:
*not* distinguishable from uniform here. The verdict recorded for `:LA3` is
tension-specific, and this file records it as such rather than leaving the
stronger reading standing.

`find` selects four of the five; the F4 falsifier named before the run —
`ants/baseline-cyber-ant`, whose `@aif-delta` is literally empty and which is also
the gate's sham arm — is not selected. The two temperaments then differ as they
should: the budget arm saturates at five members (it admits the sham), the floor
arm halts at four. **Through channel 1 the two are indistinguishable**, because
the member that separates them contributes `{}` — recorded on the gate as
`:budget-cascade-folds-to-the-same-config?`.

### The gate's result, in one line each

- **Refusal on yield.** Mean paired Δ +0.315 on a base of 18.445, from *one* of
  twenty seeds; the other nineteen tied exactly. `sign-p` 1.0. Controls sound:
  the sham cascade tied `off` exactly on every seed at every λ including 0.
- **Not a refusal on behaviour.** 16 of 20 seeds have a different action trace:
  pheromone −12.1, forage +11.45, hold +1.0, return +0.45 per run. Channel 1 is a
  live actuator on what the ant *does* and is not one on what the colony *eats*.
- **Not a tautology.** The gate refuses to run an arm until the folded config is
  shown to change the expected free energy (`efe-is-connected`) — the guard
  `README-xeno-loop.md §0` exists to ask for.
- **O4 holds, on its weaker disjunct.** Exchanging the precedence of the two
  members that write `[:efe :lambda :info]` changes the acting order and changes
  no observable: identical yields and identical traces at all four λ. The
  contention `cascade-ants.edn:110` calls incoherent is real as semantics and
  null on this oracle.

---

## Zaif — the domain that exercises `find`, and the one the reviewer gated (`:LA5`)

`construct_zaif_cascade.clj` — worklist `:LA5`, LA1c-restatement.md §7. The same
constructor on the third domain, and Zaif is there because **its repository is
the whole library**: 1,239 patterns across 98 sections, so this is the only one
of the four where `find` searches at the library's real size against a tension
that is not about the library.

The Tension is one real seat task, not a description of one.
`checks/zaif-task-a97J05.md` is a verbatim copy of a persisted `turn-start`
prompt (evidence `e-cb139dba-b1db-4e85-9353-c9b8b1c8c62d`, agent `zai-4`,
2026-08-06 — a PASS-1 Lean proving packet), and each of the five clauses cites a
line span of it.

    clojure -Sdeps '{:paths ["checks" "."] :deps {babashka/fs {:mvn/version "0.5.25"}}}' \
      -M -m construct-zaif-cascade                      # writes checks/zaif-cascade.edn
    cd ../futon3c
    clojure -Sdeps '{:aliases {:la5 {:extra-paths ["scripts" "../futon3/checks"]}}}' \
      -M:la5 -m zaif-cascade-gate coverage              # the rule table, no outcomes
    clojure -Sdeps '{...same...}' -M:la5 -m zaif-cascade-gate                       # the run
    clojure -Sdeps '{...same...}' -M:la5 -m zaif-cascade-gate holes/zaif-cohort-holdout.edn

### The cue licence, and why it is here

A cue must **occur in its own clause's cited span** of the task file, checked by
`cue-licence` on every `-main`; an unlicensed cue is a hard failure. The rule was
added after a pre-run review found that `"scope"`, `"cap"` and `"reuse"` occur
nowhere in the prompt, and that `"scope"` had put a pattern about narrowing an
academic paper into a seed about a frozen Lean theorem. Applying it struck **18
of 27** cues; the seed fell from 38 to 11 and the candidate pool from 457 to 343.
It is a removal rule and never an addition rule — a cue added once the seed is
known is tuning whatever it is called.

### Two pre-run reviews, and what each changed

This is the row's point, not an aside: LA1c §7 claims a carried cascade can be
reviewed *before* it runs, where per-decision arm arithmetic offers nothing to
read until the decisions have happened. Both reviews (codex-17, 2026-09-02)
returned **REVISE** and both found defects the constructor's own controls missed.

- Review 1, on `checks/zaif-cascade.edn`: the unlicensed cues above; a harder
  falsifier which was in fact selected; the budget arm halting before admitting
  anything; and, decisively, that the mapping from a pattern's THEN to a round
  decision did not exist yet, so the behaviour could not be gated at all.
- Review 2, on the rule table: `missing-dependency-protocol` encoded as a
  standing predicate true on 68 of 102 rounds rather than the THEN's sequence;
  comparisons not restricted to rounds carrying both a transcript and a v0
  decision; more than half the oracle labels author-assigned on `run_shell`-only
  rounds; and the treatment described as "the cascade" when a deferred round
  carries v0's own answer.

### The result

An **in-sample retrospective**, never predictive validation: the cues were
authored from the same task whose transcript is the oracle.

- v0 chose `:retrieve` on 29 of 29 primary rounds — and on 2,699 of 2,713
  recorded decisions store-wide, with `:act` and `:yield` never chosen at all.
- The 17-member cascade has **one** member that can act at a round decision. The
  override differs from v0 on 1 of 49 paired rounds and **0 of 29 primary**. On a
  held-out task (`zai-3`, a different problem, a turn that *did* report) it
  differs on **0 of 37**.
- Controls: v0 re-derives from its own recorded `:inputs-snapshot` through the
  real `zaif-controller/decide` on 50/50 and 37/37 rounds, arms and all four G
  terms exact; the sham ties v0 exactly on both cohorts.
- **Where the null lives.** Not in the ranking and not in the actuator.
  `agent/budget-bounds-exploration` and `agent/pause-is-not-failure` — whose
  THENs answer this tension directly — are not selected, because
  `antecedent-holds?` wants the IF *and* the HOWEVER each to acknowledge a
  clause, and their IFs are written in general terms that name no clause of any
  particular tension. Had they been selected the override would have differed on
  46 of 49 rounds and would have said *stop* from round 25 of a turn that ran to
  102 and never reported.
- **O4 is not exercised**: one member carries a rule, so there is no pair to
  exchange.

---

## ALFWorld — the external benchmark, and the constructor's reach on a library it did not shape (`:LA6`)

`construct_alfworld_cascade.clj` — worklist `:LA6`, P-validated-R5.md:685-694.
The fourth domain, and the first whose library was **not written inside this
line of work**: `futon3c/library/alfworld` is ten patterns committed on
2026-02-20 (futon3c 27136615, Joseph Corneli) by an agent playing ALFWorld games
under `futon3c/holes/missions/M-alfworld-pattern-discovery.md`, months before the
constructor existed. Nothing in them was shaped to be selectable.

    clojure -Sdeps '{:paths ["checks" "."] :deps {babashka/fs {:mvn/version "0.5.25"}}}' \
      -M -m construct-alfworld-cascade   # writes checks/alfworld-cascade.edn

**The repository is read in place, across a repo boundary.** `library-root` is
`../futon3c/library`, not a copy imported into futon3 — importing would make this
lane the apparent source of files it did not write.
`find_organise.clj:115-137` now derives an entry's `:file` from `library-root`
rather than from the literal string `"library/"`, because `citations-verified`
(`construct_cascade.clj:465`) *slurps* that path and a citation from another
checkout could not otherwise read back. For `library-root` `"library"` the
derived string is unchanged, and `construct-cascade.edn` (`fa644053…`),
`ants-cascade.edn` (`faccd884…`) and `zaif-cascade.edn` (`6827433a…`) all
regenerate byte-identically, which is what checks that.

### What this run does NOT deliver

It does **not** test the architecture on a repository that has a `standsOn`
relation. All ten patterns carry zero `@why`, zero `@how` and zero `@see-also`,
so the degree term is identically zero here exactly as over `library/ants`, and
`:distinguishable-from-uniform?` is `false`. `decisions.edn
:alfworld-standson-has-no-licensed-author` records why this lane may not repair
that: a `@why` is the author's causal claim, README-flexiarg §5a confines this
lane to `@how`/`@see-also` and to *proposing* `@why`, and law O2 forbids deriving
the edges from similarity. The repository half of the external benchmark is
open, and it is open on a governance question, not a labour one. ALFWorld itself
is also not installed in this checkout (no `.venv-alfworld`, no ALFWORLD_DATA),
so nothing is played and O4 is unexercised.

### What it does deliver: LA5's bound, replicated where it can be attributed

`:LA5` found that the constructor's reach is bounded by how antecedents are
**written** — `antecedent-holds?` needs the IF *and* the HOWEVER each to
acknowledge a clause, and two zaif patterns that answered the tension were passed
over because their IFs were general. Whether that was a property of the
**constructor** or of how this project happens to write patterns could not be
told apart there: all three libraries were written by the same hand inside the
same project. Here they were not, so the reach expectation is **pre-registered**
(`expected-to-fire`, six of the ten, named in the source before the run) and
reported as a confusion matrix.

- **3 of the pre-registered 6 fired.** `object-location-priors`,
  `single-carry-economy`, `systematic-search-fallback`.
- **0 fired that were not expected.** No false positives.
- **2 of the 3 misses were on the IF alone** — `search-dominates-execution` and
  `admissible-commands-are-ground-truth` each have a HOWEVER that acknowledges a
  clause and an IF that acknowledges none. `search-dominates-execution`'s IF is
  *"You try to optimize ALFWorld performance by refining execution"* — it is
  written about the reader's mistaken strategy, while the situation the tension
  names sits in its HOWEVER (*"The variable is the number of failed searches"*).
  This is LA5's failure mode exactly, on a library written by someone with no
  knowledge of the constructor, so the bound is a property of
  `antecedent-holds?` and not of this project's house style.
- The third miss, `remember-what-you-see`, acknowledges nothing in either block
  and is a genuine non-answer under these cues, not a writing artefact.
- **The anti-tuning guards held.** The F4 falsifier
  `alfworld/closed-containers-need-opening`, named before the run, was not
  selected; and the fourth clause — *when search stalls there is nobody to ask*,
  sourced to the mission's own fifth pattern class
  (`M-alfworld-pattern-discovery.md:47`, "Coordination triggers (when to bell for
  help)"), which the library never delivered — scored **0 hits**, as a clause a
  cue set tuned to the patterns would not have carried. `require-pass!` throws if
  that clause is ever acknowledged: the guard is only a guard while it hits
  nothing.
- **`organise` recovers two of the three misses, by score rather than by
  antecedent.** Under `widen-to-a-budget` the cascade reaches 6 members and
  admits `search-dominates-execution` and `admissible-commands-are-ground-truth`
  as candidates, plus `verb-to-appliance-mapping`, which no reader expected.
  Under `widen-to-the-marginal-gain-floor` it halts at the 3 that fired. So the
  `find` bound and the `organise` bound are different bounds, and the artefact
  carries both.

`require-pass!` deliberately does **not** assert the size of the reach agreement.
Whether the pre-registered six fired is the measurement, not the acceptance bar;
asserting it would turn a pre-registration into a target. It also does not treat
an empty cascade as an error, for the reason `:LA4` records. What it does throw
on is a law violation, a control failure, a citation that does not read back, F4,
and an acknowledged null clause.
