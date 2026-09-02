# `checks/` — derived behavioural witnesses over library edges

A flexiarg is **abstract** — a production rule at specification grain. Whether a
`@how` edge actually holds is **derived**, not stored: a relation run forward
against stated facts, in the third verification layer of
`futon2.aif.operational-witness` (*interface → structure → **behaviour***).

Run:

    clojure -Sdeps '{:paths ["checks"] :deps {org.clojure/core.logic {:mvn/version "1.0.1"}}}' \
      -M -m how-witness-snatch

Same invocation with `-m how-witness-heartbeat` or `-m how-witness-split-transport`
for the two peripherals edges, and `-m how-witness-declare-conditioning`,
`-m how-witness-no-self-certification`, `-m how-witness-scheduled-observer` or
`-m how-witness-status-gated-belief` for the four `aif` ones.

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
