# `checks/` — derived behavioural witnesses over library edges

A flexiarg is **abstract** — a production rule at specification grain. Whether a
`@how` edge actually holds is **derived**, not stored: a relation run forward
against stated facts, in the third verification layer of
`futon2.aif.operational-witness` (*interface → structure → **behaviour***).

Run:

    clojure -Sdeps '{:paths ["checks"] :deps {org.clojure/core.logic {:mvn/version "1.0.1"}}}' \
      -M -m how-witness-snatch

**Every witness must ship with a mirror** — the same relation asked of a case
where it must *not* hold. A witness that cannot fail is not a witness.

| file | what it does | result |
|---|---|---|
| `how_witness_snatch.clj` | the edge as a **relation** — does it hold? | **ATTESTED**; mirror silent |
| `how_kernel_snatch.clj` | the same edge as a **kernel** — with what spread? | entropy 1.3863 → 1.3121 nats under attestation; mirror zero mass |

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
