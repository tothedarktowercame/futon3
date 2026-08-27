# `snatch/` — playing Snatch or Share, read as institutions

**Added 2026-08-27** · claude-13 at Joe's direction, to test the syllogism
*Alexander patterns ≈ Ostrom institutions ≈ policies*.

Source: `github.com/AllianceBioversityCIAT/snatchgame` — a mobile multiplayer
game *"inspired by Elinor Ostrom's work"*, used in workshops. Read from its
mermaid design diagrams (`g1-no-property.mmd` … `g5-cheap-talk.mmd`), not from
the client/server source.

## The game

P1 holds 10 turkeys, P2 holds 10 corn. **Your own tokens score 1; the other
player's score 2**, so acquiring is worth twice holding. Each round P1 abstains
or proposes an offer naming what it gives and what it asks; P2 makes one
decision — **accept**, **refuse**, or **snatch** (take the offer, pay nothing).
Five treatments, five rounds each.

## Six patterns

| pattern | from |
|---|---|
| `protect-the-unprotected-move` | G1, the baseline |
| `preserve-the-right-to-abstain` | G2, forced offer — on by default |
| `mark-without-force` | G3, shame tokens |
| `revert-then-invert` | G4, the AutoJudge's two steps |
| `non-binding-talk-still-moves-play` | G5, cheap talk |
| `institutions-vary-by-position-and-force` | the family as a whole |

## What it confirms, and what it does not

**Confirms: the family is generated, not enumerated.** The five treatments are
one interaction with an intervention placed on two axes — **where** it acts
(before the choice / at the choice set / after the outcome) and **with what
force** (none / symbolic / material). Five entries carry more structure than our
own eight flat response classes or fourteen-element action-type set. That is the
*structure, not more entries* claim, demonstrated by a working artefact.

**And the position axis lands on our own columns:**

| intervention position | our column | ring |
|---|---|---|
| before the choice (G5, a channel) | PERCEIVE / the observation vector | **R2** |
| at the choice set (G2, an option removed) | SELECT / the candidate space | **R6** |
| after the outcome (G3, G4, a mark or a remedy) | EVALUATE / the fold | **R5**, **R8** |

Three intervention positions, three problem columns. Offered as a correspondence
worth noticing, not as a proof.

**Two sharper coincidences.** G3's mark *"changes nothing in the current round"*
and R5's own `THEN` asks to surface a satisfied rung *"without changing a
numerical posterior unless that artifact warrants the change"* — the same
mechanism, opposite polarity. And `futon2/src/futon2/aif/repair_obligation.clj`
is G4-shaped: it reverts and preempts. **The stack has no G3** — no mark that
records without acting.

**Does not confirm:** anything about G(π), scoring, or whether cascades are
policies in the AIF sense. This is a family of institutions written as patterns;
that they *can* be so written is the evidence, and it is evidence about the
vocabulary rather than about the formalism.

**Not taken:** the payoff asymmetry (own 1, other's 2). It is a stipulated
teaching incentive — someone else's `C`.

## Related

- `p4ng/empirics-futon/NOTE-snatchgame-as-inspiration.md` — the first reading.
- `p4ng/empirics-futon/NOTE-cascades-are-policies.md` — the syllogism.
- `futon0/analysis/business-models/NOTE-event-type-vocabulary.md` — structure vs entries.


---

## Not yet a complete theory of play (assessed 2026-08-27, at Joe's challenge)

Eleven patterns: six design-grain, five play-grain. Six gaps, the first
structural and the rest missing entries.

### 1 · Nothing states the gain — every pattern is defensive

Read the eleven conclusions together and **ten are about exposure, sanction,
exit, or bounded risk.** Only `non-binding-talk-still-moves-play` is about
producing something rather than limiting a loss, and even it is framed as
reaching cooperation without enforcement.

**No pattern says why exchange is worth doing.** Own tokens score 1 and the
other's score 2, so a completed exchange is **strictly positive-sum for both
sides** — each gives 1-point tokens and receives 2-point ones. That is the whole
reason the game exists, and the collection is silent on it.

This is the same shape as the plop-2026 catalogue finding: there, *an agent that
only ever acts*; here, **a player who only ever protects**. It is a missing
dimension rather than a missing entry, which is why it is listed first.

### 2 · P2 has no patterns at all

Every one of the eleven is written for the offerer. The accept / refuse / snatch
decision — **half the game, and the decision the whole design turns on** — has no
coverage. What an offer's *size* signals, when a mark deters, whether
denunciation risk changes the calculus under G4: none of it.

### 3 · The ask is unaddressed

An offer is *(give X, ask A)*. Every pattern discusses what to **give**; none
discusses what to **ask**. Half of every offer is outside the language.

### 4 · G2 and G5 have no play patterns

**G2** removes abstention — so `consult-the-remedy-before-exiting` and any
grim-trigger policy assume an exit that does not exist there. **G5** adds a
pre-round channel with its own decision (what to say, whether to believe), and
the collection's only cheap-talk pattern is design-grain.

### 5 · The horizon is ignored

Five rounds, known in advance. In the last round a mark deters nothing and a
future does not exist, so the reasoning inverts. **Nothing says the last round is
different.**

### 6 · No re-entry condition

`consult-the-remedy-before-exiting` gestures at it; nothing states when re-entry
after an exit is warranted, or on what evidence.

### What this says beyond Snatch

Gaps 2–6 are entries someone can write. **Gap 1 is the one worth carrying**: a
collection assembled from incidents will be defensive, because incidents are what
went wrong. Nothing in the authoring process asks *what is this for*, so the
value proposition never gets a pattern — and the same reading applies to the
War Machine catalogue and to the war-room rulings.
