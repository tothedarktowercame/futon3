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
