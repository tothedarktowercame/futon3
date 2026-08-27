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

## The gaps filled, and reviewed (2026-08-27)

codex-22 wrote seven play-grain patterns against the six gaps above
(`550d4786`); claude-13 reviewed the diff, re-ran the playout, and made the
fixes recorded below. Eighteen patterns now: six design-grain, twelve
play-grain.

**Gap 1 is closed.** `exchange-when-both-sides-gain` states the thing no pattern
stated — *"each transferred token replaces one point for its giver with two
points for its receiver; a completed bilateral exchange therefore creates value
for both sides"* — and grounds it in the recorded payoff rule rather than in an
incident. `ask-for-surplus-not-surrender` and
`accept-an-offer-that-beats-holding` follow from it by `@why`. On a recount the
defensive-to-gain balance moved from one gain conclusion in eleven to at least
three in eighteen, with two more (`price-the-final-round-as-final`,
`re-enter-after-observed-repair`) reading either way. Counting is a judgement
call, but the sentence that was missing is now written.

### Three fixes made in review

1. **`accept-an-offer-that-beats-holding` is a P2 pattern**, and this harness's
   P2 is a fixed disposition rather than a chooser. It had been given a
   P1-shaped guard identical to `ask-for-surplus-not-surrender`'s, so it fired
   whenever P1 offered — coverage credited to a decision the harness never
   makes. It now carries `:actor :p2` and is excluded from P1's applicability
   set. Making P2 pattern-driven is the natural next step and would let it back
   in.
2. **`:offer-size` and `:ask-size` were both `(if (= act :offer) 1 0)`**, so
   `(pos? …)` on either was exactly `:offer-made?`. The game does name a give
   and an ask separately; this policy does not, so the guard now says only that
   an offer is being composed, and the pattern's advice about ask *size* is
   openly untested rather than tested by a tautology.
3. **`:counterpart-tokens` was initialised and never updated** — a constant
   dressed as state, so `(pos? …)` on it could not fail. Dropped. `(pos?
   (:tokens s))` is real and now has a witness: the new twelve-round G2 run
   drives P1's stock to zero by round 11, and `exchange-when-both-sides-gain`
   stops firing there.

G2 and G5 treatments had also been added without any scenario playing them, so
`forced-play-needs-a-loss-floor` and `use-talk-to-make-a-testable-offer` were
written for gap 4 but never exercised. `(show :g2 :snatcher 12)` and
`(show :g5 :sharer)` now play them.

### What the G4 run shows

Under G4 the coverage gaps went 4 → 0, but P1's behaviour did not change:
`re-enter-after-observed-repair` fires in rounds 2–5 and π abstains anyway,
because π is grim trigger and does not consult the collection. Situations being
covered and a policy following the patterns that cover them are two different
measurements, and only the first is what "0 gaps" reports.

## A policy that consults the collection (2026-08-27)

The playout's original P1 was **grim trigger** — the repeated-games strategy of
offering until the counterpart defects once, then abstaining forever with no way
back. It is three lines and hardcoded, so patterns could fire all they liked and
P1 did the same thing regardless. `checks/playout_snatch.clj` now carries a
second policy alongside it, `pi-patterns`, whose action **is** the THEN of the
highest-precedence pattern that fires. Editing a pattern changes what P1 does.

Closing the production-rule loop needed three things:

- **A THEN as data.** Play-grain entries carry `:then`, a function from
  situation to action, and `:precedence`, the conflict-resolution order. Four
  entries have no `:then`: the ask size, the talk channel and the free mark are
  real game features this model does not represent, so they stay advisory rather
  than being given an invented action.
- **Denouncing as an action.** `revert-then-invert` records that G4's AutoJudge
  reverts *and then* inverts, and that **denouncing is optional**. So
  `consult-the-remedy-before-exiting` now emits `:denounce`, and
  `:repair-observed?` is set by that event rather than by a judge merely
  existing. A judge being available is not a repair having happened; the earlier
  version conflated them.
- **A score.** P1's payoff from the recorded rule (own tokens 1, the other's 2):
  a completed exchange of n nets +n, a snatched offer −n, and a denunciation +3n
  — n restored plus 2n transferred at P2's expense.

### What the two policies score

| scenario | grim | patterns | difference |
|---|---|---|---|
| G1 snatcher | −1 | **−5** | −4 |
| G1 sharer | +5 | **+15** | +10 |
| G1 cautious | 0 | 0 | 0 |
| G4 snatcher | −1 | **+3** | +4 |
| G2 snatcher | −10 | −10 | 0 |
| G5 sharer | +5 | **+15** | +10 |

**G4 is the case the collection was written for.** Grim trigger takes one loss
and leaves; the pattern policy denounces, gets the transfer inverted, re-enters
on the observed repair, and ends +3 where grim ends −1. Two patterns produce the
whole difference, and the hardcoded policy could not reach either.

**G1 against a snatcher is where the collection is wrong.** The pattern policy
loses five where grim loses one, because after the first snatch nothing tells it
to stop: `consult-the-remedy-before-exiting` requires a remedy to exist, and G1
has none, so `exchange-when-both-sides-gain` keeps firing and P1 keeps offering
into a player who takes. **The collection has no pattern for exiting an
arrangement that offers no recourse.** That is a seventh gap, and unlike the six
assessed by reading, this one was produced by the machinery — the score is what
distinguishes a collection that plays well from one that reads well.

Under G1 against a cautious counterpart the two policies tie at zero, but they
do not behave alike: grim offers into a refusal five times, while
`an-unmodelled-response-stops-the-line` fires in round 2 and the pattern policy
stops. Refusal is free in this model, so stopping earns nothing here — the
difference is real and the score cannot see it.

### A correction to the coverage numbers

G1's four reported gaps were an artefact. Three guards were conditioned on
`:offer-made?` — whether P1 had *already decided* to offer this round — so after
grim trigger abstained, patterns that describe the situation could not fire. A
pattern advises a decision; conditioning its IF on that decision inverts the
exercise. No guard now mentions the action taken this round, and the G1 gaps go
to zero: an exchange was available in those rounds all along, which is exactly
why the policy needed a pattern telling it not to take one.
