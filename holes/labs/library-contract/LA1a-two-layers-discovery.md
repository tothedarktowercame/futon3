# LA1 slice (a) — DISCOVERY: Snatch's two policy layers, reconstructed from the artifacts

**Row:** `worklist.edn` `:LA1`, slice (a) of three. **Date:** 2026-09-02.
**Scope:** reconstruct, with pointers, what is already built. Nothing is authored
into the library here (no `.flexiarg` is touched by this slice) and no restated
type is proposed — those are slices (b) and (c). Every claim below carries a
`file:line` or says **not found**.

Source of the question: `futon2/holes/problems/P-validated-R5.md` §1b (`:642–730`),
Joe 2026-09-02 — *"In Snatch, there were 2 layers of policy: one defined by player
temperament that built a cascade, and one defined by the cascade itself that
organized play."*

---

## 1. The eighteen patterns, and a correction to the row's premise

`futon3/library/snatch/` holds 18 `.flexiarg` files. **The row's statement that
they are "all `@audience` players" is wrong.** The `@audience` line splits them
exactly 12 / 6:

| `@audience` names players (12) | `@audience` names institution designers (6) |
|---|---|
| `an-unmodelled-response-stops-the-line:3` (players, model authors) | `protect-the-unprotected-move:3` (institution designers, pattern authors, AIF implementers) |
| `consult-the-remedy-before-exiting:3` (**players, policy authors**) | `preserve-the-right-to-abstain:3` (…, rule authors, operators arming flags) |
| `re-enter-after-observed-repair:3` | `mark-without-force:3` (…, evidence-surface authors) |
| `forced-play-needs-a-loss-floor:3` | `revert-then-invert:3` (…, adjudication authors) |
| `escalate-only-as-far-as-you-can-lose:3` | `non-binding-talk-still-moves-play:3` (…, channel authors) |
| `probe-before-committing:3` (**players, policy authors**) | `institutions-vary-by-position-and-force:3` (…, vocabulary authors) |
| `exchange-when-both-sides-gain:3` | |
| `ask-for-surplus-not-surrender:3` | |
| `a-free-mark-is-always-worth-assigning:3` | |
| `use-talk-to-make-a-testable-offer:3` | |
| `price-the-final-round-as-final:3` | |
| `accept-an-offer-that-beats-holding:3` (responding players) | |

**The split is not decorative: it coincides exactly with the harness's `:grain`
key.** The six institution-designer patterns are precisely the six entered as
`:grain :design` in `futon3/checks/playout_snatch.clj:19–30`; the twelve
player-audience patterns are precisely the twelve entered as `:grain :play`
(`:46–113`). The `@audience` line and the `:grain` key were written at different
times and neither derives from the other, so the agreement is evidence that the
grain distinction is real in the material rather than an artefact of the harness.

**Two patterns already name policy authors as their audience**
(`consult-the-remedy-before-exiting:3`, `probe-before-committing:3`) — they are the
only two of the eighteen that do. `consult-the-remedy-before-exiting` is the one
whose HOWEVER quotes the reactive rule directly (§5 below).

The paper's "twelve patterns" (`P-validated-R5.md:241`) and the record's *"`find`
was the identity on the twelve playing patterns"* (`:374`) both refer to this
play-grain twelve, not to all eighteen.

Of the twelve play-grain entries, **7 carry a `:then` and a `:precedence`**
(`playout_snatch.clj:46,52,65,72,78,87,93`); **4 are advisory** — their THEN speaks
about ask size, a talk channel, or a mark that changes no payoff, none of which the
harness models (`:102,104,106,108`, and the limit is recorded at `p4ng/app-snatch.tex:229–231`);
**1 belongs to P2**, who is a fixed disposition here rather than a chooser
(`playout_snatch.clj:112–113`).

---

## 2. Layer two — the cascade organises play

`playout_snatch.clj:150–158` `pattern-policy`: filter the collection to patterns whose
ANTECEDENT holds, sort by precedence, take the first `:then` that returns an action.
The antecedent is `IF ∧ HOWEVER`, both required (`fires?`, `:118–121`; the reason is
stated at `:39–45` — where the counter-force is not live the THEN would happen anyway).

No guard may mention the action P1 took this round (`:37–38`) — the harness's own
statement of the S-G3 discipline at play grain.

The cascade proper is not the acting sequence. It is the sub-graph of `@why`
authority edges the acting patterns stand on, **closed upwards** (`up-closure`,
`:248–257`; `induced-edges` `:259–260`; `shape` `:280–290`), read from the pattern
files at run time (`why-graph`, `:243–246`). Stated in the paper at
`p4ng/app-snatch.tex:129–134`: for G4 against a snatcher, seven nodes, ten edges,
four nodes standing on two authorities each — so it is not a tree.

---

## 3. Layer one — temperament builds and orders the cascade

Two temperaments exist today, and **both are written in Clojure rather than in the
library**:

- **`pi-grim` (`playout_snatch.clj:139–143`)** — offer one token until snatched,
  then abstain for the rest of the game, with no way back. The comment at `:137–138`
  is explicit: *"Hardcoded — the collection cannot reach it."*
- **`pattern-policy` (`:150–158`)** — a function of `overrides`, a map from pattern
  id to a replacement precedence number. **This argument is the temperament's THEN
  output as it stands today.** `patterns-overrides` is `{}` (`:160`) and
  `exchange-first-overrides` is the one-key edit `{:exchange-when-both-sides-gain 0}`
  (`:165`). Reifying temperament as a pattern means moving that map out of code and
  into a THEN clause.

### The precedence order is authored by hand, and that is recorded as a limitation

Primary record: **`p4ng/app-snatch.tex:231`** — *"Precedence is authored by hand,"*
in the `\subsection{Limits}`. The reason it cannot be derived from the patterns is
argued at `app-snatch.tex:117–126`: the classical production-system answer is
specificity (most conjuncts wins) and it fails here, because
`an-unmodelled-response-stops-the-line` has the fewest conjuncts of any acting pattern
and must be consulted first — *"a commitment about the collection rather than a
property of any member of it, and there is nowhere in a pattern to record it."*
Carried into law form as **O4** at `P-validated-R5.md:502`: precedence is data,
recorded on the cascade as a collection-level field, not derived from the patterns.

### S-G4 — the score depends on the wiring

The Lean side: `mathlib4/DarkTower/WarMachine/PolicyGrade.lean:55` `wiringSensitive`,
`:61` `wiringSensitive_needs_two_wirings`, `:78` `earnsPolicyGrade`, `:136`
`PatternWiring` (inductive), `:146` `patternScoreUnder : PatternWiring → Int`,
`:154` the theorem pairing `.onePromoted = -5` with `earnsPolicyGrade`.
`P-validated-R5.md:222` calls this *"the one place the 'operation on a cascade'
already has a type"* — and it has it as a fixture, over a two-constructor inductive,
not over a general cascade.

Measured, from the emitted artefact `futon3/checks/snatch-cascade.edn`
(written by `playout_snatch.clj:331–375`; the figure is drawn from the same file so
it cannot drift):

| treatment / disposition | `patterns` | `exchange-first` | acting → added-by-organise |
|---|---|---|---|
| g1 snatcher | −5 | −5 | 2 → 2 · 1 → 0 |
| g1 sharer | 15 | 15 | 2 → 2 · 2 → 2 |
| g1 cautious | 0 | 0 | 2 → 2 · 1 → 0 |
| **g4 snatcher** | **+3** | **−5** | 3 → 4 · 1 → 0 |
| g2 snatcher | −10 | −10 | 3 → 3 · 1 → 0 |
| g5 sharer | 15 | 15 | 2 → 2 · 2 → 2 |

`:s-g4 :verdict :holds` — **on one scenario of six.** The +3 → −5 move the records
quote (`P-validated-R5.md:241`, `app-snatch.tex:114`) is the g4/snatcher row, and it
is the only row where re-wiring moves the score. Membership is unchanged and every
pattern's text is unchanged.

A second thing the table shows that the prose does not: under `exchange-first`,
four of six scenarios collapse to **one acting pattern and zero nodes added by
organise**. That is the disconnected-cascade case — `exchange-when-both-sides-gain`
carries `@see-also` and **no `@why` at all** (`exchange-when-both-sides-gain.flexiarg:6`),
so its upward closure is itself. `M-formal-war-machine.md:1470–1473` names exactly this
pattern as the witness that firing-precondition and authority graph are different
relations; the artefact confirms it numerically.

---

## 4. The third layer

`futon2/src/futon2/aif/operational_witness.clj:1–13` states the three verification
layers: interface (`clean_argcheck`), structure (`build-match`), and **behaviour** —
a `core.logic` relation over `(before, event, after)` run FORWARD against the live
transition, *"ungameable … a goal either succeeds against reality or it does not."*

`futon3/checks/how_witness_snatch.clj` is that layer applied to one `@how` edge
(`:1–20`): `protect-the-unprotected-move @how preserve-the-right-to-abstain` is
ATTESTED iff removing abstention strictly worsens P1's guaranteed floor
(`exposure-reduced-by-abstentiono`, `:44–54`), with a mirror over two treatments that
both preserve abstention which must find nothing (`:63–67`).

Two limits on reading this as composition: `P-validated-R5.md:307` (Q2) records that
`how_witness_snatch`'s `l/all` is *conjunction inside one witness, not composition of
kernels*, and that E-R5 step 3 (Kleisli along the DAG) is **withdrawn as a claim**.
So the third layer is available to attest a temperament pattern's `@how` edges in
slice (b), but it does not yet give a semantics for running a cascade.

---

## 5. Where reactive play sits among the eighteen — Joe's question (5)

**Reported, not presumed.** The reactive rule itself — grim — is **not a pattern**.
It appears in three ways:

1. **As code the collection cannot reach**: `pi-grim`, `playout_snatch.clj:137–143`.
2. **As the thing one play-grain pattern argues against**:
   `consult-the-remedy-before-exiting.flexiarg:19–22` HOWEVER — *"A policy written for
   the state of nature exits on the first defection, and exiting is indistinguishable
   — from the outside and from the policy itself — whether or not a remedy existed."*
   That sentence is a description of grim trigger, and it is the only place in the
   library where a temperament is characterised. Its THEN (`:24–26`) — *"Consult the
   remedies the arrangement provides before exiting, and record that you did so,
   including when you decline them"* — is a constraint on **what a policy must do**,
   which is why its `@audience` reads *players, policy authors*. But in the harness
   its THEN is encoded as a game action, `{:act :denounce :size (:seized s)}`
   (`playout_snatch.clj:61–64`), so it fires at play grain. **This pattern is the
   closest thing in the library to a policy-grain pattern, and it is currently
   compiled down to a move.**
3. **As the half of grim the library supplies separately**:
   `re-enter-after-observed-repair` is re-entry, which grim by construction has no way
   to do (`playout_snatch.clj:137`, "with no way back"). Its context says *"P1
   previously stopped offering after a snatch"* (`re-enter-after-observed-repair.flexiarg:10–11`) —
   the state grim leaves you in.

Of the three candidates the row named:
`consult-the-remedy-before-exiting` and `re-enter-after-observed-repair` are the two
that speak about a policy's shape over time; **`escalate-only-as-far-as-you-can-lose`
is not reactive** — it is offer sizing after an acceptance
(`escalate-only-as-far-as-you-can-lose.flexiarg:9,22–24`), and the harness records its
HOWEVER as **non-discriminating** — always true, naming a permanent condition of the
game rather than a tension in the situation (`playout_snatch.clj:80–83`).
`revert-then-invert` is institution grain, not player reactive
(`revert-then-invert.flexiarg:3`).

**So: reactive play is policy-grained, as Joe suspected, and the library holds two
fragments of it written at play grain because there was no other grain to write them
at.** Neither fragment states a trigger-and-persist rule; the persistence is in
`pi-grim`'s code.

---

## 6. The type discipline that keeps the reflective step from being a type error

The hazard is plain: a play pattern is `GameState → Option Action`
(`P-validated-R5.md:250`) and a temperament's THEN operates on cascades and
precedence. If "pattern" is one type, asking it to consume a cascade is a type
error, and letting a pattern apply to itself is the Russell shape.

**The discipline is indexing, not widening — and the harness already implements it.**
Write the production-rule form parametrically:

```
Rule S A := { if : S → Prop, however : S → Prop, then : S → Option A }
```

- `Rule GameState Action` — the play grain. 12 patterns; 7 executable.
- `Rule DesignSituation InstitutionEdit` — the design grain. 6 patterns.
  **Not found: no design-grain entry in the harness carries a `:then`**
  (`playout_snatch.clj:19–30` — every design entry is `:id`, `:grain`, `:if` only).
  So this grain is authored but has never been run.
- `Rule CascadeState CascadeEdit` — the policy grain. **Not found in the library.**
  Its two instances exist as Clojure: `pi-grim` and the `overrides` map (§3).

What makes this safe rather than self-application is that **the grain is a checked
conjunct of every guard**. Every design guard begins `(= :design (:grain s))`
(`playout_snatch.clj:20,22,24,26,28,30`) and every play guard begins
`(= :play (:grain s))` (`:47,53,66,73,79,88,94,103,105,107,109,113`), so a rule of one
grain can never fire in a situation of another. A third value `:policy` extends the
same mechanism without touching the shape.

The levels then compose by **stratified running**, not by a rule applying to itself:
policy-grain rules run in `organise` and emit a cascade with a precedence field
(O1–O4, `P-validated-R5.md:499–502`); play-grain rules run in `act` against game state.
`M-formal-war-machine.md:1454–1463` is the same observation from the other end —
*"Work from the bottom up is a policy-over-policies"* — with the consequence stated:
*"A derived precedence changes `G(π)`; it does not merely prettify it,"* and the open
question left as whether topological order is a **good** precedence, *"answerable by
running it."*

The adjacent trap is recorded at `M-formal-war-machine.md:1465–1475`: a pattern's
`IF ∧ HOWEVER` is its firing precondition and its `@why` is what it stands on — two
different relations, and the word "dependency" invites conflating them. A temperament
pattern will have both, and they will not agree.

---

## 7. Hyperparameters set from outside — Joe's question (4), the `@aif-delta` channel

The candidate mechanism exists and is `@aif-delta`. Read as recorded in
`futon2/holes/cascade-ants.edn:24–35`:

- **channel 1** — `@aif-delta → :aif-config`, via `attach-config`, keyword id, whole
  delta merged: `futon2/src/ants/cyber.clj:213`
  `(update :aif-config merge-deep (:aif-delta config))`. This is *"the patterns'
  DECLARED PARAMETERS."*
- **channel 2** — `pattern_efe.clj` hard-coded risk/info-gain via `:id` + `lambda.pattern`
  — *"a SEPARATE reimplementation."*

The 2026-07-16 authority gate verdict (`cascade-ants.edn:21–23`): *"AUTHORITY FAIL —
no real pattern differed from `off` by held-out yield sign test at any lambda in
{0.1,0.5,1.0}"*, controls sound (the sham tied `off` exactly on every seed, matching
full event traces).

**The scope limit is confirmed at the source.** `futon2/scripts/pattern_authority_gate.clj:52–68`
`activate-pattern` sets `[:config :aif :efe :lambda :pattern]` and `(assoc ant
:cyber-pattern {:id pattern-id …})` directly, and **never calls `cyber/attach-config`**.
So the gate measured channel 2 only. The verdict reads *"the hard-coded EFE terms do
not move the ant"*, not *"the design patterns do not move the ant."*

**One refinement to "NEVER TESTED".** The merge path is not dead code: `attach-config`
is defined at `ants/cyber.clj:202`, reached in the live world at spawn time via
`attach-config*` from `ants/war.clj:266,269`, and covered by a unit test
(`futon2/test/ants/cyber_test.clj:9` `attach-config-populates-metadata`). What has
never been measured is **yield under it** — the gate has never been run through
channel 1. That is a narrower and more actionable statement than "untested", and it
means slice (c)'s hyperparameter proposal has a working actuator to name rather than
one to build.

The five deltas as authored: `hunger-precision-coupling.flexiarg:9–14` (precision
gains + `:efe {:lambda {:survival 1.4}}`), `cargo-return-discipline:8–13`,
`white-space-scout:9–15` (`:efe {:lambda {:info 0.6 :ambiguity 0.4}}`),
`pheromone-trail-tuner:8–14`, and `baseline-cyber-ant:8–11` — **empty by design**,
described in `cascade-ants.edn:48` as *"the identity element of the pattern algebra."*
Note what these deltas are: **patterns whose effect is to set the parameters of an
EFE computation** — which is Joe's "outside patterns could set (hyper)parameters in
the policy playout", already written down for a different domain.

One recorded gap to carry: `cascade-ants.edn:129` —
`white-space-scout`'s THEN says *"lower G for `:return`"* but its `@aif-delta` has no
`:actions` key, so *"the behaviour is in prose, not in the delta. Channel 1 cannot
express it."* A temperament pattern will hit the same wall if its THEN is prose.

---

## 8. Constructor precedents — the third exchange's inventory

### (i) `cascade_construct.py` — right shape, substrate law O2 rejects

`futon3a/holes/labs/M-memes-arrows/cascade_construct.py`, 321 lines. Its docstring
(`:9–23`) states the design: **greedy ordering by marginal coverage, stop when the
best marginal falls below epsilon** — `m'(p) = rel(p|ψ) · (α + connectivity(p, chosen))`,
`STOP: m(best) < ε`, and explicitly *"NO budget ceiling here by design."* Loop at
`:222–232`; the stop test is `:230`. Defaults ε = 0.15, α = 0.3 (`:206`).

Two other pieces it already has that the restatement wants:

- **A prior read off the landscape**: `base_rate_prior` (`:127–155`) — a per-pattern
  Bernoulli *inclusion* prior from co-application mass, with the reasoning for why a
  categorical collapses the cascade to size 1 written out at `:129–137`; the score is
  `coverage − λ · Σ −log P(include p)` (`:236–238`).
- **A budget ceiling, applied downstream**: `cascade_serve.py:21–27` — the parsimony
  ceiling is applied at serve time, default raised 6 → 20 by operator ruling
  2026-07-05, and `"truncated"` is reported (`:39`).

**Why the substrate is wrong under law O2** (`P-validated-R5.md:500` — edges are
authored, *"never inferred from similarity, co-occurrence or prose"*):
relevance is MiniLM cosine (`cascade_construct.py:48–58,183–204`), the coherence term
`H` is a mean over pairwise embedding cosines (`:240–241`), **and the edges are not
authored either** — they come from the phylogeny (`load_phylogeny`, `:86–112`;
`phylogeny_connectivity`, `:157–166`), whose descent edges are computed in
`futon6/scripts/pattern_phylogeny.py:19–22` by **substring containment**: for every
pair, `if y in t` — pattern X is said to descend from Y if Y's filename stem appears
anywhere in X's text. That is inference from co-occurrence, one step removed from
similarity but the same law. The co-application weights come from missions
(`pattern_phylogeny.py:45`, "HGT roads").

So the precedent gives the **loop** (greedy + saturation stop + inclusion prior +
downstream budget) and gives no part of the **substrate**. Slice (c) should name what
replaces embedding relevance and citation-descent with `find` over authored `@why`.

### (ii) The `@why` up-closure — the too-much end, quantified

`playout_snatch.clj:248–257` takes the **full** transitive `@why` closure of the acting
set, and `:352` records the difference as `:added-by-organise`. From the artefact
(table in §3): closure adds 2–4 nodes on top of 1–3 acting. In the g4/snatcher run,
**3 acted and 4 more were added — more than half the cascade is present because it was
stood on, and never fired.** In an 18-pattern fixture that is bounded; over the
85-pattern repository (`P-validated-R5.md:501,521`) there is no bound stated anywhere. The
stopping problem is therefore visible in the fixture rather than severe in it, which
is the honest form of the claim.

### (iii) Learned phylogeny edges — nearly empty

`futon6/data/pattern-phylogeny-learned.json`: `source` is
`/home/joe/code/futon6/holes/closure-folds.edn`; **0 descent edges and 2 co-app edges**
(`['mission-anchored-scan','mission-unlocks-eoi',2,'upvote:kit-outbox']` and
`['model-recompute-schedule','prototype-maturity-lifecycle',1,'seed:kit-cadence']`),
neither touching `snatch/` or `ants/`. Read against the third exchange's "learning over
multiple runs": the learning channel exists and carries provenance tags, and it has
recorded two edges. It is a mechanism, not yet a substrate.

### (iv) Named but out of this slice's list

`futon3c/src/futon3c/agents/zaif_controller.clj`, 270 lines — confirmed at the length
`P-validated-R5.md:719` states. Its shape (per-decision arm choice with fixed
constants) is the sixth exchange's contrast case. Not inventoried further here;
the slice (a) list does not name it.

---

## 9. What slices (b) and (c) inherit

**For (b) — the family shape is already in `snatch/`, at institution grain.** There are
exactly two `@how` directives in the whole section, and both have the same four targets:
`protect-the-unprotected-move:6` and `institutions-vary-by-position-and-force:6`, each
`@how {preserve-the-right-to-abstain, mark-without-force, revert-then-invert,
non-binding-talk-still-moves-play}`. That is one general pattern pointing at the named
methods by which it is carried out — the §5a reading of `@how`, and the exact template
Joe described for have-a-temperament → five temperaments. `institutions-vary-by-position-and-force`
is the model to copy. Confirmed by the linter over the section (§10): `snatch` counts
`:how 8` — two directive lines at four targets each — and no other `@how` exists in it.

The rest of the section's shape, from the same run: 18 patterns, `:why 21`,
`:see-also 11`, **15 of 18 with an outgoing `@why`**, `:fraction-organised 0.833`.
The three with no outgoing `@why` are `exchange-when-both-sides-gain`,
`institutions-vary-by-position-and-force` and `price-the-final-round-as-final` —
the first of which is the disconnected-cascade case of §3.

**Open, and to be decided rather than drifted into:**
1. Which `:grain` value a temperament pattern carries, and whether the harness's guard
   convention (grain as a checked conjunct) is the mechanism or merely the precedent.
2. Whether `consult-the-remedy-before-exiting` is re-graded to policy or kept at play
   with a policy-grain sibling. It is play-grain in the harness and its `@audience`
   already says *policy authors*; re-grading a wave-2 pattern is a library edit no
   acceptance has asked for.
3. Whether precedence stays data on the cascade (O4) once a temperament pattern's THEN
   can emit it — O4 says precedence is *not derived from the patterns*, and a
   temperament pattern is a pattern.
4. What replaces embedding relevance in the constructor loop. (i) supplies the loop and
   nothing else.

**Not authored by this slice:** no `.flexiarg` was created or edited; no `@why`,
`@why-posthoc`, `@how` or `@see-also` was written; no edge was inferred from embedding
similarity. Law O2 (`P-validated-R5.md:500`) and `decisions.edn :spider-editorial-standing`
are untouched.

---

## 10. Gates run for this slice

This slice touched **no Clojure and no `.flexiarg`** — the only file it adds is this
document — so clj-kondo and `futon4/dev/check-parens.el` have nothing in scope. Exit
codes captured to files and tested, not read through a pipe.

| gate | invocation | result |
|---|---|---|
| `library_graph_lint_test` | `bb -cp . test/library_graph_lint_test.clj` (the C88 pin's invocation, `holes/labs/C88-pinned-library-evidence-index.md:15`) | exit **0** — 13 tests / 151 assertions / 0 failures; pinned index `{:files 1247, :why 86, :why-posthoc 7, :how 27, :see-also 141}` |
| `spider_runner_test` | `bb -cp . test/spider_runner_test.clj` | exit **0** — 8 tests / 43 assertions / 0 failures |
| section lint, `snatch` | `bb -cp . checks/library_graph_lint.clj --library library --section snatch --baseline library/.spider/baseline-edges.edn --attestations library/snatch/attestations.edn --report …` | exit **0**, `:pass? true`, `:failures 0`, `:unresolved-targets 0`, `:edge-refusals 0`, `:warrant-refusals 0` |

`library/snatch/attestations.edn` **does not exist** (`ls`: No such file or directory);
the linter accepts its absence and reports zero refusals of either act. Slice (b) will
be the first thing to create it.
