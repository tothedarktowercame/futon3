# LA1 slice (c) — RESTATE: the unified types, the constructor, and a candidate §1

**Row:** `worklist.edn` `:LA1`, slice (c) of three. **Date:** 2026-09-02.
**Inherits:** slice (a) `LA1a-two-layers-discovery.md` (747e2cd) and slice (b)
`library/snatch/*.flexiarg` + `decisions.edn` (1020330).
**Status of everything below:** a *candidate* restatement of
`futon2/holes/problems/P-validated-R5.md` §1, written to be refused. It is put to
Joe through row `LJ1`. **Nothing here is built.** No Lean declaration is edited, no
Clojure is touched, no `.flexiarg` is authored or altered by this slice. Every
element that would have to be built is named as a future row in §12 and nowhere
executed.

---

## 1. The unification in one paragraph, and what it costs

Joe's sentence (`P-validated-R5.md:648-652`): *"If we included 'temperament' as a
design pattern, it would all unify, and G would be defined over policy."*

The unification is not that a policy becomes a pattern by analogy. It is that the
**production-rule form is one type indexed by a grain**, and `policy` is a third
value of that index beside `play` and `design`:

```
Rule S A := { if : S → Prop, however : S → Prop, then : S → Option A }
```

Snatch's runner already fires such rules by one loop that does not know which grain
it is running (`checks/playout_snatch.clj:150-158` — filter to the patterns whose
`IF ∧ HOWEVER` holds, sort by precedence, take the first `THEN` that returns
something). What has kept the levels apart is not the loop; it is that the only
rules the loop can reach are `:grain :play`, and the policy-grain rules are Clojure
(`pi-grim`, `:139-143`; the `overrides` map, `:160`, `:165`). Slice (b) wrote five
of them as patterns. So:

> **`organise` is not a separate function. It is the same firing loop run at policy
> grain: a cascade of policy-grain patterns is run against a `CascadeState`, each
> firing emits a `CascadeEdit`, and the fixpoint is the play-grain cascade.**

That is the whole of the restatement's structural claim, and it has a price that
should be stated before its benefits. **Three prices:**

1. **`organise`'s declared Lean type is wrong under it.** `organise : Set P →
   Repository P → Cascade P` (`mathlib4/DarkTower/WarMachine/Holes.lean:308`, `sorry`)
   has no argument for the temperament. §10.
2. **Law O1's prose and its Lean form come apart, and the Lean form is the one that
   survives.** The prose says `cascade.nodes = selected` (`P-validated-R5.md:499`);
   the witnessed instance says `nodes = selected ∪ addedByOrganise`
   (`Holes.lean:340-344`, `organiseO1NodesRecorded`). A temperament that *admits* a
   pattern adds a node that is neither selected nor stood-on — which the equality
   refuses and the union absorbs. So the defect is not falsification, it is that
   `addedByOrganise` now conflates two origins. §9.
3. **Nothing executes a policy-grain rule.** The six patterns slice (b) wrote are
   readable and refusable and do not run. `pattern-policy` still takes `overrides`
   as a Clojure argument, and the `:policy` conjunct that would keep a policy-grain
   rule out of a game situation is unbuilt (recorded at `decisions.edn`
   `:temperament-grain`, interim `:third-grain-value`, with that cost stated).

What it buys is stated in §11 and is one thing: **G becomes a function of an object
a reviewer can read before the run.**

---

## 2. The abstract domain interface

Joe's fourth exchange (`P-validated-R5.md:687-697`): *"it would be a pity if we
built something that could only play one economics game."* So the types below are
stated over a `Domain`, and §§5-8 instantiate it four times on paper.

```
Domain := {
  Situation : Type                      -- what a play-grain rule reads
  Act       : Type                      -- what a play-grain rule emits
  Obs       : Treatment → Type          -- the observation space, INDEXED BY TREATMENT
  repo      : Repository                -- patterns + authored standsOn edges
  Tension   : Type                      -- context / want / however of the PROBLEM
  oracle    : Cascade → Tension → Treatment → Dist (Obs t)   -- playout
  C         : Treatment → Dist (Obs t)  -- preferences, per treatment
}
```

`Obs` is indexed by treatment rather than fixed, and that is read off the artefact
rather than imposed: `futon3/checks/snatch-outcomes.edn:14-19` records that g2
**removes** an outcome and g3 and g4 each **split** one, with the note *"an
institution refines or coarsens the outcome space itself, and does not merely
reweight it."* The consequence for the restatement is a restriction, not a feature:
**Q(o∣π) is comparable only within a treatment**, and any cross-treatment claim
about G is comparing distributions over different spaces. The Aug-30 record's
per-vertex `C` (`P-validated-R5.md:2a`) is the same shape one level up.

`oracle` is what makes a domain usable without a semantics for cascades: it need
only *run* a cascade and report observations. `how_witness_snatch.clj`'s third layer
is not required for this, and the record is explicit that it could not supply it —
its `l/all` is conjunction inside one witness, not composition of kernels, and the
Kleisli-along-the-DAG claim is withdrawn (`P-validated-R5.md:307`).

---

## 3. The types

```
-- 3.1 the rule, indexed
Grain    := play | design | policy
Rule S A := { if : S → Prop, however : S → Prop, then : S → Option A }
Pattern g := Rule (Sit g) (Act g)
   Sit play   = Situation          Act play   = Act                -- 12 in library/snatch
   Sit design = DesignSituation    Act design = InstitutionEdit    -- 6 in library/snatch; NONE carries a :then
   Sit policy = CascadeState       Act policy = CascadeEdit        -- 6 written by slice (b); none executable

-- 3.2 the cascade
Cascade g := { nodes : Set (Pattern g)
             , edges : Pattern g → Pattern g → Prop      -- O2/O3: authored, fast-forwarded
             , precedence : List (Pattern g)             -- O4: data, written only by a policy-grain THEN
             , provenance : Pattern g → NodeOrigin       -- NEW; see O1' in §9
             , acyclic : acyclicDescent edges }
NodeOrigin := found | stoodOn | admittedBy (Pattern policy)

-- 3.3 the one firing loop, grain-polymorphic
fire : Cascade g → Sit g → Option (Act g)
fire C s = firstSome [ p.then s | p ← sortBy C.precedence C.nodes, p.if s ∧ p.however s ]
         -- checks/playout_snatch.clj:150-158, with the grain left open

-- 3.4 what the policy grain reads and writes
CascadeState := { under-construction : Cascade play
                , tension : Tension
                , record : List Event        -- the run and the construction so far
                , flags : Set Flag }         -- e.g. grim's monotone no-readmission flag
CascadeEdit  := admit (Pattern play)
              | drop (Pattern play)
              | promote (Pattern play) above (Pattern play)
              | setFlag Flag
              | halt Reason                  -- the stopping rule as an EDIT, not an outer condition

-- 3.5 the policy
Policy := { temperament : Cascade policy      -- a cascade, at policy grain: the unification
          , seed : FindResult }               -- what find handed it
-- there is no fourth type. A policy is a cascade of patterns, at a grain whose
-- operand happens to be another cascade.

-- 3.6 information state, find, construct, act, run
InformationState := { situation : Situation, history : List Event
                    , repo : Repository, tension : Tension }
find      : Tension → Repository → FindResult                      -- F1-F4; Holes.lean:264
construct : Policy → Repository → Cascade play                     -- §4; iterated `fire` at policy grain
act       : Cascade play → InformationState → Option Act           -- = fire at play grain
run       : Policy → Domain → Tension → Trajectory
run π D t = iterate (act (construct π D.repo)) over D.oracle

-- 3.7 the scored objects
Q(o∣π) := D.oracle (construct π D.repo) t treatment       -- DERIVED by playout, never authored (S-G3)
G(π)   := D_KL[ Q(o∣π) ‖ C treatment ] + 𝔼 H[P(o∣s)]      -- risk + ambiguity, per treatment
```

**Both of codex-22's and Joe's readings hold and are the two ends of one function**
(`P-validated-R5.md:340-360` records the disagreement). Codex read `Policy : Cascade
→ (History → Option Pattern)` — that is `fire` at play grain, `act`, with the cascade
given. Joe read the policy as the thing that *goes and finds* the cascade — that is
`construct`. `run = act ∘ construct ∘ find` composes them, and `Policy` names only
the part that is authored (the temperament cascade), because `find` and `construct`
are the same for every policy and the temperament is what differs.

### 3.8 Why this is indexing and not self-application

The Russell hazard is real: a pattern that applies to patterns. The discipline,
already implemented in the harness, is that **the grain is a checked conjunct of
every guard** — `(= :design (:grain s))` at `playout_snatch.clj:20,22,24,26,28,30`
and `(= :play (:grain s))` at `:47,53,66,73,79,88,94,103,105,107,109,113`. A rule of
one grain cannot fire in a situation of another. Adding `:policy` extends the
mechanism without touching its shape.

**The ladder terminates in one step, and that is a fact about the current design
rather than a theorem.** A policy-grain rule edits a `Cascade play`. Nothing edits
a `Cascade policy` — the temperament cascade's own precedence is authored by hand,
exactly as `p4ng/app-snatch.tex:231` records the play precedence today. So the
hand-authorship that the paper lists under *Limits* is not removed by this
restatement; it is **moved up one level and made smaller**: from ordering 12
play-grain patterns per scenario to ordering 5 temperaments once. If a
meta-temperament is ever wanted, it is a fourth grain value with its own conjunct,
added deliberately — not a rule applying to itself.

### 3.9 Does the `:grain` field become redundant? — answering `:temperament-grain`'s re-run condition

`decisions.edn :temperament-grain` says its interim arm should be re-run *"if slice
(c)'s restated types make the grain recoverable from the rule's type at the point
where rules are selected."* They do not, and the reason is worth stating rather than
letting the condition lapse silently.

`Pattern g := Rule (Sit g) (Act g)` does carry the grain in the type. But `fire`
(§3.3) selects over **one flat collection** — that is what `pattern-policy` does
today (`playout_snatch.clj:150-158`), and it is why the loop can be grain-polymorphic
at all. At the point of selection, the collection is homogeneous by construction only
if something has already separated the grains; the field is what does the separating.
So: **the type carries the grain for a reader and for Lean; the field carries it for
the runner.** The re-run condition is not met, `:third-grain-value` stands, and the
cost that decision records — the `:policy` conjunct is unbuilt — is unchanged and is
future row LA2.

---

## 4. The constructor — the stopping rule, the prior, and learning over runs

Joe's third exchange (`P-validated-R5.md:672-682`): a temperament alone selects too
little, all patterns too much; G should update *iteratively during construction*;
there may be a preferential-attachment method; the how/why landscape gives a prior
and is a substrate for learning over runs.

### 4.1 The loop

```
construct π repo:
  C₀ := cascade of π.seed.selected, provenance = found, precedence = π's authored order
  loop k:
    Sₖ  := CascadeState { under-construction = Cₖ, tension, record, flags }
    e   := fire π.temperament Sₖ                        -- the temperament proposes ONE edit
    case e of
      none        → stop (no temperament rule fires: saturation)
      some (halt r) → stop with reason r
      some edit   → Cₖ₊₁ := apply edit Cₖ ; if ΔG < ε then stop else continue
  return C_final with (stop reason, k) recorded on it
```

Three things this borrows and one it refuses.
**Borrowed** from `futon3a/holes/labs/M-memes-arrows/cascade_construct.py`: the
greedy-with-saturation shape (`:222-232`, stop test at `:230`, defaults ε = 0.15,
α = 0.3 at `:206`), the per-pattern inclusion prior (`base_rate_prior`, `:127-155`,
with the argument at `:129-137` for why a categorical collapses the cascade to size
1), and the downstream budget ceiling (`cascade_serve.py:21-27`, raised 6 → 20 by
operator ruling 2026-07-05, `"truncated"` reported at `:39`).
**Refused**: its substrate, on both counts slice (a) established — relevance is
MiniLM cosine (`:48-58,183-204`) and the edges come from a phylogeny whose descent
relation is substring containment (`futon6/scripts/pattern_phylogeny.py:22`,
`if y in t`). Law O2 (`P-validated-R5.md:500`): edges are authored, *"never inferred
from similarity, co-occurrence or prose."*

**The stopping rule is a `CascadeEdit`, not an outer loop condition.** That is the
one design choice in §4 worth arguing for: if the stop is a condition in the
runner, it is back in the Clojure the whole exercise is moving things out of, and
two temperaments cannot differ in where they stop. As a `halt Reason` edit it is
authored, readable before the run, and recorded on the cascade with the run — which
is what `snatch/widen-the-cascade-only-on-evidence`'s THEN already asks for
(*"stop on a rule stated in advance … Record the rule and where it stopped on the
cascade with the run"*).

### 4.2 The preferential-attachment prior, read off the authored landscape

Open question 4 from slice (a) — what replaces embedding relevance — is answered
here as a **proposal with arms**, recorded at `decisions.edn`
`:constructor-relevance-substrate`. The interim arm:

```
score(p | chosen, tension) := match(p, tension) · (α + degree(p, chosen))
  match(p, tension)   := the F2/F3 receipt — which clause of the tension p's IF ∧ HOWEVER
                         acknowledges, cited from p's TEXT (F3: "never the finder's score alone")
  degree(p, chosen)   := | { q ∈ chosen : authored @why or @how edge between p and q } |
                         i.e. O3's ReachOutside (Holes.lean:284-287) restricted to chosen
```

Both terms are authored, so O2 holds by construction rather than by audit. This is
the same functional form as `cascade_construct.py`'s `m'(p) = rel(p∣ψ) · (α +
connectivity(p, chosen))` with each factor's substrate swapped for one the laws
admit — deliberately, so that the loop remains the one that has been run and only
what it reads changes.

**Its cost, stated rather than hidden.** `degree` is zero for most candidate
patterns, because the library is mostly unorganised: **104 patterns in the authored
`@why` graph out of 1,253 files, 8.3%** after slice (b) (up from 85/1,239, 6.9%,
measured 2026-08-30, `P-validated-R5.md:459-461`). Where degree is zero the prior is
flat and the constructor is choosing on `match` alone. So the constructor's quality
is bounded by the rationale layer's coverage, which is exactly what the `L5`/`L9`
wave rows build. That is a dependency worth seeing: **the constructor does not
become good by being written; it becomes good as the library gets organised.**

### 4.3 Learning over runs, and the one line that keeps it legal

Joe: the landscape is *"a substrate for learning over multiple runs."* The mechanism
exists and is nearly empty: `futon6/data/pattern-phylogeny-learned.json` carries **0
descent edges and 2 co-application edges**, neither touching `snatch/` or `ants/`
(slice (a) §8iii).

The restatement proposes one restriction, and it is the whole of what keeps learning
inside law O2:

> **Learning may update the WEIGHTS on authored edges. It may never ADD an edge.**

So `w : Edge → ℝ`, updated by G-improvement per attachment step across runs; `edges`
fixed by authorship. A learned high weight is a hypothesis that an author should
write an edge — it is `find`'s and the spider's input, never `organise`'s. Stated as
a **proposed** law in §9 (O5); it is not adopted here.

---

## 5. Snatch on paper

| interface slot | Snatch instance |
|---|---|
| `Situation` / `Act` | the three-round game state / `{:act :offer/:accept/:denounce/:abstain}` (`checks/playout_snatch.clj:46-113`) |
| `Obs`, per treatment | `checks/snatch-outcomes.edn` — O1..O5, `:per-treatment` g1..g5, refined and coarsened per institution |
| `repo` | `library/snatch/` — 24 patterns after slice (b) (12 play, 6 design, 6 policy) |
| `Tension` | fixed: the snatch game itself. **This is why Snatch under-exercises `find`** — `find` was the identity on the twelve playing patterns (`P-validated-R5.md:374`) |
| `oracle` | `checks/playout_snatch.clj`, six scenarios, artefact `checks/snatch-cascade.edn` |
| `C` | **not stated anywhere.** `PolicyGrade.lean:13`: *"no probability, no preferences C, and no expected free energy."* The gap is the same one §2 of the record names |

**What the restatement changes here, concretely.** `patterns-overrides {}` (`:160`)
and `exchange-first-overrides {:exchange-when-both-sides-gain 0}` (`:165`) stop being
arguments and become the THENs of `snatch/play-the-authored-order-first` and
`snatch/lead-with-the-exchange-rule`. The S-G4 finding — same 18 patterns, +3 against
a snatcher under g4 with one wiring and −5 with the other — becomes a statement about
two *named, readable* policies rather than about two map literals. Slice (a) measured
that this is the only one of six scenarios where re-wiring moves the score, so the
restatement inherits a fixture with one discriminating row, not six.

**What it does not change.** Snatch cannot test `find`, and therefore cannot test the
constructor's prior: with a fixed 24-pattern repository and no varying tension,
`match` and `degree` have almost nothing to range over. Snatch tests `construct`'s
*stopping* and nothing else. A constructor validated on Snatch alone is validated on
the one axis Snatch has.

---

## 6. Ants on paper — and why it is the control

`library/ants/` holds 5 patterns: `baseline-cyber-ant`, `cargo-return-discipline`,
`hunger-precision-coupling`, `pheromone-trail-tuner`, `white-space-scout`.

| interface slot | Ants instance |
|---|---|
| `Situation` / `Act` | ant world tick / the EFE-scored action |
| `repo` | `library/ants/`, 5 patterns |
| `oracle` | `futon2/scripts/pattern_authority_gate.clj` — held-out yield sign test at λ ∈ {0.1, 0.5, 1.0} |
| actuator | the `@aif-delta` channel: `ants/cyber.clj:202` `attach-config`, `:213` `(update :aif-config merge-deep (:aif-delta config))`, reached at spawn via `ants/war.clj:266,269` |

**Ants is the domain where "outside patterns set the hyperparameters of the policy
playout" — Joe's fourth clarification — is already written down.** A pattern's THEN
*is* a parameter setting (`white-space-scout.flexiarg:9-15`, `:efe {:lambda {:info
0.6 :ambiguity 0.4}}`), and `baseline-cyber-ant:8-11` is the empty delta, described
in `futon2/holes/cascade-ants.edn:48` as *"the identity element of the pattern
algebra"* — the same role `play-the-authored-order-first` plays for temperaments.

**Two findings this instantiation turns up, both of which are work, not decoration.**

1. **The actuator takes one pattern, not a cascade.** `attach-config` has a single
   `pattern-key` argument (`cyber.clj:202-213`). A cascade of deltas has no
   expression today. It would be a small extension — fold the members' deltas in
   **precedence order** — and `merge-deep`'s semantics (`cyber.clj:193-200`, docstring
   *"later values taking precedence"*, `merge-with merge*`, last wins on a scalar) mean that **precedence would decide
   every key collision**. So in Ants, O4's "precedence is data" is not an abstraction:
   it is which λ the ant runs with. Future row LA4.
2. **The gate has never measured the channel it would need to.** Slice (a)
   established the scope limit at source: `pattern_authority_gate.clj:52-68`
   `activate-pattern` sets `[:config :aif :efe :lambda :pattern]` and
   `:cyber-pattern` directly and never calls `cyber/attach-config`. So the
   2026-07-16 verdict — *"AUTHORITY FAIL — no real pattern differed from `off` by
   held-out yield sign test at any lambda"* (`cascade-ants.edn:21-23`) — is about
   channel 2, the hard-coded EFE terms. Channel 1 is not dead code (unit-tested at
   `futon2/test/ants/cyber_test.clj:9`); **yield under it has never been measured.**

**Why Ants is the control.** Its gate refused every real pattern, with sound controls
(the sham tied `off` exactly on every seed). A constructor that only ever confirms
would be caught there. This is the property that makes Ants worth the second
instantiation and it is worth being blunt about the risk: **the honest expected
result of LA4 is that the constructor's cascades also fail the gate.** If they do,
that is a finding about the ants patterns or about G, and the restatement survives
it. If a run of LA4 is reported as a success without the gate's held-out sign test,
the control has been discarded rather than passed.

---

## 7. Zaif on paper — and the reviewability consequence

`futon3c/src/futon3c/agents/zaif_controller.clj`, 270 lines. Its `constants` map
(`:11-20`) is exactly what its own docstring says it is: *"Fixed, uncalibrated Z2 v0
constants."* `decide` (`:105`) chooses a retrieve/act/ask arm per decision.

| interface slot | Zaif instance |
|---|---|
| `Situation` / `Act` | a decision point in an agent turn / a tool round |
| `repo` | the whole `futon3/library` — 1,253 files, 104 in the authored graph |
| `Tension` | the task the seat was given. **This is the domain that exercises `find`**, and the only one of the four that does |
| `oracle` | the seat's persisted transcripts — `futon3c/src/futon3c/agents/zai_api.clj:803` *"U1: transcript persistence"*, semantic act records (tool + args + result digest) written as typed evidence entries — plus gate receipts |

The shape difference is the point. zaif v0 ranks individual actions with fixed
constants; the cascade version hands the runner **a carried, bounded program** —
Joe's ChipWits reading: a finite chip board, chips as production rules, watch-then-
rewire as learning over runs. The stopping rule is what makes the board finite.

**The reviewability consequence, which is the strongest practical argument for the
whole restatement:**

> A carried cascade can be **reviewed before the run**. Per-decision arm arithmetic
> cannot be: there is no object to read until the decisions have already happened,
> and `zaif_controller.clj`'s own docstring notes the model never sees the terms.

That is author ≠ reviewer applied to *behaviour* rather than to code — the same
separation `CLAUDE.md`'s handoff protocol applies to diffs. It also gives the
existing transcripts a second use: they are **retrodiction material** (§12, LA7).

---

## 8. ALFWorld — the stretch, and what it lacks

Named as the external-benchmark row (`P-validated-R5.md:693`). The interface makes
its cost precise in one line: **ALFWorld has `Situation`, `Act`, `Obs` and an
`oracle` for free, and no `repo`.** There is no authored pattern library for ALFWorld
and no authored `standsOn` relation over one. So LA6 is not "run the constructor on
ALFWorld"; it is "author a pattern repository for ALFWorld first, then run it" —
and the authoring is the expensive half. Recorded as a stretch row with that cost on
its face, so it is not later discovered as a surprise.

---

## 9. What changes in the laws

| law | as recorded | under the restatement |
|---|---|---|
| **F1** containment | `selected ⊆ repo.patterns`; empty ⇒ typed absence | **unchanged.** `construct` may admit only from `repo`, so F1 extends to it verbatim |
| **F2** receipted | every selected pattern carries why it matched | **unchanged, and now used twice** — the receipt is also the `match` term of §4.2's prior. The C61 amendment (`P-validated-R5.md:490-494`) is untouched and still open |
| **F3** non-self-certifying | a receipt cites text or authored edges, never the finder's score | **unchanged, and it is what refuses arm (b) of `:constructor-relevance-substrate`** — embedding relevance is refused by F3 as a warrant before O2 is reached |
| **F4** falsifiable | some pattern the finder must not return | **unchanged**, and it should be extended to the constructor: some pattern the constructor must not *admit* for a given tension |
| **O1** nodes are the input | prose: `cascade.nodes = selected` (`P-validated-R5.md:499`); Lean: `nodes = selected ∪ addedByOrganise` (`Holes.lean:340-344`) | **the Lean form holds; the prose equality does not.** A temperament's admissions land in `addedByOrganise`, which then means two different things — stood-on by up-closure, and admitted by a named temperament. Proposed **O1′**: split it, so every node carries a `provenance` — `found`, `stoodOn`, or `admittedBy p` naming the policy-grain pattern whose THEN admitted it. **Strictly stronger than either form**: it refuses a node whose origin is unrecorded, which neither the equality nor the union does |
| **O2** edges are authored | never inferred from similarity, co-occurrence or prose | **unchanged**, and it is the constraint §4.2 was designed against rather than around |
| **O3** fast-forward | reachability restricted to the selected set | **unchanged.** `ReachOutside` (`Holes.lean:284-287`) is also the `degree` term of §4.2's prior |
| **O4** precedence is data | recorded on the cascade, not derived from the patterns | **narrowed and strengthened.** `decisions.edn :o4-under-a-temperament-then` reads O4 as *not derived from the patterns it orders* — a temperament **writes** the precedence field, it does not compute it from members' contents. Proposed strengthening: **only a policy-grain THEN may write it**, which makes a precedence set by a runner argument the violation. That is checkable and is what `:160`/`:165` do today |
| **O5** *(proposed, not adopted)* | — | learning updates **weights** on authored edges and never adds an edge (§4.3). A learned weight is input to `find` and to authorship, never to `organise` |
| **S-G4** | `PatternWiring` inductive, two constructors, `patternScoreUnder` (`PolicyGrade.lean:136-154`) | the fixture becomes an instance of the general statement: G depends on `Policy`, not only on `Cascade.nodes`. The two constructors are `play-the-authored-order-first` and `lead-with-the-exchange-rule` |

---

## 10. What changes for the Lean obligations, and for L6 — stated explicitly

**`find` (`Holes.lean:264`, DELIBERATE IMPLEMENTATION REFUSAL).** Unchanged. The
restatement gives `find` no new obligation and removes none; its F1-F4 witnessed-
instance predicates (`:266-289`) are stated over recorded rows and are unaffected.

**`organise` (`Holes.lean:308`, `sorry`, DELIBERATE IMPLEMENTATION REFUSAL).** Changes
in **type** and in **status**, and both changes are worth stating because they point
in opposite directions.

- *Type.* `organise : Set P → Repository P → Cascade P` has no argument for the
  temperament. Under the restatement it is `organise : Cascade policy → Set P →
  Repository P → Cascade P`, or — better, and this is the restatement's actual
  claim — it is not a primitive at all but `construct` (§4.1), whose body is
  iterated `fire` at policy grain.
- *Status.* Its refusal note reads *"its recorded O1-O4 instance does not select one
  canonical implementation."* If `organise` is `fire` at policy grain, that
  justification weakens: the implementation *is* selected, up to which temperament
  cascade is supplied. So the hole could move from **implementation refusal** to
  **definable-once-`CascadeEdit`-exists**. It should not be moved on this document's
  say-so; it should be moved by whoever builds LA2 and can point at a running
  policy-grain rule.

**`CascadeDiff` (`Holes.lean:294`) and `wmCascadeDiffFixture` (`:319`).** `CascadeDiff`
already carries `selected`, `nodes`, `addedByOrganise`, `precedenceBefore/After` — so
O1′'s provenance is *nearly* expressible in it: `addedByOrganise` is the `stoodOn`
case, and `admittedBy` is the case it lacks. Splitting it is a one-field extension,
and `organiseO1NodesRecorded` (`:340-344`) would then be re-proved over a three-way
union rather than a two-way one — a re-proof, not a repeal. Named as part of LA2.

**The other three O-predicates are untouched by the restatement.**
`organiseO2AuthoredReachability` (`:347`), `organiseO3FastForward` (`:354`) and
`organiseO4PrecedenceGovernance` (`:361`) are all stated over `wmCascadeDiffFixture`'s
recorded fields. O4's Lean form is the S-G4 consequence (changed precedence changes
acting order or score), not the authorship claim, so §9's proposed strengthening —
*only a policy-grain THEN may write precedence* — has **no Lean statement today** and
would be a new declaration rather than an amendment.

**L6 — stated explicitly, since the row asks for it.** `L6` is *"kernel + compose on
`library/snatch` first (P-validated-R5 §5 step 3) … the generic find/organise path,
currently `sorry` at `Holes.lean:264,308`"*, acceptance *"against the pinned
`find-snatch.edn` fixture; laws F1-F4/O1-O4 as narrowed by the 2026-08-31 scope
amendment."*

- **L6 is not invalidated and its acceptance does not have to change.** Its laws are
  the *narrowed, witnessed-instance* forms over recorded rows (`findF1Containment`
  and siblings, each pinned to `find-snatch.edn` sha256
  `839897ef…7ef4dfa`). Those predicates are about recorded rows, and O1′ concerns
  what a *constructor* may put in `nodes` — a constructor L6 does not build.
- **The recommendation is therefore: run L6 against the pinned narrowed laws
  unchanged, and let the restatement add a successor row rather than edit L6.**
  Editing L6's acceptance to anticipate O1′ would make a blocked row depend on an
  unconfirmed restatement, which is the wrong direction of dependency: L6 already
  depends on `LJ1`.
- **The one thing L6 should be told:** if it defines `organise` generically with the
  three-argument type above, it will not have to be redefined when LA2 lands. If it
  defines the two-argument type, it will. That is a cheap piece of foresight and
  costs L6 nothing to take.

---

## 11. The candidate restated §1

This is the block that goes to Joe through `LJ1`. It restates the S1 fields of
`P-validated-R5.md:33-72`. **Changes from the recorded version are marked ⟨new⟩.**

```
problem:   G is computed over single actions from a channel-range C; "policy" is a
           borrowed name for a cascade, and a cascade in the code is a bag of pattern
           ids. Nothing states what G over a cascade IS, so nothing can be shown
           faithful to it, and the evaluate stage cannot say what its criteria do
           not cover.
           ⟨new⟩ And the thing that DECIDES a cascade -- which patterns are in it and in
           what order -- is not in the library at all. In Snatch it is two map literals
           in the runner (playout_snatch.clj:160,165) and one unreachable function
           (:139-143), and the wiring they choose is worth eight points against a
           snatcher under G4. A policy that cannot be read before the run cannot be
           reviewed, cited or refused, and G cannot be defined over it because there
           is no "it".

now:       - efe.clj:808 rank-actions [state candidate-actions]; core_efe.clj:94 g-efe over an action
           - fold_escrow.clj:113 cascade = (vec (get-in d [:cascade :pattern-ids]))
           - the 08-27 record: Q(o|pi) exists in three carriers, none conditioned on a cascade
           ⟨new⟩ - playout_snatch.clj:150-158 already fires patterns by one grain-polymorphic
             loop; :19-30 and :46-113 already index rules by grain with the grain a checked
             conjunct of every guard. The mechanism for a third grain exists and is unused.
           ⟨new⟩ - library/snatch now holds six policy-grain patterns (LA1 slice b, futon3
             1020330) that nothing executes: pattern-policy still takes `overrides` as a
             Clojure argument and there is no :policy conjunct.
           ⟨new⟩ - the constructor loop exists with the wrong substrate: cascade_construct.py
             :222-232 (greedy + saturation stop), relevance by MiniLM cosine (:48-58) and
             edges by substring containment (pattern_phylogeny.py:22) -- both refused by O2/F3.
           ⟨new⟩ - 104 of 1,253 library files are in the authored @why graph (8.3%).

solved:    (a property of the MODEL, checked before running -- v2 §1)
           A Lean statement, with an emitted clause and a Clojure mirror, in which
             Rule is INDEXED BY GRAIN, with play/design/policy three values of one type
               and the grain a checked conjunct of every guard;              ⟨new⟩
             Cascade is the §2.1d object (a DAG over patterns with authored edges),
               carrying a precedence field AND a provenance for every node
               (found / stood-on / admitted-by a named policy-grain pattern);  ⟨new⟩
             a Policy IS a cascade of policy-grain patterns plus the stopping rule its
               THENs emit -- not a fourth kind of thing, and not one of the three
               options the recorded §1 asked Joe to choose between;           ⟨new⟩
             organise is that cascade FIRED, not a separate function;         ⟨new⟩
             find, construct and act compose into run over an abstract Domain
               (Repository, Tension, playout oracle, per-treatment Obs and C), with at
               least two instances built, one of which is a control that has refused
               real patterns before;                                          ⟨new⟩
             Q(o|pi) is DERIVED by playout under the domain's oracle, per treatment,
               and is compared only within a treatment;
             G(pi) = risk + ambiguity over Q(o|pi) and C, quantified over ANY mission,
               and EVALUATED AT EACH ATTACHMENT STEP of the constructor, with the
               stopping rule stated in terms of it;                           ⟨new⟩
           and the model REFUSES, as theorems: a list of ids offered as a cascade; a G over
           a single action offered as G(pi); a C over channels offered as a C over outcomes;
           a prediction authored by hand (S-G3); a criterion set that cannot name an outcome
           it does not cover;
             ⟨new⟩ a precedence field written by anything but a policy-grain THEN;
             ⟨new⟩ a cascade node with no provenance;
             ⟨new⟩ a constructor whose relevance or connectivity term reads similarity,
               co-occurrence or prose (O2 and F3 applied to construction, not only to find);
             ⟨new⟩ a learned edge (O5: learning moves weights on authored edges, never adds one);
             ⟨new⟩ a policy that has run on one domain only.

facades:   [the recorded seven stand unchanged]
           ⟨new⟩ the reified runner -- a temperament written as a pattern that no runner reads,
             so the library gains six files and the behaviour stays in the Clojure. This is
             the state LA1 slice (b) actually leaves things in, and it is named here as the
             facade this restatement is most likely to become if LA2 is not built.

owner:     joe (commissioner); definition work by a Claude seat with Lean; validation by
           codex-22 per the excursion's dispatch note; witness != author
status:    open
```

**The question for Joe, in one line:** the recorded §1 asked him to choose *"identity,
distribution over, or cascade + acting order — ONE of these"* (`P-validated-R5.md:53-54`).
This restatement declines all three and offers a fourth: **a policy is a cascade, at a
grain whose operand is another cascade.** That is the thing to confirm, correct, or refuse.

---

## 12. Future rows — named here, built nowhere

Added to `worklist.edn` as `:blocked` on `LJ1` except where noted. None is started.

| id | class | what |
|---|---|---|
| `LA2` | C | the `:policy` grain conjunct, the `CascadeEdit` type, and a runner that reads a policy-grain THEN — what makes slice (b)'s six patterns execute. Includes `CascadeDiff`'s `admittedBy` field |
| `LA3` | C | the constructor loop (§4.1) over the authored substrate (§4.2), with the stopping rule as a `halt` edit |
| `LA4` | E | Ants instantiated: fold a cascade's `@aif-delta`s in precedence order through `attach-config`, and run `pattern_authority_gate.clj`'s held-out sign test on the result. The control row — its honest expected result is a refusal |
| `LA5` | E | Zaif instantiated: a carried cascade for one seat, `zai_api.clj:803` U1 transcripts + gate receipts as the oracle, reviewed before the run |
| `LA6` | E | ALFWorld — stretch. **Requires authoring a pattern repository first**; that is the expensive half and it is on the row's face |
| `LA7` | E | R5 generalisation, RETRODICTION: construct a policy from a completed item's statement as recorded and compare against the resolution the ledger records. Ground truth exists: `futon2/holes/labs/wm-contract/worklist.edn` 76 `:done` + 6 `:done-unreviewed` of 88; `futon3c/holes/tickets/tickets-index.edn` 8 `:done-in-fact` + 1 `:superseded` of 34 |
| `LA8` | E | R5 generalisation, CONSTRUCTION on open items — plausibility-reviewed only, **nothing enacted** (dark mode applied to the constructor). Denominators: 4 `:open` + 2 `:blocked` in wm-contract; 7 `:still-open` + 3 `:open` in tickets |
| `LA9` | E | learning over runs: weights on authored edges updated by G improvement; proposed law O5; `pattern-phylogeny-learned.json` is the existing carrier (0 descent, 2 co-app edges) |
| `LB1` | C | **not blocked** — the six `library/math-formalization/*.flexiarg` absent from `library/.spider/baseline-edges.edn` (1,241 digests against 1,247 files before slice (b)); they will fail `:argument-body-not-in-baseline` the first time that section is linted. From slice (b) `:also-found` |

Named and deliberately **not** made rows: a meta-temperament grain (§3.8) — there is
no instance of one anywhere, and a row for a thing with no instance is a wish.

---

## 13. Gates run for this slice

This slice touched **no Clojure, no `.flexiarg` and no Lean** — it adds this document
and edits `worklist.edn` and `decisions.edn` — so clj-kondo and
`futon4/dev/check-parens.el` have nothing in scope. Exit codes were captured to files
and tested, never read through a pipe.

| gate | invocation | result |
|---|---|---|
| `library_graph_lint_test` | `bb -cp . test/library_graph_lint_test.clj` (the C88 pin's invocation, `holes/labs/C88-pinned-library-evidence-index.md:15`) | see `:gates` on the ledger row |
| `spider_runner_test` | `bb -cp . test/spider_runner_test.clj` | see `:gates` |
| section lint, `snatch` | `bb -cp . checks/library_graph_lint.clj --library library --section snatch --baseline library/.spider/baseline-edges.edn --attestations library/snatch/attestations.edn` | see `:gates` |
| `worklist_check.bb` | in the ledger directory, before and after | see `:gates` |

**Nothing authored.** No `@why`, `@why-posthoc`, `@how` or `@see-also` was written or
edited by this slice; no edge was inferred from embedding similarity. `P-validated-R5`
law O2 and `decisions.edn :spider-editorial-standing` are untouched. The one `@why`
this document proposes — that a policy is a cascade at policy grain — is a claim in
prose put to Joe, not a directive in a pattern file.
