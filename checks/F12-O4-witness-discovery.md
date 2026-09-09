# F12: why the whole-library constructor does not exercise O4

2026-09-09 · codex-17 · discovery for claude-1, Agency packet
`invoke-1788965923930-16472-ef9fe427`. No implementation or ruling is changed.

## Finding and measured basis

The construction loop **is wired and fires**. Execution of the resulting
library members **is not wired**; it is neither a refused playout nor a playout
that happened to select nothing. The projection into a cascade deliberately
records empty precedence vectors and omits acting-order and score observations.
Moreover, neither constructed member set intersects Snatch's executable play
rules. This is not a missing call to a ready-made whole-library interpreter.

Read basis:

- futon3 HEAD `704762abe9991f76bbe703eba7c3cce1066a5968`.
- mathlib4 HEAD `0222969e1ed5181b374b9d04bd0031ed5ac20a08`.
- Constructor read-digest, independently recomputed:
  `8b20e68b50e777765eccbe5182284e773253b513fd7e7232989fbfc3b7c97672`.
- futon2 HEAD at final source check: `e4a50b8193b3268ec42ab783bd91113e313f1980`.
- Worklist F12: `futon2/holes/labs/wm-contract/worklist.edn:1400-1403`.
  It is blocked, with owner `:any`; that is not a named semantic author.

I reran the existing discovery instrument in a separate process, with output
redirected outside the repository:

```sh
# cwd /home/joe/code/futon3
bb -cp checks ../futon2/holes/labs/wm-contract/f12_naturalistic_attempt.clj \
  > /tmp/f12-o4-naturalistic.edn
# exit 2: unexercised exemplar, not a constructor failure
```

It reproduces the recorded digest and both construction sizes. The constructor
gate passes; each row has O1/O2/O3 true, O4's implication true, precedence-moved
false, and these missing fields:
`[:acting-order-before :acting-order-after :score-before :score-after]`.
The instrument checks the same facts at
`futon2/holes/labs/wm-contract/f12_naturalistic_attempt.clj:14-25` and refuses the
unexercised case at its final exit. No report generator or experiment output was
written in the repository.

## 1. Which O-laws, and what counts as exercised?

The F12 acceptance still says “existing Cascade carrier” and cites the old
Holes line 861. The later ruled carrier is the relevant refinement, not a reason
to return to the earlier, weaker interface:

- `mathlib4/DarkTower/WarMachine/F12Conformance.lean:5-11` explicitly explains
  why the earlier `Cascade` function conformance states only O1–O3: one
  precedence field, no acting order or score, and no admission attribution.
- `mathlib4/DarkTower/WarMachine/F12DischargeArm.lean:239-245` shows that a
  conformant selected-only implementation can ignore temperament altogether.
  Existence or swappability of such a function does not witness O4.
- `mathlib4/DarkTower/WarMachine/F12RuledCarrier.lean:20-40` gives the ruled
  function and all seven clauses. Its output is `CascadeDiff`, whose observation
  fields are at `mathlib4/DarkTower/WarMachine/Holes.lean:930-942`.

For inputs temperament `t`, selected space `sel`, authored repository `repo`,
and attributed admissions `adm`, write the output as `d`:

| Law | Ruled requirement |
| --- | --- |
| O1 | `d.nodes = sel ∪ d.addedByOrganise ∪ d.admittedBy`. |
| O2 | Every organised edge has reachability in `repo.standsOn`. |
| O3 | Organised edges are **exactly** the fast-forwards through authored relations with endpoints in `d.nodes \ d.addedByOrganise`. Omitted edges can fail it as well as invented ones. |
| O4 | `d.precedenceBefore ≠ d.precedenceAfter` implies `d.actingOrderBefore ≠ d.actingOrderAfter OR d.scoreBefore ≠ d.scoreAfter`. |

The other three clauses bind selected space, authored relation and admissions
to their actual inputs (`osel`, `oauth`, `oattr`). They must survive any adapter.
Executable counterparts: `futon3/checks/find_organise.clj:519-549`.

An **exercised** O4 witness supplies an actual before/after precedence change
and measured order/score observations from the corresponding executions, with
at least one consequent different. The consequent is a disjunction: the law
does **not** require the score itself to improve, or even to change if acting
order changes. The commissioned example nevertheless owes score observations,
as well as order observations, rather than omitting whichever field is unused.
The current evidence checker requires all six fields and a true antecedent;
field presence alone is not provenance or a meaningful score.

Why the stronger empirical requirement matters:
`mathlib4/DarkTower/WarMachine/F12RuledCarrier.lean:53-74` constructs a seven-clause conformant function with
both precedence lists empty, both acting lists empty and default scores. Its O4
proof is by contradiction from the false antecedent. The module explicitly
records that limitation at `mathlib4/DarkTower/WarMachine/F12RuledCarrier.lean:104-125`. Conversely,
`mathlib4/DarkTower/WarMachine/F12RuledCarrier.lean:139-162` changes precedence while keeping order and score
flat and proves failure at O4 alone. This is a useful negative control, not an
empirical demonstration that an adapter plays its rules.

The older closed instance
`mathlib4/DarkTower/WarMachine/Holes.lean:999-1007` witnesses only its named
fixture, not all organise implementations. Also, Joe's later exemplar ruling
requires a real working example outside ants, mining and Snatch:
`futon2/holes/labs/wm-contract/RULINGS-walkthrough-2026-09-08.md:66-83`.
A new Snatch rerun may be a control or bridge, but does not discharge that
naturalistic proviso.

## 2. Exact path and why the output is empty

1. `futon3/checks/find_organise.clj:115-146` reads authored IF/HOWEVER text,
   spans and relation directives. It does **not** compile arbitrary pattern
   text into executable IF/HOWEVER/THEN functions.
2. `futon3/checks/construct_cascade.clj:658-685` reads the whole library,
   constructs the cue-based seed and candidate context, runs find, and calls
   `run` followed by `cascade-of` for each temperament.
3. `futon3/checks/construct_cascade.clj:281-316` supplies three executable **policy-grain**
   rules: halt on budget, halt at the marginal-gain floor, and admit the best
   supported candidate. The two temperaments select different stop rules
   (`futon3/checks/construct_cascade.clj:324-334`).
4. `futon3/checks/construct_cascade.clj:376-397` constructs the initial state and calls
   `fo/construct`. That loop calls the shared `fo/fire` and applies typed edits
   (`futon3/checks/find_organise.clj:405-440`, `futon3/checks/find_organise.clj:448-488`). Construction is
   real execution of these rules, on a policy-grain state.
5. `futon3/checks/construct_cascade.clj:402-421` projects the final member set into a graph:
   selected, admitted, added, nodes, authored fast-forward edges. It deliberately
   writes `:precedence-before []` and `:precedence-after []`. There is no call
   here to `snatch/play`, any other domain actuator, or an outcome scorer.
6. `futon3/checks/construct_cascade.clj:732-735` checks only O1–O3 and emits
   `:o4 :not-exercised-nothing-is-played`. Thus PASS describes the constructor's
   stated checks; F12's wider witness obligation is still open.

`git blame` attributes all five O4 comment/empty-vector lines to `d73c4d56`
(2026-09-02). The empty projection is deliberate original scope, not drift
introduced by this morning's correspondence/cycle repairs.

A separate, read-only census called `c/run`, `c/cascade-of`, `fo/ordered`, and
intersected each qualified member set with qualified `snatch/play-rules`.
Its output is `/tmp/f12-o4-census.edn`:

| Temperament | Nodes / edges | Actual construction firings | Executable Snatch play members |
| --- | --- | --- | --- |
| Budget | 20 / 3 | 3 admit-rule firings, 1 budget halt | 0 |
| Floor | 41 / 14 | 24 admit-rule firings, 1 floor halt | 0 |

Both runs start with 17 members. Their final constructor states have populated
precedence maps and ordered member lists, but those are **construction state**,
not observed domain acting order. The seed is sorted by identifier and
admissions are appended (`futon3/checks/construct_cascade.clj:376-389`,
`futon3/checks/find_organise.clj:426-433`); that deterministic order is not automatically an
authored domain policy.

The budget's Snatch members are `have-a-temperament`,
`widen-the-cascade-only-on-evidence`, and `lead-with-the-exchange-rule`.
The floor additionally has `play-the-authored-order-first` and
`promote-the-remedy-before-the-exit`. These are policy/advisory members, not the
play rules defined at `futon3/checks/playout_snatch.clj:212-216`.
`futon3/checks/playout_snatch.clj:134-158` implements two policy edits, while
`futon3/checks/playout_snatch.clj:535-553` explicitly names unexecuted policy cases.
In particular, exchange-first requires the exchange rule in its operand; neither
naturalistic member set contains that play rule. Finding a policy-pattern name
in the graph is not sufficient to run its intended construction on that graph.

## 3. Smallest honest repair, and decisions before implementation

**There is no supported one-call playout repair for these two outputs.** Reuse
one of the actual constructed member sets, but first author and review a bounded
execution interpretation for a coherent naturalistic task it serves. Resolve
actionable members to typed rules, retain non-acting/support/uninterpreted
members with explicit accounting, and supply a domain transition and outcome
recorder. Then run the **same** frozen cascade/task basis before and after one
warranted precedence change through the shared firing machinery. Observe rule
ids, actual actions, resulting state and the declared outcome score; bind all
six O4 fields to those observations. Reject unresolved required rules, missing
observations and the flat-effect mutation. Preserve O1–O3 and the three input
bindings, then transcribe the real record through the ruled Lean signature.
This is the smallest structural packet I can justify; choosing its semantic
content is still a prerequisite, not an implementation default.

The budget and floor runs cannot simply be the before and after arms: they
change membership and stop rules. A changed result would not isolate precedence.
Likewise, sending their members to Snatch's policy would find none of its
executable play rules, and its fallback is `:no-pattern` abstention
(`futon3/checks/playout_snatch.clj:200-207`). Dropping the unfamiliar namespaces, filling scores
with zero, or copying admission order into acting order would change or invent
the witness.

A narrower **policy-grain** naturalistic witness might be possible using the
already executing library-construction rules. O4's type does not prescribe a
game or exclude that grain. But its record would then concern a cascade of
constructor rules, not execution of the 20/41 library-member cascade. The
current three rule ids are not a resolved action interpretation of those member
sets. Reusing that route requires an explicit account of which `P`, selected
space and authored relations the witness inhabits, and review that this task
satisfies Joe's reusable-tool proviso. It must not be relabelled as playing the
existing output. I have not run or chosen this alternative.

| Needed decision | Authority / proposed routing |
| --- | --- |
| Which naturalistic task and actual result make this a reusable tool? Which output cascade, or a separately declared policy-grain cascade, is the subject? | Joe owns the exemplar purpose/proviso; claude-1 routes a concrete proposal for review. |
| Which member IF/HOWEVER/THEN clauses have executable interpretations, at which grain? Which members are supporting or out of executable domain? | Library/content lane authors; a different reviewer checks source spans and interpretation. No implied delegation from `:owner :any`. |
| What initial state, actor, allowed effects and environment transitions supply a real attempt? | Task/domain owner with implementation lane; independent reviewer pins the interpretation. |
| Whose baseline precedence and which warranted change? How are ties resolved? | Existing authored rule if it settles the question; library author/reviewer verifies it. Any unresolved preference belongs to Joe, not identifier order or my default. |
| What does score measure, over what horizon, and who judges the result? | Domain owner proposes a witnessed measure; Joe rules any new preference. Independent reviewer checks it is informative about the task. Do not call an arbitrary proxy canonical G. |

The implementation packet can follow once these are concrete and pinned.
A successful finite witness still does not prove universal behavioural
sensitivity: it supplies the commissioned example alongside the function's
Lean conformance and swappability obligations. The F12 acceptance also retains
the staged implementation-refusal amendment; this discovery releases neither
that amendment nor a machine run.

## 4. Reuse from F11 and the existing constructor

- **Firing:** `futon3/checks/find_organise.clj:405-419` already provides the
  grain-polymorphic IF/HOWEVER/first-emitting-THEN loop. Reuse it, not a copied
  finder/interpreter. Resolution of rules and domain transitions belongs to
  the adapter, as the constructor contract says at `futon3/checks/find_organise.clj:448-462`.
- **An important correction:** F11 instruments antecedent evaluation and query
  receipts; it did not build a generic whole-library action interpreter.
  `futon3/checks/find_snatch.clj:39-103` observes the existing finder.
  `futon3/checks/find_snatch_evidence.clj:211-231` wraps the existing policy, returns
  `snatch/pi-patterns` unchanged, and runs `snatch/play`. Before/after executed
  THEN observations for a new domain still need their own adapter.
- **Pins and checks:** `futon3/checks/find_snatch_evidence.clj:37-81` reads the committed,
  reviewed manifest and refuses drift/missing executable pins; `futon3/checks/find_snatch_evidence.clj:216-231`
  checks the basis again after the run. Reuse that protocol with an F12-specific
  manifest and independent reviewer pin, not by expanding or self-repinning the
  closed F11 manifest.
- **Evidence:** canonical state digest (`futon3/checks/find_snatch_evidence.clj:23-35`),
  pinned text citations (`futon3/checks/find_snatch_evidence.clj:93-115`), complete accounting (`futon3/checks/find_snatch_evidence.clj:140-160`) and
  mutation checks are reusable mechanisms. Their existing schemas and
  expectations are Snatch/F11-specific. F12 needs its own schema, subject,
  source-derived expectations and controls for actual effect observations.

## Verification and limits

Discovery instrument exit **2**, as expected; separate member/precedence census
exit **0**. No play experiment, Lean build, live JVM load, registry change or
implementation edit was performed. No claim is made that every executor in the
workspace was searched: the zero-overlap result concerns the proposed existing
Snatch playout reuse specifically.

One diagnostic `git blame` was initially issued from the mathlib4 directory and
failed with “no such path”; the rerun in futon3 produced the attribution above.
Pointers in this note were checked against current files; the old line numbers
inside upstream docstrings are historical citations, not this note's authority.
Markdown-only change: clj-kondo and Lisp check-parens are not applicable.
`git diff --check` passes. Existing sigil-index changes and the untracked spider
receipt remain outside the packet.

Captured diagnostic output SHA256 values (temporary files, not new authorities):

- naturalistic receipt: `f3f9c52bc77f28d904428cfa26b564636159fe864507f9acb7c88abee52fff35`
- member/precedence census: `edf42821ccb6459768988433d34991828beaa8a51813a38e51ff4fa6aad3cb0a`
