# IAD→peripheral compositor (design; zai-7 with Joe, 2026-09-14)

The adapter runs pattern → institution candidate. The compositor runs
the other end: institution definition (+ a participation record) → a
PERIPHERAL BUNDLE the agent actually carries. Joe's framing: sometimes
one rule is in effect; other times a complex arrangement with multiple
rules composed. The compositor's job is that composition.

Grounding: peripherals are futon3c's constrained capability envelopes
(social/shapes.clj PeripheralSpec: id, tools, scope, entry, exit,
context, config; hop protocol preserves session-id; README-peripherals
is the agent contract). The existing peripheral/adapter.clj renders
guidance from specs — prompt text, not enforcement. Same honesty here.

## Contract

```clojure
(compose-bundle [institution-def version] participation-records)
;; => {:cards [...]        ; pattern cards (the rules in force, cited)
;;     :prompts [...]      ; rendered rule guidance per position
;;     :watchers [...]     ; proposed lint predicates (NOT gates)
;;     :conflicts [...]    ; typed findings where rules disagree
;;     :basis {...}}       ; institution hashes + participation ids
```

Input: one or more ADOPTED institution definitions (adapter output,
post-ruling) and the actor's active participations. Output is what the
agent carries — the same slot the backpack fills for one pattern,
generalized to a bundle.

## Composition laws (the actual design content)

C1 **Permissions intersect; obligations and prohibitions union.** Two
institutions in force jointly permit only what each permits; both must
be satisfied. This is the adapter's existing "compose without granting
extra permissions," made operational.

C2 **Conflicts are typed findings, never resolutions.** If
institution A must and institution B must-not on the same act for the
same actor/scope, the compositor emits a conflict record naming both
rule ids; resolution is an authorized act (aggregation rule of a
governing institution, or the operator), not compositor judgment.

C3 **One rule in effect = the degenerate case.** A single institution
with one binding rule composes to one card + one prompt + its watcher.
No bundle machinery pays for itself until the second institution
lands — build the degenerate case first (it is EX-1's 🎒 plus a
versioned card).

C4 **Cards carry versions and provenance, like sigils.** A card cites
institution id + definition hash + the clauses grounding its rules
(adapter source-links, inherited). A stale definition hash makes the
card's guidance :stale — same rule as the sigil registry.

C5 **Positions, not agents, are composed.** The bundle is keyed to the
actor's POSITIONS across participations. Concurrent positions in
different institutions compose under C1/C2; the same institution's
conflicting roles for the same claim stay excluded by entry
eligibility (adapter's reviewer-independence rule).

C6 **Watchers watch, gates gate.** Compositor output never installs a
mechanical gate; it proposes watcher predicates (cue-receipt, card-use
class) that existing review may adopt. Prompt text is not enforcement
(peripheral/adapter.clj's own limit, inherited).

## Worked example: the ◈ candidate composed

WORKED-CANDIDATE-records-carry-warrant.md, if adopted, composes for an
emitting agent as:

```clojure
{:cards [{:institution "inst/records-carry-warrant-v0"
          :hash "<definition-hash>"
          :rules-in-force [:choice :information]   ; the emitting position's
          :clauses ["+ THEN" "+ conclusion"]}]
 :prompts [{:for :emitter
            :text "every claim about the record ships with derivation,
                   inputs digest, version stamps, or a typed :not-proven"}]
 :watchers [{:predicate warrantless-citation-check
             :proposal "flag citations lacking receipt — REVIEW to
                         adopt; not a gate"}]
 :conflicts []
 :basis {:participation "<id>" :adopted-by "joe" :hashes {...}}}
```

Degenerate case: one card, one prompt, one watcher proposal. Composed
case (◈ + ⇄ + an ⚖-ruling institution): three cards; permissions
intersect (⇄'s price-and-route constrains ◈'s emit-freely); obligations
union (◈'s attach-warrant + ⇄'s route-feedback); any must/must-not
clash lands in :conflicts with both rule ids cited.

## Existing vs proposed (honest split)

EXISTS: PeripheralSpec + hop protocol + social/peripheral.clj
entry/exit; peripheral/adapter.clj prompt rendering; PSR/PUR single
card; the shapes' structural scope envelopes.

PROPOSED: the bundle shape; C1/C2 composition arithmetic; versioned
cards; conflict findings; watcher-proposal emission. None built; this
document is design + spec, per the advisory's budget scope.

Sequence when built: (1) degenerate case = version the existing PSR
card (carry institution hash, not just pattern id); (2) two-institution
composition with one planted conflict, demonstrating C2's typed
finding; (3) watcher adoption through review — one induced violation
per adopted watcher before it is trusted (the standing commissioning
rule).
