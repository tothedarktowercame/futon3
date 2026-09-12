# IAD adapter: patterns, institution definitions, and participation

Design proposal, codex-25 with Joe, 2026-09-12. The library move is authorized;
this adapter and its runtime bindings are not implemented or commissioned.

Joe's correction is the starting point: an agent may clock into an institution,
not merely a mission. An institution defines an action situation and the rules
for participating in it. A mission identifies work. An agent can participate in
several institutions while doing one task; one institution can govern many tasks.
A time clock can anchor a participation receipt, but does not supply its meaning.

## Three different artifacts

1. **Pattern**: a reusable argument about context, tension, response, and rationale.
2. **Institution definition**: a versioned rule configuration, with named authority,
   scope, role eligibility, monitoring and decision procedures. One pattern can
   support several institutions; one institution can cite several patterns.
3. **Participation record**: this actor occupies this position in this institution
   instance, at this version, for this scope. Entry, actions, findings, and departure
   refer to the same record. Selection of a pattern alone does not establish entry.

## General adapter contract

Proposed interface:

```clojure
(adapt-pattern pattern-packet context interpretation)
;; => {:candidate {...} :gaps [...] :source-links [...]}
;; or {:error {:type :invalid-source :detail ...}}
```

Use `futon.flexiarg.projection/parse-file` for the source packet. Preserve the ID,
full content hash, clause names and exact source passages. The context supplies
an action situation and the actors or accountable services involved. Interpretation
may be authored by an agent, a person, or a previously reviewed template. It is
input to the adapter, not permission for that adapter to adopt rules.

For any syntactically valid pattern, return a candidate, possibly with most of its
institutional fields unresolved. Do not promise an executable institution from
arbitrary prose. Reject malformed source; expose ambiguity and missing provisions.
A generated candidate must distinguish **source-supported**, **proposed**, and
**unresolved** content per rule. A source link does not establish that a proposed
interpretation follows from it; that is a separate review claim.

The seven rule types are targets for interpretation, not a fixed field conversion:

| Rule type | Required question |
|---|---|
| Position | Which roles exist, and which are accountable for each duty? |
| Boundary | Who is eligible for each position; how are entry, exit and removal decided? |
| Choice | What may, must, and must not each role do under specified conditions? |
| Aggregation | Whose decisions authorize an outcome, who can veto, and who resolves disagreement? |
| Scope | What outcomes and domains can this institution affect, and what lies outside them? |
| Information | What observations, reasons and receipts reach whom, when, with what visibility? |
| Payoff | Who bears costs or receives benefits; what adopted consequences attach to conduct? |

Monitoring, challenge/appeal, amendment, and enforcement bindings accompany these
seven types. Monitoring describes which predicate is actually observed, the
observer, trigger, evidence receipt and coverage limitations. A social procedure
is representable without pretending it is mechanically enforced. A rule that is
inapplicable needs a reason; a missing rule needs a gap, not an empty success.

Keep at least three independent statuses: interpretation completeness, adoption
by the named authority, and enforcement readiness. An adopted social rule is not
a commissioned mechanical gate. A commissioned detector does not adopt its rule.

## Entering an institution

Proposed record shape (illustrative, not a deployed schema):

```clojure
{:participation/id "new-stable-id"
 :institution {:id "workshop/pattern-maintenance" :version "definition-hash"}
 :actor {:id "codex-25" :session-id "current-session"}
 :position :proposer
 :scope {:pattern "workshop/revise-patterns-from-observed-use"}
 :task-ref nil                       ; optional, never fabricated
 :mission-clock-ref nil              ; optional link to an existing clock event
 :pattern-selection-ref "psr-receipt"
 :entry {:request-id "deduplication-id"
         :authority-ref "adoption-receipt"
         :eligibility-verdict "entry-check-receipt"}
 :status :entry-pending}
```

The entry request names the institution/version and position. Resolve ambiguity
before entry. Check eligibility, the definition's adopted scope, conflicting
roles for this same claim/producing part, and the actual readiness required by
that institution. Append a durable entry receipt before reporting `:active`.
If persistence fails, report apparatus failure, not successful entry. Repeated
request IDs must return the same participation, not stamp in twice.

An attempted action refers to the active participation and applicable rule IDs.
A decision records permitted/refused/unknown/apparatus-fault separately, together
with the evidence used. A refusal generates the Layer 4 capture event. Departure
records outcome, actual clause use, outstanding obligations, and applicable
witness receipts; it does not erase obligations or imply successful completion.
Concurrent institutions retain separate role/scope records. Their constraints
compose without granting extra permissions; conflicts require a typed finding
and authorized resolution. Superseding an institution version requires an explicit
migration or grandfathering decision, not silent reinterpretation of old entries.

“Clock in” therefore needs a target: “clock in to pattern maintenance as proposer”
is different from “clock in on mission X.” With no target, ask which institution
or use an already explicit conversational binding. Voxterm's current clock-in
meaning is mission-oriented; revising it is part of the future integration, not
an existing institution-entry operation.

## Worked first candidate: pattern maintenance

Sources: `open-proposals-named-adoption`, `distinguishing-cases-before-adoption`,
and `revise-patterns-from-observed-use`. Proposed institution:

- **Position**: proposer, affected participant, independent reviewer, adopter.
- **Boundary**: a participant can propose or challenge; reviewer eligibility excludes
  the producing part of the claim being reviewed; Joe currently occupies adopter.
  Exact identity/eligibility checks remain a commissioning gap.
- **Choice**: proposer records an observation and a proposed operation; reviewer
  examines motivating and contrasting cases; adopter records a reasoned decision.
- **Aggregation**: review is advice and can identify unmet prerequisites; only Joe
  adopts workshop-family changes. An objection and its disposition remain visible.
- **Scope**: retain, revise, merge, split, retire, or reorganize workshop patterns
  and their cascade links. No authority over unrelated work or automatic penalties.
- **Information**: participants can inspect the pinned proposal, source clauses,
  evidence, review, ruling and resulting diff. Visibility limits must be explicit.
- **Payoff**: proposal and review costs are recorded against their participants;
  maintenance credit requires an applied change and its verification. Allocation
  of actual budgets and any sanctions remain unresolved decisions.

For a first institutional episode, choose one observed mismatch in the process
language, enter as proposer, select the source pattern via PSR, and submit a
versioned maintenance proposal. Independent review and Joe's ruling occur within
that same episode. If adopted, the diff and source/sigil-pin checks support its
closure. A record saying “corrected” without the edit must not close the episode.
An explicitly deferred candidate is legitimate without an edit and must remain
visibly deferred. These are the first contrasting cases.

## Existing integration points, and their limits

Inspected in futon3c:

- `social/peripheral.clj`: peripheral definitions and validated hops already
  represent entry/exit and context transfer. `PeripheralSpec` is not an IAD schema;
  institution roles and authority cannot simply be renamed tool permissions.
- `peripheral/adapter.clj`: tool mappings and prompt construction can render an
  adopted definition into agent-facing guidance. Prompt text is not enforcement.
- `agency/clock_lineage.clj` and `agency_send.py --mission`: existing mission clock
  lineage is an optional reference, not an institution identifier.
- `transport/http.clj` PSR/PUR/backpack handlers: selection/use receipts can link to
  participation. The current backpack has one active pattern per agent; it cannot
  represent several concurrent institutional positions. Keep participation separate.
- Evidence append/readback: use the existing evidence system for institutional
  receipts after its schema has been explicitly extended. Do not add an isolated
  shadow ledger or claim new receipt types already exist.

Important binding condition: the inspected PSR handler updates the backpack even
when evidence append reports failure. An institution-entry adapter must check the
actual evidence result and readback; a top-level HTTP success is insufficient.
That finding is a requirement for the adapter, not repaired by this design note.

## Small implementation sequence and refusal criteria

1. Define the candidate/interpretation schema and pure validator. Use the three
   workshop process patterns plus one unrelated pattern to demonstrate that gaps
   remain explicit instead of being filled by a workshop-specific assumption.
2. Prepare the worked maintenance candidate with pinned clause provenance, an
   independent review, and an adoption decision. No decision is manufactured here.
3. Add durable participation entry/action/departure bindings to existing evidence
   and peripheral mechanisms. Show one admitted entry and one ineligible entry;
   inject stale pins, missing authority, duplicate requests and storage failure.
4. Connect a voice/keyboard request only after these receipts exist. Run one
   maintenance episode through entry, review, decision and an honestly classified
   departure. Reconcile entries with departures so missing outcomes remain visible.

Any claimed mechanical gate needs an induced-breach refusal and an admissible-case
acceptance through the real path. Pending apex choices about execution versus
acceptance/credit remain explicit and do not weaken any current gate. Costs,
observed coverage and limits are outputs of the experiment. This document neither
launches the experiment nor adopts a general sanctions regime.
