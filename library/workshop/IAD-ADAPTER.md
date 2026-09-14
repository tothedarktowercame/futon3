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

## Cue protocol from the Joe/zai-7 discussion

Recorded 2026-09-12 after joint coverage review; see
[the idea inventory](DISCUSSION-2026-09-12.md) and
[the cue institution note](../../../futon2/holes/labs/wm-contract/NOTE-voxterm-cue-institution.md).
Joe proposed the same cue language for operator turns and agent self-talk.
The following protocol details are design requirements, not deployed handlers.

A cue occurrence is first interpreted as a request, self-commitment, discussion,
negation, or unresolved intent. Rendering a glyph is not that determination.
Keep the source turn/event and occurrence ID, emitter identity and kind,
addressee, intent verdict and its provenance, requested action, institution/
version/role/scope, and the authority or delegation being exercised. Glyphs and
untrusted text fields cannot authenticate the emitter. If intent or target is
unclear, record a clarification request before creating an execution obligation.

An authorized operator request binds the addressed agent within the applicable
rules. An agent's own cue can create a self-commitment within its existing
permissions, or a proposal to enter when eligibility/authority is missing. It
cannot authorize itself merely by emitting the glyph. Asking the operator is a
request for a decision, not an obligation imposed on Joe by the agent. Receiving
an operator request also does not dispense with missing scope or current gates.

Suggested obligation states are pending, acknowledged, clarification-needed,
discharged, refused, cancelled and overdue. Acknowledgement is not discharge.
Correlation uses the cue occurrence and obligation IDs, not just a session ID:
one receipt cannot satisfy several unrelated cues. Record the completion
condition, deadline or triggering review event, cancellation/supersession policy,
and receipt lineage. Those policies need adoption; no timeout is invented here.
A watcher can cheaply check structured correlation once intent is established;
a regex cannot establish conversational intent, substantive compliance or cost.

The two proposed checks have different meanings:

- **Cue to disposition/receipt:** did an actual request or accepted self-commitment
  get the required response, evidence or explicit blocker by its due event?
- **Pattern selection to use outcome:** did departure report a clause actually
  consulted, or explicitly report unused, superseded, abandoned or unknown?
  Selecting a card creates no obligation to fake its use. A citation alone is not
  proof of good application, and neither is induced-violation commissioning.

The proposed zaif controller can interpret its own explicit externalized cues
between turns: retrieve selects a card; act attempts an authorized action; ask
requests missing information or authority; yield returns control. These are arm
bindings to design and test, not literal equivalences: clock-in is not the work,
and yielding without active participation cannot mint an institutional departure.
Private reasoning need not be exposed; commitments must use a recorded event.

Departure outcomes feed the Layer 4 maintenance intake, retaining links to source
clauses and observed tensions. This return path needs an implemented consumer and
receipt before it can be called an automatic queue. EX-1's proposed wiring is
recorded in [its experiment file](EX-1-pattern-card-entry.md).

## Dispatch-economy bindings (advisory 2026-09-14; zai-7 pass, design+spec only)

Source: futon2/holes/NOTE-zai7-institution-advisory-2026-09-14.md
(claude-15, commissioned by Joe). The adapter's outputs — institution
candidates, participation records, maintenance episodes — are
themselves deliverables in the dispatch economy, so the four find-snatch
F-laws and the advisory's five mechanism gaps bind the adapter
contract, not just the WM lanes. Per binding: its snatch ancestor, its
enforcement point in THIS adapter, and its F4 falsifier.

**B1 — Containment (F1).** A candidate interprets only its source
pattern; "no institution warranted" is a typed, creditable outcome
(advisory gap 5: not-building is scoreable). Enforcement: the
candidate's :source-links must cover every source-supported field;
a field without a passage is PROPOSED, never source-supported.
Falsifier: a candidate whose source-supported fields cite passages the
pattern does not contain (over-interpretation dressed as reading).

**B2 — Receipted congruence (F2 + gap 1).** The adapter's dual failure
pair mirrors the dispatch pair: under-interpretation (gaps silently
filled with workshop assumptions) and over-interpretation (inventing
rules the pattern never grounded — scope drift at interpretation
grain). Enforcement: every delivery (candidate, episode, departure)
QUOTES ITS ASK back — the episode's acceptance bar, the pattern's
actual clauses — and everything beyond the asks is listed and priced
as :beyond-asks, never silently credited. Falsifier: a delivered
institution rule that neither cites a clause nor appears in
:beyond-asks (unlisted drift — the review's scope-fidelity item).

**B3 — Non-self-certifying interpretation (F3).** The interpreting
agent's own assessment never certifies that a proposed interpretation
follows from its source (the adapter already says this); the binding
adds: the CONGRUENCE call — is this the institution the pattern
grounds? — belongs to the independent reviewer position, never the
adapter or its author. Enforcement: adoption-status and
interpretation-follows are separate review claims with separate
receipts. Falsifier: an adopted candidate whose "follows from source"
receipt is authored by the candidate's own producing part.

**B4 — Falsifiable adoption (F4 + gap 2).** Every candidate declares
its zero-mass member: the tempting-but-unsupported provision the
pattern does NOT ground, whose appearance in the adopted institution
is a detectable violation. (Worked example: the apex-institutions v1
tables' "Ostrom would say…" — an attractive attribution with no
supporting passage; the F4 declaration would have named it at
adoption time.) Enforcement: adoption without a declared falsifier is
incomplete. Falsifier: the declared tempting provision appears in the
institution with no new source having been added.

**B5 — Consumer-naming (gap 3, the R8 lift).** Every interpretation
names its intended consumer at creation time: which institution
instance, which position reads this rule. No consumer materialized →
the candidate retires, not explains — making "was this needed?"
decidable in advance rather than undecidable after. Enforcement:
:consumer is a required candidate field; unnamed consumers surface in
the gap list. Falsifier: a candidate whose named consumer never
materializes and which is nonetheless maintained (kept alive by
explanation instead of retired).

**B6 — Refusal pricing, second-order (gap 4).** Adapter refusals and
clarifications (invalid source, ambiguity, missing provision) cost a
pinned, falsifiable blocker note — codex-22's 95-line sha-pinned
blocker is the exemplar. The dual distortion (over-refusal as
effort-avoidance) is priced by the loss-floor analog: a refusal must
name what it forecloses, so cheap abstention is distinguishable from
priced abstention. Enforcement: refusal receipts carry :forecloses.
Falsifier: a run of refusals whose :forecloses entries repeatedly name
work that later proves trivially unblocked.

**B7 — Budget as institution variable (gap 5).** Candidates and
episodes carry token-price estimates against their named consumers;
deferred candidates are typed absences WITH credit. With scarcity live
(~11% at advisory time), not-interpreting a pattern no institution
will consume is a scored saving, not a failure. Enforcement:
:price-estimate on candidates; deferred-with-credit as a legal
disposition. Falsifier: deferred-with-credit granted to candidates
whose consumers then materialize (deferral was evasion, priced wrong).

These are rules-in-form for adoption packet-by-packet; no machinery is
built by this section, and each binding's enforcement point names
existing adapter surfaces (source-links, status triples, review
positions, gap lists) rather than new ones. The snatch ancestors are
cited in the advisory; the game corpus (futon3:checks/find-snatch.edn,
Holes.lean:914-955) is the pinned precedent for all four F-laws.
