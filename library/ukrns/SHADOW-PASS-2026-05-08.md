# UKRNS Shadow Pass — 2026-05-08

Provisional notes from auditing `library/ukrns` against the current working paper draft (`UKRN_WP_draft_v11.md`), especially §§1–3.

These are not full patterns yet. They are short sketches to preserve live design claims that the current paper appears to be earning in plainer language than the older `ukrns` domain.

## Candidate Patterns

### 1. Separate Evidence Gaps from Implementation Decisions

- Working title: `ukrns/separate-evidence-gaps-from-implementation-decisions`
- Source in paper: §1.1–§1.2
- Claim: A decision-support paper should distinguish between questions the evidence must answer and decisions the implementing organisation properly owns.
- IF: unresolved evidence questions and operational choices are flattened together
- HOWEVER: treating implementation choices as paper-failures obscures ownership; treating evidence gaps as mere implementation detail obscures risk
- THEN: name open questions separately from hand-off decisions, and bind each to its proper closure path

### 2. Distinguish Delivery at Scale from Changed Practice

- Working title: `ukrns/distinguish-delivery-from-practice-change`
- Source in paper: §2.1, §1.1 Impact
- Claim: High delivery counts can show reach at network scale, but they do not by themselves show that research practice changed afterwards.
- IF: programme reporting treats attendance as if it were impact
- HOWEVER: delivery is measurable earlier and more cleanly than downstream behaviour change; the pressure to over-read delivery is strong
- THEN: report delivery as delivery, state what downstream evidence would be needed for practice-change claims, and keep the distinction visible in design decisions

### 3. Calibrate Impact Promises to What Indicators Can Actually Show

- Working title: `ukrns/calibrate-impact-promises-to-current-indicator-capacity`
- Source in paper: §2.2
- Claim: UKRN Services should promise demonstration only for those open-research practices that current indicator infrastructure can support credibly.
- IF: impact promises are made across a heterogeneous measurement landscape
- HOWEVER: some indicators are already pilotable while others still depend on immature standards, workflows, or manual checking
- THEN: narrow promises to the currently demonstrable subset, and treat the rest as development work rather than overclaiming

### 4. Join Up Training, Indicators, and Sector Positioning

- Working title: `ukrns/join-up-training-indicators-and-sector-positioning`
- Source in paper: §2.5
- Claim: Training delivery, indicators work, and sector-facing positioning are not three adjacent workstreams; they only become a learning system when they feed each other.
- IF: the three lines run in parallel
- HOWEVER: each line can look productive on its own, and institutional habit tends to preserve the separation
- THEN: make the connections operationally explicit: training produces evidence, indicators make outcomes legible, and sector positioning translates findings into wider traction

### 5. Treat Funder Cycles as Part of the Operating Environment

- Working title: `ukrns/treat-funder-cycles-as-operating-conditions`
- Source in paper: §2.3, §2.5
- Claim: Ringfenced funding cycles are not background noise; they are one of the conditions that determine what sector-scale work is possible and when.
- IF: service design treats funding context as exogenous scenery rather than as a live constraint
- HOWEVER: institutions often only take on collaborative culture work when dedicated resource is present; ignoring that rhythm makes the design misread its own landscape
- THEN: build planning, expectation-setting, and evidence interpretation around funder-active and funder-quiet periods

### 6. Name Direct Levers, Indirect Levers, and Out-of-Scope Conditions

- Working title: `ukrns/separate-direct-indirect-and-out-of-scope-levers`
- Source in paper: §3.3
- Claim: A service-level design becomes more credible when it says which conditions it can move directly, which only indirectly, and which it cannot move at all.
- IF: design recommendations are stated without leverage typing
- HOWEVER: overclaiming direct control is rhetorically attractive, while honestly naming out-of-scope conditions narrows the design's apparent power
- THEN: classify claimed effects by leverage type and keep the out-of-scope conditions visible

## Next Use

- Revisit these after §§4–5 are drafted.
- Promote only those sketches that the final paper actually earns under evidence.
- Where an older `ukrns/*.flexiarg` already overlaps, prefer revision over parallel duplication.
