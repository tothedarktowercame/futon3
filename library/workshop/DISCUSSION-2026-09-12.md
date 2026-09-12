# Joe / zai-7 discussion: durable idea inventory

Recorded jointly by codex-25 and zai-7 at Joe's request, 2026-09-12.
This index connects the ideas in `*zai-repl:zai-7*` to their durable homes;
it is not a new implementation checklist or a declaration that the designs run.
[Selected operator excerpts](discussion-excerpts-2026-09-12.json) preserve the
source wording and a hash of the captured buffer. Source line numbers below refer
to that snapshot. Earlier claims and counts in the conversation are historical,
not current runtime findings. In particular, the reported “13 fully done” counted
a half-row as full; this inventory does not endorse that tally.

## Coverage

| Idea / source discussion | Durable home | Status and qualification |
|---|---|---|
| Stakeholder-specific agent harnesses; one board per role or task (5–133) | [IBOL/AIF](../../../futon2/holes/labs/wm-contract/NOTE-ibol-to-aif.md), [chip-board spec](../../../futon2/holes/labs/wm-contract/SPEC-chip-boards-v0.md) | Design recorded; arbitrary new role boards are not thereby commissioned. |
| Graph adjacency replaces maze tiles; hops carry evidence and precision; cascades supply board structure (36–98) | [IBOL/AIF](../../../futon2/holes/labs/wm-contract/NOTE-ibol-to-aif.md) | Proposed interpretation with typed queries, not license to infer connectivity from a picture. |
| Do not permanently prohibit self-rewriting harnesses; use explicit versioned board transitions (158–192) | [chip-board spec](../../../futon2/holes/labs/wm-contract/SPEC-chip-boards-v0.md) | Joe's design direction; ratification and continuity requirements still apply. |
| Hierarchical AIF must connect individual harnesses to crew moderation; no skipped authority levels (192–227) | [chip-board spec](../../../futon2/holes/labs/wm-contract/SPEC-chip-boards-v0.md), [handoff algebra](../../../futon2/holes/labs/wm-contract/SPEC-handoff-algebra-v0.md) | Required design criterion; trace validation and deployment must be assessed separately. |
| Inbox-zero as record homeostasis and recursion base; aligned runtime and Lean witnesses (227–439) | [inbox-zero/AIF](../../../futon2/holes/labs/wm-contract/NOTE-inbox-zero-aif.md), [mission](../../../futon2/holes/missions/M-zaif-harness-v1.md) | Notes include implementation history; current enforcement not re-audited here. |
| Derive crew needs from real backlog; verification can be the bottleneck (439–580) | [agent-needs survey](../../../futon2/holes/labs/wm-contract/NOTE-agent-needs-from-issue-board.md) | Dated survey, not a perpetual priority or current count. |
| Read operator turns as evidence of unmet capabilities; preserve counterpart differences and avoid embedding-hub collapse (580–652) | [landscape reader](../../../futon2/holes/labs/wm-contract/NOTE-operator-landscape-reader.md) | Proposed cartography; associations are not stakeholder authority. |
| Make questions askable through typed tools; separate observation, prediction and causal intervention (652–718) | [landscape reader](../../../futon2/holes/labs/wm-contract/NOTE-operator-landscape-reader.md) | Tool manifest and identifiability limits recorded; an embedding does not establish causation. |
| Futon City as a predictive playout landscape; possible capability buildouts (674–786) | [landscape reader](../../../futon2/holes/labs/wm-contract/NOTE-operator-landscape-reader.md), [task manager](../../../futon2/holes/labs/wm-contract/NOTE-task-manager-outer-loop.md) | Simulated futures remain distinct from enacted capability. |
| Task manager receives priorities; checks automatability; capability arrival triggers targeted reconsideration (718–786) | [task manager](../../../futon2/holes/labs/wm-contract/NOTE-task-manager-outer-loop.md) | Design and attributed rulings recorded; no new autonomous priority authority. |
| Bells push, clocks attribute, packets admit; idle and mid-turn conditions matter (718 onward) | [task manager](../../../futon2/holes/labs/wm-contract/NOTE-task-manager-outer-loop.md), [handoff algebra](../../../futon2/holes/labs/wm-contract/SPEC-handoff-algebra-v0.md) | Separate channels, not interchangeable entry mechanisms. |
| Accumulate roughly 10–15 operator turns across sessions; read an unordered constellation before authoring wiring (786–824) | [landscape reader](../../../futon2/holes/labs/wm-contract/NOTE-operator-landscape-reader.md) | Proposed pressure-driven cadence; a cheap per-turn scan was considered, not selected as a requirement. |
| Separate cartographer from wiring author; preserve batch provenance and proposal precision (824–844) | [landscape reader](../../../futon2/holes/labs/wm-contract/NOTE-operator-landscape-reader.md) | Role separation recorded; mapping associations does not authorize a board. |
| Recursive expansion boards expose bounded interfaces, return values and fuel costs (804–824) | [chip-board spec](../../../futon2/holes/labs/wm-contract/SPEC-chip-boards-v0.md) | Design constraint; nesting must not erase limits or authority. |
| One pattern can be read as guidance, production rule, or institution; institutions constrain edges as well as nodes (844–966) | [pattern interpretations](../../../futon2/holes/labs/wm-contract/NOTE-pattern-interpretations.md), [IAD adapter](IAD-ADAPTER.md) | Interpretations need provenance. The initial nearly-mechanical IAD mapping was corrected by review. |
| Institutions need applicability predicates with version/freshness evidence (844 onward) | [pattern interpretations](../../../futon2/holes/labs/wm-contract/NOTE-pattern-interpretations.md), [IAD adapter](IAD-ADAPTER.md) | Scope and participant eligibility are distinct; ambiguous applicability remains a gap. |
| Beer/VSM as a way to model a firm and its stakeholder channels; capability/client/pipeline strata (1017–1074) | [VSM/AIF note](../../../futon2/holes/labs/wm-contract/NOTE-vsm-aif.md) | Research/design analogy; its equivalence and effectiveness are not demonstrated by being recorded. |
| Company stand-ins express simulated demand and satisfaction criteria; calibrate against real engagements (1039–1074) | [VSM/AIF note](../../../futon2/holes/labs/wm-contract/NOTE-vsm-aif.md) | Synthetic demand stays typed and cannot become operator preference or an automatic scheduling order. |
| Complex crew handoffs need typed preconditions, provenance, authority and accountable outcomes (1074–1092) | [handoff algebra](../../../futon2/holes/labs/wm-contract/SPEC-handoff-algebra-v0.md) | Formalization requirement; this inventory does not certify its implementation. |
| Apex witnessed progress, detector commissioning, recorded policing cost and honest unknowns (1092–1404) | [workshop library](README.md), [institution draft](../../../futon2/holes/labs/wm-contract/DRAFT-apex-institutions.md), [sigil registry](../../../futon2/holes/labs/wm-contract/sigil-registry.edn) | Library admitted; outstanding severity, blocking and signature-builder rulings remain explicit. Cost claims require measurement. |
| Record the difference between narrated correction and applied correction (1336–1404) | [maintenance pattern](revise-patterns-from-observed-use.flexiarg), [institution draft](../../../futon2/holes/labs/wm-contract/DRAFT-apex-institutions.md) | Worked failure/repair episode; neither a universal success claim nor an automatic sanction. |
| Shared operator/agent cue language; cues bind to actions and checkable receipts, inbox-zero style (1428–1500) | [cue institution](../../../futon2/holes/labs/wm-contract/NOTE-voxterm-cue-institution.md), [adapter cue protocol](IAD-ADAPTER.md) | Voice rendering exists; obligation interpretation, correlation and watcher remain proposals. |
| Agent self-talk can issue its own card/retrieve/ask/act/yield cues without acquiring operator authority (1458 onward) | [cue institution](../../../futon2/holes/labs/wm-contract/NOTE-voxterm-cue-institution.md), [IAD adapter](IAD-ADAPTER.md) | Emitter/intent/authority must be recorded; asking Joe does not impose an obligation on him. |
| Card carriage differs from cited application; departures feed maintenance (1394 onward) | [EX-1](EX-1-pattern-card-entry.md), [Layer 4](LAYER4-PROCESS.md) | Use citation is not detector commissioning; unused cards can be reported honestly. Return-queue binding is not implemented. |
| Institution clock-in means actor + role + adopted rules; mission clock optional (later Joe/codex-25 clarification) | [IAD adapter](IAD-ADAPTER.md), [cue institution](../../../futon2/holes/labs/wm-contract/NOTE-voxterm-cue-institution.md) | Design direction recorded; neither glyph occurrence nor PSR alone admits a participant. |

## Reconciliation performed in this pass

zai-7's coverage review, Agency job `invoke-1789242601412-20503-63ed7e2a`,
landed in futon2 `ac6a463e`. It records the institution-clock distinction and
library admission, corrects card-use/commissioning conflation at its source,
and distinguishes a proposed watcher from existing gates. codex-25 checked the
diff, linked the broader notes here, and expanded the IAD adapter's missing
operator/agent cue layer.

zai-7 flagged that EX-1 lacked a dedicated artifact. A summary already existed
in LAYER4-PROCESS.md; [EX-1 now has its own file](EX-1-pattern-card-entry.md),
including the illustrative records, original proposal provenance, unresolved
bindings, contrasting outcomes and review criteria. No experiment is launched.

The old signature audit was also flagged as stale against subsequent row work.
Its historical classifications must not be reused as current authorization.
Re-derivation belongs with the worklist owner; it is not performed by this idea
capture. Claims in the source notes that checks are free/cheap, runtime bindings
are automatic, or theoretical mappings are identities remain claims to assess,
not evidence supplied by this index.
