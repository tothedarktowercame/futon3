# Scoping Review: M-futon1a-rebuild

Reference pattern: `library/futon-theory/mission-scoping.flexiarg`

This review checks whether `holes/missions/M-futon1a-rebuild.md` contains the 5 required scoping elements:
`owner`, `scope`, `success_criteria`, `time_box`, `exit_conditions`.

## Assessment (Present vs Missing)

### owner (explicit agent assignment)
- Status: MISSING
- Evidence: No explicit "Owner:" / "owner:" / assigned agent section found in the mission text shown.
- Risk: Diffuse ownership; hard to coordinate handoffs and decide what counts as "done" for the current owner.

Suggested addition:
```md
## Owner
Owner: <single agent/person>, effective <YYYY-MM-DD>
Handoff: explicit note required to transfer ownership
```

### scope (clear in/out boundaries)
- Status: PARTIAL (implicit, not explicit)
- Evidence:
  - The doc is very detailed about goals, invariants, tensions, architecture, and process gates.
  - It does not clearly separate "in scope for this mission" vs "explicitly out of scope / deferred".
- Risk: Scope creep; the doc reads like a full product spec plus methodology manifesto.

Suggested addition:
```md
## Scope
### Scope In
- Build futon1a as described, including Part I (process artifacts) + Part II (storage subsystem)
- Target repo layout/modules: <list top-level dirs/files>
- Canonical HTTP interface + graph-memory contract for downstream tools

### Scope Out
- Not implementing UI/client tooling beyond minimal compatibility smoke tests
- Not adding performance optimizations beyond correctness invariants (eg caches) unless required by success criteria
- Not migrating all legacy futon1 features unless explicitly enumerated
```

### success_criteria (measurable outcomes)
- Status: PRESENT
- Evidence: The doc contains a dedicated `## Success Criteria` section, split into Part I/Part II, and many items are verifiable.
- Notes:
  - The mission-scoping pattern recommends 1-5 criteria; this doc currently contains substantially more.
  - That is fine as an internal checklist, but it helps to elevate 3-5 top-level criteria that act as the mission "contract".

Suggested tightening (optional):
```md
## Success Criteria (Top-Level)
1. Part I gate satisfied (evidence doc + canonical patterns + module map + PSR/PUR exemplar + traceability example).
2. Part II invariants: proof tests for I0-I4 are green and wired to code + docs.
3. API + restart durability: POST/GET/restart/GET passes; error layer mapping validated.
4. Migration rehearsal: export/import checksum + invariant proofs + rollback plan documented.
5. Stability run: 30 consecutive days without silent failures (define what telemetry constitutes "silent").
```

### time_box (estimated effort)
- Status: MISSING
- Evidence: No time estimate / budget / milestone schedule present.
- Risk: Mission never terminates; work expands to fill available time.

Suggested addition:
```md
## Time Box
- Total budget: <e.g. 2-4 weeks part-time> or <N focused days>
- Phase budgets:
  - Part I (process gate): <N days>
  - Part II (core invariants + API): <N days>
  - Migration rehearsal: <N days>
- Review checkpoints: <dates> (scope can be revised at checkpoints)
```

### exit_conditions (when to stop)
- Status: PARTIAL (gates exist, but not explicit stop rules)
- Evidence:
  - Part I has an explicit gate: "Do not begin Part II until all items are satisfied."
  - The success criteria list implies completion, but does not define stop rules for overruns or de-scoping.
- Risk: "Almost done" forever; unclear when to pause/split.

Suggested addition:
```md
## Exit Conditions
Stop this mission when any of the following is true:
- Success criteria (top-level) are met; create follow-on missions for nice-to-haves.
- Time box expires; re-scope (split into smaller missions) and mark remaining work :blocked or new missions.
- A blocking dependency is missing for ><N days> (e.g. XTDB constraint, pattern library gaps); pause and file a dependency mission.
- Core invariant proof cannot be satisfied without expanding scope beyond agreed boundaries; split/renegotiate scope.
```

## Summary
- Present: `success_criteria` (strong, verifiable), partial `exit_conditions` via Part I gate.
- Missing: `owner`, explicit `scope in/out`, explicit `time_box`, explicit mission-level `exit_conditions`.
- Recommended next step: add short sections for Owner/Scope/Time Box/Exit Conditions near the top of `holes/missions/M-futon1a-rebuild.md` so the mission has bounded ownership and a clear finish line.
