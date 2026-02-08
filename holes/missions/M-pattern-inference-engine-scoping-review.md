# Scoping Review: `M-pattern-inference-engine.md`

Date: 2026-02-08

This review checks the mission doc against the 5 required elements requested in the handoff: `owner`, `scope`, `success_criteria`, `time_box`, `exit_conditions`.

## Checklist

| Element | Status | Notes |
|---|---|---|
| Owner | **Missing** | No single accountable owner is named; only implied actors (Agency/Codex). |
| Scope | **Partial** | Strong technical design + phased plan, but no explicit `scope-in` / `scope-out` boundaries. |
| Success criteria | **Present** | The doc has a concrete checklist under “Success Criteria” (5 items). |
| Time box | **Missing** | No duration, deadline, or bounded “stop condition” is specified. |
| Exit conditions | **Partial** | “Success Criteria” implies exit, but there is no explicit statement of what “done” means operationally (what must be merged/deployed, what can be left behind). |

## Evidence (From The Mission Doc)

Success criteria exists as a checkbox list under **“Success Criteria”**:
- “PUR creation triggers `guided-by` link materialization”
- “Agency tracks ephemeral `carrying` facts”
- “Can query ‘agents carrying nearby patterns’”
- “Pattern backlinks show both direct (PUR) and inferred (guided-by) links”
- “Docs traced through code back to patterns”

Scope is implied by the title/subtitle and “Implementation Phases”, but there is no explicit “not doing X” section.

## Gaps And Fixes (Suggested Edits To `holes/missions/M-pattern-inference-engine.md`)

Add a short scoping header near the top (after the title) like:

1. **Owner**
   - Name exactly one owner (person or role) and a handoff rule.

2. **Scope**
   - `Scope-in`: enumerate which repos/dirs/functions this mission will touch (even if “TBD”), and which facts/links are in-bounds.
   - `Scope-out`: explicitly exclude adjacent work already mentioned as “vision” (e.g., full query unification, broad embedding infra hardening) if it is not required for closure.

3. **Time box**
   - Pick a concrete time box (example: “2 days for Phase 1, 3 days for Phase 2; stop after 1 week and cut scope if incomplete”).

4. **Exit conditions**
   - State operational exit conditions, e.g.:
     - “Merged to main + tests passing” (or “landed behind a feature flag”).
     - “Backfill not required in this mission” (or explicitly required).
     - “Docs updated (which doc), demo script updated (which script).”

5. **Success criteria (tighten if needed)**
   - Current list is good (verifiable), but it would benefit from tying each checkbox to an artifact:
     - a test file name, a demo command, or a screenshot/log excerpt requirement.

## Note: Alignment With `mission-scoping.flexiarg`

`library/futon-theory/mission-scoping.flexiarg` asks for: `SUCCESS-CRITERIA`, `SCOPE-IN`, `SCOPE-OUT`, `OWNER`, `DEPENDENCIES`.

Even though the handoff asked for `time_box` and `exit_conditions`, it would be low-cost and high-value to also add:
- **Dependencies**: what must exist first (or what this mission blocks on), with links to the specific missions/files.

