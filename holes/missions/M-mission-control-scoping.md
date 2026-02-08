# M-mission-control-scoping

Apply mission-scoping pattern systematically across all mission docs.

## Owner
**Primary:** Codex
**Reviewer:** Claude Opus
**Handoff:** Bell back to Claude when complete; push to git for review.

## Status: :active

## Context

Scoping reviews of M-futon1a-rebuild and M-pattern-inference-engine (2026-02-08)
found the same gaps in both: missing `owner`, missing `time_box`, partial `scope`,
partial `exit_conditions`. Only `success_criteria` was consistently present.

This mission applies those fixes across all outstanding mission docs.

## Scope

### Scope In
- All files in `holes/missions/M-*.md` that are NOT `:done`
- Add missing scoping elements per `library/futon-theory/mission-scoping.flexiarg`
- Use the two completed scoping reviews as templates:
  - `holes/missions/M-futon1a-rebuild-scoping-review.md`
  - `holes/missions/M-pattern-inference-engine-scoping-review.md`

### Scope Out
- Do NOT change mission content, architecture, or implementation plans
- Do NOT start or advance any mission work
- Do NOT modify missions marked `:done` (e.g., M-agency-unified-routing)
- Do NOT create new missions

## Tasks

For each mission doc (excluding `:done`):

1. [ ] Check for the 5 required scoping elements:
   - `owner` (explicit agent/person assignment)
   - `scope` (clear in/out boundaries)
   - `success_criteria` (measurable outcomes)
   - `time_box` (estimated effort or deadline)
   - `exit_conditions` (when to stop, including overrun rules)

2. [ ] Add missing elements directly to the mission doc
   - Add an `## Owner` section if missing
   - Add `### Scope In` / `### Scope Out` if missing
   - Add `## Time Box` if missing
   - Add `## Exit Conditions` if missing
   - Leave `success_criteria` alone if already present

3. [ ] Set `**Status:**` to one of: `:greenfield`, `:scoped`, `:active`, `:blocked`, `:review`, `:done`
   - If the mission now has all 5 elements: `:scoped`
   - If it was already `:active` or `:blocked`, keep that status

## Success Criteria

1. Every non-`:done` mission doc has all 5 scoping elements
2. Every mission doc has an explicit `**Status:**` field
3. Changes are additive only (no content removed)
4. All changes committed and pushed to git

## Time Box

- Budget: 1 focused session (aim for < 30 minutes)
- If any mission doc is ambiguous enough that scoping elements can't be filled in,
  mark those elements as `TBD` with a note and move on

## Exit Conditions

- All mission docs updated: done
- Time box exceeded: commit what you have, bell Claude with status
- Git push fails: commit locally, bell Claude to pull
- Blocked on content questions: mark `TBD`, don't guess

## References

- `library/futon-theory/mission-scoping.flexiarg` - the pattern
- `library/futon-theory/mission-lifecycle.flexiarg` - valid status values
- `holes/missions/M-futon1a-rebuild-scoping-review.md` - example review
- `holes/missions/M-pattern-inference-engine-scoping-review.md` - example review
