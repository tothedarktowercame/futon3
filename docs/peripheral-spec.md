# Peripheral Specification (Draft)

Date: 2026-01-29
Status: :greenfield

## Concept

A **peripheral** is a constrained capability envelope for agent operation — like game modes
in a MUD/RPG (inventory mode, battle mode, shop mode, explore mode).

Constraints are **structural**, not behavioral:
- Agent can't accidentally exceed scope
- No "please be careful" compliance theater
- Memory travels with session-id across hops

## Peripheral Definition

Each peripheral specifies:
- **id**: Unique identifier (e.g., `explore`, `edit`, `reflect`)
- **tools**: Which tools are available
- **scope**: What files/paths are accessible
- **entry**: How to enter this peripheral
- **exit**: Natural exit points / hop triggers
- **context**: What gets passed in (session log, files, etc.)

## Core Peripherals

### Explore
```edn
{:id :explore
 :tools #{:read :glob :grep :bash-readonly :web-fetch}
 :scope :full-codebase
 :entry :default
 :exit [:found-target :ready-to-edit :user-request]
 :context {:session-id :inherit}}
```
Purpose: Understand the codebase, find relevant files, research.
Cannot: Edit files, commit, deploy.

### Edit
```edn
{:id :edit
 :tools #{:read :edit :write :bash}
 :scope {:paths ["src/" "docs/" "scripts/"]}  ; configurable
 :entry [:from-explore :user-request]
 :exit [:tests-pass :ready-to-commit :blocked]
 :context {:session-id :inherit
           :target-files :from-explore}}
```
Purpose: Make changes to code.
Cannot: Push to remote, deploy.

### Test
```edn
{:id :test
 :tools #{:read :bash-test}
 :scope :test-commands-only
 :entry [:from-edit :user-request]
 :exit [:pass :fail :flaky]
 :context {:session-id :inherit
           :changed-files :from-edit}}
```
Purpose: Verify changes work.
Cannot: Edit code (must hop back to edit if tests fail).

### Deploy
```edn
{:id :deploy
 :tools #{:bash-git :bash-deploy}
 :scope :git-push-only
 :entry [:from-test :tests-passed]
 :exit [:deployed :blocked]
 :context {:session-id :inherit
           :commit-message :from-edit}}
```
Purpose: Ship the change.
Cannot: Edit files, run arbitrary commands.

### Reflect
```edn
{:id :reflect
 :tools #{:read :musn-log}
 :scope :session-log-only
 :entry [:session-close :user-request :agent-request]
 :exit [:par-generated]
 :context {:session-id :inherit
           :session-log :fetch-from-musn}
 :output {:event/type "session/par"
          :to :musn-activity-log}}
```
Purpose: Generate PAR (Post-Action Review).
Cannot: Edit code, push, execute commands.

## Hop Mechanics

### Session-ID Transfer
```
Peripheral A (session-id: abc123)
    ↓ hop request
Peripheral B (session-id: abc123)  ← same session, full memory
```

The fucodex bridge demonstrated this with `--resume <thread-id>`.

### Hop Protocol
1. Agent recognizes exit condition (e.g., "ready to edit", "tests pass")
2. Agent requests hop: `{"hop": "edit", "reason": "found target file"}`
3. Orchestrator validates hop is allowed from current peripheral
4. Orchestrator spawns new peripheral with same session-id
5. New peripheral inherits context (files found, changes made, etc.)

### Agent Self-Initiation
Agent can request hop to reflect peripheral:
```json
{"hop": "reflect", "reason": "natural close point", "summary": "completed feature X"}
```

Orchestrator validates:
- Is session at a natural close point?
- Has meaningful work been done?
- Spawn reflect peripheral with session log

## Reflection Peripheral Implementation

### Input
```json
{
  "session_id": "abc123",
  "session_log": [...],  // MUSN activity entries for this session
  "changed_files": ["src/foo.clj", "docs/bar.md"],
  "commits": ["558e4de"]
}
```

### Process
1. Agent reads session log
2. Identifies patterns used (from PSR if present, or inferred from actions)
3. Reflects on what went well, what could improve
4. Notes prediction errors (expected vs actual)
5. Generates suggestions for future sessions

### Output (PAR)
```json
{
  "event/type": "session/par",
  "session/id": "abc123",
  "at": "2026-01-29T12:00:00Z",
  "patterns_used": [...],
  "what_went_well": [...],
  "what_could_improve": [...],
  "prediction_errors": [...],
  "suggestions": [...],
  "lineage_trace": {...}
}
```

Posted to MUSN activity log, aggregated by stack_learning.

## Gating

Hops can be gated by:
- **User approval**: "Agent wants to hop to deploy. Allow? [y/n]"
- **Condition**: "Only hop to deploy if tests passed"
- **Pattern**: "Use pattern X to decide if hop is appropriate"

## Relationship to Existing Code

### fucodex-chat-bridge.ts
Already implements:
- Session resumption via `--resume <thread-id>`
- Sandbox modes via `--sandbox`
- Constrained tool access via Codex SDK

Could be extended to:
- Accept peripheral spec on startup
- Emit hop requests to orchestrator
- Receive hop instructions

### MUSN Activity Log
Already supports:
- Logging events with session-id
- Querying events by session
- Activity entries for stack_learning

Needs:
- `session/par` event type handling
- Session log retrieval endpoint for reflect peripheral

### Stack HUD
Could show:
- Current peripheral mode
- Hop history
- PAR generation status

## Implementation Path

1. **Define peripheral specs** in EDN (this doc → `resources/peripherals/`)
2. **Add orchestrator** that manages hops (new module in futon3)
3. **Extend fucodex bridge** to accept peripheral constraints
4. **Add reflect peripheral** that generates PARs
5. **Wire to stack_learning** in vitality endpoint

## Open Questions

- Should peripherals be agent-specific or shared across models?
- How to handle mid-peripheral context overflow (agent crashes)?
- Should some peripherals be time-boxed?
- How to visualize peripheral state in Stack HUD?

## References

- Meeting notes: `fulab/meetings/2026-01-27-irc-coordination-review.md`
- fucodex bridge: `scripts/fucodex-chat-bridge.ts`
- PAR vocabulary: `futon5a/docs/joe-terminal-vocabulary.md`
