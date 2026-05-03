# M-PLAN: Native Plan Coherence
Status: archived

**Type**: Feature
**Status**: Phase 1 Complete, Phase 2-4 Pending
**Depends**: MUSN scribe, plan wiring evaluator
**Related**: `src/futon3/musn/service.clj`, `src/futon3/musn/plan_wiring.clj`, `dev/musn_stream_bb.clj`

---

## Objective

Ensure that all planning activity—whether via MUSN's EDN plan wiring or Claude's native TaskCreate/TaskUpdate tools—results in structured, viewable plan artifacts.

Goals:
1. Detect when agents use native planning tools
2. Convert native task lists to EDN plan format
3. Generate Mermaid diagrams for all plans
4. Surface planning hints when native tools are used

---

## Problem Statement

Claude Code provides native task management tools:
- `TaskCreate` - create a task with subject/description
- `TaskUpdate` - update status, add dependencies
- `TaskList` - list all tasks

These are useful but invisible to MUSN's plan system:
- No Mermaid diagram generated
- No plan evaluation (confidence/risk scoring)
- Not visible in notebook viewer or WebSocket stream
- No integration with mana/reward system

---

## Implementation Status

### Scribe Layer ✅

Added to `src/futon3/musn/service.clj`:

```clojure
;; Convert native tasks to plan wiring format
(tasks->plan-wiring tasks)

;; Convert native tasks to Mermaid diagram
(tasks->mermaid tasks)

;; Record a plan derived from native TaskCreate/TaskUpdate
(record-native-plan! session-id tasks :note "...")

;; Record detection of native planning tools
(note-native-planning-detected! session-id "TaskCreate" :task-id "1" :subject "...")
```

Event types:
- `:turn/plan` with `:source :native-task-list` - converted plan
- `:planning/native-detected` - hint event

### Emacs Display ✅

`contrib/fuclient-logs.el` handles:
- `planning/native-detected` events with gold hint styling
- Plan events show `[Plan diagram available]`

---

## Remaining Work

### Phase 1: Harness Integration ✅

**Goal**: Detect native planning in fuclaude/fucodex

Implemented in `dev/musn_stream_bb.clj`:
1. ✅ Real-time detection: Tool use events for TaskCreate/TaskUpdate/TaskList/TaskGet trigger `note-native-planning-detected!`
2. ✅ Batch conversion: At turn end, accumulated tasks are converted via `record-native-plan!`
3. ✅ HTTP endpoints: `POST /musn/scribe/native-planning`, `POST /musn/scribe/native-plan`

Detection flow:
```clojure
;; In musn_stream_bb.clj
(when (contains? native-planning-tools tool-name)
  (swap! state assoc :native-planning-detected true)
  (note-native-planning! session-id tool-name opts))

;; At turn end
(when (:native-planning-detected @state)
  (record-native-plan! session-id (:pending-tasks @state)))
```

### Phase 2: Plan Evaluation for Native Plans

**Goal**: Apply confidence/risk scoring to native plans

1. Convert native tasks to plan wiring format (done)
2. Run through `evaluate-plan-eval` from `plan_wiring.clj`
3. Award mana based on plan quality

```clojure
(let [wiring (tasks->plan-wiring tasks)
      actions (plan-wiring/evaluate-plan wiring)
      eval (plan-wiring/evaluate-plan-eval wiring actions)]
  ;; eval contains :musn.plan/confidence, :musn.plan/risk, :musn.plan/mana
  )
```

### Phase 3: HUD Integration

**Goal**: Show native plan status in HUD

1. Add "Plan" section to HUD when plan exists
2. Show Mermaid diagram link (like existing plan cards)
3. Show plan evaluation metrics (confidence, risk)
4. Add "[View Plan]" button that opens diagram endpoint

### Phase 4: Codex Adaptation

**Goal**: Equivalent planning coherence for Codex

Codex may not have TaskCreate/TaskUpdate, so:
1. Detect plan-like output in Codex responses
2. Parse markdown task lists or numbered steps
3. Convert to EDN plan format
4. Same downstream flow (Mermaid, evaluation, mana)

---

## Success Criteria

- [x] Native TaskCreate/TaskUpdate triggers `planning/native-detected` event
- [x] Task list is converted to Mermaid diagram at turn end
- [x] Plan appears in notebook viewer and WebSocket stream
- [ ] Plan evaluation scores native plans (Phase 2)
- [ ] HUD shows plan section when plan exists (Phase 3)
- [ ] Codex plans are captured (Phase 4)

---

## Design Decisions

### Why convert to EDN rather than store task list directly?

1. **Unified format**: All plans (EDN-native or task-derived) use same schema
2. **Mermaid generation**: `plan->mermaid` works on EDN wiring
3. **Evaluation**: `evaluate-plan-eval` expects EDN format
4. **Future-proof**: Can add plan transformations, optimizations

### Why not replace TaskCreate with MUSN-native planning?

1. **Ergonomics**: TaskCreate is built into Claude Code, familiar to users
2. **Flexibility**: Sometimes quick task tracking is enough
3. **Graceful degradation**: Native tools work even if MUSN is unavailable
4. **Coherence over control**: We want visibility, not replacement

---

## Related Patterns

- `code-coherence/plan-before-code` - Plan before implementing
- `musn/plan-sketch-coherence` - Proposed pattern for this mission
- `contributing/pr-summary-structure` - Plans inform PR summaries

---

## Notes

This mission exemplifies the "coherence over control" principle: we don't force agents to use a specific planning tool, but we ensure all planning activity flows into the same observable structure.
