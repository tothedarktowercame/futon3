---
description: Generate a Post-Action Review (PAR) for the current session
argument-hint: [session-summary]
---

Generate a Post-Action Review (PAR) to capture learning from the current work session.

## What This Does

The `/par` command invokes the **reflect** peripheral agent to:
1. Analyze what was accomplished in this session
2. Identify patterns used (explicit or inferred)
3. Note what went well and what could improve
4. Capture prediction errors (expected vs actual outcomes)
5. Generate actionable suggestions for future sessions

## Usage

```
/par                          # Generate PAR from conversation context
/par "Fixed vitality endpoint" # Generate PAR with summary hint
```

## Process

1. **Gather Context**

   First, collect information about the session:
   - Review the conversation history for this session
   - Note any commits made (check `git log --oneline -10`)
   - Identify files that were modified (check `git status` or `git diff --stat`)
   - Look for any PSR/PUR records in the conversation

2. **Launch Reflect Agent**

   Use the Task tool to launch the `reflect` agent with the gathered context:

   ```
   Task: Launch reflect agent
   Prompt: Generate a PAR for this session. Context:
   - Summary: <$ARGUMENTS or inferred summary>
   - Commits: <commit list>
   - Files changed: <file list>
   - Conversation highlights: <key decisions and outcomes>
   ```

3. **Present PAR**

   Show the generated PAR to the user in a readable format.

4. **Offer to Log**

   Ask if the user wants to:
   - Log the PAR to MUSN activity stream
   - Save to a local file
   - Just view it (no persistence)

## PAR Structure

The generated PAR includes:

| Field | Description |
|-------|-------------|
| patterns_used | Patterns applied during session |
| what_went_well | Specific successes |
| what_could_improve | Friction points and issues |
| prediction_errors | Where expectations differed from reality |
| suggestions | Actionable improvements |
| lineage_trace | Commits, files, devmap items affected |

## Why Use PAR

- **Captures learning** that would otherwise be lost
- **Feeds stack_learning metrics** in the vitality system
- **Identifies patterns** for reuse in future sessions
- **Tracks prediction error** to measure improving accuracy
- **Builds collective knowledge** across agent sessions

## Example Output

```
## Post-Action Review

**Session**: Vitality endpoint implementation
**Duration**: ~2 hours
**Status**: Completed

### Patterns Used
- stack-coherence/devmap-progress (advanced f0/P0 to :qa)
- code-coherence/test-before-commit (verified compilation)

### What Went Well
- EWMA priors implementation was clean
- Interface coverage parsing worked first try
- Clear separation of concerns (personal/interface/stack levels)

### What Could Improve
- Spent time debugging namespace issues (should check requires earlier)
- Could have tested with real personal API data

### Prediction Errors
- Expected: Quick endpoint extension
- Actual: Required 3 new functions and careful null handling
- Magnitude: 0.4

### Suggestions
- Add integration test for vitality endpoint
- Consider caching devmap parse results
- Document the MUSN activity log schema

### Lineage
- Commits: 558e4de, 9995b57, ef69ff6
- Files: src/futon3/musn/http.clj
- Devmap: f0/P0 → :qa
```

Begin by gathering context about the current session, then launch the reflect agent.
