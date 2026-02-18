---
description: Pattern Use Record - record outcome of pattern application
argument-hint: [outcome]
---

Record the outcome of applying the currently active pattern.

## What This Does

The `/pur` command completes the pattern learning cycle:
1. References the pattern from the last /psr (or asks which pattern)
2. Records what actions were taken guided by the pattern
3. Captures the outcome (success, partial, failed)
4. Notes prediction error (expected vs actual)
5. Optionally updates pattern quality metrics

## Usage

```
/pur                     # Record outcome for active pattern
/pur success             # Quick success record
/pur "partial - tests pass but perf issue remains"
```

## Process

1. **Check Active Pattern**

   Look for the pattern activated by the most recent /psr in this session.
   If no active pattern, ask which pattern was used.

2. **Gather Actions**

   Review what was done since the PSR:
   - Files modified (`git diff --stat` or conversation history)
   - Commands run
   - Decisions made

3. **Get Outcome**

   If $ARGUMENTS provided, use as outcome hint. Otherwise ask:
   - **Success**: Pattern applied, problem resolved
   - **Partial**: Pattern helped but incomplete
   - **Failed**: Pattern didn't fit or didn't help
   - **Pivoted**: Switched to different pattern mid-work

4. **Assess Prediction Error**

   Compare expected vs actual:
   - What did you expect would happen when applying this pattern?
   - What actually happened?
   - Magnitude: low (close match), medium (some surprise), high (very different)

5. **Generate PUR**

   ```
   ## PUR (Pattern Use Record)
   - **Pattern**: agent/pause-is-not-failure
   - **Actions taken**: Paused work, documented blockers in issue #42,
     asked for clarification on test requirements
   - **Outcome**: success
   - **Expected**: Would unblock after clarification
   - **Actual**: Got clear requirements, tests now passing
   - **Prediction error**: low
   - **Notes**: Pattern fit well; pause was 2 hours, resolution was quick
   ```

6. **Clear Active Pattern**

   Remove the pattern from the "backpack" - ready for next /psr cycle.

7. **Persist to Evidence Landscape**

   After recording the PUR, persist it to the evidence landscape using curl.
   Link it to the PSR evidence entry via `in-reply-to` (using the evidence ID
   saved from the /psr step).

   ```bash
   curl -sf -X POST http://localhost:${FUTON1A_PORT:-7071}/api/alpha/evidence \
     -H "Content-Type: application/json" \
     -d '{
       "type": "pattern-outcome",
       "claim-type": "conclusion",
       "author": "claude",
       "session-id": "<session-id>",
       "pattern-id": "<pattern id from PSR>",
       "in-reply-to": "<evidence/id from the PSR entry>",
       "subject": {"subject/type": "pattern", "subject/id": "<pattern id>"},
       "body": {
         "outcome": "<success/partial/failed/pivoted/deferred>",
         "actions": "<summary of actions taken>",
         "expected": "<what was expected>",
         "actual": "<what actually happened>",
         "prediction-error": "<low/medium/high>",
         "notes": "<additional context>"
       },
       "tags": ["pur"]
     }'
   ```

   Run this command via Bash. If the server isn't running, the curl will fail silently — that's fine.

8. **Update Pattern Quality** (future)

   When pattern quality tracking is implemented:
   - Increment use count
   - Update success rate
   - Adjust relevance weights based on query-to-outcome

## PUR Fields

| Field | Description |
|-------|-------------|
| pattern | The pattern ID from PSR |
| actions | What was done guided by the pattern |
| outcome | success / partial / failed / pivoted |
| expected | What you thought would happen |
| actual | What actually happened |
| prediction_error | low / medium / high |
| notes | Additional context, learnings |

## Outcome Categories

- **Success**: Pattern applied correctly, achieved goal
- **Partial**: Pattern helped but didn't fully resolve
- **Failed**: Pattern didn't fit the situation
- **Pivoted**: Switched to different approach mid-work
- **Deferred**: Work paused, will continue later

## Why Use PUR

- **Closes the loop** on pattern selection
- **Tracks what works** in which contexts
- **Identifies gaps** where no pattern fits well
- **Improves pattern quality** through use feedback
- **Feeds learning metrics** for vitality tracking
- **Creates searchable history** of problem-solving approaches

## Orphan Prevention

A PSR without a matching PUR is an orphan. If you:
- Switch tasks without completing work: record PUR with "deferred"
- Pivot to different pattern: record PUR with "pivoted"
- End session: record PUR with whatever outcome reached

Orphaned PSRs indicate interrupted learning loops.

## Example Session

```
> /pur

Active pattern: agent/pause-is-not-failure [不]
Applied during: peripheral testing work

What was the outcome?
1. Success - pattern helped resolve the issue
2. Partial - helped but didn't fully resolve
3. Failed - pattern didn't fit
4. Pivoted - switched to different approach

> Selected: Success

What actions did you take guided by this pattern?
> Paused aggressive debugging, documented the actual error in a note,
> waited for clarity on expected behavior before continuing

Any prediction error?
> Low - expected that pausing would help, and it did

## PUR (Pattern Use Record)
- **Pattern**: agent/pause-is-not-failure
- **Actions taken**: Paused debugging, documented error, waited for clarity
- **Outcome**: success
- **Prediction error**: low
- **Notes**: Pattern reminded me that stopping is sometimes progress

Pattern [不] cleared from backpack. Ready for next /psr.
```

Begin by checking for an active pattern from a previous /psr.
