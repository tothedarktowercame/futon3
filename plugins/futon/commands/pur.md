---
description: Interactive Pattern Use Record - record outcome with UI
argument-hint: [outcome]
---

Record the outcome of applying a pattern with interactive prompts.

## What This Does

The `/futon:pur` command provides an **interactive** outcome recording experience:
1. Shows the active pattern (from last /futon:psr)
2. Uses AskUserQuestion for outcome selection
3. Records prediction error and learnings
4. Logs to the Lab activity stream

This is the interactive version - use `/peripherals:pur` for autonomous/peripheral use.

## Usage

```
/futon:pur            # Interactive outcome recording
/futon:pur success    # Quick record with outcome hint
```

## Process

1. **Check Active Pattern**

   Look for the pattern from the most recent /futon:psr in this session.
   If none found, use AskUserQuestion to ask which pattern was used.

2. **Get Outcome via AskUserQuestion**

   ```
   questions:
     - question: "What was the outcome of applying this pattern?"
       header: "Outcome"
       options:
         - label: "Success"
           description: "Pattern applied, goal achieved"
         - label: "Partial"
           description: "Pattern helped but didn't fully resolve"
         - label: "Failed"
           description: "Pattern didn't fit the situation"
         - label: "Pivoted"
           description: "Switched to a different approach"
   ```

3. **Get Prediction Error**

   ```
   questions:
     - question: "How did reality compare to expectations?"
       header: "Prediction"
       options:
         - label: "Low error"
           description: "Outcome matched expectations closely"
         - label: "Medium error"
           description: "Some surprises along the way"
         - label: "High error"
           description: "Very different from expected"
   ```

4. **Record PUR**

   ```
   ## PUR (Pattern Use Record)
   - **Pattern**: <pattern id>
   - **Sigil**: <sigil>
   - **Outcome**: <selected outcome>
   - **Prediction error**: <selected error level>
   - **Notes**: <any user-provided notes>

   Pattern [sigil] cleared. Ready for next /futon:psr.
   ```

5. **Log to Activity Stream**

   ```bash
   curl -s -X POST http://localhost:6065/musn/activity/log \
     -H "Content-Type: application/json" \
     -d '{
       "event/type": "pattern/pur",
       "agent": "claude",
       "source": "futon-interactive",
       "pattern/id": "<pattern id>",
       "pattern/sigil": "<sigil>",
       "pattern/outcome": "<outcome>",
       "pattern/prediction-error": "<error level>"
     }'
   ```

## Why Interactive?

- **Structured reflection**: UI guides through outcome categories
- **Quick selection**: Click instead of type
- **Consistent data**: Standardized outcome values for analysis
- **Human insight**: User provides context Claude might miss

## Example

```
> /futon:pur

Active pattern: agent/scope-before-action [内]
Applied for: testing strategy

? What was the outcome of applying this pattern?
  ● Success - Pattern applied, goal achieved
  ○ Partial - Pattern helped but didn't fully resolve
  ○ Failed - Pattern didn't fit the situation
  ○ Pivoted - Switched to a different approach

? How did reality compare to expectations?
  ● Low error - Outcome matched expectations closely
  ○ Medium error - Some surprises along the way
  ○ High error - Very different from expected

## PUR (Pattern Use Record)
- **Pattern**: agent/scope-before-action
- **Sigil**: 内
- **Outcome**: success
- **Prediction error**: low
- **Notes**: Defining scope upfront saved debugging time later

Pattern [内] cleared. Ready for next /futon:psr.
```

Begin by checking for an active pattern from the session.
