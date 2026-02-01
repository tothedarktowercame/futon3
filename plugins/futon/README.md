# Futon Plugin

Interactive pattern-guided work commands for Claude Code sessions.

## Overview

The `futon` plugin provides **interactive** commands that leverage Claude Code's
native UI features (AskUserQuestion) for pattern-guided work. These are designed
for human-in-the-loop sessions where you want rich visual selection.

## Commands

### /futon:psr - Pattern Selection Record

Search the pattern catalog and interactively select a pattern:

```
/futon:psr coordination
/futon:psr stuck on testing
```

Features:
- Searches `resources/sigils/patterns-index.tsv`
- Presents candidates as clickable chips via AskUserQuestion
- Records selection with confidence level
- Logs to MUSN activity stream

### /futon:pur - Pattern Use Record

Record the outcome of applying a pattern:

```
/futon:pur           # Interactive outcome selection
/futon:pur success   # Quick record with hint
```

Features:
- Shows active pattern from last /futon:psr
- Outcome selection via AskUserQuestion chips
- Prediction error assessment
- Logs to MUSN activity stream

## Comparison: futon vs peripherals

| Aspect | /futon:* | /peripherals:* |
|--------|----------|----------------|
| Mode | Interactive | Hop/detach |
| UI | AskUserQuestion chips | Text or external app |
| Use case | Human-guided sessions | Autonomous or focused work |
| Context | Stays in Claude Code | Leaves and returns |

## Example Session

```
> /futon:psr code review

? Which pattern best fits your current work?
  ○ evidence-over-assertion [示]
  ● code-coherence/test-before-commit [本]
  ○ agent/scope-before-action [内]

## PSR (Pattern Selection Record)
- Pattern: code-coherence/test-before-commit
- Sigil: 本
- Confidence: high

[... do work guided by pattern ...]

> /futon:pur

? What was the outcome?
  ● Success
  ○ Partial
  ○ Failed

## PUR (Pattern Use Record)
- Pattern: code-coherence/test-before-commit
- Outcome: success
- Prediction error: low

Pattern [本] cleared. Ready for next /futon:psr.
```

## Installation

This plugin is part of the futon3 repository. To enable:

1. Add to `~/.claude/settings.json`:
   ```json
   {
     "enabledPlugins": {
       "futon@futon3": true
     }
   }
   ```

2. Restart Claude Code

3. Use `/futon:psr <query>` to start

## Integration

PSR/PUR events are logged to the MUSN activity stream at:
```
POST http://localhost:6065/musn/activity/log
```

This enables:
- Pattern usage analytics
- Session learning metrics
- Cross-agent pattern sharing

## See Also

- `/peripherals:*` - Hop commands for focused work
- `/futon-peripherals:*` - Original combined plugin
- `scripts/fuclaude-peripheral.ts` - Autonomous peripheral wrapper
