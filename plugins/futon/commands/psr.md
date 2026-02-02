---
description: Interactive Pattern Selection Record - search and select a pattern with UI
argument-hint: <query>
---

Search the pattern catalog and interactively select a pattern to guide your work.

## What This Does

The `/futon:psr` command provides an **interactive** pattern selection experience:
1. Searches the pattern catalog by keyword
2. Presents candidates using Claude Code's native UI (AskUserQuestion)
3. Records your selection with rationale
4. Logs to the Lab activity stream

This is the interactive version - use `/peripherals:psr` for autonomous/peripheral use.

## Usage

```
/futon:psr coordination     # Search for coordination patterns
/futon:psr stuck on testing # Describe your situation
```

## Process

1. **Parse Query**

   Extract the search query from $ARGUMENTS. If empty, use AskUserQuestion to ask what they're working on.

2. **Search Pattern Catalog**

   Call the pattern search API for semantic + sigil-based matching:
   ```bash
   curl -s -X POST http://localhost:6065/musn/patterns/search \
     -H "Content-Type: application/json" \
     -d '{"intent": "<query>", "limit": 5}'
   ```

   The API returns ranked candidates with metadata (title, summary, sigils, maturity).

   **Fallback**: If the API fails, grep the TSV:
   ```bash
   grep -i "<query terms>" resources/sigils/patterns-index.tsv | head -5
   ```

3. **Present with AskUserQuestion**

   Use AskUserQuestion to let the user select interactively:

   ```
   questions:
     - question: "Which pattern best fits your current work?"
       header: "Pattern"
       options:
         - label: "pause-is-not-failure [不]"
           description: "Pausing surfaces uncertainty to those who can resolve it"
         - label: "evidence-over-assertion [示]"
           description: "Evidence transforms output from 'trust me' to 'check this'"
         - label: "coordination-has-cost [功]"
           description: "Cost-aware coordination prevents under-use and over-use"
   ```

4. **Record Selection**

   After user selects, output the PSR:

   ```
   ## PSR (Pattern Selection Record)
   - **Query**: <original query>
   - **Pattern chosen**: <selected pattern>
   - **Sigil**: <sigil character>
   - **Confidence**: <ask user or infer>

   Pattern [sigil] is now active. Use /futon:pur when done.
   ```

5. **Log to Activity Stream**

   ```bash
   curl -s -X POST http://localhost:6065/musn/activity/log \
     -H "Content-Type: application/json" \
     -d '{
       "event/type": "pattern/psr",
       "agent": "claude",
       "source": "futon-interactive",
       "pattern/selected": "<pattern id>",
       "pattern/sigil": "<sigil>",
       "pattern/query": "<query>",
       "pattern/confidence": "<confidence>"
     }'
   ```

## Why Interactive?

- **Visual selection**: AskUserQuestion provides clickable chips
- **Human in the loop**: User makes the final choice
- **Context preservation**: Stays in Claude Code session
- **Quick iteration**: Easy to refine search if no good matches

## Comparison with Peripheral Version

| Feature | /futon:psr (interactive) | /peripherals:psr (autonomous) |
|---------|--------------------------|-------------------------------|
| Selection | User clicks chip | Claude picks best match |
| UI | AskUserQuestion | Text output |
| Use case | Human-guided work | Autonomous agent work |
| Speed | Requires user input | Immediate |

## Example

```
> /futon:psr testing strategy

Searching patterns for "testing strategy"...

? Which pattern best fits your current work?
  ○ evidence-over-assertion [示] - Evidence transforms output from 'trust me' to 'check this'
  ● agent/scope-before-action [内] - Declared scope makes progress measurable
  ○ code-coherence/test-before-commit [本] - Tests verify before committing

> Selected: scope-before-action

## PSR (Pattern Selection Record)
- **Query**: testing strategy
- **Pattern chosen**: agent/scope-before-action
- **Sigil**: 内
- **Rationale**: Need to define test scope before diving into implementation

Pattern [内] is now active. Use /futon:pur when done.
```

Begin by parsing the query and searching the pattern catalog.
