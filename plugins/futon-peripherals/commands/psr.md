---
description: Pattern Selection Record - search, select, and carry a pattern
argument-hint: <query>
---

Search the pattern catalog, select a pattern, and carry it for the current work.

## What This Does

The `/psr` command helps you:
1. Search the pattern catalog by keyword, domain, or problem description
2. Review 3-5 candidate patterns with their rationales
3. Select a pattern to guide your current work
4. Record the selection to the lab activity stream
5. Carry the pattern in your "backpack" for the session

## Usage

```
/psr hunger              # Search for patterns matching "hunger"
/psr code quality        # Search for patterns about code quality
/psr stuck on testing    # Describe your situation
```

## Process

1. **Parse Query**

   Extract the search query from $ARGUMENTS. If empty, ask the user what they're working on.

2. **Search Pattern Catalog**

   Call the pattern search API for semantic + sigil-based matching:
   ```bash
   curl -s -X POST http://localhost:6065/musn/patterns/search \
     -H "Content-Type: application/json" \
     -d '{"intent": "<query>", "limit": 5}'
   ```

   The API returns ranked candidates using:
   - MiniLM semantic embeddings (when portal running)
   - GloVe embeddings (local fallback)
   - Sigil distance matching

   **Fallback**: If the API fails, grep the TSV directly:
   ```bash
   grep -i "<keywords>" resources/sigils/patterns-index.tsv
   ```

3. **Present Candidates**

   Show 3-5 matching patterns in this format:

   ```
   ## Pattern Candidates

   1. **agent/budget-bounds-exploration** [分]
      Budgets convert open-ended search into bounded decisions...
      Hotwords: budget, bounds, exploration

   2. **ants/hunger-precision-coupling** [六]
      Hunger ↔ Precision Coupling Pattern...
      Hotwords: hunger, precision, coupling

   3. **aif/evidence-precision-registry** [由]
      Evidence Precision Registry...
      Hotwords: evidence, precision, trust
   ```

4. **Get Selection**

   Use AskUserQuestion to let the user pick a pattern:
   - Options: the pattern names (1-5)
   - Allow "Other" for custom input or refinement

5. **Record PSR**

   Create a Pattern Selection Record:

   ```
   ## PSR (Pattern Selection Record)
   - **Cycle**: <cycle number if tracking>
   - **Query**: <original query>
   - **Pattern chosen**: <selected pattern>
   - **Candidates considered**: <other patterns shown>
   - **Rationale**: <why this pattern fits>
   - **Confidence**: <low/medium/high>
   ```

6. **Activate Pattern**

   Store the active pattern for the session:
   - Echo the pattern's sigil character (truth column)
   - Note the pattern is now "in backpack"
   - The pattern context is available for /pur when work completes

7. **Log to Lab Stream**

   After recording the PSR, log it to the activity stream using curl:

   ```bash
   curl -s -X POST http://localhost:6065/musn/activity/log \
     -H "Content-Type: application/json" \
     -d '{
       "event/type": "pattern/psr",
       "agent": "claude",
       "source": "slash-command",
       "pattern/selected": "<selected pattern id>",
       "pattern/sigil": "<sigil character>",
       "pattern/candidates": ["<candidate1>", "<candidate2>", ...],
       "pattern/query": "<original query>",
       "pattern/confidence": "<low/medium/high>",
       "pattern/rationale": "<why this pattern fits>"
     }'
   ```

   Run this command via Bash. If the server isn't running, the curl will fail silently - that's fine.

## Pattern Catalog Columns

| Column | Description |
|--------|-------------|
| pattern | Namespaced ID (e.g., `agent/pause-is-not-failure`) |
| tokipona | Toki pona mapping (e.g., `lape`) |
| truth | Sigil character (e.g., `不`) |
| rationale | Why the pattern matters and when to use it |
| hotwords | Terms that signal the pattern may be relevant |

## Key Pattern Namespaces

- `agent/*` - Agent behavioral patterns (autonomy, coordination, trust)
- `aif/*` - Active Inference Framework patterns
- `ants/*` - Ant simulation behavioral patterns
- `code-coherence/*` - Code quality and hygiene
- `stack-coherence/*` - Cross-repo integrity
- `contributing/*` - Contribution workflow
- `control/*` - Measurement and efficiency

## Why Use PSR

- **Grounds decisions** in established patterns rather than ad-hoc reasoning
- **Creates trail** for later review (what guided this work?)
- **Enables learning** by comparing selection to outcome (/pur)
- **Shares vocabulary** across agents and sessions
- **Feeds pattern quality metrics** (which patterns get used, which succeed?)

## Example Session

```
> /psr stuck on testing

## Pattern Candidates

1. **code-coherence/dead-code-hygiene** [本]
   Dhammavicaya requires that "what the system does" and "what the devmap
   claims" match; dead code collapses that link.
   Hotwords: dead, code, hygiene, devmap

2. **agent/pause-is-not-failure** [不]
   Pausing surfaces uncertainty to those who can resolve it. An agent that
   pauses appropriately is more trustworthy than one that always "succeeds."
   Hotwords: pause, failure, uncertainty, surfaces

3. **agent/evidence-over-assertion** [示]
   Evidence transforms agent output from "trust me" to "check this." It
   enables verification, correction, and cumulative trust-building.
   Hotwords: evidence, assertion, verification

> Selected: agent/pause-is-not-failure

## PSR (Pattern Selection Record)
- **Cycle**: 1
- **Query**: stuck on testing
- **Pattern chosen**: agent/pause-is-not-failure
- **Candidates considered**: dead-code-hygiene, evidence-over-assertion
- **Rationale**: Tests are blocking; pattern says pausing to surface
  uncertainty is valid rather than forcing through
- **Confidence**: medium

Pattern [不] is now in your backpack. Use /pur when done to record outcome.
```

Begin by parsing the query and searching the pattern catalog.
