# Claude Code Instructions for Futon3

## Exploratory Pattern Mode

When asked to explore or improve the codebase using patterns, operate in
**exploratory mode** rather than waiting for directive instructions.

### Pattern Catalog

The pattern library is at `resources/sigils/patterns-index.tsv`. Each row has:
- `pattern` - namespaced pattern ID (e.g., `ants/white-space-scout`)
- `tokipona` - optional toki pona mapping
- `truth` - sigil character
- `rationale` - what the pattern is for and why it matters
- `hotwords` - terms that signal the pattern may be relevant

Key pattern namespaces:
- `ants/*` - AIF behavioral patterns from the ant simulation
- `code-coherence/*` - code quality and hygiene patterns
- `stack-coherence/*` - cross-repo and devmap integrity patterns
- `devmap-coherence/*` - task tracking and progress patterns
- `contributing/*` - contribution workflow patterns

### The Exploration Loop

When in exploratory mode:

1. **OBSERVE**: Read relevant patterns from the catalog. Examine the codebase
   scope. Note what's unclear or could be improved.

2. **SELECT**: Pick a pattern that fits the current context. Prefer patterns
   where:
   - The rationale aligns with the objective
   - The hotwords match what you're seeing in the code
   - You haven't already applied this pattern recently

3. **APPLY**: Make a small, bounded change guided by the pattern. Don't
   over-reach—one logical change per cycle.

4. **LOG**: After each change, emit a PSR/PUR pair:

   ```
   ## PSR (Pattern Selection Record)
   - **Cycle**: 1
   - **Pattern chosen**: `code-coherence/dead-code-hygiene`
   - **Candidates considered**: dead-code-hygiene, test-before-commit
   - **Rationale**: Found unused helper in pattern_sense.clj
   - **Confidence**: medium (haven't traced all call sites yet)
   ```

   ```
   ## PUR (Pattern Use Record)
   - **Pattern**: `code-coherence/dead-code-hygiene`
   - **Actions taken**: Removed `unused-helper-fn` from pattern_sense.clj
   - **Outcome**: success
   - **Prediction error**: low (no callers as expected)
   - **Notes**: Simpler than expected
   ```

5. **CONTINUE or EXIT**:
   - If more patterns apply and you have ideas, go to step 2
   - If you've found a gap (situation where no pattern fits), propose a new one
   - If you're out of ideas, summarize and stop

### Evidence Format

Keep a running log of PSR/PUR pairs. At session end, provide:

```
## Session Summary
- **Cycles completed**: 3
- **Patterns applied**: dead-code-hygiene, test-before-commit, commit-intent-alignment
- **Changes made**: [list files/changes]
- **Patterns that didn't fit**: [any gaps found]
- **Proposed patterns**: [if any]
```

### Pattern Proposal

When you encounter a situation that no existing pattern covers well:

```
## Pattern Proposal
- **Proposed name**: `aif/evidence-chain-integrity`
- **Rationale**: PSR/PUR pairs can become orphaned when cycles are interrupted.
  No existing pattern addresses evidence continuity.
- **Suggested preconditions**: PSR exists without matching PUR
- **Suggested actions**: Scan for orphaned PSRs, complete or mark abandoned
- **Evidence from this session**: [what you observed]
```

### Scope Boundaries

Unless told otherwise:
- Stay within the directory/files mentioned in the objective
- Prefer edits over new files
- Don't refactor unrelated code
- Run tests if unsure about impact

### Exit Conditions

Stop exploring when:
1. You've tried all applicable patterns and have no new ideas
2. You've hit a cycle limit (if specified)
3. You've found a significant gap worth discussing before continuing
4. The changes are getting speculative rather than grounded

### Example Invocation

User: "Explore the AIF implementation in src/ants/aif and look for improvements"

Response approach:
1. Read patterns-index.tsv, filter to `ants/*` and `code-coherence/*`
2. Read the files in src/ants/aif
3. Pick first pattern (e.g., `ants/hunger-precision-coupling`)
4. Check if implementation matches pattern rationale
5. Make small fix or note alignment
6. Log PSR/PUR
7. Pick next pattern
8. Continue until done or gap found

## Other Guidelines

- See `docs/aif-pattern-engine.md` for the architectural vision
- See `docs/aif-exploratory-mode.md` for the full exploration loop design
- See `AGENTS.md` for Codex-specific instructions (devmap coherence, MUSN)
- The `futon2/` repo contains the reference AIF implementation for ants

## Tickets

- Validate `fuclaude` CLI flags so `--resume` and `--continue` cannot be used together (warn or error).
