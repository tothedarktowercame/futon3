# Codex Instructions for Futon3

## Fixed Issues

### fuclaude: Missing --verbose flag for stream-json (FIXED)

**File:** `fuclaude:336`

Claude CLI requires `--verbose` when using `--print --output-format stream-json`. Without it, the error "Input must be provided either through stdin or as a prompt argument when using --print" appears.

**Fix applied:** Added `--verbose` to the claude invocation.

## Outstanding Issues

### fulab/clock-in and fulab/clock-out patterns missing implementation

**Patterns:** `fulab/clock-in`, `fulab/clock-out` in `resources/sigils/patterns-index.tsv`

The pattern index defines clock-in (把) and clock-out (才) events for anchoring proof paths, but neither `hud.clj` nor `pattern_competence.clj` implements these. `build-hud` generates timestamps but no clock-in event; session end has no clock-out emission.

**Fix:** Add `clock-in!` and `clock-out!` functions to `pattern_competence.clj` that emit `:session/clock-in` and `:session/clock-out` events.

### fulab/session-resume pattern not wired to HUD

**Pattern:** `fulab/session-resume` in `resources/sigils/patterns-index.tsv`

The pattern specifies explicit ancestor references for resumed sessions. Currently `build-hud` (`hud.clj:97-141`) has no `:resume-from` option and generates fresh HUD IDs without linking to prior sessions.

**Fix:** Add `:resume-from` parameter to `build-hud` that sets `:hud/ancestor` and carries forward context.

### fulab-operator.el: Delimiter regex (FIXED)

**File:** `contrib/fulab-operator.el:150-154`

Now matches both `[FULAB-REPORT]` and `---FULAB-REPORT---` formats.

### fubar-hud.el: Already fixed

**File:** `contrib/fubar-hud.el:678`

Already uses new format: `\\[FULAB-REPORT\\]...[/FULAB-REPORT]`. No action needed.

### fuclaude: Agent write permissions (FIXED)

**File:** `fuclaude:67-68, 144-147, 342-346, 359-363`

Added `--allow-edits` and `--yolo` flags to enable agent write permissions:
- `--allow-edits` sets permission-mode to `acceptEdits` (file edits without prompting)
- `--yolo` sets permission-mode to `bypassPermissions` (sandboxed environments only)

## Coherence Tasks

- After fixing delimiters, run `scripts/test-elisp.sh` to verify HUD tests still pass
- Ensure all three parsing locations (hud.clj, fubar-hud.el, fulab-operator.el) use consistent regex patterns

## Efficient FuLab Usage

### Token Cost Considerations

1. **Non-live mode (`./fuclaude --hud -p "..."`)** is cheaper than `--live` mode
   - Live mode streams JSON through Clojure processor, adding overhead
   - Use non-live for quick experiments; live mode for session archival

2. **Dead code detection** already identified in `pattern_hints.clj`:
   - `read-lines` (line 57) - unused
   - `read-file` (line 65) - unused
   - These should be removed (don't re-discover via expensive experiments)

3. **Before running expensive HUD experiments:**
   - Ensure HUD server is running: `clojure -M:dev` (port 5050)
   - Test with simple prompt first: `./fuclaude --hud -p "Say hello"`
   - Check CODEX.md for already-identified issues

### What Works

- `./fuclaude --hud --intent "..." -p "..."` correctly injects HUD context
- Resume works: `./fuclaude --resume <session-id> -p "..."`
- Continue works: `./fuclaude --continue -p "..."`
- Session traces exported to `lab/trace/<session-id>.org`
- Agent write permissions: `./fuclaude --allow-edits -p "..."` or `./fuclaude --yolo -p "..."`

## References

- Pattern scoring uses combined sigil/glove weights (0.6/0.4) - see `pattern_hints.clj:249-251`
- GloVe embeddings now in `resources/embeddings/glove_pattern_neighbors.json`
- HUD format spec in `hud.clj:143-188`
