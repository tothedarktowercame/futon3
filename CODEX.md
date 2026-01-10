# Codex Instructions for Futon3

## Fixed Issues

### fuclaude: Missing --verbose flag for stream-json (FIXED)

**File:** `fuclaude:336`

Claude CLI requires `--verbose` when using `--print --output-format stream-json`. Without it, the error "Input must be provided either through stdin or as a prompt argument when using --print" appears.

**Fix applied:** Added `--verbose` to the claude invocation.

## Outstanding Issues

### fulab-operator.el: Inconsistent FULAB-REPORT delimiter regex

**File:** `contrib/fulab-operator.el:152`

The `fulab-operator--parse-fulab-report` function only matches the old delimiter format:

```elisp
(string-match "---FULAB-REPORT---\\([\\s\\S]*?\\)---END-FULAB-REPORT---" text)
```

However, `hud.clj:195-196` now supports both old and new formats:

```clojure
(or (re-find #"(?s)\[FULAB-REPORT\](.*?)\[/FULAB-REPORT\]" text)
    (re-find #"(?s)---FULAB-REPORT---(.*?)---END-FULAB-REPORT---" text))
```

**Fix:** Update `fulab-operator--parse-fulab-report` to also match `[FULAB-REPORT]...[/FULAB-REPORT]` format, similar to the Clojure implementation.

### fubar-hud.el: Already fixed

**File:** `contrib/fubar-hud.el:678`

Already uses new format: `\\[FULAB-REPORT\\]...[/FULAB-REPORT]`. No action needed.

## Coherence Tasks

- After fixing delimiters, run `scripts/test-elisp.sh` to verify HUD tests still pass
- Ensure all three parsing locations (hud.clj, fubar-hud.el, fulab-operator.el) use consistent regex patterns

## References

- Pattern scoring uses combined sigil/glove weights (0.6/0.4) - see `pattern_hints.clj:249-251`
- GloVe embeddings now in `resources/embeddings/glove_pattern_neighbors.json`
- HUD format spec in `hud.clj:143-188`
