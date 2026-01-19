# fubar.el signal parity checklist

Goal: make the human operator layer emit the same MUSN signals as fucodex.

Gap list (current vs fucodex)
- Plan signal: add a first-class `turn/plan` API in fubar.el and a HUD/UI action to emit it.
- Pattern selection reads: log explicit selection reads (PSR) when browsing patterns via HUD or file opens.
- Pattern use: ensure `turn/use` includes the same evidence hooks as fucodex (note parsing for files).
- Pattern action: mirror `pattern-action` helpers (read/update/implement) with consistent notes + file lists.
- Evidence: auto-attach evidence on save or on explicit action notes (optional toggle).
- MUSN help/HUD: add `musn-help` and `musn-hud` commands that re-render HUD on demand (already started).
- Session registry: keep a registry of sessions (fubar/fucodex/fuclaude) and bind HUD to the active session (in progress).
- Chat bridge: record chat send/unlatch events as MUSN actions or evidence when appropriate.
- Pause/unpause: ensure pause/resume signals appear in HUD and log (align with MUSN view).
- AIF live stats: surface latest AIF stats on demand without stomping HUD navigation.

Suggested next steps
1) Add `fubar-turn-plan` to emit `/musn/turn/plan` with the same payload schema as fucodex.
2) Add read instrumentation to pattern browse links + file opens (already started for HUD buttons).
3) Add a `fubar-pattern-action` wrapper that mirrors scripts/pattern-action semantics.
4) Add evidence parsing helpers for `note` strings and wire into `fubar-turn-use`.
5) Add a lightweight "signal parity" test checklist under `test/` or `scripts/` for manual verification.
