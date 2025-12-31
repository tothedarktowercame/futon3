# Tatami HUD Pipeline

This note sketches what happens between a chatgpt-shell turn and the HUD
refresh. It complements `docs/cues-and-orbs.md` by showing the whole route.

## Overview

```
Chat buffer → `/musn/hints` → Tatami entry → `/musn/cues` → HUD render
```

Every turn goes through these stages:

1. **Chat buffer capture**
   - `my-chatgpt-shell-after-command` scans the chatgpt-shell buffer for
     `---FROM-CHATGPT-EDN---` / `---FROM-TATAMI-EDN---` blocks.
   - `my-chatgpt-shell--process-buffer-edn` parses the blocks, updates
     `my-chatgpt-shell-last-edn` (ChatGPT reply) and
     `my-chatgpt-shell-last-inbound-edn` (Tatami hints snapshot).

2. **Hint refresh**
   - `my-chatgpt-shell--refresh-context-hints` sends the latest intent,
     prototypes, and (if available) intent sigils to `/musn/hints`.
   - Tatami responds with pattern evidence, fruits, and pāramitās. The HUD
     stores this in `my-chatgpt-shell-last-inbound-edn` and sets the salients
     (e.g., `sigils=FROM-CHATGPT`, `cues=fallback`).
   - You can replay the same request by curling the dev server:
     ```bash
     curl -s -X POST http://localhost:6060/musn/hints \
          -H 'Content-Type: application/json' \
          -d '{"intent":"rest cycle","prototypes":["f3/p0"],"sigils":[{"emoji":"🌙","hanzi":"休"}]}' | jq .
     ```
   Replace `prototypes`/`sigils`/`intent` with the values from
    `my-chatgpt-shell-last-edn`.

3. **Cue embedding**
   - `my-chatgpt-shell--enrich-with-cues` forwards the entry to
     `/musn/cues` **only after** `my-chatgpt-shell-last-inbound-edn` exists.
   - The `/musn/cues` handler (`src/f2/ui.clj`) runs
     `futon3.cue-embedding/entry-intent-cues`. If the entry contains pattern
     sigils, they get projected into fruits/orbs immediately. If not, the
     fallback corpus described in `docs/cues-and-orbs.md` supplies a text-based
     estimate.
   - On success, Emacs writes the returned `:fruits` / `:paramitas` back into
     `my-chatgpt-shell-last-inbound-edn` and rerenders the HUD.
   - Every hints refresh is appended to `resources/hints-log.edn`, so you can
     diff the exact `/musn/hints` request/response against your manual curl.
   - To replicate the server call, send the merged inbound entry via curl:
     ```bash
     curl -s -X POST http://localhost:6060/musn/cues \
          -H 'Content-Type: application/json' \
          -d @fixture.json | jq .
     ```
     where `fixture.json` contains the entry you can export with
     `M-x my-chatgpt-shell-append-hud-fixture`.

4. **HUD render**
   - `my-chatgpt-shell--render-context` pulls everything together:
     intent/proto selection, pattern verdicts, “Tatami cues”, pāramitā orbs,
     and the latest FROM-CHATGPT events.
   - Line 2 (sigils) is computed via `my-chatgpt-shell--resolve-intent-sigils`
     with dedupe + trim (`my-chatgpt-shell-intent-sigil-limit`).
   - Salients come from `my-chatgpt-shell--salient-tags`, which reflects the
     current sigil status and cue status.

## Common failure modes

| Symptom                      | Likely cause                                      | Where to inspect                      |
|------------------------------|----------------------------------------------------|---------------------------------------|
| `sigils=prototype` persists  | Intent sigils never recomputed; Tatami wasn’t run | `my-chatgpt-shell--sigils-from-intent`, `my-chatgpt-shell--last-intent-sigil-origin` |
| `cues=fallback`              | `/musn/cues` never received inbound entry          | `my-chatgpt-shell--last-inbound-edn`, HUD fixtures, `/tmp/futon3/server.log` |
| “Tatami cues missing” text   | Tatami hints absent or `/musn/cues` error          | `my-chatgpt-shell--last-hints-error`, `my-chatgpt-shell--last-cues-error` |
| Stale “Tatami hint” block    | HUD using cached ChatGPT payload (fallback)        | `my-chatgpt-shell--last-cue-salient` shows `:status :chatgpt`; cue entries live in `my-chatgpt-shell-last-edn` |

## Capturing fixtures / tests

* `M-x my-chatgpt-shell-append-hud-fixture` writes the current inbound +
  ChatGPT payload (including cue/sigil errors) into `resources/hud-fixtures.edn`.
  These fixtures drive the `futon3-hud-fixtures-replay-cues` ERT test so we
  can replay real-world HUD states without hitting Tatami.
* `scripts/cue_embedding.clj` replays `resources/tatami-context.edn` through
  `futon3.cue-embedding` to produce `resources/tatami-cues.edn`. This is
  useful when auditing how fruits/orbs were scored for past turns.

### Future cleanup: central state map

Today every chat buffer and the HUD mirror share state via dozens of
buffer-local variables (`my-chatgpt-shell-last-inbound-edn`, etc.). This has
led to repeated bugs where updates happen in the HUD buffer instead of the
chat buffer. We should eventually replace all of these with a single
per-buffer map stored in a central registry (e.g., a hash keyed by buffer or
an atom), and have each refresh function take the map, return a new map, and
swap it once. Tracking this here so the eventual rewrite has a discrete plan
rather than being tribal knowledge.

### State registry guardrails

Until that refactor exists, Futon3 now enforces a lightweight version of the
registry: `my-chatgpt-shell--refresh-context-hints` and
`my-chatgpt-shell--maybe-render-context` run inside
`my-chatgpt-shell--with-state-buffer`, so the originating chatgpt-shell buffer
always owns the cues/orbs. Each update is copied into
`my-chatgpt-shell--hud-state-registry` via
`my-chatgpt-shell--record-hud-state`, and HUD renders read those snapshots via
`my-chatgpt-shell--state-buffer`. If the registry and live buffer ever diverge
(`my-chatgpt-shell--warn-on-hud-drift`), the debug log surfaces the mismatch
so you know a code path forgot to record the cue merge.

With this pipeline you can trace any HUD element—from the sigil pair on line 2
to the Tatami cues panel—back to the precise EDN payload and API calls that
produced it.

## Tracing missing or canned cues

When you see “Tatami cues” showing the canned defaults (“🍒 doable …”), do the following:

1. **Check cue status**
   ```elisp
   (plist-get my-chatgpt-shell--last-cue-salient :status)
   ```
   - `:inbound` means Tatami returned fresh cues; inspect
     `(plist-get my-chatgpt-shell-last-inbound-edn :fruits)`.
   - `:chatgpt` means we fell back to the cached ChatGPT payload; inspect
     `(plist-get my-chatgpt-shell-last-edn :fruits)`.
   - `:empty` means neither source had cues; see the guidance message for the
     last `/musn/hints`/`/musn/cues` error recorded in
     `my-chatgpt-shell--last-hints-error` or `my-chatgpt-shell--last-cues-error`.

2. **Append a HUD fixture**
   After a bad turn, run `M-x my-chatgpt-shell-append-hud-fixture`. The new
   entry in `resources/hud-fixtures.edn` shows exactly which EDN fields were
   missing and gets replayed by `futon3-hud-fixtures-replay-cues`.

3. **Server log**
   Check `futon3.log` (or `trail_bridge.log`) for `/musn/cues` errors. The UI
   handler logs every `parse-cue-entry` failure and cue embedding exception.

4. **Manual replay**
   Use `scripts/cue_embedding.clj` to re-run the last entry you captured in
   `resources/tatami-context.edn`; compare the resulting cues with what the HUD
   showed to determine whether the missing fruit/orb was due to the server or
   Emacs ignoring the payload.
