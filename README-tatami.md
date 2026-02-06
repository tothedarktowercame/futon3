# Tatami HUD Details

This document covers the Tatami HUD integration, cue embeddings, and intent sigil lifecycle.
For the overview and quickstart, see `README.md`.

## Tatami HUD Quick Reference

- `M-x my-chatgpt-shell-toggle-context` opens the HUD beside the active `chatgpt-shell` buffer. Each refresh calls `my-futon3-fetch-hints`, so the cue percentages update only when Tatami has received a fresh `/musn/hints` response.
- The raw HUD payloads are appended to `resources/tatami-context.edn` every turn (see `my-chatgpt-shell-persist-edn`). Inspecting that log shows the exact `:fruits` / `:paramitas` objects (IDs, scores, summaries) plus the `:cue/intent` metadata (tokens, matched patterns, fallback hits) that produced the on-screen percentages, so you can confirm the porcelain matches the plumbing created by `futon3.cue-embedding`.
- If the numbers look stale, run `M-x my-chatgpt-shell--refresh-context-buffer` (or submit a new message) to force another `/musn/hints` fetch; the HUD is stateless beyond the captured EDN.
- `C-c C-a` updates the "clocked" pattern slug for the current prototype, letting you keep the HUD focused on the clause you are actively editing. `C-c C-e` opens that pattern via Futon4's `arxana-patterns-open` so you can edit it in place and then pop back to the chat buffer.
- Saving the pattern inside the Futon4 buffer (`C-c C-s`) automatically records a `prototype -> pattern` link in Futon1 for the currently clocked session, so future audits know which clause you were editing.
- The "Tatami cues" (fruits) and "Paramitas" lines inside the HUD come straight from the Tatami hints API. Every Tatami response includes structured fruit/paramita entries (`:fruit/id`, `:emoji`, `:summary`, `:score`). `contrib/aob-chatgpt.el` sorts them by score and renders a short description, so the cues you see are exactly the ones Tatami reported for the current session.
- Run `clojure -M:tatami-cues` to materialise the cue embeddings captured so far; the script reads `resources/tatami-context.edn`, computes fruit/paramita vectors via `futon3.cue-embedding`, and writes the annotated timeline to `resources/tatami-cues.edn` for downstream analysis.
- Use `M-x my-chatgpt-shell-replay-last-turn` to reapply the most recent FROM-TATAMI/FROM-CHATGPT EDNs locally (no network calls). This lets you iterate on HUD rendering or salients offline by re-running the last turn's snapshots.
- A `Salients:` line now surfaces whether the HUD is showing fresh hints, a FROM-CHATGPT fallback, or prototype defaults (e.g., `sigils=FROM-CHATGPT`, `cues=fallback (No response from Futon3)`). If `/musn/hints` or `/musn/cues` fail, the fallback reason is rendered inline so you can spot "stale" line-2 embeddings or missing fruits immediately.

## Intent Sigil Refresh Lifecycle

Intent sigils are recomputed twice per chat turn so the HUD stays responsive
even while Tatami is processing:

1. **Outbound (pre-send).** Pressing `RET` triggers
   `my-chatgpt-shell--build-inbound-edn`, which posts the current intent and the
   *previous* turn's reasoning events to `/musn/hints`. This keeps the prototype
   defaults warm, but it also logs `[DEBUG] Embedded: ...` with sigils derived
   from the last turn. That is expected: no new Tatami evidence exists yet.

2. **Inbound (post-reply).** When Tatami returns a
   `FROM-CHATGPT-EDN` payload, `my-chatgpt-shell--apply-chatgpt-edn` saves it to
   `my-chatgpt-shell-last-edn` and calls
   `my-chatgpt-shell--refresh-context-hints`, which re-runs the embedding using
   the *fresh* intent, events, and patterns. The new sigils land under
   `:intent-sigils`, the HUD buffer is re-rendered immediately, and a second
   `[DEBUG] Embedded: ...` line appears--this time capturing the up-to-date pairs.

Because of this two-phase loop it is normal to see the previous turn's sigils in
the debug log right after sending a prompt. They are automatically replaced when
the reply arrives. If the second refresh never happens, check `*Messages*` for
`/musn/hints` failures or verify that the Tatami response contained a
`FROM-CHATGPT-EDN` block for `my-chatgpt-shell--apply-chatgpt-edn` to ingest.

## Cues and Paramitas

Tatami cues / paramita readouts inside the Emacs HUD now come from the same embedding path we use during batch processing. Each FROM-CHATGPT entry (tatami intent + pattern list) is posted to `/musn/cues`, which runs `futon3.cue-embedding/entry-intent-cues` server-side and returns the fruit x orb projection plus the structured `:cue/intent` metadata. When Tatami doesn't emit any patterns, the service falls back to the descriptive corpus built by `scripts/build_fruit_orb_corpus.clj` (see `resources/sigils/fruit-orb-corpus.edn`), so plain English like "Boundless friendliness toward all beings" still lands on the expected paramita. Emacs renders those values in place of the old static prototype defaults, so the HUD mirrors the exact reasoning on every turn. See `docs/cues-and-orbs.md` for the scoring pipeline and `docs/hud-pipeline.md` for the end-to-end HUD flow.

## Future Cleanup

All of these HUD stats are held in dozens of buffer-local vars (`my-chatgpt-shell-last-inbound-edn`, etc.) that we shuttle between the chat buffer and the HUD clone. This keeps biting us when updates run in the wrong buffer. We plan to replace that apparatus with a single per-chat map stored in a central registry (hash keyed by buffer) and swap it atomically, so the HUD logic stops juggling copies. Tracking it here so the refactor doesn't get lost.
