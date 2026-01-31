# Mission: Make Agency Work Properly

Agency currently behaves opposite to the intended "keep the same Codex thread until it won't fit" rule. This mission isolates the broken behavior and defines concrete fixes so we can coordinate with Claude without relying on Agency itself.

## Goal

Align Agency with the intended behavior:
- Keep using the same LLM session/thread id (Codex or Claude) while it fits.
- On overflow, roll to a new LLM session and carry forward summary + state capsule.
- Never confuse Forum thread IDs with LLM resume IDs.
- Keep Forum replies clean (no system-prefixed boilerplate unless explicitly requested).

## First principles

Agency is the **client-side session continuity layer**. It is not the agent itself.

Its job is to:
- Preserve LLM session continuity across peripherals (Codex or Claude).
- Enforce rollover only when necessary, while carrying forward state capsule + summary.
- Keep transport identifiers (forum thread ids, musn ids) separate from LLM session ids.
- Minimize noise in user-facing channels (no Agency boilerplate unless explicitly requested).

## Invariants (must always hold)

- `:agent/current-thread-id` stores **LLM session ids only** (Codex or Claude).
- Forum thread ids are never stored as LLM resume ids.
- Rollover must produce a new LLM session id and return it to the caller.
- Forum replies default to the agent’s natural voice (no “Agency completed/rollover” prefixes).

## Evidence / Findings (as of 2026-01-31)

1) **Forum thread IDs are used as Codex resume IDs.**
   - In `run-peripheral!`, incoming `thread-id` is stored in `:agent/current-thread-id`.
   - `/forum/thread/:id/dispatch` sends the forum thread id as `thread-id`, so the Codex resume id gets overwritten.
   - Result: Codex never truly resumes; it keeps "resuming" with a forum id.
   - Files:
     - `futon3/src/futon3/agency/service.clj`
     - `futon3/src/futon3/forum/http.clj`

2) **Rollover wipes the active Codex session id.**
   - `roll-over!` clears `:agent/current-thread-id` and returns nil to callers.
   - This is the opposite of “roll to a new thread and keep going.”
   - Files:
     - `futon3/src/futon3/agency/service.clj`

3) **Default CLI bin points to Claude, not Codex.**
   - Default config uses `:codex-bin "claude"`, so unless `AGENCY_CODEX_BIN` is set, Agency runs a different agent.
   - File:
     - `futon3/src/futon3/agency/service.clj`

4) **Forum replies are prefixed with “Agency completed…” / “Agency rollover…”.**
   - Agency injects system strings into the user-facing reply body.
   - That makes conversation threads noisy and harder to read.
   - File:
     - `futon3/src/futon3/agency/service.clj`

## Desired behavior (acceptance criteria)

- Forum dispatch should pass **forum-thread-id** separately from **codex-resume-id**.
- Agency should only update `:agent/current-thread-id` with **LLM session ids** (Codex or Claude) returned by the runner.
- On context overflow, Agency should:
  1. Summarize and save state capsule,
  2. Start a **new LLM session** (Codex or Claude), and
  3. Return the new `:agent/current-thread-id` to the caller.
- Forum replies should contain only the assistant response (no “Agency completed” prefix).
- `AGENCY_CODEX_BIN` should default to `codex` (or explicitly require setting).

## Proposed fixes (sketch)

1) **Split identifiers**
   - Add `forum-thread-id` and `resume-id` as distinct keys.
   - In `/forum/thread/:id/dispatch`, pass `forum-thread-id` and omit `thread-id`.
   - In Agency, store `:agent/current-thread-id` only from Codex output.

2) **Rollover flow**
   - `roll-over!` should return the new Codex session id (or a struct including it).
   - `run-peripheral!` should run a fresh Codex session after rollover and return that id.

3) **Remove system prefixes**
   - Post the assistant response verbatim to the forum (plus optional metadata fields).

4) **Default runner**
   - Set default `:codex-bin` to `codex` or fail fast if unset.

## Tasks

1. Update `/forum/thread/:id/dispatch` to send `forum-thread-id` (not `thread-id`).
2. In Agency, treat `thread-id` as **resume id only** (rename to `resume-id` in payload).
3. Update state fields and reply logic to keep Codex thread ids intact.
4. Fix rollover to return + persist the new Codex session id.
5. Remove “Agency completed/rollover” prefixes in forum replies.
6. Switch default `:codex-bin` to `codex` (or require explicit setting).

## Notes

- Keep this mission in sync with `M-agency-forum.md` and `M-par-session-punctuation.md`.
- Avoid relying on Agency to coordinate until this is fixed (use direct forum posts).
