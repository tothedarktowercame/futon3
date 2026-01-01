# Lab notebook contract (Codex sessions → XTDB)

This defines a "lab notebook" stream distinct from docbook. The lab notebook
stores per-session summaries and metadata intended to feed literate docs.
It is staged in the repo under `lab/` and later ingested into XTDB.

## Storage layout (repo-local staging)

- `lab/raw/<session-id>.json` — structured session trace (summary-focused).
- `lab/stubs/<session-id>.org` — distilled literate stub (Context/Delta/Verification).
- `lab/index.org` — optional index that links to stubs.

## Schema (lab entries)

Top-level keys (JSON):
- `lab/session-id` (string): unique per Codex session (rollout id).
- `lab/repo-root` (string): absolute path to repo root.
- `lab/timestamp-start` (ISO-8601).
- `lab/timestamp-end` (ISO-8601).
- `lab/user-messages` (array of maps): user turns with ids.
- `lab/assistant-messages` (array of maps): assistant replies with ids.
- `lab/trace-path` (string, optional): repo-relative path to the trace org file.
- `lab/files-touched` (array of strings): files touched in this session.
- `lab/commands` (array of strings, optional): shell commands (opt-in).
- `lab/errors` (array of strings, optional): error summaries (opt-in).
- `lab/notes` (string, optional): manual annotations.
- `lab/doc-draft-path` (string, optional): repo-relative path to draft doc JSON.

Message map shape:
- `id` (string): stable id (user-supplied, e.g., `<session-id>:u001`).
- `timestamp` (ISO-8601).
- `text` (string).

Derived keys (optional, for summary generation):
- `lab/file-notes` (array of maps):
  - `file` (string)
  - `summary` (string)
  - `links` (array of strings, optional)

## Ingest rules (XTDB)

- Use `lab/session-id` as `:xt/id`.
- Store raw values under `:lab/*` keys.
- Preserve ordering for `lab/user-messages` and `lab/assistant-messages`.
- Sessions are append-only; updates should create a new entry that supersedes
  an older `lab/session-id` if rewriting is needed.

## Summary prompt (default policy)

The summarizer should:
- Filter the session based on `lab/files-touched`.
- Produce per-file bullets plus a session-level Context/Delta/Verification.
- Avoid "chat recap" language; focus on code changes and outcomes.
- Link to key messages using `lab/trace-path` anchors when helpful.

## Doc drafts

Summaries can be emitted as doc-style draft JSON for later promotion into
docbook. Drafts are stored under `lab/doc-drafts/<session-id>.json` with keys
in the `doc/*` namespace and `doc/status` set to `draft`.

## Privacy / capture levels

Default capture is summary-focused (user + assistant messages + file touches).
Full transcript capture is opt-in and stored separately if enabled.
