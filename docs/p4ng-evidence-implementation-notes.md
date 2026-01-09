# P4NG Evidence Implementation Notes

This note records implementation realities for `docs/p4ng-evidence-tickets.md` (commit 47b6163).
It keeps the original plan intact while documenting corrections, risks, and practical adjustments.

## Command Corrections (Tickets E04, E08)

- E04 uses `./fulab-pattern-check --session lab/sessions/SESSION_ID.edn --verbose`, but the CLI expects
  `--session-id` or `--session-file` and does not support `--verbose`.
  - Correct forms:
    - `./fulab-pattern-check --session-id SESSION_ID`
    - `./fulab-pattern-check --session-file lab/sessions/SESSION_ID.edn`
- E08 uses `clojure -Sdeps "$LAB_DEPS" -M dev/lab-session-report.clj --session "$session"`.
  The report tool expects `--session-id` (not a file path) and optional `--lab-root`.
  - Correct forms:
    - `clojure -M dev/lab-session-report.clj --session-id SESSION_ID`
    - `clojure -M dev/lab-session-report.clj --session-id SESSION_ID --lab-root lab`

## Evidence Capture Notes

- Live fucodex runs emit PSR/PUR records on every `turn.completed` event when
  using `dev/lab-stream-codex.clj`. That means a run can produce multiple PSR/PUR
  pairs even if the prompt looks like a single task.
- `--clock-in` sets the chosen pattern for that turn, but `--aif-select` will
  still compute an AIF selection when no explicit `--chosen`/`--clock-in` is in force.
- Decision IDs are auto-generated per turn (e.g., `SESSION_ID:turn-N`). When
  extracting vignettes, cite the PSR/PUR IDs and the decision ID for clarity.

## Invariants and Ingest Coupling

- Model invariants in Futon1 require `pattern-language/* -> pattern/*` include
  relations. If those are missing for a language, `ensure-entity` will fail with
  `:patterns/language-has-includes` errors.
- The ingest script now pre-creates those relations before pattern upserts to
  avoid invariant failures. This is necessary for the evidence workflow to be
  repeatable during the paper deadline window.

## Risks / Concerns

- Evidence sessions can be noisy because PSR/PURs are created per turn; without
  a post-filter pass, the vignette selection step (E03/E06) may overfit to one
  noisy PSR/PUR pair rather than the intended task framing.
- `lab/session-report` currently marks tool calls as "not yet captured" in the
  loop evidence checklist, so E08 output will under-report action detail.
- `fulab-pattern-check` uses append-only verification events; if E04 is run
  multiple times on the same session, the trail contains multiple verification
  passes. Use the latest verification when summarising results.

## Recommended Adjustments (Non-binding)

- When running E01/E02, prefer a single turn per task (short prompt, explicit
  stopping condition) to keep PSR/PUR evidence scoped and vignette-friendly.
- After E04, capture a one-line summary of verified/failed checks per session
  and store it alongside the session ID to simplify E10 claim verification.
