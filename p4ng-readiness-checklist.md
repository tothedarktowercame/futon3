# P4NG Readiness Checklist

Implementation-agnostic checks for live PSR/PUR + AIF readiness in Futon3.
Use this as a quick QA rubric for MUSN and related live runs.

## Fubar MUSN Launch + View Prompts
Use `M-x fubar-musn-launch-and-view` (or `m` in *FuLab HUD*) with one of the prompts
below. Each prompt is designed to exercise a specific part of the readiness
surface.

### Prompt A: Selection-only baseline (no edits)
```
Pick the most relevant pattern for this run, explicitly name the chosen pattern
id, explain why, and do not apply it or edit any files.
```
Evidence to look for:
- [ ] :pattern/selection-claimed with candidates + chosen.
- [ ] :aif/summary for PSR (selection boundary).
- [ ] Selection-unapplied emitted if no PUR occurs.

### Prompt B: Minimal code edit + use
```
Make a tiny, safe code edit (add a comment or rename a local) in a single .clj
or .el file, then explicitly name the pattern you used and why.
```
Evidence to look for:
- [ ] :code/edit event for the touched file.
- [ ] :pattern/use-claimed with anchors pointing at the edit.
- [ ] :aif/summary for PUR (outcome boundary).

### Prompt C: Pattern file edit (pattern-action surface)
```
Open one pattern file in library/ and make a small, documented tweak, then
explicitly name the pattern id and the pattern action you took.
```
Evidence to look for:
- [ ] :pattern/action recorded for the pattern edit.
- [ ] :pattern/use-claimed or :pattern/selection-claimed tied to the change.

### Prompt D: Missing-plan pause + resume
```
Start by running a safe tool command before stating any plan line. After the
warning/pause, add a one-line plan and continue the task normally.
```
Evidence to look for:
- [ ] Inline [musn-warning] missing-plan appears before MUSN-PAUSE.
- [ ] Pause reason cites missing-plan.
- [ ] Resume clears the pause and the run continues on the same session.

### Prompt E: Multiarg disambiguation
```
Start with a one-line plan in your reply (line starts with "Plan:") before any
tool use. Pick a multiarg pattern file (e.g. library/or/or.flexiarg). Use
pattern-select for the specific @arg pattern id before editing. Then make a
small edit in that pattern block and name the pattern/action used (pattern-use).
```
Evidence to look for:
- [ ] Pattern id used is the specific @arg entry (not the file basename).
- [ ] No pattern-edit-without-selection warning if selection is explicit.
- [ ] File-change auto action uses the selected pattern id.

### Prompt F: Use-mode without writes (no-write halt)
```
Select a pattern for use and claim you applied it, but do not edit any files.
```
Evidence to look for:
- [ ] MUSN pause at turn end cites :no-write or missing evidence for use-mode.
- [ ] Resume clears the halt once a write/evidence event is added.

## Sidecar Missions (futon3a)

Use these prompts to exercise cross-repo work in `~/code/futon3a`, guided by
the sidecar patterns in `library/sidecar/*.flexiarg`.

### Prompt G: Sidecar overview doc (pattern-guided)
```
Work in ~/code/futon3a. Read the sidecar patterns in library/sidecar.
Select exactly two sidecar patterns (pattern-select) that justify an overview.
Create docs/sidecar-overview.md that summarizes the tri-store separation and the
proposal ledger invariants. Then explicitly claim pattern use (pattern-use),
anchored to the new file. Do not edit futon3 files.
```
Evidence to look for:
- [ ] PSR names sidecar patterns (sidecar/*).
- [ ] PUR anchors point to docs/sidecar-overview.md in futon3a.
- [ ] :code/edit event references the new futon3a doc.

### Prompt H: SQLite schema skeleton (pattern-guided)
```
Work in ~/code/futon3a. Read the sidecar patterns in library/sidecar.
Select the patterns that govern artifacts/entities/mentions and arrows/proposals.
Create resources/sidecar/schema.sql with core tables and minimal columns.
Then explicitly claim pattern use (pattern-use) anchored to schema.sql.
Do not edit futon3 files.
```
Evidence to look for:
- [ ] PSR names sidecar patterns used for schema decisions.
- [ ] PUR anchors point to resources/sidecar/schema.sql.
- [ ] :code/edit event references the schema file.

### Prompt I: Chain builder notes (pattern-guided)
```
Work in ~/code/futon3a. Read the sidecar patterns in library/sidecar.
Select the patterns that cover chain scoring and sense-shift gating.
Create docs/sidecar-chains.md describing chain scoring + softness accounting and
the sense-shift gate, with one example chain. Then explicitly claim pattern use
(pattern-use) anchored to the new file. Do not edit futon3 files.
```
Evidence to look for:
- [ ] PSR names sidecar patterns for chain/scoring and sense shifts.
- [ ] PUR anchors point to docs/sidecar-chains.md.
- [ ] :code/edit event references the new futon3a doc.

## Trace Integrity
- [ ] Session trace is append-only (no in-place edits or reordering).
- [ ] Each turn has :turn/started and :turn/completed events.
- [ ] All PSR/PUR events include :session/id and a concrete :at timestamp.

## PSR Creation (Selection)
- [ ] Live run emits :pattern/selection-claimed with:
  - :decision/id
  - :candidates
  - :chosen
  - :context/anchors
  - :forecast
- [ ] Candidate integrity: chosen is in candidates.
- [ ] Candidate count is >= 2 or an explicit solo override is recorded.
- [ ] PSR anchors resolve to concrete session events (turn/action/code-edit).

## PUR Creation (Use)
- [ ] Live run emits :pattern/use-claimed with:
  - :pattern/id
  - :instance/id
  - :anchors
  - :fields
  - :certificates
- [ ] PUR anchors resolve to concrete session events (turn/action/code-edit).
- [ ] If a PSR exists without a PUR, a selection-unapplied event is emitted.

## Live Validation
- [ ] Logic checker emits :pattern/selection-verified events during the run.
- [ ] Logic checker emits :pattern/use-verified events during the run.
- [ ] Invalid PSR/PUR triggers a halt/pause with a human-readable reason.
- [ ] Resume clears the halt and the run continues from the same session.

## AIF Bookkeeping
- [ ] :aif/summary emitted at selection boundary (:aif/kind :psr).
- [ ] :aif/summary emitted at outcome boundary (:aif/kind :pur).
- [ ] AIF explain/dry-run command prints effective weights + worked example.
- [ ] Optional :aif/tap events are either present or explicitly out of scope.
- [ ] `[aif] policy` lines appear when AIF resolves (or abstains from) ambiguous pattern choices.

## Code-Edit Anchoring (Desired)
- [ ] Any .clj/.el edit emits a :code/edit event.
- [ ] PSR/PUR anchors can reference :code/edit events when applicable.
- [ ] Code-edit anchors include file path and turn (or equivalent).

## Live Run Evidence (Per Run)
- [ ] Report includes session id and lab export paths.
- [ ] PSR/PUR counts are visible in the report.
- [ ] AIF summaries are visible in the report.
- [ ] Any pauses include the specific missing obligation.

## Evidence Review Checklists (Per Run)

### MUSN Stream Log (/tmp/musn_stream.log)
- [ ] Session id appears at start and stays consistent.
- [ ] turn.started and turn.completed appear for each turn.
- [ ] pattern-selection and pattern-use lines include aif markers when enabled.
- [ ] pause/halt lines include a human-readable reason.

### MUSN Persistence (lab/musn/<session>.edn)
- [ ] Each request/response pair is appended (no truncation).
- [ ] Snapshot file exists and matches the latest state.

### Lab Session Archive (lab/sessions/<session>.edn)
- [ ] :pattern/selection-claimed and :pattern/use-claimed records are present.
- [ ] :aif/summary events are present for PSR/PUR boundaries.
- [ ] :code/edit or :pattern/action events appear when expected.

### Lab Export Artifacts (lab/raw, lab/trace, lab/doc-drafts, lab/stubs)
- [ ] Raw export contains timestamps and file list.
- [ ] Trace file is generated and includes user/assistant turns.
- [ ] Doc draft stub exists (even if empty content).
