# Fucodex AIF Trial Setup (Reproducible)

This README describes how to reproducibly set up the AIF trial run described in
`docs/fucodex-trial-mission.org`. It does not include run outputs; log those in
`docs/fucodex-trial-mission-analysis.org` after execution.

## Purpose

Set up a repeatable environment so fucodex can execute a small AIF trace
walkthrough and capture proof output with `:check/aif-trace`.

## Prerequisites

- Clojure (REPL or `clojure -M` runner)
- Repo checked out at a known commit

## Setup checklist

1. Record the repo state:
   - `git rev-parse HEAD`
   - `git status --short` (should be clean, or note the diff)
2. Start a REPL in this repo.
3. Load the AIF bridge:
   - `(require '[futon3.aif-bridge :as aif])`
4. Verify the AIF trace schema exists:
   - `futon3.checks/aif-evidence-schema` should be present in `src/futon3/checks.clj`.

## Repro notes

- Use the exact trace snippet from the runbook (`docs/fucodex-trial-mission.org`)
  unless you are intentionally varying the scenario.
- If you adjust the trace, note it in the analysis file so the run remains
  comparable.

## Next step

Follow the runbook in `docs/fucodex-trial-mission.org`, then paste the proof
output into `docs/fucodex-trial-mission-analysis.org`.
