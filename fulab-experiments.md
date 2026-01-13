# FuLab Experiments

## E02 prompt set (AIF pattern next-steps)

### E02-A: Belief state deltas (mu-diff + :aif/belief-delta)
- Branch: `e02-belief-delta`
- Prompt (fucodex):
  ```
  Implement the next-step from aif/belief-state-operational-hypotheses: add a mu-diff helper and log :aif/belief-delta events.
  Scope: src/futon3/fulab/pattern_competence.clj (mu-diff helper + event builder), and any minimal call site that emits the delta when mu changes.
  Update evidence in library/aif/belief-state-operational-hypotheses.flexiarg.
  No repo-wide search; ask if you need more scope.
  ```

### E02-B: Precision registry defaults + helper
- Branch: `e02-precision-registry`
- Prompt:
  ```
  Implement the next-step from aif/evidence-precision-registry: add default-precision-registry and a helper to fetch precision by channel.
  Scope: src/futon3/fulab/pattern_competence.clj (default map), src/futon3/aif_bridge.clj (precision-for helper).
  Update evidence in library/aif/evidence-precision-registry.flexiarg.
  No repo-wide search; ask if you need more scope.
  ```

### E02-C: Term provenance validation + dominant channels
- Branch: `e02-term-traceability`
- Prompt:
  ```
  Implement next-steps from aif/term-to-channel-traceability:
  1) extend check-psr in src/futon3/hx/logic.clj to validate :aif/g-terms provenance (term-id, observation-keys, precision-channels, intermediate-values, final-contribution).
  2) add a dominant-channels helper that flags any term where one channel >60%.
  Update evidence in library/aif/term-to-channel-traceability.flexiarg.
  Keep scope to src/futon3/hx/logic.clj plus any small helper you need; no repo-wide search.
  ```

### E02-D: MUSN lab export (format shift)
- Branch: `e02-musn-lab-export`
- Prompt:
  ```
  Implement a MUSN-to-lab exporter so futon4 Arxana can read MUSN sessions.

  1) Add dev/lab-export-musn.clj: reads lab/musn/<session>.edn (or accepts --musn-file) and writes lab/raw/<session>.json using the same schema as dev/lab-export-claude.clj.
     Required keys: lab/session-id, lab/repo-root, lab/timestamp-start, lab/timestamp-end,
     lab/user-messages, lab/assistant-messages, lab/files-touched, lab/trace-path, lab/doc-draft-path.
     Include lab/errors [].
     Use earliest/latest event timestamps for start/end. Derive files-touched from pattern/action files.
     Build minimal messages: user message should include intent/resume notes; assistant message should include PSR/PUR + AIF summary if present.

  2) Also write lab/trace/<session>.org (simple org outline of event stream) and lab/doc-drafts/<session>.json (empty stub is OK).

  3) Support --session-id, --lab-root, --dry-run.

  Keep scope to dev/lab-export-musn.clj (+ small helper if needed). No repo-wide search.
  ```

### E02-E: MUSN lab export wrapper + quick list
- Branch: `e02-musn-lab-wrapper`
- Prompt:
  ```
  Add a simple wrapper script for MUSN lab export and make it discoverable.

  1) Add a new script fulab-export-musn (bash) that wraps dev/lab-export-musn.clj.
     Support:
       --session-id ID
       --musn-file PATH
       --lab-root PATH
       --repo-root PATH
       --dry-run
       --list (print available lab/musn/*.edn sessions, newest first)
     Default lab-root should be ./lab.

  2) Update README-reading-lab-notes.md to mention the wrapper and that Arxana can read
     futon3/lab.

  Keep scope to the new script + README-reading-lab-notes.md. No repo-wide search.
  ```
