# PUR: Annotation Lifecycle — Persistent Retraction Visibility

pattern (re-confirmed):
- `system-coherence/separate-configurable-choices-from-load-bearing-couplings`
  ✓ retraction visibility now survives on the persisted read path instead
  of being treated as a transient UI nicety.
- `system-coherence/turn-design-into-checks` ✓ the notes pane now keeps
  the evidence that an annotation was raised and later retired, which
  makes the editorial move inspectable.
- `storage/durability-first` ✓ manifest-persisted `:retracted t` state is
  rendered after reopen rather than discarded by the browser.

actions taken:
- Updated `futon4/dev/arxana-browser-essays.el` so section-open renders
  notes from `include-retracted t` annotations while leaving text render
  on the live-only annotation set.
- Added `arxana-browser-essays-retracted-face` and applied it to both the
  existing live retraction preview path and the persisted notes entries.
- Added a helper that renders `[retracted]` markers plus optional
  kind/timestamp metadata when `:retraction-kind` or `:retracted-at` are
  present.
- Added two ERTs covering retained retracted-note rendering and the
  section-open argument split between text and notes renderers.
- Updated `M-essays-edit-cycle.md` with the patch checkpoint and current
  test counts.

outcome: success. Persisted retracted annotations no longer disappear from
the notes pane after save/reopen, and the render path is ready to expose
richer retraction metadata when the manifest schema grows that way.

prediction errors:

1. **The handoff described the wrong defect boundary.**
   The current codebase already had a live-session strikethrough preview,
   so the missing piece was not “implement strikethrough” in general. The
   real defect was the persisted section-open path dropping retracted
   entries on reload.

2. **The fix belonged in the notes-pane call path, not in annotation
   retrieval globally.**
   Switching all readers to `include-retracted t` would have widened the
   semantics and risked polluting the prose render with dead anchors. The
   correct boundary was “notes keep history; text shows live anchors”.

3. **Schema expansion was optional, not blocking.**
   The handoff’s open questions suggested deciding retraction metadata up
   front. In practice, visibility could be restored immediately while
   making the renderer metadata-aware for a later schema extension.

test state:
- `bash futon4/dev/check-parens.sh futon4/dev/arxana-browser-essays.el futon4/test/arxana-browser-essays-test.el` → OK
- `emacs -Q --batch -L futon4/dev -L futon4/test -l futon4/test/arxana-browser-essays-test.el -f ert-run-tests-batch-and-exit`
  → 22 tests, 22 passed, 0 failures

live-data note:
- Persisted `:retracted t` annotations were confirmed in the Anthropic
  Institute Analyst and UKRN manifest files, so the repaired reopen path
  now has real corpus data behind it.
- Manual interactive verification through `arxana://view/essays-home` was
  not run in this turn.

next:
- Open the live essays in Emacs and confirm retained retraction display on
  Hyperreal Side A and the other registered essays.
- Spin the diachronic argumentation-graph work into its own WebArxana-side
  mission after that UI verification passes.
