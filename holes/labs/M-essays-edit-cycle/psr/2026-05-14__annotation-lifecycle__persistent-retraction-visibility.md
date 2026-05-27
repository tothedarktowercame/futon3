# PSR: Annotation Lifecycle — Persistent Retraction Visibility

context: the Essays notes pane already had a live-session preview path
that struck through annotations when their overlays evaporated during an
edit. But the persisted section-open path still called
`arxana-browser-essays--annotations-for-section` without
`include-retracted`, so after save/reopen the same annotation vanished
from the notes pane. That broke the audit trail exactly at the point
where the edit became durable.

patterns:
- `system-coherence/separate-configurable-choices-from-load-bearing-couplings`
  — the visual treatment is configurable, but visibility of a retraction
  itself is load-bearing argument history and cannot be dropped from the
  persisted read path.
- `system-coherence/turn-design-into-checks` — if substrate-removal is a
  legitimate dialectical move, the system must make that move inspectable
  after reopen rather than only during the transient edit session.
- `storage/durability-first` — a state transition recorded in the manifest
  should remain visible when reloaded from persisted state.

decision:
- Keep retracted annotations in the notes-pane render on section-open by
  passing `include-retracted t` on the persisted notes path only.
- Preserve the existing text-buffer behavior: the prose render continues
  to operate on live annotations, not synthetic overlays for retracted
  entries.
- Add a shared `arxana-browser-essays-retracted-face` and use it both for
  live preview and persisted notes rendering.
- Add metadata-aware marker rendering that can surface optional future
  `:retraction-kind` and `:retracted-at` props when present, without
  widening this patch into a manifest schema migration.

alternatives:
- Keep filtering retracted annotations on reopen and rely on live preview
  alone (rejected: loses durable audit visibility).
- Recreate text overlays for retracted annotations in the prose buffer
  (rejected for this patch: larger semantic change than needed to restore
  notes-pane history).
- Introduce and require new retraction metadata fields before fixing the
  visibility defect (rejected: schema expansion should not block restoring
  the existing persisted signal).

outcome (target):
- A retracted annotation remains visible in the notes pane after
  save/reopen, with explicit retraction styling and optional metadata.
- Live and persisted retraction views converge on one presentation
  vocabulary instead of diverging by code path.
- The retained entry makes substrate-removal legible as an authored
  argument move rather than a silent disappearance.

confidence: high. The patch is narrow, testable, and restores durable
visibility without changing the manifest writeback contract.
