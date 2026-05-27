# PUR: Stable Registry — XTDB-Projected Essay Catalog

pattern (re-confirmed):
- `futon-theory/single-source-of-truth` ✓ registry identity now rebuilds
  from XTDB rather than editor-local config.
- `system-coherence/separate-configurable-choices-from-load-bearing-couplings`
  ✓ the augmentation layer remains configurable, while identity-bearing
  manifest/source provenance moved into persisted entity props.
- `storage/durability-first` ✓ restart durability is now part of the
  browser read path rather than an operator ritual.

actions taken:
- Updated `futon4/dev/arxana-browser-essays.el` so essay import persists
  resolved `source-file`, `manifest-file`, `label`, and `description`
  into XTDB essay entity props.
- Added an XTDB-projected catalog accessor with cache invalidation on
  `arxana-browser-essays-refresh` and import completion.
- Switched manifest loading and source-file lookup to prefer
  `:manifest-file`, falling back to `:manifest-symbol` only for older
  augmentation entries.
- Added five ERTs covering registry prop persistence, manifest-file
  loading, relative source-file resolution, XTDB/augmentation merge
  behavior, and refresh cache invalidation.
- Updated `M-essays-edit-cycle.md` status/mission text and recorded a
  checkpoint.

outcome: success. Targeted essay tests pass (`20/20`) and the new code
path is structurally aligned with the handoff: the browser can now
derive the catalog from persisted XTDB essay state, with the defcustom
reduced to augmentation rather than authority.

prediction errors:

1. **Fallback metadata needed merge semantics, not plain dedupe.**
   First pass treated XTDB-first catalog entries as winners under
   duplicate `:essay-id`, which discarded augmentation descriptions when
   the XTDB entity lacked them. Fix: merge later fallback fields into the
   primary XTDB entry only when the primary value is nil.

2. **Cache needed a sentinel, not nil.**
   A plain nil cache could not distinguish “uncached” from “valid empty
   projection”, which would have retried the store read on every access
   when sync was disabled or no essays were present. Fix: explicit
   `:unset` sentinel.

3. **Open Question 1 really was scope-sensitive.**
   Moving all manifest resolution to direct `read` from file would have
   been a bigger refactor than the registry defect required. Using
   durable `:manifest-file` pointers captured the needed restart safety
   without widening the change past the handoff boundary.

test state:
- `bash futon4/dev/check-parens.sh futon4/dev/arxana-browser-essays.el futon4/test/arxana-browser-essays-test.el` → OK
- `emacs -Q --batch -L futon4/dev -L futon4/test -l futon4/test/arxana-browser-essays-test.el -f ert-run-tests-batch-and-exit`
  → 20 tests, 20 passed, 0 failures

next:
- Run the live migration path by re-importing configured essay manifests
  so pre-existing XTDB essay entities pick up the new persisted props.
- Verify cold-start and save-refresh behavior against the real UKRN /
  Anthropic / Peeragogy essays without relying on manual registration.
