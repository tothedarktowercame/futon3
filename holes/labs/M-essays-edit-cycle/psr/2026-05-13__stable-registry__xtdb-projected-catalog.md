# PSR: Stable Registry — XTDB-Projected Essay Catalog

context: the Essays browser was storing catalog identity in
`arxana-browser-essays-catalogs`, an Emacs defcustom. That made the
home view and section-open path dependent on editor-local state even
after the essay entities themselves had been persisted to XTDB. On
reload or restart, the entity could survive while the registry entry
disappeared, which broke source-file resolution and forced operator
re-registration.

patterns:
- `futon-theory/single-source-of-truth` — the registry of essay identity
  should live in one durable substrate, not be split between XTDB and
  editor config.
- `system-coherence/separate-configurable-choices-from-load-bearing-couplings`
  — operator-visible catalog augmentation is configurable; the binding
  from essay id to manifest/source provenance is load-bearing and must
  not depend on mutable local config.
- `storage/durability-first` — restart survival is part of the contract,
  so the browser must rebuild from persisted state rather than volatile
  session state.

decision:
- Persist registry reconstruction props on each XTDB essay entity during
  import: resolved `source-file`, `manifest-file`, `label`,
  `description`.
- Replace direct reads of `arxana-browser-essays-catalogs` with an XTDB
  projection function plus a small cache invalidated on refresh/import.
- Keep `arxana-browser-essays-catalogs` as a fallback augmentation layer
  for pre-import manifests and explicit operator overrides; merge it by
  `:essay-id` so fallback metadata fills gaps without outranking XTDB
  identity.
- Resolve Open Question 1 conservatively: adopt `:manifest-file` as the
  durable pointer and load manifests through that path; retain
  `:manifest-symbol` only as a backward-compatible fallback rather than
  doing the larger direct-`read` manifest refactor in this change.

alternatives:
- Keep the defcustom as primary and add advice/workaround reload hooks
  (rejected: workaround against the invariant; leaves split authority).
- Eliminate `:manifest-symbol` entirely and read manifest plists
  straight from file contents in one step (deferred: architecturally
  cleaner, but larger than needed to restore restart durability now).
- Add a new XTDB query endpoint first (rejected for this change: the
  existing latest-entities read surface is sufficient for the immediate
  projection).

outcome (target):
- After import, XTDB essay entities carry enough metadata to rebuild the
  catalog without Emacs-local registration.
- Browser catalog reads come from XTDB first, with augmentation only as
  fallback/override.
- Relative essay source paths resolve against the manifest file location
  rather than the old hardcoded `~/npt/working-paper/` assumption.

confidence: high. The move is narrow, preserves the existing manifest
format, and adds explicit tests around the defect surface.
