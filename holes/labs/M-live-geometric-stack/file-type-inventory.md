# Substrate-2 File-Type Inventory

**Date:** 2026-04-27
**Purpose:** before adding any new ingest paths, enumerate every
file type that has a *home* in the futon stack so we don't
duplicate. Joe (2026-04-27): "flexiargs already have an import
path, as do futon4 docbook entries, and maybe a few other special
kinds of things. Let's not duplicate that sort of stuff directly
(even if we have to refactor it)."

## Table

| File type | Existing ingest path | Substrate-2 handles? | Disposition |
|---|---|---|---|
| `.flexiarg` (patterns) | `futon4/dev/arxana-{patterns-ingest, flexiarg-normalize, flexiarg-collection, browser-patterns}.el` + substrate-1 `ingest_pattern_provenance` | No | **Refactor** existing → emit `code/v05/pattern` hyperedges. Don't write a new parser. |
| docbook entries | `futon4/dev/arxana-docbook{,-core,-export,-checkout}.el` + `arxana-browser-docbook.el` + `docbook-toc-export.el` | No | **Refactor** existing → emit `code/v05/doc` hyperedges in substrate-2 shape. Or surface via cross-ref edges from substrate-2 to existing docbook IDs. |
| Mission docs `M-*.md` | substrate-1 `ingest_mission_provenance` + futon3c mission inventory scan | **Yes (push-sync bridge)** | **Watch + sync.** Per-file watcher dispatches `M-*.md` to futon3c mission sync and materializes a substrate-2 mission vertex; deeper mission-binding edges remain follow-on work. |
| Evidence records (PSR/PUR/PAR) | substrate-1 `ingest_evidence_bindings`; futon1a evidence API | No | **Keep substrate-1 + bridge.** Substrate-2's `:psr` / `:pur` / `:par` is the bonus-round work in the mission doc — wait until that round. |
| Devmaps `*.devmap` | substrate-1 `ingest_project_column` (project/devmap, project/component) | No | **Keep substrate-1.** Bridge later via cross-ref edges if useful. |
| `.clj`, `.cljs`, `.cljc` | substrate-2 (`v0_codebase_hypergraph.clj`) | **Yes** | Already done. |
| `.el` | substrate-2 (`elisp_projection.clj` via Codex) | **Yes** | Already done. |
| `.py` | Codex hand-off in flight (`AGENTS-codex-python-projector.md`) | Pending | Wait for Codex; review on completion. |
| Vocab `.md` (`futon5/docs/*-terminal-vocabulary.md`) | substrate-2 (vocab-term extraction in v0/ingest) | **Yes** | Already done. |
| Other `.md` (READMEs, technotes, mission docs) | partially (mission docs by substrate-1; others not at all) | No | **Defer.** Treat as low-value substrate target until phase 5 surfaces a need. |
| `.edn` (data, configs, mission-diagrams) | piecemeal (mission-diagram validation in futon5) | No | **Defer.** Treat as low-value substrate target. |
| `.json` (substrate-1 ingestion artifacts) | substrate-1 various | No | **Keep substrate-1.** No reason for substrate-2 to re-ingest. |
| `.flexiarg` exports / `.tex` / `.pdf` / build artifacts | none; intentionally untracked | No | **Skip.** |

## Categorisation

### A. **Substrate-2 owns (already done):**
- `.clj`, `.cljs`, `.cljc` (Clojure family)
- `.el` (Emacs Lisp)
- Vocab `.md` (term extraction)

### B. **Substrate-2 will own (in flight or queued):**
- `.py` (Codex hand-off)

### C. **Existing path; refactor to also emit substrate-2 edges:**
- `.flexiarg` patterns → emit `code/v05/pattern` hyperedges via the
  existing `arxana-patterns-ingest.el` rather than a new bb projector.
- Docbook entries → emit `code/v05/doc` hyperedges via the existing
  `arxana-docbook-*.el` family rather than scraping the docbook
  store ourselves.

### D. **Existing substrate-1 path; bridge later via cross-refs:**
- Evidence records (substrate-1 `ingest_evidence_bindings`).
- Devmaps (substrate-1 `ingest_project_column`).

### D.5 **Push-sync bridge (now handled):**
- Mission docs (`holes/missions/M-*.md`) — watcher-driven push into
  futon3c mission snapshots plus substrate-2 `mission-doc` vertices.

### E. **Defer:**
- Non-vocab `.md` (READMEs, technotes).
- `.edn` data and configs.
- `.json` substrate-1 artifacts.
- Build artifacts (`.tex`, `.pdf`, `.aux`, etc.).

## Implication for the wide build

Joe's "do projector/ingest on everything else first" maps to:

**Wide build now:** per-file ingest + multi-watcher for category A
(extensions `.clj` / `.cljs` / `.cljc` / `.el`) — plus a
"vocab-md" handler (an existing substrate-2 path) and mission-doc
push-sync for `holes/missions/M-*.md`. Once Codex lands `.py`,
dispatch picks it up automatically.

**Wide build later (separate effort):** Category C refactors:
adapt `arxana-patterns-ingest.el` to emit substrate-2 hyperedges
alongside whatever it currently does, and likewise for
`arxana-docbook-core.el`. These are *refactors of working
code*, not new projectors.

**Wide build never (or only via cross-ref):** Category D and E.

## Operational consequence

The watcher daemon's per-file ingest, when implemented, should
have a **dispatch table** that explicitly *recognises* category-C
file types and *defers* to the existing path rather than no-op'ing
or re-parsing. The dispatch table is the single source of truth
for "what does substrate-2 do with this file?". The table being
written down somewhere queryable (this doc) is part of the
substrate's self-description.

## Next concrete action

Implement per-file ingest + multi-watcher for category A (clj/cljs/cljc/el)
+ vocab-md. Do NOT add a `.flexiarg` projector. Do NOT add a docbook
projector. Refactor those into category C as a separate task once
the wide-build infrastructure is stable.
