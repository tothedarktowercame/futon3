# Excursion: Substrate-2 Emacs Lisp Projection

**Status:** RESOLVED (2026-04-27, same day as discovery) by Codex,
reviewed and integrated by Claude.
**Owner mission:** `holes/missions/M-live-geometric-stack.md`
**Discovered during:** pyramidal expansion phase 1 on futon4.

## Resolution

Codex implemented `futon3/scripts/elisp_projection.clj` (144 LOC):

- Handles `defun / defun- / defvar / defconst / defcustom / defmacro
  / cl-defmacro / cl-defun / cl-defmethod / cl-defgeneric` as `:var`.
- Handles `ert-deftest` as `:test`.
- Reader-safe sanitization of `1+` / `1-` (elisp number-symbols
  Clojure's reader rejects).
- Namespace via `(provide 'feature)`; falls back to filename stem.
- Cross-file resolution via `(require 'feature)` declarations and
  prefix-based name matching.
- Test detection: path patterns + form-based (presence of any
  `ert-deftest`).

Wired into `futon3/scripts/v0_codebase_hypergraph.clj` and
`futon3/scripts/ingest_v05_to_futon1a.clj`. Tests at
`futon3/holes/labs/M-live-geometric-stack/tests/elisp_projection_test.clj`
(3 deftests, 10 assertions, 0 failures).

Phase-3 commit ingest also extended (Claude side) to delegate
to the elisp projector for `.el` files in the HEAD var index,
so `:edits` edges resolve to elisp vertices.

## Verification on futon4

| Quantity | futon4-d (Clojure only) | **futon4-elisp-d** | Δ |
|---|---:|---:|---:|
| Vars | 74 | **2,140** | +2,066 |
| Tests | 0 | **311** | +311 |
| Coverage edges | 0 | **531** | +531 |
| Call edges | 128 | **2,837** | +2,709 |
| Vocabulary uses | 10 | **174** | +164 |
| Namespaces | 10 | **154** | +144 |
| Components | 4 | **420** | +416 |
| Coverage % | 0 | **11.0** | +11.0 |
| nonzero-ΔT vertices | 0 | **>200** | qualitatively informative |
| Phase-3 :edits edges | 917 | **17,104** | +16,187 |
| Total hyperedges | 586 | **8,579** | 14.6× |

Top-K top -ΔT for futon4-elisp-d:
- `arxana-store/arxana-store-sync-enabled-p` (-23)
- `arxana-store/arxana-store--request` (-21)
- `arxana-store/arxana-store-assert-ok` (-13)

Top-K +ΔT:
- `arxana-store/arxana-store--record-error` (+14)
- `arxana-store-qa/arxana-store-qa-run` (+9)
- `arxana-browser-essays-wikibooks/.../import-book` (+7)

**The geometry now reproduces Arxana's documented architecture
without prior knowledge:** `arxana-store` is the data layer
(heavy load-bearing); browsers consume it; QA orchestrates.
Same pattern of "the substrate sees the architecture without
being told" as futon1a's read of its own L4→L0 pipeline.

## Found in flight

1. **One duplicate elisp defn** — bb projector raw count 2,141;
   distinct-qname 2,140. The substrate's L1 dedupe collapsed it,
   surfacing a genuine code-coherence smell on the elisp side
   (mirrors the `ants.war` finding on the Clojure side). Same
   futonic-zapper signature, different language.

2. **Phase-3 :edits required separate fix.** The phase-3 commit
   ingest's `vars-in-file` was clj-only; updated to delegate to
   the elisp projector for `.el` files. Pre-fix count 917;
   post-fix 17,104.

3. **Test invariants unchanged.** No new tests needed for the
   review — Codex's unit tests + the existing phase-1+2+3 invariant
   tests are sufficient. All pass at strict equality on
   `futon4-elisp-d`.

## Goal

Extend `futon3/scripts/v0_codebase_hypergraph.clj` (and the
substrate-2 phase-1 ingest) to project Emacs Lisp source
into the same `(:var, :calls, :coverage)` typed-edge schema
the Clojure projector already produces. Without this,
substrate-2 sees only a thin slice of any predominantly-
Emacs-Lisp codebase.

## Why this matters

Concrete finding from phase-1-on-futon4 (2026-04-27):

| File type | futon4 file count |
|---|---:|
| `.clj` | 9 |
| `.cljs` | 6 |
| `.cljc` | 0 |
| `.el` | **147** |

The substrate's projection saw 15 Clojure files; **132 .el
files were invisible** (the Arxana browser, the data layer,
the user-facing emacs surface). Geometric quantities
computed against this 15-file slice cannot be informative
about futon4's actual structure — they describe the
webarxana web frontend only, which is a small projection
of a much larger codebase.

The same constraint will bite futon3c (also clj+el),
futon3a, and any other codebase whose user-facing surface
lives in elisp.

## Scope

- Parse `.el` files to extract:
  - `(defun foo ...)`, `(defun- foo ...)`, `(cl-defun foo ...)`
    → `:var` vertices with namespace = file stem (or
    declared `(provide 'feature)`).
  - `(defvar foo ...)`, `(defconst foo ...)`, `(defcustom foo ...)`
    → `:var` vertices.
  - `(defmacro foo ...)`, `(cl-defmacro foo ...)` → `:var`.
  - `(cl-defmethod foo ...)`, `(cl-defgeneric foo ...)` → `:var`.
  - `(ert-deftest foo ...)` → `:test` vertex.
  - Body symbols → `:calls` and `:coverage` edges via
    same-file or cross-file resolution (elisp uses
    `(require 'feature)` and feature-name conventions).
- Stable ID scheme: same as Clojure side
  (`hx:code/v05/var:<feature>/<name>` or similar
  qname-style).
- Test coverage detection: `ert-deftest` body-symbol scan
  produces `:coverage` edges, exactly as `deftest` does
  for Clojure.

## Pitfalls to look for

- **Elisp parsing is non-trivial.** `read` works but reader
  conditionals, `;;;###autoload` cookies, and macro-defined
  defns (e.g. `define-derived-mode`,
  `define-minor-mode`) all introduce vars that aren't
  obvious from `(defun ...)` patterns.
- **Cross-file resolution is heuristic.** Elisp doesn't
  have `:as` aliases; symbols are typically `feature-name`
  prefixed by convention but not enforced. The resolver
  will need to walk `(require 'X)` declarations and use
  prefix-matching.
- **No clear namespace.** Elisp has no `(ns ...)` form;
  the closest analogues are `(provide 'feature)` at
  end-of-file and the convention of prefixing all symbols
  with the feature name. The projector should adopt the
  prefix as the namespace, and verify against the
  `(provide ...)` form when present.
- **Macro-defined vars are common.** `define-minor-mode foo`
  silently defines `foo`, `foo-hook`, `foo-mode`, etc.
  v0 should ignore these (don't expand macros); v1 might
  walk a small list of known macros.

## Expected payoff

When this lands, substrate-2 ingests of futon4 will
produce ~10-20× the current count of `:var` vertices and
correspondingly more `:calls` edges. The substrate-2
geometric layer will then be informative on the
elisp-heavy futon repos (futon4, futon3c, futon3a), and
cross-prototype generalisation will hold across all of:
Clojure code, Emacs Lisp code, math typed-claim/proof
hypergraphs.

The futonic-zapper signatures will also start firing on
elisp code: drift hotspots between elisp and clj
namespaces (which have so far been silent because the
elisp side wasn't ingested), coverage retreat in elisp
modules, etc.

## When to schedule

Before phase-1 expansion to the elisp-heavy futon repos
(futon3c, futon3a). Otherwise those expansions will all
produce the same uniform-T / ΔT=0 finding that futon4's
phase-1 produced, and the geometric layer's
informativeness will plateau.

## References

- futon2 + WM finding (`E-cross-prototype-geometry.md`
  §"Findings (post-WM import)") — the WM lobe is uniform-T
  for the same reason: no Clojure-side coverage. WM is
  cljs+playwright, futon4 is clj+el. The pattern is
  language-coverage-gap, not language-specific.
- futon4 substrate-2 phase-1 ingest (label
  `futon4-phase1`) — the empirical demonstration of the
  gap.
