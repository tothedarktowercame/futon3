# AGENTS.md — Codex task: Emacs Lisp projector for substrate-2

**For:** Codex (async batch agent)
**Reviewer:** Claude (interactive)
**Owner mission:** `futon3/holes/missions/M-live-geometric-stack.md`
**Excursion:** `futon3/holes/excursions/E-substrate-2-elisp-projection.md`
**Discovered:** 2026-04-27, during phase-1 expansion to futon4 (88% .el)

## What Codex needs to do

Extend `futon3/scripts/v0_codebase_hypergraph.clj` (and the
substrate-2 phase-1 ingest) to project Emacs Lisp source into the
same `(:var, :calls, :coverage, :namespace)` typed-edge schema
the existing Clojure projector produces.

The Clojure projector is the reference: read it end-to-end before
designing the elisp side. Match its shape exactly so downstream
geometric quantities work identically.

## Reference files (read these first)

| File | Why |
|---|---|
| `futon3/scripts/v0_codebase_hypergraph.clj` | The Clojure projector. Your elisp projector should produce the same in-memory shape. |
| `futon3/scripts/ingest_v05_to_futon1a.clj` | The substrate-2 ingest. After your projector lands, plumb `.el` through this same script. |
| `futon3/holes/labs/M-live-geometric-stack/tests/phase_1_invariants_test.clj` | The invariant tests. Your work is done when these pass on an .el-bearing codebase. |
| `futon3/holes/excursions/E-substrate-2-elisp-projection.md` | Full spec, pitfalls, expected payoff. Read this for context. |
| `futon4/dev/arxana-store.el`, `futon4/dev/arxana-browser-core.el` | Real-world elisp the projector must handle. Use as test corpus. |

## Concrete deliverables

1. **Elisp parser module** — new file `futon3/scripts/elisp_projection.clj`
   (bb-runnable). Walks `.el` files; extracts:
   - `(defun foo …)`, `(cl-defun …)`, `(defun- …)` → `:var`
   - `(defvar foo …)`, `(defconst foo …)`, `(defcustom foo …)` → `:var`
   - `(defmacro …)`, `(cl-defmacro …)` → `:var`
   - `(cl-defmethod …)`, `(cl-defgeneric …)` → `:var`
   - `(ert-deftest foo …)` → `:test`
   - `(provide 'feature)` → file's `:namespace` (use feature name; if
     no `provide`, use the filename stem with `_` → `-`)
   - Body symbols → `:calls` and `:coverage` edges via prefix-based
     resolution (elisp uses feature-name prefix convention)

2. **Integration into v0 projector** — add an `el` extension handler
   to `futon3/scripts/v0_codebase_hypergraph.clj`. Should produce
   the same `{:vars [...] :tests [...] :ns-set #{} :ns→aliases {}
   :ns→file {}}` shape so the existing edge-building code works
   without further changes.

3. **Integration into ingest** — likewise extend
   `futon3/scripts/ingest_v05_to_futon1a.clj` so a single
   `bb ingest_v05_to_futon1a.clj <repo>` ingests both clj and el
   under one label.

4. **Unit tests** — `futon3/holes/labs/M-live-geometric-stack/tests/elisp_projection_test.clj`
   with at least:
   - parse a known small .el file from futon4 (e.g.
     `futon4/dev/arxana-store.el`); assert var count matches a
     hand-counted ground truth.
   - parse `arxana-browser-core.el`; assert ert-deftest extraction.
   - assert prefix-based resolution: a call to `arxana-store-get`
     from inside `arxana-browser-core.el` resolves to the var
     defined in `arxana-store.el`.

5. **Regression test** — re-run phase-1 invariant tests on futon4
   under a new label `futon4-elisp-d`. The 132 previously-invisible
   .el files should now produce real var/edge counts. All 40
   invariants should pass.

## Hard requirements

- **NEVER restart futon1a or any JVM server** (per CLAUDE.md
  invariants in futon1a/futon3c/futon4). All ingestion goes
  through HTTP API.
- **The penholder header value must be `api`** (or whatever
  `FUTON1A_PENHOLDER` env var supplies).
- **Match the existing stable-ID convention** — futon1a computes
  it server-side from sorted endpoints. For directed elisp edges
  (calls, coverage), use the same synthetic third-endpoint trick
  the Clojure ingest now uses (`directed-endpoints` helper in
  `ingest_v05_to_futon1a.clj`).
- **L4→L0 invariants from `futon1a/README-best-practice.md`
  apply** — use the same five-layer error shape on any new
  validation paths.
- **No new dependencies on futon3 being importable** (futon3 is
  no longer a running codebase per its CLAUDE.md). The script
  lives in `futon3/scripts/` as bb-runnable analysis tooling,
  same as the Clojure projector.

## Pitfalls to expect (from the excursion stub)

1. **Macro-defined vars.** `(define-derived-mode foo …)` defines
   `foo`, `foo-mode`, `foo-hook`, etc. silently. v0 should
   *not* expand macros — just parse top-level forms. v1 might
   handle a small allow-list of well-known macros.
2. **Reader cookies.** `;;;###autoload` is significant to elisp
   tooling but should be ignored by the projector.
3. **Cross-file resolution is heuristic.** Elisp has no `:as`
   aliases. Resolution: if a body symbol is `<feature>-<name>`
   and `<feature>` is a known provided feature, resolve to that
   feature's namespace. If the symbol is unprefixed and matches
   a same-file var, resolve locally. Otherwise drop (don't
   guess).
4. **No clear test/source split.** Some elisp test files are
   named `*-test.el`; some live under `test/`. The Clojure
   projector's heuristics (path-based + `_test` suffix) work
   for the first; a test for `ert-deftest` form presence
   works for both.

## What success looks like

After Codex's work lands:

```bash
$ bb futon3/scripts/ingest_v05_to_futon1a.clj /home/joe/code/futon4 \
       --label futon4-elisp-d \
       [--vocab paths...]
```

…ingests both the 15 Clojure files AND the 147 elisp files,
producing several thousand hyperedges (vs the current 586).
The phase-1 invariant tests pass at strict equality. The
phase-2 geometric report on futon4 stops showing
"uniform-T everywhere, ΔT=0" and starts showing real
operator-readable load-bearing utilities and orchestrator
"punchlines" from the elisp surface.

## Review protocol (Claude side)

When Codex submits, Claude will:
1. Read each new/modified file end-to-end.
2. Check the L4→L0 invariant scaffolding is honored.
3. Run the unit tests + the phase-1+2 invariant tests on
   futon4 under the new `-elisp-d` label.
4. Spot-check 5-10 vars to confirm parse correctness against
   the source.
5. Compare the futon4 phase-2 report against operator
   intuition (does the new top-K |ΔT| name plausible
   load-bearing functions in the elisp surface?).
6. If issues: file findings as PSR/PUR-style notes per
   `futon4/holes/mission-lifecycle.md` §"PSR/PUR Discipline".

## Out of scope

- Don't touch the existing Clojure projector logic except to
  dispatch by file extension.
- Don't refactor `ingest_v05_to_futon1a.clj`'s structure
  beyond adding the `.el` route.
- Don't change futon1a server code (no JVM restart, no patch
  via Drawbridge).
- Don't try to handle byte-compiled `.elc` files (skip them).
- Don't try to handle `org-babel`-tangled elisp (out of scope
  for v0).
