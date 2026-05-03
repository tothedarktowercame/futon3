# AGENTS.md — Codex task: Python projector for substrate-2

**For:** Codex (async batch agent)
**Reviewer:** Claude (interactive)
**Owner mission:** `futon3/holes/missions/M-live-geometric-stack.md`
**Discovered:** 2026-04-27, during pyramidal-expansion pre-flight on
futon6 (314 .py source files) and futon3c (676 .py).

## What Codex needs to do

Extend `futon3/scripts/v0_codebase_hypergraph.clj` (and the
substrate-2 phase-1 ingest) to project Python source into the
same `(:var, :calls, :coverage, :namespace)` typed-edge schema
the Clojure and Emacs Lisp projectors already produce.

The Clojure projector (`futon3/scripts/v0_codebase_hypergraph.clj`)
is the reference for the in-memory shape; the Emacs Lisp
projector (`futon3/scripts/elisp_projection.clj`) is the
reference for *how to add a new language* — read both end-to-end
before designing.

## Reference files (read first)

| File | Why |
|---|---|
| `futon3/scripts/elisp_projection.clj` | The model to follow: same module shape, same return contract, same integration via `load-file`. |
| `futon3/scripts/v0_codebase_hypergraph.clj` | The orchestrator. Your projector should be dispatched from `collect` by file extension exactly the way `elisp_projection/collect-file` is. |
| `futon3/scripts/ingest_v05_to_futon1a.clj` | The substrate-2 ingest. Inherits whatever the projector emits; no changes needed there once dispatch is wired. |
| `futon3/holes/labs/M-live-geometric-stack/tests/elisp_projection_test.clj` | Test pattern to mirror — load-file the projector, parse a known fixture file, assert var/test counts and cross-file resolution. |
| `futon3/holes/labs/M-live-geometric-stack/AGENTS-codex-elisp-projector.md` | The previous Codex hand-off; same review protocol, same out-of-scope list shape. |
| `futon6/src/futon6/paper_hypergraph.py` | Real-world test corpus #1 (substantive module). |
| `futon6/tests/test_paper_hypergraph.py` | Real-world test corpus #2 (typical pytest file). |
| `futon3c/scripts/cr` (and other futon3c .py) | Real-world test corpus #3 (different repo style). |

## Architectural choice — recommended

Python AST parsing in pure babashka is hard. The clean approach is
**a small Python helper that emits EDN, called from bb via `sh`**:

1. `futon3/scripts/python_ast_helper.py` — stdlib-only Python script
   (uses `ast.parse`); takes a `.py` path on stdin or argv; emits
   one EDN map per file describing `{:module :imports :defs :tests
   :body-syms-per-def}`.
2. `futon3/scripts/python_projection.clj` — bb module that mirrors
   `elisp_projection.clj`'s shape; calls the Python helper via `sh`,
   parses the EDN, returns the same `{:ns :aliases :vars :tests
   :is-test?}` shape the orchestrator expects.

Trade-off: per-file overhead is one Python invocation (~50ms cold
start). For a one-shot full-repo ingest this is fine. For phase-4
watcher per-file ingest, can batch by feeding the helper a list of
paths on stdin and emitting one EDN doc per file.

Alternative: babashka pods (e.g. `sci.python` or similar). Rejected
because it adds a dependency and the stdlib ast helper is simpler.

## Concrete deliverables

1. **`futon3/scripts/python_ast_helper.py`** — stdlib-only.
   Reads paths from argv or stdin (one path per line); emits one
   EDN map per path with shape:

   ```edn
   {:path "..."
    :module "futon6.paper_hypergraph"
    :imports {"parse_latex_blocks" "futon6.paper_hypergraph"
              "json" "json"
              "Path" "pathlib"}
    :defs [{:name "extract_paper_hypergraph_classical"
            :kind "def"          ; or "async-def" or "class"
            :body-syms ["json" "Path" "_parse_llm_edges" ...]
            :has-doc true}]
    :tests [{:name "test_paper_hypergraph_basic"
             :body-syms [...]}]
    :is-test? false}
   ```

   Module path is derived from the file path: walk up looking for
   the nearest `__init__.py` or `pyproject.toml` to find the
   package root, then express as dotted name. Top-level scripts
   without a package use the file stem.

2. **`futon3/scripts/python_projection.clj`** — bb projector
   mirroring `elisp_projection.clj`. Has:
   - `(def src-exts #{"py"})`
   - `collect-file [path]` returning `{:ns :aliases :vars :tests
     :is-test?}` exactly the same shape as the elisp/clojure side.
   - Internally shells out to `python_ast_helper.py`; converts the
     emitted EDN to substrate-2 vertex/edge maps.

3. **Wiring** in `futon3/scripts/v0_codebase_hypergraph.clj`:
   add a third extension dispatch alongside the existing clj/el
   handlers. Single `load-file` line at the top.

4. **Wiring** in `futon3/scripts/ingest_v05_to_futon1a.clj`:
   probably no change needed if the orchestrator dispatches
   correctly. Verify.

5. **Tests:** `futon3/holes/labs/M-live-geometric-stack/tests/python_projection_test.clj`
   — at minimum:
   - parse `futon6/src/futon6/paper_hypergraph.py`; assert known
     `extract_paper_hypergraph_classical` and `parse_latex_blocks`
     are extracted as `:var` with the right qname.
   - parse `futon6/tests/test_paper_hypergraph.py`; assert
     `is-test?` is true; assert `test_*` functions extracted as
     `:test`; assert `from futon6.paper_hypergraph import (...)`
     populates `:aliases`.
   - cross-file resolution check: a call from `test_paper_hypergraph.py`
     to `extract_paper_hypergraph_classical` resolves to the
     `:var` defined in `paper_hypergraph.py`.

6. **Regression on a real codebase:** re-run phase-1+2+3 invariant
   tests on `futon6` under label `futon6-py-d`. Expect ~300 vars,
   ~200 :calls, some :coverage edges (from pytest tests). All
   existing invariants should pass at strict equality.

## Hard requirements

- **NEVER restart futon1a or any JVM server** (per CLAUDE.md
  invariants). All ingestion goes through HTTP API.
- **Penholder header value must be `api`** (or
  `FUTON1A_PENHOLDER` env override).
- **Match the directed-endpoints convention** — for any directed
  edge types you emit (calls/coverage), use the synthetic
  third-endpoint marker the existing ingest already does. You
  shouldn't need to touch this if you stay within the projector
  layer.
- **L4→L0 invariants from `futon1a/README-best-practice.md`
  apply** — same five-layer error shape on any new validation
  paths.
- **No new dependencies** beyond Python stdlib + babashka stdlib.
  No pip install, no clojars.
- **Don't try to expand decorators or runtime metaclasses** —
  v0 reads the AST as written; v1 (later) might handle a small
  allow-list of well-known decorators.
- **Module-name resolution: walk up for package root** (look for
  `__init__.py`, `pyproject.toml`, `setup.py`). For a file
  outside any package, use the bare filename stem. Use `.` as
  the package separator and `/` as the var separator
  (`futon6.paper_hypergraph/extract_paper_hypergraph_classical`).

## Pitfalls to expect

1. **Pytest test discovery.** Convention: `test_*.py` and
   `*_test.py` filenames; `def test_*` and `class Test*` for
   methods. v0 should handle the file-name + `def test_*`
   pattern; class-based tests can be deferred (they're rarer in
   modern Python).
2. **Conditional imports** under `if TYPE_CHECKING:` or
   `try/except ImportError:`. v0 should treat them like any
   other import — extract the symbol, resolve when possible.
3. **Star imports** (`from foo import *`) — can't resolve
   without executing. Skip; emit a `:vocabulary-use`-style
   weak edge if you want the substrate to see the relationship.
4. **Decorators.** `@property`, `@staticmethod`, etc. on
   methods — for v0, ignore decorators; emit the underlying
   `def` as a `:var`. (Class methods are out of scope for v0.)
5. **Type annotations.** `def foo(x: int) -> str:` — body-syms
   should NOT include `int` and `str` (they're not call
   targets). Use `ast.NodeVisitor` to walk only the function
   body, not its signature.
6. **Multi-line strings as docstrings.** First statement of
   `def`/`class` is a string literal → docstring; otherwise no
   docstring. Set `:has-doc` accordingly.
7. **Same-name top-level def overrides** (`def foo(): ...` then
   `def foo(): ...`). Same as the substrate-2 dedupe story:
   bb projector counts each; futon1a's L1 stable-ID collapses
   them. The discrepancy IS the code-coherence finding —
   surface, don't fail.

## What success looks like

After Codex's work lands:

```bash
$ bb futon3/scripts/ingest_v05_to_futon1a.clj /home/joe/code/futon6 \
       --label futon6-py-d \
       [--vocab paths...]
```

…ingests the 314 .py source files (and the few .md), producing
several thousand hyperedges. The phase-1 invariant tests pass
at strict equality on `futon6-py-d`. The phase-2 geometric
report names the documented Python architecture
(`scripts/superpod-job.py` as the orchestrator,
`src/futon6/paper_hypergraph.py` as the load-bearing utility,
etc.). futon3c's 676 Python files (which currently are
invisible) also start showing up in `futon3c-d` re-ingests.

## Review protocol (Claude side)

When Codex submits, Claude will:
1. Read `python_ast_helper.py` and `python_projection.clj` end-to-end.
2. Run Codex's unit tests + the existing phase-1+2+3 invariant
   tests on futon6 under `futon6-py-d`.
3. Spot-check 5-10 vars to confirm parse correctness against
   the source.
4. Run phase-2 geometric layer on futon6; check the top-K against
   operator intuition (does `superpod-job.py` show up as a
   recognised orchestrator? does `paper_hypergraph` look load-
   bearing?).
5. Re-run phase-1 on futon3c with the new dispatch enabled to
   verify the .py surface is now visible.
6. Surface PSR/PUR notes per `futon4/holes/mission-lifecycle.md`
   §"PSR/PUR Discipline" — by the time this lands, that
   discipline is the working norm.

## Out of scope

- Don't try to handle `.pyx` / `.pyi` / Jupyter `.ipynb` files
  (separate effort).
- Don't try to import / execute Python code; pure AST only.
- Don't handle namespace packages without `__init__.py` (PEP
  420) — defer until a real codebase forces it.
- Don't infer call targets through dynamic dispatch (`getattr`,
  metaclasses, etc.) — pure static AST.
- Don't touch the existing Clojure or Elisp projectors except
  to add the `.py` extension to the orchestrator's dispatch.
- Don't change futon1a server code (no JVM restart, no patch
  via Drawbridge).
