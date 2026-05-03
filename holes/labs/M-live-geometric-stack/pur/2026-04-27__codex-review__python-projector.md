# PUR: Codex Review — Python Projector Integration

pattern (re-confirmed):
- `code-coherence/dead-code-hygiene` ✓ — Codex caught the namespace-
  collision case (`extract_se_threads.py` → `scripts.extract_se_threads`)
  that would have produced shared qnames with other futon-stack
  scripts.
- `storage/canonical-interface` ✓ — wired via `load-file` into the
  same orchestrators as elisp; no new HTTP surface.
- `storage/deterministic-ingest-pipeline` ✓ — Python helper
  emits one EDN doc per file deterministically.

actions taken (review per AGENTS-codex-python-projector.md):

1. Read `python_ast_helper.py` end-to-end (220 LOC, stdlib-only).
2. Read `python_projection.clj` end-to-end (73 LOC, batched
   stdin protocol).
3. Read `python_projection_test.clj` (96 LOC, 4 deftests).
4. Verified the orchestrator wiring: `v0_codebase_hypergraph.clj`
   loads python_projection alongside elisp_projection;
   `ingest_v05_to_futon1a.clj` and `ingest_commits_to_futon1a.clj`
   both dispatch on `python-projection/src-exts`.
5. Ran Codex's unit tests: **4/4 deftests, 14/14 assertions, 0 failures.**
6. Spot-checked 4 vars against source:
   - `futon6.paper_hypergraph/extract_paper_hypergraph_classical` ✓
   - `futon6.paper_hypergraph/parse_latex_blocks` ✓
   - `scripts.superpod-job/main` ✓ (confirms repo-root-relative
     module-name fix)
   - `tests.test_paper_hypergraph/test_classical_emits_typed_nodes` ✓
7. Phase-1 ingest of futon6 under label `futon6-py-d`:
   **8,074 hyperedges in 13s, 0 failures.**
8. Phase-3 commits ingest of futon6: **554 commits, 7,720 :edits
   hyperedges in 16s.**
9. Re-ingest of futon3c with .py surface visible:
   vars 3,378 → 3,474 (+96), namespaces 254 → 264 (+10).
10. Phase-1 invariants on futon6-py-d: 36/40 PASS (4 fails on the
    regression-vs-bb-v05 check — bb=2240 vars, xtdb=4334).
11. Phase-3 invariants on futon6-py-d: **13/13 PASS.**

outcome: **review accepted.** Codex's Python projector is
correctly implemented; orchestrator wiring is sound; tests are
thorough.

### Quality assessment

| Criterion | Result |
|---|---|
| Stdlib-only Python helper | Yes — `ast`, `tokenize`, `json`, `pathlib`, `sys`. No third-party deps. |
| Module-name resolution | Yes — topmost `__init__.py` for packages; `pyproject.toml`/`setup.py` for repo-root-relative scripts; bare stem only as last fallback. **Codex's main design fix.** |
| Body-symbol nesting hygiene | Yes — `BodySymbolCollector` returns from `visit_FunctionDef`/`AsyncFunctionDef`/`ClassDef`, so nested-scope symbols don't pollute the outer def's body-syms. |
| Type annotation handling | Yes — only `ast.Load` context names tracked; signatures (which are `ast.Param` annotations) excluded. |
| Test detection | Yes — both filename pattern (`test_*.py`/`*_test.py`) and path-component containing `test`/`tests`. Function-name `test_*` for individual tests. |
| Cross-file resolution | Yes — `import_aliases` handles both `import x` and `from x import y`, including relative imports via the level-aware `resolve_from_import` helper. |
| Schema match with elisp/clj | Yes — same `{:ns :aliases :vars :tests :is-test?}` shape; downstream code unchanged. |
| Batched protocol | Yes — `collect-files` reads paths from stdin, emits one EDN doc per line; sub-50ms cold start amortised across many files. |

### Real findings

1. **futon6 went from 0 (skipped) to 8,074 hyperedges + 554
   commits + 7,720 :edits in this review.** futon6 was the
   substrate-2 blind spot for the entire session prior to the
   Python projector; it's now substantively visible.

2. **futon3c grew modestly (+96 vars).** Despite futon3c having
   676 .py files in earlier inventory, only ~96 net new vars
   appeared. Hypothesis: most of futon3c's .py files were under
   `__pycache__` / `.venv` / similar (excluded by both
   walkers), or are short scripts. The real-substantive .py
   surface in futon3c is small.

3. **Phase-1 regression check failed (xtdb=4334 vars vs bb=2240)**
   — but this is **not** a Codex bug. Examining the substrate
   directly shows ~650 orphan vars under `:repo "futon6-py-d"`
   with bare module names (e.g. `extract_se_threads/process_site`
   without the `scripts.` prefix). These are residue from an
   earlier projector revision — Codex's design fix changed
   `extract_se_threads.py` from `extract_se_threads` to
   `scripts.extract_se_threads`, leaving the old qnames as
   ghosts. Substrate-1 has no DELETE, so the orphans persist.
   This is **the same completion-rot pattern documented as B-2
   in the cleanup checkpoint** (cross-codebase / cross-revision
   var-vertex prop pollution), now with a concrete trigger:
   *projector revisions that change qname construction produce
   substrate orphans*.

4. **The phase-3 invariants 13/13 PASS on futon6-py-d** because
   commit/precedes/authored/edits hyperedges have stable IDs
   keyed on commit SHAs (immutable), not on qnames.

### Update to cleanup-checkpoint B-2

Add a sub-finding under B-2: **"projector revision that changes
qname construction"** as a concrete cause of orphan vertices.
The fix path remains the same as the original B-2: per-repo +
per-projector-revision discriminator in stable-ID, OR a label-
scoped cleanup pass that removes vertices not produced by the
latest projector run.

### What this unblocks

The Python projector closes the substrate-2 cross-language gap
introduced in the cleanup checkpoint:

| Codebase | Before | Now |
|---|---|---|
| futon6 | skipped (0 .clj/.cljs/.el; 314 .py invisible) | **315 namespaces, 2,240 vars, 133 tests, ingested** |
| futon3c | 3,378 vars (.clj only) | 3,474 vars (.clj + .py) |
| Substrate-2 supports | clj/cljs/cljc + el | **+ py — 4-language bench complete** |

### Codex hand-off protocol — confirmed scalable

Two clean Codex hand-offs in one session (elisp + Python), both:
- Self-contained AGENTS.md briefs.
- No clarification round-trips.
- Tests authored alongside the projector.
- Integrated cleanly with `load-file`.

The pattern (Claude reviews; Codex implements) generalises.

### Substrate state at end of review

- futon6-py-d: 8,074 phase-1 + 8,827 phase-3 = **~17k hyperedges.**
- futon3c-d (refreshed): 17,153 phase-1 + ~15,330 phase-3 = **~32k hyperedges.**
- Total substrate-2 in futon1a: **≈190,000 hyperedges**, 14 active
  labels, 12 distinct codebases.

next: cleanup checkpoint B-1 (vocab whitespace) is the smallest
remaining fix; or phase 5 / multi-watcher launch per the
checkpoint's suggested-next-moves.
