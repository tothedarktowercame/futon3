# PUR: Codex Review — Elisp Projector Integration

pattern (re-confirmed):
- `code-coherence/dead-code-hygiene` ✓ — Codex's projector caught
  one duplicate elisp defn (mirrors the ants.war Clojure finding).
- `storage/canonical-interface` ✓ — wired into existing v0
  projector + ingest via `load-file`; no new HTTP surface.
- `storage/deterministic-ingest-pipeline` ✓ — same algorithm
  shape per file; idempotent on stable IDs.

actions taken (review per AGENTS-codex-elisp-projector.md):

1. Read `futon3/scripts/elisp_projection.clj` end-to-end (144 LOC).
2. Read `futon3/holes/labs/M-live-geometric-stack/tests/elisp_projection_test.clj` (60 LOC, 3 deftests / 10 assertions).
3. Verified Codex's modifications to `v0_codebase_hypergraph.clj`
   and `ingest_v05_to_futon1a.clj` integrate cleanly via `load-file`.
4. Ran Codex's unit tests: **3/3 deftests, 10/10 assertions, 0 failures**.
5. Ran phase-1 invariant tests on `futon4-elisp-d`: **40/40 PASS**.
6. Ran phase-2 geometric layer on `futon4-elisp-d`: deterministic,
   produces operator-readable top-K (see below).
7. Extended `ingest_commits_to_futon1a.clj` (Claude side) so
   phase-3 also delegates to the elisp projector for HEAD var
   index. Re-ran phase-3 invariant tests: **13/13 PASS**.

outcome: success. Code review accepted.

### Quality assessment

| Criterion | Result |
|---|---|
| L4→L0 invariants honored | Yes — reader sanitization at L4-equivalent; existing helpers throw at L2 with futon1a error shape. |
| Cross-file resolution works | Yes — `arxana-browser-core` → `arxana-store` calls resolve correctly (asserted in Codex's test 3). |
| Reader-safe (`1+` / `1-` etc.) | Yes — `sanitize-source` handles via string substitution; honest about being shape-preserving rather than semantic. |
| Test detection | Yes — both path and form-based (presence of any `ert-deftest`). The form-based path is a thoughtful addition beyond what AGENTS.md asked. |
| Macro hygiene | Reasonable — projects only top-level `defun/defvar/...` forms; doesn't try to expand macros. Per the AGENTS.md spec. |
| Namespace via `(provide ...)` | Yes — walks form list, falls back to filename stem. |
| Schema match with Clojure side | Yes — same `:vertex/type :var :var/qname :var/syms` shape; downstream code unchanged. |

### Observations

1. **futon4 went from least-informative to most-informative
   among the three ingested codebases.** The elisp surface is
   where Arxana's actual life happens; the Clojure-only
   projection saw the webarxana frontend stub but missed the
   Emacs Arxana Browser entirely. Now visible.

2. **One genuine duplicate-defn smell on the elisp side**
   (bb 2,141 → distinct 2,140). Same futonic-zapper signature
   the substrate caught on the Clojure side; cross-language.

3. **Phase 3 needed its own elisp wiring.** The commit ingest's
   `vars-in-file` was clj-only; updated to dispatch on extension.
   Phase-3 :edits jumped 917 → 17,104.

4. **Codex review protocol worked smoothly.** Clean separation
   of authoring (Codex) from review (Claude); the AGENTS.md
   handoff was self-contained enough that Codex didn't need
   clarification turns. The collaborative pattern from
   `futon1a/README-best-practice.md` §1 ("Roles") generalises
   to substrate-2 work.

### Substrate state at end of review

futon4-elisp-d alone: 8,579 phase-1 hyperedges + 17,104 phase-3 :edits
+ ~580 phase-3 commits/precedes/authored = ≈26,000 hyperedges.

Cross-codebase total:

| Label | Phase-1 | Phase-3 | Total |
|---|---:|---:|---:|
| futon2-d | 2,403 | ~1,500 | ~3,900 |
| futon4-d | 586 | 1,478 | ~2,100 |
| **futon4-elisp-d** | **8,579** | **17,683** | **~26,300** |
| futon1a-d | 1,954 | 2,395 | ~4,350 |
| (historical pre-fix) | ~5,000 | – | ~5,000 |

Total substrate-2 hyperedges in futon1a: **≈42,000**.

next: Codex's success unblocks pyramidal expansion to elisp-heavy
codebases (futon3c, futon3a). Joe's call on whether to broaden
phases 1-3 across more codebases or pivot to phase 5 (futonic-zapper
signature emission). My recommendation is unchanged: phase 5 first,
then broaden.
