# PUR: B-2 — Per-Repo Qname Prefix (cross-codebase prop pollution)

pattern (re-confirmed):
- `storage/canonical-interface` ✓ — pure client-side change; no
  futon1a server modification.
- `storage/deterministic-ingest-pipeline` ✓ — same algorithm;
  the prefix is mechanical.
- `futon-theory/counter-ratchet` ✓ — *now structurally guaranteed
  for per-repo types*; per-(repo, qname) stable ID makes cross-label
  collision impossible.

actions taken (2026-04-27):
- Edited `futon3/scripts/ingest_v05_to_futon1a.clj` — added a
  per-call `pf` helper `(fn [q] (str repo-label "/" q))` and
  applied it to per-repo endpoint generation:
  - `code/v05/{var,test,namespace,calls,coverage,contains}`:
    both endpoints prefixed.
  - `code/v05/vocabulary-use`: src (ns) prefixed; dst (term)
    not (term is global).
  - `code/v05/{term,doc,term-defines,author,commit,
    authored,precedes}`: unprefixed (all global).
  - Stored unprefixed `:var/qname` and `:test/qname` in props
    so adapter-detection and ns-extraction can still operate
    on raw names.
- Edited `futon3/scripts/ingest_one_file.clj` — same prefixing
  logic in the per-file watcher dispatch.
- Edited `futon3/scripts/ingest_commits_to_futon1a.clj` — `:edits`
  edge's var endpoint gets the per-repo prefix.
- Smoke-tested on futon7-d3 (fresh label).

outcome: success.

| Verification | Result |
|---|---|
| Phase-1 ingest of futon7-d3 | 658 hyperedges written, 0 failures; var endpoints all `futon7-d3/f7.core/...` |
| Phase-1 idempotency | Re-ingest produced same `:by-type` counts |
| Phase-3 commits | 25 commits + 159 :edits, all with prefixed var endpoints |
| **Counter-ratchet under foreign ingest** | futon7-d3 count stayed at 112 vars after futon4-elisp-d3 ingest landed 2200 vars |
| Phase-5 signatures | 3 work-around-drift records emitted; query layer handles prefixed strings transparently via `ns-of-vertex` |

### What was solved

The original B-2 symptom was **counter-ratchet violation** on
futon7-d after futon4-d ingest: shared qnames between codebases
(generic `clojure.core` symbols, third-party namespaces) had
their `:repo` prop overwritten by whichever ingest ran most
recently. Filter-by-label then *miscounted* — calls that
genuinely existed in futon7-d became "invisible" because their
endpoint vars were now tagged with futon4-d's repo.

The B-2-Symptom-B (projector-revision orphans, surfaced
2026-04-27 during Codex Python projector review) had the same
shape: when Codex's revised projector emitted
`scripts.extract_se_threads/process_site` instead of bare
`extract_se_threads/process_site`, the OLD un-revised qnames
remained as orphans under the same label.

**The per-repo prefix solves both** — every label has its
own ID space; foreign ingests can't touch each other's
vertices; projector revisions emit fresh stable IDs (the new
qname construction is part of the ID, so old and new are
distinct vertices, and the label still shows the new ones
exclusively).

### Migration policy

- Old labels (`-d`, `-d2`, `-elisp-d`, etc.) keep their
  un-prefixed data. They're internally consistent when queried
  alone (which is most operator usage).
- New labels use `-d3` suffix and get B-2 protection.
- Operators can opt in by re-ingesting any label under a fresh
  `-d3` suffix when they want B-2 cross-codebase protection.
- We *don't* migrate the existing 350k+ hyperedges wholesale
  because:
  - substrate-1 has no DELETE; old hyperedges would persist
    as orphans even under a fresh suffix.
  - Mass re-ingest is expensive (futon3c-d alone took ~25s
    phase-1 + 2 min phase-3).
  - Per-codebase queries on existing labels work correctly;
    only cross-codebase counter-ratchet checks were affected.

### Substrate state at end of fix

- 16 active labels including 2 fresh B-2-protected labels
  (`futon7-d3`, `futon4-elisp-d3`).
- Total hyperedges: ~352k (up ~3k from session-2 sign-off).
- The substrate now has *both* schemes side-by-side; query
  scripts handle both (ns-of-vertex works on prefixed and
  un-prefixed qnames identically).

### Updates to cleanup-checkpoint

- B-2 marked **v0 RESOLVED**.
- Migration to all labels deferred (per-repo opt-in via fresh
  `-d3` ingestion).

### What this enables

1. **Cross-codebase signature emission becomes safe.** Phase-5
   was already running on individual labels, but a future
   "scan all labels" mode (with all-labels --all-labels) is
   no longer at risk of double-counting shared qnames.
2. **The pyramidal expansion's regression tests pass** —
   counter-ratchet is now structural rather than label-filter-
   bookkeeping.
3. **Projector evolution is safe.** Future Codex hand-offs
   that change qname construction won't pollute existing
   labels' counts.

The cleanup checkpoint now has B-1, B-3 v0, R-1, P-1 v0, P-2,
**B-2 v0** all RESOLVED. Remaining: A-1 (architecture note;
no urgency), P-3 (4.6 quality-of-life), P-4 (PSR/PUR/PAR-as-
edges bonus), P-5 v0.5 (4 time-series signatures), R-2
(docbook), R-3 (substrate-1 backfill).
