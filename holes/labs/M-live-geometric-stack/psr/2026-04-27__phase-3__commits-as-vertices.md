# PSR: Phase 3 — Commits as First-Class Vertices

context: phases 1+2 produced a queryable substrate of typed
vertices and edges per codebase, with the geometric layer
matching the v0.5 projection numbers. Time has so far been
*outside* the substrate — the diachronic driver
(`futon3/scripts/diachronic_v0.clj`) walks revs externally
and re-runs the projection per commit. Phase 3 promotes
commits to first-class hyperedges, closing the asymmetry
with the math manifold (where `citation-grounding` edges
are already first-class). It also delivers the substrate
M-stack-stereolithography needs (Joe, 2026-04-27: "the DAG
is the spanning object for the stack as it is printed").

patterns:
- `storage/deterministic-ingest-pipeline` — git log is a
  deterministic source; the ingestion is replay-able.
- `futon-theory/counter-ratchet` — commits are immutable;
  per-repo `:commit` count strictly monotonic across runs.
  Re-ingestion adds new commits made since last run; never
  decreases.
- `storage/canonical-interface` — same HTTP API as phase 1.
- `storage/durability-first` — L0 success per commit-vertex
  before its `:edits` cascade.
- `futon-theory/curry-howard-operational` — `:edits` is a
  proof-arrow: the commit *constructs* the changed vertices'
  next state from their previous state. The commit DAG is
  literally a proof-tree of the codebase's evolution.

decision:
- New script `futon3/scripts/ingest_commits_to_futon1a.clj` (bb).
  Walks `git log --reverse --name-only --format=...` for the
  target repo; emits one `:commit` vertex per SHA, one
  `:author` vertex per unique author, `:authored` edges,
  `:precedes` edges between consecutive commits, and `:edits`
  edges from each commit to each var in each file changed by
  that commit.
- Vertex types added (under existing `code/v05/...` prefix to
  signal substrate-2 ownership):
  - `code/v05/commit` — endpoints `[sha]`, props
    `{repo author timestamp subject}`.
  - `code/v05/author` — endpoints `[email]`, props
    `{repo name}`.
- Edge types added (all directed; use the `directed-types`
  set from phase 1's fix):
  - `code/v05/authored` — author → commit
  - `code/v05/precedes` — commit_t → commit_{t+1}
    (per-repo linear chain; `git log --topo-order` for
    branched histories — futon2 is linear so this is trivial)
  - `code/v05/edits` — commit → var (or test/namespace)
- Stable IDs piggyback on phase 1's directional fix
  (synthetic `dir:` third endpoint).
- L2 invariants: every `:edits` target must be a known
  vertex in the same `:repo`. Every `:precedes` parent must
  be the immediate prior commit on the linear chain (or a
  declared parent SHA from `git log --parents` for
  branched cases).
- Tests assert: per-repo commit count matches `git rev-list
  --count HEAD`; `:precedes` chain is a single connected
  path (for linear histories); for any vertex `v`, the set
  of `:edits` predecessors covers every commit that touched
  v's source file.

alternatives:
- Keep diachronic walking as an external driver script
  (rejected: substrate-2's design commitment is "time is
  inside the hypergraph"; external driver is the
  substrate-1-style "data persists if you re-run the script"
  problem we are explicitly fixing).
- Store only commit SHAs as edge labels, not vertices
  (rejected: edges-as-vertices is exactly the cross-prototype
  parity move; without commit-as-vertex, `T(v, c)` queries
  can't be expressed cleanly).
- Use git plumbing (`git fast-import` style) to ingest at
  scale (rejected for v0: `git log` text-parsing is enough
  for the ≤50-commit-each codebases we have; can swap to
  plumbing if futon3c (~thousands of commits) becomes the
  bottleneck).

outcome (target):
- `bb ingest_commits_to_futon1a.clj /home/joe/code/futon2
  --label futon2-d` adds ~37 `:commit` vertices, a small
  number of `:author` vertices, ~36 `:precedes` edges, and
  several hundred `:edits` edges. Idempotent on re-run.
- Tests pass: per-repo commit count exact, precedes chain
  consistent, edits coverage complete.
- Diachronic regression: querying T(v, c) at a sequence of
  commits matches the
  `futon3/scripts/diachronic_v0.clj` time-series we
  already have.
- M-stack-stereolithography can now ask "which vars exist
  at commit c?" as a single XTDB query.

confidence: high. The git log surface is well-trodden; the
ingestion shape is the same as phase 1; the only new
algorithmic concern is `:edits` resolution — mapping
`(commit, file)` to `(commit, vars-defined-in-that-file-at-
that-commit)`. v0 approximates: edits resolve to the var
set as it exists at HEAD (not at the historical commit).
v1 (when needed) re-projects per commit; that's
substantially more work but unblocks T(v, c) for older
revisions. Flagged as DERIVE feeder for phase 3 v1.
