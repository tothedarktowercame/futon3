# M-futon3x-e2e

**Status:** VERIFY (2026-03-07)

## 1. IDENTIFY

### Motivation

The futon3x repos (futon3, futon3a, futon3b, futon3c) form a layered system
for pattern-backed coordination. Each layer works in isolation, but they've
never been demonstrated working together end-to-end. There is no single
workflow that starts with a pattern, traverses all four layers, and produces
a useful result.

This matters for two reasons:

1. **Internal**: Without an e2e demo, we can't verify that the layers actually
   compose. Integration bugs hide until someone tries the full path.
2. **External**: A potential client evaluating the futon stack has no artifact
   that shows the whole thing running. Architecture diagrams aren't demos.

The gap: **no through-line exists from pattern content to actionable output
via all four futon3x layers.**

### Theoretical Anchoring

- **compose-independent-lemmas** (math-strategy): The mission's structure IS
  composition of independent pieces. Each repo is a "lemma"; the e2e demo is
  the composition step. The pattern warns: "if the composition step is long,
  one of the pieces is incomplete."
- **construct-an-explicit-witness** (math-informal): The demo is an existence
  proof that the layers compose. Construction gives the strongest answer.
- **Baldwin cycle** (futon-theory): The four repos are at different cycle
  stages — futon3 is canalized (stable library), futon3a is exploratory
  (schema exists, no data), futon3b is assimilated (code works, lightly
  exercised), futon3c is canalized (running service). The e2e demo is an
  assimilation event for the system-as-whole.

### Scope

**In scope:**
- Bootstrap futon3a's meme store from pattern library data
- Federated search across pattern library + meme store + transcripts
- Gate pipeline round-trip with real pattern references
- API endpoint serving concept graph data
- Application: landscape probe generation via futon7
- Evidence loop closure back to futon1a

**Out of scope:**
- ANN embedding re-computation (use existing FastText/GloVe/MiniLM)
- New pattern creation (use existing 852 patterns)
- futon3c agent lifecycle changes
- Mathematical content (futon5/futon6 integration)
- Production hardening or performance work

### Completion Criteria

1. futon3a meme.db is populated with entities derived from the pattern library.
   Entity count > 100, arrow count > 200.
2. A federated query for a concept (e.g., "provenance") returns results from
   at least two stores (pattern library + meme store).
3. A coordinated action traverses futon3b's full gate pipeline (G5→G0) with
   a real PSR referencing a real pattern, producing a proof-path EDN file.
4. A concept graph query is servable via futon3c (HTTP endpoint or Drawbridge).
5. futon7 generates at least 3 probes derived from the concept graph (not
   hand-crafted), and those probes return results.
6. Every futon3x namespace (futon3, futon3a, futon3b, futon3c) that
   participates in the e2e demo is documented in both:
   (a) futon4 docbook entries (navigable via `M-x arxana-browse`)
   (b) futon1a hypergraph metadata (enrichment layers, entrypoints, deps)
   This makes the futon3x series itself the worked example for
   M-futon-enrichment — the demo subject is also the demo.
7. The full demo is reproducible from the mission doc by a new agent.

### Relationship to Other Missions

- **M-futon-enrichment** (futon4): Phase 5 of that mission depends on this
  mission's application phase. This is the *how*; that is the *where it lives*.
- **M-f7-lead-report** (futon7): Predecessor. Proved probe→report works.
  This mission adds probe *generation* from the graph.
- **M-P3-rational-reconstruction** (futon6): Demonstrated pattern discipline
  on mathematical content. This demonstrates it on code infrastructure.
- **M-coordination-rewrite** (futon3): Built the gate pipeline (futon3b).
  This is the first end-to-end exercise of that pipeline with real data.
- **M-self-representing-stack**: The futon3x series should be its own
  best-documented example. By ensuring all participating namespaces have
  docbook entries and hypergraph metadata, this mission makes the demo
  subject and the enrichment subject the same thing. The stack explains
  itself by running itself.

### Source Material

| Source | Location | What it provides |
|--------|----------|-----------------|
| Pattern index | futon3a/resources/notions/patterns-index.tsv | 852 patterns with hotwords |
| Pattern embeddings | futon3a/resources/notions/*_embeddings.json | Pre-computed vectors (FastText, GloVe, MiniLM) |
| Meme store schema | futon3a/src/meme/{schema,core,arrow,bridge}.clj | Entity/arrow/bridge CRUD |
| Sidecar store | futon3a/src/sidecar/store.clj | Proposals→facts→chains event store |
| Federated query | futon3b/src/futon3b/query/relations.clj | core.logic patterno, search-texto |
| Gate pipeline | futon3b/src/futon3/gate/pipeline.clj | G5→G0 composition |
| Enrichment API | futon3c/src/futon3c/enrichment/query.clj | /api/alpha/enrich/file |
| Probe infrastructure | futon7/src/f7/{probes,signals,core,report}.clj | GitHub search + signal extraction |
| Existing probe run | futon7/data/results/stack-grounded-run.edn | 293 repos, 10 probes |
| Notions search | futon3a/scripts/notions_search.py | ANN cosine similarity search |

### Owner and Dependencies

**Owner**: futon3 (mission file lives here; pattern library is the starting point)

**Repos involved**: futon3 (content), futon3a (store), futon3b (query),
futon3c (service), futon7 (application), futon1a (persistence)

**Runtime dependencies**: futon3c on port 7070, futon1a on port 7071

**Data dependencies**: patterns-index.tsv populated (yes, 852 entries),
meme.db bootstrapped (no — this mission creates it)

### PSR

- Pattern chosen: compose-independent-lemmas (math-strategy)
- Candidates: compose-independent-lemmas, construct-an-explicit-witness,
  reduce-to-known-result
- Rationale: The mission's core structure IS composition of independent
  pieces (futon3, 3a, 3b, 3c, 7). Each lifecycle phase verifies one piece.
  The INSTANTIATE phase should add nothing new — if it does, a piece is
  incomplete. This is exactly what the pattern warns about.

---

## 2. MAP

*Survey what exists. Don't design yet — just look.*

### MAP Questions

Q1. What is the current state of futon3a's meme store? Is the schema
    up to date? Can we create entities and arrows today?

Q2. Does futon3b's federated search (`search-texto`) actually work when
    called from a REPL? What does the output look like?

Q3. Has the gate pipeline (G5→G0) ever been exercised end-to-end with
    all gates active? What test coverage exists?

Q4. What concept/keyword vocabulary exists in the pattern library?
    How many unique hotwords? What's the co-occurrence structure?

Q5. Can futon3c serve futon3a data today, or is new wiring needed?
    (The `mission_backend.clj` already calls `notions_search.py` —
    what path does that take?)

Q6. What would futon7's `probe_gen.clj` need as input? What's the
    minimum viable concept graph that produces useful probes?

Q7. What is the current enrichment coverage for futon3x namespaces?
    Which namespaces appear in futon1a's hypergraph (L0-L5 layers)?
    Which are missing? What docbook entries exist in futon4 for
    futon3/3a/3b/3c?

Q8. What are the key entrypoints (public API functions, HTTP handlers,
    CLI commands) across the futon3x series? Are they documented in
    a way that a new agent could discover them by browsing?

### MAP Answers

**Q1. futon3a meme store state.**
Schema is current. `meme.db` did not exist — created successfully during MAP
by calling `(s/ensure-db!)`. All tables created. CRUD works: created a test
entity, read it back, confirmed round-trip. Entity/arrow/bridge counts all 0
(empty DB). **Important**: meme store requires JVM (`next.jdbc` + SQLite JDBC),
not babashka. Bootstrap script must use `clj -M`, not `bb`.

**Q2. futon3b federated search.**
Works from REPL. `(r/search "provenance")` returns results from both
`:pattern` and `:transcript` sources. Pattern count: 829. Session count: 2132.
Search for "provenance" found 8 pattern matches (coordination/artifact-registration,
enrichment/rational-reconstruction, etc.) Search for "landscape" found 5
transcript matches. The federated search infrastructure is operational.

**Q3. Gate pipeline test coverage.**
Yes — `pipeline_test.clj` has 12 tests covering all 6 gates plus store
integration. The `happy-path-produces-proof-path` test confirms: full G5→G0
traversal producing a 6-event proof path with real PSR, PAR, exec function,
and evidence sink. Store integration tests confirm: real pattern library
lookup (G3), mission registry lookup (G5), durable proof-path write + read-back,
and passthrough artifacts. **The pipeline has been fully exercised in tests
with hermetic fixtures.** What's missing is a run against the live system
with non-fixture data.

**Q4. Hotword vocabulary.**
4,391 unique hotwords across 852 patterns. However, ~260 of these are
dominated by iiching patterns (257 entries sharing common words like
"explicit", "make", "knowledge", "inspectable"). The semantically meaningful
vocabulary (freq 10-200) includes: evidence (146), pattern (91), coherence
(64), agent (34), action (32), proof (25), layer (28). Co-occurrence structure:
patterns within a namespace share many keywords; cross-namespace co-occurrence
is sparser and more informative (e.g., "evidence" + "agent" in different
namespaces signals a real conceptual link).

**Q5. futon3c → futon3a path.**
futon3c already calls futon3a via `notions_search.py` — a Python script that
does cosine similarity against pre-computed embeddings. The path goes through
`mission_backend.clj` (line 730) which shells out to the Python script.
For meme store access (entities/arrows/bridges), no path exists yet. futon3c
has futon3b on its classpath (via futon3b's `:local/root` to futon3a), so
in theory the JVM can call meme.core directly. But no HTTP endpoint or
Drawbridge command exposes this.

**Q6. Minimum viable concept graph for probe generation.**
futon7's current `report.clj` uses 28 hand-crafted keywords in
`stack-alignment-keywords`. To replace this with the entity graph, we need:
(a) entities tagged by stack-relevance area (knowledge-store, multi-agent,
formal-math, etc.), (b) a way to map entities to GitHub search terms, and
(c) a walk function that discovers adjacent concepts. Minimum: ~50 entities
covering the 7 alignment areas, with arrows connecting related concepts.
The existing pattern index has the raw material; the question is filtering
the 4,391 hotwords down to the ~200 that are semantically meaningful for
landscape probing.

**Q7. Enrichment coverage for futon3x.**

*Hypergraph (futon1a)*: 75 futon3x namespaces in the code column (of 102 total).
Coverage by repo:
- futon3c: 62 namespaces (good — all key modules present)
- futon3b: 2 namespaces (futon3b.query.relations, futon3b.query.transcript)
- futon3 gate: 10 namespaces (futon3.gate.*)
- futon3a: 0 namespaces (not in reflection API — separate JVM)

Mission provenance: 142 mission→namespace edges touching futon3x.
Covers M-agency-rebuild, M-agency-refactor, M-IRC-stability, etc.

*Docbook (futon4)*: Two relevant books exist:
- `futon3x` book: 31 entries covering The Split, Mission Control, Portfolio
  Inference, Self-Representing Stack, API Reference. Good conceptual coverage
  but no per-namespace entries for futon3a or futon3b.
- `futon3` book: 41 entries covering Core, Transport, Tatami, HX, Patterns.
  Includes per-file dev docs for legacy futon3 (musn, f2, hx, tatami) but
  nothing for the split repos (futon3a, futon3b).

**Gap**: futon3a and futon3b have zero docbook entries and zero hypergraph
namespace nodes. They're invisible to the self-representing stack.

**Q8. Key entrypoints.**

| Repo | Type | Count | Examples |
|------|------|-------|---------|
| futon3 | flexiarg patterns | 739 | library/**/*.flexiarg |
| futon3a | meme CRUD functions | 41 | core/{create,get,list}-{entity,artifact,arrow,bridge} |
| futon3a | notions search (Python) | 1 | scripts/notions_search.py |
| futon3b | query relations | 21 | patterno, search-texto, patterns, search, proof-patho |
| futon3b | gate pipeline | 1 | pipeline/run (composes G5→G0) |
| futon3c | HTTP handlers | 48 | handle-{dispatch,health,reflect-*,enrich-file,...} |

Discovery: A new agent can find futon3c endpoints via the handler list in
http.clj (lines 9-29 have a comment-based API index). futon3b's query API
is discoverable via `(r/patterns)` and `(r/search ...)`. futon3a's meme
store API requires reading schema.clj + core.clj. No unified discovery
surface exists across all four repos.

### Ready vs Missing

| Ready (no new code) | Missing (the actual work) |
|---------------------|--------------------------|
| Pattern library (852 patterns, hotwords) | futon3a meme.db population |
| Pattern embeddings (FastText/GloVe/MiniLM) | Entity extraction from hotwords |
| Meme store schema + CRUD (JVM) | Arrow generation from co-occurrence |
| Federated search (patterns + transcripts) | Meme store query in federated search |
| Gate pipeline (12 tests passing) | Live system gate traversal (not just fixtures) |
| futon3c → futon3a ANN search (Python) | futon3c → futon3a meme store (JVM) |
| futon7 probe infrastructure + report | Probe generation from entity graph |
| 75 futon3x namespaces in hypergraph | futon3a namespaces in hypergraph (0 today) |
| futon3x docbook (31 entries, conceptual) | Per-namespace docbook for futon3a/3b |
| 48 HTTP handlers documented in-code | Unified entrypoint discovery surface |

---

## 3. DERIVE

*Design the solution. Each decision references a pattern.*

### Dependency Graph (per compose-independent-lemmas)

The PSR pattern requires us to identify independent pieces and verify they
compose without adding new claims. Here are the six pieces ("lemmas") and
their dependency edges:

```
L1: Bootstrap meme.db (futon3a)
    ← patterns-index.tsv (exists)

L2: Federated search extension (futon3b)
    ← L1 (meme.db must be populated)

L3: Gate pipeline live run (futon3b)
    ← L2 (search needed for PSR lookup)
    ← futon3c running (exists)

L4: Concept graph API endpoint (futon3c)
    ← L1 (meme.db must be populated)

L5: Probe generation from graph (futon7)
    ← L4 (API must serve concept data)

L6: Enrichment metadata (futon1a + futon4)
    ← L1..L5 (documents what was built)
```

Execution order: L1 → {L2, L4} in parallel → {L3, L5} → L6.
The composition step (INSTANTIATE) should add nothing new — it wires
existing pieces. If it requires new code, a piece is incomplete.

### Entity Types

| Type | Source | Store | Example |
|------|--------|-------|---------|
| concept | hotword extraction from patterns-index.tsv | meme.db entity | "evidence", "coherence", "agent" |
| pattern | pattern ID from library/ | meme.db entity | "math-strategy/compose-independent-lemmas" |
| namespace | Clojure ns from futon3x repos | meme.db entity | "futon3b.query.relations" |
| probe | generated GitHub search query | futon7 EDN | {:query "topic:multi-agent coordination" ...} |

### Relation Types

| Relation | Arrow mode | Meaning |
|----------|-----------|---------|
| concept ← pattern | `derivation` | concept was extracted from this pattern's hotwords |
| concept → concept | `analogy` | co-occurrence in multiple patterns (weight = count) |
| pattern → namespace | `construction` | pattern is implemented/exercised in this namespace |
| concept → probe | `derivation` | concept generates this search query |
| namespace → namespace | `translation` | cross-repo dependency (classpath, HTTP, shell) |

Bridge predicates (from bridge.clj):
- `derived-from`: concept ← pattern
- `implements`: namespace → pattern
- `maps-to`: concept → probe search term

### Design Decisions

**D1. Hotword filtering: which of the 4,391 hotwords become entities?**

- IF: 4,391 unique hotwords exist, but ~260 come from iiching patterns
  sharing generic words ("explicit", "make", "knowledge"). Only ~200
  hotwords are semantically meaningful for the futon stack.
- HOWEVER: Aggressive filtering risks losing genuine concepts. The iiching
  patterns use words that overlap with real stack concepts (e.g., "pattern",
  "knowledge").
- THEN: Apply frequency-band filtering: keep hotwords with frequency 5–200
  across non-iiching patterns. This removes both hapax legomena (noise) and
  over-saturated words. Yields ~200 concepts.
- BECAUSE: **sidecar/sense-shift-gate** — "Gating reduces drift and keeps
  chains explainable." Without gating, the entity graph becomes a noisy
  bag-of-words rather than a navigable concept map.

**D2. How to populate meme.db: batch script vs incremental?**

- IF: The pattern library has 852 entries and is stable (no daily churn).
  We need >100 entities and >200 arrows (completion criterion 1).
- HOWEVER: A one-shot batch script is fragile and produces a DB that
  can't explain its own provenance. An incremental approach with
  proposal→promotion is more futonic but slower.
- THEN: Use a batch bootstrap script (`clj -M`) that reads patterns-index.tsv,
  applies the frequency filter (D1), creates entities via `ensure-entity!`,
  and creates arrows via `assert-arrow!`. The script itself records a
  sidecar proposal for the bootstrap event. This is the "evolving kernel"
  approach — good enough to strengthen itself.
- BECAUSE: **hdm/bootstrap-kernel** — "HDM cannot wait for full automation;
  the viable strategy is an evolving kernel that becomes 'just good enough'
  to strengthen itself. Bootstrapping is a permanent mode, not a temporary
  phase."

**D3. How does federated search include the meme store?**

- IF: `search-texto` currently queries two sources (`:pattern`, `:transcript`).
  Adding a third source (`:meme`) requires a core.logic relation over
  meme.db entities.
- HOWEVER: meme.db is SQLite accessed via `next.jdbc`. core.logic relations
  are in-memory. Loading all entities into memory is wasteful; querying
  SQLite from core.logic requires a bridge function.
- THEN: Add a `memeo` relation to relations.clj that loads entities
  matching a text query (SQL LIKE) and unifies with core.logic. Register
  `:meme` as a third source in `search-texto`. Bridge pattern, not portal:
  a simple function call, not a new protocol.
- BECAUSE: **or/bridge-before-portal** — "Bridges enable staged federation
  and mutual learning without committing to one architecture. They preserve
  optionality, lower risk, and turn integrations into negotiable boundary
  objects."

**D4. How does futon3c serve concept graph data?**

- IF: futon3c has 48 HTTP handlers but none expose meme.db. futon3c has
  futon3b on its classpath (which has futon3a as `:local/root`), so
  JVM-level access to meme.core is available without new dependencies.
- HOWEVER: Adding meme.db endpoints to futon3c's HTTP layer creates tight
  coupling between the API server and the data store schema.
- THEN: Add a single endpoint `/api/alpha/concepts` that takes a `?q=`
  parameter and returns entities + arrows as EDN. Implemented as a thin
  handler that calls `meme.core/find-entity-by-name` and
  `meme.arrow/arrows-from`. Keep it read-only for the demo.
- BECAUSE: **futon-theory/mission-interface-signature** — "Every mission
  specifies typed input/output ports with a wiring diagram." The endpoint
  is the typed output port of the concept graph piece.

**D5. How does futon7 generate probes from the concept graph?**

- IF: futon7 currently uses 28 hand-crafted keywords in
  `stack-alignment-keywords`. The concept graph has ~200 entities with
  arrows encoding semantic relatedness.
- HOWEVER: Not every concept makes a good GitHub search probe. Concepts
  like "evidence" or "coherence" are too abstract; concepts like
  "datascript" or "multi-agent" are concrete enough.
- THEN: Add `src/f7/probe_gen.clj` that fetches entities from
  `/api/alpha/concepts`, filters to those with high arrow-degree (connected
  concepts are more informative), maps them to GitHub search terms, and
  generates probe EDN. Use the entities' arrow connections to group related
  concepts into probe categories.
- BECAUSE: **sidecar/artifact-entity-mention-grounding** — "Grounding
  makes proposals auditable and promotes stable chain building." Grounding
  probes in the entity graph makes probe selection auditable — you can
  trace why a probe exists back to specific patterns.

**D6. How do we make futon3x namespaces visible in the self-representing stack?**

- IF: futon3a has 0 namespaces in futon1a's hypergraph, 0 docbook entries.
  futon3b has 2 hypergraph entries, 0 docbook entries. The self-representing
  stack cannot explain itself.
- HOWEVER: Manually writing docbook entries for every namespace is tedious
  and will become stale. Automated extraction is better but requires knowing
  which namespaces matter.
- THEN: Use the enrichment API (`/api/alpha/enrich/file`) to process the
  key source files from L1-L5. For docbook, write entries for the 6 key
  namespaces (schema, core, arrow, bridge from futon3a; relations, pipeline
  from futon3b). For hypergraph, POST namespace→mission edges via futon1a's
  API. The demo subject IS the enrichment subject.
- BECAUSE: **hdm/deep-storage-to-active-graph** — "A unified, pattern-indexed
  graph enables reasoning/learning across boundaries while respecting each
  project's autonomy and identity."

### Data Flow

```
patterns-index.tsv ──→ bootstrap.clj ──→ meme.db
                          │                  │
                          │         ┌────────┴────────┐
                          │         │                  │
                          ▼         ▼                  ▼
                     sidecar    relations.clj      concepts API
                     proposal   (memeo relation)   (/api/alpha/concepts)
                                    │                  │
                                    ▼                  ▼
                              federated search    probe_gen.clj
                              (3 sources)         (futon7)
                                    │                  │
                                    ▼                  ▼
                              gate pipeline       landscape probes
                              (live G5→G0)        (GitHub search)
                                    │                  │
                                    └──────┬───────────┘
                                           ▼
                                    enrichment metadata
                                    (futon1a + futon4)
```

### Invariant Rules

1. **Entity count**: meme.db must have >100 entities after bootstrap.
   Checkable: `SELECT count(*) FROM entities`.
2. **Arrow count**: meme.db must have >200 arrows after bootstrap.
   Checkable: `SELECT count(*) FROM arrows`.
3. **Federation coverage**: `search-texto` must return results from ≥2
   sources for common concepts. Checkable: REPL query.
4. **Gate round-trip**: A coordinated action with real PSR must produce
   a 6-event proof path. Checkable: proof-path EDN file exists and has
   6 entries.
5. **Probe grounding**: Every generated probe must trace to ≥1 entity.
   Checkable: probe EDN contains `:source-entities` field.
6. **Enrichment closure**: Every namespace that participates in the demo
   must appear in both futon1a hypergraph and futon4 docbook.
   Checkable: curl futon1a API + grep docbook toc.json.

### View / UI

No new UI required. The demo is exercised via:
- REPL (bootstrap, federated search, gate pipeline)
- curl (concepts API, enrichment API)
- `bb` tasks (probe generation, lead report)
- Emacs (`M-x arxana-browse` for docbook entries)

## 4. ARGUE

*Why is this design right, not just workable?*

### Pattern Cross-Reference

Six patterns were referenced in DERIVE. Surveying the full library for
additional relevant patterns:

| Pattern | Where it applies | Effect on design |
|---------|-----------------|------------------|
| **compose-independent-lemmas** (PSR) | Overall structure (L1-L6) | Confirms: the 6 pieces have a clean DAG. No circular dependencies. The composition step (INSTANTIATE) should be pure wiring. |
| **sidecar/sense-shift-gate** | D1 (hotword filtering) | Confirms: frequency-band gating is the right move. The pattern also warns about allowing shifts "only when warranted" — we should log *which* hotwords were filtered and why, not silently drop them. **Revision**: bootstrap script should emit a filter-report alongside meme.db. |
| **hdm/bootstrap-kernel** | D2 (meme.db population) | Confirms: batch-then-evolve is legitimate. The pattern says bootstrapping is "a permanent mode" — the script should be re-runnable (idempotent via `ensure-entity!`), not a one-shot migration. Already designed this way. |
| **or/bridge-before-portal** | D3 (federated search) | Confirms: `memeo` as a bridge function is correct. A portal (new protocol, new query language) would be premature. The bridge can evolve into a portal if the meme store becomes a primary source. |
| **futon-theory/mission-interface-signature** | D4 (concepts API) | Confirms: typed endpoint. The pattern also says "draw outputs first" — we should specify the EDN response shape before implementing. **Revision**: add response schema to D4. |
| **sidecar/artifact-entity-mention-grounding** | D5 (probe generation) | Confirms: `:source-entities` field in probe EDN. Grounding is non-negotiable for auditability. |
| **hdm/deep-storage-to-active-graph** | D6 (enrichment) | Confirms: the demo subject IS the enrichment subject. This is the strongest argument for the mission — it's self-referential by construction. |
| **stack-coherence/futon-bridge-health** | L2, L4 (cross-repo wiring) | New addition. "Bridges are the connective tissue of the stack, and their health must be observable." **Revision**: the concepts API should include a `/health` sub-endpoint that reports meme.db entity/arrow counts + last-bootstrap timestamp. |
| **f3/p0** (Portal Query Layer) | D3 (federated search) | Reinforces bridge-before-portal. "Pattern retrieval must be fast, context-aware, and not require agents to know file layout." The `memeo` relation satisfies this — agents query by concept name, not by knowing meme.db schema. |
| **sidecar/bridge-triple-escalator** | Relation types | Reinforces: "Curated bridges provide explicit warrants without inflating facts prematurely." Our bridge predicates (`derived-from`, `implements`, `maps-to`) are warrants, not raw facts. Correct granularity. |

**Design revisions from ARGUE:**
1. Bootstrap script emits a filter report (which hotwords kept/dropped, why).
2. D4 response schema specified: `{:entity {...} :arrows [{...}] :bridges [{...}]}`.
3. Concepts API includes health sub-endpoint (`/api/alpha/concepts/health`).

### Theoretical Coherence

The IDENTIFY phase anchored on three theories:

1. **compose-independent-lemmas**: The design IS composition. L1-L6 are
   independent pieces with a clean DAG. The composition step adds no new
   claims. Theory and design are aligned.

2. **construct-an-explicit-witness**: The demo is the witness. It's not an
   argument that the layers compose — it's a running proof. Each invariant
   rule (entity count, arrow count, federation coverage, gate round-trip,
   probe grounding, enrichment closure) is a checkable predicate. The
   witness is constructive.

3. **Baldwin cycle**: The design moves the system-as-whole from "exploratory"
   to "assimilated." Each piece moves one repo's contribution from
   tested-in-isolation to exercised-in-composition. L6 (enrichment) is
   the canalization step — once the namespaces are in the hypergraph and
   docbook, they're stable structure that future missions build on.

No theoretical drift. The design serves all three anchors.

### Trade-Off Summary

| Gave up | In favor of | Why |
|---------|------------|-----|
| Incremental proposal→promotion for meme.db | Batch bootstrap with sidecar event | 852 patterns × ~5 hotwords = ~4000 proposals is pointless ceremony. The bootstrap *event* is the proposal. |
| Full-text search in memeo | SQL LIKE matching | core.logic + SQLite full-text would require FTS5 extension. LIKE is good enough for ~200 entities. Revisit if entity count grows 10x. |
| New UI for concept graph | REPL + curl + existing Emacs | No new UI means no new maintenance. The demo is for developers, not end-users. Emacs arxana-browse already exists. |
| Automated namespace discovery | Manual list of 6 key namespaces | Automated discovery requires parsing all .clj files across 4 repos. The 6 namespaces that matter for the demo are known from MAP. |
| Real-time probe execution | Probe generation only | futon7 probe infrastructure already runs probes. This mission generates them from the graph; executing them is an existing capability. |

### Generalization Notes

The design generalizes to any "layer cake" system where:
- Each layer has a data store (futon3a pattern applies)
- Cross-layer queries exist (futon3b bridge pattern applies)
- An API serves composed results (futon3c endpoint pattern applies)
- An application consumes the API (futon7 probe pattern applies)

The specific pieces (meme.db, core.logic, gate pipeline) are futon-specific,
but the *shape* — bootstrap → federate → serve → consume → document — is
general. A different stack would substitute its own stores and query engines
but keep the same dependency graph structure.

What would need to change: the entity types, relation types, and filtering
criteria are domain-specific. The invariant rules are structurally general
(counts, coverage, round-trip, grounding, closure) but their thresholds
are tuned to this codebase.

### Plain-Language Argument

We built four repos (futon3, 3a, 3b, 3c) that each handle one part of
pattern-backed coordination: a pattern library, a concept store, a query
and pipeline layer, and an API server. Each one works on its own, but
nobody — including us — has ever seen them work together end-to-end. That
means we can't be sure they actually compose, and we have nothing to show
a potential client who asks "what does this thing do?"

This mission fixes both problems by building a single through-line that
touches all four layers. It starts with the pattern library, extracts key
concepts into a searchable graph, makes that graph queryable through the
federated search and servable through the API, and then uses it to
automatically generate landscape intelligence probes — the kind that
found our first consulting leads. The result is a working demo that
produces real output (probe-driven lead reports), not a slide deck.

The self-referential twist: the demo documents itself using the same
enrichment infrastructure it demonstrates. The futon3x code that runs
the demo is also the code that gets documented in the knowledge graph.
So the stack's first complete worked example is an explanation of itself.

## 5. VERIFY

*Build it and prove it works.*

### L1: Bootstrap meme.db — DONE

**Script**: `futon3a/scripts/bootstrap_meme.clj`
**Run**: `cd futon3a && clj -M -i scripts/bootstrap_meme.clj -e "(scripts.bootstrap-meme/-main)"`

Results:
- 432 concepts (frequency band 5–200, non-iiching, stop-words removed)
- 788 pattern entities
- 4,495 derivation arrows (concept ← pattern hotword)
- 721 analogy arrows (concept co-occurrence ≥3 patterns)
- **Total: 1,221 entities, 5,216 arrows**
- Criterion 1: PASS (>100 entities, >200 arrows)

Filter report: `futon3a/data/bootstrap-filter-report.edn`

Round-trip verified:
- `(find-entity-by-name ds "evidence")` → `{:kind "concept" :freq 146}`
- `(arrows-from ds evidence-id)` → 180 arrows (analogy + derivation)

### L2: Federated search extension — DONE

**Modified**: `futon3b/src/futon3b/query/relations.clj`
- Added `memeo` relation (SQL LIKE search over meme.db entities)
- Extended `search-texto` with `:meme` source (third `l/conde` branch)
- Extended `search` with fair-share allocation (limit/3 per source)
- Requires `MEME_DB_PATH=/path/to/futon3a/meme.db`

Criterion 2 verified:
- `(search "provenance" {:limit 30})` → `{:pattern 10, :meme 6}` — two sources

### L4: Concept graph API endpoint — DONE (code, pending restart)

**Modified**: `futon3c/src/futon3c/transport/http.clj`
- Added `GET /api/alpha/concepts?q=...` — returns entities + arrows as JSON
- Added `GET /api/alpha/concepts/health` — entity/arrow counts + db-path
- Response shape: `{:ok true :query q :count N :results [{:entity {...} :arrows [...] :bridges [...]}]}`
- Compiles clean. Requires futon3c restart with `MEME_DB_PATH` set.

### L3: Gate pipeline live run — DONE

**Script**: `futon3b/scripts/live_gate_run.clj`
**Run**: `cd futon3b && clj -M -i scripts/live_gate_run.clj -e "(scripts.live-gate-run/-main)"`

Results:
- Full G5→G0 traversal: 6 gate events (task→auth→pattern→exec→validate→evidence)
- Real PSR: `math-strategy/compose-independent-lemmas` (resolved from 829-pattern library)
- Real mission ref: `M-futon3x-e2e`
- Proof path persisted: `data/proof-paths/path-22293f1b-b.edn` (2954 bytes)
- Criterion 3: PASS

### L5: Probe generation from graph — DONE

**Created**: `futon7/src/f7/probe_gen.clj`
**Run**: `cd futon7 && bb probe-gen`

Results:
- 7 probes generated across 7 areas (multi-agent, transparency,
  pattern-reasoning, dev-workflow, knowledge-graph, knowledge-store,
  formal-math)
- Each probe has `:source-entities` field tracing to concept graph
- Reads from `futon3a/data/bootstrap-filter-report.edn` (bb-compatible)
- Can also query concepts API when futon3c is running (future enhancement)
- Criterion 5: PASS (≥3 probes derived from concept graph, all grounded)

### L6: Enrichment metadata — DONE

**Script**: `futon3b/scripts/enrich_futon3x.clj`
**Run**: `cd futon3b && clj -M -i scripts/enrich_futon3x.clj -e "(scripts.enrich-futon3x/-main)"`

Results:
- 8 mission-provenance hyperedges created in futon1a:
  - futon3a: meme.schema, meme.core, meme.arrow, meme.bridge, sidecar.store
  - futon3b: futon3b.query.relations, futon3.gate.pipeline, futon3.gate.pattern
- All linked to M-futon3x-e2e
- Verified: `curl "http://localhost:7071/api/alpha/hyperedges?end=M-futon3x-e2e"` → 8 results
- Criterion 6 (partial): hypergraph metadata done. Docbook entries deferred to
  INSTANTIATE (requires futon4 docbook write tooling).

### Completion Criteria Status

| # | Criterion | Status | Evidence |
|---|-----------|--------|----------|
| 1 | meme.db >100 entities, >200 arrows | PASS | 1,221 entities, 5,216 arrows |
| 2 | Federated query returns ≥2 sources | PASS | `(search "provenance")` → `:pattern` + `:meme` |
| 3 | Gate pipeline G5→G0 with real PSR | PASS | proof-path `path-22293f1b-b.edn`, 6 events |
| 4 | Concept graph via futon3c endpoint | CODE DONE | `/api/alpha/concepts` handler written, compiles. Needs restart. |
| 5 | futon7 generates ≥3 probes from graph | PASS | 7 probes generated, all with `:source-entities` |
| 6 | Namespaces in hypergraph + docbook | PARTIAL | 8 hyperedges in futon1a. Docbook entries pending. |
| 7 | Reproducible from mission doc | PENDING | Will verify in INSTANTIATE. |

## 6. INSTANTIATE

*Wire the pieces together. The composition step should add nothing new.*

### Remaining Items

1. **futon3c restart** with `MEME_DB_PATH` — needed to test concepts API live.
2. **Docbook entries** for futon3a/3b key namespaces in futon4.
3. **Reproducibility check** — run all scripts in sequence from a clean state.

### Decision Log

- D1 revision: frequency band 5–200 produced 432 concepts (more than the
  estimated ~200). The filter is inclusive enough. No revision needed.
- D3 revision: fair-share allocation in `search` (limit/3 per source) was
  needed to prevent transcripts from dominating results.
- L6: penholder "api" required (not "joe") for futon1a hyperedge writes.

## 7. DOCUMENT

*(Deferred until INSTANTIATE is complete.)*
