# M-futon3x-e2e

**Status:** IDENTIFY (2026-03-07)

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

*(Deferred until MAP is complete.)*

## 4. ARGUE

*(Deferred until DERIVE is complete.)*

## 5. VERIFY

*(Deferred until ARGUE is complete.)*

## 6. INSTANTIATE

*(Deferred until VERIFY is complete.)*

## 7. DOCUMENT

*(Deferred until INSTANTIATE is complete.)*
