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
3. A coordinated action traverses all 6 gates (G5→G0) with a real PSR
   referencing a real pattern, producing a proof-path EDN file.
4. A concept graph query is servable via futon3c (HTTP endpoint or Drawbridge).
5. futon7 generates at least 3 probes derived from the concept graph (not
   hand-crafted), and those probes return results.
6. The full demo is reproducible from the mission doc by a new agent.

### Relationship to Other Missions

- **M-futon-enrichment** (futon4): Phase 5 of that mission depends on this
  mission's application phase. This is the *how*; that is the *where it lives*.
- **M-f7-lead-report** (futon7): Predecessor. Proved probe→report works.
  This mission adds probe *generation* from the graph.
- **M-P3-rational-reconstruction** (futon6): Demonstrated pattern discipline
  on mathematical content. This demonstrates it on code infrastructure.
- **M-coordination-rewrite** (futon3): Built the gate pipeline (futon3b).
  This is the first end-to-end exercise of that pipeline with real data.

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

### Ready vs Missing

*(To be filled in during MAP phase)*

| Ready (no new code) | Missing (the actual work) |
|---------------------|--------------------------|
| | |

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
