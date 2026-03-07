# M-futon3x-e2e

## Status: IN-PROGRESS

## Goal

Demonstrate end-to-end integration of the futon3x series (futon3, futon3a,
futon3b, futon3c) through a concrete workflow that exercises every layer of
the stack. The demo should be legible to someone evaluating the futon stack
for consulting work.

## Context

The futon3x repos form a layered system, but they've never been demonstrated
working together end-to-end:

| Repo | Layer | Role | Current state |
|------|-------|------|--------------|
| futon3 | Content | Pattern library (852 patterns, 38+ namespaces, flexiarg format) | Mature, well-populated |
| futon3a | Store | Meme store (SQLite entities/arrows/bridges), ANN embeddings (FastText/GloVe/MiniLM), sidecar event store (proposals→facts→chains) | Schema + code exist, DB not bootstrapped |
| futon3b | Query | core.logic federated search, G5→G0 gate pipeline, proof paths | Library, used by futon3c |
| futon3c | Service | HTTP API, reflection, enrichment queries, agent dispatch | Running on port 7070 |

Each piece works in isolation. What's missing is the *through-line*: a single
workflow that starts with a pattern, traverses all four layers, and produces
a useful result.

## The Demo Workflow

**Input**: A question like "What does the futon stack know about X?"

**Flow**:
1. **futon3** — Find relevant patterns via keyword/content search
2. **futon3b** — Federated search across patterns + transcripts + proof paths
3. **futon3a** — Store discovered concepts as entities, build arrows between them
4. **futon3c** — Serve the results via API (enrichment endpoint or new endpoint)
5. **Application** — Use the concept graph to generate futon7 landscape probes

**Output**: A set of landscape probes derived from the stack's own knowledge,
plus a lead report showing what the probes found.

## Architecture

```
                    futon3 (pattern library)
                         |
                    852 flexiargs with @keywords
                         |
              futon3b (federated query)
              |                    |
    core.logic patterno     transcript-texto
    (keyword + content)     (session history)
              |                    |
              +--------+-----------+
                       |
              futon3a (entity graph)
              |                    |
    entities + aliases      arrows + bridges
    (concepts from          (relationships from
     pattern keywords)       co-occurrence + modes)
              |
              +--- embeddings (ANN neighbor search)
              |
       futon3c (HTTP API)
              |
    /api/alpha/concepts?q=...  (new endpoint)
              |
       futon7 (probe generation)
              |
    GitHub search → signal extraction → lead report
```

## Phases

### Phase 1: Bootstrap — Seed the Entity Graph

**Goal**: Populate futon3a's meme store from the pattern library.

The pattern library has 852 entries with hotwords. Each hotword is a concept.
Patterns that share hotwords are related. This gives us:

1. **Entities**: One per unique concept (deduplicated hotword)
2. **Aliases**: Surface forms that map to the same entity
3. **Arrows**: Co-occurrence within a pattern = `:derivation` or `:analogy` arrow
4. **Scope tags**: Pattern namespace (agent, math-informal, code-coherence, etc.)

Approach:
- Parse patterns-index.tsv (already in futon3a/resources/notions/)
- For each pattern: create entity for each keyword, arrow between keywords
  that co-occur in the same pattern
- Use futon3b's `patterno` relation to validate: every pattern-id in the
  entity graph should resolve via core.logic

**Deliverable**: `scripts/bootstrap-meme.clj` (or .bb), populated meme.db,
entity/arrow counts.

### Phase 2: Query — Federated Search Integration

**Goal**: Show that a single query traverses all stores.

Build a query that:
1. Takes a concept (e.g., "provenance")
2. Finds matching patterns via futon3b `pattern-texto`
3. Finds matching entities via futon3a meme store
4. Finds matching transcripts via futon3b `transcript-texto`
5. Returns a unified result with source attribution

This exercises the federated search that futon3b was designed for but hasn't
been demonstrated end-to-end with a populated meme store.

**Deliverable**: `scripts/federated-query.clj`, example output showing results
from all three stores.

### Phase 3: Validate — Gate Pipeline Round-Trip

**Goal**: Show that the G5→G0 gate pipeline works with real data.

Construct a minimal coordinated action that:
1. G5 (task): Define a task — "find landscape probes for concept X"
2. G4 (auth): Pass auth check
3. G3 (pattern): Carry a PSR referencing a real pattern
4. G2 (exec): Execute the probe generation
5. G1 (validate): Validate the output
6. G0 (evidence): Record the result as evidence with a proof path

This is the gate pipeline's intended workflow — the one documented in futon3b
but never demonstrated with all gates active and real pattern references.

**Deliverable**: A complete proof-path EDN file showing all 6 gates passed,
with a real PSR referencing a real pattern.

### Phase 4: Serve — API Endpoint

**Goal**: Expose the concept graph via futon3c's HTTP API.

Add a minimal endpoint:
```
GET /api/alpha/concepts?q=provenance&depth=2
```

Returns entities reachable within `depth` hops, with their arrows and
source patterns. This is the API that futon7's probe generator will call.

Could also be a Drawbridge REPL query if adding an HTTP endpoint is too
heavyweight for now.

**Deliverable**: Working query path from HTTP → futon3b → futon3a → response.

### Phase 5: Application — Landscape Probe Generation

**Goal**: Use the concept graph to generate futon7 probes, run them, and
produce a lead report.

This is the payoff phase — the one that shows the integration is *valuable*.

1. Query the concept graph for entities in stack-relevant namespaces
   (agent, code-coherence, math-informal, etc.)
2. Walk outward via arrows to find adjacent concepts
3. Map concept clusters to GitHub search queries
4. Run probes via futon7's existing infrastructure
5. Generate a lead report comparing generated probes vs hand-crafted ones

**Deliverable**: `futon7/src/f7/probe_gen.clj`, comparison report, evidence
that generated probes discover leads the hand-crafted probes missed.

### Phase 6: Evidence — Close the Loop

**Goal**: Record the entire demo as evidence in the enrichment graph.

1. Write lead results back to futon1a as `landscape/lead` hyperedges
2. Record the mission's own PSR/PUR trail
3. Update M-futon-enrichment Phase 5 with concrete evidence

**Deliverable**: Hyperedge count delta, PSR/PUR log, updated mission docs.

## What This Demonstrates

For someone evaluating the futon stack:

1. **Pattern discipline scales**: 852 patterns aren't just documentation —
   they're a queryable knowledge base that drives decisions.

2. **Stores compose**: SQLite meme store + XTDB evidence store + filesystem
   pattern library, unified by core.logic federated search.

3. **Gates enforce quality**: Every coordinated action traverses 6 gates
   with proof-path evidence. You can audit why any decision was made.

4. **The graph is useful**: The concept graph derived from patterns generates
   landscape intelligence that finds real commercial leads.

5. **It runs**: Not slides, not architecture diagrams — running code serving
   real HTTP requests with real data.

## Dependencies

- futon3c running (port 7070) — for API serving
- futon1a running (port 7071) — for hyperedge persistence
- futon3a meme.db initialized — Phase 1 creates this
- futon7 probe infrastructure — exists (bb lead-report works)
- Pattern library populated — exists (852 entries)

## Relationship to Other Missions

- **M-futon-enrichment**: Phase 5 of that mission depends on this mission's
  Phase 5. This mission is the *how*; that mission is the *where it lives*.
- **M-f7-lead-report**: Predecessor. Proved that probe→report works.
  This mission adds the missing piece: deriving probes from the graph.
- **M-P3-rational-reconstruction**: Demonstrated pattern discipline on
  mathematical content. This mission demonstrates it on code infrastructure.
- **M-coordination-rewrite**: Built the gate pipeline (futon3b).
  This mission is the first end-to-end exercise of that pipeline.

## PSR

- Pattern chosen: compose-independent-lemmas (math-strategy)
- Candidates: compose-independent-lemmas, construct-an-explicit-witness,
  reduce-to-known-result
- Rationale: The mission's core structure IS composition of independent
  pieces (futon3, 3a, 3b, 3c, 7). Each phase establishes one piece.
  The composition step (Phase 5) should add nothing new — if it does,
  a piece is incomplete. This is exactly what the pattern warns about.
