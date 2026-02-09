# Mission: F6 Arxiv Ingest — Scaling to Research Mathematics

Extend the math.SE pipeline to process Arxiv papers. Different beast:
longer documents, deeper proof structures, citation networks, and the
full spectrum from informal blog-posts-as-papers to Bourbaki-level rigour.
This is where the decade-long vision meets real mathematical research.

## Owner

Joe (+ Claude for implementation, potentially Rob for superpod access)

## Scope

### Scope In

- Adapt the StackExchange pipeline (superpod-job.py) for Arxiv LaTeX sources
- Handle Arxiv-specific structure: sections, theorems, proofs, citations
- Build document-level knowledge graphs (not just QA-pair-level)
- Connect Arxiv graphs to the math.SE graph via shared terms and concepts
- Process a pilot corpus: papers citing the sources we already know about

### Scope Out

- Full Arxiv bulk processing (millions of papers — future infrastructure work)
- PDF-only papers without LaTeX source (different pipeline, out of scope)
- Citation network analysis as a primary goal (useful byproduct, not the mission)
- Formal verification of paper proofs (out of scope)

## Time Box

Pilot corpus: 2-3 sessions. Pipeline adaptation: 1-2 sessions.
First significant run: 1 superpod session.

## Prerequisites

- [ ] M-f6-ingest completed: math.SE pipeline proven at scale
- [ ] M-f6-eval completed: we know graph-enhanced QA works (validates the approach)
- [ ] M-f6-agents at least Phase 1: agents can consume graph context
- [ ] Arxiv bulk data access resolved (S3 requester-pays or institutional mirror)
- [ ] LaTeX parser handles Arxiv conventions (custom macros, multi-file projects)
- [ ] NER kernel expanded with Arxiv-specific terms (from M-f6-ingest bootstrap)

## Design

### Why Arxiv Is Different from Stack Exchange

| Dimension | Math.SE | Arxiv |
|-----------|---------|-------|
| Unit size | 1 question + answers (~500 words) | 1 paper (~5,000-50,000 words) |
| Structure | Flat: question, answer, comments | Hierarchical: sections, theorems, proofs, lemmas |
| Formality | Mixed, often informal | Spectrum: informal survey to formal proof |
| Citations | Rare, usually links | Dense, structured bibliography |
| Scope chains | Short (1-3 Let bindings) | Deep (nested assumptions across sections) |
| Social signal | Votes, accepted answer | Peer review, citation count |

The pipeline must handle this complexity without losing the simplicity that
made the SE pipeline work.

### Adaptation Strategy

**Don't rebuild — extend.** The SE pipeline's 5 stages map to Arxiv:

1. **Parse**: XML streaming → LaTeX parsing (LaTeXML or custom).
   Split papers into sections, extract theorem/proof/definition environments.
   Each environment becomes an entity; the paper becomes a container entity.

2. **Embeddings**: Same bge-large-en-v1.5. Embed at multiple granularities:
   paper abstract, section, theorem/proof pair, individual paragraph.

3. **LLM pattern tagging**: Same 25 math-informal patterns, but applied
   per-section rather than per-QA-pair. Expect different pattern distributions
   (more argue-by-contradiction, fewer try-a-simpler-case).

4. **Clustering**: Cluster papers by topic (using abstract embeddings) and
   sections by technique (using section embeddings). Cross-reference with
   math.SE clusters.

5. **NER + scope detection**: Same kernel, but scope chains are longer.
   Need to track scope across theorem → proof → lemma → sub-proof.
   Section boundaries as scope resets.

### Document-Level Graph

Each paper produces a local knowledge graph:

```
Paper
├── Abstract (entity, embedded)
├── Section 1: Introduction
│   ├── Paragraph entities
│   └── Term references (links to kernel)
├── Section 2: Preliminaries
│   ├── Definition 2.1 (entity, NER-rich)
│   ├── Definition 2.2
│   └── Scope: variables introduced here carry forward
├── Section 3: Main Result
│   ├── Theorem 3.1 (entity)
│   ├── Proof of Theorem 3.1
│   │   ├── Lemma 3.2 (sub-entity)
│   │   ├── Pattern tags: argue-by-contradiction, reduce-to-known-result
│   │   └── Scope: inherits from Section 2 + local Let bindings
│   └── Corollary 3.3
├── Section 4: Applications
│   └── Links to math.SE questions in same topic cluster
└── Bibliography
    └── Citation links to other paper entities
```

### The Citation Bridge

Citations create cross-paper relations. When paper A cites paper B:
- If B is in our corpus: create a direct relation with context
  (what claim in A depends on what result in B)
- If B is not in our corpus: create a stub entity with metadata
  (title, authors, year) as a frontier node for future processing

This grows the graph organically: process papers that fill the most gaps.

### Pilot Corpus Selection

Start with papers we can validate against existing knowledge:

1. **Papers by math.SE answerers**: Cross-reference SE user profiles with
   Arxiv author lists. These papers likely cover topics already in the graph.

2. **Papers cited in math.SE answers**: Direct links from QA pairs we've
   already processed.

3. **Survey papers in well-covered topics**: High NER kernel coverage expected,
   good test of the pattern tagger on longer texts.

4. **PlanetMath-adjacent papers**: Papers whose terminology overlaps heavily
   with the NER kernel's PlanetMath core.

Target: 500-1000 papers for pilot, selected for maximum overlap with
existing graph.

## Outpost

### Outpost O-4: One Paper

Before building the full Arxiv pipeline, test the approach on a single paper
whose topic overlaps with existing physics.SE data.

**Data**: One Arxiv paper selected for: (a) LaTeX source available, (b) topic
covered in physics.SE, (c) moderate length (10-20 pages), (d) standard
structure (theorems, proofs, definitions). A paper cited in a physics.SE
answer is ideal.
**Method**:
1. Download LaTeX source, extract sections and theorem/proof environments
2. Run NER kernel term spotting on each section
3. Run scope detection on theorem/proof pairs
4. Count pattern tag matches per section
5. Identify bridge concepts: NER terms shared with physics.SE entities

**Validates**:
- [ ] LaTeX parser handles at least this paper's structure without crashing
- [ ] NER kernel achieves reasonable coverage (>50% of sections have term hits)
- [ ] Scope chains are detectable across theorem→proof boundaries
- [ ] At least 5 bridge concepts connecting this paper to physics.SE entities
- [ ] Pattern distribution differs visibly from SE (more formal, fewer heuristic)

**Depends on**: NER kernel, pattern tagger, one Arxiv paper (manual selection)
**Feeds**: Feasibility signal for the full Arxiv pipeline; identifies LaTeX
parsing issues before investing in bulk processing

**Pattern references**: `f6/stratum-bridge`, `f6/scope-chain-tracking`

---

## Implementation Sketch

### Phase 1: LaTeX parser for Arxiv

Extend or wrap existing LaTeX processing (futon6 already parses PlanetMath .tex):
- Handle `\begin{theorem}...\end{theorem}` environments
- Extract `\label` / `\ref` cross-references within papers
- Parse `\bibliography` / `\bibitem` for citation extraction
- Handle multi-file projects (`\input`, `\include`)
- Graceful degradation: if parsing fails on a section, skip it, don't crash

### Phase 2: Pilot corpus acquisition

- Use Arxiv bulk data access (S3 or OAI-PMH metadata + source download)
- Select pilot papers using the criteria above
- Download, extract, validate LaTeX source availability
- Build manifest of processable papers

### Phase 3: Pipeline run

- Adapt superpod-job.py for Arxiv input format
- Run on pilot corpus (expect 2-4 hours for 1000 papers)
- Produce same output artifacts as SE pipeline plus:
  - `paper-graphs.json` — per-paper local knowledge graphs
  - `citations.json` — cross-paper citation relations
  - `environments.json` — theorem/proof/definition entities

### Phase 4: Graph integration

- Merge Arxiv paper graphs with math.SE graph
- Align shared concepts (same NER terms, same topic clusters)
- Identify bridge nodes: entities that appear in both SE and Arxiv
- Feed integrated graph to agents (M-f6-agents Phase 2+)

## Success Criteria

- [ ] LaTeX parser handles >90% of pilot papers without crashing
- [ ] All 25 patterns fire on Arxiv data (expect different distribution from SE)
- [ ] NER kernel achieves >80% entity coverage on pilot papers
- [ ] Scope detection tracks variable bindings across theorem/proof pairs
- [ ] Citation extraction produces valid cross-paper relations
- [ ] At least 100 bridge nodes connecting Arxiv graph to math.SE graph
- [ ] Agents (from M-f6-agents) can answer questions using combined graph
- [ ] Pipeline completes 1000 papers within a single superpod session

## Failure Modes

| Failure | What it means |
|---------|---------------|
| LaTeX parsing too brittle | Arxiv papers use wild custom macros; may need LaTeXML as preprocessor |
| NER coverage drops below 50% | Research math uses terminology not in PlanetMath; need Arxiv-sourced term expansion |
| Scope chains too deep to track | May need to simplify to section-level scope rather than full nesting |
| Citation extraction unreliable | BibTeX parsing is well-solved; custom \bibitem formats less so |
| Graph integration produces noise | Alignment heuristics (shared terms) may over-connect; need confidence thresholds |
| Pilot corpus too homogeneous | Selection bias toward well-covered topics; won't test generalization |

## Connects To

- **Depends on**: M-f6-ingest (pipeline), M-f6-eval (validates approach),
  M-f6-agents (consumers of the graph)
- **Feeds**: Future work on full Arxiv processing, mathematical search,
  research recommendation
- **Devmap**: f6/P7 (StackExchange Import — extends the pipeline),
  f6/P9 (Arxiv Integration — this IS P9)
- **Grant lineage**: AI4CI Month 1 (corpus preparation, Arxiv mentioned
  explicitly), LEAPQA T1 (translation of technical NL), ATI Fellowship
  (Q&A for Computers — the "Computers" are researchers using Arxiv)

## Context

Every grant proposal listed Arxiv as a target corpus. The ATI proposal
(2015) wanted to process "research-level mathematics". LEAPQA (2017)
proposed "Stack Exchange as training ground, Arxiv as deployment".
The AI4CI proposal (2026) listed Arxiv as a Month 1 data source.

The strategic ordering matters: math.SE first because it's structured
(questions have answers, answers have votes), smaller, and provides
ground truth. Arxiv second because the SE-trained pipeline and
SE-expanded NER kernel make Arxiv processing tractable. Without the
SE foundation, Arxiv would be a cold start.

The Peeragogy connection: math.SE is the "peer learning" corpus (people
helping each other understand). Arxiv is the "peer production" corpus
(people creating new knowledge). The knowledge graph bridges both:
what people ask about and what people prove about the same concepts.
