# Mission: F6 Evaluation — Graph-Enhanced Mathematical Q&A

Prove that the structured knowledge graph from M-f6-ingest makes mathematical
question answering measurably better. Two agents, same questions, one has the
graph. This is the test that turns corpus linguistics into something with teeth.

## Owner

Joe (+ Claude for implementation)

## Scope

### Scope In

- Design evaluation protocol: graph-enhanced vs bare LLM on math questions
- Build the graph query layer that an LLM agent can use
- Run paired evaluation: question generation + question answering
- Measure: answer quality, factual accuracy, citation of relevant concepts
- Test the negative-space hypothesis: does knowing which parts of an answer
  are formal scaffolding vs informal reasoning improve comprehension?

### Scope Out

- Full tutoring system (that emerges from M-f6-agents)
- Arxiv-scale evaluation (deferred to M-f6-arxiv)
- User studies with human mathematicians (future work, cf. AI4CI Month 4-5)

## Time Box

2-3 sessions for design + implementation. 1 session for running evaluation.

## Prerequisites

- [ ] M-f6-ingest completed: annotated math.SE corpus with entities, relations,
      pattern tags, NER terms, scope records, embeddings, clusters
- [ ] Local access to processed output (entities.json, pattern-tags.json, etc.)
- [ ] LLM access for both agents (Claude API or local model)

## Design

### The Two Agents

**Agent A (Bare)**: receives only the raw question text and answer text.
No access to structured data.

**Agent B (Graph-enhanced)**: receives the question text plus:
- NER term annotations (which mathematical terms appear, with definitions)
- Pattern tags (which informal reasoning patterns the answer uses)
- Scope records (what variables are bound where, with types)
- Cluster context (what other QA pairs are in the same topic cluster)
- Related entities (via the relation graph)

### Evaluation Tasks

**Task 1: Question Answering**
Given a math.SE question, both agents produce an answer.
A held-out human answer (the actual accepted answer) serves as ground truth.
Metrics: factual overlap, conceptual coverage, reasoning coherence.

**Task 2: Question Generation**
Given a math.SE answer, both agents generate follow-up questions.
Metrics: question relevance, depth, whether questions target genuine gaps.

**Task 3: Concept Explanation**
Given a mathematical term from the NER kernel, both agents explain it
in the context of a specific QA pair where it appears.
Metrics: accuracy, use of related concepts, connection to proof structure.

**Task 4: Negative Space Navigation**
Given an answer with both NER annotations and pattern tags, Agent B
is asked to identify the informal reasoning moves and explain *why*
they work in this context. Agent A gets only the raw text.
Metrics: identification accuracy vs the pattern tagger's labels,
explanatory quality.

### Evaluation Method

**Self-play scoring**: Each agent's answers are scored by an LLM judge
(following the AI4CI "self-play question-answering" design). The judge
sees both answers blind and rates: clarity, accuracy, completeness, depth.

**Automated metrics**: ROUGE/BERTScore against held-out answers for Task 1.
Pattern recall against tagger labels for Task 4.

**Stratified sampling**: Evaluate across difficulty levels:
- Easy: single-concept questions with short, direct answers
- Medium: multi-step reasoning with 2-3 concepts
- Hard: proof-based answers with scope chains and multiple patterns

## Outposts

### Outpost O-0: The Classical Baseline

Before running any LLM evaluation, reproduce the Corneli (2014) Chapter 6
method on physics.SE data using the NER kernel. This is a standalone research
result that also creates the human-learning baseline for agent comparison.

**Data**: physics.SE (`data/se-physics.json`, 114K pairs) — edit histories
available in the SE data dump's PostHistory table
**Method**:
1. For each SE user with sufficient history: extract their term vocabulary
   trajectory using the NER kernel (19,236 terms)
2. Identify learning events: first use of each technical term by that user
3. Identify treatment events: answers received, comments received, accepted
   answers on their questions (all timestamped)
4. Fit the Ornstein-Uhlenbeck impulse-damping model: does a treatment event
   produce a detectable impulse in learning-event rate?

**Validates**:
- [ ] NER kernel produces meaningful vocabulary trajectories on SE data
- [ ] Learning events are detectable (non-zero rate of new term adoption)
- [ ] O-U model fits with plausible parameters (positive impulse, finite damping)
- [ ] Results are comparable to Corneli (2014) PlanetMath findings (445 users)

**Depends on**: NER kernel (already built), physics.SE data (already have),
SE PostHistory table (need to extract from data dump)
**Feeds**: Human-learning baseline for M-f6-agents agent comparison;
validation that term spotting measures something real

**Pattern references**: `f6/learning-event-detection`, `f6/bootstrap-loop`

### Outpost O-2: Tiny Eval

Before the full 200-question evaluation, run a micro-evaluation on 20
physics.SE questions to test the protocol and graph query interface.

**Data**: 20 physics.SE questions hand-selected for difficulty spread
(7 easy, 7 medium, 6 hard)
**Method**:
1. Build a minimal MathKnowledgeGraph from existing NER annotations
   (term hits from spot-terms.bb output, pattern tags from tag-patterns.bb)
2. Agent A: Claude answers each question with raw text only
3. Agent B: Claude answers with raw text + graph context (terms, patterns,
   scope, related QA)
4. Judge: Claude scores both answers blind, randomised order

**Validates**:
- [ ] Graph query interface works and returns useful context
- [ ] Agent B prompt template produces coherent responses
- [ ] Judge can distinguish answer quality (non-random scores)
- [ ] At least a trend showing Agent B advantage on harder questions

**Depends on**: NER kernel, pattern tags on physics.SE, spot-terms output
**Feeds**: Protocol refinement before the full 200-question run;
early signal on whether graph enhancement helps at all

**Pattern references**: `f6/graph-enhanced-evaluation`, `f6/negative-space-duality`

---

## Implementation Sketch

### Graph Query Interface

```python
class MathKnowledgeGraph:
    """Query interface for the annotated math.SE corpus."""

    def lookup_terms(self, entity_id) -> list[TermHit]:
        """What NER terms appear in this entity?"""

    def lookup_patterns(self, entity_id) -> list[PatternTag]:
        """What informal reasoning patterns does this answer use?"""

    def lookup_scopes(self, entity_id) -> list[ScopeRecord]:
        """What variable bindings / scope openers appear?"""

    def related_entities(self, entity_id, max_hops=2) -> list[Entity]:
        """Entities connected via shared terms, tags, or clusters."""

    def term_definition(self, term: str) -> str:
        """Definition of a term from the NER kernel."""

    def similar_qa(self, entity_id, k=5) -> list[Entity]:
        """Nearest neighbors by embedding similarity."""
```

### Agent B Prompt Template

```
You are a mathematics Q&A assistant with access to a structured
knowledge graph. For this question, the graph tells you:

TERMS PRESENT: {term_list with definitions}
REASONING PATTERNS USED IN KNOWN ANSWERS: {pattern_list}
VARIABLE BINDINGS: {scope_records}
RELATED Q&A: {similar_qa_summaries}

Use this structured information to provide a clear, accurate answer.
When you use a reasoning pattern, name it explicitly.
When you introduce a variable, note its scope.
```

### Scoring Protocol

For each question in the evaluation set (N=200, stratified):
1. Agent A generates answer from raw question only
2. Agent B generates answer from question + graph context
3. Judge LLM scores both answers (blind, randomised order)
4. Automated metrics computed against held-out answer

## Success Criteria

- [ ] Agent B outperforms Agent A on answer quality (judge scores)
- [ ] Agent B shows higher factual accuracy (fewer hallucinations)
- [ ] Agent B identifies more reasoning patterns correctly (Task 4)
- [ ] Agent B produces better follow-up questions (Task 2)
- [ ] Results are statistically significant across the 200-question sample
- [ ] Improvement is largest on Hard questions (where structure matters most)
- [ ] Results documented with reproducible methodology

## Failure Modes

| Failure | What it means |
|---------|---------------|
| No significant difference | Graph annotations may be too shallow; need richer structure |
| Agent B worse on Easy questions | Graph context may be distracting for simple questions; need adaptive context |
| Judge scores unreliable | Need human spot-check or inter-rater agreement test |
| Pattern identification low | Pattern tagger may need tuning; or patterns too coarse |

## Connects To

- **Depends on**: M-f6-ingest (needs the annotated corpus)
- **Feeds**: M-f6-agents (evaluation protocol becomes the agent's self-assessment)
- **Feeds**: M-f6-arxiv (same evaluation framework applied to papers)
- **Devmap**: f6/P4 (Reasoning Map — the graph IS the reasoning map)
- **Grant lineage**: AI4CI Months 4-5 (Evaluation), LEAPQA Objective 3

## Context

The AI4CI proposal designed this evaluation but never ran it. The key insight
from the negative-space analysis: NER covers formal scaffolding, patterns
cover informal reasoning, and the two together give an agent a structured
understanding of mathematical text that flat text doesn't provide.

If this works, it proves the decade-long thesis: structured mathematical
knowledge representations make AI systems measurably better at mathematical
reasoning. If it doesn't work, it tells us exactly where the structure is
insufficient and what to fix before scaling to Arxiv.
