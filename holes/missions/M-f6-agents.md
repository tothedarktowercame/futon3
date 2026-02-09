# Mission: F6 Artificial Stack Exchange

Build agents that don't just answer mathematical questions but *ask* them.
The Q⊢A framing: a well-posed question constrains its answer. Agents that
identify what they don't know and formulate the right question are doing
mathematics, not retrieval.

## Owner

Joe (+ Claude for implementation, potentially other agents)

## Scope

### Scope In

- Design agent architecture for mathematical Q&A: asker, answerer, critic
- Implement Pólya-pattern-driven question decomposition ("find an easier problem")
- Build the self-play loop: agents ask, answer, evaluate, and learn
- Use the evaluation framework from M-f6-eval as the agents' self-assessment
- Integrate with the knowledge graph: agents update the graph as they learn

### Scope Out

- Full tutoring system for human students (future work)
- Deployment to public platforms (future work)
- Formal verification of agent reasoning (out of scope)

## Time Box

Open-ended exploration. Initial prototype: 2-3 sessions.

## Prerequisites

- [ ] M-f6-ingest completed: annotated corpus available
- [ ] M-f6-eval completed: we know graph-enhanced QA works and by how much
- [ ] Knowledge graph query interface operational
- [ ] Pattern library accessible to agents (25 math-informal patterns + kernel)

## Design

### The LEAPQA Vision, Realised

The EPSRC proposal described three objectives:
- T1: translate technical NL into formal knowledge representations ← M-f6-ingest
- T2: functional processes for heuristic reasoning ← **this mission**
- T3: evaluate via synthetic tasks and tutoring ← M-f6-eval (already done)

The SFI proposal added: Tangled Program Graphs (agents as linked rule sets),
active inference (agents seek to minimise uncertainty), Ostrom's IAD
(institutional rules governing agent interaction).

What we actually build draws on all of these but stays grounded in what works.

### Agent Roles

**The Asker** — generates questions by identifying gaps in the knowledge graph.
- Finds entities with few connections (isolated nodes)
- Notices clusters with no inter-cluster links (conceptual gaps)
- Detects answers tagged with patterns but missing scope records (informal
  reasoning that hasn't been grounded)
- Uses Pólya patterns to decompose hard questions into easier sub-questions

**The Answerer** — uses the knowledge graph to construct answers.
- Retrieves relevant entities, terms, patterns via graph queries
- Follows scope chains to build proof sketches
- Cites related QA pairs as evidence
- Names the reasoning patterns it's using (from the math-informal library)

**The Critic** — evaluates QA pairs using the M-f6-eval scoring protocol.
- Scores answers for accuracy, completeness, coherence
- Identifies where the answer is weakest
- Suggests follow-up questions (feeds back to the Asker)
- Flags disagreements between NER annotations and answer content

### The Self-Play Loop

```
┌─────────────┐
│   ASKER     │──── generates question ────→┌──────────────┐
│ (gap finder)│                              │   ANSWERER   │
└─────────────┘←── follow-up questions ─────│ (graph-aware) │
       ↑                                     └──────────────┘
       │                                            │
       │              evaluates pair                │
       │         ┌──────────────┐                   │
       └─────────│    CRITIC    │←──────────────────┘
                 │ (M-f6-eval)  │
                 └──────────────┘
                        │
                  updates graph
                  (new relations,
                   confidence scores,
                   gap markers)
```

### Pólya Patterns as Agent Strategies

The 25 math-informal patterns become operational agent strategies:

| Pattern | Agent strategy |
|---------|---------------|
| try-a-simpler-case | Asker decomposes question, reduces parameters |
| work-examples-first | Answerer computes concrete instances before generalising |
| argue-by-contradiction | Answerer assumes negation, derives conflict |
| reduce-to-known-result | Answerer searches graph for applicable theorems |
| unfold-the-definition | Answerer retrieves term definition from NER kernel |
| split-into-cases | Asker generates sub-questions for each case |
| check-the-extreme-cases | Critic tests answer against boundary conditions |
| construct-an-explicit-witness | Answerer builds specific example as proof |

### Graph Updates

Agents don't just consume the graph — they contribute to it:

- **New relations**: Answerer discovers connection between two entities
- **Confidence scores**: Critic rates the reliability of existing relations
- **Gap markers**: Asker flags nodes/edges that need investigation
- **Pattern annotations**: Answerer labels its own reasoning with patterns
- **Scope refinements**: Answerer clarifies variable bindings

This is how the graph grows beyond what the initial NER + pattern tagging produced.

### The Kolmogorov Framing

From the EPSRC proposal: Q⊢A. A question is a problem statement; an accepted
answer is a proof of adequacy. The Stack Exchange voting mechanism is a social
proof system. The Artificial Stack Exchange replaces social voting with
agent-based evaluation (the Critic).

In Kolmogorov's calculus of problems:
- A problem is solved by a construction
- A compound problem (A ∧ B) is solved by solving both parts
- A conditional problem (A → B) is solved by a method transforming any
  solution of A into a solution of B

The Asker generates problems. The Answerer constructs solutions.
The Critic verifies the construction.

## Implementation Sketch

### Phase 1: Single-turn Q&A with graph context

Start simple: Answerer uses knowledge graph to answer existing math.SE questions.
Measure improvement over bare LLM (reuse M-f6-eval framework).

### Phase 2: Asker generates questions from graph gaps

Asker inspects the graph, finds poorly-connected regions, generates questions.
Answerer attempts to answer. Critic scores. Graph updated.

### Phase 3: Multi-turn dialogue

Asker and Answerer engage in multi-turn exchanges. Answerer can ask clarifying
questions. Critic monitors and intervenes when reasoning goes off track.

### Phase 4: Self-improvement loop

Run the loop at scale: generate 1000 questions, answer them, evaluate, update
the graph. Measure whether the graph improves over iterations (more connections,
fewer gaps, higher evaluation scores).

## Success Criteria

- [ ] Asker generates relevant, non-trivial mathematical questions from graph gaps
- [ ] Answerer uses graph context to produce better answers than bare LLM
- [ ] Critic's evaluations correlate with human judgement (spot-check)
- [ ] The graph grows: new relations and confidence scores after each loop
- [ ] Evaluation scores improve over iterations (the system learns)
- [ ] At least 3 Pólya patterns demonstrably used as agent strategies

## Failure Modes

| Failure | What it means |
|---------|---------------|
| Asker generates trivial questions | Gap detection needs better heuristics |
| Answerer ignores graph context | Prompt engineering; may need tool-use rather than context injection |
| No improvement over iterations | Graph updates may be noise; need quality filter |
| Critic unreliable | Calibrate against human evaluations from M-f6-eval |

## Connects To

- **Depends on**: M-f6-ingest (corpus), M-f6-eval (evaluation framework)
- **Feeds**: M-f6-arxiv (agents help process and understand papers)
- **Devmap**: f6/P5 (Interactive Tutorials — agents become tutors),
  f6/P8 (Agent Protocol — this defines what agents can do)
- **Grant lineage**: LEAPQA T2 (heuristic reasoning), SFI (agent self-play),
  ATI (Stack Exchange for Computers), AI4CI (collective intelligence)

## Context

This is the heart of the decade-long vision. The ATI proposal called it
"Q&A for Computers". LEAPQA called it "epistemic agents". The SFI proposal
called it a sequel to the Artificial Stock Market. The AI4CI proposal
called it "collective intelligence for mathematics".

What makes it possible now: we have the annotated corpus (M-f6-ingest),
we have the evaluation framework (M-f6-eval), we have LLMs that can
actually do mathematical reasoning (not just pattern matching), and we
have the Pólya patterns formalised as operational strategies.

The Peeragogy insight applies here too: the agents learn from each other
through structured interaction, not from a static dataset. The knowledge
graph is the shared artifact that mediates their collaboration.
