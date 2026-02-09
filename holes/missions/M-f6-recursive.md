# Mission: F6 Recursive — Applying the Pipeline to Code

Everything from Missions 1-4 applied recursively: not to mathematical text
but to computer programming, and specifically to the futon codebase that
built the pipeline in the first place. If Missions 1-4 are tai chi —
developing the forms — Mission 5 is kung fu — applying them with force.

## Owner

Joe (+ Claude + whatever agents Missions 1-4 produce)

## Scope

### Scope In

- Adapt the F6 pipeline (NER, pattern tagging, scope detection, graph
  construction, evaluation) from mathematical text to source code
- Build a code knowledge graph from the futon repos (futon0-futon5, futon6)
- Apply the self-play loop: agents that ask questions about code, answer
  using the graph, and evaluate each other
- Run the Chapter 6 learning-event method on git history: track when developers
  (human and agent) adopt new abstractions
- Test whether graph-enhanced code reasoning outperforms bare LLM code reasoning

### Scope Out

- General-purpose code assistant (not building a competitor to Claude Code)
- Applying to arbitrary codebases without pattern libraries (needs the
  flexiarg infrastructure)
- Formal verification of code correctness (out of scope, same as for math)

## Time Box

Open-ended. Can only begin when Missions 1-4 have demonstrated the pipeline
works for mathematics. Outpost O-5 can run earlier as a feasibility check.

## Prerequisites

- [ ] M-f6-agents at least Phase 2: self-play loop operational on math
- [ ] Futon repos (0-5) in stable state with pattern coverage
- [ ] Devmap current and cross-repo dependencies mapped
- [ ] Session transcripts available as corpus (Claude Code sessions in
      ~/.claude/projects/)

## Design

### Why Code Is Both Easier and Harder Than Math

| Dimension | Mathematical text | Source code |
|-----------|------------------|-------------|
| Scope chains | Implicit (Let/Define/Assume) | Explicit (lexical scope, modules) |
| Named entities | Need NER kernel to detect | Already named (functions, classes, variables) |
| Patterns | 25 math-informal + discovery | library/* already exists (80+ patterns) |
| Evaluation | Hard (mathematical correctness) | Easier (tests pass/fail, types check) |
| Social signal | SE votes, accepted answers | Git blame, PR reviews, session transcripts |
| Informal reasoning | Hidden in prose between formulas | Hidden in comments, commit messages, PR descriptions |
| Negative space | 87-99% of tokens | Comments, TODOs, dead code, naming choices |

**Easier**: scope is explicit, entities are named, evaluation has hard signals
(tests, types). **Harder**: the informal reasoning layer in code is thinner
and more dispersed — a commit message is a pale shadow of a math.SE answer.
The PSR/PUR records and flexiarg patterns are the exception: they make the
informal reasoning explicit, which is exactly why the futon codebase is the
ideal test corpus.

### The Recursive Pipeline

**Stage 1: Parse** — Extract entities from code: functions, classes, modules,
imports, config entries. Extract from git: commits, diffs, blame, session
transcripts. The futon repos already have structured metadata (devmap,
pattern-tags.edn, queue.edn).

**Stage 2: NER** — Code NER is partially solved (AST parsing gives you
named entities for free). The interesting NER is on the *informal* layer:
detecting pattern references in commit messages, PSR/PUR records in session
transcripts, design decisions in comments. The math NER kernel becomes a
code pattern dictionary built from `library/*.flexiarg` keywords.

**Stage 3: Pattern tagging** — The 80+ flexiarg patterns in the library
become the code equivalent of the 25 math-informal patterns. Tag each
commit, PR, and session transcript with the patterns it applies. The
existing `patterns-index.tsv` hotword lists enable classical tagging.

**Stage 4: Graph construction** — Build a code knowledge graph:
- Nodes: repos, modules, functions, patterns, commits, sessions, agents
- Edges: depends-on, applies-pattern, modifies, reviews, produces
- The devmap is already a partial graph; this makes it computational

**Stage 5: Scope + negative space** — Code scope is explicit (parse the AST).
The negative space is where informal reasoning lives: comments, naming
choices, architectural decisions that aren't in the code but are in the
commit messages and session transcripts. The duality from math applies:
AST covers formal structure, everything else is informal ground.

### The Self-Play Loop for Code

**Asker**: Finds gaps in the code knowledge graph.
- Functions with no tests (missing edge: function → test)
- Patterns referenced in PSR records but never applied (gap between
  intention and implementation)
- Cross-repo dependencies that are implicit (futon3 patterns used in
  futon6 scripts but not declared in devmap)
- Commits with no PSR/PUR record (undocumented decisions)

**Answerer**: Uses graph context to answer questions about code.
- "Why was this function written this way?" → traces commit history,
  finds the PSR record, cites the pattern
- "What would break if we changed this?" → follows dependency edges
  in the graph
- "What pattern applies here?" → searches the library using context

**Critic**: Evaluates code QA pairs.
- Does the answer cite real graph entities (not hallucinated)?
- Does the pattern citation match the actual code?
- Would a developer find this answer useful?

### The Chapter 6 Method on Git History

Track developer (and agent) vocabulary expansion over commit history:
1. Build a "code term kernel" from library pattern keywords + API names
2. For each committer: extract their term vocabulary trajectory across commits
3. Identify learning events: first use of a new pattern, abstraction, or API
4. Identify treatment events: code reviews received, sessions with agents,
   pattern library updates
5. Fit the O-U model: do interactions cause learning?

This produces the code-domain equivalent of the math-domain baseline from O-0.
If agents show similar impulse-damping dynamics to human developers, the
paragogy model generalises from mathematical Q&A to software development.

### The Peeragogy Connection

The thesis developed paragogy by studying PlanetMath — people collaborating
on mathematical content. The Peeragogy Handbook extended this to general
peer learning. Mission 5 closes the loop: agents collaborating on code
are doing peer-produced peer learning, with the code knowledge graph as
the shared artifact that mediates their collaboration.

The five paragogy dimensions apply directly:
1. **Context**: The codebase evolves with each commit
2. **Meta-learning**: Agents learn what they don't know about the code
3. **Feedback**: Code review, test results, Critic evaluations
4. **Structure**: Graph grows with new connections and patterns
5. **Achievement**: Test coverage, pattern coverage, code quality metrics

## Outpost

### Outpost O-5: One Repo, One Session

Before building the full recursive pipeline, test the approach on a single
futon repo using a single Claude Code session transcript as corpus.

**Data**: One futon repo (e.g. futon6, which has the richest recent history)
+ one session transcript from ~/.claude/projects/
**Method**:
1. Extract function/module entities from the repo (AST or simple grep)
2. Run library pattern keywords against the session transcript
3. Build a minimal code knowledge graph: entities → patterns → commits
4. Give an agent a question about the code + graph context
5. Compare answer quality with and without graph context

**Validates**:
- [ ] Pattern keywords from library/*.flexiarg fire on session transcripts
- [ ] Code knowledge graph can be constructed from repo + transcript
- [ ] Graph context improves agent's answer about the codebase
- [ ] At least one gap identified (untested function, undocumented decision)

**Depends on**: library patterns (already exist), one repo in stable state
**Feeds**: Feasibility signal for full recursive pipeline

**Pattern references**: `f6/negative-space-duality`, `f6/pattern-as-strategy`,
`f6/self-play-loop`

## Implementation Sketch

### Phase 1: Code knowledge graph from futon repos

Build the graph from existing structured data:
- Devmap entries → nodes with maturity, dependencies, evidence
- library/*.flexiarg → pattern nodes with keywords, references
- Git log → commit nodes with timestamps, authors, diffs
- queue.edn → mission nodes with status, dependencies
- Session transcripts → session nodes with PSR/PUR records

### Phase 2: Pattern tagging on git history

Run the pattern-index.tsv hotword lists against commit messages and
session transcripts. Tag each commit/session with patterns applied.
Compare with actual PSR/PUR records where they exist (ground truth).

### Phase 3: Graph-enhanced code reasoning

Agent A (bare): answers questions about the futon codebase from code only.
Agent B (graph-enhanced): answers with code + graph context (patterns applied,
commit rationale, cross-repo dependencies, session history).
Evaluate using the M-f6-eval protocol.

### Phase 4: Self-play on the codebase

Asker finds code gaps. Answerer uses graph to explain or fix. Critic evaluates.
Graph updated with new connections, confidence scores, gap markers.
Measure whether code quality improves over iterations.

## Success Criteria

- [ ] Code knowledge graph constructed from futon repos with >80% entity coverage
- [ ] Pattern tagging on git history matches PSR/PUR records (where they exist)
- [ ] Graph-enhanced code reasoning outperforms bare LLM on futon-specific questions
- [ ] Asker identifies real gaps (untested code, undocumented patterns, stale devmap)
- [ ] Self-play loop produces useful code improvements (not just noise)
- [ ] Chapter 6 method detects learning events in developer git history
- [ ] The pipeline that analyses the futon codebase was itself built by the
      pipeline it analyses (bootstrap closure)

## Failure Modes

| Failure | What it means |
|---------|---------------|
| Pattern keywords don't fire on code/commits | Library patterns are too domain-specific; need code-adapted keywords |
| Graph too sparse to help | Need richer metadata; PSR/PUR records are key and may be too rare |
| Agent ignores graph for code questions | Code context may be sufficient; graph adds value only for architectural questions |
| No learning events detected in git history | Developers may already know their tools; learning signal is in new contributors |
| Bootstrap closure feels circular | It is circular — that's the point. Validate externally against non-futon codebases |

## Connects To

- **Depends on**: M-f6-agents (need working self-play loop), M-f6-eval
  (need evaluation protocol), all of futon0-futon5 (need stable substrate)
- **Feeds**: The futon ecosystem itself — agents that understand the codebase
  can maintain and extend it
- **Devmap**: f6/P8 (Agent Protocol — this defines what agents can do to code),
  f6/P5 (Interactive Tutorials — agents that explain code)
- **Grant lineage**: The full arc. ATI (2015) wanted "Q&A for Computers" —
  this is literally Q&A for computers about computer programs. LEAPQA
  (2017) proposed agents that reason about structured knowledge — this
  is agents reasoning about structured code. The thesis (2014) described
  peer-produced peer learning — this is agents learning from each other's
  commits.
- **Peeragogy**: The Handbook's patterns (Heartbeat, Roadmap, Reduce/Reuse/Recycle)
  apply directly to agent collaboration on code

## Context

This mission is why the futon numbering goes to 6 but the work starts at 0.
The whole stack — durable store (futon1a), embeddings (futon3a), patterns
(futon3), category theory (futon5), the dictionary itself (futon6) — exists
to be reasoned about as much as to reason with. When agents can understand
the codebase that built them, the loop closes. The thesis's "computational
model of mathematical collaboration" becomes a computational model of
computational collaboration.

"Even this, eventually, a computer will be able to solve."
