# Verification: F6 Against Corneli (2014) PhD Thesis

Verify the futon6 system against the theoretical claims and computational
vision of *Peer Produced Peer Learning in Mathematics* (Open University, 2014).
The thesis predated LLMs but laid the theoretical groundwork that F6 now
instantiates. This document maps thesis claims to system properties.

## The Central Prediction (§10.6, p.166)

The thesis's Future Work section describes exactly what F6 is building:

> In order to build a computational study following from this work, one
> would need: **a reasonably large computer-accessible body of mathematical
> content** [...] along with **computational agents** that are able to navigate
> the relevant mathematical structures [...] able to apply "standard" and
> "social" problem solving heuristics [...] sufficiently **metacognitively
> aware** as to be able to set and solve problems by generating and applying
> new heuristics in the form of **design patterns** [...] and ideally able to
> **annotate, reflect on, diagnose, and extend** the overall process.

This decomposes into verifiable claims:

| Thesis claim | F6 component | Status |
|-------------|-------------|--------|
| "reasonably large computer-accessible body of mathematical content" | M-f6-ingest: math.SE corpus (200K+ QA pairs) + NER kernel (19,236 terms) | **Partially realized** — physics.SE processed (114K pairs), math.SE pending superpod run |
| "computational agents that can navigate relevant mathematical structures" | M-f6-agents: Asker/Answerer/Critic with graph queries | **Designed**, not yet implemented |
| "apply standard and social problem solving heuristics" | 25 math-informal patterns as operational agent strategies (Table in M-f6-agents) | **Patterns created**, agent integration pending |
| "metacognitively aware" — can set and solve problems | Asker generates questions from graph gaps; self-play loop | **Designed**, not yet implemented |
| "in the form of design patterns" | Pólya patterns as flexiarg entries in futon3 library | **Realized** — 25 patterns, all fire on SE data |
| "annotate, reflect on, diagnose, and extend" | Critic agent + graph updates (new relations, confidence scores, gap markers) | **Designed**, not yet implemented |
| "a society-of-mind style architecture (Minsky, 1988)" | Asker/Answerer/Critic = three specialized agents in a self-play loop | **Designed** — direct realization of Minsky's critics |

## The Five Dimensions of Change (§3.3)

The thesis defines paragogy through five dimensions of change. These map
onto the F6 agent architecture:

| Dimension | Paragogy principle | F6 realization |
|-----------|-------------------|----------------|
| **Context** (Δ context) | "Changing the nature of the space" | Knowledge graph grows with each loop iteration; new relations, gap markers |
| **Meta-learning** (Δ metalearning) | "Changing what I know about myself" | Asker identifies gaps — the system learns *what it doesn't know* |
| **Feedback** (Δ feedback) | "Changing my perspective" | Critic evaluates QA pairs; scores feed back to Asker and Answerer |
| **Structure** (Δ distributivity) | "Changing content or connectivity" | Graph updates: new inter-cluster links, scope refinements, pattern annotations |
| **Heuristic** (Δ achievement) | "Changing objectives" | Improving evaluation scores over iterations; agent strategies evolve |

**Verification question**: Does the self-play loop (M-f6-agents) produce
measurable change along all five dimensions? This is testable once the
loop runs.

## Appendix B: Proof as Social Process

The thesis codes two mathematical proofs using a 5-category scheme matching
the paragogy dimensions:

| Code | Thesis meaning | F6 pattern equivalent |
|------|---------------|----------------------|
| (1) Context | Expand definitions, scope the problem, find relevant concepts | `unfold-the-definition`, `reduce-to-known-result` |
| (2) Feedback | Generate examples, introduce variables, try special cases | `work-examples-first`, `try-a-simpler-case` |
| (3) Quality | Form conjectures, build models, outline plans | `argue-by-contradiction`, `construct-an-explicit-witness` |
| (4) Structure | Change descriptions, apply definitions, deduce | `split-into-cases`, `reduce-to-known-result` |
| (5) Heuristic | Restrict to known, certify results, create roadmaps | `check-the-extreme-cases`, `give-a-name-to-it` |

**Verification question**: Do the 25 math-informal patterns cover all five
categories from the thesis coding? The pattern library should be auditable
against this scheme.

The thesis also states (Appendix B, p.197):

> (2) "Polling for ideas" can help generate a set of examples. [...] Within
> a proof, **variables can be introduced as examples**, which can have either
> "global" or "local" significance.

This is exactly the scope model designed for F6 — GlobalEntity vs LocalBinding,
with scope detection patterns for Let/Define/Assume openers.

## The Entity-Relation Model (Table 24, p.159)

The thesis defines PlanetMath 3.0's entity types and relations:

| Thesis entity | F6 equivalent | Notes |
|--------------|--------------|-------|
| A (article) | Entity in knowledge graph | Articles become QA pairs in SE context |
| X (object) | NER kernel term | Mathematical objects, definitions |
| T (post) | SE answer/question | Direct mapping |
| S (solution) | SE accepted answer | Direct mapping |
| R (review) | Critic evaluation | Agent-generated, not human |
| Q (question) | Asker-generated question | Or SE question |
| C (correction) | Graph confidence update | Agent-generated corrections to graph |
| P (problem) | Asker-generated problem from graph gaps | |
| H (heuristic) | Math-informal pattern | 25 patterns = operational heuristics |
| L (collection) | Topic cluster | HDBSCAN clusters from embeddings |
| M (classification) | MSC code | Via PlanetMath MSC taxonomy |
| G (group) | Agent role | Asker/Answerer/Critic |
| J (conjecture) | Gap marker | Asker flags nodes needing investigation |
| X♯ (update) | Graph update | New relations, confidence scores |
| X′ (fork) | Alternative answer | Answerer can produce multiple answers |
| X⋆ (outcome) | Evaluation score | Critic's M-f6-eval scores |

Relations from the thesis (reading the arrows):

| Thesis relation | F6 realization |
|----------------|---------------|
| A ← A (article links to article) | Entity-to-entity relations in knowledge graph |
| X ↪ X (object referenced by object) | NER term co-occurrence; cross-references |
| X ← T (object from post) | Term extraction from SE answers |
| S ← R (solution gets review) | Critic evaluates Answerer's output |
| Q ← A (question from article) | Asker generates questions from graph entities |
| Q,T ⇀ C,W,P (questions/posts generate corrections, requests, problems) | Self-play loop: QA generates new problems and graph corrections |

**Verification question**: Does F6's knowledge graph support all the entity
types and relations from Table 24? Some (like U/user, E/ephemera) are
social-layer entities that the agent system doesn't need, but the
mathematical entities should all have correspondents.

## NNexus Succession (§8.3-8.4)

The thesis discusses NNexus (Deyan Ginev's auto-linker) extensively:

> In the future, we could do much better, by generating a "course packet"
> on the fly to assist the problem solver. The latest version of NNexus
> embeds RDFa into links, which makes the connections between pages
> explicit; the relevant information for building this sort of on-the-fly
> course packet is now available via a SPARQL query. (§8.4, p.120)

> It will be important to develop better strategies for NNexus to use to
> steer links accurately, since there are often different definitions for
> the same term: some of these strategies include steering based on the
> Mathematics Subject Classification, entropy-based approaches, and adding
> mechanisms for users to give feedback on mis-directed links. (§8.4, p.121)

F6's NER kernel + classical term spotter is a direct successor to NNexus:
- 19,236 terms vs NNexus's PlanetMath-only dictionary
- Scope detection adds variable-binding awareness NNexus lacked
- Disambiguation via MSC codes (thesis suggestion) is available via PlanetMath MSC metadata
- Graph context steering (thesis suggestion) is exactly what Agent B gets in M-f6-eval

**Verification question**: Does F6's term spotter match or exceed NNexus's
coverage? We have 100% SE entity coverage at 22 terms/entity mean — this
likely exceeds NNexus, but a direct comparison would require running NNexus
on the same corpus.

## The "Simulated Learners" Gap (§10.5)

The thesis explicitly marks this as out-of-scope:

> Simulation work [with] artificial agents has remained entirely out of
> scope. Many of the ideas discussed in the thesis may be germane to such
> studies, but this claim has been in no way tested. This means that even
> though the thesis points toward a computational model of mathematical
> collaboration, the usefulness of this model remains conjectural. (p.164)

And:

> Work with simulated learners would be one route to testing and improving
> the model. (p.165)

M-f6-agents IS the simulated-learner system the thesis couldn't build.
The Asker/Answerer/Critic self-play loop is artificial agents doing exactly
what the thesis described as future work: navigating mathematical structures,
applying heuristics, learning from each other.

**This is the strongest verification target**: if M-f6-agents works, it
retroactively validates the thesis's computational model as non-conjectural.

## Problem Posing (§10.6)

> The notion of problem posing gets at the essence of pattern-making: we
> will need patterns about patterns in order to obtain the kind of closure
> alluded to in the introductory comments on the pattern analysis. (p.166)

F6's Asker agent does problem posing — generating questions from graph gaps.
The Pólya patterns (especially `try-a-simpler-case` and `split-into-cases`)
are operational problem-posing strategies.

The "patterns about patterns" meta-level is partially addressed by the
flexiarg format itself, which has a uniform structure across all 25 patterns.
But true meta-patterns (patterns for selecting which pattern to apply) are
not yet formalized. This is a gap.

## Channels Between the Strata (§10.3)

> PlanetMath may find new purpose in fulfilling the request voiced by
> Thurston (1990) for "the creation of channels for communication between
> the strata." (p.161)

F6 creates these channels:
- math.SE = the "peer learning" stratum (people helping each other understand)
- Arxiv = the "peer production" stratum (people creating new knowledge)
- The knowledge graph bridges both: shared NER terms, shared patterns,
  shared topic clusters connecting questions people ask with theorems people prove

**Verification question**: After M-f6-arxiv, can we identify "bridge concepts"
that appear in both SE questions and Arxiv proofs? The number and quality of
such bridges would measure how well the system creates inter-stratum channels.

## The Geertz Framing: Culture as Program (§1.4)

> Culture is best seen [...] as a set of control mechanisms — plans, recipes,
> rules, instructions (what computer engineers call "programs") — for the
> governing of behavior. (Geertz, 1973, p. 44, quoted p.9)

The thesis argues that mathematics-as-culture is already programming.
F6 makes this literal: mathematical reasoning patterns become executable
agent strategies. The knowledge graph is the "cultural program" governing
agent behavior.

**This is a philosophical verification, not a technical one.** But it's
worth noting that the thesis's framing predicted exactly this move — from
cultural patterns to computational patterns.

## Prediction Errors (What the Thesis Got Wrong)

1. **"Society-of-mind" expected Minsky-style symbolic AI.**
   The thesis (p.166) cites Singh & Minsky (2005). F6 uses LLMs, not
   symbolic reasoning. The agent architecture is society-of-mind in
   structure but neural in implementation. The thesis couldn't have
   predicted this — LLMs didn't exist in 2014.

2. **"Design patterns" expected human-authored pattern catalogs.**
   The thesis expected patterns would be authored by humans (PlanetMath
   users writing patterns, §10.5 p.164). F6 instead has patterns authored
   once (as flexiarg entries) and applied by LLMs. The human-in-the-loop
   for pattern authoring is Joe, not a community.

3. **The thesis expected PlanetMath to be the platform.**
   PlanetMath's community didn't grow as hoped (§10.5, p.164: "Uptake of
   the new tools on PlanetMath has not been sufficient"). Stack Exchange
   became the actual commons for mathematical Q&A. F6 correctly pivoted
   to SE as the data source.

4. **Drupal/Planetary as infrastructure.**
   The thesis invested heavily in Drupal-based infrastructure (Chapter 8).
   F6 replaces this with a knowledge graph + LLM agents. The web platform
   is no longer the locus of intelligence — the graph is.

5. **No prediction of the "negative space" insight.**
   The thesis treats NER and informal reasoning as separate concerns.
   F6's key insight — that what NER *doesn't* cover is where informal
   patterns live — is a novel contribution beyond the thesis.

## Chapter 6 Method at SE Scale (The Classical Baseline)

The thesis's Chapter 6 already demonstrates the F6 methodology in miniature,
applied by hand to PlanetMath edit histories:

> The last ten years of versioned encyclopedia articles were loaded into Git,
> which made it relatively easy to extract line-by-line differences between
> versions, using git log and git show. These change sets were analyzed using
> a term spotting approach, with technical terms drawn from the thesaurus-like
> metadata from PlanetMath's encyclopedia articles. [...] The functionality is
> similar to that of PlanetMath's autolinking tool, NNexus. (§6.3)

| Chapter 6 step (2014) | F6 equivalent (2026) |
|----------------------|---------------------|
| PlanetMath articles loaded into Git | SE/Arxiv into knowledge graph |
| Line-by-line diffs | Edit histories in SE data dump |
| Term spotting with PM metadata | NER kernel (19,236 terms) via spot-terms.bb |
| Regex-based search like NNexus | Classical term spotter = better NNexus |
| New terms per user = learning events | Vocabulary trajectory per SE user |
| Forum posts/corrections = treatments | Answers/comments/votes = treatments |
| Ornstein-Uhlenbeck impulse-damping | Same model, 1000x more data |

The thesis fitted this model to 445 PlanetMath users over 9 years (8,051
forum posts, 14,064 corrections, 3,867 learning-event edits). **The same
method can now run on all of math.SE using classical (no-GPU) techniques
and the 19,236-term NER kernel.**

This creates two verification opportunities:

1. **Scale validation**: Do the same impulse-damping dynamics that appeared
   in PlanetMath also appear in Stack Exchange? If yes, the thesis's model
   of peer-produced peer learning generalises beyond PlanetMath.

2. **Agent baseline**: The fitted human-learning parameters become a benchmark
   for M-f6-agents. Do artificial agents (Asker/Answerer/Critic) produce
   learning dynamics comparable to human SE interactions? If the self-play
   loop exhibits similar impulse-damping curves, it validates the thesis's
   claim (§10.5) that "simulation work with artificial agents" would test
   the computational model.

The method is purely classical — term spotting + statistical fitting — and
could run as a standalone analysis before M-f6-agents is built. It would
be a research result in its own right: a computational study of learning
dynamics across 200K+ mathematical Q&A interactions.

See pattern: `f6/learning-event-detection`

## Summary Verification Checklist

| # | Thesis claim | F6 status | Evidence needed |
|---|-------------|-----------|----------------|
| 1 | Large computer-accessible math corpus | Partial | Math.SE superpod run (M-f6-ingest) |
| 2 | Agents navigate mathematical structures | Designed | M-f6-agents Phase 1 implementation |
| 3 | Standard + social heuristics as patterns | Realized | 25 patterns fire on SE data; pattern tags in corpus |
| 4 | Metacognitive awareness (problem posing) | Designed | Asker generates non-trivial questions from graph gaps |
| 5 | Annotate, reflect, diagnose, extend | Designed | Critic + graph updates working |
| 6 | Society-of-mind architecture | Designed | Asker/Answerer/Critic self-play loop functional |
| 7 | NNexus-successor term linking | Realized | 19,236-term kernel at 100% SE coverage |
| 8 | Simulated learners | Designed | M-f6-agents produces improving scores over iterations |
| 9 | Five dimensions of change | Testable | Self-play loop changes along all 5 paragogy dimensions |
| 10 | Entity-relation model (Table 24) | Partial | Graph schema covers thesis entity types |
| 11 | Inter-stratum channels | Designed | Bridge concepts between SE and Arxiv (M-f6-arxiv) |
| 12 | Proof as social-computational process | Partial | Pattern tagger codes proofs per Appendix B scheme |
| 13 | Chapter 6 learning-event detection | Ready | Run NER kernel on SE edit histories, fit O-U model |
| 14 | O-U dynamics generalise beyond PlanetMath | Testable | Compare fitted parameters: PM (445 users) vs SE (200K+ QA) |
| 15 | Agent learning dynamics match human dynamics | Testable | Compare self-play loop O-U parameters with SE human baseline |

**Items 3 and 7 are already verified.** Items 1, 10, 12 are partially verified.
**Item 13 is ready to run** — requires only the math.SE data dump and the
existing NER kernel; no GPU, no LLM, purely classical. This is the cheapest
high-value verification: it produces a standalone research result (learning
dynamics at SE scale) while creating the human baseline for agent comparison.
Items 2, 4, 5, 6, 8, 9, 11, 14, 15 require implementation of M-f6-agents
and M-f6-arxiv.

## What the Thesis Adds to F6

The verification isn't one-directional. The thesis offers F6 things it
doesn't yet have:

1. **The paragogy framework as evaluation criteria.** The five dimensions
   of change give us a richer way to evaluate the self-play loop than
   raw accuracy scores.

2. **The Appendix B coding scheme.** A worked methodology for analyzing
   mathematical proofs as social-computational processes. This could be
   used to validate the pattern tagger's output.

3. **The entity-relation model.** Table 24 is a more complete ontology than
   what F6 currently uses. Gap markers, conjectures, and forks are not yet
   in the graph schema.

4. **The Geertz/Ingold theoretical grounding.** F6 can be described as
   "making mathematical culture computable" — the thesis provides the
   anthropological warrant for this claim.

5. **The McLuhan tetrad.** Each system feature analyzed through
   Enhance/Obsolesce/Retrieve/Reverse. This is a useful lens for evaluating
   F6's impact that we haven't applied yet.
