# Sokoban — Pattern Peripheral (Level-1)

Short, sustainable handoff so the peripheral isn't forgotten between sessions.

## What

A pre-write capability envelope for authoring `.flexiarg` patterns. An agent
submits a draft via a single bound action; either the draft lands as a
canonical pattern, or the surface refuses with structured violations and
**writes nothing**. No leakage — non-canonical drafts cannot be emitted.

Sokoban metaphor: inside the peripheral, the only legal moves canonicalise
the draft. Anything else gets refused.

## Where the code lives

- **Engine**: `futon3a/src/futon/peripheral/pattern_author.clj`
  Single bound action: `submit-draft!`.
- **Tests**: `futon3a/test/futon/peripheral/pattern_author_test.clj`
  6 deftests covering admission + refusal shapes; run with
  `cd /home/joe/code/futon3a && clojure -X:test`.
- **Underlying parser**: `futon3a/src/futon/flexiarg/projection.clj`
  (the canonical parser P-1; the Sokoban is a thin validator over it).

## How to use

```clojure
(require '[futon.peripheral.pattern-author :as pa])

(pa/submit-draft!
 {:author "claude-N"
  :target-path "/home/joe/code/futon3/library/<ns>/<name>.flexiarg"
  :draft-body  "@flexiarg <ns>/<name>\n@title ...\n! conclusion: ...\n  + context: ...\n  ..."})
```

Result on success:

```clojure
{:landed? true
 :path "..."
 :pattern-id "<ns>/<name>"
 :violations []}
```

Result on refusal — file is **not written**:

```clojure
{:landed? false
 :path "..."
 :violations [{:kind :missing-conclusion
               :detail "Required: ! conclusion: ... (or ! claim: / ! summary: / ! instantiated-by:)"
               :where #:pattern{:id "..."}}]
 :note "Sokoban refusal: re-draft and resubmit."}
```

## What gets admitted

A draft must:

1. **Parse cleanly** through the canonical parser — i.e. start with
   `@flexiarg <ns>/<name>` (or `@arg`/`@multiarg` for multi-block files).
2. **Have a conclusion** — any of `! conclusion:`, `! claim:`, `! summary:`,
   or `! instantiated-by:` (all four normalise to the same slot).
3. **Use only admitted clause names** at any level:
   - **Canonical seven**: `context`, `if`, `however`, `then`, `because`,
     `next-steps`, `conclusion`.
   - **Rulebook substructure** (parsed flat, but admitted because the
     rulebook recognises them under canonical parents):
     - under `then`: `compositions`, `check`, `enforcement`, `lean`
     - under `however`: `failure-modes`, `anti-patterns`, `absence-signals`,
       `signals`
     - under `because`: `evidence`, `evidence-base`, `because->evidence`,
       `mechanism`, `counterfactual`
     - under `if`: `does-not-apply`
     - under `next-steps`: `use`

Anything else at top level gets refused with a structured violation.

## Refusal kinds

- `:missing-flexiarg-header` — no `@flexiarg`/`@arg`/`@multiarg` line.
- `:missing-conclusion` — no `! conclusion:` (or alias).
- `:non-canonical-clause` — clause name outside the admitted set.
- `:empty-draft` — body produced no parseable blocks.

## Reference docs

- **Reshape rulebook**: `futon3/holes/excursions/E-clause-vocabulary-reshape.sexp`
  (the canonical/aliased/substructure decisions; verb taxonomy of
  replace/substructure/metadata/facet/reject; long-tail heuristic).
- **Clause-vocabulary survey**: `futon3/holes/excursions/E-clause-vocabulary-survey.md`
  (frequency-tallied across 919 patterns; the empirical ground for the
  canonical-seven decision).
- **Mission**: `futon0/holes/missions/M-patterns-done-right.md`
  (four levels: this Sokoban is Level 1; Levels 2/3/4 are named long-arc
  work — algorithm-guided authoring, functorial admission, pattern→code
  receipts).
- **Algorithm (agent prep)**:
  `~/code/algorithms/author-flexiarg-core-and-wire-discoverability.md`
  (the recommended preparation step before invoking `submit-draft!`).
- **Parent excursion**: `futon3/holes/excursions/E-pattern-peripheral.md`
  (originally drafted as a Codex handoff; reframed under M-patterns-done-right
  as the Level-1 floor).

## Status

**Level-1 Sokoban — working** (committed as
`futon3a/e3f68e8 Pattern peripheral Level-1 Sokoban mockup`,
canonical-parser dep `futon3a/5664147 Canonical flexiarg parser`).

Future graduation hooks named in the namespace docstring:

- **Level 2** — wire `algorithms/author-flexiarg-core-and-wire-discoverability.md`
  as a session-mode preparation step.
- **Level 3** — functorial admission via `futon5/src/futon5/ct/dsl.clj`'s
  design-pattern-category; failure feedback names which morphism doesn't
  compose with which sibling pattern.
- **Level 4** — commit-time `Pattern: <ns>/<name>` footers + verify
  invariant `invariant-pattern-receipts-resolve` + geometric witness check.

## Cold-start evidence

A separate Claude session — with no context about this peripheral — recently
authored `library/writing-coherence/meet-the-reader-where-they-are.flexiarg`,
and validating it against the Sokoban returns `:status :ok` with full
canonical structure. The substrate's ambient signal (985 already-canonical
patterns in the corpus + futon3 `CLAUDE.md` + the reshape rulebook on disk)
is enough for unprimed agents to author Sokoban-admissible patterns.

The peripheral's job is therefore to **catch the failure cases** the corpus
alone doesn't prevent — not to scaffold success from scratch.

## Worked example: insights from M-patterns-done-right

Four flexiargs in `library/pattern-discipline/` were authored *through* the
Sokoban as the mission was drafted, capturing the level-by-level insights:

- `peripheral-as-sokoban.flexiarg` — the Level-1 framing itself.
- `conclusion-required-others-recommended.flexiarg` — conclusion-only-required.
- `patterns-as-categorical-objects.flexiarg` — Level-3 framing.
- `pattern-to-code-receipts.flexiarg` — Level-4 framing.

The first submission was **refused** (caught a parser-flatness tension where
`+ CHECK:` was treated as top-level rather than substructure under `+ THEN:`)
and led to extending the admitted-set to include rulebook substructure
names. Resubmission succeeded. That's the Sokoban genuinely doing its job —
catching drift, surfacing it as actionable feedback, and letting the agent
re-draft.
