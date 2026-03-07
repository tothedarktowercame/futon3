# Draft Standard: Code Provenance via Enrichment Graph

**Status:** Draft
**Date:** 2026-03-06
**Context:** M-futon-enrichment Phase 2, futon1a docstring precedent

## Problem

When you open a source file, you should be able to answer: which pattern
governs this code? Which mission created it? What invariant does it uphold?
What evidence exists for these claims?

Futon1a solved this partially by embedding structured fields in namespace
docstrings:

```clojure
(ns futon1a.core.pipeline
  "Cross-layer write pipeline.
   Invariant: I3 (Hierarchy)
   Pattern:   storage/error-layer-hierarchy
   Theory:    futon-theory/error-hierarchy")
```

This works but has limits:

- **Not queryable** without parsing source files
- **Drifts** — docstrings aren't checked against the graph
- **Language-specific** — Clojure has docstrings; not everything does
- **One-directional** — you can read the docstring but can't ask "which
  namespaces use this pattern?"

## Standard

### 1. The graph is the system of record

Provenance lives in hyperedges in the futon1a store. A namespace's pattern
provenance is a hyperedge, not a comment:

```edn
{:hx/type :provenance/pattern-applied
 :hx/endpoints ["ns:futon1a.core.pipeline"
                "pattern:storage/error-layer-hierarchy"]
 :hx/props {:source :psr
            :psr-id "PSR-2026-03-06-001"
            :confidence :high}}
```

Similarly for invariants and theory:

```edn
{:hx/type :provenance/invariant-upheld
 :hx/endpoints ["ns:futon1a.core.pipeline"
                "invariant:I3"]
 :hx/props {:source :psr}}

{:hx/type :provenance/theory-grounded
 :hx/endpoints ["ns:futon1a.core.pipeline"
                "theory:futon-theory/error-hierarchy"]
 :hx/props {:source :psr}}
```

### 2. PSR is the write path

The existing PSR discipline already records pattern selections. The
extension: PSRs carry a `:scope` field identifying what code the pattern
was applied to.

```
## PSR (Pattern Selection Record)
- **Cycle**: 1
- **Pattern chosen**: `storage/error-layer-hierarchy`
- **Scope**: `futon1a.core.pipeline`
- **Scope granularity**: namespace
- **Candidates considered**: error-layer-hierarchy, guardrails-vs-tooling
- **Rationale**: Pipeline must surface errors at the layer that caused them
- **Confidence**: high
```

Scope granularity levels:

| Granularity | Example | When to use |
|-------------|---------|-------------|
| `repo` | `futon1a` | Architectural pattern (whole repo) |
| `namespace` | `futon1a.core.pipeline` | Module-level pattern |
| `var` | `futon1a.core.pipeline/execute!` | Function-level pattern |
| `file` | `src/futon1a/core/pipeline.clj` | Non-Clojure or multi-ns files |

Most PSRs will be namespace-scoped. Var-level scope is for cases where
different functions in the same namespace follow different patterns.

### 3. The enrichment pipeline harvests PSRs into the graph

The enrichment pipeline (already running as L0-L5) gains a new capability:
when it encounters a PSR with a `:scope` field, it writes a
`provenance/pattern-applied` hyperedge connecting the pattern to the scoped
code entity. This is automatic — no manual graph writing needed.

The pipeline also harvests PUR outcomes, so the graph records not just "this
pattern was selected" but "this pattern was applied and the outcome was X":

```edn
{:hx/type :provenance/pattern-outcome
 :hx/endpoints ["ns:futon1a.core.pipeline"
                "pattern:storage/error-layer-hierarchy"]
 :hx/props {:source :pur
            :outcome :success
            :prediction-error :low}}
```

### 4. Docstrings are optional human courtesy

Source-level annotations like futon1a's `Pattern:` fields remain welcome
as a human convenience. They are not the canonical source. The graph is.

If a project chooses to include them, the enrichment pipeline can validate
consistency: does the docstring's `Pattern:` match what the graph says?
Divergence is a tension signal (a new tension type:
`tension/docstring-graph-drift`).

### 5. Backward compatibility with futon1a

The futon1a docstrings represent existing provenance data that predates
this standard. A one-time migration harvests them into the graph:

1. Parse `Pattern:`, `Invariant:`, `Theory:` from namespace docstrings
2. Write corresponding `provenance/*` hyperedges with `:source :docstring`
3. From that point forward, the graph is authoritative

The docstrings can stay as-is (human courtesy) or be removed (the graph
has the data). No docstring editing is required going forward.

## What this enables

With provenance in the graph, the enrichment panel shows per-namespace
and per-var pattern provenance alongside missions, tensions, and churn:

```
── File Enrichment ──
Namespace: futon1a.core.pipeline
Layer: 6  |  Vars: 3

Missions
  • M-futon1a-rebuild

Patterns
  • storage/error-layer-hierarchy (via PSR, high confidence)

Invariants
  • I3: Hierarchy — errors surface at the layer that caused them

Churn / Complexity
  commits: 12
  max indent: 22
  lines: 180
```

Reverse queries also become possible:
- "Which namespaces use pattern X?" — query by pattern endpoint
- "Which invariants have no code backing them?" — tension detection
- "Which patterns have poor PUR outcomes?" — pattern library hygiene

## Design decisions

1. **PUR carries scope; PSR scope is optional.** PUR records concrete
   outcomes — the code changed, the functions affected. Scope belongs
   there naturally. PSR is speculative ("I'm considering this pattern for
   this area") so scope is useful when known but not required. A PUR
   without scope is a linting warning; a PSR without scope is normal
   during exploration.

2. **No post-hoc reconstruction of scope.** Inferring scope from edit
   context after the fact is fragile and produces low-confidence edges.
   The standard captures provenance going forward, at the point of work.
   Retroactive enrichment (like harvesting futon1a docstrings) is a
   one-time migration, not an ongoing mechanism.

3. **Link to pattern name, not definition location.** The endpoint
   `pattern:storage/error-layer-hierarchy` references the pattern by
   name. The enrichment pipeline does not resolve where the pattern is
   defined — that's the pattern catalog's job. If the catalog is stale,
   the link still works as a name; resolution catches up later. This
   avoids coupling provenance edges to the pattern library's file layout.

4. **Var-level granularity: worth it, but only if done well.** Namespace
   scope covers 90% of cases. Var-level scope (which functions were
   affected, not just which file) would be genuinely useful for
   heterogeneous namespaces like `transport/http.clj`. But doing it
   poorly (noisy git blame, wrong function boundaries) is worse than
   not doing it. The right approach is to pick a greenfield project
   where treesitter-based function boundary detection can be built in
   from the start, producing var-level scope as a natural byproduct of
   the development workflow. This parallels the mathematics case, where
   scope annotations on Stack Exchange questions (per the superpod run)
   are being developed with similar granularity concerns — the question
   is not whether fine-grained scope is useful, but whether the tooling
   produces it reliably enough to trust.

## Prototype plan

**Primary target: futon6.** The mathematics dictionary is on the critical
path (documenting informal reasoning is the core futon6 mission), actively
in progress, and tests pattern discipline in a non-code domain. If provenance
scope works for mathematical arguments — "which pattern guided this proof
strategy, applied to which theorem" — that's strong evidence the standard
generalises beyond software.

The futon6 codebase is split: mining/extraction is Python (superpod runs),
proof construction is Clojure. The provenance standard applies to the
Clojure side, where patterns govern how proofs are structured and
formalised. The Python mining side produces raw material; the Clojure
proving side is where pattern discipline and scope annotation matter.

This work naturally extends the futon3x book series, which already
demonstrates self-representation across the stack. Enrichment provenance
for futon6 would become a new chapter showing how mathematical reasoning
carries the same evidence trail as code development.

**Secondary sites for evidence gathering:**

- **futon7** (greenfield) — when futon7 expands beyond its first hour of
  work, pattern discipline can be baked in from the start. A mission
  there would test the standard on the most ambitious layer.
- **futon5** (strategic rebuild) — futon5's ad hoc architecture is calling
  for the same kind of rebuild that produced futon1a and futon3a/3b/3c.
  A rebuild mission would test provenance on medium-complexity reorganisation.
- **Peripherals across the stack** — normal practice going forward. Every
  new or modified peripheral carries PUR scope. This accumulates evidence
  gradually without requiring dedicated tooling.

These are not alternatives to futon6 — they are excursion sites for
gathering further evidence that the standard works across contexts.

## Open questions

1. **Mathematical scope granularity.** Code scope has clear levels
   (repo / namespace / var). Mathematical scope is less obvious: theorem?
   proof step? lemma application? The futon6 prototype needs to discover
   what granularity is natural for mathematical provenance, likely informed
   by the superpod scope annotations on Stack Exchange questions.

2. **PUR scope format.** Should PUR scope be free-text ("I changed
   `execute!` and `validate-gate`") or structured (a list of qualified
   names)? Structured is better for the pipeline but higher friction
   to write. The mission peripheral could assist by pre-populating scope
   from the git diff (code) or proof context (mathematics). Resolution
   likely emerges from the futon6 prototype — whatever format is natural
   for recording "which theorem did this pattern apply to" will inform
   the code-side format too.

## References

- futon1a namespace docstrings (precedent): `grep -r "Pattern:" futon1a/src/`
- M-futon-enrichment mission doc: `futon4/holes/missions/M-futon-enrichment.md`
- Enrichment layers L0-L5: ibid, §Layer Evidence
- PSR/PUR format: `futon3/CLAUDE.md` §Exploratory Pattern Mode
- futon0 CLAUDE.md §2 Traceability Chain (the chain this standard completes)
