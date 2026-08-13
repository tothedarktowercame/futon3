# The flexiarg format — specification

**Status: normative. Drafted 2026-08-13 by claude-2 at Joe's instruction, as a
translation of the de facto standard implemented in `contrib/flexiarg.el`
(v0.1, Jan 2026). Where this document and that implementation disagree, the
elisp is authoritative and this document is a bug.**

A flexiarg is a flexiformal argument: structured enough to be machine-checkable,
loose enough to be written by hand. One file, one pattern.

---

## 1. File shape

```
@flexiarg musn/plan-before-tool
@title Plan Before Tool Use
@style pattern

! conclusion: Require a short plan line before any tool or wide scan.
  + context: You are running an agent with live tool calls.
  + IF: The agent reaches for tools before stating its plan.
  + HOWEVER: Tool-first actions hide intent and make audits harder.
  + THEN: Require a brief plan line before tool use and pause if missing.
  + BECAUSE: Plans create anchors for PSR/PUR and keep scope explicit.
    + evidence: lab sessions show plan-before-tool warnings when missing.
  + NEXT-STEPS:
    - Add a check that pauses when the plan line is missing.
```

Three kinds of line: **meta** (`@name value`), **nodes** (`!`, `+`, `?`), and
**continuations**. Blank lines and lines beginning `;` are ignored.

## 2. Nodes, and the one rule people get wrong

A node line matches:

```
^(\s*)([!+?])\s*([^:]+):\s*(.*)$
   │      │        │          └── text
   │      │        └── relation (e.g. "because", "example(optional)")
   │      └── marker
   └── INDENT — SIGNIFICANT. This is the tree.
```

**Indentation is structure, not formatting.** A node indented more deeply than
the node above it is that node's **child**. Equal or lesser indent makes it a
sibling or an uncle. `contrib/flexiarg.el` implements this with a stack of
`(indent . node)` pairs (`flexiarg--parse-buffer`); strictly-greater indent
descends, anything else unwinds.

This is the rule that a parser discarding leading whitespace silently violates,
and the failure is invisible: every sub-component is promoted to a top-level
component and the file still "parses". See §7.

### Markers

| Marker | Role | Meaning |
|---|---|---|
| `!` | `:conclusion` | The pattern's claim. Always required. |
| `+` | `:premise` | A component or sub-component; its label becomes the relation keyword. |
| `?` | `:hole` | An unfilled slot, relation `:illustrates`. `?example(optional):` is not required; `?evidence:` and `?evidence(required):` are. |

The `(optional)` / `(required)` suffix is parsed, not decoration
(`flexiarg--compute-role-and-rel`).

### Continuation lines

A non-empty line that is neither meta nor a node appends to the current node's
text. This is how multi-line components are written; no marker is repeated.

## 3. The required structure

**`! conclusion:` plus five components, all required:**

| Component | Meaning |
|---|---|
| `+ context:` | the sphere of endeavor — where this applies |
| `+ IF:` | the condition that brings the tension into play |
| `+ HOWEVER:` | the tension itself; what goes wrong, honestly stated |
| `+ THEN:` | the actions that improve or manage the tension |
| `+ BECAUSE:` | the rationale |

Labels are **case-insensitive** (`+ IF:` and `+ if:` are the same relation).
`! summary:` is an accepted alias for `! conclusion:`.

`+ NEXT-STEPS:` is a recognised top-level component but is **not** one of the
required five — steps that would strengthen the pattern, written as bullets or
`next[...]` lines.

**A pattern missing one of the five is incomplete.** An importer meeting one
should report it as a finding, not drop it and not fail silently.

## 4. Sub-components: open vocabulary, strictly nested

Below the five, the vocabulary is **open**. Any `+ label:` is legal as a child
of a component. `+ evidence:` under `+ BECAUSE:` is the canonical example and
is documented in the elisp header; `+ COUNTERFACTUAL:` under `+ BECAUSE:` (a
falsifier — what observation would show this claim is false) is in current use
in `library/baldwin/` and `library/pattern-discipline/`.

The openness is deliberate: a pattern language that cannot grow a new facet
cannot describe anything its authors had not already thought of.

**But the nesting is not optional.** Joe, 2026-08-13:

> `!conclusion` plus five components which are required, and then
> sub-components are allowed as an open vocabulary. But `+counterfactual`
> should never be imported alongside the five.

A sub-component imported as a peer of the five is a **corrupted import**, not a
new facet. The five are a closed, required set; everything under them is open
and subordinate. Any consumer that flattens the tree destroys exactly this
distinction.

## 5. Meta directives

`@name value` lines, before the argument body. `@flexiarg <family>/<name>` is
the pattern's qualified id and is what everything else keys on. Others in
current use include `@title`, `@style`, `@grade`, `@keywords`, `@audience`,
`@tone`, `@sigils`, `@subjects`, `@examples`, `@difficulty`, `@instantiates`,
`@provenance`.

`@grade` takes `principle | technique | snippet`.

## 6. Identity

The qualified id is `<family>/<name>`, where family is the directory under
`library/` and name is the file stem. It is **not** `flexiarg.<family>/<name>`
— that spelling is an artifact of one importer having modelled patterns as
code namespaces (§7) and should not appear in new work.

## 7. Conformance — two implementations, and they must agree

There are two parsers for this format:

| Implementation | Handles nesting |
|---|---|
| `futon3/contrib/flexiarg.el` — the standard | **yes**, indent stack |
| `futon3a/src/futon/flexiarg/projection.clj` — feeds the substrate | **no**, as of 2026-08-13 |

The Clojure section header regex is `#"^\s*[!+]\s+([^:]+):\s*(.*)$"`. The
leading `^\s*` discards the indent, so every node becomes a root. The store
consequently holds sub-components as siblings of the five — 32 `slot/counterfactual`
edges alongside `slot/because` at the time of writing. A re-typing fix is in
flight.

**Requirement going forward: a conformance check that both parsers produce the
same tree for the same file.** Two implementations of one format with no test
that they agree is how this divergence survived. The same shape has now been
found three times in this codebase — two divergent copies of `patterns-index.tsv`,
a forked ground-control README, and this. Whichever parser is "right" matters
less than that nothing tells you when they differ.

## 8. What an importer owes this format

1. Preserve the tree. Indent is structure.
2. Keep the five distinct from everything below them.
3. Do not classify patterns as code. They are not code, and modelling them as
   code namespaces puts them where pattern retrieval does not look.
4. Report anomalies — missing required components, unknown top-level labels,
   duplicate ids — rather than dropping them. Silent normalisation is how a
   library stops meaning what its authors wrote.

---

## 9. Rulings

- **2026-08-13, Joe:** `! conclusion` plus five required components; below them,
  sub-components are an open vocabulary; a sub-component must never be imported
  alongside the five.
- **2026-08-13, Joe:** `NEXT-STEPS` is **recognised but optional** — a
  top-level component, not one of the required five. An importer must accept a
  pattern that has no `NEXT-STEPS`, and must not treat its absence as an
  incompleteness finding under §3.
