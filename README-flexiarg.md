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

## 3a. When the components collapse into one

Sometimes the five will not genuinely differ: `context`, `IF` and `HOWEVER`
come out as three paraphrases of a single description. **That is
informative, not a failure**, and there are exactly two cases, which must be
told apart:

- **(a) The content is genuinely single-axis.** Its contribution is one
  observation — a clarification, a definition extension, a survey. A pattern
  can legitimately be this. It must then be **declared** as such, with the
  reason stated, rather than passing a completeness check silently.
- **(b) The extraction failed.** Whoever (or whatever) drafted the pattern
  could not differentiate the slots. That is a producer bug, and the fix is
  to redraft, not to accept the collapse.

**Do not lump (a) and (b) together.** A declared single-axis pattern with a
stated reason is a valid mark of single-axis content; a collapse with no
reason should be treated as a producer bug to fix. Conflating them hides
both signals — one a fact about the material, the other a fact about the
pipeline.

A workable test: if the pairwise semantic overlap between the slots is very
high (~80%+), you have a collapse and owe a ruling on which case it is.

*Provenance: this section is the surviving content of the former
`math-strategy/clarification-meta.flexiarg`, which was titled "Clarification
(Meta-Tag, not a Pattern)" and said of itself that it was not a pattern. It
described what to do when the `(context, tension, move)` triple cannot be
decomposed. That is a statement about the FORMAT, so it belongs here; Joe
ruled 2026-08-13 that it move into this specification and out of the pattern
library. The original also carried exemplar paper ids and a routing rule to
M-superpod-mark3's slot-distinctness enforcement, which stay with*
`library/math-strategy/PAPER-SHAPES-INDEX.md` *§5.*

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
`@tone`, `@sigils`, `@examples`, `@difficulty`, `@provenance`.

`@grade` takes `principle | technique | snippet`.

## 5a. Structural directives — the map between patterns

*(Standard revised 2026-08-17, Joe's direction. Before this, structure was
carried by three fields with overlapping meanings — `@family`, `@references`,
`@instantiates` — plus, in one case, a comment, because no field fit.)*

Patterns relate to other patterns. Two relations are declared, and **they are
not inverses of each other**:

| directive | direction | meaning | who writes it |
|---|---|---|---|
| `@why <id> [<id> …]` | toward the general | the authority this pattern rests on — the strategy it is an instance of | the pattern's own author |
| `@how <id> [<id> …]` | toward the specific | the named methods by which this pattern is carried out | an editor, later |
| `@see-also <id> [<id> …]` | sideways | a peer technique; no claim of authority in either direction | either |

**Why they are not inverses.** `@why` is *total and structural*: a specific
pattern rests on something, and its author knows what. `@how` is *partial and
curatorial*: only some patterns acquire methods worth naming, and the judgement
of which ones is editorial. Inverting `@why` mechanically would give every
general pattern a list of everything that ever instantiated it — unbounded,
ever-growing, and useless for reading. `math-formalization/layer-cake-crossover-split`
`@why math-informal/estimate-by-bounding` is true; the reverse is not, because
layer-cake is one instance among many and not *the* method.

**Coverage is a third axis, and is not `@how`.** A pattern may also be recorded
as holding, or failing to hold, at a node of an external map — for the war-room
namespace, the AIF control map's R-nodes:

| directive | meaning |
|---|---|
| `@holds-at <node> [<node> …]` | the nodes where this pattern supplies a mechanism |
| `@holds-open <node> [<node> …]` | the nodes where it is hollow — a **red ring** |

These point *out of the library*, at map nodes, not at patterns or memories, so
they are a different relation from `@how` and must not reuse its name. (They
were briefly written as `@how`/`@how-open` in `war-room/*` before this section
was read; renamed 2026-08-22, no consumers existed.) Emptiness at a node is a
claim about the world and is what the pattern cascade draws as a hole.

`@how` targets may be patterns OR reviewed memories. A memory attached to a
pattern is already a refinement of it, recorded by the attachment economy —
so do not restate an attachment as an `@how`.

**Deprecated spellings**, to be migrated, not used in new work:

- `@instantiates` → `@why` (same meaning, 19 files at the time of writing).
- `@references` → split. It conflated authority with peerage; roughly half its
  edges point across families (authority, → `@why`) and half within one
  (peers, → `@see-also`). The split is a judgement per edge, not a rename.
- `@subjects` → `@cross-list` (see §5b), with a controlled vocabulary.
- `@family` is a *kind* classification (e.g. `math-strategy/characterization-result`),
  not a parent. It is unaffected by this revision and should not be read as
  structure.

## 5b. Subject categories — primary and cross-list

*(Standard set 2026-08-17, Joe's direction: follow arXiv.)*

- **The primary category is the directory.** `library/<kind>-<CODE>/<name>` —
  e.g. `math-informal-CT/check-it-on-generators`, already conforming. `<CODE>`
  is the arXiv subject code without its `math.` prefix: `AT` algebraic topology,
  `GN` general topology, `GT` geometric topology, `CA` classical analysis, `FA`
  functional analysis, `CT` category theory, and so on. Exactly one primary
  category, because a file lives in exactly one directory.
- **Cross-listing is a field.** `@cross-list [FA PR]` — the other categories the
  pattern genuinely belongs to. Optional, plural, no ordering.

The primary category is not restated in a field: the directory is the single
spelling of that fact. An importer reads the primary from the path.

The directory therefore carries two axes at once — kind (`informal`,
`formalization`, `strategy`) and subject code — which is what `math-informal-CT`
already did before the convention was written down. The motivation is
filesystem manageability at library scale, not taxonomy for its own sake.

## 5c. The directive ontology, and why the whitelist stays

*(Ruling 2026-08-17, Joe.)*

`flexiarg-directives.edn`, beside this file, is the machine-readable half of
§5/§5a/§5b **and the ingest whitelist**. A directive reaches the store only if
it is `:standard` there.

**The whitelist is not a defect to be removed — it is the semantic gate.** An
unknown label cannot be assigned semantics, so it must not propagate. What was
wrong was only the *silence*: unknown directives were dropped without report,
against §8. Both halves are now required — gate AND report.

Standardising a directive is an editorial act in that one file, reviewable in a
single diff. It is deliberately not automatic and not a code change.

**The census that motivated it** (all 1151 library files, 2026-08-17): 80
distinct directives; 11 parsed; 69 dropped in silence. 77% of the dropped mass
is bespoke single-family payload (`@bits`, `@hex`, `@trigrams`, `@exotype-*` —
257 uses each in one family) which should never have reached the store.

**And a trap the census caught.** Several directives look structural and are
not:

- `@references-extra` (60 uses) is **not** an extension of `@references`. It
  holds free-text citations and origin notes — *"Lewontin (1983)…"*, *"Instance
  of origin: frame-daily-scan-006…"*. Admitting it as a pattern edge would have
  injected sixty citation blobs into the pattern graph.
- `@ancestors` (35) is likewise sources, not patterns.
- `@illustrates` (23) is free-text claims.
- `@next` (45) and `@part` (38) are document reading-order and section names.

Name similarity is not semantic similarity, and only reading the values tells
you which is which. This is the concrete reason the gate stays.

**Three axes, not one.** Reading the directives showed structure of three
different kinds, which must not be merged:

| axis | directives | what it records |
|---|---|---|
| **semantic** | `@why` `@how` `@see-also` | pattern to pattern by *meaning* |
| **document** | `@part` `@up` `@next` | where a pattern sits in the prose it was extracted from |
| **neither** | `@references-extra` `@ancestors` `@illustrates` | citations and claims wearing structural-looking names |

`@up` is the case that decides it. `t3/architectural-pair` declares
`@up t3/transformation-space` — and `transformation-space` is the **section it
lives in**, not the strategy it instantiates. Its targets are section hubs
(`t4r/main-case` with 8 children, `or/foundations-openness` 8), and it appears
only in document-shaped families beside `@part` and `@next`. Document position
is provenance, not authority, and folding it into `@why` would put a table of
contents into the semantic graph.

**Generation config is not pattern data.** `@allow-new-claims`, `@ban`
(*"cutting-edge, state-of-the-art, leveraging synergies…"*), `@max-iterations`
and `@length` instruct a writing agent. They describe how the text was to be
produced, not what the pattern means, and belong in generator config.

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

- **2026-08-17, Joe:** pattern-to-pattern structure is declared by `@why`
  (authority, toward the general) and `@how` (refinement, toward the specific),
  which are **not inverses** — `@why` is total and authored, `@how` is partial
  and editorial. Peers get `@see-also`. `@instantiates` and `@references` are
  the deprecated spellings (§5a). Rationale: at the time of the ruling the
  library carried 80 authored pattern-to-pattern edges, **every one of them
  pointing up or sideways and not one pointing down** — authors write authority
  unprompted and never write refinement, which is the evidence that the two
  relations differ in kind rather than in sign.

- **2026-08-17, Joe:** subject categories follow arXiv — **primary category is
  the directory**, cross-listing is the `@cross-list` field (§5b). The primary
  is never restated in a field. Motivation: a single directory holding
  thousands of patterns is unmanageable; `math-informal-CT` was already the
  worked example of the convention.

- **2026-08-17, Joe:** `@family` and `@childof` are the same shape as `@up` —
  document parents, not authority. `baldwin/mechanism` parents 10 of the 11
  files in its family; that is a section hub. They migrate to `@up`.

- **2026-08-17, Joe:** workflow state (`@status`, `@verdict`, `@confidence`,
  `@review`) does not belong in the pattern text at all, even as metadata.
  Handled by the Arxana ledger subsystems instead; queued as
  `E-arxana-workflow-management`.

- **2026-08-17, Joe:** content that looks like metadata belongs in the pattern
  body as a sub-block (`+governance:` under `+THEN:`), not in a directive.

- **2026-08-17, Joe:** the map is not the route. The declared edges are terrain;
  a *cascade* is a reading of that terrain assembled for a task, and is not
  declared in the files. The War Machine's `:cascade` argument — a vector of
  pattern-ids with the order discarded downstream — is a **selection**, not a
  cascade, and the terminology should not be confused. Nothing in §5a fixes a
  cascade in advance.
