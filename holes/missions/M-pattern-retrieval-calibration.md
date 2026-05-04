# Mission: Pattern Retrieval Calibration

**Status:** DERIVE (entered 2026-05-04, MAP 2026-05-04, IDENTIFY 2026-05-04).
**Owner:** Joe
**Cross-refs:**
- `holes/missions/M-pattern-mining.md` — established that the current
  futon3a MiniLM-cosine substrate is artifact-prone and unsafe to treat
  as a clean sensor.
- `holes/missions/M-pattern-application-diagnostic.md` — carries the
  build-the-new typed substrate and the requirement that pattern
  application be well-specified, repeatable, and demonstrably useful.
- `holes/excursions/E-pattern-peripheral.md` — companion Codex-handoff
  excursion; the symmetric *creation*-side gate to this mission's
  *retrieval*-side calibration. Blocks on P-1 (canonical parser at
  `futon3a/src/futon/flexiarg/projection.clj`) shipping; lands in
  advisory mode first, enforced mode after a violation corpus exists.
- `futon5a/holes/missions/M-trip-journal.md` — the B→A complement; this
  mission should improve the A→B side in a form that the B→A side can
  actually learn from.
- `futon5a/holes/excursions/E-btoa-candidate-queue-reweighting-v0.md`
  and `E-btoa-evidence-sample-mapping-v0.md` — phase-1 B→A/SORRY
  weighting and the first hand-labeled sample.

## Motivation

Pattern retrieval is already live enough to matter:

- futon3a retrieves patterns every turn and surfaces them operationally
- the evidence landscape is accumulating pattern-attributed turn records
- downstream consumers are starting to treat those retrievals as priors
  and notifications, rather than as disposable decoration

But the current retrieval surface is weak:

- embedding ranking is a single-stage cosine lookup with thin
  provenance
- keyword ranking is simplistic
- the notification/report surface collapses enriched pattern structure
  down to `{id, title, score}`
- the ingest side currently does not preserve flexiarg structure with
  the fidelity needed for a trustworthy retrieval substrate

This is dangerous for two reasons. First, low-quality pattern
retrieval wastes operator attention turn by turn. Second, the B→A /
SORRY side will inherit whatever biases the A→B side emits if we let
it learn from noisy labels.

## The actual sorry being identified

**We are using per-turn pattern retrieval as if it were a calibrated
sensor on what the work is about, while the present substrate does not
yet preserve enough pattern structure, retrieval provenance, or
feedback signal to justify that trust.**

The sorry is not "embeddings are bad" or "humans should label more."
It is the combination:

- low-fidelity pattern representation
- weak ranking surface
- no disciplined HIT loop
- no clean path from adjudicated retrievals back into future ranking,
  tagging, or pattern maintenance

If left alone, this creates a bad symmetry: A→B retrieval emits noisy
signals, and B→A/SORRY mining learns from the noise.

## IFR

> **For each turn, the stack presents a small, legible candidate set of
> patterns in a structurally uniform form, with explicit rationale for
> why each candidate was retrieved; Joe can adjudicate the result
> cheaply; and those adjudications become durable evidence that improves
> future retrieval, tagging, and pattern quality.**

This means:

1. **High-fidelity pattern presentation.**
   Canonical patterns must be retrievable as structured objects, not
   flattened blobs.
2. **Small candidate set.**
   The retrieval surface should converge to something like top-3, not a
   raw ranked dump.
3. **Legible rationale.**
   Each candidate should say why it is here: source path, matching
   features, and clause-level justification where available.
4. **Cheap adjudication.**
   The operator should be able to answer "best / none / ambiguous"
   without opening five more windows.
5. **Learning loop.**
   HIT outcomes must write back into durable evidence so the substrate
   can be recalibrated.
6. **A↔B compatibility.**
   The same evidence contract should help both A→B pattern retrieval and
   B→A / Strategic SORRY mining.

## Non-negotiable invariants

1. **Pattern structure is authoritative.**
   Retrieval calibration may not depend on lossy flattening as the only
   representation.
2. **Canonical patterns need a canonical projection.**
   The standard-library flexiarg shape remains load-bearing for
   retrieval, comparison, and embeddings.
3. **Haiku is an advocate, not an oracle.**
   A cheap LLM may make the case for a candidate, but it does not assign
   truth. The HIT decision and the durable evidence record do.
4. **Feedback must be reusable.**
   HIT outputs must land in a form future ranking/tagging jobs can read.
5. **B→A symmetry is a consumer, not an afterthought.**
   Any retrieval evidence contract designed here must be inspectable
   from the B→A/SORRY side as well.

## Proposed retrieval contract

### Stage 1: candidate generation

Generate a bounded candidate pool from multiple deterministic sources:

- top-3 classical / keyword retrieval
- top-3 embedding retrieval
- dedupe by pattern id, preserving source provenance

This keeps recall while avoiding a single-model monoculture.

### Stage 2: structured rendering

For each candidate, render the pattern in a uniform packet:

- id, title, source path
- canonical clauses / components in order
- summary / because / next-steps projections
- feature hits used in retrieval

The packet must not silently degrade to title + score.

### Stage 3: cheap rationale synthesis

Produce a brief "why this candidate?" argument for each candidate.

Start with classical features:

- hotword overlap
- title overlap
- `BECAUSE` overlap
- `IF` / scope overlap
- recent prior evidence, if available

Optional cheap Haiku call:

- given the turn and the structured candidate packets, ask Haiku to
  argue the salience of each candidate and note disanalogies
- treat this as an additional rationale layer, not as ranking authority

### Stage 4: HIT packet

Present Joe with a small review packet:

- top candidates
- brief rationale per candidate
- option to pick best / multiple / none / not-sure

The packet should be fast enough to use inline during ordinary work.

### Stage 5: feedback incorporation

Persist the HIT outcome as durable evidence keyed to:

- turn / session
- candidate set shown
- chosen pattern(s), if any
- rejection / ambiguity when relevant
- retrieval provenance

This is the seed data for:

- reranker calibration
- embedding audit
- pattern text refinement
- B→A family / motif / Strategic SORRY learning

## Outposts

### O-1: Structural fidelity audit

Verify that canonical flexiargs can be presented uniformly:

- preserve all pattern components in ingest
- identify which current surfaces flatten them away
- name the minimum projection required for retrieval/HIT review

### O-2: Hybrid top-k baseline

Build the deterministic retrieval packet:

- classical top-3
- embedding top-3
- deduped candidate pool
- classical rationale features

### O-3: Haiku-assisted comparison

Add the optional cheap adjudicator:

- structured prompt over the candidate packets
- concise "case for / case against" per candidate
- no authority to assign the final label

### O-4: HIT logging

Write the operator decision as durable evidence in a reusable shape.

### O-5: B→A / SORRY bridge

Show how HIT-labeled A→B retrievals help:

- family-level priors
- motif-level evidence bundles
- Strategic SORRY pressure updates

The desired outcome is one substrate serving both directions, not two
isolated annotation programs.

## Explicit non-goals

- Replacing the whole retrieval system with Haiku adjudication.
- Declaring free-form patterns "good enough" without a canonical
  projection for retrieval.
- Training a learned reranker before the HIT substrate exists.
- Forcing exact Strategic SORRY assignment on every turn too early.

## Exit signal

This mission exits IDENTIFY when we have:

1. a named evidence shape for retrieval HITs
2. a clear top-3 review packet contract
3. a decision on what structured pattern projection is canonical for the
   retrieval surface
4. an explicit bridge statement showing how the same feedback improves
   both A→B pattern retrieval and B→A/SORRY mining

All four are closed in §MAP below (M-3 / M-4 / M-5 / M-6 respectively).

---

## MAP (2026-05-04)

### M-1. The retrieval surface as a typed pipeline

Drawing this side parallel to M-pattern-mining §M-1 and the B→A
graph in `E-btoa-candidate-queue-reweighting-v0.md`:

**Nodes.**
- `B-turn` — agent assistant message (Claude or Codex completion).
- `query-snippet` — `(user-msg + response-preview)` slice that goes
  into retrieval (currently `subs … 200` chars each, see
  `futon3c/dev/futon3c/dev.clj:836–839`).
- `candidate` — a single retrieved pattern, *not* a `{:id :title :score}`
  flattening: a structured packet (see M-3).
- `candidate-set` — the deduped top-K pool (see M-2).
- `rationale` — per-candidate "why this candidate?" object (see M-4).
- `hit-packet` — the operator-facing review packet that bundles
  `{candidate-set, rationale, turn-ref}`.
- `hit-decision` — the operator's adjudication
  (`best | multiple | none | ambiguous`).
- `hit-record` — the durable evidence triple (see M-5).
- `consumer` — Stack HUD patterns widget; Arxana Browser pattern
  activation view; the B→A learner that derives `A-turn → family`
  edges (see M-6).

**Edges.**
- `B-turn → query-snippet` — `extract-user-message` + response
  preview; deterministic.
- `query-snippet → candidate-set` — Stage-1 retrieval (M-2).
  Today: MiniLM cosine top-3 only. Target: classical top-3 ⊕
  embedding top-3, deduped.
- `candidate-set → hit-packet` — Stage-2/3 structured render +
  rationale synthesis (M-3, M-4).
- `hit-packet → hit-decision` — operator review.
- `hit-decision → hit-record` — Stage-5 evidence write (M-5).
- `hit-record → consumer` — read via the existing futon1a
  evidence projection endpoint, filtered on `body.event ==
  "retrieval-hit"`.

The mission's DERIVE work decomposes by edge: the only edges that
contribute new *substrate* are the second (multi-source candidate
generation) and fourth-into-fifth (HIT loop). Stages 2/3 are
mechanical projections of structure that already exists on disk;
M-pattern-application-diagnostic's typed-slot work will progressively
upgrade what those projections can carry, but neither of those edges
needs to wait on it.

### M-2. Candidate-set contract (top-K packet)

A retrieval call returns a single `candidate-set` object:

```clojure
{:retrieval/version    "calibration-v0"
 :retrieval/turn-ref   {:agent-id "claude-4"
                        :session-id "..."
                        :turn-counter 137
                        :evidence-id "e-…"}            ; the B-turn record
 :retrieval/query      {:proto-text "..."              ; ≤200+200
                        :hash "sha256:..."}             ; for repeatability
 :retrieval/sources    [{:source :classical
                         :ranker "hotword-overlap+title"
                         :top-k 3
                         :ids ["agent/intent-handshake-is-binding" …]}
                        {:source :embedding
                         :ranker "minilm-cosine"
                         :top-k 3
                         :ids ["f3/p4" …]}]
 :retrieval/candidates [<candidate-packet> …]           ; deduped, ≤6
 :retrieval/elapsed-ms 142}
```

Invariants:
- `count(candidates) ≤ K_classical + K_embedding` after dedupe by
  pattern id (default 6, target presented set 3 — see M-7 for
  selection from pool).
- `:retrieval/sources` preserves provenance even after dedupe: each
  candidate carries the set of sources that surfaced it (M-3).
- `:retrieval/query.hash` makes the call idempotent under replay —
  identical query against an unchanged corpus must yield an
  identical candidate set, modulo ranker/version bumps. This is the
  IFR's *repeatable* property in operational form.

### M-3. Canonical pattern projection — *closes IDENTIFY exit signal 3*

**Decision: the canonical retrieval projection is the
`flexiarg-projection-v0` (below), with a forward-compatible slot for
the typed `{:context :tension :move :witness-shape :domain}` shape
M-pattern-application-diagnostic is building.**

Rationale: today's pattern library is 850+ flexiargs with stable
clause structure (`@flexiarg`, `@title`, `@sigils`, `@keywords`,
`@audience`, `@references`, then `! conclusion:` with named `+
clauses` including `+ context:`, `+ BECAUSE:`, `+ NEXT-STEPS:`, plus
pattern-specific named clauses). Spot-checked against
`library/futon-theory/task-as-arrow.flexiarg`: every clause name and
the `! conclusion:` block are first-class structure that the current
`{:id :title :score}` flattening throws away. A canonical projection
that preserves this structure *now* gives the HIT loop something to
adjudicate against, and a typed-slot lift later does not require
rewriting the projection — only enriching it.

The projection per candidate:

```clojure
{:pattern/id           "futon-theory/task-as-arrow"           ; canonical id
 :pattern/title        "A Task Is a BHK Arrow: ..."
 :pattern/source-path  "library/futon-theory/task-as-arrow.flexiarg"
 :pattern/sigils       ["⇒/箭"]
 :pattern/keywords     ["task" "arrow" "bhk" …]
 :pattern/references   ["futon-theory/curry-howard-operational" …]
 :pattern/conclusion   "A task in the futon stack is not a work …"
 :pattern/clauses      [{:name "context"
                         :body "The Curry-Howard operational principle …"}
                        {:name "BHK-ARROW-SEMANTICS"
                         :body "In BHK proof theory: …"}
                        {:name "BECAUSE"
                         :body "Without arrow semantics, tasks are …"}
                        {:name "NEXT-STEPS"
                         :body "next[Rewrite 2-3 common task types …]"}]
 :pattern/typed-slots  nil                                  ; populated when M-pattern-application-diagnostic lifts this pattern
 :pattern/sources      #{:classical :embedding}              ; provenance
 :pattern/scores       {:classical {:hotword-overlap 4
                                    :title-overlap 1
                                    :because-overlap 0}
                        :embedding {:minilm-cosine 0.71}}
 :pattern/projection-version "flexiarg-v0"}
```

What this commits us to:

- **Ingest preserves clause structure.** The
  `flexiarg-projection-v0` is computed once at ingest time
  (M-pattern-mining §M-6 P-8 auto-ingest pipeline becomes the
  natural home), persisted alongside the `.flexiarg` source, and
  read by the retrieval surface. This is non-trivial: it means
  the `minilm_pattern_embeddings.json` is no longer the canonical
  retrieval surface — it becomes one *score* feeding into a
  structured packet whose canonical form is the projection.
- **Typed-slot lift is additive.** Patterns that
  M-pattern-application-diagnostic has lifted to typed slots
  populate `:pattern/typed-slots`; patterns that haven't carry
  `nil` and the HIT loop falls back to clause-level rationale.
  The two missions share the projection without one blocking the
  other.
- **Embedding-only patterns are visible failures, not silent
  drops.** A retrieval that finds a candidate by embedding for
  which no `flexiarg-projection-v0` exists (because the source file
  is missing or unparsed) is rendered as a partial packet with
  `:pattern/projection-version "missing"` and an explicit
  rationale "candidate retrieved but not structurally available"
  — closing M-pattern-mining F-1's invisible-coverage gap on the
  HIT side.

### M-4. Rationale contract

For each candidate in the set, a `rationale` object accompanies it:

```clojure
{:rationale/version "v0"
 :rationale/classical-features
   {:hotword-overlap   ["task" "arrow"]                    ; tokens hit
    :title-overlap     []
    :because-overlap   ["constructive" "proof"]
    :scope-overlap     []                                   ; IF / scope clauses
    :recent-evidence   {:last-fired "2026-05-03T14:21Z"
                        :prior-hits-30d 4
                        :prior-best-1d 1}}                  ; from durable evidence
 :rationale/why-this    "hot-word match on 'task' and 'arrow'; recent
                         operator selection on 2026-05-02"
 :rationale/disanalogy  "this turn's query mentions 'tension'; the
                         pattern's BHK reading does not address tension
                         directly"                          ; optional
 :rationale/llm-advocate                                    ; optional Stage-3 layer
   {:model "claude-haiku-4-5"
    :prompt-version "retrieval-advocate-v0"
    :case-for "..."   :case-against "..."
    :tokens-in 1240   :tokens-out 280
    :elapsed-ms 1100}}
```

Discipline (matches §Non-negotiable invariants 3):

- Classical features land first and are mandatory; Haiku advocate
  is **opt-in per call** (e.g. only on the top-3 of the deduped
  pool), not part of the default ranker.
- The advocate produces an *argument*, not a label. The HIT
  decision overrides any case the advocate makes.
- `:rationale/llm-advocate` carries cost (`tokens-in`, `tokens-out`,
  `elapsed-ms`) so the substrate's spend stays observable. The
  default policy is "Haiku on top-3 only when operator is awake and
  reviewing"; surface controls in the HIT widget can pause it.

### M-5. HIT evidence triple — *closes IDENTIFY exit signal 1*

**Decision: adopt the canonical / run / snapshot triple from
`E-candidate-queue-xtdb-shape-v0.md`, with item type
`retrieval-hit/*`.** Rationale: parallels M-pattern-mining §M-5 and
the B→A queue, so the A↔B bridge in M-6 is mechanical.

**1. Canonical hit doc** (one per HIT decision):

```clojure
{:xt/id "retrieval-hit/<turn-evidence-id>"
 :hit/doc-type           :retrieval-hit
 :hit/turn-ref           {:agent-id "claude-4"
                          :session-id "..."
                          :turn-counter 137
                          :evidence-id "e-…"}
 :hit/at                 #inst "2026-05-04T..."
 :hit/candidate-set-hash "sha256:..."                       ; full packet
 :hit/candidates-shown   ["futon-theory/task-as-arrow"
                          "agent/intent-handshake-is-binding"
                          "f3/p4"]
 :hit/sources-by-id      {"futon-theory/task-as-arrow" #{:classical :embedding}
                          …}
 :hit/decision           :best                              ; :best :multiple :none :ambiguous
 :hit/chosen             ["futon-theory/task-as-arrow"]      ; subset of candidates-shown; [] if :none
 :hit/operator           "joe"
 :hit/elapsed-ms         8200                                ; review latency
 :hit/notes              "task-as-arrow nails the BHK framing;
                          intent-handshake is broad-catchment here"
 :hit/projection-version "flexiarg-v0"
 :hit/rationale-version  "v0"
 :hit/llm-advocate-used? true
 :hit/calibration-version "calibration-v0"}
```

**2. Run doc** (one per session — analogous to
`pattern-activation-run/*`):

```clojure
{:xt/id "retrieval-hit-run/<session-id>"
 :run/doc-type      :retrieval-hit-run
 :run/session-id    "..."
 :run/started-at    #inst "..."
 :run/closed-at     #inst "..."
 :run/hit-ids       ["retrieval-hit/e-…" …]
 :run/turn-count    24
 :run/hit-rate      {:best 14 :multiple 3 :none 5 :ambiguous 2}
 :run/calibration-version "calibration-v0"}
```

**3. Snapshot doc** (rendered top-N for a surface, refreshed on
session close):

```clojure
{:xt/id "retrieval-hit-snapshot/<session-id>/<surface>"
 :snapshot/doc-type      :retrieval-hit-snapshot
 :snapshot/session-id    "..."
 :snapshot/surface       :stack-hud-patterns               ; or :arxana-pattern-activation
 :snapshot/at            #inst "..."
 :snapshot/top-patterns  [{:pattern/id "...", :hits 7,
                           :best 4, :multiple 1, :none 2}
                          …]
 :snapshot/calibration-version "calibration-v0"}
```

Emission shape on the live evidence stream (so the existing
`/api/alpha/evidence` projection works without endpoint surgery):

```clojure
{:type      :coordination
 :body      {"event" "retrieval-hit"
             "agent-id"  "..."
             "decision"  "best"
             "chosen"    ["..."]
             "shown"     ["..." "..." "..."]
             "turn-ref"  {…}
             "calibration-version" "calibration-v0"}
 :tags      [:hit :retrieval :calibration]}
```

This piggybacks on the same `body.event`-filter convention the
patterns widget already uses (M-pattern-mining §M-4 row 1), so a
new `event "retrieval-hit"` filter immediately feeds the calibration
view alongside the existing `event "context-retrieval"` view.

### M-6. A↔B bridge — *closes IDENTIFY exit signal 4*

The HIT triple is the substrate the B→A learner has been waiting
for. Concretely, every `retrieval-hit/*` with `:hit/decision :best`
and a non-empty `:hit/chosen` produces, in the B→A graph
(`E-btoa-candidate-queue-reweighting-v0.md` §The Graph To Score):

```text
A-turn(turn-ref) ── confirmed ──▶ pattern(:hit/chosen[0])
                                        │
                                        ▼
                                family(:pattern/family)
                                        │
                                        ▼
                                motif / Strategic SORRY
```

The `A-turn → family` edge — currently hand-labeled in
`E-btoa-evidence-sample-mapping-v0.md` — becomes a query over
`retrieval-hit` records joined to `pattern → family` membership in
`structural-law-inventory.sexp`. **The hand-labeled sample's
ground-truth role is preserved**: it remains the calibration sample
for the joint, and any drift between hand labels and HIT-derived
labels is itself a signal that retrieval is biased.

Symmetric direction: a `:hit/decision :none` is a *negative*
training signal that the A-side substrate doesn't currently emit.
Negative HITs land as the "cannot-find-pattern" denominator that
both M-pattern-mining F-4 (silent fail) and M-pattern-application-
diagnostic (coverage IFR property) need. **The same record serves
both directions.**

This is the bridge statement: a single `retrieval-hit/*` record
participates in (a) A-side reranker calibration / embedding audit /
projection-completeness check, (b) B-side family-priors and
Strategic SORRY pressure updates. Two consumers, one substrate; no
parallel annotation programs.

### M-7. Surfaces inventory

| Surface | Role | Status |
|---|---|---|
| `futon3c/dev/futon3c/dev.clj:830` `context-retrieval!` | Producer of today's `event "context-retrieval"` — needs to grow into producer of `event "retrieval-candidate-set"` (the M-2 packet) and trigger the HIT widget | Modify |
| HIT widget (Stack HUD) | Renders the M-2 packet + M-4 rationale; captures `:best/multiple/none/ambiguous` decision; emits `retrieval-hit/*` | New, lives next to `futon0/contrib/stack-hud-widgets.el:478` |
| Arxana Browser → Pattern Activation (`futon4/dev/arxana-browser-pattern-activation.el`) | Already reads activation; gains a "HITs" view filtered on `event "retrieval-hit"` | Extend |
| `/api/alpha/evidence?event=retrieval-hit` | Shared read surface for both A-side calibration and B→A learner | Reuses existing endpoint |
| `flexiarg-projection-v0` ingestion | Computes / persists the M-3 projection alongside the source `.flexiarg` | New, sequenced after M-pattern-mining P-8 lands |

The HIT widget must satisfy §IFR property 4 (cheap adjudication):
it lives inline (Emacs posframe / chat-buffer-side, per the
*REPL UX preferences* discipline — never side windows, never steals
cursor) and accepts a 4-key decision. The packet is rendered
flexiarg-clause-by-clause, not as a wall of text.

### M-8. Bounded probes (DERIVE candidates)

Each probe answers one design question and is sized to one focused
session. Listed by priority.

**P-1. `flexiarg-projection-v0` parser + persister.**
- Implement parsing of `.flexiarg` to the M-3 packet shape.
- Persist projections at ingest: emit `event "pattern-projected"`
  evidence per file with `{path, hash, projection-version,
  clause-count, ms}`.
- Spot-check on `library/futon-theory/`, `library/realtime/`,
  `library/agent/`, `library/aif/` (heterogeneous clause shapes).
- Exit: every `.flexiarg` under watched roots has a current
  projection record; missing-projection coverage gap (M-3) is
  zero on those roots.
- Sequencing: depends on M-pattern-mining P-8 (auto-ingest
  watcher); parser landing first lets P-8 wire the watcher to
  call it.

**P-2. Hybrid candidate-set builder.**
- Replace `run-futon3a-search` with a function that emits the
  M-2 `candidate-set` object.
- Classical ranker: hotword-overlap + title-overlap + `BECAUSE`
  overlap, top-3.
- Embedding ranker: existing MiniLM cosine, top-3.
- Dedupe by `:pattern/id`, preserve `:retrieval/sources`.
- Exit: a turn produces a deduped pool of ≤6 structured packets;
  identical query against identical corpus is byte-identical.

**P-3. HIT widget (read-only first).**
- Render the candidate-set + rationale inline.
- Decision keys (`b`/`m`/`n`/`a`); free-text note buffer.
- Emit `event "retrieval-hit"` on submit.
- No advocate yet (P-5 below).
- Exit: Joe can adjudicate a turn in ≤30 s without leaving the
  REPL; one HIT record persists per submitted decision.

**P-4. Run + snapshot derivation.**
- On session close, compute the `retrieval-hit-run/*` doc and
  per-surface `retrieval-hit-snapshot/*` docs from accumulated
  HITs.
- Exit: a closed session produces exactly one run doc plus one
  snapshot per active surface; doc shapes match M-5.

**P-5. Haiku advocate (opt-in).**
- Add the M-4 `:rationale/llm-advocate` layer.
- Default off; toggle in HIT widget; budget cap per session.
- Exit: when enabled, top-3 candidates carry advocate text; cost
  per session is ≤$0.05 at default budget.

**P-6. B→A bridge consumer spike.**
- One-shot script that reads `retrieval-hit/*` with `:best`
  decisions, joins to `structural-law-inventory.sexp` to derive
  `A-turn → family` edges, and compares against the
  hand-labeled sample in `E-btoa-evidence-sample-mapping-v0.md`.
- Exit: agreement / disagreement table; any disagreement is a
  named bias hypothesis (broad-catchment family, missing
  projection, etc.).
- This is the M-6 bridge made operational on real data.

**P-7. Coverage / drop diagnostics.**
- Counters on the producer: `retrieval-candidate-set-empty` (no
  candidates at all), `retrieval-candidate-projection-missing`
  (candidate found but no `flexiarg-projection-v0`), per-source
  attribution.
- Exit: a Stack HUD column showing per-day denominators alongside
  HIT decisions.

**P-8. Recalibration loop.**
- Once N≥30 HITs land, fit a simple reranker over the M-4
  classical features against the `:hit/decision :best` labels
  (logistic regression scale; do not train an end-to-end model
  yet).
- Treat the result as a **calibration probe**, not a deployed
  ranker — i.e., a notebook artifact that says "if we reweighted
  classical features by these coefficients, top-3 best-hit-rate
  goes from X to Y."
- Exit: a numeric answer to "is the substrate getting better
  with feedback?" that is reproducible from the HIT records.

### M-9. Sequencing

```
P-1 (parser) ──┬──▶ P-2 (hybrid set) ──▶ P-3 (HIT widget) ──▶ P-4 (run/snapshot)
               │                                  │
               │                                  ├──▶ P-5 (advocate, opt-in)
               │                                  │
               │                                  └──▶ P-7 (coverage diagnostics)
               │
               └──────── (P-8 auto-ingest watcher from M-pattern-mining feeds P-1)

once P-3+P-4 have run for ~30 turns:
                                                   └──▶ P-6 (B→A bridge spike)
                                                   └──▶ P-8 (recalibration probe)
```

P-1 and the upstream auto-ingest watcher (M-pattern-mining P-8)
are the prerequisite — without them the projection is a one-shot
local hack rather than substrate. P-2 and P-3 are the minimum
viable HIT loop; everything else is calibration-of-calibration.

### M-10. Out-of-scope guards (carried into MAP)

The §Explicit non-goals from IDENTIFY survive intact, and gain
two operational guards under the MAP shape:

- **No reranker training before P-3+P-4 have produced ≥30
  HITs.** P-8 is a calibration probe over real adjudications, not
  a deployed model.
- **No projection-version migration without an explicit version
  bump.** Patterns persist with `:pattern/projection-version`;
  changes to clause extraction are a `flexiarg-projection-v1`,
  not a silent rewrite of v0 records — so HIT records remain
  interpretable across substrate evolution.

---

### Checkpoint MAP-entry — 2026-05-04

**What was done:** restructured the mission from IDENTIFY into a
typed pipeline (M-1), specified the candidate-set contract (M-2),
fixed the canonical pattern projection as `flexiarg-projection-v0`
with a forward-compatible typed-slot field (M-3, closes exit
signal 3), specified the rationale shape with Haiku as opt-in
advocate (M-4), adopted the canonical/run/snapshot triple for HIT
records as `retrieval-hit/*` (M-5, closes exit signal 1), wrote
the explicit A↔B bridge statement (M-6, closes exit signal 4),
inventoried the surfaces (M-7), listed eight bounded probes
P-1..P-8 with sequencing (M-8, M-9), and added two out-of-scope
guards (M-10).

The candidate-set contract (M-2) plus the HIT triple (M-5) plus
the inline review packet (M-7) jointly close exit signal 2.

**Test state:** N/A (mission doc only; no code changes in this
checkpoint).

**Dependencies surfaced:** P-1 needs M-pattern-mining P-8
(auto-ingest watcher) for the projection to be a substrate
rather than a one-shot. P-6 depends on P-3 + P-4 producing real
HITs and on `structural-law-inventory.sexp` exposing the
pattern→family map the join requires; verify that join shape
exists before running P-6.

**Next:** pick P-1 or P-2 for the first DERIVE session. P-1
(projection parser) is the lower-risk entry — it's a pure
function over disk, produces durable artifacts (one projection
per .flexiarg) that survive even if the rest of the calibration
loop changes shape, and unblocks both this mission and the
coverage half of M-pattern-mining.

---

## DERIVE (2026-05-04)

### D-1. Verified reuse points in the current code

Before choosing the first probe, verify what substrate already exists:

- `futon3c/dev/futon3c/dev.clj` still builds `proto-text` as the first
  200 chars of the user message plus the first 200 chars of the
  response preview, then sends that through `run-futon3a-search`
  inside `context-retrieval!`. So M-2's candidate-set seam is real and
  localized; we are not chasing a phantom boundary.
- `futon3/scripts/build_pattern_index.clj` already has the richer
  parsing machinery we need for clause preservation:
  `split-arg-blocks`, `parse-components`, `extract-meta`, and the
  `by-label` summary / because derivations.
- `futon3/scripts/flexiarg_projection.clj` already names the right
  conceptual seam, but its current v0 is shallow: it emits only the
  header-derived qname, a coarse `:var/has-doc` signal, and body
  symbols. It does **not** preserve clause structure.
- `futon3c/src/futon3c/logic/inventory.clj` already parses
  `structural-law-inventory.sexp` and extracts family entries, which
  means M-6's pattern→family join has a real code substrate on the
  B→A side.

### D-2. Adjustment to MAP

MAP implicitly allowed for a new projection parser to appear as a fresh
surface. That would be a mistake. The codebase already contains:

1. a shallow projection path (`scripts/flexiarg_projection.clj`)
2. a richer clause parser (`scripts/build_pattern_index.clj`)
3. a second near-copy in `scripts/pattern_sync.clj`

So DERIVE adds a hard guard:

**P-1 may not introduce a third independent flexiarg parser.**

Instead, P-1 must:

- extract or promote one shared parser shape from the richer existing
  machinery
- make `flexiarg_projection.clj` the compatibility adapter for that
  shared parser, rather than a parallel lossy implementation
- delete or retire duplicated parsing logic only after parity is proven

This is not tidiness. It is an invariant against parser drift, where
retrieval, indexing, and sync would each silently develop their own
pattern semantics.

### D-3. First DERIVE choice

**Choose P-1 first: `flexiarg-projection-v0` parser + persister.**

Reason:

- It is the narrowest substrate move that unlocks everything else.
- It converts clause structure from a disk-local fact into a durable
  artifact retrievable by M-2, M-4, and M-5.
- It closes the current gap where retrieval and embeddings see only a
  flattened surface even when the source pattern is richly structured.
- It can be verified without involving Haiku, the HIT widget, or live
  UI work.

### D-4. Concrete DERIVE output for P-1

The first DERIVE session should produce a file-backed projection
artifact alongside the other retrieval inputs rather than an ephemeral
in-memory shape.

Initial target:

`futon3a/resources/notions/pattern-projections.edn`

Rationale:

- `futon3a/docs/pattern-indexing.md` already treats `resources/notions`
  as the canonical home of retrieval-side derived artifacts
  (`patterns-index.tsv`, `*_pattern_embeddings.json`, etc.).
- Placing the projection there keeps the retrieval substrate co-located:
  pattern metadata, projections, and embeddings share one consumer
  directory.
- This does **not** make `futon3a` the source of truth. The `.flexiarg`
  files remain canonical; `pattern-projections.edn` is a derived cache.

### D-5. P-1 acceptance criteria (tightened)

P-1 is complete only if all of the following hold:

1. A shared parser can read heterogeneous clause shapes from at least:
   `library/futon-theory/`, `library/agent/`, `library/realtime/`,
   `library/aif/`.
2. The emitted projection preserves:
   `id`, `title`, `source-path`, `conclusion`, ordered clauses, and
   clause names exactly as authored.
3. `scripts/flexiarg_projection.clj` is no longer the sole keeper of a
   shallow semantics; it delegates to or is replaced by the shared
   projection path.
4. The projection artifact is deterministic:
   unchanged source tree -> byte-identical `pattern-projections.edn`.
5. Missing / malformed patterns fail visibly in the artifact with an
   explicit status, rather than disappearing from retrieval inputs.

### D-6. What DERIVE does not do yet

This checkpoint still does **not**:

- implement the HIT widget
- add Haiku calls
- train any reranker
- claim the pattern→family join is solved end-to-end

It only fixes the first substrate seam so later work is calibrating a
real structured object rather than a flattened surrogate.

### D-7. Canonical-parser thread up-link (2026-05-04, claude-4)

D-2's "no third independent flexiarg parser" guard is one row in a
stack-wide concern: the `.flexiarg` shape currently has three parsers
(`build_pattern_index.clj`, `flexiarg_projection.clj`,
`pattern_sync.clj`); the mission-doc shape has at least two
end-to-end parsers (`cyder/parse-mission-doc`,
`mission_control_backend/parse-mission-md`) plus several listing
helpers (`archaeology.clj`, `locus.clj`, `disposition_derive.clj`).

Tracked stack-wide in
`futon0/holes/missions/M-the-futon-stack.md` §"Cross-cutting threads"
/ CCT-1 — Canonical parser per data-object class.

P-1 owns the flexiarg row of that inventory: shipping P-1 must move
CCT-1's flexiarg row from *Doubled, unrationalised* to *Single by
construction* (or, if a v0/v1 version-stamp is the honest reading,
to *Doubled, rationalised*). Either way, the row moves; it does
not stay where it is.

### D-8. Concrete drift evidence across the three flexiarg parsers (2026-05-04, claude-4)

Reading the three parsers side by side, the doubling is not "the
same code copy-pasted." There are real semantic differences that
would silently produce divergent retrieval / ingestion / projection
results from the same source file:

| Aspect | `build_pattern_index.clj` | `pattern_sync.clj` | `flexiarg_projection.clj` |
|---|---|---|---|
| Block split | `split-arg-blocks` (line-based, splits at `@arg `) | `split-arg-blocks` (near-identical reimplementation) | none — reads whole file |
| Section parser | `parse-components` (header re `^\s*[!+]\s+([^:]+):\s*(.*)$`) | `parse-components` + `parse-section` (same header re; **also strips edge blank lines** via `trim-empty-lines`) | none — uses fixed regexes for `! conclusion:` and `^\s*\+\s*(IF\|HOWEVER\|THEN\|BECAUSE):` only |
| Section label casing | **lower-cased** (`(str/lower-case (str/trim label))`) | **original case preserved** (`(str/trim label)`) | n/a |
| Section output shape | `{:label :text}` | `{:label :slug :text}` | n/a |
| Empty-line handling in section text | only outer `str/trim` on joined lines | **edge blank lines removed** before join | n/a |
| `extract-meta` | local copy, regex `@<key>\s+(.*)` | local copy, same regex | local copy, regex `(?m)^@<key>\s+(.+?)\s*$` (anchored, non-greedy) |
| Multi-block per file | yes (each `@arg` block becomes a row) | yes (each block becomes a pattern + components) | **no — one var per file** |
| Identifier/qname | `:id` from `@arg`/`@flexiarg`/`@multiarg` | `:name` from `@arg` first then path-derived | `:qname` from `@flexiarg <ns>/<name>` header, with `library/foo/bar` rewritten to `flexiarg.foo/bar` |
| Doc-quality signal | derived from presence of `:title` | derived from summary text | derived from presence of all four IF/HOWEVER/THEN/BECAUSE clauses (`has-toulmin-slots?`) |
| Library scan roots | `["library" "holes"]` (also scans `holes/`) | walks `(:dirs opts)` plus `<root>/library/*` | path passed by caller (per-file) |

The most consequential drift: **section-label casing**. Today,
`build_pattern_index` looks up by `"conclusion"` / `"because"` /
`"then"`; `pattern_sync` would look up by `"conclusion"` /
`"BECAUSE"` / `"BHK-ARROW-SEMANTICS"` if it built such lookups —
which it doesn't, but anything calling its output would. Two
lookup tables built from the two parsers' output disagree on
which key holds the BHK section.

The next-most-consequential: **scan roots**. `build_pattern_index`
includes `holes/` (so devmaps and ad-hoc patterns under `holes/`
are indexed); `pattern_sync` does not by default; M-pattern-mining
F-1's "548 missing patterns" gap is partly a function of **which
parser was last asked**.

`flexiarg_projection` is functionally a third axis: it doesn't
read clauses at all, so anything that wants to layer
substrate-2 metadata onto a clause-aware projection has to either
use both parsers (with the drift above) or run substrate-2's logic
on its own re-parse. Today it does the third thing.

### D-9. Reconciliation decisions for P-1 (2026-05-04, claude-4)

Given D-8's drift evidence, the following decisions sharpen P-1's
acceptance criteria (D-5):

1. **Section-label representation: keep both.** The canonical
   packet's `:pattern/clauses` carries each clause as
   `{:name <original-string>, :name-key <lowercased>, :slug
   <slug>, :text <body>}`. Lookups use `:name-key`; display uses
   `:name`. Slugs become URL/id fragments. This subsumes both
   `build_pattern_index`'s lookup discipline and `pattern_sync`'s
   slug discipline without forcing either consumer to relitigate
   case-folding.

2. **Edge blank-line trimming: yes.** Adopt
   `pattern_sync`'s `trim-empty-lines` step. The section body is
   defined as the inner content with leading/trailing blank
   lines removed; this is more stable across whitespace edits to
   the source `.flexiarg` and matches authorial intent.

3. **Block split: one path.** `split-arg-blocks` becomes a
   single function in the shared namespace; both
   `build_pattern_index` and `pattern_sync` delete their copies
   and require it.

4. **`extract-meta`: one path, anchored regex.** Adopt
   `flexiarg_projection`'s anchored multi-line regex
   (`(?m)^@<key>\s+(.+?)\s*$`) — it's stricter and avoids
   matching `@key` substrings inside body prose. The other two
   parsers' looser regex is a latent bug (it would match `@title`
   appearing inside `+ context:` body text).

5. **Substrate-2 signals as derived, not parsed separately.**
   `flexiarg_projection`'s `has-toulmin-slots?` and
   library-reference-symbol collection become functions that take
   the canonical packet and project from it. `flexiarg_projection`
   then becomes a thin adapter: parse via canonical → derive
   substrate-2 shape.

6. **Scan roots: explicit, configurable, default to library +
   holes.** Today the answer differs by parser. Make the watched
   roots a parameter (default `["library" "holes"]` — matching
   `build_pattern_index`); both retrieval indexing and XTDB sync
   use the same default. M-pattern-mining F-1's coverage gap then
   has a single owner: the configured roots.

7. **`:pattern/projection-version "flexiarg-v0"` is what this
   parser emits.** Future migrations bump to `v1`; v0 records
   stay valid until explicitly rewritten. (M-10 guard.)

### D-10. Home for the canonical parser (2026-05-04, joe + claude-4)

**Decision: futon3a, in a new namespace `futon.flexiarg.projection`.**

futon3a is the running pattern-search project (`deps.edn`,
`src/futon/notions.clj`, plus the embedding pipeline and
`notions_search.py`); the canonical parser produces the artifact
the search surface consumes, so the parser belongs alongside the
consumer. futon3 is design documentation per its CLAUDE.md;
adding new running code there would extend the "no running code"
exception unnecessarily.

Migration shape:

- New file: `futon3a/src/futon/flexiarg/projection.clj` —
  exports `parse-file`, `parse-block`, `split-arg-blocks`,
  `parse-components`, `extract-meta`, plus a `-main` for
  whole-tree projection that emits `pattern-projections.edn`.
- `futon3/scripts/build_pattern_index.clj`,
  `futon3/scripts/pattern_sync.clj`, and
  `futon3/scripts/flexiarg_projection.clj` are retained as thin
  wrappers that require the futon3a namespace and project from
  its canonical packet; their internal `split-arg-blocks` /
  `parse-components` / `extract-meta` copies are deleted.
- `futon3a/scripts/index_patterns.sh` already invokes
  `clj -M -m scripts.build-pattern-index` from inside
  `$FUTON3_ROOT`; that invocation flips to
  `clj -M -m futon.flexiarg.projection.build` from inside
  `$FUTON3A_ROOT` (with `--source-roots <futon3-paths>`), since
  the build now lives where the wrapper does.

### D-11. Updated P-1 acceptance criteria (supersedes D-5)

P-1 is complete only if all of the following hold:

1. **Single shared parser exists** at
   `futon3a/src/futon/flexiarg/projection.clj` (per D-10), and
   reads heterogeneous clause shapes from at least:
   `library/futon-theory/`, `library/agent/`, `library/realtime/`,
   `library/aif/`, plus at least one `.flexiarg` under `holes/`.
2. **The emitted canonical packet** preserves: `:pattern/id`,
   `:pattern/title`, `:pattern/source-path`, `:pattern/sigils`,
   `:pattern/keywords`, `:pattern/references`,
   `:pattern/conclusion`, ordered `:pattern/clauses` (each carrying
   `:name`, `:name-key`, `:slug`, `:text` per D-9.1, with edge
   blank lines trimmed per D-9.2).
3. **`scripts/build_pattern_index.clj`,
   `scripts/pattern_sync.clj`, and
   `scripts/flexiarg_projection.clj`** all delegate to the shared
   parser. None retains its own `split-arg-blocks`,
   `parse-components`, or `extract-meta`. Substrate-2 signals
   (`has-toulmin-slots?`, library-ref symbols) are derivations
   over the canonical packet, not parallel parses (D-9.5).
4. **Anchored `extract-meta` regex** is in force everywhere
   (D-9.4), and a regression test exists for the
   `@title` / `@key`-inside-body-prose case the looser regex
   would have matched.
5. **Scan roots are a parameter** (D-9.6) with default
   `["library" "holes"]`; the same default flows through retrieval
   indexing and XTDB sync.
6. **Projection artifact is deterministic**: unchanged source
   tree → byte-identical `pattern-projections.edn`.
7. **Missing / malformed patterns fail visibly** in the artifact
   with `:pattern/projection-version "missing"` or an explicit
   error status, rather than disappearing from retrieval inputs.
8. **CCT-1's flexiarg row** in
   `futon0/holes/missions/M-the-futon-stack.md` is updated from
   *Doubled, unrationalised* to *Single by construction* (per
   D-7).
