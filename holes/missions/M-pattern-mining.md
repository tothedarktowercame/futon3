# Mission: Pattern Mining

**Status:** MAP (entered 2026-04-27, IDENTIFY 2026-04-26).
**Substrate verdict (2026-04-27):** the existing futon3a MiniLM-cosine
pipeline is **artifact, not signal** — confirmed via the activation
browser (P-5a). Decoration on top of A→B turns rather than the
diagnostic we wanted. Mission's centre of gravity has shifted from
"fix the biases" to "specify the goal-state and replace."
**Owner:** Joe

## IFR (Ideal Final Result)

> **Each turn A→B is coded as an activation of a pattern that is
> well-specified, repeatable, and of demonstrable value in the
> codebase; this means we have a good understanding of our own
> workflow and it has a high chance of being replicable, even under
> full automation.**

This unpacks into five properties the substrate must satisfy. The
current pipeline fails on every one of them, which is why the
activation browser produced an artifact:

| IFR property        | What it requires                                              | Current pipeline                                |
|---------------------|---------------------------------------------------------------|-------------------------------------------------|
| **Coverage**        | every turn produces a record, including "no pattern matched" | F-4 silent-fail; turn → no record               |
| **Well-specified**  | typed `{context, tension, move}` slots on each pattern        | only `{id, title, hotwords}`                    |
| **Repeatable**      | same input → same output, day to day, agent to agent          | MiniLM cosine top-3 is brittle near boundaries  |
| **Demonstrable value** | the pattern's claim is verifiable against codebase state    | no claim, just a similarity score               |
| **Replicable under automation** | self-explaining diagnostic an agent can run unsupervised | opaque retrieval; no provenance for *why*       |

The architectural implication: a pattern is `context → tension →
ameliorating-move`, where **"context" means "a region of the futon
stack codebase"** — not an abstract scenario. Therefore the codebase
itself needs a geometric projection (hypergraph and/or embedding),
and patterns live as typed regions / subgraphs / records inside
that projection. Retrieval becomes "where does this turn land in
the projection, and which pattern-shaped neighbourhoods is it
inside?" — diagnostic, not similarity.

## Path verdict: hypergraph-first

Of the three paths considered (hypergraph-first / codebase-embedding-
first / layered), hypergraph-first dominates against the IFR:

| Property         | Hypergraph-first | Embedding-first        |
|------------------|------------------|------------------------|
| Coverage         | ✓ trivially      | ✓ trivially            |
| Well-specified   | ✓ typed slots are the whole point | △ requires extra slot model |
| Repeatable       | ✓ subgraph-match is deterministic | ✗ cosine is brittle |
| Demonstrable value | ✓ claim is structural, checkable | ✗ no structural claim to check |
| Replicable / explainable | ✓ provenance is constructive | ✗ "MiniLM-said-so" is not a reason |

Embedding-first does worse on three of the five — exactly the
properties the activation-browser probe just exposed as missing.
**Layered (hypergraph + embedding for novelty)** is reserved as
phase-2 if subgraph-match's recall proves too narrow once the
typed-slot work is done.

This redirects the mission's deferred work:

- **P-5b** (synthetic catchment) is now **out-of-scope**. The verdict
  the catchment diagnostic was meant to support has been reached
  with stronger evidence by reading real activations.
- **P-8** (auto-ingest pipeline for the existing embedding) is
  **deferred-pending-replacement**, not just sequenced. Pumping
  more files into a substrate we're retiring would amplify the
  artifact. The auto-ingest *invariant* (F-6) survives; it lands
  on whatever replaces this pipeline.
- **P-6** (MiniLM/GloVe/FastText comparison) demoted from a probe
  to a **disconfirmation check** before retirement: if a different
  embedding looks materially saner via the activation browser, the
  pipeline survives with a model swap and we revisit. Otherwise
  the embedding-class verdict stands.
- **P-7** (A-side prior/posterior) **inherits the new substrate**.
  The prior/posterior framing is correct; what gets updated is the
  hypergraph's typed records, not embedding scores.

## Lineage

The IFR is not a from-scratch invention; four partial projections
have been growing toward it without a single seam where they meet.
The pattern-application diagnostic is that seam:

- **futon5a stack stereolithography** — invariant axes, family /
  motif structure, strategic-closure as first-class. Already a
  geometric projection of the codebase along the strategic axis.
- **B→A typed graph** in
  `futon5a/holes/excursions/E-btoa-candidate-queue-reweighting-v0.md`
  — `A-turn → family → motif → Strategic SORRY` with prior →
  posterior updates. The same problem in different clothes.
- **Arxana hypergraph + futon1a's hyperedge API** — the substrate
  that was always supposed to host this; never wired for patterns.
- **futon3a embeddings** — the *one* projection unsuited to the
  IFR (English token statistics, not codebase geometry); the
  decoration this mission's activation browser just unmasked.

## Sibling mission

Spawned 2026-04-27:
**`holes/missions/M-pattern-application-diagnostic.md`** carries
the IFR-direction work — typed pattern shape, hypergraph-first
substrate, BHK arrow framing, and the **prototype-1 framing**
(self-application to the futon stack as instance 1 of a series
that anticipates prototype 2 = math / arxiv-as-hypergraph,
prototype N = other typed knowledge corpora). Domain-agnostic
abstractions in that mission's schema are a hard scope
constraint, not a nice-to-have.

This mission (M-pattern-mining) keeps the **retire-the-old**
scope cleanly: triage, disconfirmation check, consumer unwiring,
archive. Exit when no surface still depends on the futon3a
context-retrieval substrate.
**Cross-refs:**
- `futon5a/holes/missions/M-trip-journal.md` — installs the **B→A**
  register (operator's turn → pattern). M-pattern-mining is the
  precondition for it: B→A learning depends on the A→B side being
  honest. If we can't mine patterns from agent turns reliably, we
  can't learn from the operator's hinge-crossings either.
- `futon5a/holes/excursions/E-btoa-candidate-queue-reweighting-v0.md`
  — the live B→A design (A-turn → family → motif → Strategic SORRY,
  with session-end **prior → posterior** updates). M-pattern-mining
  is the **mirror direction** running on the same substrate; what
  follows treats that mirror explicitly.
- `futon5a/holes/excursions/E-candidate-queue-xtdb-shape-v0.md` —
  hybrid XTDB shape (canonical per-item docs + derived run +
  derived snapshot) being designed for B→A. The A-side may want to
  adopt the same triple, with its own item type
  (`pattern-activation/*`).
- `futon5a/holes/excursions/E-btoa-evidence-sample-mapping-v0.md` —
  hand-labeled A-side → family mappings; ground truth for whatever
  catchment / accuracy diagnostic the A-side ends up running.
- `futon0/holes/excursions/E-current-usage-report.md` — sibling
  excursion that established per-session token visibility. Same
  surface (Arxana Browser headlines, Stack HUD widgets) is now used
  to display pattern-mining tallies.

## Motivation

Across the futon stack we treat pattern retrieval as a working
substrate: futon3a embeddings, retrieval-tagged evidence, the
patterns widget in Stack HUD, and the implicit assumption that "the
patterns being mined" is a meaningful low-cost signal of what an
agent's turn is actually about. M-trip-journal extends this in the
other direction (operator-side B→A mining), and M-learning-loop
depends on the same substrate being well-formed.

What we discovered while debugging the patterns widget on
2026-04-26 is that the substrate is not, in fact, well-formed yet.
We have only just started **persisting** pattern-mining evidence
reliably — Claude turns since ~2026-04-25, Codex turns since
2026-04-26 14:00 (this turn's wiring fix at
`futon3c/dev/futon3c/dev.clj` `make-codex-invoke-fn` after
`emit-invoke-evidence! "invoke-complete"`). Earlier retrievals were
ephemeral: notifications fired, the *context* blackboard updated,
but the evidence record was either not written or only partially
written. So **most of what we believe about pattern-mining behaviour
is reconstructed from a few days of data, with one transport silently
absent until just now.**

Layered on top of that recency problem are at least three
**structural** problems with the retrieval pipeline itself, which
mean even a long history of retrievals would not be safe to interpret
naively. They are listed below.

## Sorry being identified

**We are emitting pattern-retrieval evidence as if it were a clean
sensor reading on "what patterns are firing in the work," when in
fact it is a noisy, biased, partially-collated reading whose biases
we have not characterised.** Until those biases are named and
either fixed or compensated, the downstream uses (Stack HUD widget,
M-trip-journal's B→A learning loop, any decision-rule that says
"this pattern is hot, look at it") risk amplifying artifacts.

Joe's framing on 2026-04-26: "M-trip-report is about mining the
opposite kind of relation to patterns — and if we can't even mine
patterns well, that's going to be hard to do." The asymmetry is
real: A→B (agent-turn → pattern) is the simpler direction and is
already wired. If A→B is half-broken we have no platform to
build the harder B→A register on.

## Findings (2026-04-26 IDENTIFY pass)

### F-1: searchable corpus is half the catalog

```
patterns-index.tsv:                  955 entries
minilm_pattern_embeddings.json:      407 entries
```

`notions_search.py` matches every query against ~43% of the
catalog. Patterns not in the embeddings file can never appear in a
top-3, regardless of relevance. This is invisible at the retrieval
surface — patterns just look like they don't exist.

The 548 missing patterns are not random; their selection (whatever
selected the 407) is itself a bias we don't know yet. Re-embedding
the missing 548 would change the dominance landscape on its own,
before any model or query change.

### F-2: ID collisions in the embeddings file

`f2/p16` resolves to two different patterns in the embeddings JSON:
"Lobby Bus & MUSN Intake" and "Wisdom Trail Harvest & Pattern
Seeding". Whichever wins the index lookup wins the score; the
evidence record shows the same `id` twice with no way to
disambiguate. There may be other collisions; we have not done a
full audit. A duplicate-id sweep is a one-shot offline computation.

### F-3: producer-side coverage was incomplete until 2026-04-26

Until today's wiring fix, only Claude turns triggered
`context-retrieval!`. Codex turns went through
`make-codex-invoke-fn` which had no hook. So our entire prior
history of pattern-mining is **Claude-only** — heavily biased
toward whatever Claude conversations covered. Any tally that
reaches back more than a day is selection-biased on transport.

The fix is in (`make-codex-invoke-fn` now mirrors the Claude path,
fire-and-forget `(future (context-retrieval! …))` after
`emit-invoke-evidence! "invoke-complete"` when `ok?`). Codex turn
on 2026-04-26 13:58:12Z confirmed the wiring lands evidence
end-to-end (3 patterns retrieved, `evidence/author = codex-3`).
Other invoke paths (`make-irc-codex-invoke-fn`,
`make-tickle-invoke-fn`, any other invoke factory) have not been
audited; absence elsewhere may still be silently distorting tallies.

### F-4: silent-fail on empty futon3a result

`context-retrieval!` only persists evidence when
`(seq results)` is non-empty (`futon3c/dev/futon3c/dev.clj:774`).
Turns whose query produced zero futon3a hits leave no record at
all — neither "I tried", nor "nothing matched". This is fine as a
storage discipline but it means **failure cases are invisible to
any later analysis.** A pattern that only fires on certain query
shapes looks more dominant than it is, because the queries it
didn't match are not in the denominator.

A minimal fix: emit an evidence entry with `body.event =
"context-retrieval-empty"` (or `:results []`) for the empty case.
Costs nothing extra at retrieval time; gives us a denominator.

### F-6: searchable corpus drifts out of sync with on-disk pattern library

The 548-pattern coverage gap (F-1) is one snapshot of a deeper
invariant violation: **new patterns added under
`futon3/library/<subdir>/` (and other identified roots like
`futon3/holes/strategy/`) should automatically become available to
retrieval, but currently do not.** Adding a new pattern means
authoring a `.flexiarg` file, then manually re-running an embedding
build. Most authoring sessions skip the rebuild, so the searchable
corpus monotonically falls behind the canonical corpus.

This is a real invariant violation, not just an inconvenience: the
catalog is the canonical surface (humans browse it, agents reference
it), but retrieval scores against a stale subset. The longer the
gap, the more retrieval bias accumulates toward whatever was around
at last rebuild — exactly the broad-catchment-meta-pattern dominance
seen in F-5 may partly be an artifact of the freshest patterns
never reaching the embeddings file.

The fix is a **debounced, content-hashed, per-file incremental
embed pipeline** — not a naïve file-watcher, which would re-run
MiniLM on every keystroke. Specifically:
- watch the configured roots (initially
  `futon3/library/<subdir>/` and `futon3/holes/strategy/`) for
  `.flexiarg` (and similar) writes
- debounce per-file (e.g. 5s after last write)
- hash file contents; only re-embed if hash changed
- update only the affected entries in the embeddings JSON; do not
  rebuild the whole file
- emit an evidence record per ingestion (`event "pattern-ingest"`
  with file, hash, action, ms) so the auto-pipeline is itself
  observable

See P-8 below.

### F-5: top hits are mostly meta-patterns about pattern infrastructure

Tally over the available evidence (Claude only until today, n ≈ 5
days):

```
7  f3/p4    Trail & Proof-State Journal
6  f3/p3    Applicability Engine & Check DSL
6  f2/p16   (collision: Lobby Bus | Wisdom Trail Harvest)
6  agent/intent-handshake-is-binding
5  f3/p6    Pattern Creation Workbench
5  f2/p6
5  ants/baseline-cyber-ant
```

These are mostly meta-patterns about pattern infrastructure
itself. That's plausibly true signal (we've spent days talking
about pattern infrastructure) but it's also exactly what a
broad-catchment abstraction looks like when fed generic
software-talk. We cannot disambiguate "real attention" from "wide
catchment" without a catchment diagnostic (see Open question 1).

## Open questions

1. **Catchment-area diagnostic.** For each embedded pattern, run
   `notions_search` against a corpus of *deliberately unrelated*
   queries (drawn from daily-scan frames, mission docs, even random
   external paragraphs) and tally how often each pattern appears in
   the top-3. Patterns that score in the top-3 of unrelated
   queries have a wide catchment regardless of relevance; patterns
   that only fire on topical queries are sharper signals. One-shot
   offline computation; doesn't touch the live pipeline.

2. **What's the right embedding for futon-internal text?** MiniLM
   was trained on general English. A lot of futon prose is highly
   self-referential (pattern names, sigils, futon2/3/5/6
   abbreviations). Some of the broad-catchment behaviour may be
   "MiniLM doesn't really see what's distinct about these
   sentences." Worth comparing MiniLM, GloVe, FastText (all three
   embeddings already exist as files in `notions/`) on the same
   queries to see whether the dominance landscape shifts.

3. **Should we re-embed the missing 548 patterns first, or is
   coverage selection itself meaningful?** If the 407 were chosen
   for a reason (e.g., they had complete sources at embed time),
   simply re-embedding the rest with stale source could be worse.
   Need to find the embed-time provenance.

4. **What does B→A mining inherit?** M-trip-journal's hinge-log
   intends to learn from operator-side turns. If the A-side
   pipeline systematically promotes meta-patterns, the operator
   side will likely inherit that selection: hinge-crossings will
   be tagged with whichever patterns the underlying retrieval
   surfaces, and the learning loop will reinforce the same
   distortions. Need to either (a) fix A-side first, (b) make B→A
   robust to A-side bias, or (c) accept the bias as part of the
   stack's current voice.

5. **What use cases require a clean substrate vs. tolerate
   noise?** The Stack HUD widget showing "patterns firing right
   now" is fine with noise; a triage rule that says "this pattern
   is hot, escalate" is not. Mapping consumers to noise tolerance
   tells us the priority of fixing each finding.

## Out-of-scope (for this mission)

- Pattern *authoring* — re-writing existing patterns to be sharper
  or more specific. Belongs to whichever pattern-cluster's owner
  decides the current pattern is too broad.
- Replacing futon3a — this mission characterises and unblocks the
  current substrate, not a different one.
- Building the B→A side of the queue. That's M-trip-journal /
  E-btoa-candidate-queue-reweighting-v0. The two sides share
  substrate concerns and should converge on representation
  decisions, but each side owns its own producer+consumer wiring.

---

## MAP (2026-04-27)

### M-1. The pipeline as a typed graph

Drawing this side parallel to the B→A graph in
`E-btoa-candidate-queue-reweighting-v0.md`:

**Nodes.**
- `B-turn` — agent assistant message (Claude or Codex completion).
- `query-snippet` — `(user-msg + response-preview)` slice that
  goes into futon3a (currently `subs … 200` chars each, see
  `dev.clj:769`).
- `pattern` — futon3a pattern record `{:id :title :score}`.
- `evidence-record` — the `event "context-retrieval"` entry that
  lands in futon1a.
- `consumer` — Stack HUD patterns widget; future war-machine /
  arxana-browser surfaces; downstream B→A reweighting that tags
  A-side turns with families.

**Edges.**
- `B-turn → query-snippet` — `extract-user-message` + response
  preview; trivially deterministic, no scoring.
- `query-snippet → pattern[]` — MiniLM cosine top-3 against the
  embeddings JSON. **This is the only learned edge.**
- `pattern[] → evidence-record` — `emit-context-evidence!`
  (`dev.clj:716`); only fires when `(seq results)` (F-4 silent
  fail).
- `evidence-record → consumer` — read via
  `GET /api/alpha/evidence?since=…&limit=…` filtered by
  `body.event == "context-retrieval"`.

The only edge that contributes uncertainty / bias is the second
one (`query-snippet → pattern[]`). All other edges are mechanical.
**Therefore the entire mission collapses to characterising and
fixing that one edge plus the producer-side wiring around it.**

### M-2. Producer surfaces inventory

| Invoke factory | Calls `context-retrieval!`? | Status |
|---|---|---|
| `make-claude-invoke-fn` (`dev.clj:3168`) | yes (`dev.clj:3436`) | OK |
| `make-codex-invoke-fn` (`dev.clj:3662`) | yes (`dev.clj:4004`, fix on 2026-04-26) | OK |
| `make-irc-codex-invoke-fn` (`dev.clj:4053+`) | unknown — not audited | **AUDIT** |
| `make-tickle-invoke-fn` (declared `dev.clj:226`) | unknown — not audited | **AUDIT** |
| Peripheral-internal invokes (mission_backend, proof_backend, …) | n/a (different surface) | Out-of-scope unless they consume patterns |

The 2026-04-26 fix demonstrated the cost of an unaudited gap:
Codex turns produced **zero** pattern evidence for the entire
prior history, and we did not notice until the Stack HUD widget
went looking. Until every invoke factory is checked, every tally
is missing-data-biased on transport.

### M-3. Searchable-corpus inventory

Snapshot 2026-04-26:

| File | Count | Notes |
|---|---|---|
| `futon3a/resources/notions/patterns-index.tsv` | 955 | full catalog |
| `…/minilm_pattern_embeddings.json` | 407 | what's actually searchable |
| `…/glove_pattern_embeddings.json` | ? | not yet audited |
| `…/fasttext_pattern_embeddings.json` | ? | not yet audited |

The 548 patterns absent from MiniLM embeddings are **invisible**
to retrieval. Selection-bias of the visible 407 is unknown.

ID collision audit: `f2/p16` collides between "Lobby Bus & MUSN
Intake" and "Wisdom Trail Harvest & Pattern Seeding." Full
collision count not yet computed.

### M-4. Consumer surfaces inventory

| Surface | Reads | Status |
|---|---|---|
| Stack HUD patterns widget (`futon0/contrib/stack-hud-widgets.el:478`) | filter `event=="context-retrieval"`, top-N by id-count | Working since fix |
| Arxana Browser → patterns headline (`futon4/dev/arxana-browser-core.el`) | usage-headline pattern, not yet patterns-headline | Open |
| War Machine → invariants hexagon (codex-5 work-in-progress) | will read `/api/alpha/aif-stack/live` payload + ranked queue | Coming via B→A side |
| Future B→A learner (E-btoa-candidate-queue-reweighting-v0) | will use A-turn → family mappings derived **from** these same evidence records | Direct dependency |

The B→A side will consume A-side evidence records. **Whatever
biases live in the producer, the B→A learner will inherit and
amplify** unless those biases are named first.

### M-5. The prior/posterior frame applies symmetrically

E-btoa-candidate-queue-reweighting-v0 names the right shape for
the B→A side: each invariant carries a **prior**; the session
contributes derived evidence; the session writes a **posterior**
at boundary. The A-side currently has no analogue — every
retrieval is a fresh top-3 with no memory.

Two adoption options to consider in DERIVE:

1. **A-side has its own prior / posterior.** Each pattern carries
   `{:pattern/prior-activation :pattern/session-delta
   :pattern/posterior-activation}`. Session-end aggregation tells
   you which patterns *increased* in attention this session, not
   just which fired most. Compresses the F-5 finding (broad
   meta-patterns dominate) by giving us a "is this pattern
   *unusually* active right now?" signal.
2. **Co-conditional priors.** A-side prior conditioned on
   B-side family ranking and vice versa. More expressive, harder
   to validate; phase-2 candidate. Out-of-scope for v0.

If we adopt (1), the XTDB shape from
`E-candidate-queue-xtdb-shape-v0.md` ports almost directly:
`pattern-activation/*` (canonical per-pattern doc),
`pattern-activation-run/*` (per-session run with evidence ids),
`pattern-activation-snapshot/*` (rendered top-N for a surface).
This is recommended for symmetry with B→A.

### M-6. Bounded probes (DERIVE candidates, not yet committed)

Each probe answers exactly one of the open questions and is
sized to one session. Listed by priority:

**P-1. Invoke-factory audit** (resolves F-3 fully).
- Read `make-irc-codex-invoke-fn` and `make-tickle-invoke-fn`
  for `context-retrieval!` calls.
- Add the fire-and-forget block where missing, mirroring
  `dev.clj:4004`.
- Verify with one-turn test per surface; expect a notification +
  evidence record per successful turn.
- Exit: every invoke factory either calls `context-retrieval!`
  on success or has an explicit `#_ TODO` documenting why not.

**P-2. Embeddings-coverage audit** (resolves F-1).
- Diff the 955 patterns-index ids against the 407 embedding ids.
- For each missing pattern, classify by reason: (a) source file
  exists but wasn't embedded → re-embed; (b) source file moved/
  deleted → either drop from index or restore; (c) intentional
  exclusion → document.
- Exit: a single artifact at
  `futon3a/holes/embedding-coverage-audit-2026-04.md` listing
  every missing id with classification + recommended action.

**P-3. ID-collision sweep** (resolves F-2).
- One-shot script over `minilm_pattern_embeddings.json`:
  `(group-by :id)` and report any group with >1 entry.
- For each collision, decide which entry is canonical; rename
  the other or merge.
- Exit: collision count is zero; one-line note recording the
  finding for posterity.

**P-4. Empty-result evidence emission** (resolves F-4).
- In `context-retrieval!`, replace `(when (seq results) …)` with
  a branch that emits a record either way; for empty case,
  `body.event = "context-retrieval-empty"` and `body.results []`.
- Update Stack HUD widget filter to count empties as "denominator
  only," not as pattern hits.
- Exit: every successful turn produces exactly one
  retrieval-related evidence entry; coverage of the producer is
  visible by counting `event` types.

**P-5a. Real-evidence Activation browser** (resolves F-5 / Q1
without synthetic queries; supersedes the original P-5 as the
first probe in this family).
- Add a JSON projection endpoint
  `GET /api/alpha/patterns/activation?since=…` to futon1a that
  aggregates `event "context-retrieval"` evidence by pattern id
  and returns `{patterns: [{id, title, count, avg-score,
  last-fired, activations: [{at, session-id, score, query,
  evidence-id, agent-id}, …]}, …]}`.
- Add Arxana Browser (※) → Patterns → Activation as a new file
  `futon4/dev/arxana-browser-pattern-activation.el`. List view
  ranks patterns by activation count; RET drills into per-pattern
  detail showing every retrieval with query snippet + session +
  score; second RET on a row opens the underlying turn via
  `evidence-id`.
- The endpoint is the **single source of truth**: Joe's Emacs
  view, Codex, and agents-via-Drawbridge all consume the same
  shape. This satisfies the "drill-in must work for sense-making
  by agents, not just visually for Joe" constraint.
- Exit: Joe can answer "is f3/p4 hot because relevant or because
  broad-catchment?" by reading actual queries; same answer is
  also reachable by `curl /api/alpha/patterns/activation` from any
  agent surface.

**P-5b. Synthetic catchment diagnostic** (resolves remaining Q1
quantification once P-5a has surfaced obviously-broad patterns;
deferred until P-5a tells us whether the synthetic step is still
needed).
- Same as the original P-5: control queries from unrelated
  sources, top-3 tally, broad-catchment flag.
- Exit: `futon3a/holes/catchment-2026-04.md` with per-pattern
  table; flag emitted as `body.broad-catchment? true` on
  retrievals where any of the top-3 is flagged.

**P-6. Embedding-comparison spot-check** (resolves Q2).
- Pick 20 representative recent queries from evidence.
- Run all three (MiniLM, GloVe, FastText) on each.
- Score: do they agree on top-3? where they disagree, are the
  GloVe / FastText choices more or less "broad-catchment"?
- Exit: short note recommending whether to keep MiniLM, switch,
  or ensemble.

**P-8. Auto-ingest / auto-reembed pipeline** (resolves F-6, can
proceed independently of P-5/P-6 once trust in the substrate is
established).
- Watcher over configured roots (initial:
  `futon3/library/<subdir>/`, `futon3/holes/strategy/`).
- Debounce 5s per file after last write; content-hash check;
  per-file incremental MiniLM embed; in-place JSON update.
- Emit `event "pattern-ingest"` evidence per write (with file,
  hash, action, latency-ms).
- Configurable roots and debounce via env / defcustom; ship
  with the discipline that ingestion CAN be paused
  (`pattern-ingest-pause-until <iso>`) so a heavy library
  rewrite session doesn't thrash.
- Exit: a new `.flexiarg` written under any watched root becomes
  retrievable within ≤30s without manual rebuild; ingestion
  evidence visible in Stack HUD widget; CPU usage during normal
  authoring negligible (target <2% mean, <20% p99).
- **Sequencing note:** do not ship before P-5a + P-2 are done.
  Auto-ingest amplifies whatever bias the substrate has; need to
  see the activation behaviour first and have the
  embeddings-coverage audit done so the pipeline isn't pumping
  into a known-broken corpus.

**P-7. A-side prior/posterior shape spike** (resolves M-5,
depends on P-3 + P-4).
- Mirror E-candidate-queue-xtdb-shape-v0 for `pattern-activation`.
- Implement session-end posterior update: read all
  context-retrieval entries with `evidence/session-id` matching
  the closing session, apply `prior + delta → posterior`.
- One snapshot doc + one run doc per session.
- Exit: a Stack HUD column showing "delta this session" alongside
  raw count.

### M-7. Sequencing

P-1, P-2, P-3 are independent and parallelisable (one Codex
session each, ~2h human-review).

P-4 depends on nothing; ship anytime.

**P-5a (real-evidence Activation browser) is the recommended
entry point** — it builds trust in / suspicion of the substrate
using data we already have, and exposes the projection as JSON
so agents can drill in alongside Joe.

P-5b (synthetic catchment) depends on P-3 and is reconsidered
after P-5a.

P-6 depends on nothing; can run in parallel with P-5a.

P-7 depends on P-3 and P-4 (canonical ids + denominator); should
land **after** the B→A `candidate-queue-*` XTDB shape is real
in code, so we can adopt the same ergonomics rather than
inventing a parallel one.

P-8 (auto-ingest pipeline) depends on P-5a + P-2; do not
amplify a substrate we don't yet trust.

---

## Notes

- Evidence-mining producer wiring fix on 2026-04-26 (Codex side)
  was a sibling-bug of this mission's larger sorry — small enough
  to bundle inline, large enough to log: that single missing call
  hid Codex's pattern-firing behaviour entirely until today.
- Once the substrate biases are named, M-trip-journal's
  hinge-evaluation step can pick up: tagging hinge-crossings
  becomes a defensible signal rather than a downstream amplifier
  of upstream bias.
- Codex-5's work on reading evidence into a ranked queue
  (`E-btoa-candidate-queue-reweighting-v0`,
  `E-candidate-queue-xtdb-shape-v0`) is moving fast. The A-side
  should not invent a parallel shape — we adopt theirs once it
  lands in code, which is why P-7 is sequenced last.

### Checkpoint MAP-entry — 2026-04-27

**What was done:** restructured the mission from IDENTIFY
findings (F-1..F-5) into a typed pipeline graph (M-1), inventoried
producers (M-2), corpus (M-3), consumers (M-4), and named the
prior/posterior framing as the symmetric hinge between A→B and
B→A (M-5). Listed seven bounded probes P-1..P-7 with sequencing.

**Test state:** N/A (no code changes in this checkpoint; mission
doc only).

**Next:** pick one of P-1..P-4 to run in DERIVE; suggest P-1
(invoke-factory audit) since it is the lowest-risk and closes the
last "are we even sampling everything?" gap before any
catchment / coverage analysis is meaningful.
