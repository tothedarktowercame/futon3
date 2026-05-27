# Mission: Pattern Ingest — XTDB as the Authoritative Record

**Status:** IDENTIFY (2026-05-26)
**Owner:** Joe
**Drafted by:** claude (live trace + Joe's framing in conversation 2026-05-26).

**Parent / Siblings:**
- `futon3/holes/missions/M-live-geometric-stack.md` — substrate-2 owner.
  This mission upgrades the **flexiarg projector** that feeds substrate-2;
  the live-geometric-stack mission established the substrate, this one
  thickens what it carries for pattern files specifically.
- `futon3/holes/missions/M-pattern-application-diagnostic.md` — consumer.
  Pattern-application records are the downstream artefact that benefits
  most from richer pattern props in substrate-2 (you cannot meaningfully
  evaluate an application against a pattern whose IF/HOWEVER/THEN aren't
  in the substrate).
- `futon3c/src/futon3c/watcher/projections/flexiarg.clj` — the v0
  projector this mission rewrites toward v1.
- `futon3/scripts/build_pattern_index.clj` — the sigil-validation step;
  this mission flips its disposition from reject to assign-and-pend.

**Cross-refs:**
- `futon3c/README-multi-watcher.md` — the daemon that calls the projector
  per file change.
- `library/orchestration/` (new namespace, 2026-05-25) — the pilot
  patterns this mission uses to illustrate why richer projection matters.

## IFR

> **The flexiarg file system is a checked-out projection of an XTDB
> record that is authoritative for pattern semantics — sigils, slots,
> rationale, lineage — and `futon3c` ingest enforces and enriches that
> projection rather than degrading it on the way in.**

Five properties unpacked: **shape-checked** (Toulmin/sigil shapes are
preconditions, not afterthoughts), **content-projected** (title +
conclusion + IF/HOWEVER/THEN/BECAUSE land on the vertex as props),
**sigil-assigned** (invalid placeholders get canonicalised and the
file is rewritten; they don't silently degrade to `unknown/unknown`),
**lineage-linked** (newly-ingested patterns connect to the Evidence
where they were first proposed), **substrate-authoritative** (XTDB
answers richer queries than the TSV; the TSV becomes a derived view
rather than the canonical reference for sigil canonicalisation).

## Background — what surfaced this mission

Two new flexiargs landed in `library/orchestration/` on 2026-05-25:
`state-in-substrate-deltas-in-messages` and `rule-evolves-from-its-deferrals`,
plus an `INDEX.md`. The agent that authored them left sigils as
`[📁/?]` and `[🌂/?]` — first-segment emoji picked for visual rhyme,
second segment intentionally left as `?` since "it should be assigned
canonically against `resources/sigils/patterns-index.tsv` rather than
guessed."

A live trace through the pipeline (multi-watcher → file-ingest →
substrate-2 → MiniLM → patterns-index.tsv → notions_search.py →
War Machine candidates) revealed:

| Stage | Status | Observation |
|---|---|---|
| multi-watcher cache | ✓ | Both files ingested under label `futon3-d`. |
| `INDEX.md` | ✗ | Excluded by the `.md` mission-only filter (`multi.clj:188`). |
| substrate-2 vertex | ✓ existence, ✗ richness | Bare `code/v05/var` with `:var/kind "flexiarg" :var/has-doc true`. **No `@sigils`, no `@title`, no `! conclusion:`, no IF/HOWEVER/THEN/BECAUSE clauses are projected.** v0 projector reads them only to set `has-doc`. |
| MiniLM embeddings | ✓ | Refreshed daily 04:30 UTC by `futon3a/scripts/index_patterns.sh`; new patterns have 384-dim vectors in both `~/code/futon3/resources/embeddings/` and `~/code/futon3a/resources/notions/`. **Independent of substrate-2** — walks `library/` directly. |
| `patterns-index.tsv` | ✓ row, ✗ sigil columns empty | Builder validates `[emoji/hanzi]` against `holes/tokipona.org`; `📁`, `🌂`, and `?` aren't in the allowlist; both got logged as "Ignored 144 invalid sigil tokens (not in allowlist)" and the row landed with `unknown/unknown` in the rationale. **144 other patterns sit in this same hole today** — pre-existing condition. |
| `notions_search.py` retrieval | ✓ | Both patterns rank well on shaped queries: rule-evolves comes back rank 1 (0.6396) on a defer-classifier query; state-in-substrate rank 2 (0.5196) on a coordination query. |
| War Machine candidate window | ⚠ pending | `futon2.aif.pattern-registry` consumes `context-retrieval` evidence emitted by agent turns — it doesn't enumerate the library. The new patterns are available *to retrieval*, but won't surface in WM's candidate set until a real A→B turn has retrieved them in top-k. |

The substrate-2 thinness is the load-bearing finding. The other stages
work; the projector is the weak link, and a richer projection (a)
unlocks cross-content inference today, (b) makes the TSV a derived
view rather than the authority on sigils, (c) lets WM and future
consumers query the substrate directly for IF/THEN-shaped lookups
instead of round-tripping through the file system.

## The legacy framing Joe wants restored

> *Legacy ingest had all of those, checked the shape, rejected any
> patterns that didn't match; and that meant that XTDB was the
> authoritative record and the file system was a "checked out"
> version from the point of view of Arxana.*

This re-frames the projector's job. The current v0 stance is "honest:
emit only what's structurally trivial; let downstream consumers re-parse."
The legacy stance is "XTDB is the canonical Arxana record; the file
system is its working tree." Under the legacy stance:

- Pattern content (sigils, slots, rationale, lineage) lives **on the
  vertex** in substrate-2.
- The flexiarg file is a checkout-like rendering of that vertex, not
  an independent source of truth.
- Linking and inference can be done as substrate queries (e.g. "all
  flexiargs with sigil emoji X and an IF-clause mentioning Y") without
  re-walking the corpus.
- Rejection-on-shape becomes a meaningful gate: a malformed flexiarg
  doesn't make it into the authoritative record, period.

This mission does *not* require flipping to legacy file-system-as-checkout
in one shot. It asks: project enough onto the vertex that XTDB *could*
serve as the authoritative record, and demonstrate the inference moves
that become cheap once it does.

## Scope IN

1. **v1 flexiarg projector** — emit `@title`, `! conclusion:`, the named
   slot clauses (IF, HOWEVER, THEN, BECAUSE, NEXT-STEPS, DOES-NOT-APPLY,
   INSTANCES, RELATED — accept the orchestration-namespace extension as
   data-driven), `@audience`, `@tone`, `@style`, and `@sigils` as props
   on `code/v05/var`. Also emit a `code/v05/pattern-slot` edge per slot
   so slots are individually queryable (slot type, clause text,
   position). Shape-failure → log + skip (don't ingest a partial
   pattern).

2. **Sigil assign-and-commit (residual pend)** —
   `build_pattern_index.clj`'s current reject-on-invalid becomes a
   three-step verified canonicalisation, **authoritative by default**
   (see M-2 for the verification contract):
   - **(a) Canonicalise**: derive a canonical (emoji, hanzi) pair from
     the allowlist using the verifier (≥2 agreeing signals).
   - **(b) Verified-hit → auto-rewrite + git-commit**: write the
     canonical pair to the flexiarg's `@sigils` line, preserving the
     agent's original value in the commit message body for
     reversibility; commit-author `bot/sigil-canonicaliser`; emit
     `event "sigil-assignment"` evidence with source-pair, assigned
     pair, signals-agreement, and the originating commit SHA. Re-runs
     of the canonicaliser are idempotent — the verifier sees a
     canonical pair and exits.
   - **(c) Residual pend**: when the verifier cannot meet its
     two-signal-agreement bar, mark the vertex `:sigil/pending true`,
     leave `@sigils` as-is, emit `event "sigil-assignment-deferred"`.
     The TSV row carries an explicit `pending` marker (not `unknown`).
     The pending pile is the directly-observable residual; its size
     measures the verifier's headroom.

3. **New-pattern provenance link** — on first ingest of a flexiarg
   whose qname is new to substrate-2, emit a `code/v05/pattern-origin`
   hyperedge linking the pattern vertex to:
   - the commit that introduced the file (already trackable via
     `code/v05/edits` on phase-3 commit-vertex ingest)
   - **the Evidence entry where the pattern was first proposed** —
     this is the new contract. Heuristic v0: look up evidence entries
     whose body text references the file path or the pattern qname
     within the last N hours of file appearance, and link the highest-
     ranked such entry. (Concrete shape to be settled in MAP.)

4. **Pilot via `library/orchestration/`** — use the two new patterns
   as the worked example. By the end of the mission:
   - Both vertices in substrate-2 carry full props (title, slots,
     sigils, audience, etc.)
   - Both have canonical sigils (auto-assigned or human-set; the
     pipeline decision logged either way)
   - Both have `pattern-origin` edges back to the agent-evidence that
     proposed them (this is harder — there may not BE an evidence
     entry yet, since the patterns were authored outside the formal
     `flexiarg-proposed` evidence shape — see DERIVE)
   - One concrete inference move is demonstrated that requires the
     richer projection: e.g. "find all patterns whose IF-clause
     references *defer*" or "rank pattern candidates by structural
     overlap of their IF/HOWEVER with the current turn's protopattern."

## Scope OUT

- **INDEX.md ingestion.** The orchestration `INDEX.md` falling outside
  the mission-doc filter is a real gap, but it's a different projector
  (a namespace-index projector, not a pattern projector). Punt to a
  follow-on or to widening the `.md` filter — captured here so it isn't
  lost, but not part of this mission's deliverable.
- **War Machine candidate-window upgrades.** WM gating on
  `context-retrieval` evidence (rather than enumerating the library)
  is a separate design call that interacts with WM-I4 / consent-gate
  discipline. This mission stops at "the new patterns are retrievable
  and substrate-rich"; whether WM should enumerate is owned by
  `M-war-machine-wiring` or a sibling.
- **MiniLM regen cadence.** The daily 04:30 UTC cron is fine for now.
  If richer substrate-2 props let the embedder use slot-aware text
  instead of flat concatenation, that's a separate enrichment.
- **Tokipona allowlist extension.** Whether `📁` (file folder),
  `🌂` (umbrella), or other plausible namespace emoji should be added
  to `holes/tokipona.org` is an editorial call for Joe, not a
  pipeline-fix. The pipeline must do something coherent with whatever
  allowlist exists; that's this mission's responsibility.

## MAP — open questions

- **M-1.** What's the right substrate-2 schema for pattern slots? Two
  candidates: (a) one `code/v05/pattern-slot` hyperedge per slot,
  endpoints `[pattern-qname slot-name]`, props `{:slot/text ... :slot/idx
  ...}`; (b) a single `code/v05/pattern-content` hyperedge with all
  slots as nested props. (a) is queryable per-slot; (b) is cheaper to
  write+read whole. Spec-tradeoff: hypergraph-of-content vs document-
  per-pattern.

- **M-2.** ~~What confidence threshold gates auto-rewrite of an invalid
  sigil?~~ **Settled 2026-05-26 (Joe):** *"Sigil rewrite should be
  automatic, correct+verified, and totally git-commitable."* The
  pipeline is authoritative for sigils — no consent gate, no working-
  tree-pending state, no operator-review queue. Auto-rewrite is
  committed. What remains is **what "correct+verified" means
  operationally**:
  - **Correct**: both segments come from the canonical allowlists —
    emoji ∈ tokipona.org's emoji-set, hanzi ∈ the corresponding
    hanzi-set. Non-negotiable.
  - **Verified**: the assignment is justified by more than one signal.
    Candidate signals: (i) the pattern's title + conclusion text
    embedded against the allowlist's existing emoji↔pattern
    associations (semantic neighbours); (ii) namespace-locality —
    other patterns in the same namespace cluster on a small set of
    sigils, the new pattern likely belongs in that cluster; (iii) the
    agent-supplied placeholder when partial (`📁/?` means "first
    segment is intended, fill the second"; if `📁` happens to be in
    the allowlist already, treat it as a hint). The pipeline runs ≥2
    signals; agreement → assign and commit; disagreement → fall
    through to pending. **The bar is: the assignment is defensible
    enough that the pipeline puts its name on the commit.**
  - **Sub-question (open):** when no confident assignment exists,
    `:sigil/pending true` is still the right escape valve — "automatic"
    doesn't mean "force a guess." The pending case is the residual,
    not the default. Estimate at MAP-close: what fraction of today's
    144 invalid-sigil patterns can the verifier confidently canonicalise?
    That fraction is the mission's directly-observable success metric.
  - **Sub-question (open):** what does the commit message look like,
    and who's the author? Likely `bot/sigil-canonicaliser` with a
    structured body recording the signals + canonical-source SHA, so
    the assignment is auditable post-hoc. The agent's original
    `@sigils` value is preserved in the commit message body for
    reversibility.

- **M-3.** How does pattern-origin link to evidence in the
  retroactively-true case? The orchestration patterns were authored
  outside a formal `flexiarg-proposed` evidence event. The agent that
  wrote them did so in chat. Do we backfill an evidence entry from
  the chat transcript? Do we accept that pre-2026-05-26 patterns won't
  have origin edges? Do we allow operator-asserted origin edges?

- **M-4.** What does "shape-rejection" mean operationally? If the
  watcher's per-cycle ingest sees a malformed flexiarg, it currently
  emits the bare vertex (`has-doc false` and not much else). Legacy
  rejection would emit *no vertex at all*, with a structured failure
  record. That changes the substrate's invariants — every `code/v05/var`
  of kind `flexiarg` would now be guaranteed-shaped. Worth the
  invariant? Trade-off: invariant strength vs. partial-information
  visibility.

- **M-5.** What's the inference move we pilot? Joe's framing: *"this
  would be relevant to further linking and inference based on the
  pattern contents (and we might pilot that in the mission to illustrate
  why this matters)."* The three concrete options, sharpened against
  the two orchestration patterns:

  **(i) Slot-aware retrieval — rank by IF / HOWEVER overlap rather
  than flat-text cosine.** Today's `notions_search.py` embeds the
  whole flexiarg text as one string; an A→B turn's protopattern is
  matched against that flat blob. With slots in substrate-2, retrieval
  could weight IF-overlap higher (the IF clause describes the
  *trigger condition*; that's what an in-flight turn matches against),
  and use HOWEVER as a *negative signal* (a turn that's hitting the
  HOWEVER's pain point is one the pattern is *for*). For
  `rule-evolves-from-its-deferrals`: today's flat-text retrieval
  returned it at score 0.6396 on a defer-classifier query; with
  slot-weighted retrieval, the same query should pin it firmly at
  rank 1 with a wider gap to the runners-up.
  - *Demonstrates:* substrate richness directly improves retrieval
    quality, which directly improves WM candidate sets.
  - *Cost:* nontrivial. Requires reworking `notions_search.py` to use
    slot-aware projections, and either re-embedding slots separately
    or post-hoc reweighting.
  - *Risk:* the existing 0.64 cosine is already strong; the marginal
    improvement may not be dramatic, weakening the "why this matters"
    pilot.

  **(ii) RELATED-graph enrichment — emit `code/v05/related-to` edges
  from each pattern's RELATED clause.** The orchestration patterns'
  RELATED sections explicitly name other library patterns
  (`realtime/authoritative-transcript`, `aif/prediction-error`,
  `code-coherence/dead-code-hygiene`, etc). Today those are dead
  references — text inside a slot. With this move, every named
  cross-reference becomes a typed edge in substrate-2, and the
  WebArxana pattern view can render the local neighbourhood (which
  patterns this one composes with, which it's-a-variant-of, which it
  contrasts with).
  - *Demonstrates:* substrate richness unlocks a *navigation surface*
    that didn't exist — the pattern library becomes a graph, not just
    a collection. Cross-pattern composition becomes visually obvious.
  - *Cost:* low. Edge type already fits the v0 schema; the parsing is
    a regex over the RELATED block; rendering hooks into existing
    WebArxana machinery.
  - *Risk:* the RELATED slot is a new orchestration-namespace
    convention; legacy patterns don't have it, so the pilot graph is
    sparse outside orchestration. That's a feature for illustrating
    the value of the convention but a limitation as a comprehensive
    demo.

  **(iii) Clause-shape-overlap diagnostics — feed the trip-journal /
  pattern-application machinery.** When an A→B turn produces a
  pattern-application record, today's machinery compares the turn's
  protopattern against the *pattern title + flat text*. With slots,
  the machinery can do a clause-aware overlap: did the turn's
  observation match the pattern's IF? Did the turn's outcome resolve
  the HOWEVER? Did the turn's action follow the THEN? This is the
  diagnostic shape `M-pattern-application-diagnostic` actually wants.
  - *Demonstrates:* substrate richness unlocks the consumer mission's
    intended evaluation move. The two missions' interface becomes
    concrete instead of speculative.
  - *Cost:* highest. Touches the pattern-application diagnostic
    machinery, which is itself mid-flight. Coupling two in-flight
    missions risks blocking either.
  - *Risk:* if `M-pattern-application-diagnostic` hasn't settled its
    own slot-overlap shape, this pilot is built on shifting sand. Better
    to wait for the consumer mission's contract.

  **Default recommendation if you don't want to choose now:** option
  (ii) RELATED-graph enrichment — it's the cheapest concrete win, has
  the most visible artefact (a rendered cross-pattern graph), and
  doesn't couple to other in-flight missions. (i) is the highest-
  impact retrieval-quality move but the marginal gain is uncertain;
  (iii) is the highest-leverage cross-mission move but should wait.
  Settled at MAP-close.

## DERIVE — anticipated moves

1. **Projector v1 in `futon3c.watcher.projections.flexiarg`** —
   parse the full flexiarg structure (Joe's auth dictates Toulmin
   slots; the orchestration namespace's `DOES-NOT-APPLY` /
   `INSTANCES` / `RELATED` extensions are accepted as data); emit the
   richer vertex + slot edges; preserve the v0 contract of always
   emitting *some* vertex for backwards-compatible queries.

2. **Sigil canonicaliser in `scripts/build_pattern_index.clj`** —
   refactor `parse-inline-sigils` to return `{:valid? :assigned-from
   :original :canonical :confidence}` instead of `nil` on rejection;
   pipe the result back to a small rewrite-flexiarg helper that
   updates the `@sigils` line in place when confidence is high.

3. **Pattern-origin edge emitter** — likely a new ns
   `futon3c.watcher.projections.pattern-origin` that, on first-seen
   of a flexiarg qname, queries the evidence store for proximate
   references and emits `code/v05/pattern-origin`.

4. **Pilot demonstration** — a short script or interactive query that
   exercises one inference move over the richer substrate-2 props for
   the two orchestration patterns, with output that's recognisably
   non-degenerate compared to the v0 substrate.

## ARGUE — design decisions to settle

(Empty until DERIVE has surfaced concrete moves. Expected ARGUE
inputs: the slot-schema choice from M-1, the auto-rewrite threshold
from M-2, the inference-move choice from M-5. Each gets an
IF / HOWEVER / THEN / BECAUSE block here once derived.)

## VERIFY

- Substrate-2 invariant: every `code/v05/var` of kind `flexiarg` has
  non-empty `:pattern/title` and `:pattern/sigils-canonical` (the
  latter possibly tagged `pending` rather than carrying a value).
- TSV invariant: zero patterns with literal `unknown/unknown` in the
  rationale, post-mission. (Today: ~144.)
- Pilot invariant: the two orchestration patterns each carry a
  `code/v05/pattern-origin` edge with a non-trivial witness
  (commit-sha + evidence-id, or operator-asserted origin marker).
- One inference move (M-5 winner) runs against substrate-2 and produces
  output that would have been impossible against v0 props.

## INSTANTIATE

- Hot-reload the projector via Drawbridge (`load-file
  "src/futon3c/watcher/projections/flexiarg.clj"`); confirm the next
  watcher cycle emits the richer props for the two pilot patterns.
- The sigil canonicaliser change touches a script run by cron, not the
  serving JVM — runs at the next 04:30 UTC cron tick, or manually via
  the same script. Confirm the TSV's `unknown/unknown` row count drops
  to whatever floor the assignment heuristic supports.
- Pattern-origin edge emitter — depending on M-3, either runs as a
  one-shot backfill over existing patterns or hooks into the watcher's
  per-file-ingest path for new ones.

## Out-of-band notes

- **Where work actually lands.** The mission lives in `futon3` (this
  file) because pattern-ingest is stack-level concern. The code that
  changes lives mostly in `futon3c` (`src/futon3c/watcher/projections/`)
  with one script in `futon3` (`scripts/build_pattern_index.clj`).
  Cross-repo commits are expected; checkpoint both per the §8
  "checkpointing companion repos" rule in `futon0/CLAUDE.md`.

- **The "verbatim sigils" point — and why this is NOT consent-gate
  territory.** The agent that authored the new orchestration patterns
  chose `📁` and `🌂` deliberately — `📁` for the namespace's
  filesystem feel, `🌂` for "umbrella" rhyming with
  `realtime/learn-as-you-go`. Those are real authorial signals. An
  earlier draft of this mission floated consent-gate framing à la
  `M-vsatarcs-writer` here. **Joe's call 2026-05-26: sigil
  canonicalisation is mechanical, not semantic** — the pipeline is
  authoritative, auto-rewrites, and commits. Reasoning: sigil
  validity is *checkable against a canonical allowlist* (unlike a
  semantic decision such as which Toulmin slot some text belongs in,
  which has no canonical ground truth). Mechanical canonicalisation
  is more like `gofmt` than like a writer-action-class — you don't
  gate code formatting on operator approval. The agent's intent is
  preserved by recording the original `@sigils` value in the commit
  message body (reversibility), and by the auditable structured
  signals used to choose the canonical pair. If `📁` *should* be in
  the allowlist, that's an editorial decision Joe makes by editing
  `holes/tokipona.org`; the pipeline doesn't extend the allowlist on
  its own. Authorial agency is preserved upstream (allowlist content)
  and after-the-fact (commit message + reversibility), not at the
  per-pattern ingest moment.

- **Why now.** Two contributing forces: (i) the orchestration namespace
  just appeared and is the most tractable pilot we'll have for months;
  (ii) substrate-2 thinness on patterns is currently masked because
  the embedding+TSV pipeline is *separate from* substrate-2, so the
  cost of the thinness is invisible in retrieval but bites every time
  someone wants to query patterns *as content* (e.g. for cross-pattern
  link inference). Closing this before more patterns accrete in the
  thin state.
