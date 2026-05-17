# War Bulletin 9: The Stack Authors Its Own Pilot

**Date:** 2026-05-17
**Context:** Three weeks since bulletin-8 (2026-04-24). On the
  surface this was an external-work period — the UKRN Working
  Paper transitioned v11 → v12 — with last broad futon
  commits on 2026-05-04 and apparent silence after. *Underneath*
  the surface, the week of 2026-05-10 → 2026-05-14 carried the
  deepest structural work of the cycle: an outward-facing
  operational layer authored in mission-doc form, with UKRN-S
  v12 named explicitly as the predecessor exemplar to port
  inward. This bulletin reads that work as a single move.
**Trigger:** Joe's survey request, plus his clarifying frame —
  *"port the UKRN-S v12 pro bono consulting to a Futon-facing
  outlook so that I can run a 'pilot' for futon becoming
  self-sustaining"* (`futon7/holes/M-interim-director.md`).

## The Arc

| Bulletin | What it added | What was still implicit |
|----------|--------------|------------------------|
| 7 (Apr 13) | War Machine + daily scan | Stack's own argument as data |
| 8 (Apr 24) | AIF+ self-model + empirical bite check | Continuous re-derivation of next move |
| **9 (May 17)** | **Outward-facing operational layer + UKRN-S as predecessor exemplar to port back inward** | **Vocabulary-step landing and first checkpoint-emit firing** |

Bulletin-8 said *the stack speaks AIF+ to itself and demands
evidence*. Bulletin-9 says *the stack writes its own job
description and asks whether the work is purchasable.*

## Findings

### Finding 1: What looked like 13 quiet days was the deepest structural week of the cycle

Zero source-code commits across `futon0`, `futon3*`, `futon4`,
`futon5*`, `futon6`, `futon7*` between 2026-05-04 and
2026-05-17. Reading commit graphs only, the bulletin-8 prediction
("step PI with THE-STACK as observation") looks unexecuted and
the stack looks idle. Both readings are wrong. The work landed
in mission documents and the EoI corpus, and it constitutes a
new operational layer the stack did not have on 2026-04-24:

| Mission / Artefact | Date | Status |
|---|---|---|
| `M-mission-coherence-patterns` | 2026-05-11 | HEAD complete (eoi-engine flash `eoi-inward-mission`, N=1 of class) |
| `M-essays-edit-cycle` lab psr/pur (xtdb-projected catalog) | 2026-05-13 | landed |
| `M-essays-edit-cycle` lab psr/pur (annotation-lifecycle, persistent-retraction-visibility) | 2026-05-14 | landed |
| `M-essays-retraction-visibility` | 2026-05-14 | READY TO EXECUTE |
| `M-essays-diachronic-model` | 2026-05-14 | SPECIFIED |
| `M-interest-network-coupling` | 2026-05-14 | **Design-frozen v1** (codex-7 sign-off; event vocabulary + checkpoint format + projection + 3-vector completeness signal) |
| `M-interim-director` | 2026-05-14 | HEAD authored (futon7; 2-week timebox; eoi-engine flash; N=2 of class — successor to `M-mission-coherence-patterns`) |
| `atthangika-buckets.json` `eoi_instances[]` + `institution_objects[]` | continuous | N=5 corpus with first cross-essay structure (Hyperreal Side B engages Side A §2/§5/§6) |

Reading them as a single move: the stack now has an *outward-
facing* mission class with its own infrastructure
(M-interest-network-coupling) and its own pilot (M-interim-
director), and the two are coupled via a typed event vocabulary
that flows into the existing XTDB substrate.

### Finding 2: UKRN-S v12 is the predecessor exemplar, not an external pull

Bulletin-9-draft-1 framed the v11→v12 work as an "external pull"
that out-voted bulletin-8's recommended next-move. That framing
was incomplete. M-interim-director Q1.2 (Right Intention) names
the relationship directly:

> *"A useful predecessor project that shows me that 'something
> like this is possible' is the business plan I just developed
> for UKRN-S in ~/npt/working-paper/ (especially
> v11_year1_scenarios_for_jacobs.xlsx)."*

The UKRN-S apparatus — 5-role design, evaluator-centric
operation, Bayesian simulation with belief-state and EFE,
AIF² annotation layer over the paper, year-1 scenarios
spreadsheet, commissioning sheet, R1-R12 completeness contract —
is the *template* M-interim-director is now pointed inward at,
with futon7 as the outward-facing Markov blanket and futon0 as
the Joe-facing cyborg layer (M-interim-director Q1.2). The
two-week mission is the design phase; a downstream June-August
2026 validation-execution phase consumes M-interim-director's
exit artefact (a preregistered plan another operator could run).

The reframing matters because the "out-vote" reading treated the
pivot as defection from the stack's recommended move; the
predecessor-exemplar reading treats the pivot as the stack
discovering that *its own next move had a predecessor it needed
to author first*. WR-14 (operator-override-as-criterion, below)
stands; but the criterion is now better-named as
*predecessor-completion-pull* rather than mere external pull.

### Finding 3: M-interest-network-coupling is the AIF apparatus generalised to a wider scope

The codex-7 + claude-3 design in
`futon4/holes/missions/M-interest-network-coupling.md` is a
faithful generalisation of the AIF substrate that bulletin-8
built at the stack level:

| Stack-AIF (bulletin-8) | M-interest-network-coupling (bulletin-9) |
|---|---|
| `THE-STACK.aif.edn` + 16 leaves | EoI corpus + `institution_objects[]` |
| Mission overlay onto spine | Posterior projection per EoI entity (event log replay) |
| `:reading :next-move` recommendation | Operator emits explicit `:emitted-events` in checkpoint EDN |
| Bite empiricality (logical vs measured) | 3-vector completeness signal (Path / basin / resolution-path) |
| Drift detector + re-render gating | Event-log replay; `state/reopened` for retraction-isn't-terminal |

Eight event types (`state/spawned`, `state/refined`,
`state/strengthened`, `state/addressed`, `state/falsified`,
`state/foreclosed`, `state/reopened`, `link/asserted`) plus a
checkpoint-emission discipline (markdown with embedded EDN,
*no-op days emit no checkpoint*) plus a two-stage projection
(append-only event log + derived posterior). Codex-7's sign-off
landed after a second whistle round resolved replay/idempotence,
daily-cadence-without-synthetic-churn, and the HEAD-as-escrow
pattern (Finding 5).

S6 (AIF substrate) is no longer one apparatus inside one stack.
It is a *family* whose members run at different scopes: stack-
spine (THE-STACK.aif.edn), portfolio inference (futon3c), UKRN
simulation (v2/v3 runner), cross-mission posterior updates
(M-interest-network-coupling), single-essay rewriting (Stream B
in bulletin-9-draft-1's Finding 3). All five members
instantiate the same `library/aif/*` patterns.

### Finding 4: The "model essay health" crossover is the AIF observation channel for M-interim-director

Joe's framing in this conversation: *"a quantitatively-backed way
to model the health and wellbeing of essays in general."* The
M-interest-network-coupling design makes this concrete:

- **Hidden state** = each EoI entity's standing (live /
  addressed / strengthened / falsified / foreclosed / reopened).
- **Observation channel** = checkpoint-emitted events targeting
  entities, with `:evidence/refs` pointing at the artefacts that
  justify each transition (interview notes, business-plan
  drafts, go/nogo memos).
- **Belief μ** = the projection's posterior state per entity, with
  the event trail as its provenance.
- **Action space** = the COMPOSITIONS menu of each pattern (for
  essay-side moves) and the next-checkpoint candidate
  delta-batches (for mission-side moves).
- **EFE proxy** = the 3-vector completeness signal. Pragmatic
  axis: resolution-path coverage (lived adequacy). Epistemic
  axes: Path coverage + basin coverage (exploration).

The combined apparatus *is* the futon-as-business AIF.
M-interim-director's preregistered validation plan is the
preregistered protocol under which the AIF runs in June-August
2026 with revenue-evidence as the most discriminating event
class.

The v3-runner work in npt and the cross-mission coupling work in
futon are now visibly the same project at two scales: a
quantitative apparatus for ranking interventions under
uncertainty, deposited as patterns that are reusable across
domains (training delivery, business validation, essay
rewriting). Stream A and Stream B from bulletin-9-draft-1 are
two readings of one stream.

### Finding 5: HEAD-as-escrow is a sanctioned new pattern

M-interim-director's T2 ("dialogue-debt") and
M-interest-network-coupling §3.5 jointly surface a pattern that
had no name a week ago: a **mission HEAD that crystallises
intentions without committing to data shape**, used to decouple
HEAD-authoring from a predecessor's schema commit. Joe authored
M-interim-director's HEAD on 2026-05-14 even though step (b)
(event vocabulary in code) is the hard predecessor for
*emitting* checkpoints. The HEAD escrows the operator's intent
for the predecessor to redeem once schema lands.

Codex-7 flagged this for formalisation as a flexiarg under
`structure/` or `pattern-discipline/`. Two stack-side
consequences:

1. Missions can launch before their tooling is ready, provided
   the not-yet-committed pieces are *named* in the HEAD's
   tensions. This makes the inhabitation gap (C1) less likely to
   be silently masked by HEAD-stage motion.
2. The pattern is a candidate for `library/mission-coherence/`
   (the namespace M-mission-coherence-patterns is bootstrapping).
   Three patterns are now ready for that namespace: HEAD-as-
   escrow, eoi-engine-flash-as-HEAD-engine, and the
   "completeness-not-closure as 3-vector not scalar" stance from
   M-interest-network-coupling.

### Finding 6: The futon0 / futon7 split is now load-bearing architecture

M-interim-director Q1.2 names the cut in passing — *"futon0 as
the main Joe-facing 'cyborg layer' and futon7 as the main
outward-facing 'Markov blanket'"* — and treats the cut as
already-positioned. As of bulletin-8 this was a working note;
as of bulletin-9 it is the architectural axis under which the
self-sustaining pilot runs. The cut is consistent with the AIF
Markov-blanket framing in `annotations-v12.edn`'s `:system`
declaration and gives an explicit answer to *who reads what
when*: the cyborg layer carries Joe's working surface; the
Markov blanket carries the outward presentation; M-interim-
director runs primarily on futon7.

This warrants spine acknowledgement (status section below).
The cut may need its own substrate-shape — an organising
analogue to S9's "a-sorry-enterprise frame" — once it is run
under load.

### Finding 7: Operationalised exploit-loops already exist on linode-chicago — and have been quiescent for two weeks

A fresh check (`ssh linode-chicago`, 2026-05-17) finds the
`~/mark2/` arxiv-paper-processing pipeline live: a transfer-lane
between Joe-side input batches and Rob-side automated processing,
authoritative state in `~/mark2/state.json` (owner: `rob`),
manifest at `~/mark2/arxiv_manifest.sqlite` (801MB). Lifecycle
per `README-OPERATOR.txt`: Joe builds batch → Rob pulls and runs
→ Rob uploads results → Joe collects → Joe marks done.

State at the time of writing:

| Batch | Papers | OK | Failed | Returned | Status |
|---|---|---|---|---|---|
| 1 | 5000 | 4893 | 107 (2.1%) | 2026-04-21 | done |
| 2 | 5000 | 4869 | 131 (2.6%) | 2026-04-21 | done |
| 3 | 10000 | 9767 | 233 (2.3%) | 2026-04-22 | done |
| 4 | 10000 | 9680 | 320 (3.2%) | 2026-04-22 | done |
| 5 | 5000 | 4859 | 141 (2.8%) | 2026-04-25 | done |
| 6 | 5000 | 4821 | 179 (3.6%) | 2026-04-25 | done |
| 7 | 5000 | 4349 | **651 (13.0%)** | 2026-05-02 | **results-ready (uncollected)** |
| 8 | 5000 | 4567 | **433 (8.7%)** | 2026-05-02 | **results-ready (uncollected)** |

Total: 50,000 papers processed across two months, with a
**discontinuity in failure rate at batches 7-8** (2.1-3.6% → 8.7%
and 13.0%) and a **14-day Joe-side collection lag** that is the
longest of the pipeline's lifetime. The sibling `apm-lean`
daily-batch (`/home/joe/code/apm-lean/problems/a97A08/status.json`
→ `:imported_at "2026-05-02T19:07:38Z"`, claude-1+codex-1
pipeline) **stopped on the same day** — the v11 → v12 working-paper
period also pulled attention away from both exploit loops.

This is structurally important for three reasons:

1. **Rob's `state.json` is the pattern that M-interest-network-coupling
   §2.0-2.2 formalises.** A third-party operator emitting
   typed structured state (`status: results-ready`, timestamps,
   counts) into a shared substrate that Joe reads at his own
   cadence — exactly the cross-operator checkpoint discipline the
   new event vocabulary specifies, just under an ad-hoc schema. *The
   mission-doc work this week wrote down the pattern that mark2 had
   already proven works in the wild for two months.*
2. **The proxy metric Joe asked for already has its raw signal
   stream.** Batch success rate (97% → 87%), per-paper lean sorry
   counts (apm-lean `status.json :sorry_count_total`), and
   classification outcomes (`:partial` vs solved) are exactly the
   kind of progress marker that distinguishes *serious advance* from
   *personal-level study* from *chimera*. Currently none of these
   feed Stack HUD or the War Machine.
3. **The collection→surface loop is the gap.** Rob's side runs
   without operator attention. Joe's side has 2 stale batches in
   outbox, a 13% failure-rate signal nobody surfaced, and a
   daily-batch loop that stopped silently. The exploit half of
   the explore/exploit pair is *under-instrumented*, not absent.

This finding directly addresses the explore/exploit tension Joe
just named. The habits *are* possible to offload because Rob has
already offloaded the hardest one. The unfinished part is the
*observation channel* from operationalised exploits back into the
self-representing stack, which is precisely what
M-interest-network-coupling's vocabulary is built to carry.

### Finding 8: Stack-code futonic debt is real but reweighted

The debts named in bulletin-9-draft-1 — F4-F7 devmap evidence
gaps, 25 missing `@sigils`, 242 missing `@references`,
M-the-futon-stack Q1-Q6 verifications unanswered, drift-detector
not re-run, sibling-bug sweep on 2026-05-04 fixes — are still
real. They map onto C2 (Closure-evidence drift).

The reweighting: the *hard* predecessor work for the pilot is
M-interest-network-coupling step (b) [event vocabulary
committed in code under `library/`], step (c) [projection wired
into XTDB], step (d) [WebArxana interest-network view]. Without
these, M-interim-director's HEAD remains escrowed indefinitely
and the pilot can author intent but cannot emit posterior
evidence. The bug-fix sweep (half-day) is fine and should still
land. The library-hygiene sweep is **deferred** behind step
(b)/(c)/(d).

The bug-shaped items from draft-1 still hold:

- Sibling-bug sweep on the 2026-05-04 fix patterns (dynamic
  binding scope; tied-timestamp re-ingest; default-on
  auto-register; ingest re-fire).
- F6/F7 devmap `next[...]` gap (per `futon3/TODO.md`).
- `detect_drift.clj` run.

These remain a half-day pass.

## WR Decisions

### WR-11: External applications carry *predecessor-exemplar* relationships back into the stack

Strengthened from draft-1. The v3-runner work is not just
"library/aif/ getting external citation" — it is *the predecessor
project Joe explicitly cites when authoring an inward-pointed
pilot*. When an application produces an artefact that an inward
mission's HEAD names as predecessor (here: UKRN-S commissioning
sheet + v11_year1_scenarios_for_jacobs.xlsx), the application
has graduated from substrate-deliverable to *operational
blueprint*. Such graduations carry bulletin-grade visibility
and write to the spine.

### WR-12: Essay health is the AIF observation channel for cross-mission posterior updates

The annotation hypergraph (`annotations-v12.edn` schema) plus the
M-interest-network-coupling event vocabulary together form an
observation channel: typed events targeting essay/section/
annotation/institution-object/basin/composite-arrow entities,
with `:evidence/refs` and `:operator/rationale` per event. The
EFE machinery in `library/aif/*` reads this channel directly.
"Essay health" is the projected belief state under that channel.

### WR-13: Futonic debt is paid in order — predecessor work first, library hygiene deferred

Revised from draft-1. **Order: M-interest-network-coupling step
(b) → M-interim-director Checkpoint 0 → step (c)/(d) in
parallel with daily-until-done checkpoints → bug-fix half-day
sweep → library hygiene → v3-runner build → wider crossover.**
Reasoning: step (b) is a 2-3 day codex-7 task that unblocks the
pilot; without it, the pilot's design phase is HEAD-as-escrow.
Library hygiene can wait two weeks.

### WR-14: Predecessor-completion pull is a fourth next-move criterion

Renamed from draft-1's "operator override." The 23-day pivot to
npt was not override against the stack's recommendation; it was
the stack discovering that its recommended move had an
unauthored predecessor (an outward exemplar to port inward).
PI, once stepped, ingests three signal classes: the cached
:reading :next-move (criterion 1-3), the operator-executed move
that diverged (this criterion), and any predecessor artefact
the divergence produced.

### WR-15: HEAD-as-escrow is a sanctioned pattern, candidate for `library/mission-coherence/`

A mission HEAD that crystallises intentions without committing
to data shape, used to decouple HEAD-authoring from a
predecessor's schema commit. Safe when the not-yet-committed
pieces are named explicitly as tensions. Flexiarg pending under
`mission-coherence/` (the namespace M-mission-coherence-patterns
is bootstrapping). Sibling candidates: eoi-engine-flash-as-HEAD-
engine; completeness-not-closure-as-3-vector.

### WR-16: Operationalised exploit-loops are first-class observation channels

mark2 (Rob-operated arxiv pipeline) and apm-lean daily-batch
(claude-1+codex-1 pipeline) emit *typed structured state* on
cadences independent of Joe's attention. They are
**observation channels in the AIF-substrate sense** and should
be wired into Stack HUD / War Machine as direct evidence streams,
not as adjacent reading material. Three immediate uses:

1. **Habit-completion telemetry.** *Batch collected within N
   days of `results-ready`* is a binary observable that says
   whether Joe's side of the loop is keeping up. Currently
   14-day lag on batches 7-8 = the habit is broken; visible
   binary failure.
2. **Pipeline-health telemetry.** Success rate drift (97% →
   87%) is a first-class signal that should generate a
   bulletin-grade alert (or at minimum a HUD widget) when the
   discontinuity is this sharp. The signal exists; the surface
   for reading it doesn't.
3. **Proxy-metric substrate for M-interim-director.** The
   "serious advance / personal breakthrough / chimera"
   trichotomy Joe named maps onto observable quantities: papers
   processed per week, downstream apm-lean sorry-count
   reductions, external citations or buyer-language traces.
   M-interim-director CP-0 should attach to this stream
   *immediately* because it is the only existing source of
   accumulating evidence the mission can point to without
   doing fresh primary research.

### WR-17: futon0-as-cyborg / futon7-as-Markov-blanket is stack architecture

Architectural axis under which M-interim-director operates and
under which the self-sustaining pilot will be run. Outward-
facing work consolidates in futon7; Joe-working-surface work
stays in futon0. The cut is consistent with AIF² Markov-blanket
framing. The cut may, after the pilot, become a substrate shape
in S9's frame.

## Status

```
S0 (Thesis: stack maintains itself)        — UNCHANGED-IN-CLAIM, REFRAMED
                                             (now testable via M-interim-
                                              director: does the system
                                              produce purchasable work?)
S1 (Pillar I: argument operational)        — STABLE
S2 (Pillar II: invariants)                 — STRENGTHENED-CONDITIONALLY
                                             (10 shapes; HEAD-as-escrow
                                              candidate for an 11th
                                              meta-shape)
S3 (Pillar III: missions)                  — STRENGTHENED
                                             (M-mission-coherence-patterns
                                              + M-interest-network-coupling
                                              + M-interim-director; new
                                              mission class instantiated)
S4 (Cycle: 5-step generative)              — STABLE
                                             (eoi-engine flashes now
                                              author HEADs; engine is the
                                              cycle's authoring surface)
S5 (Self-representation substrate)         — DRIFT-RISK
                                             (no detect_drift.clj in 13d)
S6 (AIF substrate: PI/HO/MC)               — GENERALISED + EXTERNALLY-VALIDATED
                                             (now a family of apparatuses
                                              at different scopes; R1-R12
                                              external contract on the
                                              library/aif/ namespace)
S7 (Pattern canon)                         — STRENGTHENED
                                             (3 v3-runner deposits;
                                              external citation;
                                              mission-coherence namespace
                                              bootstrapping)
S8 (Durable storage / Arxana)              — STRENGTHENED
                                             (XTDB-projected essay
                                              registry; retention/retraction
                                              psr/pur; cross-mission event
                                              log scoped)
S9 (a-sorry-enterprise frame)              — OPERATIONALISED
                                             (futon0-cyborg / futon7-Markov-
                                              blanket cut now load-bearing;
                                              M-interim-director is the
                                              first pilot under this frame)

C1 (Inhabitation gap, weight 7)            — UNRESOLVED + UNDER-OBSERVED
                                             (not re-measured in 23 days)
C2 (Closure-evidence drift, weight 4)      — WIDENED + REWEIGHTED
                                             (predecessor-work debt
                                              (step b/c/d) now load-bearing;
                                              library-hygiene gaps deferred;
                                              + mark2 batches 7-8 uncollected
                                              for 14 days, apm-lean daily-batch
                                              stopped 2026-05-02 — operationalised
                                              exploit-loops unsurfaced is a
                                              first-class instance of C2)
C3 (Weak generalisation evidence, weight 4)— SUBSTANTIALLY ADDRESSED
                                             (S6 generalised across 5
                                              scopes; library/aif/ carries
                                              external citation; cross-
                                              mission coupling generalises
                                              single-essay lifecycle)
C4 (Internal grasp ≠ external reach, w 3)  — UNDER ACTIVE TEST
                                             (M-interim-director's 2-week
                                              design phase is the test;
                                              June-August validation
                                              window is the measurement)

A1 (Observation half-blind)                — STARTING TO ADDRESS
                                             (cross-mission event log gives
                                              a posterior observation surface
                                              the stack didn't have; mark2 +
                                              apm-lean are pre-existing
                                              observation channels not yet
                                              wired into Stack HUD / War Machine)
A4 (Hermit trap)                           — FURTHER ADDRESSED
                                             (codex-7 review loop; outward
                                              pilot named; futon7-as-Markov-
                                              blanket axis)
```

## What's Built (Concrete Inventory, since bulletin-8)

| Artefact | Path | Status |
|----------|------|--------|
| UKRN WP v12 prose + cuts manifest | `npt/working-paper/UKRN_WP_draft_v12.md`, `notes_v12_cuts_manifest.md` | landed |
| UKRN WP v12 commissioning sheet + year-1 scenarios | `npt/working-paper/UKRN_WP_v11_commissioning_sheet.{md,pdf}`, `v11_year1_scenarios_for_jacobs.xlsx` | landed |
| Essay AIF² hypergraph | `npt/working-paper/annotations-v12.edn` (252KB) | landed |
| Argument spine (machine-readable) | `npt/working-paper/argument_v11.sexp` (82KB) | landed |
| AIF completeness contract | `npt/working-paper/docs/aif-completeness.md`, `tech_note_aif_completeness_criteria.md` | landed |
| Library ↔ criteria crossref | `npt/working-paper/aif_library_to_criteria_crossref.md` | landed |
| v3-runner scoping (~27 days) | `npt/working-paper/tech_note_v12_v3_runner_aif_completion.md` | scoped, three revisions |
| New AIF library deposits | `futon3/library/aif/{hierarchical-budget-aware-action-selection, shared-kernel-predictive-forward-model, experimental-comparison-of-EFE-variants}.flexiarg` | landed |
| Pattern canon 7-component reshape + conclusions | `futon3/library/**/*.flexiarg` + clause-reshape labs | landed 2026-05-04 |
| Canonical flexiarg parser P-1 | `futon3a` (`futon.flexiarg.projection`) | landed 2026-05-04 |
| `M-mission-coherence-patterns` (futon3) | `futon3/holes/missions/M-mission-coherence-patterns.md` | HEAD 2026-05-11 |
| `M-essays-edit-cycle` psr/pur | `futon3/holes/labs/M-essays-edit-cycle/{psr,pur}/` | landed 2026-05-13/14 |
| `M-essays-retraction-visibility` | `futon4/holes/missions/M-essays-retraction-visibility.md` | READY 2026-05-14 |
| `M-essays-diachronic-model` | `futon4/holes/missions/M-essays-diachronic-model.md` | SPECIFIED 2026-05-14 |
| **`M-interest-network-coupling`** | `futon4/holes/missions/M-interest-network-coupling.md` | **design-frozen v1 2026-05-14** |
| **`M-interim-director`** | `futon7/holes/M-interim-director.md` | **HEAD 2026-05-14** (2-week timebox; HEAD-as-escrow) |
| EoI corpus (N=5) | `atthangika-buckets.json` | live |
| Hyperreal-current-director strawman | `futon5a/.../strawmen/hyperreal-current-director.md` | strawman, awaiting hand-correction |
| `M-bounded-in-flight-state` D-01..D-06 INSTANTIATE | `futon3c` commits 2026-05-03/04 | landed; weeks-of-real-use evidence still missing |
| Web War Machine `:aif-stack` view | `futon0/web/war-machine/.../*` | unchanged since 2026-04-24 |
| Bulletin-8 stack self-model | `futon5a/holes/stories/THE-STACK.aif.edn` + 16 leaves | unchanged since 2026-04-24 |
| **mark2 arxiv pipeline** (Rob-operated) | `linode-chicago:~/mark2/{state.json, arxiv_manifest.sqlite, inbox/, outbox/}` | 8 batches, 50K papers, batches 7-8 results-ready and **uncollected 14d**; failure-rate discontinuity at batch 7 (2.8% → 13%) |
| **apm-lean daily-batch** (claude-1+codex-1) | `~/code/apm-lean/problems/a??[A,J]??/status.json` | 506 problems imported; latest a97A08 imported 2026-05-02; **stopped same day v12 paper work began** |
| Joe-side mark2 archive | `~/code/storage/mark2/{outbox,outbox.abstract-only,inbox,qc}` | 8 result bundles archived; archive lifecycle operational |

### 2026-05-17 afternoon addendum (salt-down sprint S1-S6)

| Landed today | Path | Notes |
|---|---|---|
| Mark2 batches 7-8 fetched | `~/code/storage/mark2/outbox/results-00{7,8}.tar.gz` | 69 MB + 70 MB; via codex-10 bell; size-vs-state.json anomaly flagged |
| **Proxy-metric inventory** | `~/code/futon7/holes/M-interim-director-proxy-metric-inventory.md` | 7 arms (A-G) scaffold + §2.5 math trajectories + §2.6 fitness trajectories + §4.5 session plan |
| **Invoices source-of-truth** | `~/code/invoices/{README.md, log.edn, *Invoice*.pdf}` | 1 engagement, 14 work-items, 3 paid invoices, £2,662.50 / 35.5 hours over Sep 2025 → Mar 2026 |
| **VSATARCS clutter filter** | `~/code/futon4/dev/arxana-browser-vsatarcs.el` + `~/code/futon4/README-vsatarcs.md` | `*.aif.md` filtered from anthology scan; naming-convention README |
| **Stack-fitness completeness contract** | `~/code/futon0/docs/stack-fitness-completeness.md` | F1-F10 criteria parallel to npt's R1-R12; v0.1 |
| Phase 2.a refactor | `~/code/futon4/dev/arxana-browser-rewrites.el` | ⏳ in flight (codex-10 bell `invoke-1779019183723-139-3187ad45`) |

## What's Next

The recommended next-move sequence — re-ordered to put habit-
completion ahead of new infrastructure, per WR-16:

1. **Collect mark2 batches 7-8 and audit the failure-rate
   discontinuity** (~1-2 hours). `scp linode-chicago:~/mark2/outbox/
   results-00{7,8}.tar.gz ~/code/storage/mark2/outbox/`; run
   `~/mark2/mark2 collected 7 8`; eyeball the 13% / 8.7%
   failure-rate jump (likely an arXiv-side or processing-side
   regression, not random noise). Records the first observation
   M-interim-director CP-0 can attach to.
2. **Restart apm-lean daily-batch** (~1 hour). Run
   `~/code/algorithms/apm-daily-batch.md` for one batch to
   confirm the pipeline still works after 14 days idle. If it
   doesn't, the regression-recovery is itself a CP-1 event.
3. **Wire mark2 + apm-lean status into Stack HUD / War Machine**
   (~half day each, can run in parallel). At minimum: a HUD
   widget rendering `~/mark2/state.json` status of each batch
   plus a failure-rate sparkline, and an `:aif-stack`-style
   tile rendering the apm-lean sorry-count distribution over
   recent batches. These are the proxy-metric surfaces Joe asked
   for. Closes the WR-16 observation-channel gap.
4. **Step (b) — event vocabulary committed in code** under
   `~/code/futon3/library/`. Codex-7 owns; ~2-3 days. Hard
   predecessor for M-interim-director Checkpoint 0 firing.
5. **M-interim-director Checkpoint 0** — first business-evidence
   inventory (Joe owns), attached to the operationalised-exploit-
   loop observations from steps 1-3 plus the UKRN-S apparatus
   evidence in npt. Converts the mission from HEAD-as-escrow to
   posterior-update source.
6. **Step (c)/(d) — projection wired into XTDB + WebArxana
   `arxana://view/interest-network` route** (codex-7 owns).
   Land in parallel with daily-until-done checkpoints.
7. **Bug-fix half-day sweep** (the four sibling patterns from
   2026-05-04; F6/F7 `next[...]` gap; `detect_drift.clj` run).
   Can interleave with step (c)/(d).
8. **Re-measure C1 bites in `:aif-stack`** — the bulletin-8
   prediction's actual check. WR-9 evidence.
9. **M-mission-coherence-patterns MAP → DERIVE** — turn the
   three-mission pilot into draft flexiargs for
   `library/mission-coherence/`, including HEAD-as-escrow
   (WR-15) and *explore/exploit-as-mission-pattern-axis* (the
   new pattern Joe surfaced in this conversation).
10. **Library hygiene sweep** (`@sigils`, `@references`,
    `library/README.md` skeleton). Deferred behind 1-6.
11. **PI step** with THE-STACK as observation — now with the
    M-interim-director checkpoint stream AND the mark2 /
    apm-lean telemetry channels available as observations
    (WR-14 + WR-16). Bulletin-8's recommended move, run on a
    foundation that finally has the observation channels its
    posterior was always supposed to be conditioned on.
12. **v3-runner build (~27 days)** — Stream A AIF-completeness
    on an external deliverable.
13. **M-interim-director close + validation-execution handoff**
    (June-August 2026). Revenue-evidence accumulates as
    `state/strengthened` / `state/addressed` events;
    `state/foreclosed` events name basins that didn't yield.

When the next bulletin is written, the questions to ask are:
*did the proxy-metric stream start flowing (Stack HUD shows
mark2 + apm-lean status; failure-rate discontinuity surfaced or
explained)? did step (b) land and did M-interim-director begin
emitting real checkpoints? does the 3-vector completeness signal
move under live evidence? did revenue-evidence — purchasable-work
evidence — actually accumulate? on the explore/exploit axis Joe
named, did any habit get genuinely offloaded, or did the
collection-loop relapse again?*
