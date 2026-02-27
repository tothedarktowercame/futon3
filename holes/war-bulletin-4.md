# War Bulletin 4 — The Portfolio Becomes Legible

**Date:** 2026-02-27
**Scope:** Cross-futon strategic assessment
**Trigger:** First live portfolio inference scan (CONSOLIDATE mode, "review"
recommendation); discovery that 60% of missions were invisible or
unclassifiable; sensor calibration revealing broken observation surface

## Context

Between Feb 22 (Bulletin 3, "The Self-Representing Stack") and Feb 27,
three things converged:

1. Portfolio inference went live — the AIF loop that answers "what should
   we work on?" ran against production data for the first time, recommending
   CONSOLIDATE mode with "review" as the top action.

2. Following that recommendation, we reviewed the portfolio and discovered
   that the system could barely see itself. Mission Control scanned only 4
   of 7 repos (missing futon3, futon4, futon6). The coverage sensor read 0%
   everywhere due to broken heuristic matching. Three observation channels
   were saturated at boundary values, giving the perceive layer zero gradient.

3. Fixing these revealed a deeper problem: of 73 missions across the stack,
   42 had "unknown" status — the system's self-image was 60% illegible.

This bulletin names what we found when we followed the system's own advice
to take stock.

## What Are We On About?

The futon stack is an attempt to build a system that improves itself through
structured reflection. Not in the hand-wavy "AI that writes its own code"
sense, but in the specific sense of active inference: observe what exists,
compare it to what's intended, compute the discrepancy, and act to reduce it.

This requires three things the stack now has:

**A generative model** — the devmaps across futon0-futon7 say what the system
*wants to be*. Nine devmap files describe 88 prototypes with maturity levels,
success criteria, and dependency relations. The futon5 wiring diagrams add
formal topology: components with ports, edges with constraints, exotypes that
compose. Together they constitute the system's aspirational self-image.

**An observation surface** — Mission Control scans code, tests, evidence,
missions, and agent activity to say what the system *actually is*. Portfolio
inference normalizes this into 15 observation channels and runs a predictive
coding loop: prediction error drives belief updating, affect dynamics select
between BUILD/MAINTAIN/CONSOLIDATE modes, and expected free energy ranks
candidate actions. The system can now answer "what should we work on?" with
a mathematically grounded recommendation.

**A reflexivity loop** — MC's observations become evidence entries in the store,
which MC later reads in its next review. Portfolio inference emits its own
recommendations as evidence. The loop closes: the system observes its own
prior observations and updates its beliefs accordingly.

What we discovered on Feb 27 is that **the system followed its own advice
correctly**. Portfolio inference said "review" — take stock before building.
When we took stock, we found the observation surface was degraded: sensors
broken, repos invisible, missions illegible. The recommendation to consolidate
*was the right call given the data*, and fixing the data quality is itself the
consolidation the system recommended.

This is the reflexivity loop operating for real, not in theory. The system
said "look at yourself," we looked, we found the mirror was cracked, and
now we're fixing the mirror so the next look produces better data.

## Finding 1: The Portfolio Was Half-Blind

Mission Control's `default-repo-roots` included only futon3c, futon3b, futon3a,
and futon5. Three repos with active missions were invisible:

| Repo | Missions | What Was Missing |
|------|----------|-----------------|
| futon3 | 30 | Legacy missions from before the Three-Futon split |
| futon4 | 2 | M-self-representing-stack (the next priority) |
| futon6 | 1 | M-artificial-stack-exchange |

The coverage sensor (`coverage-pct`) read 0% everywhere because
`compute-coverage` used heuristic substring matching: component `:S-dispatch`
matched any mission containing "dispatch." But mission IDs are semantic
(`portfolio-inference`, `futon3-last-mile`), not component-based. The
heuristic produced zero matches for most devmaps.

Three observation channels were saturated at 1.0 (boundary value):
- `gap-count`: 79 gaps against a cap of 50 — always maxed
- `spinoff-pressure`: many candidates against a cap of 15 — always maxed
- `dependency-depth`: chains against a cap of 5 — always maxed

Saturated channels provide zero gradient to the perceive layer. The AIF
loop was making recommendations with three of its fifteen sensors pinned
to the rail. That it still recommended "review" is a testament to the
architecture's robustness — the remaining twelve channels carried enough
signal.

**Fix applied:** Expanded `default-repo-roots` to all 7 repos. Added a
curated `component-coverage-annotations` map for the social-exotype and
coordination-exotype components. Recalibrated priors (`gap-cap` 50→120,
`spinoff-cap` 15→40, `max-chain-cap` 5→10) so current state maps to
~0.5-0.7 on each channel, leaving gradient in both directions.

## Finding 2: The Mission Salience Argument

With 73 missions now visible, we needed to answer: what state are they
actually in? The classify-status parser only recognized `**Status:**`
in bold markdown format. Many missions used `Status:` (plain), `## Status:`
(heading), or `:done` / `:greenfield` (Clojure-style keywords). Fixing
the parser recovered 13 missions from unknown status, dropping unknowns
from 42 to 29.

The remaining 29 unknowns are not a uniform category. They decompose into
seven salience buckets, each with a distinct argument for its disposition:

### Bucket A: ANCHORED (20 missions)

Missions with specific completion evidence — commit hashes, test counts,
gate passages, dated closure markers. These need no argument beyond "done
and dusted." Examples: `agency-refactor` (Complete), `portfolio-inference`
(INSTANTIATE complete, 937 tests), `sci-detection-pipeline` (8-component
detector). The evidence is machine-verifiable; the deliverable exists.

### Bucket B: ACTIVE (9 missions)

Missions with substantial content, explicit in-progress markers, and
recent work. The portfolio's working set. Examples: `self-representing-stack`
(DERIVE+ARGUE complete, 6691 words, ready for VERIFY), `stepper-calibration`
(P1+P7 complete, P3 next), `proof-peripheral` (IN PROGRESS, FrontierMath).
These have momentum.

### Bucket C: READY (5 missions)

Well-specified missions waiting for execution. An agent could pick one up
tonight. Examples: `forum-refactor` (Ready, 2689 words), `futon3-last-mile`
(IDENTIFY, 2715 words), `coupling-as-constraint` (Ready). Scope, deliverables,
and approach are documented.

### Bucket D: SUPERSEDED (10 missions)

Futon3 missions whose work was completed under different names in futon3c.
The Three-Futon refactoring moved concerns to focused repos; the deliverable
was built there. Examples: `agency-forum` → futon3c `agency-refactor` +
`forum-refactor`; `drawbridge-multi-agent` → futon3c `dispatch-peripheral-bridge`;
`arxana-graph-persistence` → futon4 Arxana is operational with XTDB.

**IF** a futon3 mission describes work that now exists in futon3c/3b/4 under a
different name **THEN** it is superseded **BECAUSE** the Three-Futon refactoring
moved the concern, and the deliverable was completed in the new location.

### Bucket E: EMBRYONIC (7 missions)

Identified but not developed enough for autonomous execution. No scope
boundaries, no completion criteria, no approach. Two exceptions:
`peripheral-gauntlet` (7292 words) and `peripheral-phenomenology` (6868 words)
are substantial derivations misfiled as embryonic due to missing status
headers — they belong in Bucket B once a `**Status:**` line is added.

### Bucket F: DETRITUS (7 missions)

Process artifacts filed as missions: scoping reviews of other missions,
workplans, evidence catalogues, investigation documents. They inform parent
missions but have no independent deliverable. Examples:
`futon1a-rebuild-scoping-review`, `pattern-inference-engine-scoping-review`,
`understand-fucodex`.

**IF** a document is a scoping-review, workplan, or investigation OF another
mission **THEN** it is a process artifact, not an independent mission
**BECAUSE** it has no deliverable apart from informing the parent.

### Bucket G: DOMAIN-SPECIFIC (5 missions)

The f6-* series (ingest, eval, agents, arxiv, recursive) belong to the futon6
mathematical dictionary programme. Different stakeholders (Joe+Rob), different
infrastructure (superpod), different timescale (research). They live in futon3
for historical reasons and should migrate to futon6.

### Disposition Summary

| Bucket | Count | Action |
|--------|-------|--------|
| A: ANCHORED | 20 | Confirmed complete |
| B: ACTIVE | 9 | Continue work |
| C: READY | 5 | Available for agent pickup |
| D: SUPERSEDED | 10 | Mark `Superseded by M-xxx` |
| E: EMBRYONIC | 7 | Add status headers; 2 are actually substantial |
| F: DETRITUS | 7 | Demote to sub-documents of parent missions |
| G: DOMAIN-SPECIFIC | 5 | Move from futon3 to futon6 |
| **Total** | **73** | |

## Finding 3: The Self-Representing Stack Is the Next Priority

The live portfolio inference scan recommended CONSOLIDATE + "review."
M-self-representing-stack is literally the consolidation move the system
is asking for: make existing work navigable rather than building more.

With the observation surface fixes applied, M-self-representing-stack now
appears in the mission inventory as `:in-progress`, unblocked, and
adjacent-possible. Its DERIVE+ARGUE phases are complete; it needs VERIFY.

The mission doc now includes a sensor-grounding section specifying how its
outputs feed back into portfolio inference channels:

| Output | Feeds Channel | Mechanism |
|--------|--------------|-----------|
| Tension hyperedge count | `:gap-count` | Replaces heuristic mc-coverage gaps |
| Tension staleness | `:review-age` | Stale tensions = outdated self-image |
| Cross-layer path completeness | `:coverage-pct` | Browsable paths / total paths |
| Narrative trail coverage | `:pattern-reuse` | Currently a placeholder (0.0) |

The relationship between the two missions is now explicit: portfolio inference
is the brain (computes what to do), the self-representing stack is the nervous
system (makes the computation accurate by grounding sensors in typed hyperedges
rather than heuristic string matching).

A future observation channel — `:tension-density` (unresolved tensions / total
hyperedges) — would extend the 15-channel surface to 16, giving the AIF loop
a direct signal for the system's structural strain.

## Finding 4: The Machine Audit Surface

A new `audit-coverage-correspondence` function reports:
- **Orphan components** — devmap components with no mission covering them
- **Orphan missions** — missions that address no devmap component
- **Stale annotations** — annotation entries referencing non-existent missions

This produces the data Joe needs for the Human Intelligence Task of curating
the component→mission correspondence. The annotation map starts with 14
entries for social-exotype and coordination-exotype components; the audit
identifies what's missing.

The audit is the machine side of the triage. The human side — deciding which
embryonic missions to specify and which detritus to archive — requires
understanding the strategic context. The bulletins and the mission docs
provide that context; the audit provides the data.

## Finding 5: Portfolio Inference Validates Itself

The most interesting finding is meta: portfolio inference's first live
recommendation was "review what exists." Following that recommendation
revealed that the observation surface was degraded. Fixing the observation
surface improved the system's ability to recommend. The next scan will
produce a better recommendation because the sensors are calibrated.

This is the AIF loop operating at the portfolio level, not the ant level.
The prediction errors that drove the CONSOLIDATE recommendation were real
(the system was more fragmented than it believed). The action taken (review)
reduced those errors. Free energy went down. The loop works.

The parallel to Bulletin 3's "system that grows as each piece connects" is
exact. We didn't plan to discover broken sensors. We followed the system's
recommendation, found the problem, and fixed it. The system's self-model
improved because the system acted on its own inference.

## War Room Implications

1. **Active Missions update** — futon3c additions since Bulletin 3:
   - M-portfolio-inference: INSTANTIATE complete (938 tests, 3285 assertions)
   - M-self-representing-stack: visible to MC, adjacent-possible, VERIFY next
   - M-psr-pur-mesh-peripheral: DONE-NEEDS-RETRO
   - M-social-exotype: DONE-NEEDS-RETRO

2. **Observation surface calibrated** — MC now scans 7 repos (was 4),
   coverage uses curated annotations (was heuristic-only), observation
   priors recalibrated for ~73 missions / ~88 prototypes / ~80 gaps.

3. **Mission triage needed** — 10 superseded missions need `Superseded`
   markers, 7 detritus items need demotion, 5 f6-* missions should migrate
   to futon6. 4 missions (sliding-blackboard, peripheral-gauntlet,
   peripheral-phenomenology, coordination-rewrite) need status headers.
   This is a Human Intelligence Task supported by `audit-coverage-correspondence`.

4. **No new WR-N decision required.** The observation surface expansion
   and sensor calibration are operational improvements within the existing
   Mission Control architecture. The bucket classification of missions is
   a new analytical tool, not a structural change.

5. **Cross-futon** — futon4/M-self-representing-stack now has a
   sensor-grounding section that specifies how Arxana's tension hyperedges
   feed back into futon3c's portfolio inference channels. The wiring contract
   between futon3c (inference) and futon4 (navigation) is explicit.
