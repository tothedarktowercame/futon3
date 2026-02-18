# War Bulletin 2 — The Evidence Landscape Takes Shape

**Date:** 2026-02-18
**Scope:** Cross-futon strategic assessment
**Trigger:** Completion of futon3c mission control peripheral; devmap updated
with P12-P16; futon5 mission integration in progress

## Context

Between Feb 9 (the three-futon split, WR-1) and Feb 18, futon3c went from a
spec to a substantial operational system. The evidence landscape, peripheral
runtime, social pipeline, agency, transport, mission lifecycle, and mission
control are now wired and tested. This bulletin extracts the strategic
implications of having an operational coordination layer.

## Finding 1: The Coordination Layer is Real

Nine days ago, futon3c was a repo with social pattern specifications and an
empty test suite. Today:

| Component | Status | Evidence |
|-----------|--------|----------|
| Social pipeline | 5 stages wired | shapes, presence, auth, mode, dispatch, persist |
| Agency | Registry + sessions | Multi-agent dispatch, peripheral routing |
| Peripherals | 11 specs, 10 wired | explore, edit, test, deploy, reflect, proof, discipline, mission, mission-control, alfworld |
| Evidence landscape | Store + XTDB backend | append/query/reply-chain/fork; 7 evidence types; HTTP API via futon1a |
| Transport | HTTP + WS + IRC | 112 tests; F1-F6 IRC stability |
| Mission lifecycle | Cycle machine | 9-phase gates, obligation tracking, evidence snapshots |
| Mission control | Portfolio observation | Cross-repo inventory, devmap coverage, mana queries |
| **Total** | **614 tests, 1996 assertions** | **0 failures** |

This is not a prototype. The alleycat verification passed 4/5 gates including
P-4 (inhabitation race, 20/20) and P-6 (structural closure, 479+ tests). Tech
debt items (I3 snapshot enforcement, lossy payload coercion, hop exit inference)
are all resolved.

**Implication:** The three-futon split (WR-1) is empirically validated. futon3c
has an independent test suite, independent transport, and independent deployment.
It shares patterns (via futon3/library/, per WR-2) and evidence (via futon1a
XTDB) but is otherwise autonomous.

## Finding 2: Evidence Supersedes War Artifacts

The evidence landscape is the shared medium that Bulletin 1 and the war room
were proxying. Compare:

| Artifact | Before (prose) | After (evidence) |
|----------|----------------|-------------------|
| War bulletins | Markdown files in futon3/holes/ | `:evidence/type :coordination`, `:claim-type :observation`, `:ref/type :portfolio` |
| Mission status | `**Status:**` headers in .md files | Mission snapshots via `:mission-save` evidence; queryable by subject |
| Portfolio state | Hand-maintained inventory + next-missions.md | `mc-review` produces structured PortfolioReview with gaps, coverage, actionable list |
| Design decisions | IF/HOWEVER/THEN/BECAUSE in mission docs | Evidence entries with `:claim-type :conjecture` and argumentation tags |
| PSR/PUR records | Inline in session logs | `:evidence/type :pattern-selection` / `:pattern-outcome` with reply chains |
| Failed routes | Lost or buried in transcripts | `:evidence/type :gate-traversal` with structural obstruction body |

The evidence landscape doesn't replace prose — we still write mission docs and
bulletins because they serve a different audience (humans thinking strategically).
But the *queryable, structural* version of every coordination artifact now exists
as evidence. The landscape is the authority; prose documents are projections.

**Implication for futon3b:** Proof-paths produced by the gate pipeline should
flow into the evidence landscape. Today proof-paths are persisted as EDN files
in `futon3b/data/proof-paths/`. The bridge: on G0 success, emit the proof-path
as an evidence entry with `:evidence/type :gate-traversal`. This makes futon3b's
task-timescale work visible to futon3c's social-timescale queries.

**Implication for futon4:** The Arxana Lab viewer should read from the evidence
endpoints (`GET /api/alpha/evidence`), not from session JSON files. The evidence
landscape is the canonical source for PSRs, PURs, PARs, mission snapshots, and
portfolio observations. The viewer becomes a projection of evidence, not a
separate data path.

## Finding 3: Mission Control Makes the Portfolio Computable

Before mission control, answering "what should we work on next?" required:

1. Reading 15+ markdown files across 4 repos
2. Mentally tracking which devmap components have missions
3. Checking if nonstarter.db exists (it doesn't yet)
4. Correlating the implementation inventory with the devmap
5. Holding it all in your head

Now:

```clojure
(mcb/build-portfolio-review)
;; => {:portfolio/missions     [25 entries across 4 repos]
;;     :portfolio/devmap-summaries [9 structural summaries]
;;     :portfolio/coverage     [9 coverage entries with gap lists]
;;     :portfolio/mana         {:mana/available false}
;;     :portfolio/summary      "25 missions (N complete, N in-progress, ...)"
;;     :portfolio/gaps         ["component X — no mission", ...]
;;     :portfolio/actionable   ["mission-Y (ready) [futon3c]", ...]}
```

This is the `look` command for the portfolio. You see all the cabinets, what's
in them, and what's missing. The coverage heuristic (name-matching) is approximate
(gap G1), but even approximate visibility beats navigating by memory.

**Key design decision:** Mission control is read-only with respect to external
systems (D3, D4, D5). It observes devmaps, mana, and missions; it does not
modify them. This respects I4 (preference exogeneity) — the observation
peripheral cannot modify the constraint inputs it reads. The agent decides
whether to act on the review; mission control provides the perception.

**Implication:** The `mc-review` tool is the first consumer that reads across
all four repos and all three artifact types (markdown missions, EDN devmaps,
SQLite mana). When futon5 missions get included in the scan (in progress via
another session), the coverage analysis will become significantly more accurate
because devmap component names can be matched against the devmaps' own
`:mission/id` values instead of heuristic name-matching.

## Finding 4: The Devmap Reflects Reality

The futon3/holes/futon3.devmap was frozen at Feb 9 — the date of the three-futon
split. It had 12 prototypes (P0-P11) covering the original futon3 scope.
Everything built in futon3c since then was invisible to the devmap.

Updated today (commit `8b8301c`):

| New Prototype | What It Covers | Status |
|---------------|---------------|--------|
| P12: Social Pipeline & Agency | 5-stage pipeline, registry, bells, sessions | :active |
| P13: Evidence Landscape | Store, XTDB backend, 7 types, threads, HTTP API | :active |
| P14: Peripheral Runtime | 11 peripherals, runner, tools, hop, chains, backward verification | :active |
| P15: Transport Layer | HTTP, WS, IRC, F1-F6 stability, 112 tests | :active |
| P16: Mission System | Mission peripheral (cycle machine) + mission control (portfolio) | :active |

Also updated: P2 (gate pipeline — L1 canonicalizer operational), P3 (proof-path
store — typed rejection events), P7 (pattern workbench — promoted to :active),
P11 (self-description — futon5 devmaps + mission control coverage tools).

**Implication:** The devmap is now a reliable index of the stack. Mission
control's `mc-devmaps` tool reads these same devmaps programmatically. The
devmap → mission control → evidence landscape pipeline means structural specs
become queryable observations — closing a loop that was previously manual.

## Finding 5: WR-3 Was Followed and Validated

WR-3 (social exotype before implementation) mandated: "Write the social exotype
diagram and validate it before writing any futon3c implementation code."

This was followed. The social-exotype.edn was written and validated (8/8 checks
in futon5.ct.mission) before the social pipeline was implemented. The
M-social-exotype mission completed all three parts (standalone validation,
nested composition, gate-governed build).

The result: zero structural rewrites in futon3c's social pipeline. The 5-stage
pipeline (S-presence → S-authenticate → S-mode → S-dispatch → S-persist)
was designed from the exotype's component decomposition. Tech debt that did
arise (I3 snapshot enforcement, lossy payload coercion, hop exit inference) was
all at the integration boundary, not in the structural design. All three items
were resolved during the mission peripheral and mission control derivations.

**Comparison with pre-WR-3:** The original futon3 had three generations of
Agency code, an abandoned Forum implementation, and a perpetual bridge fix-cycle.
futon3c has one generation of each, all passing tests.

## Finding 6: The Three Timescales Are Connecting

The three-futon split was justified by timescale separation (I3). Nine days
later, the timescales are beginning to connect:

```
Glacial (futon3b L1)     Task (futon3b G5-G0)     Social (futon3c)
        │                        │                        │
   canonizes patterns      validates work          coordinates agents
        │                        │                        │
        └── patterns ───────────→│                        │
                                 └── proof-paths ────────→│ (pending bridge)
                                                          │
                                 ┌── evidence queries ←──┘
                                 │
                           futon1a (durable store)
```

What's connected:
- futon3c reads futon5 devmaps (cross-repo filesystem reads)
- futon3c writes evidence to futon1a (XTDB backend)
- Mission control scans futon3b missions (1 .md file found)
- futon3b's bootstrap closure runs through the gate pipeline with PSR/PUR exemplars

What's not connected yet:
- futon3b proof-paths don't flow into the evidence landscape
- futon3a similarity search isn't wired into G3 pattern selection
- Evidence landscape observations don't feed back into futon3b's L1 tension observer

These are the next integration seams. Each one is a mission.

## Strategic Assessment: What Comes Next

### Immediate (in progress)

- **futon5 mission integration** — Include futon5 EDN missions in mission
  control's inventory and coverage analysis. Being done by another session.
  This improves coverage accuracy significantly.

### Near-term

- **Proof-path → evidence bridge** — Emit gate pipeline proof-paths as
  evidence entries on G0 success. Small lift: futon3b already has typed
  proof-path events; futon3c already has `store/append*`. The bridge is
  a few lines of code at the gate boundary.

- **Arxana evidence viewer** — Wire futon4 Lab browser to the evidence
  HTTP endpoints. The plan exists (see plan file). PSR/PUR/PAR rendering
  with type-aware faces.

- **M-peripheral-gauntlet** — Real-time multi-peripheral scenarios. The
  peripheral runtime is wired but hasn't been exercised under realistic
  multi-agent load.

### Medium-term

- **futon3a → G3 integration** — Pattern similarity search as a gate
  input for PSR selection. The embeddings exist; the gate hook exists;
  the wire between them does not.

- **Evidence → L1 bridge** — Feed evidence landscape observations into
  futon3b's tension observer. This closes the glacial feedback loop:
  social-timescale evidence generates task-timescale tensions which
  produce glacial-timescale pattern evolution.

## War Room Implications

This bulletin informs the following operational updates:

1. **Active Missions (futon3c)** — The list in war-room.md is stale.
   M-social-exotype, M-agency-refactor, M-peripheral-model, and
   M-dispatch-peripheral-bridge are all complete. M-mission-peripheral
   and M-mission-control are complete. The active frontier is
   M-peripheral-gauntlet, M-sliding-blackboard, and the evidence
   landscape viewer.

2. **Cross-Futon section** — futon3c is no longer "social patterns + war room."
   It is the operational coordination layer with 614 tests. The description
   should reflect this.

3. **WR-3 validation** — The exotype-before-implementation approach produced
   zero structural rewrites in futon3c. This should inform future futon5
   missions: write the wiring diagram, validate it, then implement.

4. **New coordination need** — The proof-path → evidence bridge is a
   cross-futon effort (futon3b gate boundary + futon3c evidence store +
   futon1a persistence). This may warrant a War Room decision (WR-4).
