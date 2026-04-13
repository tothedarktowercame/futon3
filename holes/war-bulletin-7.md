# War Bulletin 7: The War Machine Is Operational

**Date:** 2026-04-13
**Context:** M-war-machine (COMPLETE), M-webarxana (INSTANTIATE),
  M-daily-scan (Day 1). Two extended sessions spanning April 12-13.
**Trigger:** A disconnected WebArxana prototype led to building a
  full collaborative hypergraph surface, which led to the frame schema,
  which led to the daily scan pipeline, which produced the first
  depositing-direction frame. The War Machine is no longer a concept
  in a mission doc — it is running code that reads its own evidence.

## The Arc

Each bulletin added a capability. This one closes the loop.

| Bulletin | What it added | What was still manual |
|----------|--------------|---------------------|
| 1 (Feb 14) | Wiring diagrams operational | Strategic synthesis |
| 2 (Feb 18) | Evidence landscape | Cross-futon coordination |
| 3 (Feb 22) | Self-representing stack | Dynamic self-assessment |
| 4 (Feb 27) | Portfolio inference | Observation calibration |
| 5 (Mar 1) | Multi-agent quality loop | Infrastructure resilience |
| 6 (Apr 10) | Inhabitation threshold | The operator's own strategic view |
| **7 (Apr 13)** | **War Machine + daily scan** | **TBD — the loop runs** |

## Findings

### Finding 1: The War Machine Reads Its Own Evidence

The War Machine (`futon0/scripts/futon0/report/war_machine.clj`)
runs 10 scans across the futon stack: loop health, support/attack
balance, mission triage, graph structure, sessions, portfolio
inference, mission detail, patterns, AIF heads, and now **daily
scan frames**. It produces a 13-channel observation vector, infers
a strategic mode, and renders a four-view hex visualiser.

The loop promised in the holistic argument — `work → proof →
patterns → coordination → self-representation → better work` — is
now mechanised. Not automated (Joe still decides what to do), but
the observation and inference steps are computed, not manual.

### Finding 2: WebArxana Is a Collaborative Hypergraph Surface

WebArxana went from "No nema in focus" to a working multi-focus
canvas with d3-force layout, stacked pin cards, directional edges,
link annotations, diagrams (save/expand/compress), and real-time
WebSocket sync — in a single session. 49 commits to futon4.

It is the first surface where two people can co-create within the
Arxana hypertext. The ARGUE phase grounds it in 5 vsatelier
patterns. The plain-language argument: creative collaboration
requires both the freedom to explore and the ability to accumulate.

### Finding 3: Frames Are Financial Instruments on Mana

The frame schema (`futon5a/data/frame-schema.edn`) models strategic
state as a portfolio of instruments on a fictional quantity (mana):
- **Options** — missions (right to future capability)
- **Futures** — pattern activations (bets on approach)
- **Shorts** — sorrys (open obligations)
- **Yields** — AIF heads (installed infrastructure)

Cardinal directions (foraging, cargo, depositing, hermit) replace
failure modes. A healthy trajectory visits all four. The hermit
trap is zero angular velocity — stuck on one axis.

### Finding 4: The Daily Scan Pipeline Works

M-daily-scan Day 1 (`futon7/bb daily-scan`): 10 probes, 298 repos,
5 in intersection, 889 cache files. Workup produced 5 leads with
concrete next steps. 5 expansion probes generated from the workup.
3 new depositing patterns written.

The adapter pattern: scan → talk → frame. The conversation with
the agent IS the structured workup. Each daily hour produces a
frame as a natural byproduct.

### Finding 5: The Depositing Signal Exists

For the first time, the War Machine's observation vector has a
non-zero `:depositing-signal` channel. Day 1's frame shows:
```
{:hermit 0.15 :foraging 0.50 :cargo 0.30 :depositing 0.05}
```

Compare the retrospective frames (sessions faa3ea73 and 14459c97):
```
{:hermit 0.55-0.60 :foraging 0.30 :cargo 0.10-0.15 :depositing 0.0}
```

The vector is rotating. The income deadline (140 days) is being
addressed, even if no revenue has arrived yet. The mode diagnosis
transitions from `:hermit` to `:scanning`.

## WR Decisions

### WR-5: The War Machine Is Not a Mission

M-war-machine is COMPLETE, but the War Machine continues to run.
It is infrastructure, not a project. Like the pocketwatch or the
evidence store, it observes continuously. Future improvements
(new scans, new channels, new modes) are maintenance, not missions.

### WR-6: Daily Scan Is the Depositing Heartbeat

The 20-day M-daily-scan test is the first structured depositing
activity. If it produces at least one consulting conversation by
day 20, the 20h/month hypothesis has evidence. If not, the approach
needs revision. The War Machine monitors this via the frame
derivative pipeline.

### WR-7: Frames Interoperate or They Don't Exist

The frame schema's cross-model ports show that War Machine frames
depend on models that may not exist (futon7 business model, mana
accounting, pocketwatch operational). Missing models are sorrys.
Computing a frame and finding an unresolved port IS the detection
mechanism. Don't build the model until the port demands it.

## Status

```
S1 (Stack builds itself)        — STRENGTHENED (WebArxana built in one session)
S2 (Evidence observable)        — STRENGTHENED (frames + daily scan + WS sync)
S3 (Infrastructure resilient)   — STABLE
S4 (Self-representing)          — STRENGTHENED (War Machine operational)
S5 (Methodology transfers)      — STABLE

A1 (Observation half-blind)     — PARTIALLY ADDRESSED (13 channels, frame ports)
A2 (Abandoned missions)         — STABLE (M-war-machine COMPLETE, M-webarxana advancing)
A3 (Evidence waste)             — STABLE
A4 (Hermit trap)                — UNDER TREATMENT (daily scan, depositing patterns)
```
