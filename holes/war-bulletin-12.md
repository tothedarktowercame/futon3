# War Bulletin 12: Fabled Sessions — Two Arcs, One Reconstruction

**Date:** 2026-07-14
**Context:** Six weeks since bulletin-11 (2026-06-30). Bulletin 11 made the
  stack's operational self-model *real* — five dimensions composing on one
  canonical identity — and left the honest hole that a self-model must keep
  surfacing its own next work. This bulletin is a different kind of document.
  It is not a campaign narrative; it is a **reconstruction**. A long run of
  work was carried out across two arcs — the first before an interregnum in
  model availability, the second after — much of it by a model (Fable) whose
  promotional window has now closed. Opus takes over the operator's chair and,
  as the first act, walks the git history and the working trees to answer the
  operator's question: *what actually got built, and what did we drop on the
  floor?*
**Trigger:** Joe's "reconstruct all the things that happened… help me figure
  out if I missed anything important." The bulletin exists because the sessions
  were productive enough that no single head holds them, and because a handoff
  between models is exactly the moment self-knowledge gets lost.
**Function:** This bulletin doubles as a **ledger of open loops**. The Findings
  record what shipped; the Standings table records what is stranded, dormant,
  or un-armed — the work that will silently rot unless named. Foregrounding
  honest holes is house discipline (WR-9); here it is the entire point.

## The Arc

| Bulletin | What it added | What was still implicit |
|----------|---------------|-------------------------|
| 9 (May 17) | The stack authors its own pilot | Does the WM loop run unattended? |
| 10 (May 31) | WM demonstrated end-to-end; the manifold the stack needs | The single shared primitive dimensions converge on |
| 11 (Jun 30) | Operational self-model made REAL — 171 shared nodes, 0 conflicts; the blocker was identity, the fix is governance | Whether the honest-holes discipline survives a productive burst across a model handoff |
| **12 (Jul 14)** | **Reconstruction of two Fabled arcs: the Emacs peripherals (Arc I) and the store/harness/paper convergence (Arc II). A durable evidence store (XTDB 2), a text-index companion under live soak, the zai/zaif harness through ZU-4, the p4ng sequel, tunnel-free federation — and three pieces of work stranded off the main path.** | **The reconstruction is a snapshot; without a standing ledger it drifts the moment the stack moves again** |

Bulletin-11 said *the stack now writes its own map.* Bulletin-12 says *the map
had a gap the size of a model's promotional window, and reading it back is how
we find the work that fell out of the frame.*

## Arc I — The Emacs Peripherals (Jan–Feb seed, June revival)

The first arc gave agents a *body* in Joe's live Emacs and gave the operator a
*view* into the mission substrate.

### Finding 1 — Smart Emacs Cursor: proven, then switched off
A read-only Emacs peripheral that renders an agent as a visible `◆` cursor with
a caption, driven over WebSocket (`ws://localhost:7070/agency/ws`), under a
strict **non-editing invariant** — the agent can move and gesture but not mutate
buffers. Seeded 2026-01-29 as `M-emacs-cursor-peripheral` (futon3, greenfield);
implemented as `futon3c/emacs/smart-cursor.el` (~52 KB) with transport in
`futon-agency-ws.el` / `futon-agent-cursor.el`; E2E proof in
`futon3c/docs/technote-smart-cursor-external-e2e-handoff.md`. The June revival
(`M-smart-emacs-cursor`, 2026-06-11) extended it into a full spoken mission
loop: voice → Whisper (`futon0/contrib/voice-typing.el`) → agent → cursor
choreography → an on-screen mission with scopes → a posframe "Done, what next?"
speech bubble anchored at the cursor. **Status: proven prototype, currently
dormant** — `smart-cursor-mode` is OFF in the live Emacs; the revival reached
`:documented` with outstanding IOUs, and attention moved to federation.

### Finding 2 — Mission Mode: the operator's live scope panel, in use
`futon3c/emacs/mission-mode.el` (~63 KB, first committed 2026-06-09) — a
read-only Emacs view (`M-x mission-mode`) over substrate-2 mission-scope
hyperedges. Renders the eightfold-phase panel (HEAD/IDENTIFY/MAP/DERIVE/ARGUE/
VERIFY/INSTANTIATE/DOCUMENT, missing phases shown as ghost lines) with scope
bullets, PSR/PUR records, and concepts. Pipeline:
`futon6/scripts/mission_scope_detect.py` →
`futon3c/src/futon3c/scripts/mission_scope_ingest.clj` → substrate-2 →
`mission-scope-view-fast.sh` → panel. **Status: actively maintained** (last
edited 2026-07-10; added a vitals block + verify-gate chips). A sibling
`flight-mode.el` is the mission-mode analogue for War Machine flights. This is
the June–July realization of the Jan–Feb "mission / P0" workflow concept
(`P0-complete-mission.md`, 2026-01-27).

## Arc II — The Store, the Harness, the Paper (mid-June → today)

The second arc is a convergence: a durable store, a proof harness that grades
itself, and the paper that argues the whole loop — all feeding one demonstration.

### Finding 3 — XTDB 2 port: the durable store is real (futon1b)
`futon1b` is now the XTDB 2 migration repo. Core port + parity proven
2026-07-04 (P1–P3); full-store migration and query-layer-at-scale hardening
through 2026-07-11. The findings are the value: OOM/heap-spiral diagnosis
(GC/swap spiral, not sudden death; `-Xmx` bumps + `ExitOnOOM`), JSON-wire
support found live at Gate 2, and the projection/pushdown/windowed-hydration
pattern for corpus-scale reads. Runs as the `futon1b` store JVM on `:7074`
(evidence/hyperedges/memory-search), the sanctioned transitional second JVM.

### Finding 4 — Text-index companion under live soak: the XTDB #5637 play (futon1bi)
The FTS5 text sidecar (`M-text-sidecar`) landed in `futon1b` 2026-07-11
(SQLite FTS5, `unicode61`, no stemming; WAL so reads don't block during
rebuild), then was **extracted 2026-07-13 into `futon1bi`**, a standalone
companion that is the reference implementation for the **XTDB #5637** field
report and the `M-demonstration-foundry` F1' removal spec. **This is the live
WIP on the box**: a HTTP-seam soak harness is running now against `futon1b`
(outage-tolerant backfill, resumable; active `soak/state/` churn), and the
first oracle numbers landed today — recall 1.0. Deferred by design: HTTP
service wrapper, tombstone/delete for non-append-only tables, native XTDB
index adapter. This is the piece closest to an outward-facing artifact
(a field report on a real upstream issue).

### Finding 5 — The zai/zaif harness through ZU-4 (futon2 / futon3c)
`M-zaif-harness` worked all week. ZU-1 promoted a **status oracle** — mission
status *derived from the event stream* (turn-commits + chat turns), with
`:stale-header?` firing on disagreement between derivation and doc header
(one-curl callable; the pre-fix DRAFT doc is its own regression test). ZU-2
**calibrated the zaif controller by replay**, publishing the calibration gap
rather than silently tuning. ZU-4 added **tool-failure visibility + a
CI-in-the-loop bug queue** (cache-errors-to-bugfix-queue; false-pass = anti-
laundering applied to CI, with three mechanical transcript detectors from the
live zai-15 specimen). zai/zaif agents now **self-mark with M-points-de-fuite
glyphs** (futon3c c41ef33). The **40-flight assessment closed 2026-07-13**: S2
NOT MET (0.0625 vs 0.200 target), kill-criterion retired un-tripped, LOMO below
null — features diagnosis confirmed; recommendation **retire the S2 claim, keep
the apparatus, reposition GFN as a diversity supplier, and name reward as the
binding constraint.**

### Finding 6 — Reward learning: the engine under the paper (futon3a)
`M-composition-aware-reward` (reward_v0 → v1.2) and `M-mission-conditional-
reward` (reward_v2, kernel-conditioned reliability) built a learned R-hat with
LOO harnesses and hard anti-gaming gates. **Honest negative, foregrounded:**
reward_v2 LOMO 0.645 < v1.2 0.675, both below null; kill-test passes; 21/22
folds chose the narrowest tau (inner-LOO selects for memorization). This is the
empirical engine the p4ng sequel documents, and the "reward is the binding
constraint" verdict from Finding 5 is the same finding seen from the harness
side.

### Finding 7 — The p4ng sequel: the loop, preregistered (p4ng)
`p4ng/main-2026.tex` developed heavily 2026-07-05→11: the catalogue framed as a
learning loop (discharge-trained proposal), the semilattice policy ("a policy
is a semilattice, not a chain"), a preregistered full loop with a learned
proposer (§Preregistration, 2026-07-10), the "fourth memory / wiring corpus as
compositional memory" appendix, and a **zaif minimal PoC in Appendix A** with
the dependency-ordered routemap. Honest-scope moved "armed-but-latent →
exercised" (first live fold-escrow pass), with the one-label-wide transfer
shortfall retained rather than hidden.

### Finding 8 — Futon futures as a typed cascade (futon7 / futon2)
Under `M-futon-forward-model` (descriptive, not predictive: "where the next
~½-million LOC lands," on the thesis that in an open-source world the IP *is*
the unfinished-mission backlog). The 12–13 Jul work makes the backlog itself a
**typed cascade**: box = mission advanced, wire = enablement edge, psi = the
strategic want, **hole = a mission that should exist and doesn't**, terminals =
missions that discharge the want. Artifacts: `backlog-cascade-v0` (psi = the
general loop), `-stars-v0` (psi = the capability-star ladder), and `-merged-v0`
with the "best strategy ≈ a disciplined merge" method captured as
`HOWTO-backlog-cascade` + `N-multi-cascade-strategy` (futon2). Earlier personal
framing in `futon5a/speculative-futures-*.md` names the real constraint: the
Q1/Q2/Q3 income streams terminate ~Aug 2026 (~28 weeks).

### Finding 9 — Tunnel-free federation (futon3c) — the unmentioned heavyweight
Not in the operator's own recall, but the single largest recent code push:
`M-federated-agency-hardening` achieved **tunnel-free WS federation** — a live
end-to-end uplink to Chicago (2026-07-13), federation sync made durable across
stack restarts, park-delivery loss fixes (deadline backstop wakes the parked
agent; auto-bellback suppressed when a park awaits the job), and roster/uplink
status propagation. The `park` mechanism (bulletin-adjacent infra, see
`futon3c/README-park.md`) is now load-bearing for cross-turn work on the
ephemeral pouches.

### Finding 10 — futon5 cellular automata: reproduced, but off-branch
futon5 is the MMCA meta-evolution layer (edge-of-chaos search, exotype/xenotype
evolution, sigils back to futon3 patterns). The revival is **M-sci-reproduction**:
rigorously reproduce Corneli & Maclean, *The Search for Computational
Intelligence* (arXiv:1502.00130), with the 2014 `256ca.el` as ground truth
(where paper and code disagree, code wins and the discrepancy is logged as a
methods finding). Motive in Joe's words: futon5 is "home to a lot of bright
ideas that don't go anywhere… a computational version of my Mount Analogue
problem" — go back to the paper, reproduce with baselines, build up. **Status:
reproduction COMPLETE** (nb01–nb04, Clojure/Clay), published via the new
`M-lab-standard` protocol to `futon7a/lab/sci-repro/`.

## Standings — the open-loops ledger

*Read this as the honest-holes row of the checklist. It is meant to stay
populated; the discipline is that nothing here is a surprise later.*

| Item | State | The hole |
|------|-------|----------|
| **futon1bi text-index soak** | 🟢 LIVE | Running now; recall 1.0 today. Next: the deferred HTTP wrapper + tombstone/delete semantics before it is #5637-submittable. This is the closest thing to an outward artifact — keep it warm. |
| **APM Zai cron** | 🔴 UN-ARMED | `futon3c/scripts/apm_formal_zai_cron.py` (committed 2026-07-13) is a quota-gated Agency dispatch for the 489 UT Austin prelim problems, but **nothing triggers it** — no crontab, no systemd timer, no log, no `.state/apm-formal-zai/`. Its docstring claims "every 15 minutes"; that is aspirational. Also: verify the `apm-lean` output repo exists before arming. If the intent was a grinding formalizer, it isn't grinding. |
| **futon5 sci-reproduction** | 🟡 STRANDED | Complete, but on branch `warranted-play/2026-06-08-wiring` (tip `2133e2d`), not checked out or merged to main — which is why futon5 looked frozen at 2026-06-10. The public mirror (`futon7a/lab/sci-repro/`) only renders nb01/nb02, so it trails the branch. Decide: merge + finish the publish, or mark it a lab excursion. |
| **Smart Emacs Cursor** | 🟡 DORMANT | Proven and documented; `smart-cursor-mode` is OFF. Decide whether it is shelved or resuming — it has outstanding IOUs and will bit-rot against the moving `futon3c`. |
| **reward line** | 🟡 HONEST-NEGATIVE | reward_v2 below v1.2, both below null; inner-LOO selects for memorization. The apparatus is kept; the claim is retired. "Reward is the binding constraint" is the live question, not a solved one. |
| **backlog cascades** | 🟢 v0, ACTIVE | v0/stars/merged committed; the method is a reusable recipe. Sits under the still-open, deliberately-descriptive `M-futon-forward-model`. |

## What this refactors about the strategy

1. **A model handoff is a self-knowledge event.** The work did not stop when
   Fable's window closed; the *account* of it nearly did. The three stranded
   items (Findings 10, the APM cron, the cursor) were all one reconstruction
   away from being lost — not because they failed, but because nobody wrote down
   that they were waiting. WR-9's "empirically tested or marked logical"
   discipline has a sibling: *finished-but-unwired must be marked un-wired, or
   it reads as done.*

2. **The store arc has a spine, and it points outward.** futon1b → futon1bi →
   the #5637 field report → `M-demonstration-foundry` is a single load-bearing
   line ending in an artifact aimed at people outside the stack. Of everything
   in Arc II, this is the one with an external addressee; the ~28-week income
   cliff (Finding 8) argues for treating it as the priority, not the federation
   plumbing that felt urgent.

3. **The harness and the paper are the same finding twice.** "Reward is the
   binding constraint" arrived independently from the 40-flight assessment
   (Finding 5) and the reward-learning honest negative (Finding 6), and it is
   what the p4ng sequel now argues (Finding 7). When three surfaces converge on
   one constraint, that constraint is the next mission, not a footnote.

4. **This bulletin drifts unless it becomes a standing ledger.** The Standings
   table is a snapshot taken at 2026-07-14. Per WR-8's logic (canonical source,
   regenerated prose), the honest move is to let Mission Control / the
   backlog-cascade machinery *derive* this ledger, so the next handoff reads a
   live view instead of re-walking git. Until then, this is the manual snapshot.
   *Follow-on idea (Joe, 2026-07-14): make the War Bulletin series and the War
   Room chamber a **WM-owned documentation surface** — WM detects when a
   bulletin is due and delegates the draft, operator ratifies. Recorded as
   chain-link application #8 in
   [`futon7/holes/M-self-documenting-stack.md`](../../futon7/holes/M-self-documenting-stack.md)
   §9.*

*Bulletin 12 does not close on a victory; it closes on a handoff. Arc I gave
agents a body and the operator a view; Arc II built a durable store, a self-
grading harness, and the paper that argues the loop — and left three pieces of
finished work off the main path. The reconstruction's payoff is not the
inventory but the three flags: the un-armed APM cron, the stranded futon5
branch, the dormant cursor. Naming them is the whole job of a bulletin written
across a seam. The trail is warm; this is the map of it before it cools.*
