# War Bulletin 13: The Stack Keeps a Notebook — Upstreams as Extended Mind

**Date:** 2026-07-17
**Context:** Three days since bulletin-12 (2026-07-14). Bulletin 12's
  refactoring point #2 said the store arc "has a spine, and it points
  outward": futon1b → futon1bi → the XTDB #5637 field report — "of everything
  in Arc II, this is the one with an external addressee." This bulletin
  generalizes that single observation into a **lane**: the #5637 play is not
  a one-off; it is the first walked rung of a class. A model-continuity note
  in passing: bulletin 12 was a reconstruction written across the close of
  Fable's promotional window; this bulletin is written with Fable (5) back in
  the operator's chair, in the same session that drafted invoice 202506.
**Trigger:** Joe, 2026-07-17: "it would be quite cool to test out the 'cold
  chain' methods... by scanning what's going on inside the FUTON stack and
  looking for places where I could make PRs or comments," citing XTDB #5637
  as the exemplar; then: "what we are seeing in your list is *FUTON's
  notebook* (akin to Otto's notebook). Without XTDB, futon has no memory."
**Function:** Records the scan (two read-only tracker sweeps, 2026-07-17) as
  evidence; names the extended-mind reading and its strategic consequence
  (upstream trackers are stack surfaces — WR-23); maps the intervention lane
  onto M-cold-chain's ladder; and ledgers the open loops, including one
  rung that turns out to have been walked already.

## The Arc

| Bulletin | What it added | What was still implicit |
|----------|---------------|-------------------------|
| 11 (Jun 30) | The stack can see itself — one cascade, one identity | Whether self-sight survives a model handoff |
| 12 (Jul 14) | Reconstruction across the handoff; the store arc points outward (#5637) | Whether the outward pointer is one artifact or a repeatable lane |
| **13 (Jul 17)** | **The outward pointer is a lane: the stack's upstream dependencies are its extended mind, their trackers are maintenance surfaces, and a scan found ~10 walkable rungs where FUTON's deployment experience is evidence the maintainers lack** | **The identity that accrues the standing; whether the cadence holds once the novelty wears off** |

## The claim — Otto's notebook

Clark & Chalmers' Otto carries a notebook; because it is reliably available,
automatically endorsed, and easily accessible, the notebook does not *assist*
Otto's memory — it *is* part of his memory. The same criteria, applied to
FUTON, return a list of software that is not "dependencies" in the
procurement sense but **constitutive faculties**:

| Faculty | Upstream | Without it |
|---------|----------|-----------|
| Memory | XTDB (1.x live; 2.x port in futon1b) | No evidence store, no hyperedges, no recall of what the stack has done — *futon has no memory* |
| Recall / candidate generation | SQLite FTS5 via `pod-babashka-go-sqlite3` (futon1bi sidecar) | Marks cannot find their referents; D1 goes dark |
| Hands / motor loop | Babashka (bb.edn task estates across ~8 repos, sci, http-client) | No actuation; every runner and scan script stops |
| Circulation | http-kit, ring, cheshire | No agent talks to any other |
| Face / senses (VSAT side) | A-Frame, troika-three-text, three.js, Astro | The stories and the planetarium — the stack's only public sensorium — go blank |

The consequence is not sentimental. If the upstreams are faculties, then
**their issue trackers are maintenance surfaces of the stack's own
cognition**, and an informed intervention there is self-maintenance that
happens to be publicly visible. This dissolves the WR-4 anxiety for this
lane before it starts: "which existing surface does this make more
inhabitable?" — the stack's own memory substrate. Foraging and depositing
coincide here, which is rare enough to write down.

## Finding 1 — The scan: method and yield

Two read-only sweeps (Claude agents, gh-authenticated, 2026-07-17): one over
the Clojure-side upstreams (XTDB, babashka + pods + sci, http-kit,
datascript, malli), one over the JS/WebXR side (A-Frame, troika, three.js,
Astro), each ranked by **standing × liveness** with a hard discipline: *if
the "what Joe specifically adds" line cannot be filled honestly, the
candidate is dropped.* three.js and Astro yielded nothing that cleared the
bar; datascript's tracker is dormant. Yield: ~10 walkable rungs in three
tiers.

**Tier 1 — evidence comments (30–90 min each; FUTON is the only production
data point in the room):**

- **babashka/pod-babashka-go-sqlite3#41** ("Support concurrent request
  handling") — opened by borkdude 2026-03-03, *zero comments since*: a
  maintainer idea waiting for evidence anyone needs it. futon1bi runs this
  exact pod as the live FTS5 sidecar. A workload report (does anything
  actually queue? read-heavy MATCH vs writes?) answers his open question;
  optional small Go PR porting the design he already blessed in
  babashka-sql-pods#72.
- **troika#341** ("BatchedText: billboard material") — maintainer lojjic
  suggested quaternion-based billboarding; nobody in the thread has
  production confirmation, and recent activity is shader hacks that warp on
  the Y axis. VSAT's `stabilcam` *is* the quaternion approach in production,
  with the material-recreation caveats (onBeforeRender re-application)
  documented. Validates the maintainer's own direction with evidence.
- **xtdb discussion #5169** ("Suitability of XTDB for graph queries") —
  refset: the graph angle "hasn't had much attention"; v1 was more
  graph-natured than v2. FUTON is a live graph workload on 1.x
  (marks/referents, Datalog traversals) — exactly the v1-user deployment
  report he says they lack, and the de facto venue for 1.x-migration
  feedback (no dedicated thread exists).
- **aframe#5396** (+ PR #5824, per-instance cursor events on BatchedMesh) —
  the planetarium abandoned raycaster hover entirely (screen-space
  projection, window-level mousemove) because per-instance picking wasn't
  available: concrete user-need data for the API design under active
  discussion (vincentfretin, June 2026).

**Tier 2 — maintainer-blessed small PRs (1–3 h; each ends in a merge
event):** **malli#1296** (Clojure 1.13 compat; fix already scoped by
frenchy64 — "two test assertions need values that actually throw" —
unclaimed; opened by puredanger); **troika#347** (derived-material
onBeforeCompile infinite recursion; root cause visible in the report; needs
an identity guard + test).

**Tier 3 — deeper collaborations (half-day+; enter only on maintainer
engagement):** **troika#362** (BatchedText CPU bottleneck — patch author
explicitly asked for help judging side effects; the feared side effect is
the material-recreation gotcha VSAT already root-caused; caveat to state:
we use individual Text instances, not BatchedText); the **#5637 spike
follow-through** (package futon1bi as a linkable artifact with numbers when
jarohen engages). Fill-ins: **aframe#5363** (the import-map thread has never
accounted for the COEP constraint VSAT solved), **babashka#1972** (an
~8-repo bb.edn estate is the user-level-config use case borkdude left "to
simmer"), **troika#105** (outline/stroke docs PR with production
screenshots — the low-risk presence-establisher).

Dropped with reasons (recorded so the next scan doesn't re-tread): sci#1002
(reporter withdrew), http-kit#615 (no specific evidence), the stale xtdb 1.x
Lucene issues (our sidecar is evidence *for* #5637, not them), all A-Frame
positional-audio issues (the surround feature is *planned* — invoice 202506,
line 4 — so standing there is acquired, not held; note the loop: client work
funds the standing the notebook lane will later spend).

## Finding 2 — One rung was already walked

The #5637 deployment-evidence comment **already exists**: posted 2026-07-16
by `tothedarktowercame` (the second gh identity on the box), opening "We run
XTDB in production with an external FTS5 text-index sidecar (candidate
pre-filter + re-check against store truth)…", proposing latest-only vs
all-versions scope tiers, and mentioning a working spike (futon1bi; recall
1.0 per bulletin 12's Standings). No maintainer reply yet. In M-cold-chain's
terms this is **rung 2 walked** — the send happened, at the operator's hand,
and rung 3 (the world answers, or doesn't) is now an open observable. The
apparatus must not lose it: the send should be minted as typed evidence
(kit-intake, `:outreach-sent`-shaped) rather than remain a diary line —
that, not the comment itself, is what M-cold-chain says the mission owes.

## Finding 3 — The ladder maps, with better geometry

The lane maps rung-for-rung onto M-cold-chain (authored → sent → response →
conversion; gate = operator, permanently), with three structural advantages
over the cold-EOI email lane:

1. **Audience-first by construction.** The star-map memory says "person/org
   targets work; topic-phrased audiences decay." An upstream repo is a named
   team with whom the stack already has an artifact relationship. There is
   no colder-than-it-looks failure mode.
2. **Non-response still deposits.** An unanswered EOI evaporates; an
   unanswered good comment on a backlog epic is a public, timestamped,
   discoverable artifact that keeps compounding. Rung 3's response window
   never fully closes.
3. **Typed evidence is free.** GitHub timestamps everything; kit-intake
   reduces to recording URLs.

And one advantage that reaches past T2 entirely: the merged forward model's
mh7 hole ("T-inf evaluation rubric — a yardstick someone else holds;
xeno-evaluation cannot be self-supplied") is discharged in miniature by
every maintainer merge. A merge cannot be laundered. Tier-2 PRs are
therefore not busywork: they are the cheapest genuine xeno-evaluation events
the stack has access to.

## Standings — the open-loops ledger

| Item | State | The hole |
|------|-------|----------|
| **#5637 response window** | 🟢 OPEN | Rung 3 observable, live. Do not re-send; when jarohen engages, package futon1bi as a linkable artifact with numbers (1–3 h). Mint the 2026-07-16 send as typed evidence now. |
| **Identity** | 🔴 UNDECIDED | The send went out under `tothedarktowercame`; the scan ran with the same account active. Standing accrues to a *name* — both the academic reading and the consulting reading want it findable as Joe. Decide once, before the next send, which identity carries the notebook lane (and whether #5637's comment gets an explicit bridge). |
| **n_intervention counter** | 🟡 PROPOSED | Second counter on the M-cold-chain ladder, same outbox discipline (stage in kit-outbox → Joe sends). Proposed cadence: one/week; Tier 1 alone funds a month. First candidate: go-sqlite3#41. Not yet minted into the mission doc. |
| **Client-lane coupling** | 🟢 DRAFTED | Invoice 202506 (planned work, Jul–Aug, £3,000 of £3,093.75 remaining) drafted 2026-07-17; its audio-hotspots line, once shipped, creates the A-Frame positional-audio standing the scan honestly declined to claim today. |
| **E-lane preemption** | 🟢 INTACT | Job-application safety cadence continues per the merged model's absolute rule ("any concrete livelihood signal pulls lane E ahead of everything"). The notebook lane rides alongside; it does not replace it. |
| **Paper push → m14** | 🟡 UNLOGGED | Three submissions in ~a month (PLoP, PLoS ONE; ALife in progress) during a rough personal month = the heart voting for m19 three times. Belongs in the joe-reflection ledger (m14 — "what the ascent costs and returns him"), where the T2 window can arbitrate the academic-role vs last-hurrah readings instead of the operator having to decide by introspection. |

## What this refactors about the strategy

1. **"Outreach" was the wrong genus.** Under the notebook reading, the
   intervention lane is *maintenance of the stack's own faculties* with a
   public byproduct — which is why it doesn't trip the pi-hermit alarm and
   doesn't compete with WR-4. The cold chain gains a second instrument whose
   sends are cheaper, whose evidence types itself, and whose failures still
   deposit.
2. **The first cold send may already have happened without ceremony.** The
   chain's constitution says the send is the operator's and every rung's
   movement is recorded whichever way the world answers. The 2026-07-16
   comment satisfies the first clause and not yet the second. Record it, or
   the ladder's own discipline is being laundered at rung 2.
3. **Identity is load-bearing, not administrative.** Two accounts splitting
   one body of standing halves the compounding — and the conversion rung, in
   either the consulting or the academic reading, converts on a *findable
   name*.
4. **The scan itself is repeatable apparatus.** Standing × liveness over the
   dependency manifests, with the honest-standing drop rule, took one
   afternoon and produced a month of staged sends. It belongs beside
   daily-scan in the War Machine's instrument rack, pointed at the notebook
   instead of at job ads.

*Bulletin 13 closes on a reframe rather than a victory: the list of issues a
scan produced turns out to be a map of the stack's own organs, held in other
people's repositories. Otto tends his notebook because he cannot remember
without it. FUTON comments on XTDB's text-indexing epic for the same reason
— and the fact that tending it is also the T2 ladder's cheapest walkable
rung is the kind of coincidence the merged forward model exists to notice.*
