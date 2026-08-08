# War Bulletin 15: The Ants Are Epistemic — One Instrument, Three Customers

**Date:** 2026-08-06
**Context:** Seven days since bulletin-14 (2026-07-30). Bulletin 14 demoted the
  cash gate, named **witnessed external verdict** as the master gate, and — in
  its same-day revision — replaced the boolean gate with a **gain**: *how much
  of what comes back changes what gets built*, with `pi-hermit` redefined as
  outward mobility without coupling. It left the Henderson/XTDB chat
  (2026-08-05) as the next verdict window. The week since has been the most
  compute-saturated in the stack's history: a commissioned bare-metal server
  (256GB / 32 threads) running Z.ai Air for the superpod arXiv pilot, 40 prelim
  problems in flight on GLM agents wired to the custom memory system, a
  many-day MetaCA computational-control experiment, a PLoP revision shipped on
  shepherd feedback, and the last month of the day job fading into the
  background. Joe's own summary of the configuration: *"I am, as far as I can
  see, a full time computational research scientist now."*
**Trigger:** Joe, 2026-08-06: *"Things feel slightly out of hand at the moment,
  which is usually a sign that it is a good time to write a War Bulletin"* —
  followed, mid-conversation, by the question the bulletin turns on: *"what
  happened to the idea that the War Machine would work while I sleep?"*
**Function:** Measures the saturation claim against the machines themselves
  (both boxes probed live); records the Henderson call's conversion into a
  collaboration path; identifies the evaluation methodology as one instrument
  with three customers (superpod go-live, WM restart, consulting offer);
  sketches the APM × Arxiv.CT service architecture with **demand-paged mining**
  as its novel loop; and finds bulletin 14's gain parameter recurring, for the
  third time in three substrates, as the order parameter of the epistemic
  economy.

## The Arc

| Bulletin | What it added | What was still implicit |
|----------|---------------|-------------------------|
| 12 (Jul 14) | Reconstruction across the handoff; the store arc points outward (#5637) | Whether the outward pointer is one artifact or a repeatable lane |
| 13 (Jul 17) | The pointer is a lane: upstreams are the stack's extended mind, their trackers are maintenance surfaces (WR-23); ~10 rungs staged; rung 3 open | Whether anyone answers — and which identity accrues the standing |
| 14 (Jul 30) | Rung 3 fired (first reply of any campaign); the cash gate demoted for witnessed external verdict; the gate re-shaped from boolean to **gain** (coupling, not activity) | What supplies the discipline — and whether the freedom is spent on the ascent or on comfort |
| **15 (Aug 6)** | **Both machines measured full — and the binding constraint is neither CPU nor RAM but operator attention; the Henderson reply converted to a collaboration path; one evaluation instrument identified behind three blocked doors; foraging admitted as fully epistemic, demand-paged mining named as its loop; the gain parameter recurs in the epistemic economy** | **Whether memory and corpus compose (the 2×2 interaction term); whether the CA finds a shaping under which endogenous feedback is constructive; whether August converts quality-checking from an operator activity into a harness** |

## Finding 1 — The saturation claim, witnessed against the machines

Joe's opening estimate — "already pretty much maxing it (I think, we could
check that)" — was checked directly, by probe, 2026-08-06 ~08:13 local.

**The bare metal (zone-joe, 32 threads / 249GiB):** load average 30.3 on 32
threads, and the load is essentially **one process**: a single llama.cpp
server holding GLM-4.5-Air Q4_K_XL (111.5GB resident, `-t 30`, 32k context,
`--reasoning-budget 0`, ~36,000 CPU-minutes since Aug 5), plus a futon3c dev
JVM at Xmx12g. **CPU-saturated; RAM is not** — ~108GiB free, ~192GiB
available. The machine's binding constraint is tokens/second. Consequences:

- A second Air instance would *fit* in RAM but would halve throughput, not add
  capacity. The levers that raise the ceiling are batching against the one
  server, speculative decoding with a draft model, or Rob's GPUs.
- The idle RAM is not waste; it is **queued capital**. The Henderson call
  (Finding 2) explicitly wants 256G-machine testing for XTDB 2.2.x, and the
  two workloads contend on CPU but not on memory.

**The laptop:** at probe time, load 8.8 on 8 cores, swap 100% full, a Lean
process pinning a core alongside the graph-profile Emacs and the futon JVM.
Joe killed Firefox the same morning — "that should get us back into OK
operating territory" — so this reads as *was-saturated, now-OK*, but the
margin is thin: swap remains the tripwire.

The honest reading of both probes together: **the compute is full and
healthily employed, and the resource that is actually binding is the
operator's attention** — every job in flight currently routes its quality
gate through Joe. That observation is what the rest of this bulletin is
about.

## Finding 2 — The Henderson reply converted; the benchmark and the service are the same build

Bulletin 14 left the chat as the open verdict window. It was held 2026-08-05
(debrief: `futon7/data/outbox/staged/2026-07-26--james-henderson--xtdb-3663--jx5637/call-outcome-2026-08-05.md`).
Outcomes, compressed:

1. **Collaboration path agreed** — James is happy to collaborate on **#3663
   (user-specified secondary indices) first**, with Joe proposing a **#5637
   text-indexing benchmark** on top; candidate corpora are the superpod's
   arXiv harvest (9,916 math.CT papers, 22,721 relations; manifest verified at
   570,209 math papers with abstracts) and/or Stack Overflow with PostHistory
   (the chalk notes' rewrite-heavy stress case).
2. **A migration replaces a hardening** — 2.1.0 → 2.2.x (better auth); the
   fresh migration doubles as an ever-held-reset data point. Mission doc:
   `futon1b/holes/M-xtdb-22x-benchmarking.md`.
3. **SQL-first confirmed** — most XTDB users don't touch XTQL; XTQL `in` is a
   candidate first in-core PR. RSS ≈ 2×Xmx is *by design*, which retires a
   worry and creates the 256G-testing request Finding 1 can serve.

The strategic point sits underneath the logistics: **the benchmark corpus is
the service corpus.** Arxiv.CT feeds Henderson's benchmark *and* the APM
service of Finding 4; the text-sidecar retrieval engine (M-text-sidecar's
candidate pre-filter + re-check design) is the natural shared index for both
the memory store and the background corpus. One artifact, two customers — the
WR-23 lane and the commercial lane have converged on the same build, which is
the strongest form the notebook argument has yet taken.

## Finding 3 — One instrument, three customers

The answer to the trigger question — *what happened to "works while I
sleep"* — is precise, and it is not "the machinery is missing." The overnight
apparatus is built and live-verified (guardrails, needs-you emitter,
hole-counter, gate-runner; bounded autonomy ARMED; the star-map C3 gate
passed). What is missing is **measured capability**: Joe has not run the
machine overnight because he does not yet have a grounded sense of what it
can do. That is a measurement gap, not an engineering gap.

Now put three currently-blocked doors side by side:

| Door | What it is blocked on |
|------|----------------------|
| **WM sequel paper** (post-PLoP; "engineering more or less done, start rigorous evaluation") | An evaluation methodology for agent-system capability |
| **Superpod go-live** (pilot → sleeping-hours production runs) | A quality measure for mining runs: *"the ability to answer questions related to the corpus quickly and easily"* — ArSE and/or custom prelim batteries qualifying agents in specialist topics |
| **Consulting offer** ("Charlie could start doing sales for us, if we knew what we were selling") | A demonstrable, receipted claim that agent fleets do useful work on a client's corpus at measurable quality |

These are **one methodology with three customers**. The prelim-problem
battery — invented for Joe's own mathematics, currently 40 problems in
flight, with the capability star minting at ≈390 more (i.e. all remaining;
Joe: "at this point it's pretty procedural") — is the qualification
instrument in all three cases. The sequel paper does not have to "fit in with
everything else"; done right, it *is* the superpod QA design and the product
definition. Write once, spend three times.

**A WR-26 flag, raised here deliberately:** the WM overnight capability is
switched off without a written re-arm condition — exactly the stranded-flag
shape WR-26 exists to prevent, and this bulletin is the third to note the WM
"paused." Proposed countable condition, for Joe to accept or amend: *the WM
re-arms for overnight guardrails-mode runs when the evaluation instrument has
produced its first capability measurement on any fleet (superpod or WM), i.e.
one qualified cohort scored end-to-end without operator adjudication in the
loop.* Until a condition is written at the switch, the pause is a mood.

## Finding 4 — APM × Arxiv.CT: the service has an architecture, and its novel loop has a name

Joe's definition of the offering: **APM** = off-the-shelf LLM (on-prem or
API) + the custom memory system that makes it more efficient; **Arxiv.CT** =
custom NL and datamining building a background corpus. Still to prove: *"the
memory system and background corpus play nicely together."*

**Two stores, two epistemics — federate, don't merge.** Memory is
experience-derived: agent-written, high-precision, low-coverage, *proven in
use* (the load-bearing adjudication apparatus exists precisely to certify
this). The corpus is mined: batch-built, high-coverage, uncertain precision,
read-only to the agent. An answer grounded in a load-bearing memory is a
different epistemic object from one grounded in an extracted hyperedge; the
v1.2 receipts layer is where that provenance distinction lives.

**The seam already exists: the hunger channel.** The 08-03 hunger-audit
design (failed retrieval queries = demand signals) is the integration
architecture: a hungry query tries memory first; on miss, falls through to
the corpus; a corpus hit that proves load-bearing is promoted into memory by
the scribe pass, carrying the *asked* vocabulary as tags (demand-side
tagging, the discipline already established). Memory becomes a cache of the
corpus's proven-useful subset, warmed by actual problem-solving.

**And it runs the other way — demand-paged mining.** Hungry queries the
corpus cannot answer become **mining targets**. The superpod's Learn-As-We-Go
loop currently sharpens on supply-side signals (extraction gaps); the hunger
log gives it demand-side signals. The corpus build stops being a one-shot
batch job and becomes demand-paged: mine deeper where qualified agents are
starving. In Joe's words, this *"takes the ants foraging and admits that it's
fully epistemic in nature."* It is also the honest answer to what the service
sells: not a corpus, but a corpus that gets better where you actually use it.

**"Play nicely" is an interaction term, and the apparatus to measure it
exists.** The proof experiment is E2-shaped, extended to a 2×2:
memory-only / corpus-only / both / neither, on a CT-flavoured problem
stratum; DV = one-shot closure + token cost. The risk worth naming is that
the interaction is *negative* — corpus access diluting context or seducing
agents away from proven lessons toward plausible literature. Per the
critical-path discipline agreed with Rob's causal engine: encode the DAG and
estimands before the run. This rides the ≈390-problem cohort rather than
competing with it — a stratum of the procedural grind does double duty as the
integration evidence.

**Service layers, with the money argument located:**

| Layer | What | Commercial reading |
|-------|------|--------------------|
| L0 compute | On-prem llama server or API, swappable | On-prem cost is a tokens/second ceiling (Finding 1), so memory efficiency converts directly into served capacity — *the memory system is the on-prem pricing story* |
| L1 corpus build | Per-domain batch mining (superpod/GPU leg): NL pipeline, hypergraph, index | One-off deliverable |
| L2 memory | Per-engagement, starts near-empty, appreciates with use | Recurring revenue + lock-in: month three's agents are measurably better than month one's; the memory is client-specific IP |
| L3 qualification | Prelim battery + ArSE per domain | The receipts that make quality a number — Finding 3's instrument |
| L4 serving | Q&A + mining tasks over the federated retrieval surface | The visible product |

The human resources exist: Rob (more valuable than the superpod, Joe notes),
Charlie (50% consulting, 50% sales capacity looking for something to sell),
Joe (method + evaluation). The XTDB engagement is the template — research
capability → warm technical relationship → collaboration → paid path — and
the ~£5k MoU is the second data point. Per WR-25, the MoU acts on *ordering*
now and on *numbers* only when an artifact anchors it.

## Finding 5 — The gain parameter recurs: third substrate

Bulletin 14's revision found the order parameter of the CA family is **the
gain of the loop from phenotype back to genotype** (`TN-coupling-gain.md`),
applied it to the strategy layer (the verdict gate is a gain, not a boolean;
`pi-hermit` = outward mobility without coupling), and warned that the
*selective* half was unproven. The epistemic economy of Finding 4 is now the
**third substrate carrying the same parameter**: demand-coupling gain = *what
fraction of consumption-side signals (hunger, load-bearing verdicts) changes
what gets acquired (mined, remembered, indexed)*. Batch mining at any
throughput with the demand channel disconnected is the epistemic hermit —
activity level looks right, coupling is zero.

Joe's conjecture, stated in-session: the CA experiment may show *"we can
endogenously control the epistemic process"* — and where the CA makes this
hard by controlling something very abstract, the WM setting is hard too,
despite concrete textual and code artifacts, native-language for LLMs. The
bulletin's assessment: the hardness gradient does not run along
abstract→concrete. It runs along **the latency and cost of the error signal
that would let the system steer itself**. In the CA the coupling is fast but
semantics-free (you can measure everything and aim at nothing); in the WM the
semantics are rich but the steering observable is brutally expensive
("load-bearing" took an adjudication apparatus to measure at all, and V2's
verdict was that 38% is adjudication, not measurement). The hunger channel is
significant precisely because it is a **local, immediate, free** epistemic
error signal — arriving at solve-time, not adjudication-time. It is the
epistemic economy's candidate for what regime balance was in the CA paper
(where fractal dimension failed to discriminate interesting from homogeneous
and Shannon entropy of the regime mix succeeded).

The claim ladder, stage-named per discipline:

- **Proven:** foraging can be made demand-driven — the hunger audit works
  (E10's mid-solve hunger closed exactly; asked terms became tags).
- **Measured-negative:** *unshaped* endogenous feedback subtracts value — the
  repaired E5/E7 ablation: feedback-OFF predicted better (−0.149 bits/bit),
  more structures in 5/6 seeds. Closing the loop is not automatically good;
  the coupling must be shaped (block-system diversity in the braiding result;
  "step size is part of the operator" in WS2).
- **Open:** whether the running CA experiment finds a shaping under which
  endogenous control is constructive — this is what the many-day
  computational-control run is *for*, and not knowing how it winds up is, as
  Joe says, part of what makes it research.
- **Conjecture:** that the shaping transfers to the epistemic economy.

One boundary keeps the transfer safe, and it is where the WM setting is
*easier* than the CA: the two-loop design splits what can be endogenized.
**Where-to-look** (foraging routes, mining targets, retrieval) is safe to
hand to the dynamics; **what-to-want** stays exogenous by construction — I4,
water doesn't flow uphill, ants don't start parliaments. Demand-paged mining
endogenizes attention with the goal loop clamped. The CA has no such split —
one dynamics, everything coupled — which is why it is the harder instrument
and the right place to learn the shaping.

## Finding 6 — Sabbatical feasibility: yes, conditionally

The question as posed: is it reasonably possible that the jobs run
well-pushed-through the superpod during the ≈3-month mini-sabbatical
(Sept–Nov)? Assessment: **yes, on Joe's own stated logic** — quality-checking
de-risks, and any successful measured run de-risks further runs — with the
conditions made explicit:

1. **August must convert quality-checking from a Joe-activity into a
   harness.** Today Joe is the QA loop for both the prelims and the pilot,
   which is why the jobs consume all his time as well as all the compute. The
   de-risking only compounds if it leaves behind an automated gate (prelim
   batteries per corpus + ArSE queries with expected answers). That artifact
   is what makes sleeping-hours runs possible — for the superpod *and*,
   via Finding 3's re-arm condition, for the WM.
2. **The prelim completion is load-bearing beyond the star** — it is the
   first complete worked example of agent qualification, the template for the
   client-facing version.
3. **Known frictions to budget:** the ~30-min job-cap class of infra wedges,
   wall-clock-vs-stuckness false alarms, and the author≠reviewer discipline —
   correct, and where the hours go.

The sabbatical's stated dual aim — blue-skies research *and* a viable
consulting pipeline — is served by the same August artifact. A concrete
first-month deliverable proposal: a one-page offer document derived from the
completed prelims run (what was actually done, with receipts, priced) for
Charlie to test on the market.

## Standings — the open-loops ledger

| Item | State | The hole |
|------|-------|----------|
| **Prelims → star** | 🟢 PROCEDURAL | 40 in flight; star mints at ≈390 (all remaining). The cohort is also the carrier for the 2×2 stratum — preregister before dispatch reaches that stratum. |
| **Memory × corpus 2×2** | 🟡 PROPOSED | Not yet preregistered. DAG + estimands into Rob's engine first (the E2 discipline). The interaction term IS the "play nicely" claim. |
| **WM overnight re-arm** | 🟢 WRITTEN (2026-08-08) | Condition written at the switch (`futon3c/holes/WM-OVERNIGHT-RUNBOOK.md`): re-arm on first fleet capability measurement, one cohort scored with zero operator-adjudication events. Pause reason made specific: engineering complete, "so what" not instrumented. Points at `p4ng/science-2026.tex` + `supplement5.tex` (the miner as reference implementation for WM × eval-harness pairing). |
| **Bare-metal RAM (~190GiB idle)** | 🟢 QUEUED | Named consumer: XTDB 2.2.x benchmark (Henderson wants 256G testing; contends on CPU only). |
| **XTDB collaboration** | 🟢 CONVERTED | #3663 first, #5637 benchmark on the arXiv/SO corpora; 2.1.0→2.2.x migration planned; XTQL `in` = candidate first in-core PR. Mission: `futon1b/holes/M-xtdb-22x-benchmarking.md`. |
| **MoU (~£5k)** | 🟡 OPERATOR-REPORTED | Per WR-25: ordering-effective now, number-inert until the signed artifact lands. Client-facing work also due this month. |
| **CA control experiment** | 🟢 RUNNING | Open by design; the E5/E7 negative bounds the claim; draft4 text rewrite still pending behind it. |
| **Sequel paper** | 🟡 REFRAMED | From "Closing the Loop" (engineering-forward) to evaluation-forward: engineering taken as done, rigorous capability evaluation as the thesis. Same instrument as Finding 3. |
| **Laptop margin** | 🟢 RESTORED (thin) | Firefox killed 08-06; swap remains the tripwire while the Lean lane runs. |
| **Day job** | 🟢 FINAL MONTH | Fading into background; residual close-out items only. |

## What this refactors about the strategy

1. **"Which existing surface does this make more inhabitable?" now has a
   one-word answer for the eval work: three of them.** The evaluation
   instrument passes WR-4's test for the superpod, the WM, and the consulting
   lane simultaneously. It is not a fourth project competing for attention;
   it is the keystone the other three load onto. Prioritize accordingly.
2. **The commercial question inverted.** "If we knew what we were selling"
   assumed the product was missing. It isn't: the product is the receipted
   qualification loop (L2+L3), and August produces its first complete
   instance as a byproduct of work already scheduled. What's missing is only
   the write-up Charlie can carry.
3. **The gain parameter is now a design rule, not just a diagnosis.** Three
   substrates (CA coupling, strategy verdicts, epistemic economy) carrying
   one order parameter means new loops should be *born instrumented for their
   gain* — the demand-paged mining loop should log, from day one, what
   fraction of hunger signals change the next mining batch. **Minted as
   WR-27 (2026-08-06, Joe's call, same day).**
4. **Attention, not compute, is the scarce resource — and the model predicted
   this.** WR-24's comfortable-operator warning was about coupling, and the
   present configuration (every quality gate routes through Joe) is its
   benign-looking cousin: full mobility, operator-bottlenecked coupling. The
   answer is the same in both registers: build the gain into the harness so
   the coupling doesn't require the operator's continuous presence.

## Addendum (2026-08-08) — the evaluation settings are modular

Joe, two days on, making the instrument concrete: the APM problems are real
and currently ~35% **fully formally solved**, with work in progress to finish
on an almost-fully automated basis. **BPM (Berkeley Problems in Mathematics,
v3) is kept as the actual held-out test.** The demo shape: DjVu textbook in,
proofs out — fully automated proofs and upgrades from the APM baseline,
demonstrating proof capability and WM capability at once. The central *goal*
for the WM, however, is **capability in open-source programming** — a
naturalistic proof of value rather than a benchmark; "provably passes
Berkeley mathematics prelims" is the headline-grabber that the real-world
OSS milestones can follow. Toward WR-26: the evaluation settings **swap in
modularly** — the harness fixes the cohort definition, dispatch protocol,
receipts schema, scoring function, and the zero-adjudication requirement;
the *setting* supplies the problem source and the grader. BPM's grader is
the Lean kernel (immediate, countable, incorruptible); the OSS setting's
grader is the maintainer merge + CI (slow, sparse, and unlaunderable per
WR-23 — "a merge cannot be laundered"). The grader-latency difference is
itself the argument for the sequencing: the fast incorruptible grader
discharges the re-arm condition first; the naturalistic setting follows.

Two disciplines recorded at the point of commitment, because each is the
first thing a *Science*-grade reviewer will reach for:

1. **Held-out hygiene.** The split is clean only while BPM stays untouched —
   and the memory system is a contamination channel. Memories written during
   APM work are the learning claim; any BPM-derived memory poisons the test.
   What agents may retain *between BPM problems* must be defined in the
   preregistration, not decided at run time. (Base-model memorization of
   BPM's printed solutions is partially neutralized by the DV being formal
   closure with the delta measured against a same-model baseline — but say
   so explicitly, don't wait to be asked.)
2. **The kernel checks the proof, not the statement.** "Provably pass" rests
   on faithful formalization of the English problem statement — the one step
   the kernel cannot audit, and the known defect class (statement defects,
   vacuously provable; the six-step closure gate's NON-triviality check
   exists because f=0 slipped through once). Statement fidelity keeps
   author≠reviewer audit in the loop even in the "almost-fully automated"
   regime; the audit protocol belongs in the paper, not the appendix.

   **The protocol sketch (Joe's proposal + hardening, 2026-08-08):**
   round-trip each formal statement back to English and have an independent
   model judge equivalence against the original — with four provisos that
   convert the screen into a protocol:
   - **Inline local definitions before back-translation.** The 08-08 bulk
     statement-review finding: *every* miss was in a local def, not the
     statement proper. A judge shown a clean statement over opaque defective
     defs passes it. Batch small (~6) per the same finding.
   - **Calibrate the judge before trusting it.** The six known statement
     defects + the historical f=0 are a labeled defect set; add seeded
     mutations (swapped quantifiers, dropped hypotheses, ≤/< swaps, weakened
     conclusions) and report the judge's sensitivity/specificity per defect
     class. Escalation rule: judge disagreements + a random sample go to
     human author≠reviewer audit. ("A judge with measured error rates on
     seeded defects" survives review; "we used an LLM judge" does not.)
   - **Round-trip does not subsume the formal-side checks.** Non-triviality
     (a faithful statement can still admit a trivial witness — f=0's English
     round-trip would have read fine) and hypothesis-satisfiability
     (contradictory hypotheses back-translate innocently) stay as separate,
     cheap, kernel-side probes. The checks compose.
   - **Instrument the judge's gain (WR-27).** The disagreement log is a
     demand signal: where judge and formalizer disagree is where the
     formalization pipeline spends its next improvement pass. A disagreement
     log that never changes the pipeline is the uninstrumented-loop smell.
   The residual risk to state honestly: correlated blindness — formalizer
   and judge share training distributions, so *natural* mistranslations
   (implicit nonemptiness, boundary-strictness, typeclass conventions) can
   survive the round trip as a fixed point of the shared bias. Judge-family
   diversity reduces this; the seeded-mutation error rates *measure* it.

*Bulletin 15 closes where bulletin 7 opened, with ants. Bulletin 7 declared
the War Machine operational and made sessions into foragers; fifteen
bulletins later the foraging is admitted to be fully epistemic, the colony
has a second body it mines (other people's mathematics, not just its own
repos), and the open question has sharpened from "does it run?" to "can it
steer itself — and can we measure that it steers well?" The CA experiment,
the sequel paper, and the superpod pilot are that one question asked in three
languages. The machines are full; the instrument is next.*
