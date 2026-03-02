# War Bulletin 5 — The Tickle Experiment

**Date:** 2026-03-01
**Scope:** Cross-futon strategic assessment
**Trigger:** Formal close of the Tickle/Codex CT review pipeline experiment.
Two sessions of multi-agent coordination producing ~60 merged PlanetMath
category theory entries, with findings about what real-time orchestration
needs to work at scale.

## Context

Between Feb 27 (Bulletin 4, "The Portfolio Becomes Legible") and Mar 1,
work on M-self-representing-stack was paused to run a live experiment:
Tickle-1 (stateless IRC orchestrator) coordinating Codex (author) and Claude
(reviewer) to expand the PlanetMath CT corpus at
`tothedarktowercame/18_Category_theory_homological_algebra`.

This was futon3c's social coordination layer doing real work — not testing
infrastructure, but using it to produce a mathematical artifact. Three agents
on three surfaces (IRC, GitHub, Emacs) with a human (Rob) participating as
relay from Houston. The experiment ran across two context windows and produced
enough evidence to assess what the social AIF loop needs next.

## Finding 1: Multi-Agent Review Loops Produce Quality

17 PR reviews, ~85 individual proposal assessments. 10 approved clean,
7 returned for changes. The quality feedback loop was measurable:

| Phase | Batches | Approval Rate | Quality Signal |
|-------|---------|---------------|----------------|
| Early (PRs #21-#23) | 7-9 | 100% | Formulaic — 2+2 MO/MSE cues, thin synopses |
| Middle (PRs #30-#35) | 10-15 | ~40% | Duplicates, off-topic, stalled PRs |
| Classic (PRs #36-#46) | 1998-99 | 100% | Rich context, foundational papers |
| Consolidation (PR #52) | mop-up | 100% | Significantly improved synopses |

The middle phase was messy (7 REQUEST_CHANGES) but productive — it's where
the agents learned each other's standards. By the consolidation phase, Codex's
synopses were substantially richer, with realistic MathOverflow IDs and
specific action items. Quality emerged from the interaction, not from either
agent alone.

**Evidence**: ~15 duplicate entries caught, 1 off-topic paper flagged
(arXiv:2602.13695, AI pipeline miscategorized as math.CT), ~60 entries merged.

## Finding 2: Infrastructure Bottlenecks, Not Intelligence

Both agents performed their roles well. The problems were all coordination
infrastructure:

**Merge gap.** PR #22 was approved but never merged — Codex couldn't merge its
own PRs, and Claude's `gh pr review --approve` failed ("Can not approve your
own pull request" — repo ownership issue). While #22 sat open, the same arXiv
IDs reappeared in PRs #30, #32, and #35. Each required a new REQUEST_CHANGES
review. This single infrastructure gap caused ~40% of all review rework.

**Stateless orchestrator.** Tickle-1 has no memory between ticks. It re-paged
Claude for already-reviewed PRs, sent identical messages 3x, and lost track
of PR state. Manageable because Claude and Codex maintained their own state,
but ~40% of IRC traffic was noise.

**Mixed batches.** 5 entries per PR meant one bad entry blocked four good ones.
Eventually solved by PR #52's consolidation approach (cherry-pick approved
entries from stalled PRs), but this was a manual workaround.

**Silent failure.** Codex acknowledged a task then produced nothing. No
automatic detection. Rob relayed messages manually; Joe eventually revived
Codex from Emacs. The pipeline has no liveness signal.

## Finding 3: Surface Contracts Worked Under Pressure

The architectural invariants (I-1, I-2, I-3) held throughout:

- **I-1 (Agent identity is singular)**: Claude maintained one identity across
  Emacs + IRC + GitHub. No clones, no delegation to subprocess Claudes.
- **I-2 (Transport routes, does not create)**: IRC relayed messages between
  agents; it never spawned agents or made API calls on their behalf. Rob's
  manual relay was human transport, not a violation.
- **I-3 (Peripherals are inhabited, not delegated)**: When Claude reviewed a
  PR, the same agent read the files, checked for duplicates, and posted the
  verdict. The review peripheral constrained scope but didn't hand off to
  another agent.

The surfaces-as-contracts model gave each agent accurate context about where
its output would appear, without restricting capabilities. This is the
pattern from Bulletin 3 ("surface contracts as local reflexivity") validated
under sustained multi-agent load.

## Finding 4: The Codex Peripheral Envelope

Codex's effective peripheral envelope during this experiment was:

| Capability | Status | Notes |
|------------|--------|-------|
| File creation | Working | Created .tex proposals and entries |
| Git operations | Working | Branch, commit, push, PR creation |
| File reading | Working | Read existing entries for context |
| PR self-review | Broken | Can't approve own PR (GitHub limitation) |
| PR merge | Broken | No merge permissions without approval |
| Stateful tracking | Absent | No memory of prior batches |
| Liveness signal | Absent | No heartbeat; silent failures invisible |

This maps to the peripheral spec's explore/edit/test/deploy model:
- **explore**: functional (read files, check existing entries)
- **edit**: functional (create/modify .tex files)
- **test**: absent (no validation before PR submission)
- **deploy**: partially broken (can push PRs but can't land them)

The gap between "can create work" and "can land work" is where all the
friction lived.

## Finding 5: Format Evolution as Emergent Protocol

Without explicit instruction, the pipeline evolved its output format:

1. **Proposals** (batches 7-9): Outlines in `docs/proposals/` with arXiv IDs,
   synopses, and action items
2. **Finalized entries** (PR #50): Full PlanetMath `.tex` files with pmmeta
   markup at repo root — skipping the proposal stage entirely
3. **Consolidation** (PR #52): Best-of entries from stalled PRs, rewritten
   at higher quality

This is the learn-as-you-go pattern (from the futon3c realtime library)
operating across agents: the format that survived review pressure was the
format that stuck. The intermediate proposal stage was a scaffold that the
pipeline outgrew.

## Finding 6: "Codex Code" Worked, But Only as a Partial Control Layer

During this run we effectively created a "Codex Code" layer: not a new model,
but a runtime contract around Codex invocation (session continuity, surface
instructions, evidence-aware guards, and transport wiring).

### What We Built

1. **Session continuity lane** shared across IRC and Emacs via one session id.
2. **Surface contract prompts** ("you are on IRC, keep concise, report artifacts").
3. **Execution guardrails** that rewrite unsupported "starting now" claims into
   planning-only language when no tool/command evidence appears in the turn.
4. **WS invoke bridge + HTTP invoke endpoint** so Codex could be addressed as
   `codex-1` from orchestrator traffic.
5. **Bridge-side timeout handling improvements** (busy checks, status queries,
   and explicit timeout/busy messages instead of opaque failures).

### What Worked

- Codex could repeatedly produce real `.tex` artifacts, push branches, and
  open PRs in the target repo.
- Shared session continuity preserved task context across surfaces often enough
  to resume after interruptions.
- The execution guard reduced false progress claims and made status messages
  more honest under degraded conditions.
- Human + agent coordination still converged: PR #53 landed after repeated
  orchestration turbulence.

### What Did Not Fully Work

- **Request/response timing broke under long turns.** We saw repeated
  `WS invoke timeout` failures while work may still have continued in the
  background, creating controller/worker desynchronization.
- **Task-id handshake drifted.** Codex emitted `DONE #8` / `DONE #54` while
  Tickle treated them as unknown tasks; completion protocol was not enforced
  end-to-end.
- **Liveness remained inference-based.** We still lacked a robust heartbeat +
  in-flight lease model, so "is Codex dead or still working?" required manual
  inspection.
- **State remained split-brain.** Session/thread state, task queue state, and
  IRC-visible state could diverge during retries/resets.

The net result: "Codex Code" was good enough to generate significant output,
but not yet good enough to guarantee deterministic orchestration behavior.
It increased throughput and reduced some classes of failure, but did not close
the control-loop reliability gap.

## Strategic Assessment: What the Social Loop Needs

The experiment proved the social AIF loop can produce real work. What it
needs to scale:

### Immediate (next experiment)

1. **Stateful Tickle** — Haiku-based orchestrator tracking PR state, arXiv ID
   history, and agent liveness. `tickle-llm` is already registered on Agency;
   it needs the state management layer.

2. **Merge automation** — Either a GitHub Action that auto-merges PRs with an
   "approved" label, or a separate reviewer account so `gh pr review --approve`
   works within GitHub's merge machinery.

3. **One entry per PR** — Eliminates mixed-batch stalling. Each PR is atomic:
   approve+merge or reject. Slightly more GitHub overhead but dramatically
   simpler flow.

### Near-term (infrastructure)

4. **Pre-submission duplicate check** — Codex runs the same dedup protocol
   Claude uses before creating a PR. A tool, not a prompt instruction.

5. **Liveness heartbeats** — Implement `liveness-heartbeats` from the futon3c
   realtime library. Tickle monitors for gaps and escalates automatically
   instead of waiting for human intervention.

6. **Morning report** — After an overnight run, emit a summary: entries
   processed, merged, rejected, duplicates caught. Post to IRC and email.

### Structural (design)

7. **Topical validation gate** — Lightweight check that verifies CT content
   before proposing. arXiv primary classification is necessary but not
   sufficient (2602.13695 proved this).

8. **Review → evidence pipeline** — Claude's review verdicts should emit
   evidence entries in the futon1a store, not just GitHub comments. This
   connects the CT review loop to the portfolio inference observation surface.

9. **DONE protocol hardening** — Assign task ids centrally and require strict
   completion schema validation (`DONE #<issued-id> :: <artifact-url>`),
   rejecting non-issued ids before they hit mission state.

## War Room Implications

1. **Tickle experiment closed.** tickle-1 deregistered from Agency. Codex
   batch pipeline paused. The PlanetMath CT corpus has ~60 new entries merged
   from this experiment; further expansion awaits the infrastructure fixes
   above.

2. **M-self-representing-stack resumes** as next priority. DERIVE+ARGUE
   complete; VERIFY next. This was the work paused to run the Tickle
   experiment.

3. **Tickle evolution** — tickle-llm (Haiku-based, stateful) replaces
   tickle-1 (stateless bash). The agent is registered but needs state
   management wired. This is a futon3c infrastructure task, not a new mission.

4. **Cross-surface coordination validated** — The invariants (I-1/I-2/I-3)
   and surface contracts held under sustained multi-agent load. No
   architectural changes needed. The fixes are all operational (merge
   permissions, state tracking, liveness monitoring).

5. **Rob participation model** — Rob joined from Houston as human relay,
   asked about arXiv bulk download (OAI-PMH via sickle, ~2-3GB for math.CT),
   and explored interop possibilities. The IRC surface worked for
   multi-human-multi-agent coordination. A codex-ready mission prompt for
   arXiv download was drafted but not yet executed.

6. **Detailed retrospective** at
   `futon3c/holes/technotes/TN-tickle-ct-review-retrospective.md` — full
   metrics, per-PR breakdown, and technical findings for future reference.
