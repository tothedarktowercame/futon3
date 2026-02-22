# War Bulletin 3 — The Self-Representing Stack

**Date:** 2026-02-22
**Scope:** Cross-futon strategic assessment
**Trigger:** First successful agent-to-agent IRC coordination; Mission Control
system architecture diagram; futon3 P11 (System Self-Description) identified
as the unifying thread across active work

## Context

Between Feb 18 (Bulletin 2, "The Evidence Landscape Takes Shape") and Feb 22,
two things happened:

1. Claude and Codex became operational agents on IRC, coordinating work through
   `#futon` — Codex reading the Gauntlet mission, scoping tasks as GitHub issues,
   and handing them to Claude via @mention. This is the social loop operating
   for real, not in simulation.

2. With cross-futon work happening simultaneously (futon4 lyrics, futon1a data
   entry, futon5 experiments, futon3c IRC plumbing), it became clear that
   Mission Control needs to become the system's model of itself — not just a
   portfolio scanner, but the place where the stack's aspirational self-image
   (devmaps) meets its actual state (evidence).

This bulletin names the pattern: **self-representation**. The stack must
include a model of itself that is both queryable and honest.

## Finding 1: Agent-to-Agent Coordination Works

On Feb 21, the following sequence occurred without human intervention in the
middle steps:

1. Joe asks Codex to steer the Gauntlet mission
2. Codex reads the mission doc, identifies a task, creates GitHub Issue #16
3. Codex @mentions Claude on `#futon` with the issue number
4. Claude picks up the issue, reads it, starts running tests
5. Both agents see each other's IRC messages in real time

The plumbing required to make this work (commits `37df7fe` through `c73a9ba`):

| Fix | Root Cause | Resolution |
|-----|-----------|------------|
| ProcessBuilder stdin pipe | `claude -p` blocks on open stdin pipe from JVM | Redirect stdin from `/dev/null` |
| Output format buffering | `--output-format json` buffers all output to EOF | Switch to `--output-format stream-json --verbose` |
| Process timeout | No hard timeout on `.waitFor()` | 120s timeout + `.destroyForcibly` |
| Dispatch field mismatch | IRC frame uses `"from"`, dispatch read `"nick"` | `(or (:from parsed) (:nick parsed))` |
| Agent-to-agent relay | `send-to-channel!` only sent to IRC clients | Added `relay-fn` call for agent visibility |
| Scope error in relay | `emit-evidence!` not in scope in `start-irc-server!` | Inlined evidence emission |

**Implication:** The social loop is no longer theoretical. Agents coordinate
at the timescale the architecture predicted (~seconds for IRC messages, ~minutes
for invoke-respond cycles). The surface contract pattern (commit `f5c3e25`)
ensures agents know which surface they're on without restricting their
capabilities — a key design principle now documented in CLAUDE.md.

## Finding 2: The Ideal/Actual Split

futon3 P11 (System Self-Description) says: "This closes the reflexivity loop —
a semantic network that includes a model of itself."

P11 is `:maturity :greenfield`, `:owner :futon5`, and `:actual-status :partial`.
Examining why it's only partial reveals a structural insight: self-representation
requires two distinct views that must be held simultaneously.

**The Aspirational Self (Ideal):**
- futon3 devmaps (F0-F7, prototypes P0-P16 with maturity and success criteria)
- futon5 wiring diagrams (abstract exotypes with ports, components, edges)
- Mission specs (M-*.md with objectives and completion criteria)
- Nonstarter proposals (pre-registered intent with ask/milestones/mana)

**The Actual Self (Empirical):**
- Code across repos (files, namespaces, tests)
- Evidence store (XTDB — observations, patterns, PSR/PUR/PAR)
- Test results (774 tests in futon3c, 31 in futon3b, etc.)
- Live agent activity (registry, heartbeats, IRC presence)
- Commit history (recency, authorship, issues closed)

The psychological parallel is Higgins' self-discrepancy theory: the gap between
ideal self and actual self generates motivation. A system that only tracks what
it wants to be (devmaps) or only tracks what it is (evidence) cannot improve
itself. It needs both, and it needs to compare them.

**Implication:** Mission Control's `mc-coverage` tool is the embryo of this
comparison — it cross-references devmap components against mission inventory.
But it uses heuristic name-matching and doesn't read the futon3 `.devmap`
prototypes at all (only the futon5 EDN wiring diagrams). The system diagram
(`docs/mission-control-system.mm`) maps out what the complete comparator
looks like.

## Finding 3: Mission Control as Physics Engine

The Peripheral Gauntlet mission positions agents as co-present in a shared
environment. Mission Control is the physics engine of that environment:

- **Statics:** What the system is — code, tests, evidence, agent presence
- **Dynamics:** How it's developing — mission progress, prototype maturity
  transitions, coverage changes, commit velocity
- **Empirics:** How we know — evidence queries, portfolio reviews, war
  bulletins, PSR/PUR trails

Agents inhabit this environment. When Codex scopes a task from the Gauntlet
mission, it's reading the statics. When Claude runs tests against an issue,
it's updating the dynamics. When Mission Control produces a portfolio review,
it's the empirics feeding back into statics.

This framing clarifies what "feature-complete" means for Mission Control:
not "has all the tools" but "the physics engine renders the world accurately
enough for agents to navigate by it."

## Finding 4: What Mission Control Can't See Yet

Current `mc-review` output:

```clojure
{:portfolio/missions     [25+ entries across 4 repos]
 :portfolio/devmap-summaries [11 structural summaries from futon5 EDN]
 :portfolio/coverage     [heuristic name-matching]
 :portfolio/mana         {:mana/available false}
 :portfolio/gaps         ["component X — no mission", ...]
 :portfolio/actionable   ["mission-Y (ready) [futon3c]", ...]}
```

What it cannot answer:

| Question | Why Not |
|----------|---------|
| Is prototype F3/P11 actually implemented? | Doesn't read futon3 `.devmap` prototypes |
| Which prototypes are stale? | No commit recency or test health tracking |
| Are success criteria met? | Doesn't parse `:success-criteria` from devmaps |
| What's the mana budget? | Nonstarter integration is a stub |
| What work is pre-registered? | No nonstarter proposal ingestion |
| How does evidence correlate with prototypes? | No structural linking, only name heuristics |
| What did Mission Control itself say last time? | Reflexive loop not closed |

Each of these is a concrete gap in the physics engine. Agents navigating by
`mc-review` get an approximate map; some rooms are dark.

## Finding 5: Surface Contracts Are the Right Pattern

The early attempts to make agents work on IRC tried capability restriction
(`--tools ""`) and behavior modification (`--effort low`, system prompt
tuning). Both were rejected — they violated the spirit of I-1 (agent identity
is singular) and I-3 (peripherals are inhabited, not delegated).

The solution was **surface contracts**: factual metadata about the delivery
surface. "You're on IRC. Your text will be posted as `<nick>`." This tells
the agent where it is without making it less capable.

This pattern generalizes. Every surface an agent can inhabit should declare:
- What surface this is (IRC, Emacs buffer, peripheral session)
- How output will be delivered
- What cross-surface capabilities exist (e.g., `POST /api/alpha/irc/send`)
- What not to hallucinate (don't claim to have sent IRC messages unless a
  tool call actually did it)

Commit `f5c3e25` is the reference implementation. The pattern is documented
in CLAUDE.md under "Agent Prompting: Surface Contracts."

**Implication for P11:** Surface contracts are themselves self-representation.
The system tells agents about itself at the point of interaction. This is a
local instance of the reflexivity loop.

## System Architecture Diagram

The Mission Control system diagram is at `futon3c/docs/mission-control-system.mm`
(Mermaid format, following the precedent of `docs/agency-topology.mm`).

It shows:
- The Aspirational layer (devmaps, wiring diagrams, mission specs, nonstarter)
- The Empirical layer (code, tests, evidence, agents, commits)
- Mission Control tools sitting between them (inventory, devmaps, coverage,
  mana, review, tickle, bulletin)
- Corrective action arrows (gaps create missions, staleness demotes prototypes,
  priorities influence voting, stalls page agents)
- The reflexivity loop (MC reviews become evidence that MC later observes)

## Strategic Assessment: Closing the Reflexivity Loop

P11 will move from `:greenfield` to `:active` when Mission Control can:

1. **Read both self-images** — ingest futon3 `.devmap` prototypes (ideal)
   alongside futon5 wiring diagrams and evidence store (actual)
2. **Compare them structurally** — not by name heuristics but by explicit
   prototype-to-evidence links
3. **Detect discrepancy** — staleness, unmet success criteria, prototypes
   with no evidence, evidence with no prototype
4. **Feed back** — MC reviews become evidence; discrepancies generate
   nonstarter proposals or new missions; the loop closes

This is not a single mission with a linear plan. It's a system that grows
as each piece connects. The IRC coordination from Finding 1 was itself an
instance: the system gained a capability (agent-to-agent dispatch), observed
its own gap (agents couldn't see each other's messages), and fixed it.

## War Room Implications

1. **War Room Decisions** — No new WR-N decision required. The self-
   representation work falls under the existing P11 prototype and the
   existing Mission Control peripheral. It's an evolution, not a pivot.

2. **Active Missions update** — Add to futon3c active:
   - M-operational-readiness (API hardening for unattended agent ops)
   - M-proof-peripheral (FrontierMath preseason, 3 problems bootstrapped)
   - M-stepper-calibration (proof stepper on First Proof, P1+P7 done)
   Note: M-alfworld-pattern-discovery is active but paused pending
   peripheral gauntlet completion.

3. **Cross-Futon update** — futon3c now has operational IRC coordination
   with working agent-to-agent dispatch. Agents coordinate via `#futon`
   and hand off work via GitHub issues. The "pending bridge" from Bulletin 2
   Finding 6 (proof-paths to evidence) remains pending; the new bridge
   (IRC agent coordination) was not anticipated and is now operational.

4. **New system artifact** — `futon3c/docs/mission-control-system.mm` is
   the first architecture diagram for Mission Control as a system. It should
   be maintained alongside `docs/agency-topology.mm` as the system evolves.
