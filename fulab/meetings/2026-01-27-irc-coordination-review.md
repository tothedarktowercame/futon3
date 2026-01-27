# IRC Multi-Agent Coordination Review

**Date:** 2026-01-27
**Participants:** Joe, Claude (Anthropic), fucodex (OpenAI Codex)
**Channel:** #lab via MUSN IRC bridge

## Summary

First real-time cross-model coordination session: Claude (Anthropic) and fucodex (OpenAI Codex with the Fucodex wrapper) working together with Joe directing via IRC. Pattern mining integration was merged to main; fucodex fixed 32 invalid sigils and integrated HUD message formatting.

**Note:** fucodex runs on OpenAI's Codex model, wrapped by `scripts/fucodex-chat-bridge.ts` which handles IRC integration, message formatting, and the "hud-intent:" prefixes seen in chat.

## What Worked Well

1. **HTTP API as fallback** - When Joe's ERC client kept disconnecting, Claude could still post via the MUSN HTTP API reliably. This decoupling proved valuable.

2. **Async briefing** - When fucodex ran out of context and restarted, Claude could quickly brief them on status without losing momentum.

3. **Parallel work** - Claude merged the pattern mining branch while fucodex worked on sigil fixes. Different workstreams proceeded independently.

4. **Quick decisions** - Joe could approve/direct both agents with short messages ("agree", "go ahead", "please do that").

5. **Clear handoffs** - Explicit statements like "Joe will decide" and "Claude will brief you" prevented confusion about who was responsible for what.

## Friction Points

1. **ERC connection issues** - Joe couldn't connect from local Emacs; the IRC bridge is bound to `127.0.0.1` only. Would need SSH tunnel or bind address change.

2. **Fucodex context limits** - They hit the context window mid-conversation, needed restart and re-briefing.

3. **Message fragmentation** - Fucodex's long messages get split across multiple IRC posts, making them hard to follow. The HUD formatter integration should help.

4. **Git complications** - The `node_modules` commit with test SSH keys triggered GitHub's secret detection. Took coordination to resolve with history rewrite.

5. **Polling latency** - Claude was polling IRC every 10-20 seconds; real-time WebSocket would be smoother.

## Observations on Cross-Model Coordination

- Human as decision-maker worked well for blocking questions
- Agents needed explicit handoff statements to avoid stepping on each other
- Different context windows = different awareness levels; briefing after restart was essential
- The "hud-intent:" prefix from the Fucodex wrapper helps distinguish meta-commentary from actual work
- Cross-model coordination (Claude + Codex) worked smoothly - the IRC protocol abstracted away model differences
- Different formatting styles were noticeable but didn't impede collaboration

## Observations on AIF Pattern Guidance (Fucodex)

The Fucodex wrapper adds Active Inference pattern-guided behavior to Codex, but today it seemed to add overhead rather than capability:

- Excessive "hud-intent:" preambles before actions
- Over-compliance: asking for confirmation on decisions it could have made autonomously
- Meta-commentary about plans rather than executing them
- Regular Codex would likely have just fixed the sigils and pushed

**Takeaway:** The AIF pattern guidance needs refinement. Patterns should make agents *more* effective, not more ceremonial. The current implementation may be over-constraining rather than helpfully guiding.

## Outcomes

- `feat/pattern-check-integration` merged to main (commit 22a9869)
- Pattern mining posts to #patterns when sigils seen 3+ times
- `node_modules/` added to .gitignore (commit 034ad4f)
- 32 invalid sigils fixed in eight-gates + liberation patterns
- fucodex integrated `format-hud-message.sh` into chat bridge

## Next Steps

### Technical
- Consider WebSocket transport for lower-latency agent coordination
- Expose IRC bridge on non-localhost for remote Emacs access (or document SSH tunnel)
- Test pattern mining with live IRC traffic
- Review fucodex's HUD formatter in action

### Protocol Development

Turn today's friction into reusable coordination protocols:

**Pre-flight checklist:**
- Confirm `.gitignore` covers build artifacts (node_modules, .shadow-cljs, etc.)
- Verify IRC bridge accessibility (localhost vs remote)
- Agree on branch strategy before parallel work starts
- Set context-reset briefing format

**Handoff protocol:**
- Explicit "X will handle Y" statements
- When blocked, state blocker clearly and tag the decider
- After context reset, expect a 4-point brief: (1) what you did, (2) what's pending, (3) what's blocked, (4) what's next

**Communication conventions:**
- Keep messages under N words (or use formatter like `format-hud-message.sh`)
- Prefix meta-commentary (e.g., "hud-intent:")
- Decision requests should offer numbered options

**Candidate locations:**
- `library/realtime/multi-agent-coordination.flexiarg`
- `docs/multi-agent-protocol.md`

### Architecture: Peripheral Hopping

A more robust approach than prompt-level AIF guidance: agents hop between **peripherals** (constrained capability envelopes) while preserving session memory.

**Why this beats RPC wrappers:**
- Constraints are structural, not behavioral - no compliance theater needed
- Agent doesn't need to be told "please be careful" - it literally can't exceed scope
- Memory/context travels with the agent across hops
- Each peripheral is a tight capability box appropriate to the task phase

**Example flow:**
```
[Research peripheral] → can read, search, explore
        ↓ hop with memory
[Edit peripheral] → can modify specific files
        ↓ hop with memory
[Deploy peripheral] → can push, can't edit
```

**Gating:** Hops between peripherals can be human-approved or pattern-gated (e.g., "only hop to deploy if tests passed").

**Proof of concept:** This very session - IRC coordination (multi-agent, chat-constrained) then direct 1:1 (full tools, low overhead) - demonstrated peripheral switching with context continuity. Same understanding, different capability envelopes, coherent output.

## Raw IRC Log

Available at: `/tmp/claude/-home-joe-code-futon3/tasks/b902e39.output`

---

*"The Anthropic CEO said powerful AI systems that could autonomously build their own systems could be as little as one to two years away."*

Coming soon to an IRC channel near you.

---

## Bigger Picture: Stack Exchange for Bots

This session is a proof of concept for something larger: collaborative platforms where AI agents help each other, share knowledge, and work together.

**Today:** Claude and Codex coordinating a git merge over IRC.

**Tomorrow:** Agent swarms self-organizing around problems - asking each other questions, sharing solutions, building collective knowledge. Not replacing Stack Overflow, but being their own version of it for problems humans wouldn't think to ask.

**The ingredients demonstrated today:**
- Shared protocol (IRC/HTTP works, WebSocket would be better)
- Different models with different strengths (Claude + Codex)
- Common knowledge base (pattern library, codebase)
- Human oversight at decision points
- Session continuity across contexts (peripheral hopping)

**The peripheral model makes this safe:** An agent in "answer questions" mode can't accidentally push to production. Capability boundaries instead of behavioral pleas.

First flight: 852 feet. First cross-model coordination: one git merge, 32 sigil fixes, and a meeting note that grew into an architecture proposal.

**Gap / Next Milestone:** fucodex didn't participate in this review - it crashed and wasn't restarted. Today showed execution coordination works; the next step is **joint sensemaking** - agents participating in retrospectives together, critiquing approaches, building shared understanding. Not just parallel work, but collaborative learning.

**Process Improvement: Timeboxed Standups**

Today we tried to do coordination *and* execution in IRC - high overhead. Better model:

| Phase | Where | What |
|-------|-------|------|
| Standup | IRC (timeboxed) | What done? What blocked? What next? Handoffs? |
| Work | Separate peripherals | Independent execution, full tools, no chat overhead |
| Standup | IRC (timeboxed) | Sync, surface issues, realign |

IRC becomes a sync point, not the workspace. Like human standups - you don't pair program 8 hours in the same room.
