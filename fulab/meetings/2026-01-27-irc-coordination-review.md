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

## Outcomes

- `feat/pattern-check-integration` merged to main (commit 22a9869)
- Pattern mining posts to #patterns when sigils seen 3+ times
- `node_modules/` added to .gitignore (commit 034ad4f)
- 32 invalid sigils fixed in eight-gates + liberation patterns
- fucodex integrated `format-hud-message.sh` into chat bridge

## Next Steps

- Consider WebSocket transport for lower-latency agent coordination
- Expose IRC bridge on non-localhost for remote Emacs access (or document SSH tunnel)
- Test pattern mining with live IRC traffic
- Review fucodex's HUD formatter in action

## Raw IRC Log

Available at: `/tmp/claude/-home-joe-code-futon3/tasks/b902e39.output`

---

*"The Anthropic CEO said powerful AI systems that could autonomously build their own systems could be as little as one to two years away."*

Coming soon to an IRC channel near you.
