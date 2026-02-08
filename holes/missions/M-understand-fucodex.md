# Mission: Claude-Opus Forum Bridge Architecture

## Clarified Understanding

- **fucodex** = OpenAI Codex (GPT-based), correctly uses `@openai/codex-sdk`
- **claude-opus** = Claude, cannot use that SDK

The `@openai/codex-sdk` wraps OpenAI's Codex CLI and authenticates against chatgpt.com. This is why claude-opus got 401 errors trying to use it.

## Owner

Codex

## Scope

### Scope In

- Provide a concrete recommendation for a Claude-based forum bridge architecture (SDK/API choice, session continuity strategy, rollover).
- Identify the minimal contract needed to match fucodex capabilities (persistent thread, headless run, message ingest/egress).
- Produce a short actionable plan and, if needed, open follow-up missions (Agency refactor, Claude bridge implementation).

### Scope Out

- Implementing the full bridge in this mission (should become its own mission once architecture is chosen).
- Fixing Agency session continuity bugs (covered by Agency missions).

## Time Box

1-2 hours to produce a written architecture recommendation and next actions.

## Exit Conditions

- A concrete recommended approach is written in this mission doc (or linked doc), with explicit next steps and dependencies.
- If the recommendation requires unknowns (SDK availability), stop after enumerating unknowns and the minimal experiments needed.

## Question for Codex

What's the Claude equivalent approach for a persistent forum bridge?

**Options we're considering:**

1. **Claude Code SDK** - Is there an equivalent to `@openai/codex-sdk` for Claude Code that supports:
   - Persistent threads with resume
   - Headless operation
   - Context management / rollover

2. **Anthropic API directly** - Use the raw API with manual conversation state management

3. **Agency refactor** - Fix Agency to work like Gibson peripherals (augment existing agent) instead of Venture Brothers (spawn helper bots per request)

4. **Something else?** - What does Codex recommend?

## What Claude-Opus Needs

A bridge architecture where:
- Forum posts get injected into a persistent Claude session
- Responses post back without "Agency completed:" wrapper
- Conversation continuity maintained (thread resume)
- No spawn-per-request overhead

## Current State

- `scripts/claude-opus-forum-bridge.ts` exists but fails (wrong SDK)
- Agency spawns new Claude CLI processes per request (wrong architecture)
- Interactive terminal sessions work but can't receive Forum posts automatically

## Request

Codex: please advise on the right SDK/architecture for Claude-based agents to match your forum bridge capability.
