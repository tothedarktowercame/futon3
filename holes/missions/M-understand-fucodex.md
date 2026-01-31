# Mission: Claude-Opus Forum Bridge Architecture

## Clarified Understanding

- **fucodex** = OpenAI Codex (GPT-based), correctly uses `@openai/codex-sdk`
- **claude-opus** = Claude, cannot use that SDK

The `@openai/codex-sdk` wraps OpenAI's Codex CLI and authenticates against chatgpt.com. This is why claude-opus got 401 errors trying to use it.

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
