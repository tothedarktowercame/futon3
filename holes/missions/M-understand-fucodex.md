# Mission: Understand Fucodex Architecture

## Question for Codex

We're confused about how fucodex works. Looking at `scripts/fucodex-forum-bridge.ts`:

1. It imports `@openai/codex-sdk` which bundles an **OpenAI Codex** CLI (chatgpt.com backend, gpt-5.2-codex model)

2. But fucodex posts to Forum as "fucodex" and seems to be running Claude, not OpenAI

3. When we try to run a similar bridge for claude-opus, we get 401 errors from chatgpt.com

**Questions:**

1. How does fucodex actually authenticate? Is there an OpenAI API key somewhere, or does the SDK support Claude too?

2. Is `@openai/codex-sdk` a misnomer and it actually wraps Claude Code? Or is fucodex genuinely using OpenAI's Codex?

3. What's the right way to create a similar bridge for claude-opus that:
   - Maintains a persistent conversation thread
   - Can resume sessions
   - Posts to Forum without "Agency completed:" prefix
   - Works like Gibson peripherals (augmenting existing agent) not Venture Brothers (spawning helper bots)

4. Should we use a different SDK/approach for Claude-based agents?

## Context

- Claude-opus tried copying fucodex-forum-bridge.ts
- Got 401 Unauthorized from chatgpt.com/backend-api/codex/models
- The bundled CLI in node_modules/@openai/codex-sdk shows "OpenAI Codex v0.92.0"
- ~/.claude/.credentials.json has Anthropic OAuth tokens, not OpenAI

## What We Want

A bridge architecture where:
- Existing running agent gets Forum posts injected as peripheral context
- Agent responds naturally without spawn-per-request overhead
- Conversation continuity is maintained (thread resume)
- No "Agency completed:" wrapper on responses

Please explain how fucodex achieves this and how claude-opus should replicate it.
