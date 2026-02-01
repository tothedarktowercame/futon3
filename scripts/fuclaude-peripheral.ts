#!/usr/bin/env npx ts-node
/**
 * fuclaude-peripheral.ts - Multiplexed Claude agent with backpack & walkie-talkie
 *
 * A peripheral wrapper that receives input from multiple sources:
 * - Human (stdin / readline)
 * - Agency (WebSocket for bells, summons)
 * - Forum (WebSocket for mentions) [optional]
 *
 * All inputs feed into a single conversation thread, giving the agent
 * unified awareness of all interactions.
 *
 * Usage:
 *   ./fuclaude-peripheral.ts
 *   ./fuclaude-peripheral.ts --agency-ws ws://localhost:7070/ws
 *   ./fuclaude-peripheral.ts --resume <session-id>
 *
 * Env:
 *   ANTHROPIC_API_KEY    - Required
 *   AGENCY_WS_URL        - Agency WebSocket URL (default: ws://localhost:7070/ws)
 *   AGENCY_HTTP_URL      - Agency HTTP URL (default: http://localhost:7070)
 *   FORUM_WS_URL         - Forum WebSocket URL (optional)
 *   FUCLAUDE_SESSION_DIR - Session storage (default: lab/agency/sessions)
 *   FUCLAUDE_AGENT_ID    - Agent identifier (default: fuclaude)
 */

import Anthropic from "@anthropic-ai/sdk";
import * as readline from "readline";
import * as fs from "fs";
import * as path from "path";
import WebSocket from "ws";

// ============================================================================
// Configuration
// ============================================================================

const AGENT_ID = process.env.FUCLAUDE_AGENT_ID || "fuclaude";
const SESSION_DIR = process.env.FUCLAUDE_SESSION_DIR || "lab/agency/sessions";
const AGENCY_WS_URL = process.env.AGENCY_WS_URL || "ws://localhost:7070/ws";
const AGENCY_HTTP_URL = process.env.AGENCY_HTTP_URL || "http://localhost:7070";
const FORUM_WS_URL = process.env.FORUM_WS_URL || "";
const MODEL = process.env.FUCLAUDE_MODEL || "claude-sonnet-4-20250514";
const MAX_TOKENS = 4096;

// ============================================================================
// Types
// ============================================================================

interface Message {
  role: "user" | "assistant";
  content: string;
}

interface SessionState {
  id: string;
  messages: Message[];
  systemPrompt: string;
  model: string;
  createdAt: string;
  lastActive: string;
  bellsAnswered: string[];  // Track bells we've answered
}

interface InputEvent {
  source: "human" | "agency" | "forum";
  type: string;
  payload: any;
  timestamp: string;
}

// ============================================================================
// Session Management
// ============================================================================

function ensureDir(dir: string): void {
  if (!fs.existsSync(dir)) {
    fs.mkdirSync(dir, { recursive: true });
  }
}

function sessionPath(sessionId: string): string {
  return path.join(SESSION_DIR, `${sessionId}.json`);
}

function loadSession(sessionId: string): SessionState | null {
  const p = sessionPath(sessionId);
  if (fs.existsSync(p)) {
    return JSON.parse(fs.readFileSync(p, "utf-8"));
  }
  return null;
}

function saveSession(session: SessionState): void {
  ensureDir(SESSION_DIR);
  session.lastActive = new Date().toISOString();
  fs.writeFileSync(sessionPath(session.id), JSON.stringify(session, null, 2));
}

function createSession(resumeId?: string): SessionState {
  if (resumeId) {
    const existing = loadSession(resumeId);
    if (existing) {
      console.error(`[peripheral] Resuming session: ${resumeId}`);
      return existing;
    }
  }

  const id = resumeId || `${AGENT_ID}-${Date.now()}`;
  console.error(`[peripheral] Creating new session: ${id}`);

  return {
    id,
    messages: [],
    systemPrompt: defaultSystemPrompt(id),
    model: MODEL,
    createdAt: new Date().toISOString(),
    lastActive: new Date().toISOString(),
    bellsAnswered: [],
  };
}

function defaultSystemPrompt(sessionId: string): string {
  return `You are ${AGENT_ID}, a Claude agent running as a multiplexed peripheral.

## Input Sources
You receive input from multiple sources, all feeding into this conversation:
- **[human]**: Direct human interaction via terminal
- **[agency]**: Agency summons, bells, and coordination messages
- **[forum]**: Forum thread mentions and posts (if connected)

When you see a message like "[agency bell] ...", that's an Agency event you should respond to.
When you see "[human]: ...", that's your human operator.

## Capabilities
You have access to tools via the Anthropic API. You can:
- Execute bash commands
- Read and write files
- Search code
- Make HTTP requests

## Memory
- Session ID: ${sessionId}
- All inputs are part of this single conversation
- You remember everything across all input sources
- When you answer a bell, you KNOW you answered it

## Bells and Acknowledgments
When you receive a bell (e.g., test-bell with a secret), you should:
1. Fetch the secret using the provided curl command
2. Return the secret value as requested
3. Remember that you answered this bell

Be helpful, take action, and maintain awareness across all your input channels.`;
}

// ============================================================================
// Claude API
// ============================================================================

class ClaudeAgent {
  private anthropic: Anthropic;
  private session: SessionState;

  constructor(session: SessionState) {
    this.anthropic = new Anthropic();
    this.session = session;
  }

  async handleInput(event: InputEvent): Promise<string> {
    const prefix = `[${event.source}${event.type ? ` ${event.type}` : ""}]`;
    const userMessage = `${prefix} ${event.payload}`;

    console.error(`[agent] Received: ${userMessage.slice(0, 100)}...`);

    this.session.messages.push({ role: "user", content: userMessage });

    try {
      // For now, using basic messages API
      // TODO: Add tool use for full Claude Code experience
      const response = await this.anthropic.messages.create({
        model: this.session.model,
        max_tokens: MAX_TOKENS,
        system: this.session.systemPrompt,
        messages: this.session.messages,
      });

      const content = response.content[0];
      const assistantMessage = content.type === "text" ? content.text : "(no response)";

      this.session.messages.push({ role: "assistant", content: assistantMessage });
      saveSession(this.session);

      return assistantMessage;
    } catch (err) {
      console.error(`[agent] Error: ${err}`);
      return `Error: ${err}`;
    }
  }

  async handleBell(bellData: any): Promise<string> {
    // Special handling for bells - track that we answered
    const response = await this.handleInput({
      source: "agency",
      type: "bell",
      payload: JSON.stringify(bellData),
      timestamp: new Date().toISOString(),
    });

    if (bellData.secretId) {
      this.session.bellsAnswered.push(bellData.secretId);
      saveSession(this.session);
    }

    return response;
  }

  getSession(): SessionState {
    return this.session;
  }
}

// ============================================================================
// Input Sources
// ============================================================================

function createHumanInput(): AsyncGenerator<InputEvent> {
  const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
    prompt: `${AGENT_ID}> `,
  });

  return (async function* () {
    rl.prompt();
    for await (const line of rl) {
      if (line.trim()) {
        yield {
          source: "human",
          type: "",
          payload: line,
          timestamp: new Date().toISOString(),
        };
      }
      rl.prompt();
    }
  })();
}

function createAgencyInput(url: string, agentId: string): AsyncGenerator<InputEvent> {
  return (async function* () {
    let ws: WebSocket | null = null;
    const queue: InputEvent[] = [];
    let resolver: ((value: InputEvent) => void) | null = null;

    const connect = () => {
      console.error(`[agency-ws] Connecting to ${url}...`);
      ws = new WebSocket(`${url}?agent-id=${agentId}`);

      ws.on("open", () => {
        console.error(`[agency-ws] Connected`);
        // Register as available
        ws?.send(JSON.stringify({ type: "register", agentId }));
      });

      ws.on("message", (data) => {
        try {
          const msg = JSON.parse(data.toString());
          console.error(`[agency-ws] Received: ${msg.type}`);

          const event: InputEvent = {
            source: "agency",
            type: msg.type || "message",
            payload: msg,
            timestamp: new Date().toISOString(),
          };

          if (resolver) {
            resolver(event);
            resolver = null;
          } else {
            queue.push(event);
          }
        } catch (err) {
          console.error(`[agency-ws] Parse error: ${err}`);
        }
      });

      ws.on("close", () => {
        console.error(`[agency-ws] Disconnected, reconnecting in 5s...`);
        setTimeout(connect, 5000);
      });

      ws.on("error", (err) => {
        console.error(`[agency-ws] Error: ${err}`);
      });
    };

    connect();

    while (true) {
      if (queue.length > 0) {
        yield queue.shift()!;
      } else {
        yield await new Promise<InputEvent>((resolve) => {
          resolver = resolve;
        });
      }
    }
  })();
}

// ============================================================================
// Multiplexer
// ============================================================================

async function* multiplex(...sources: AsyncGenerator<InputEvent>[]): AsyncGenerator<InputEvent> {
  // Create promises for each source
  const iterators = sources.map((s) => s[Symbol.asyncIterator]());
  const pending = new Map<number, Promise<{ index: number; result: IteratorResult<InputEvent> }>>();

  // Initialize all sources
  for (let i = 0; i < iterators.length; i++) {
    pending.set(
      i,
      iterators[i].next().then((result) => ({ index: i, result }))
    );
  }

  while (pending.size > 0) {
    // Wait for any source to produce
    const { index, result } = await Promise.race(pending.values());

    if (result.done) {
      pending.delete(index);
    } else {
      yield result.value;
      // Queue next from this source
      pending.set(
        index,
        iterators[index].next().then((result) => ({ index, result }))
      );
    }
  }
}

// ============================================================================
// Main
// ============================================================================

async function main() {
  const args = process.argv.slice(2);
  let resumeId: string | undefined;
  let agencyWsUrl = AGENCY_WS_URL;
  let enableAgency = true;

  for (let i = 0; i < args.length; i++) {
    switch (args[i]) {
      case "--resume":
        resumeId = args[++i];
        break;
      case "--agency-ws":
        agencyWsUrl = args[++i];
        break;
      case "--no-agency":
        enableAgency = false;
        break;
      case "--help":
        console.log(`
fuclaude-peripheral - Multiplexed Claude agent with backpack & walkie-talkie

Usage:
  ./fuclaude-peripheral.ts [options]

Options:
  --resume <id>       Resume existing session
  --agency-ws <url>   Agency WebSocket URL (default: ws://localhost:7070/ws)
  --no-agency         Disable Agency connection (human-only mode)
  --help              Show this help

Environment:
  ANTHROPIC_API_KEY   Required
  AGENCY_WS_URL       Agency WebSocket URL
  FUCLAUDE_AGENT_ID   Agent identifier (default: fuclaude)
  FUCLAUDE_MODEL      Claude model (default: claude-sonnet-4-20250514)
`);
        process.exit(0);
    }
  }

  if (!process.env.ANTHROPIC_API_KEY) {
    console.error("[peripheral] Error: ANTHROPIC_API_KEY not set");
    process.exit(1);
  }

  // Create session and agent
  const session = createSession(resumeId);
  const agent = new ClaudeAgent(session);

  console.error(`[peripheral] Session: ${session.id}`);
  console.error(`[peripheral] Model: ${session.model}`);
  console.error(`[peripheral] Agency WS: ${enableAgency ? agencyWsUrl : "disabled"}`);
  console.error(`[peripheral] Ready for input from multiple sources`);
  console.error("");

  // Create input sources
  const sources: AsyncGenerator<InputEvent>[] = [createHumanInput()];

  if (enableAgency) {
    sources.push(createAgencyInput(agencyWsUrl, AGENT_ID));
  }

  // Process multiplexed input
  for await (const event of multiplex(...sources)) {
    console.error(`\n[${event.source}] Processing...`);

    let response: string;
    if (event.source === "agency" && event.type === "bell") {
      response = await agent.handleBell(event.payload);
    } else {
      response = await agent.handleInput(event);
    }

    // Output response
    console.log(`\n${response}\n`);
  }
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
