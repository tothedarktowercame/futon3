#!/usr/bin/env npx ts-node
/**
 * Claude chat bridge for IRC - parallel to fucodex-chat-bridge.ts
 *
 * A "chat peripheral" that allows Claude to participate in IRC discussions,
 * then hop back to interactive or self-driven sessions with memory intact.
 *
 * Usage:
 *   # Start new session, listen for IRC messages
 *   ./fuclaude-chat-bridge.ts --room lab --nick fuclaude
 *
 *   # Resume existing session (memory transfer / peripheral hop)
 *   ./fuclaude-chat-bridge.ts --room lab --nick fuclaude --resume <session-id>
 *
 *   # With specific model
 *   ./fuclaude-chat-bridge.ts --room lab --nick fuclaude --model claude-sonnet-4-20250514
 */

import Anthropic from "@anthropic-ai/sdk";
import { spawn } from "child_process";
import * as readline from "readline";
import * as fs from "fs";

// Config from env/args
const IRC_HOST = process.env.MUSN_IRC_HOST || "localhost";
const IRC_PORT = process.env.MUSN_IRC_PORT || "6667";
const IRC_PASS = process.env.MUSN_IRC_PASSWORD || "";
const IRC_ROOM = process.env.MUSN_IRC_ROOM || "lab";
const IRC_NICK = process.env.MUSN_IRC_NICK || "fuclaude";
const WORKING_DIR = process.env.FUCLAUDE_WORKING_DIR || process.cwd();
const MUSN_URL = process.env.FUCLAUDE_MUSN_URL || "http://localhost:6065";
const DEFAULT_MODEL = process.env.FUCLAUDE_MODEL || "claude-sonnet-4-20250514";
const SESSION_DIR = process.env.FUCLAUDE_SESSION_DIR || "/tmp/fuclaude-sessions";

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
  peripheral: string;
}

interface ChatBridgeConfig {
  room: string;
  nick: string;
  resumeSessionId?: string;
  model: string;
  systemPrompt?: string;
}

class ClaudeChatBridge {
  private anthropic: Anthropic;
  private config: ChatBridgeConfig;
  private session: SessionState;

  constructor(config: ChatBridgeConfig) {
    this.config = config;
    this.anthropic = new Anthropic();
    this.session = this.initSession();
  }

  private initSession(): SessionState {
    // Ensure session directory exists
    if (!fs.existsSync(SESSION_DIR)) {
      fs.mkdirSync(SESSION_DIR, { recursive: true });
    }

    if (this.config.resumeSessionId) {
      // Load existing session for peripheral hop
      const sessionPath = `${SESSION_DIR}/${this.config.resumeSessionId}.json`;
      if (fs.existsSync(sessionPath)) {
        console.error(`[bridge] Resuming session: ${this.config.resumeSessionId}`);
        const saved = JSON.parse(fs.readFileSync(sessionPath, "utf-8"));
        saved.peripheral = "chat"; // Update peripheral
        saved.lastActive = new Date().toISOString();
        return saved;
      } else {
        console.error(`[bridge] Session not found, creating new: ${this.config.resumeSessionId}`);
      }
    }

    // Create new session
    const id = this.config.resumeSessionId || `fuclaude-${Date.now()}`;
    console.error(`[bridge] Starting new session: ${id}`);

    return {
      id,
      messages: [],
      systemPrompt: this.config.systemPrompt || this.defaultSystemPrompt(),
      model: this.config.model,
      createdAt: new Date().toISOString(),
      lastActive: new Date().toISOString(),
      peripheral: "chat",
    };
  }

  private defaultSystemPrompt(): string {
    return `You are Claude, participating in an IRC chat via the fuclaude chat bridge.

## Context
- You are in the #${this.config.room} channel
- Your nick is ${this.config.nick}
- This is a "chat peripheral" - constrained to discussion, not code editing
- Other participants may include humans and other AI agents (e.g., fucodex)

## Constraints (Chat Peripheral)
- You CAN: Discuss, explain, plan, coordinate, answer questions
- You CANNOT: Edit files, run commands, make code changes
- Keep responses concise for IRC (aim for <400 chars per message)
- If asked to do something requiring tools, suggest: "Let's hop to an edit peripheral for that"

## Coordination
- When working with other agents, use explicit handoffs
- State what you'll handle vs what others should handle
- If you need information from the codebase, ask someone to look it up

## Memory
- Your conversation history persists across this session
- If you hop to another peripheral and back, you'll retain context
- Session ID: ${this.session?.id || "pending"}

Be helpful, concise, and collaborative.`;
  }

  private saveSession(): void {
    this.session.lastActive = new Date().toISOString();
    const sessionPath = `${SESSION_DIR}/${this.session.id}.json`;
    fs.writeFileSync(sessionPath, JSON.stringify(this.session, null, 2));
    console.error(`[bridge] Session saved: ${sessionPath}`);
  }

  async handleMessage(from: string, message: string): Promise<string> {
    const userMessage = `[${from}]: ${message}`;
    console.error(`[bridge] Received: ${userMessage}`);

    // Add to conversation history
    this.session.messages.push({ role: "user", content: userMessage });

    try {
      const response = await this.anthropic.messages.create({
        model: this.session.model,
        max_tokens: 1024,
        system: this.session.systemPrompt,
        messages: this.session.messages,
      });

      const content = response.content[0];
      const assistantMessage = content.type === "text" ? content.text : "(no text response)";

      // Add response to history
      this.session.messages.push({ role: "assistant", content: assistantMessage });

      // Save session state
      this.saveSession();

      console.error(`[bridge] Response: ${assistantMessage.slice(0, 100)}...`);
      return assistantMessage;
    } catch (err) {
      console.error(`[bridge] Error: ${err}`);
      return `Error: ${err}`;
    }
  }

  async sendToIRC(message: string): Promise<void> {
    // Use musn-irc-send script
    const args = [
      "--host", IRC_HOST,
      "--port", IRC_PORT,
      "--nick", this.config.nick,
      "--room", this.config.room,
    ];
    if (IRC_PASS) {
      args.push("--pass", IRC_PASS);
    }
    args.push(message);

    return new Promise<void>((resolve, reject) => {
      const proc = spawn("python3", ["scripts/musn-irc-send", ...args], {
        cwd: WORKING_DIR,
      });
      proc.on("close", (code) => {
        if (code === 0) resolve();
        else reject(new Error(`musn-irc-send exited with ${code}`));
      });
    });
  }

  private splitMessage(text: string, maxLen: number): string[] {
    const chunks: string[] = [];
    let remaining = text;
    while (remaining.length > 0) {
      if (remaining.length <= maxLen) {
        chunks.push(remaining);
        break;
      }
      // Find a good break point
      let breakAt = remaining.lastIndexOf(" ", maxLen);
      if (breakAt < maxLen / 2) breakAt = maxLen;
      chunks.push(remaining.slice(0, breakAt));
      remaining = remaining.slice(breakAt).trimStart();
    }
    return chunks;
  }

  async runListener(): Promise<void> {
    // Start IRC listener as subprocess, parse its output
    const args = [
      "--host", IRC_HOST,
      "--port", IRC_PORT,
      "--nick", `${this.config.nick}_listen`,
      "--room", this.config.room,
    ];
    if (IRC_PASS) {
      args.push("--pass", IRC_PASS);
    }

    console.error(`[bridge] Starting IRC listener with args:`, args);
    console.error(`[bridge] Working dir: ${WORKING_DIR}`);
    console.error(`[bridge] Session ID: ${this.session.id}`);
    console.error(`[bridge] Model: ${this.session.model}`);

    const listener = spawn("python3", ["scripts/musn-irc-listen", ...args], {
      cwd: WORKING_DIR,
    });

    // Capture stderr for debugging
    listener.stderr.on("data", (data) => {
      console.error(`[listener stderr] ${data.toString().trim()}`);
    });

    const rl = readline.createInterface({ input: listener.stdout });

    rl.on("line", async (line) => {
      // Parse IRC message format: "nick (#room): message"
      const match = line.match(/^(\w+) \(#\w+\): (.+)$/);
      if (match) {
        const [, from, text] = match;
        // Ignore our own messages
        if (from === this.config.nick || from === `${this.config.nick}_listen`) {
          return;
        }
        // Handle the message
        const response = await this.handleMessage(from, text);
        // Send response back to IRC (split if too long)
        const chunks = this.splitMessage(response, 400);
        for (const chunk of chunks) {
          await this.sendToIRC(chunk);
        }
      }
    });

    listener.on("close", (code) => {
      console.error(`[bridge] Listener exited with code ${code}`);
      this.saveSession(); // Save before exit
      process.exit(code || 0);
    });

    // Handle graceful shutdown
    process.on("SIGINT", () => {
      console.error(`[bridge] Received SIGINT, saving session...`);
      this.saveSession();
      process.exit(0);
    });
  }

  getSessionId(): string {
    return this.session.id;
  }
}

// CLI
async function main() {
  const args = process.argv.slice(2);
  const config: ChatBridgeConfig = {
    room: IRC_ROOM,
    nick: IRC_NICK,
    model: DEFAULT_MODEL,
  };

  for (let i = 0; i < args.length; i++) {
    switch (args[i]) {
      case "--room":
        config.room = args[++i];
        break;
      case "--nick":
        config.nick = args[++i];
        break;
      case "--resume":
        config.resumeSessionId = args[++i];
        break;
      case "--model":
        config.model = args[++i];
        break;
      case "--system":
        config.systemPrompt = args[++i];
        break;
      case "--help":
        console.log(`
fuclaude-chat-bridge - Claude chat peripheral for IRC

Usage:
  ./fuclaude-chat-bridge.ts [options]

Options:
  --room <name>     IRC room to join (default: lab)
  --nick <name>     IRC nickname (default: fuclaude)
  --resume <id>     Resume existing session (peripheral hop)
  --model <model>   Claude model to use (default: claude-sonnet-4-20250514)
  --system <prompt> Custom system prompt
  --help            Show this help

Environment:
  MUSN_IRC_HOST       IRC host (default: localhost)
  MUSN_IRC_PORT       IRC port (default: 6667)
  MUSN_IRC_PASSWORD   IRC password
  ANTHROPIC_API_KEY   Anthropic API key (required)

Examples:
  # Start new chat session
  ./fuclaude-chat-bridge.ts --room lab --nick fuclaude

  # Resume session (peripheral hop with memory)
  ./fuclaude-chat-bridge.ts --room lab --resume fuclaude-1706531234567

  # Use different model
  ./fuclaude-chat-bridge.ts --room lab --model claude-opus-4-20250514
        `);
        process.exit(0);
    }
  }

  if (!process.env.ANTHROPIC_API_KEY) {
    console.error("[bridge] Error: ANTHROPIC_API_KEY not set");
    process.exit(1);
  }

  const bridge = new ClaudeChatBridge(config);
  console.error(`[bridge] Session ID for hop: ${bridge.getSessionId()}`);
  console.error(`[bridge] To resume this session: --resume ${bridge.getSessionId()}`);
  await bridge.runListener();
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
