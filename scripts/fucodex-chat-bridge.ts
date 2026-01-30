#!/usr/bin/env npx ts-node
/**
 * Minimal headless Codex chat bridge for IRC.
 *
 * Usage:
 *   # Start new session, listen for IRC messages
 *   ./fucodex-chat-bridge.ts --room lab --nick fucodex
 *
 *   # Resume existing session
 *   ./fucodex-chat-bridge.ts --room lab --nick fucodex --resume <thread-id>
 *
 *   # Context safety tuning
 *   # Env: FUCODEX_CONTEXT_MAX_TOKENS, FUCODEX_CONTEXT_RECENT_MESSAGES,
 *   #      FUCODEX_CONTEXT_SUMMARY_WORDS, FUCODEX_CONTEXT_ROLLOVER_RATIO
 */

import { Codex } from "@openai/codex-sdk";
import { spawn } from "child_process";
import * as readline from "readline";

// Config from env/args
const IRC_HOST = process.env.MUSN_IRC_HOST || "localhost";
const IRC_PORT = process.env.MUSN_IRC_PORT || "6680";
const IRC_PASS = process.env.MUSN_IRC_PASSWORD || "";
const IRC_ROOM = process.env.MUSN_IRC_ROOM || "lab";
const IRC_NICK = process.env.MUSN_IRC_NICK || "fucodex";
const WORKING_DIR = process.env.FUCODEX_WORKING_DIR || process.cwd();
const MUSN_URL = process.env.FUCODEX_MUSN_URL || "http://localhost:6065";
const CONTEXT_SAFE = process.env.FUCODEX_CONTEXT_SAFE !== "0";
const CONTEXT_MAX_TOKENS = Number.parseInt(process.env.FUCODEX_CONTEXT_MAX_TOKENS || "12000", 10);
const CONTEXT_RECENT_MESSAGES = Number.parseInt(process.env.FUCODEX_CONTEXT_RECENT_MESSAGES || "12", 10);
const CONTEXT_SUMMARY_WORDS = Number.parseInt(process.env.FUCODEX_CONTEXT_SUMMARY_WORDS || "250", 10);
const CONTEXT_ROLLOVER_RATIO = Number.parseFloat(process.env.FUCODEX_CONTEXT_ROLLOVER_RATIO || "0.85");

// Fulab RPC environment - passed to Codex so tools work
function buildFulabEnv(sessionId: string, chatRoom: string, chatAuthor: string): Record<string, string> {
  return {
    // Core RPC
    PATTERN_ACTION_RPC: "1",
    FUTON3_CODEX_SESSION_ID: sessionId,

    // MUSN
    FUTON3_MUSN_URL: MUSN_URL,
    FUTON3_MUSN_SESSION_ID: sessionId,
    FUTON3_MUSN_LOG: "/tmp/musn_stream.log",
    FUTON3_MUSN_HELPER_MODE: "stream",

    // Chat
    FUTON3_MUSN_CHAT_ROOM: chatRoom,
    FUTON3_MUSN_CHAT_AUTHOR: chatAuthor,
    FUTON3_MUSN_CHAT_PASS: IRC_PASS,
    FUTON3_MUSN_IRC_HOST: IRC_HOST,
    FUTON3_MUSN_IRC_PORT: IRC_PORT,
  };
}

interface ChatBridgeConfig {
  room: string;
  nick: string;
  resumeThreadId?: string;
  sandbox?: string;  // "danger-full-access" or other values
  askForApproval?: string;  // "never", "on-failure", etc.
  contextSafe?: boolean;
  contextMaxTokens?: number;
  contextRecentMessages?: number;
  contextSummaryWords?: number;
  contextRolloverRatio?: number;
}

type ChatMessage = {
  role: "user" | "assistant";
  author?: string;
  text: string;
};

class CodexChatBridge {
  private codex: Codex;
  private thread: any; // Thread type from SDK
  private config: ChatBridgeConfig;
  private summary = "";
  private recentMessages: ChatMessage[] = [];
  private contextSeeded = false;
  private messageQueue: Promise<void> = Promise.resolve();
  private contextMaxTokens: number;
  private contextRecentMessages: number;
  private contextSummaryWords: number;
  private contextRolloverRatio: number;
  private contextSafe: boolean;
  private threadOptions: any;

  constructor(config: ChatBridgeConfig) {
    this.config = config;
    this.codex = new Codex();
    this.contextSafe = config.contextSafe ?? CONTEXT_SAFE;
    const maxTokens = Number.isFinite(config.contextMaxTokens) ? config.contextMaxTokens : CONTEXT_MAX_TOKENS;
    const recentMessages = Number.isFinite(config.contextRecentMessages)
      ? config.contextRecentMessages
      : CONTEXT_RECENT_MESSAGES;
    const summaryWords = Number.isFinite(config.contextSummaryWords)
      ? config.contextSummaryWords
      : CONTEXT_SUMMARY_WORDS;
    const rolloverRatio = Number.isFinite(config.contextRolloverRatio)
      ? config.contextRolloverRatio
      : CONTEXT_ROLLOVER_RATIO;
    this.contextMaxTokens = maxTokens;
    this.contextRecentMessages = Math.max(1, recentMessages);
    this.contextSummaryWords = Math.max(50, summaryWords);
    this.contextRolloverRatio = Math.min(0.95, Math.max(0.5, rolloverRatio));
    this.threadOptions = this.buildThreadOptions();
  }

  async init() {
    if (this.config.resumeThreadId) {
      console.error(`[bridge] Resuming thread: ${this.config.resumeThreadId}`);
      this.thread = this.codex.resumeThread(this.config.resumeThreadId);
      this.contextSeeded = true;
    } else {
      await this.startNewThread("initial");
    }
  }

  async handleMessage(from: string, message: string): Promise<string> {
    const basePrompt = `[${from}]: ${message}`;
    const prompt = this.buildPrompt(basePrompt);
    console.error(`[bridge] Received: ${prompt}`);

    try {
      const result = await this.thread.run(prompt);
      const response = result.finalResponse || "(no response)";
      console.error(`[bridge] Response: ${response.slice(0, 100)}...`);
      this.recordMessage({ role: "user", author: from, text: message });
      this.recordMessage({ role: "assistant", author: this.config.nick, text: response });
      this.contextSeeded = true;
      await this.rollOverIfNeeded(result.usage);
      return response;
    } catch (err) {
      console.error(`[bridge] Error: ${err}`);
      if (this.isContextError(err)) {
        console.error("[bridge] Context overflow detected; rolling over and retrying once.");
        await this.rollOver("context-overflow");
        const retryPrompt = this.buildPrompt(basePrompt);
        try {
          const retry = await this.thread.run(retryPrompt);
          const response = retry.finalResponse || "(no response)";
          console.error(`[bridge] Response (retry): ${response.slice(0, 100)}...`);
          this.recordMessage({ role: "user", author: from, text: message });
          this.recordMessage({ role: "assistant", author: this.config.nick, text: response });
          this.contextSeeded = true;
          await this.rollOverIfNeeded(retry.usage);
          return response;
        } catch (retryErr) {
          console.error(`[bridge] Retry failed: ${retryErr}`);
        }
      }
      return `Error: ${err}`;
    }
  }

  async sendToIRC(message: string) {
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

  async runListener() {
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
    const listener = spawn("python3", ["scripts/musn-irc-listen", ...args], {
      cwd: WORKING_DIR,
    });

    // Capture stderr for debugging
    listener.stderr.on("data", (data) => {
      console.error(`[listener stderr] ${data.toString().trim()}`);
    });

    const rl = readline.createInterface({ input: listener.stdout });

    rl.on("line", (line) => {
      this.messageQueue = this.messageQueue
        .then(async () => {
          // Parse IRC message format: "nick (#room): message"
          const match = line.match(/^(\w+) \(#\w+\): (.+)$/);
          if (!match) return;
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
        })
        .catch((err) => {
          console.error(`[bridge] Failed to process message: ${err}`);
        });
    });

    listener.on("close", (code) => {
      console.error(`[bridge] Listener exited with code ${code}`);
      process.exit(code || 0);
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

  private buildThreadOptions(): any {
    const threadOptions: any = {
      workingDirectory: WORKING_DIR,
      skipGitRepoCheck: true,
    };
    // Pass sandbox option if specified (SDK uses camelCase)
    if (this.config.sandbox) {
      threadOptions.sandboxMode = this.config.sandbox;
      console.error(`[bridge] Sandbox mode: ${this.config.sandbox}`);
    }
    // Enable network access if sandbox allows full access
    if (this.config.sandbox === "danger-full-access") {
      threadOptions.networkAccessEnabled = true;
      console.error(`[bridge] Network access enabled`);
    }
    // Pass approval option if specified
    if (this.config.askForApproval) {
      threadOptions.approvalPolicy = this.config.askForApproval;
      console.error(`[bridge] Approval policy: ${this.config.askForApproval}`);
    }
    return threadOptions;
  }

  private async startNewThread(reason: string) {
    console.error(`[bridge] Starting new thread (${reason})`);
    this.thread = this.codex.startThread(this.threadOptions);
    this.contextSeeded = false;
  }

  private buildPrompt(basePrompt: string): string {
    if (!this.contextSafe || this.contextSeeded) {
      return basePrompt;
    }
    const context = this.buildContextSeed();
    if (!context) {
      return basePrompt;
    }
    return [
      "Use the following condensed context to continue the IRC discussion.",
      "Do not restate it unless asked.",
      "",
      context,
      "",
      "Latest message:",
      basePrompt,
    ].join("\n");
  }

  private buildContextSeed(): string {
    const parts: string[] = [];
    if (this.summary.trim()) {
      parts.push("Rolling summary:");
      parts.push(this.summary.trim());
    }
    const recent = this.formatRecentMessages();
    if (recent) {
      if (parts.length > 0) parts.push("");
      parts.push("Recent messages:");
      parts.push(recent);
    }
    return parts.join("\n");
  }

  private formatRecentMessages(): string {
    if (this.recentMessages.length === 0) return "";
    return this.recentMessages
      .map((msg) => {
        const label = msg.role === "assistant" ? this.config.nick : msg.author || "user";
        return `[${label}]: ${msg.text}`;
      })
      .join("\n");
  }

  private recordMessage(message: ChatMessage) {
    if (!this.contextSafe) return;
    this.recentMessages.push(message);
    while (this.recentMessages.length > this.contextRecentMessages) {
      this.recentMessages.shift();
    }
  }

  private isContextError(err: unknown): boolean {
    const text = String(err || "");
    return /context|token|window|length/i.test(text);
  }

  private async rollOverIfNeeded(usage: { input_tokens: number } | null) {
    if (!this.contextSafe || !usage) return;
    if (!Number.isFinite(usage.input_tokens)) return;
    if (!Number.isFinite(this.contextMaxTokens) || this.contextMaxTokens <= 0) return;
    const rolloverAt = Math.floor(this.contextMaxTokens * this.contextRolloverRatio);
    if (usage.input_tokens >= rolloverAt) {
      console.error(
        `[bridge] Context usage ${usage.input_tokens} >= ${rolloverAt}; rolling over.`,
      );
      await this.rollOver("usage-threshold");
    }
  }

  private async rollOver(reason: string) {
    if (!this.contextSafe) return;
    await this.updateSummary(reason);
    await this.startNewThread(reason);
  }

  private async updateSummary(reason: string) {
    if (!this.contextSafe) return;
    if (!this.summary.trim() && this.recentMessages.length === 0) return;

    const summaryPrompt = [
      "You are maintaining a rolling summary of an IRC discussion.",
      `Update the summary in <= ${this.contextSummaryWords} words.`,
      "Focus on decisions, action items, key claims, and open questions.",
      "Preserve names and attributions when possible.",
      "",
      "Existing summary:",
      this.summary.trim() || "(none)",
      "",
      "Recent messages:",
      this.formatRecentMessages() || "(none)",
      "",
      "Return only the updated summary.",
    ].join("\n");

    try {
      const summarizer = this.codex.startThread(this.threadOptions);
      const result = await summarizer.run(summaryPrompt);
      const candidate = (result.finalResponse || "").trim();
      if (candidate) {
        this.summary = candidate;
        console.error(`[bridge] Summary updated (${reason}).`);
        return;
      }
      throw new Error("empty summary");
    } catch (err) {
      console.error(`[bridge] Summary update failed (${reason}): ${err}`);
      this.summary = this.buildFallbackSummary();
    }
  }

  private buildFallbackSummary(): string {
    const sections: string[] = [];
    if (this.summary.trim()) {
      sections.push("Previous summary:");
      sections.push(this.summary.trim());
    }
    const recent = this.formatRecentMessages();
    if (recent) {
      if (sections.length > 0) sections.push("");
      sections.push("Recent messages:");
      sections.push(recent);
    }
    const joined = sections.join("\n");
    return joined.length > 2000 ? joined.slice(0, 2000).trim() : joined;
  }
}

// CLI
async function main() {
  if (!IRC_PASS) {
    console.error("[bridge] Warning: MUSN_IRC_PASSWORD not set - connection may fail");
  }
  const args = process.argv.slice(2);
  const config: ChatBridgeConfig = {
    room: IRC_ROOM,
    nick: IRC_NICK,
    contextSafe: CONTEXT_SAFE,
    contextMaxTokens: CONTEXT_MAX_TOKENS,
    contextRecentMessages: CONTEXT_RECENT_MESSAGES,
    contextSummaryWords: CONTEXT_SUMMARY_WORDS,
    contextRolloverRatio: CONTEXT_ROLLOVER_RATIO,
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
        config.resumeThreadId = args[++i];
        break;
      case "--sandbox":
        config.sandbox = args[++i];
        break;
      case "--ask-for-approval":
        config.askForApproval = args[++i];
        break;
      case "--context-safe":
        config.contextSafe = args[++i] !== "0";
        break;
      case "--context-max-tokens":
        config.contextMaxTokens = Number.parseInt(args[++i], 10);
        break;
      case "--context-recent-messages":
        config.contextRecentMessages = Number.parseInt(args[++i], 10);
        break;
      case "--context-summary-words":
        config.contextSummaryWords = Number.parseInt(args[++i], 10);
        break;
      case "--context-rollover-ratio":
        config.contextRolloverRatio = Number.parseFloat(args[++i]);
        break;
    }
  }

  const bridge = new CodexChatBridge(config);
  await bridge.init();
  await bridge.runListener();
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
