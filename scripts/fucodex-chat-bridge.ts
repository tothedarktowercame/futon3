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

interface ChatBridgeConfig {
  room: string;
  nick: string;
  resumeThreadId?: string;
}

class CodexChatBridge {
  private codex: Codex;
  private thread: any; // Thread type from SDK
  private config: ChatBridgeConfig;

  constructor(config: ChatBridgeConfig) {
    this.config = config;
    this.codex = new Codex();
  }

  async init() {
    if (this.config.resumeThreadId) {
      console.error(`[bridge] Resuming thread: ${this.config.resumeThreadId}`);
      this.thread = this.codex.resumeThread(this.config.resumeThreadId);
    } else {
      console.error(`[bridge] Starting new thread`);
      this.thread = this.codex.startThread({
        workingDirectory: WORKING_DIR,
        skipGitRepoCheck: true,
      });
      console.error(`[bridge] Thread ID: ${this.thread.id}`);
    }
  }

  async handleMessage(from: string, message: string): Promise<string> {
    const prompt = `[${from}]: ${message}`;
    console.error(`[bridge] Received: ${prompt}`);

    try {
      const result = await this.thread.run(prompt);
      const response = result.finalResponse || "(no response)";
      console.error(`[bridge] Response: ${response.slice(0, 100)}...`);
      return response;
    } catch (err) {
      console.error(`[bridge] Error: ${err}`);
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

    console.error(`[bridge] Starting IRC listener...`);
    const listener = spawn("python3", ["scripts/musn-irc-listen", ...args], {
      cwd: WORKING_DIR,
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
}

// CLI
async function main() {
  const args = process.argv.slice(2);
  const config: ChatBridgeConfig = {
    room: IRC_ROOM,
    nick: IRC_NICK,
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
