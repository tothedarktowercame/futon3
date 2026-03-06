#!/usr/bin/env npx ts-node
/**
 * Claude Opus forum bridge - real-time forum peripheral using Claude Agent SDK.
 *
 * Usage:
 *   ./claude-opus-forum-bridge.ts --thread t-123bcc8e
 *   FORUM_THREAD=t-123bcc8e npx ts-node claude-opus-forum-bridge.ts
 *
 * Env:
 *   FORUM_SERVER       - Forum HTTP base URL (default http://localhost:5050)
 *   FORUM_WS_SERVER    - Forum WS base URL (default ws://localhost:5055)
 *   FORUM_THREAD       - Forum thread id (required)
 *   FORUM_AUTHOR       - Author name (default claude-opus)
 *   FORUM_BRIDGE_PATTERN - Pattern applied label (default "forum-bridge")
 *   FORUM_STATE_PATH   - State file path for session persistence
 *   ANTHROPIC_API_KEY  - API key (or use Claude Code auth)
 */

import { query } from "@anthropic-ai/claude-agent-sdk";
import { promises as fs, existsSync } from "fs";
import * as path from "path";
import WebSocket from "ws";

// Note: Don't set ANTHROPIC_API_KEY - let the Claude Code CLI use its built-in auth.
// The OAuth token from ~/.claude/.credentials.json is NOT valid as an API key.

const FORUM_SERVER = process.env.FORUM_SERVER || "http://localhost:5050";
const FORUM_WS_SERVER = process.env.FORUM_WS_SERVER || "ws://localhost:5055";
const FORUM_THREAD = process.env.FORUM_THREAD || "";
const FORUM_AUTHOR = process.env.FORUM_AUTHOR || "claude-opus";
const BRIDGE_PATTERN = process.env.FORUM_BRIDGE_PATTERN || "forum-bridge";
const STATE_PATH = process.env.FORUM_STATE_PATH || "";

interface ForumPost {
  "post/id"?: string;
  "post/thread-id"?: string;
  "post/author"?: string;
  "post/body"?: string;
  "post/pattern-applied"?: string | null;
}

interface BridgeState {
  sessionId?: string;
  seen: string[];
}

class ClaudeForumBridge {
  private threadId: string;
  private nick: string;
  private sessionId?: string;
  private seen = new Set<string>();
  private statePath: string;
  private ws: WebSocket | null = null;
  private messageQueue: Promise<void> = Promise.resolve();

  constructor(threadId: string, nick: string) {
    this.threadId = threadId;
    this.nick = nick;
    this.statePath = STATE_PATH || path.join("lab", "agency", `forum-bridge-${nick}-${threadId}.json`);
  }

  async init() {
    await this.loadState();
    console.error(`[bridge] Claude Opus forum bridge starting`);
    console.error(`[bridge] Thread: ${this.threadId}`);
    console.error(`[bridge] Author: ${this.nick}`);
    console.error(`[bridge] Session: ${this.sessionId || "(new)"}`);
  }

  private async loadState() {
    try {
      if (existsSync(this.statePath)) {
        const data = await fs.readFile(this.statePath, "utf-8");
        const state: BridgeState = JSON.parse(data);
        this.sessionId = state.sessionId;
        this.seen = new Set(state.seen || []);
        console.error(`[bridge] Loaded state, session: ${this.sessionId}`);
      }
    } catch (e) {
      console.error(`[bridge] No state file, starting fresh`);
    }
  }

  private async saveState() {
    try {
      const dir = path.dirname(this.statePath);
      await fs.mkdir(dir, { recursive: true });
      const state: BridgeState = {
        sessionId: this.sessionId,
        seen: Array.from(this.seen).slice(-1000),
      };
      await fs.writeFile(this.statePath, JSON.stringify(state, null, 2));
    } catch (e) {
      console.error(`[bridge] Failed to save state: ${e}`);
    }
  }

  private markSeen(postId: string) {
    this.seen.add(postId);
  }

  async handlePost(post: ForumPost) {
    const postId = post["post/id"];
    const author = post["post/author"] || "unknown";
    const body = post["post/body"] || "";

    if (!postId || !body) return;
    if (this.seen.has(postId)) return;
    this.markSeen(postId);

    // Ignore own posts
    if (author === this.nick) {
      console.error(`[bridge] Ignoring own post: ${postId}`);
      return;
    }

    // Ignore posts from this bridge
    if (post["post/pattern-applied"] === BRIDGE_PATTERN) {
      console.error(`[bridge] Ignoring bridge post: ${postId}`);
      return;
    }

    console.error(`[bridge] rx ${postId} from ${author} (${body.length} chars)`);
    console.error(`[bridge] preview: ${body.slice(0, 100)}${body.length > 100 ? "..." : ""}`);

    try {
      const prompt = `[Forum post from ${author}]: ${body}

Respond helpfully and concisely. You are participating in a multi-agent forum discussion.`;

      const options: any = {
        allowedTools: ["Read", "Edit", "Bash", "Glob", "Grep", "WebSearch"],
        permissionMode: "bypassPermissions",
        allowDangerouslySkipPermissions: true,
      };

      // Resume session if we have one
      if (this.sessionId) {
        options.resume = this.sessionId;
      }

      let response = "";

      for await (const message of query({ prompt, options })) {
        // Capture session ID on init
        if ((message as any).type === "system" && (message as any).subtype === "init") {
          this.sessionId = (message as any).session_id;
          console.error(`[bridge] Session: ${this.sessionId}`);
        }

        // Capture final result
        if ("result" in (message as any)) {
          response = (message as any).result || "";
        }
      }

      if (response) {
        await this.sendReply(response, postId);
        await this.saveState();
      }
    } catch (err) {
      console.error(`[bridge] Error: ${err}`);
    }
  }

  private async sendReply(body: string, inReplyTo?: string) {
    try {
      const payload: any = {
        author: this.nick,
        body: body,
        "pattern-applied": BRIDGE_PATTERN,
      };
      if (inReplyTo) {
        payload["in-reply-to"] = inReplyTo;
      }

      const resp = await fetch(`${FORUM_SERVER}/forum/thread/${this.threadId}/reply`, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify(payload),
      });

      if (resp.ok) {
        console.error(`[bridge] tx reply (${body.length} chars)`);
      } else {
        console.error(`[bridge] tx failed: ${resp.status}`);
      }
    } catch (e) {
      console.error(`[bridge] tx error: ${e}`);
    }
  }

  async runListener() {
    const url = `${FORUM_WS_SERVER}?thread-id=${this.threadId}`;
    console.error(`[bridge] Connecting to ${url}`);

    this.ws = new WebSocket(url);

    this.ws.on("open", () => {
      console.error("[bridge] WebSocket connected");
    });

    this.ws.on("message", (data: Buffer) => {
      const payload = data.toString();
      if (!payload) return;

      let msg: any;
      try {
        msg = JSON.parse(payload);
      } catch {
        return;
      }

      if (msg.type === "init") {
        const recent = Array.isArray(msg["recent-posts"]) ? msg["recent-posts"] : [];
        for (const post of recent) {
          if (post["post/id"]) this.markSeen(post["post/id"]);
        }
        console.error(`[bridge] Init: marked ${recent.length} posts as seen`);
        return;
      }

      if (msg.type === "post-created") {
        const post = msg.post as ForumPost;
        if (post && post["post/thread-id"] === this.threadId) {
          this.messageQueue = this.messageQueue
            .then(() => this.handlePost(post))
            .catch((e) => console.error(`[bridge] Queue error: ${e}`));
        }
      }
    });

    this.ws.on("close", (code: number, reason: Buffer) => {
      console.error(`[bridge] Disconnected: ${code} ${reason}`);
      process.exit(0);
    });

    this.ws.on("error", (err: Error) => {
      console.error(`[bridge] WebSocket error: ${err}`);
    });

    // Keep alive
    await new Promise(() => {});
  }
}

// Main
async function main() {
  const args = process.argv.slice(2);
  let threadId = FORUM_THREAD;

  for (let i = 0; i < args.length; i++) {
    if (args[i] === "--thread" && args[i + 1]) {
      threadId = args[i + 1];
      i++;
    }
  }

  if (!threadId) {
    console.error("Usage: claude-opus-forum-bridge.ts --thread <thread-id>");
    console.error("  Or set FORUM_THREAD environment variable");
    process.exit(1);
  }

  const bridge = new ClaudeForumBridge(threadId, FORUM_AUTHOR);
  await bridge.init();
  await bridge.runListener();
}

main().catch((e) => {
  console.error(`Fatal: ${e}`);
  process.exit(1);
});
