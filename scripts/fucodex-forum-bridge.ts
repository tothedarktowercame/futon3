#!/usr/bin/env npx ts-node
/**
 * Codex forum bridge - real-time forum peripheral (WebSocket + HTTP).
 *
 * Usage:
 *   ./fucodex-forum-bridge.ts --thread t-123bcc8e --nick fucodex
 *   ./fucodex-forum-bridge.ts --thread t-123bcc8e --nick fucodex --resume <thread-id>
 *   ./fucodex-forum-bridge.ts --thread t-123bcc8e --poll-interval 3000
 *
 * Env:
 *   FORUM_SERVER       - Forum HTTP base URL (default http://localhost:5050)
 *   FORUM_WS_SERVER    - Forum WS base URL (default derived from FORUM_SERVER)
 *   FORUM_THREAD       - Forum thread id (fallback if --thread not set)
 *   FORUM_AUTHOR       - Author name (default fucodex)
 *   FORUM_BRIDGE_PATTERN - Pattern applied label to mark bridge replies (default "forum-bridge")
 *   FORUM_POLL_INTERVAL - Poll interval ms (default 0, disables polling)
 *
 * Context safety tuning (same as IRC bridge):
 *   FUCODEX_CONTEXT_MAX_TOKENS, FUCODEX_CONTEXT_RECENT_MESSAGES,
 *   FUCODEX_CONTEXT_SUMMARY_WORDS, FUCODEX_CONTEXT_ROLLOVER_RATIO
 */

import { Codex } from "@openai/codex-sdk";

const FORUM_SERVER = process.env.FORUM_SERVER || "http://localhost:5050";
const FORUM_WS_SERVER = process.env.FORUM_WS_SERVER || "";
const FORUM_THREAD = process.env.FORUM_THREAD || "";
const FORUM_AUTHOR = process.env.FORUM_AUTHOR || "fucodex";
const BRIDGE_PATTERN = process.env.FORUM_BRIDGE_PATTERN || "forum-bridge";
const FORUM_POLL_INTERVAL = Number.parseInt(process.env.FORUM_POLL_INTERVAL || "0", 10);
const CONTEXT_SAFE = process.env.FUCODEX_CONTEXT_SAFE !== "0";
const CONTEXT_MAX_TOKENS = Number.parseInt(process.env.FUCODEX_CONTEXT_MAX_TOKENS || "12000", 10);
const CONTEXT_RECENT_MESSAGES = Number.parseInt(process.env.FUCODEX_CONTEXT_RECENT_MESSAGES || "12", 10);
const CONTEXT_SUMMARY_WORDS = Number.parseInt(process.env.FUCODEX_CONTEXT_SUMMARY_WORDS || "250", 10);
const CONTEXT_ROLLOVER_RATIO = Number.parseFloat(process.env.FUCODEX_CONTEXT_ROLLOVER_RATIO || "0.85");
const WS_RECONNECT_DELAY = Number.parseInt(process.env.FORUM_WS_RECONNECT_DELAY || "3", 10);

type ChatMessage = {
  role: "user" | "assistant";
  author?: string;
  text: string;
};

interface ForumBridgeConfig {
  threadId: string;
  nick: string;
  resumeThreadId?: string;
  sandbox?: string;
  askForApproval?: string;
  contextSafe?: boolean;
  contextMaxTokens?: number;
  contextRecentMessages?: number;
  contextSummaryWords?: number;
  contextRolloverRatio?: number;
  pollInterval?: number;
}

type ForumPost = {
  "post/id"?: string;
  "post/thread-id"?: string;
  "post/author"?: string;
  "post/body"?: string;
  "post/pattern-applied"?: string | null;
  "post/timestamp"?: string;
};

class CodexForumBridge {
  private codex: Codex;
  private thread: any;
  private config: ForumBridgeConfig;
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
  private ws: WebSocket | null = null;
  private seen = new Set<string>();
  private pollInterval: number;
  private pollInitialized = false;

  constructor(config: ForumBridgeConfig) {
    this.config = config;
    this.codex = new Codex();
    this.contextSafe = config.contextSafe ?? CONTEXT_SAFE;
    const maxTokens =
      typeof config.contextMaxTokens === "number" && Number.isFinite(config.contextMaxTokens)
        ? config.contextMaxTokens
        : CONTEXT_MAX_TOKENS;
    const recentMessages =
      typeof config.contextRecentMessages === "number" && Number.isFinite(config.contextRecentMessages)
        ? config.contextRecentMessages
        : CONTEXT_RECENT_MESSAGES;
    const summaryWords =
      typeof config.contextSummaryWords === "number" && Number.isFinite(config.contextSummaryWords)
        ? config.contextSummaryWords
        : CONTEXT_SUMMARY_WORDS;
    const rolloverRatio =
      typeof config.contextRolloverRatio === "number" && Number.isFinite(config.contextRolloverRatio)
        ? config.contextRolloverRatio
        : CONTEXT_ROLLOVER_RATIO;
    this.contextMaxTokens = maxTokens;
    this.contextRecentMessages = Math.max(1, recentMessages);
    this.contextSummaryWords = Math.max(50, summaryWords);
    this.contextRolloverRatio = Math.min(0.95, Math.max(0.5, rolloverRatio));
    this.pollInterval = Number.isFinite(config.pollInterval) ? Number(config.pollInterval) : FORUM_POLL_INTERVAL;
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

  async handlePost(post: ForumPost) {
    const postId = post["post/id"];
    const author = post["post/author"] || "unknown";
    const body = post["post/body"] || "";
    if (!postId || !body) return;
    if (this.seen.has(postId)) return;
    this.seen.add(postId);
    if (author === this.config.nick) return;
    if (post["post/pattern-applied"] === BRIDGE_PATTERN) return;

    const basePrompt = `[${author}]: ${body}`;
    const prompt = this.buildPrompt(basePrompt);
    console.error(`[bridge] Received: ${prompt.slice(0, 200)}...`);

    try {
      const result = await this.thread.run(prompt);
      const response = (result.finalResponse || "").trim() || "(no response)";
      this.recordMessage({ role: "user", author, text: body });
      this.recordMessage({ role: "assistant", author: this.config.nick, text: response });
      this.contextSeeded = true;
      await this.rollOverIfNeeded(result.usage);
      await this.sendReply(response);
    } catch (err) {
      console.error(`[bridge] Error: ${err}`);
      if (this.isContextError(err)) {
        console.error("[bridge] Context overflow detected; rolling over and retrying once.");
        await this.rollOver("context-overflow");
        const retryPrompt = this.buildPrompt(basePrompt);
        try {
          const retry = await this.thread.run(retryPrompt);
          const response = (retry.finalResponse || "").trim() || "(no response)";
          this.recordMessage({ role: "user", author, text: body });
          this.recordMessage({ role: "assistant", author: this.config.nick, text: response });
          this.contextSeeded = true;
          await this.rollOverIfNeeded(retry.usage);
          await this.sendReply(response);
        } catch (retryErr) {
          console.error(`[bridge] Retry failed: ${retryErr}`);
        }
      }
    }
  }

  async runListener() {
    if (this.pollInterval > 0) {
      console.error(`[bridge] Polling enabled (${this.pollInterval} ms)`);
      await this.runPollingLoop();
      return;
    }
    const url = this.wsUrl();
    console.error(`[bridge] Connecting to ${url}`);
    this.ws = new WebSocket(url);
    this.ws.addEventListener("open", () => {
      console.error("[bridge] WebSocket connected.");
    });
    this.ws.addEventListener("message", (event) => {
      const payload = typeof event.data === "string" ? event.data : "";
      if (!payload) return;
      let msg: any;
      try {
        msg = JSON.parse(payload);
      } catch {
        return;
      }
      const type = msg.type;
      if (type === "init") {
        const recent = Array.isArray(msg["recent-posts"]) ? msg["recent-posts"] : [];
        // Seed recent messages for context (do not respond)
        for (const post of recent) {
          const postId = post["post/id"];
          if (postId) this.seen.add(postId);
        }
        if (this.contextSafe && !this.contextSeeded) {
          const tail = recent.slice(-this.contextRecentMessages);
          for (const post of tail) {
            const author = post["post/author"] || "unknown";
            const body = post["post/body"] || "";
            if (!body) continue;
            this.recordMessage({ role: "user", author, text: body });
          }
        }
        return;
      }
      if (type === "post-created") {
        const post = msg.post as ForumPost;
        if (post && post["post/thread-id"] === this.config.threadId) {
          this.messageQueue = this.messageQueue
            .then(() => this.handlePost(post))
            .catch((err) => console.error(`[bridge] queue error: ${err}`));
        }
      }
    });
    this.ws.addEventListener("close", () => {
      console.error("[bridge] WebSocket closed; reconnecting...");
      this.ws = null;
      setTimeout(() => this.runListener(), WS_RECONNECT_DELAY * 1000);
    });
    this.ws.addEventListener("error", (err) => {
      console.error("[bridge] WebSocket error:", err);
    });
  }

  private async runPollingLoop() {
    for (;;) {
      try {
        const data = await this.fetchThread();
        const posts: ForumPost[] = Array.isArray(data?.posts) ? data.posts : [];
        if (!this.pollInitialized) {
          for (const post of posts) {
            const postId = post["post/id"];
            if (postId) this.seen.add(postId);
          }
          if (this.contextSafe && !this.contextSeeded) {
            const tail = posts.slice(-this.contextRecentMessages);
            for (const post of tail) {
              const author = post["post/author"] || "unknown";
              const body = post["post/body"] || "";
              if (!body) continue;
              this.recordMessage({ role: "user", author, text: body });
            }
          }
          this.pollInitialized = true;
        } else {
          const newPosts = posts.filter((post) => {
            const postId = post["post/id"];
            return postId && !this.seen.has(postId);
          });
          if (newPosts.length > 0) {
            newPosts.sort((a, b) => {
              const at = a["post/timestamp"] || "";
              const bt = b["post/timestamp"] || "";
              return at.localeCompare(bt);
            });
            for (const post of newPosts) {
              await this.handlePost(post);
            }
          }
        }
      } catch (err) {
        console.error(`[bridge] Polling error: ${err}`);
      }
      await new Promise((resolve) => setTimeout(resolve, this.pollInterval));
    }
  }

  private async fetchThread(): Promise<{ posts?: ForumPost[] } | null> {
    const response = await fetch(`${FORUM_SERVER.replace(/\/+$/, "")}/forum/thread/${this.config.threadId}`, {
      method: "GET",
      headers: { Accept: "application/json" },
    });
    if (!response.ok) {
      throw new Error(`Forum thread fetch failed: ${response.status}`);
    }
    return (await response.json()) as { posts?: ForumPost[] };
  }

  private wsUrl(): string {
    const base = FORUM_WS_SERVER || this.deriveWsServer(FORUM_SERVER);
    const hasQuery = base.includes("?");
    const sep = hasQuery ? "&" : "?";
    return `${base}${sep}thread-id=${encodeURIComponent(this.config.threadId)}`;
  }

  private deriveWsServer(httpBase: string): string {
    const trimmed = httpBase.replace(/\/+$/, "");
    if (trimmed.startsWith("wss://") || trimmed.startsWith("ws://")) {
      return trimmed;
    }
    if (trimmed.startsWith("https://")) {
      return `wss://${trimmed.slice("https://".length)}/forum/stream/ws`;
    }
    if (trimmed.startsWith("http://")) {
      return `ws://${trimmed.slice("http://".length)}/forum/stream/ws`;
    }
    return `ws://${trimmed}/forum/stream/ws`;
  }

  private async sendReply(text: string) {
    const chunks = this.splitMessage(text, 1800);
    for (const chunk of chunks) {
      const payload = {
        author: this.config.nick,
        body: chunk,
        "pattern-applied": BRIDGE_PATTERN,
      };
      const response = await fetch(`${FORUM_SERVER.replace(/\/+$/, "")}/forum/thread/${this.config.threadId}/reply`, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify(payload),
      });
      if (!response.ok) {
        console.error(`[bridge] Post failed: ${response.status}`);
      }
    }
  }

  private splitMessage(text: string, maxLen: number): string[] {
    const chunks: string[] = [];
    let remaining = text;
    while (remaining.length > 0) {
      if (remaining.length <= maxLen) {
        chunks.push(remaining);
        break;
      }
      let breakAt = remaining.lastIndexOf(" ", maxLen);
      if (breakAt < maxLen / 2) breakAt = maxLen;
      chunks.push(remaining.slice(0, breakAt));
      remaining = remaining.slice(breakAt).trimStart();
    }
    return chunks;
  }

  private buildThreadOptions(): any {
    const threadOptions: any = {
      workingDirectory: process.cwd(),
      skipGitRepoCheck: true,
    };
    if (this.config.sandbox) {
      threadOptions.sandboxMode = this.config.sandbox;
      console.error(`[bridge] Sandbox mode: ${this.config.sandbox}`);
    }
    if (this.config.sandbox === "danger-full-access") {
      threadOptions.networkAccessEnabled = true;
      console.error("[bridge] Network access enabled");
    }
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
      "Use the following condensed context to continue the forum discussion.",
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
      console.error(`[bridge] Context usage ${usage.input_tokens} >= ${rolloverAt}; rolling over.`);
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
      "You are maintaining a rolling summary of a forum discussion.",
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

async function main() {
  const args = process.argv.slice(2);
  const config: ForumBridgeConfig = {
    threadId: FORUM_THREAD,
    nick: FORUM_AUTHOR,
    contextSafe: CONTEXT_SAFE,
    contextMaxTokens: CONTEXT_MAX_TOKENS,
    contextRecentMessages: CONTEXT_RECENT_MESSAGES,
    contextSummaryWords: CONTEXT_SUMMARY_WORDS,
    contextRolloverRatio: CONTEXT_ROLLOVER_RATIO,
    pollInterval: FORUM_POLL_INTERVAL,
  };

  for (let i = 0; i < args.length; i++) {
    switch (args[i]) {
      case "--thread":
        config.threadId = args[++i];
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
      case "--poll-interval":
        config.pollInterval = Number.parseInt(args[++i], 10);
        break;
    }
  }

  if (!config.threadId) {
    console.error("Forum thread id is required. Use --thread or set FORUM_THREAD.");
    process.exit(2);
  }

  const bridge = new CodexForumBridge(config);
  await bridge.init();
  await bridge.runListener();
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
