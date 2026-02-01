#!/usr/bin/env -S npx ts-node
/**
 * fuclaude-peripheral.ts - Multiplexed Claude Code wrapper with backpack & walkie-talkie
 *
 * A peripheral wrapper that receives input from multiple sources:
 * - Human (stdin / readline)
 * - Agency (WebSocket for bells, summons)
 * - Forum (WebSocket for mentions) [optional]
 *
 * All inputs feed into a single Claude Code session, giving the agent
 * unified awareness of all interactions.
 *
 * Usage:
 *   ./fuclaude-peripheral.ts
 *   ./fuclaude-peripheral.ts --agency-ws ws://localhost:7070/agency/ws
 *   ./fuclaude-peripheral.ts --resume <session-id>
 *
 * Env:
 *   AGENCY_WS_URL        - Agency WebSocket URL (default: ws://localhost:7070/agency/ws)
 *   AGENCY_HTTP_URL      - Agency HTTP URL (default: http://localhost:7070)
 *   FORUM_WS_URL         - Forum WebSocket URL (optional)
 *   FUCLAUDE_AGENT_ID    - Agent identifier (default: fuclaude)
 */

import { execSync } from "child_process";
import * as fs from "fs";
import * as os from "os";
import * as path from "path";
import * as readline from "readline";
import WebSocket from "ws";

// ============================================================================
// Configuration
// ============================================================================

const AGENT_ID = process.env.FUCLAUDE_AGENT_ID || "fuclaude";
const AGENCY_WS_URL = process.env.AGENCY_WS_URL || "ws://localhost:7070/agency/ws";
const AGENCY_HTTP_URL = process.env.AGENCY_HTTP_URL || "http://localhost:7070";
const FORUM_WS_URL = process.env.FORUM_WS_URL || "ws://localhost:5055";
const FORUM_HTTP_URL = process.env.FORUM_HTTP_URL || "http://localhost:5050";

// ============================================================================
// Types
// ============================================================================

interface InputEvent {
  source: "human" | "agency" | "forum";
  type: string;
  payload: any;
  timestamp: string;
}

// ============================================================================
// Claude Code Process
// ============================================================================

// Session state for persistence across restarts
interface SessionState {
  sessionId: string;
  createdAt: string;
  lastActive: string;
}

const SESSION_DIR = "lab/agency/sessions";
const CLAUDE_SESSIONS_DIR = path.join(os.homedir(), ".claude/projects/-home-joe-code-futon3");

class ClaudeCodeWrapper {
  private sessionId: string | undefined;
  private statePath: string;
  private firstRun = true;

  constructor(resumeId?: string, statePath?: string) {
    this.statePath = statePath || path.join(SESSION_DIR, `peripheral-${AGENT_ID}-state.json`);

    if (resumeId) {
      // Explicit resume ID provided
      this.sessionId = resumeId;
      console.error(`[session] Using provided session: ${resumeId}`);
    } else {
      // Try to load from state file
      this.loadState();
    }
  }

  private loadState(): void {
    try {
      if (fs.existsSync(this.statePath)) {
        const data = JSON.parse(fs.readFileSync(this.statePath, "utf-8")) as SessionState;
        this.sessionId = data.sessionId;
        this.firstRun = false;
        console.error(`[session] Resumed from state: ${this.sessionId}`);
      }
    } catch (e) {
      console.error(`[session] No state file, will start fresh`);
    }
  }

  private saveState(): void {
    if (!this.sessionId) return;
    try {
      fs.mkdirSync(path.dirname(this.statePath), { recursive: true });
      const state: SessionState = {
        sessionId: this.sessionId,
        createdAt: this.firstRun ? new Date().toISOString() : "",
        lastActive: new Date().toISOString(),
      };
      fs.writeFileSync(this.statePath, JSON.stringify(state, null, 2));
    } catch (e) {
      console.error(`[session] Failed to save state: ${e}`);
    }
  }

  private detectSessionId(): void {
    // Find the most recently modified session file
    try {
      if (!fs.existsSync(CLAUDE_SESSIONS_DIR)) return;

      const files = fs.readdirSync(CLAUDE_SESSIONS_DIR)
        .filter(f => f.endsWith(".jsonl"))
        .map(f => ({
          name: f,
          path: path.join(CLAUDE_SESSIONS_DIR, f),
          mtime: fs.statSync(path.join(CLAUDE_SESSIONS_DIR, f)).mtime.getTime()
        }))
        .sort((a, b) => b.mtime - a.mtime);

      if (files.length > 0) {
        // Extract session ID from filename (e.g., "abc123.jsonl" -> "abc123")
        const newSessionId = files[0].name.replace(".jsonl", "");
        if (newSessionId !== this.sessionId) {
          this.sessionId = newSessionId;
          console.error(`[session] Detected new session: ${this.sessionId}`);
          this.saveState();
        }
      }
    } catch (e) {
      console.error(`[session] Failed to detect session: ${e}`);
    }
  }

  getSessionId(): string | undefined {
    return this.sessionId;
  }

  runClaude(input: string): string {
    // Escape the input for shell
    const escapedInput = input.replace(/'/g, "'\\''");
    const tmpFile = path.join(os.tmpdir(), `claude-out-${Date.now()}.txt`);

    let cmd = `claude --permission-mode bypassPermissions -p '${escapedInput}' > '${tmpFile}' 2>&1`;
    if (this.sessionId) {
      cmd = `claude --resume '${this.sessionId}' --permission-mode bypassPermissions -p '${escapedInput}' > '${tmpFile}' 2>&1`;
    }

    console.error(`[claude] Running: ${cmd.slice(0, 80)}...`);

    let output = "";
    try {
      execSync(cmd, { timeout: 120000 }); // 2 minute timeout
      output = fs.readFileSync(tmpFile, "utf-8");
      console.error(`[claude] Completed`);

      // After first run, detect and save session ID
      if (this.firstRun) {
        this.detectSessionId();
        this.firstRun = false;
      }
      this.saveState(); // Update lastActive
    } catch (error: any) {
      console.error(`[claude] Error: ${error.message}`);
      // Try to read partial output
      try {
        output = fs.readFileSync(tmpFile, "utf-8");
      } catch {}
    } finally {
      // Cleanup temp file
      try { fs.unlinkSync(tmpFile); } catch {}
    }
    return output;
  }

  handleInput(event: InputEvent): string {
    const prefix = `[${event.source}${event.type ? ` ${event.type}` : ""}]`;
    let message: string;

    if (event.source === "agency" && event.type === "bell") {
      const bellData = event.payload;
      const bellType = bellData["bell-type"] || bellData.bellType || "unknown";

      // Special handling for PAR bells
      if (bellType === "par-summon") {
        return this.handleParBell(bellData);
      }

      // Regular bell - format nicely
      message = `${prefix} Bell received!\n` +
        `Type: ${bellType}\n` +
        `Secret ID: ${bellData["secret-id"] || bellData.secretId || "none"}\n` +
        `To verify receipt, fetch: curl ${AGENCY_HTTP_URL}/agency/secret/${bellData["secret-id"] || bellData.secretId}\n` +
        `Payload: ${JSON.stringify(bellData.payload || {})}`;
    } else if (event.source === "forum" && event.type === "post") {
      // Format forum post
      const { author, body } = event.payload;
      message = `${prefix} Post from ${author}:\n${body}\n\nRespond helpfully and concisely.`;
    } else if (typeof event.payload === "object") {
      message = `${prefix} ${JSON.stringify(event.payload)}`;
    } else {
      message = `${prefix} ${event.payload}`;
    }

    console.error(`[peripheral] Processing: ${message.slice(0, 80)}...`);
    return this.runClaude(message);
  }

  private handleParBell(bellData: any): string {
    const payload = bellData.payload || {};
    const parTitle = payload.title || payload["par/title"] || "Untitled PAR";
    const parContent = payload.content || payload["par/content"] || "";
    const sections = payload.sections || ["happening", "perspectives", "learned"];

    console.error(`[par] Received PAR summon: ${parTitle}`);
    console.error(`[par] Contributing to sections: ${sections.join(", ")}`);

    // Build a comprehensive PAR prompt
    const prompt = `[agency par-summon] You have been summoned to participate in a Post-Action Review (PAR).

**PAR Title:** ${parTitle}

**Current PAR State:**
${parContent || "(No content yet - you may be the first contributor)"}

**Your Task:** Contribute your perspective to the following PAR sections.
For each section, provide 2-4 concise sentences from your unique viewpoint as ${AGENT_ID}.
Do not repeat what others have said. Be specific and actionable.

Sections to contribute to:
${sections.map((s: string, i: number) => {
  const questions: Record<string, string> = {
    "happening": "What actually happened? What approaches were taken and what worked?",
    "perspectives": "What's your unique perspective on what occurred? What did you notice?",
    "learned": "What explicit takeaways emerged? What was learned?",
    "forward": "What should happen next? What actions or experiments to try?"
  };
  return `${i + 1}. **${s}**: ${questions[s] || "Share your perspective."}`;
}).join("\n")}

Format your response as:

## ${sections[0]}
[Your contribution]

## ${sections[1] || ""}
[Your contribution]

${sections[2] ? `## ${sections[2]}\n[Your contribution]` : ""}
`;

    const response = this.runClaude(prompt);

    // Post contributions back to Agency if we have a callback URL
    const callbackUrl = payload.callback || payload["par/callback"];
    if (callbackUrl) {
      this.postParContributions(callbackUrl, parTitle, response);
    }

    return response;
  }

  private async postParContributions(url: string, title: string, contributions: string): Promise<void> {
    try {
      const resp = await fetch(url, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({
          "agent-id": AGENT_ID,
          "par/title": title,
          contributions: contributions,
        }),
      });
      if (resp.ok) {
        console.error(`[par] Posted contributions to ${url}`);
      } else {
        console.error(`[par] Failed to post contributions: ${resp.status}`);
      }
    } catch (e) {
      console.error(`[par] Error posting contributions: ${e}`);
    }
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
        ws?.send(JSON.stringify({ type: "register", agentId }));
      });

      ws.on("message", (data) => {
        try {
          const msg = JSON.parse(data.toString());
          console.error(`[agency-ws] Received: ${msg.type}`);

          // Skip connection confirmations
          if (msg.type === "connected" || msg.type === "registered" || msg.type === "pong") {
            return;
          }

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

function createForumInput(threadId: string, agentId: string): AsyncGenerator<InputEvent> {
  const seenPosts = new Set<string>();

  return (async function* () {
    let ws: WebSocket | null = null;
    const queue: InputEvent[] = [];
    let resolver: ((value: InputEvent) => void) | null = null;

    const connect = () => {
      const url = `${FORUM_WS_URL}?thread-id=${threadId}`;
      console.error(`[forum-ws] Connecting to ${url}...`);
      ws = new WebSocket(url);

      ws.on("open", () => {
        console.error(`[forum-ws] Connected to thread ${threadId}`);
      });

      ws.on("message", (data) => {
        try {
          const msg = JSON.parse(data.toString());

          // Handle init message - mark recent posts as seen
          if (msg.type === "init") {
            const recent = Array.isArray(msg["recent-posts"]) ? msg["recent-posts"] : [];
            for (const post of recent) {
              if (post["post/id"]) seenPosts.add(post["post/id"]);
            }
            console.error(`[forum-ws] Init: marked ${recent.length} recent posts as seen`);
            return;
          }

          // Handle new post
          if (msg.type === "post-created") {
            const post = msg.post || msg;
            const postId = post["post/id"];
            const author = post["post/author"] || "unknown";
            const body = post["post/body"] || "";

            // Skip if no post ID, already seen, or own post
            if (!postId || seenPosts.has(postId)) return;
            if (author === agentId || author === "fuclaude") return;
            // Skip posts marked as from this peripheral
            if (post["post/pattern-applied"] === "peripheral") return;

            seenPosts.add(postId);
            console.error(`[forum-ws] Post from ${author}: ${body.slice(0, 60)}...`);

            const event: InputEvent = {
              source: "forum",
              type: "post",
              payload: {
                postId,
                threadId,
                author,
                body,
              },
              timestamp: new Date().toISOString(),
            };

            if (resolver) {
              resolver(event);
              resolver = null;
            } else {
              queue.push(event);
            }
          }
        } catch (err) {
          console.error(`[forum-ws] Parse error: ${err}`);
        }
      });

      ws.on("close", () => {
        console.error(`[forum-ws] Disconnected, reconnecting in 5s...`);
        setTimeout(connect, 5000);
      });

      ws.on("error", (err) => {
        console.error(`[forum-ws] Error: ${err}`);
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

async function postForumReply(threadId: string, body: string, inReplyTo?: string): Promise<void> {
  try {
    const payload: any = {
      author: AGENT_ID,
      body: body,
      "pattern-applied": "peripheral",
    };
    if (inReplyTo) {
      payload["in-reply-to"] = inReplyTo;
    }

    const resp = await fetch(`${FORUM_HTTP_URL}/forum/thread/${threadId}/reply`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(payload),
    });

    if (resp.ok) {
      console.error(`[forum] Posted reply (${body.length} chars)`);
    } else {
      console.error(`[forum] Post failed: ${resp.status}`);
    }
  } catch (e) {
    console.error(`[forum] Post error: ${e}`);
  }
}

// ============================================================================
// Multiplexer
// ============================================================================

async function* multiplex(...sources: AsyncGenerator<InputEvent>[]): AsyncGenerator<InputEvent> {
  const iterators = sources.map((s) => s[Symbol.asyncIterator]());
  const pending = new Map<number, Promise<{ index: number; result: IteratorResult<InputEvent> }>>();

  for (let i = 0; i < iterators.length; i++) {
    pending.set(
      i,
      iterators[i].next().then((result) => ({ index: i, result }))
    );
  }

  while (pending.size > 0) {
    const { index, result } = await Promise.race(pending.values());

    if (result.done) {
      pending.delete(index);
    } else {
      yield result.value;
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
  let forumThread: string | undefined;

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
      case "--forum-thread":
        forumThread = args[++i];
        break;
      case "--help":
        console.log(`
fuclaude-peripheral - Multiplexed Claude Code wrapper with backpack & walkie-talkie

Usage:
  ./fuclaude-peripheral.ts [options]

Options:
  --resume <id>       Resume existing Claude Code session
  --agency-ws <url>   Agency WebSocket URL (default: ws://localhost:7070/agency/ws)
  --no-agency         Disable Agency connection (human-only mode)
  --forum-thread <id> Join a Forum thread (receives posts, replies automatically)
  --help              Show this help

Environment:
  AGENCY_WS_URL       Agency WebSocket URL
  FORUM_WS_URL        Forum WebSocket URL (default: ws://localhost:5055)
  FORUM_HTTP_URL      Forum HTTP URL (default: http://localhost:5050)
  FUCLAUDE_AGENT_ID   Agent identifier (default: fuclaude)

This wrapper multiplexes human input, Agency events, and Forum posts into a single
Claude Code session. Sessions are automatically persisted to a state file and
resumed on restart - the agent remembers previous interactions.

Example:
  # Start fresh session connected to Agency (auto-persists)
  ./fuclaude-peripheral.ts

  # Restart and auto-resume previous session
  ./fuclaude-peripheral.ts

  # Join a Forum thread
  ./fuclaude-peripheral.ts --forum-thread t-abc123

  # Explicitly resume a specific session
  ./fuclaude-peripheral.ts --resume 973b4921-7efc-4d15-bb0e-3eabfe652a17
`);
        process.exit(0);
    }
  }

  // Create Claude Code wrapper (auto-resumes from state file if available)
  const claude = new ClaudeCodeWrapper(resumeId);

  console.error(`[peripheral] Agent ID: ${AGENT_ID}`);
  console.error(`[peripheral] Session: ${claude.getSessionId() || "(will detect on first run)"}`);
  console.error(`[peripheral] Agency WS: ${enableAgency ? agencyWsUrl : "disabled"}`);
  console.error(`[peripheral] Forum: ${forumThread || "disabled"}`);
  console.error(`[peripheral] State: lab/agency/sessions/peripheral-${AGENT_ID}-state.json`);
  console.error(`[peripheral] Ready for multiplexed input`);
  console.error("");

  // Create input sources
  const sources: AsyncGenerator<InputEvent>[] = [createHumanInput()];

  if (enableAgency) {
    sources.push(createAgencyInput(agencyWsUrl, AGENT_ID));
  }

  if (forumThread) {
    sources.push(createForumInput(forumThread, AGENT_ID));
  }

  // Process multiplexed input
  for await (const event of multiplex(...sources)) {
    console.error(`\n[${event.source}] Processing ${event.type || "input"}...`);
    const response = claude.handleInput(event);

    // Output response
    if (response) {
      process.stdout.write(response);
      process.stdout.write("\n");

      // If from forum, post reply
      if (event.source === "forum" && forumThread) {
        await postForumReply(forumThread, response, event.payload.postId);
      }
    }
  }
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
