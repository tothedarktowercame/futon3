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
const MUSN_HTTP_URL = process.env.MUSN_HTTP_URL || "http://localhost:6065";
const PATTERN_CATALOG_PATH = process.env.PATTERN_CATALOG_PATH || "resources/sigils/patterns-index.tsv";

// ============================================================================
// Types
// ============================================================================

interface InputEvent {
  source: "human" | "agency" | "forum";
  type: string;
  payload: any;
  timestamp: string;
}

interface Pattern {
  id: string;           // e.g., "agent/pause-is-not-failure"
  tokipona: string;     // e.g., "lape"
  sigil: string;        // e.g., "不"
  rationale: string;    // Why/when to use
  hotwords: string[];   // Search terms
}

interface ActivePattern {
  pattern: Pattern;
  query: string;
  candidates: string[];
  selectedAt: string;
  confidence: "low" | "medium" | "high";
  rationale: string;
}

// ============================================================================
// Pattern Catalog
// ============================================================================

class PatternCatalog {
  private patterns: Pattern[] = [];
  private loaded = false;

  load(catalogPath: string = PATTERN_CATALOG_PATH): void {
    if (this.loaded) return;
    try {
      const content = fs.readFileSync(catalogPath, "utf-8");
      const lines = content.split("\n").slice(1); // Skip header

      for (const line of lines) {
        if (!line.trim()) continue;
        const [id, tokipona, sigil, rationale, hotwordsStr] = line.split("\t");
        if (!id) continue;

        this.patterns.push({
          id: id.trim(),
          tokipona: tokipona?.trim() || "",
          sigil: sigil?.trim() || "",
          rationale: rationale?.trim() || "",
          hotwords: (hotwordsStr || "").split(",").map(h => h.trim().toLowerCase()),
        });
      }

      this.loaded = true;
      console.error(`[patterns] Loaded ${this.patterns.length} patterns from ${catalogPath}`);
    } catch (e) {
      console.error(`[patterns] Failed to load catalog: ${e}`);
    }
  }

  search(query: string, limit: number = 5): Pattern[] {
    if (!this.loaded) this.load();

    const terms = query.toLowerCase().split(/\s+/);

    // Score each pattern by how many query terms match
    const scored = this.patterns.map(p => {
      let score = 0;
      const searchText = `${p.id} ${p.rationale} ${p.hotwords.join(" ")}`.toLowerCase();

      for (const term of terms) {
        if (searchText.includes(term)) score += 1;
        // Bonus for hotword exact match
        if (p.hotwords.includes(term)) score += 2;
        // Bonus for pattern name match
        if (p.id.toLowerCase().includes(term)) score += 3;
      }

      return { pattern: p, score };
    });

    return scored
      .filter(s => s.score > 0)
      .sort((a, b) => b.score - a.score)
      .slice(0, limit)
      .map(s => s.pattern);
  }

  getById(id: string): Pattern | undefined {
    if (!this.loaded) this.load();
    return this.patterns.find(p => p.id === id);
  }
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
  private patternCatalog = new PatternCatalog();
  private activePattern: ActivePattern | null = null;

  constructor(resumeId?: string, statePath?: string) {
    // Load pattern catalog at startup
    this.patternCatalog.load();
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

    // Check for PSR/PUR commands from human input
    if (event.source === "human") {
      const input = String(event.payload).trim();

      // /psr <query> - Pattern Selection Record
      if (input.startsWith("/psr ")) {
        const query = input.slice(5).trim();
        return this.handlePsrRequest(query);
      }
      if (input === "/psr") {
        return "Usage: /psr <query>\nExample: /psr stuck on testing";
      }

      // /pur [outcome] - Pattern Use Record
      if (input.startsWith("/pur")) {
        const outcomeHint = input.slice(4).trim() || undefined;
        return this.handlePurRequest(outcomeHint);
      }

      // /pattern - Show active pattern
      if (input === "/pattern" || input === "/backpack") {
        const active = this.getActivePattern();
        if (active) {
          return `Active pattern: ${active.pattern.id} [${active.pattern.sigil}]\nSelected for: ${active.query}\nConfidence: ${active.confidence}`;
        } else {
          return "No active pattern. Use /psr <query> to select one.";
        }
      }
    }

    if (event.source === "agency" && event.type === "bell") {
      const bellData = event.payload;
      const bellType = bellData["bell-type"] || bellData.bellType || "unknown";

      // Special handling for PAR bells
      if (bellType === "par-summon") {
        return this.handleParBell(bellData);
      }

      // Special handling for PSR bells
      if (bellType === "psr-request") {
        const query = bellData.payload?.query || bellData.query || "general guidance";
        return this.handlePsrRequest(query);
      }

      // Special handling for PUR bells
      if (bellType === "pur-request") {
        const outcomeHint = bellData.payload?.outcome || bellData.outcome;
        return this.handlePurRequest(outcomeHint);
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

  // ===========================================================================
  // PSR/PUR - Pattern Selection and Use Records (Pattern Card in Backpack)
  // ===========================================================================

  getActivePattern(): ActivePattern | null {
    return this.activePattern;
  }

  handlePsrRequest(query: string): string {
    console.error(`[psr] Searching patterns for: ${query}`);

    const candidates = this.patternCatalog.search(query, 5);

    if (candidates.length === 0) {
      return `No patterns found matching "${query}". Try different keywords.`;
    }

    // Format candidates for display
    const candidateList = candidates.map((p, i) =>
      `${i + 1}. **${p.id}** [${p.sigil}]\n   ${p.rationale}\n   Hotwords: ${p.hotwords.slice(0, 5).join(", ")}`
    ).join("\n\n");

    // Build prompt for Claude to help select
    const prompt = `[psr] Pattern Selection Record

You're selecting a pattern to guide your current work. Query: "${query}"

## Pattern Candidates

${candidateList}

## Your Task

Review these patterns and select the one that best fits the query "${query}".
Explain briefly why this pattern is the best fit (2-3 sentences).
Rate your confidence: low, medium, or high.

Format your response as:
SELECTED: <pattern id>
CONFIDENCE: <low/medium/high>
RATIONALE: <why this pattern fits>`;

    const response = this.runClaude(prompt);

    // Parse the response to extract selection
    const selectedMatch = response.match(/SELECTED:\s*(\S+)/i);
    const confidenceMatch = response.match(/CONFIDENCE:\s*(low|medium|high)/i);
    const rationaleMatch = response.match(/RATIONALE:\s*(.+?)(?:\n|$)/is);

    if (selectedMatch) {
      const selectedId = selectedMatch[1];
      const pattern = this.patternCatalog.getById(selectedId) || candidates[0];
      const confidence = (confidenceMatch?.[1] as "low" | "medium" | "high") || "medium";
      const rationale = rationaleMatch?.[1]?.trim() || "Selected as best match";

      // Store active pattern
      this.activePattern = {
        pattern,
        query,
        candidates: candidates.map(c => c.id),
        selectedAt: new Date().toISOString(),
        confidence,
        rationale,
      };

      // Log to activity stream
      this.logPsrToActivity();

      console.error(`[psr] Selected: ${pattern.id} [${pattern.sigil}] (${confidence})`);

      return `## PSR (Pattern Selection Record)
- **Cycle**: ${this.activePattern ? 1 : 0}
- **Query**: ${query}
- **Pattern chosen**: ${pattern.id}
- **Sigil**: ${pattern.sigil}
- **Candidates considered**: ${candidates.map(c => c.id).join(", ")}
- **Rationale**: ${rationale}
- **Confidence**: ${confidence}

---
${pattern.sigil} Pattern **${pattern.id}** is now in your backpack.

${pattern.rationale}

Use /pur when your work is complete to record the outcome.`;
    }

    return response;
  }

  handlePurRequest(outcomeHint?: string): string {
    if (!this.activePattern) {
      return `No active pattern in backpack. Use /psr <query> to select a pattern first.`;
    }

    const { pattern, query, selectedAt, confidence } = this.activePattern;

    console.error(`[pur] Recording outcome for: ${pattern.id}`);

    // Build prompt for Claude to assess outcome
    const prompt = `[pur] Pattern Use Record

You applied pattern **${pattern.id}** [${pattern.sigil}] to the query "${query}".
Selected at: ${selectedAt}
Confidence was: ${confidence}
${outcomeHint ? `Outcome hint: ${outcomeHint}` : ""}

Pattern rationale: ${pattern.rationale}

## Your Task

Reflect on how well this pattern served the work:
1. What actions did you take guided by this pattern?
2. What was the outcome? (success / partial / failed / pivoted / deferred)
3. Compare expected vs actual - what was the prediction error? (low / medium / high)
4. Any notes or learnings?

Format your response as:
ACTIONS: <brief summary of actions>
OUTCOME: <success/partial/failed/pivoted/deferred>
EXPECTED: <what you expected>
ACTUAL: <what actually happened>
PREDICTION_ERROR: <low/medium/high>
NOTES: <any additional learnings>`;

    const response = this.runClaude(prompt);

    // Parse the response
    const actionsMatch = response.match(/ACTIONS:\s*(.+?)(?:\n|$)/is);
    const outcomeMatch = response.match(/OUTCOME:\s*(success|partial|failed|pivoted|deferred)/i);
    const expectedMatch = response.match(/EXPECTED:\s*(.+?)(?:\n|$)/is);
    const actualMatch = response.match(/ACTUAL:\s*(.+?)(?:\n|$)/is);
    const errorMatch = response.match(/PREDICTION_ERROR:\s*(low|medium|high)/i);
    const notesMatch = response.match(/NOTES:\s*(.+?)(?:\n|$)/is);

    const outcome = outcomeMatch?.[1] || outcomeHint || "unknown";
    const actions = actionsMatch?.[1]?.trim() || "Not specified";
    const expected = expectedMatch?.[1]?.trim() || "Not specified";
    const actual = actualMatch?.[1]?.trim() || "Not specified";
    const predictionError = errorMatch?.[1] || "medium";
    const notes = notesMatch?.[1]?.trim() || "";

    // Log to activity stream
    this.logPurToActivity(outcome, actions, expected, actual, predictionError, notes);

    // Clear active pattern
    const clearedPattern = this.activePattern;
    this.activePattern = null;

    console.error(`[pur] Recorded: ${outcome}, prediction error: ${predictionError}`);

    return `## PUR (Pattern Use Record)
- **Pattern**: ${clearedPattern.pattern.id}
- **Sigil**: ${clearedPattern.pattern.sigil}
- **Actions taken**: ${actions}
- **Outcome**: ${outcome}
- **Expected**: ${expected}
- **Actual**: ${actual}
- **Prediction error**: ${predictionError}
- **Notes**: ${notes}

---
Pattern [${clearedPattern.pattern.sigil}] cleared from backpack. Ready for next /psr.`;
  }

  private async logPsrToActivity(): Promise<void> {
    if (!this.activePattern) return;

    const { pattern, query, candidates, confidence, rationale } = this.activePattern;

    try {
      const resp = await fetch(`${MUSN_HTTP_URL}/musn/activity/log`, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({
          "event/type": "pattern/psr",
          agent: AGENT_ID,
          source: "peripheral",
          "session/id": this.sessionId,
          "pattern/selected": pattern.id,
          "pattern/sigil": pattern.sigil,
          "pattern/candidates": candidates,
          "pattern/query": query,
          "pattern/confidence": confidence,
          "pattern/rationale": rationale,
        }),
      });
      if (resp.ok) {
        console.error(`[psr] Logged to activity stream`);
      }
    } catch (e) {
      console.error(`[psr] Failed to log to activity: ${e}`);
    }
  }

  private async logPurToActivity(
    outcome: string,
    actions: string,
    expected: string,
    actual: string,
    predictionError: string,
    notes: string
  ): Promise<void> {
    if (!this.activePattern) return;

    const { pattern, query } = this.activePattern;

    try {
      const resp = await fetch(`${MUSN_HTTP_URL}/musn/activity/log`, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({
          "event/type": "pattern/pur",
          agent: AGENT_ID,
          source: "peripheral",
          "session/id": this.sessionId,
          "pattern/id": pattern.id,
          "pattern/sigil": pattern.sigil,
          "pattern/query": query,
          "pattern/outcome": outcome,
          "pattern/actions": actions,
          "pattern/expected": expected,
          "pattern/actual": actual,
          "pattern/prediction-error": predictionError,
          "pattern/notes": notes,
        }),
      });
      if (resp.ok) {
        console.error(`[pur] Logged to activity stream`);
      }
    } catch (e) {
      console.error(`[pur] Failed to log to activity: ${e}`);
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

Backpack Commands:
  /psr <query>    Search patterns, select one to carry (Pattern Selection Record)
  /pur [outcome]  Record outcome of pattern application (Pattern Use Record)
  /pattern        Show the currently active pattern
  /backpack       Alias for /pattern

Example:
  # Start fresh session connected to Agency (auto-persists)
  ./fuclaude-peripheral.ts

  # Restart and auto-resume previous session
  ./fuclaude-peripheral.ts

  # Join a Forum thread
  ./fuclaude-peripheral.ts --forum-thread t-abc123

  # Explicitly resume a specific session
  ./fuclaude-peripheral.ts --resume 973b4921-7efc-4d15-bb0e-3eabfe652a17

  # Select a pattern to guide work
  /psr stuck on testing

  # Record outcome when done
  /pur success
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
  console.error(`[peripheral] Patterns: loaded from ${PATTERN_CATALOG_PATH}`);
  console.error(`[peripheral] State: lab/agency/sessions/peripheral-${AGENT_ID}-state.json`);
  console.error(`[peripheral] Backpack: walkie-talkie, ID card, pattern card`);
  console.error(`[peripheral] Ready for multiplexed input`);
  console.error(`[peripheral] Commands: /psr <query>, /pur [outcome], /pattern`);
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
