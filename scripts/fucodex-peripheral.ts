#!/usr/bin/env -S npx ts-node
/**
 * fucodex-peripheral.ts - Multiplexed Codex wrapper with backpack & walkie-talkie
 *
 * A peripheral wrapper that receives input from multiple sources:
 * - Human (stdin / readline)
 * - Agency (WebSocket for bells, summons)
 *
 * All inputs feed into a single fucodex session, giving the agent
 * unified awareness of all interactions.
 *
 * Usage:
 *   ./fucodex-peripheral.ts
 *   ./fucodex-peripheral.ts --agency-ws ws://localhost:7070/agency/ws
 *   ./fucodex-peripheral.ts --resume <session-id>
 *
 * Env:
 *   AGENCY_WS_URL            - Agency WebSocket URL (default: ws://localhost:7070/agency/ws)
 *   AGENCY_HTTP_URL          - Agency HTTP URL (default: http://localhost:7070)
 *   FUCODEX_AGENT_ID         - Agent identifier (default: fucodex)
 *   FUCODEX_BIN              - Path to fucodex script (default: ../fucodex)
 *   FUCODEX_CODEX_BIN        - Path to codex binary (default: codex in PATH)
 *   FUCODEX_APPROVAL_POLICY  - Approval policy (default: never)
 *   FUCODEX_NO_SANDBOX       - Set to 1/true to pass --no-sandbox
 *   FUCODEX_PRINT_AGENT_OUTPUT - Set to 0/false to suppress agent output
 *   FUCODEX_IDLE_TIMEOUT_MS  - Kill fucodex after N ms of no output (default: 60000)
 *   FUCODEX_SIMPLE_MODE      - Set to 1/true to use codex exec directly (no lab stream)
 *   MUSN_HTTP_URL            - MUSN activity log URL (default: http://localhost:6065)
 *   PATTERN_CATALOG_PATH     - Path to patterns-index.tsv
 *   FORUM_WS_URL             - Forum WebSocket URL (optional)
 *   FORUM_HTTP_URL           - Forum HTTP URL (optional)
 */

import { spawn, spawnSync } from "child_process";
import * as fs from "fs";
import * as path from "path";
import * as readline from "readline";
// @ts-ignore - runtime module is available, but typings may be missing in this repo
import WebSocket from "ws";

// ============================================================================
// Configuration
// ============================================================================

const AGENT_ID = process.env.FUCODEX_AGENT_ID || "fucodex";
const AGENCY_WS_URL = process.env.AGENCY_WS_URL || "ws://localhost:7070/agency/ws";
const AGENCY_HTTP_URL = process.env.AGENCY_HTTP_URL || "http://localhost:7070";
const FORUM_WS_URL = process.env.FORUM_WS_URL || "wss://172-236-28-208.ip.linodeusercontent.com:5051/forum/stream/ws";
const FORUM_HTTP_URL = process.env.FORUM_HTTP_URL || "http://localhost:5050";
const MUSN_HTTP_URL = process.env.MUSN_HTTP_URL || "http://localhost:6065";
const PATTERN_CATALOG_PATH = process.env.PATTERN_CATALOG_PATH || "resources/sigils/patterns-index.tsv";
const scriptDir = path.dirname(process.argv[1] || ".");
const REPO_ROOT = path.resolve(scriptDir, "..");
const FUCODEX_BIN = process.env.FUCODEX_BIN || path.resolve(REPO_ROOT, "fucodex");
const FUCODEX_CODEX_BIN = process.env.FUCODEX_CODEX_BIN || "codex";
const FUCODEX_APPROVAL_POLICY = process.env.FUCODEX_APPROVAL_POLICY || "never";
const FUCODEX_NO_SANDBOX = ["1", "true", "yes"].includes(
  (process.env.FUCODEX_NO_SANDBOX || "").toLowerCase()
);
const FUCODEX_PRINT_AGENT_OUTPUT = !["0", "false", "no"].includes(
  (process.env.FUCODEX_PRINT_AGENT_OUTPUT || "").toLowerCase()
);
const FUCODEX_IDLE_TIMEOUT_MS = Number.parseInt(
  process.env.FUCODEX_IDLE_TIMEOUT_MS || "60000",
  10
);
const FUCODEX_SIMPLE_MODE = ["1", "true", "yes"].includes(
  (process.env.FUCODEX_SIMPLE_MODE || "").toLowerCase()
);

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
  id: string;
  tokipona: string;
  sigil: string;
  rationale: string;
  hotwords: string[];
}

interface ActivePattern {
  pattern: Pattern;
  query: string;
  candidates: string[];
  selectedAt: string;
  confidence: "low" | "medium" | "high";
  rationale: string;
}

interface SessionState {
  sessionId?: string;
  createdAt?: string;
  lastActive?: string;
  activePattern?: ActivePattern | null;
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
      const resolved = path.isAbsolute(catalogPath)
        ? catalogPath
        : path.resolve(REPO_ROOT, catalogPath);
      const content = fs.readFileSync(resolved, "utf-8");
      const lines = content.split("\n").slice(1);
      for (const line of lines) {
        if (!line.trim()) continue;
        const [id, tokipona, sigil, rationale, hotwordsStr] = line.split("\t");
        if (!id) continue;
        this.patterns.push({
          id: id.trim(),
          tokipona: tokipona?.trim() || "",
          sigil: sigil?.trim() || "",
          rationale: rationale?.trim() || "",
          hotwords: (hotwordsStr || "").split(",").map((h) => h.trim().toLowerCase()),
        });
      }
      this.loaded = true;
      console.error(`[patterns] Loaded ${this.patterns.length} patterns from ${resolved}`);
    } catch (e) {
      console.error(`[patterns] Failed to load catalog: ${e}`);
    }
  }

  search(query: string, limit: number = 5): Pattern[] {
    if (!this.loaded) this.load();
    const terms = query.toLowerCase().split(/\s+/);
    const scored = this.patterns.map((p) => {
      let score = 0;
      const searchText = `${p.id} ${p.rationale} ${p.hotwords.join(" ")}`.toLowerCase();
      for (const term of terms) {
        if (searchText.includes(term)) score += 1;
        if (p.hotwords.includes(term)) score += 2;
        if (p.id.toLowerCase().includes(term)) score += 3;
      }
      return { pattern: p, score };
    });
    return scored
      .filter((s) => s.score > 0)
      .sort((a, b) => b.score - a.score)
      .slice(0, limit)
      .map((s) => s.pattern);
  }

  getById(id: string): Pattern | undefined {
    if (!this.loaded) this.load();
    return this.patterns.find((p) => p.id === id);
  }
}

// ============================================================================
// fucodex Process
// ============================================================================

class FucodexWrapper {
  private resumeId: string | undefined;
  private sessionId: string | undefined;
  private statePath: string;
  private firstRun = true;
  private patternCatalog = new PatternCatalog();
  private activePattern: ActivePattern | null = null;
  private inputQueue: string[] = [];
  private isProcessing = false;
  private onOutput: (text: string) => void;
  private approvalPolicy: string;
  private noSandbox: boolean;
  private fucodexBin: string;
  private codexBin: string;
  private disableHud: boolean;
  private printAgentOutput: boolean;
  private simpleMode: boolean;

  constructor(
    resumeId?: string,
    onOutput?: (text: string) => void,
    approvalPolicy?: string,
    noSandbox?: boolean,
    fucodexBin?: string,
    codexBin?: string
  ) {
    this.patternCatalog.load();
    const sessionDir = path.join(REPO_ROOT, "lab", "agency", "sessions");
    this.statePath = path.join(sessionDir, `peripheral-${AGENT_ID}-state.json`);
    this.resumeId = resumeId;
    this.onOutput = onOutput || ((text) => process.stdout.write(text));
    this.approvalPolicy = approvalPolicy || FUCODEX_APPROVAL_POLICY;
    this.noSandbox = noSandbox ?? FUCODEX_NO_SANDBOX;
    this.fucodexBin = fucodexBin || FUCODEX_BIN;
    this.codexBin = codexBin || FUCODEX_CODEX_BIN;
    this.disableHud = true;
    this.printAgentOutput = true;
    this.simpleMode = FUCODEX_SIMPLE_MODE;
    this.loadState();
    if (resumeId) {
      this.sessionId = resumeId;
      this.firstRun = false;
    }
  }

  private loadState(): void {
    try {
      if (fs.existsSync(this.statePath)) {
        const data = JSON.parse(fs.readFileSync(this.statePath, "utf-8")) as SessionState;
        this.sessionId = data.sessionId;
        this.activePattern = data.activePattern || null;
        this.firstRun = false;
        console.error(`[session] Resumed from state: ${this.sessionId || "(none)"}`);
      }
    } catch {
      console.error(`[session] No state file, will start fresh`);
    }
  }

  private saveState(): void {
    try {
      fs.mkdirSync(path.dirname(this.statePath), { recursive: true });
      const state: SessionState = {
        sessionId: this.sessionId,
        createdAt: this.firstRun ? new Date().toISOString() : undefined,
        lastActive: new Date().toISOString(),
        activePattern: this.activePattern,
      };
      fs.writeFileSync(this.statePath, JSON.stringify(state, null, 2));
    } catch (e) {
      console.error(`[session] Failed to save state: ${e}`);
    }
  }

  private updateSessionIdFromOutput(text: string): void {
    const sessionMatch = text.match(/session id:\s*([a-z0-9-]+)/i);
    const labMatch = text.match(/session=([a-z0-9-]+)/i);
    const id = sessionMatch?.[1] || labMatch?.[1];
    if (id) {
      this.sessionId = id;
      if (this.firstRun) {
        this.firstRun = false;
      }
      this.saveState();
    }
  }

  setDisableHud(value: boolean): void {
    this.disableHud = value;
  }

  setPrintAgentOutput(value: boolean): void {
    this.printAgentOutput = value;
  }

  setSimpleMode(value: boolean): void {
    this.simpleMode = value;
  }

  private processNext(): void {
    if (this.isProcessing || this.inputQueue.length === 0) {
      return;
    }

    const input = this.inputQueue.shift()!;
    if (this.simpleMode) {
      this.runCodexSimple(input);
    } else {
      this.runFucodex(input, this.disableHud, this.printAgentOutput);
    }
  }

  private runCodexSimple(input: string): void {
    const args: string[] = ["exec", "--skip-git-repo-check"];
    if (this.approvalPolicy) {
      args.push("-c", `approval_policy="${this.approvalPolicy}"`);
    }
    if (this.noSandbox) {
      args.push("--dangerously-bypass-approvals-and-sandbox");
    }
    args.push(input);

    console.error(`[codex] Running: ${this.codexBin} ${args.slice(0, 6).join(" ")}...`);
    this.isProcessing = true;

    const child = spawn(this.codexBin, args, { cwd: REPO_ROOT, env: { ...process.env } });
    child.stdout.on("data", (data) => {
      const text = data.toString();
      this.onOutput(text);
      this.updateSessionIdFromOutput(text);
    });
    child.stderr.on("data", (data) => {
      const text = data.toString();
      process.stderr.write(text);
      this.updateSessionIdFromOutput(text);
    });
    child.on("close", (code) => {
      if (code && code !== 0) {
        console.error(`[codex] Error: exited with ${code}`);
      }
      this.saveState();
      console.error(`[codex] Completed`);
      this.isProcessing = false;
      this.processNext();
    });
  }

  private runFucodex(input: string, disableHud: boolean, printOutput: boolean): void {
    const args: string[] = ["--live"];

    if (this.approvalPolicy) {
      args.push("--approval-policy", this.approvalPolicy);
    }
    if (this.noSandbox) {
      args.push("--no-sandbox");
    }
    if (this.resumeId) {
      args.push("--resume", this.resumeId);
    }

    // NOTE: do not use --prompt here; resume relies on passing a bare prompt string.
    args.push(input);

    console.error(`[fucodex] Running: ${this.fucodexBin} ${args.slice(0, 6).join(" ")}...`);
    this.isProcessing = true;

    const env = { ...process.env };
    if (disableHud) {
      env.FUCODEX_HUD = "0";
    }
    env.FULAB_PRINT_AGENT_OUTPUT = printOutput ? "1" : "0";

    const child = spawn(this.fucodexBin, args, { cwd: REPO_ROOT, env });
    let lastOutput = Date.now();
    let summarySeen = false;
    let summaryTimer: NodeJS.Timeout | null = null;
    let forceTimer: NodeJS.Timeout | null = null;
    let completed = false;

    const finish = (code?: number | null) => {
      if (completed) {
        return;
      }
      completed = true;
      if (summaryTimer) {
        clearTimeout(summaryTimer);
      }
      if (forceTimer) {
        clearTimeout(forceTimer);
      }
      if (code && code !== 0) {
        console.error(`[fucodex] Error: exited with ${code}`);
      }
      this.saveState();
      console.error(`[fucodex] Completed`);
      this.isProcessing = false;
      this.processNext();
    };

    const scheduleSummaryKill = () => {
      if (summaryTimer) {
        return;
      }
      summaryTimer = setTimeout(() => {
        if (!child.killed) {
          console.error("[fucodex] Summary seen; terminating child to advance queue");
          child.kill("SIGTERM");
          if (!forceTimer) {
            forceTimer = setTimeout(() => {
              console.error("[fucodex] Forcing completion after summary timeout");
              try {
                child.kill("SIGKILL");
              } catch (_) {
                // ignore
              }
              finish(null);
            }, 3000);
          }
        }
      }, 1500);
    };

    const recordOutput = (text: string, isErr = false) => {
      lastOutput = Date.now();
      if (isErr) {
        process.stderr.write(text);
      } else {
        this.onOutput(text);
      }
      this.updateSessionIdFromOutput(text);
      if (!summarySeen && text.includes("[/FULAB-SUMMARY]")) {
        summarySeen = true;
        scheduleSummaryKill();
      }
    };

    const idleTimer = setInterval(() => {
      if (FUCODEX_IDLE_TIMEOUT_MS <= 0) {
        return;
      }
      if (Date.now() - lastOutput > FUCODEX_IDLE_TIMEOUT_MS) {
        console.error(`[fucodex] Idle timeout after ${FUCODEX_IDLE_TIMEOUT_MS}ms; terminating child`);
        child.kill("SIGTERM");
        clearInterval(idleTimer);
        if (!forceTimer) {
          forceTimer = setTimeout(() => {
            console.error("[fucodex] Forcing completion after idle timeout");
            try {
              child.kill("SIGKILL");
            } catch (_) {
              // ignore
            }
            finish(null);
          }, 3000);
        }
      }
    }, 1000);

    child.stdout.on("data", (data) => recordOutput(data.toString(), false));
    child.stderr.on("data", (data) => recordOutput(data.toString(), true));
    child.on("close", (code) => {
      clearInterval(idleTimer);
      finish(code);
    });
  }

  private runCodexBlocking(prompt: string): string {
    const args: string[] = ["exec", "--skip-git-repo-check"];
    if (this.approvalPolicy) {
      args.push("-c", `approval_policy="${this.approvalPolicy}"`);
    }
    if (this.noSandbox) {
      args.push("--dangerously-bypass-approvals-and-sandbox");
    }
    args.push(prompt);

    console.error(`[codex] Running: ${this.codexBin} ${args.slice(0, 6).join(" ")}...`);
    const result = spawnSync(this.codexBin, args, {
      cwd: REPO_ROOT,
      env: { ...process.env },
      encoding: "utf-8",
      maxBuffer: 10 * 1024 * 1024,
    });
    const output = `${result.stdout || ""}${result.stderr || ""}`;
    this.updateSessionIdFromOutput(output);
    this.saveState();
    return output;
  }

  runPromptBlocking(prompt: string): string {
    return this.runCodexBlocking(prompt);
  }

  getActivePattern(): ActivePattern | null {
    return this.activePattern;
  }

  private handlePsrRequest(query: string): string {
    console.error(`[psr] Searching patterns for: ${query}`);
    const candidates = this.patternCatalog.search(query, 5);
    if (candidates.length === 0) {
      return `No patterns found matching \"${query}\". Try different keywords.`;
    }

    const candidateList = candidates.map((p, i) =>
      `${i + 1}. **${p.id}** [${p.sigil}]\n   ${p.rationale}\n   Hotwords: ${p.hotwords.slice(0, 5).join(", ")}`
    ).join("\n\n");

    const prompt = `[psr] Pattern Selection Record

You're selecting a pattern to guide your current work. Query: \"${query}\"

## Pattern Candidates

${candidateList}

## Your Task

Review these patterns and select the one that best fits the query \"${query}\".
Explain briefly why this pattern is the best fit (2-3 sentences).
Rate your confidence: low, medium, or high.

Format your response as:
SELECTED: <pattern id>
CONFIDENCE: <low/medium/high>
RATIONALE: <why this pattern fits>`;

    const response = this.runCodexBlocking(prompt);
    const selectedMatch = response.match(/SELECTED:\s*(\S+)/i);
    const confidenceMatch = response.match(/CONFIDENCE:\s*(low|medium|high)/i);
    const rationaleMatch = response.match(/RATIONALE:\s*(.+?)(?:\n|$)/is);

    if (selectedMatch) {
      const selectedId = selectedMatch[1];
      const pattern = this.patternCatalog.getById(selectedId) || candidates[0];
      const confidence = (confidenceMatch?.[1] as "low" | "medium" | "high") || "medium";
      const rationale = rationaleMatch?.[1]?.trim() || "Selected as best match";

      this.activePattern = {
        pattern,
        query,
        candidates: candidates.map((c) => c.id),
        selectedAt: new Date().toISOString(),
        confidence,
        rationale,
      };
      this.saveState();
      void this.logPsrToActivity();
      console.error(`[psr] Selected: ${pattern.id} [${pattern.sigil}] (${confidence})`);

      return `## PSR (Pattern Selection Record)
- **Query**: ${query}
- **Pattern chosen**: ${pattern.id}
- **Sigil**: ${pattern.sigil}
- **Candidates considered**: ${candidates.map((c) => c.id).join(", ")}
- **Rationale**: ${rationale}
- **Confidence**: ${confidence}

---
${pattern.sigil} Pattern **${pattern.id}** is now in your backpack.

${pattern.rationale}

Use /pur when your work is complete to record the outcome.`;
    }

    return response;
  }

  private handlePurRequest(outcomeHint?: string): string {
    if (!this.activePattern) {
      return `No active pattern in backpack. Use /psr <query> to select a pattern first.`;
    }

    const { pattern, query, selectedAt, confidence } = this.activePattern;
    console.error(`[pur] Recording outcome for: ${pattern.id}`);

    const prompt = `[pur] Pattern Use Record

You applied pattern **${pattern.id}** [${pattern.sigil}] to the query \"${query}\".
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

    const response = this.runCodexBlocking(prompt);
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

    void this.logPurToActivity(outcome, actions, expected, actual, predictionError, notes);
    const clearedPattern = this.activePattern;
    this.activePattern = null;
    this.saveState();

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

  private handleParBell(bellData: any): string {
    const payload = bellData.payload || {};
    const parTitle = payload.title || payload["par/title"] || "Untitled PAR";
    const parContent = payload.content || payload["par/content"] || "";
    const sections = payload.sections || ["happening", "perspectives", "learned"];

    console.error(`[par] Received PAR summon: ${parTitle}`);
    console.error(`[par] Contributing to sections: ${sections.join(", ")}`);

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
    happening: "What actually happened? What approaches were taken and what worked?",
    perspectives: "What's your unique perspective on what occurred? What did you notice?",
    learned: "What explicit takeaways emerged? What was learned?",
    forward: "What should happen next? What actions or experiments to try?"
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

    const response = this.runCodexBlocking(prompt);
    const callbackUrl = payload.callback || payload["par/callback"];
    if (callbackUrl) {
      void this.postParContributions(callbackUrl, parTitle, response);
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
          contributions,
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
        console.error("[psr] Logged to activity stream");
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
        console.error("[pur] Logged to activity stream");
      }
    } catch (e) {
      console.error(`[pur] Failed to log to activity: ${e}`);
    }
  }

  async handleInput(event: InputEvent): Promise<void> {
    const prefix = `[${event.source}${event.type ? ` ${event.type}` : ""}]`;
    let message: string;

    if (event.source === "human") {
      const input = String(event.payload).trim();
      if (input.startsWith("/psr ")) {
        const query = input.slice(5).trim();
        const response = this.handlePsrRequest(query);
        this.onOutput(`${response}\n`);
        return;
      }
      if (input === "/psr") {
        this.onOutput("Usage: /psr <query>\nExample: /psr stuck on testing\n");
        return;
      }
      if (input.startsWith("/pur")) {
        const outcomeHint = input.slice(4).trim() || undefined;
        const response = this.handlePurRequest(outcomeHint);
        this.onOutput(`${response}\n`);
        return;
      }
      if (input === "/pattern" || input === "/backpack") {
        const active = this.getActivePattern();
        if (active) {
          this.onOutput(
            `Active pattern: ${active.pattern.id} [${active.pattern.sigil}]\nSelected for: ${active.query}\nConfidence: ${active.confidence}\n`
          );
        } else {
          this.onOutput("No active pattern. Use /psr <query> to select one.\n");
        }
        return;
      }
    }

    if (event.source === "agency" && event.type === "bell") {
      const bellData = event.payload;
      const bellType = bellData["bell-type"] || bellData.bellType || "unknown";

      if (bellType === "par-summon") {
        const response = this.handleParBell(bellData);
        this.onOutput(`${response}\n`);
        return;
      }

      if (bellType === "psr-request") {
        const query = bellData.payload?.query || bellData.query || "general guidance";
        const response = this.handlePsrRequest(query);
        this.onOutput(`${response}\n`);
        return;
      }

      if (bellType === "pur-request") {
        const outcomeHint = bellData.payload?.outcome || bellData.outcome;
        const response = this.handlePurRequest(outcomeHint);
        this.onOutput(`${response}\n`);
        return;
      }

      message = `${prefix} Bell received!\n` +
        `Type: ${bellType}\n` +
        `Secret ID: ${bellData["secret-id"] || bellData.secretId || "none"}\n` +
        `To verify receipt, fetch: curl ${AGENCY_HTTP_URL}/agency/secret/${bellData["secret-id"] || bellData.secretId}\n` +
        `Payload: ${JSON.stringify(bellData.payload || {})}`;
    } else if (event.source === "forum" && event.type === "post") {
      const { author, body } = event.payload || {};
      message = `${prefix} Post from ${author}:\n${body}\n\nRespond helpfully and concisely.`;
    } else if (typeof event.payload === "object") {
      message = `${prefix} ${JSON.stringify(event.payload)}`;
    } else {
      message = `${prefix} ${event.payload}`;
    }

    console.error(`[peripheral] Queuing: ${message.slice(0, 80)}...`);
    this.inputQueue.push(message);
    this.processNext();
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

let agencyWs: any = null;

function sendAgencyMessage(msg: any): void {
  if (agencyWs && agencyWs.readyState === WebSocket.OPEN) {
    agencyWs.send(JSON.stringify(msg));
  }
}

function createAgencyInput(url: string, agentId: string): AsyncGenerator<InputEvent> {
  return (async function* () {
    const queue: InputEvent[] = [];
    let resolver: ((value: InputEvent) => void) | null = null;

    const connect = () => {
      console.error(`[agency-ws] Connecting to ${url}...`);
      const socket: any = new WebSocket(`${url}?agent-id=${agentId}`);
      agencyWs = socket;

      socket.on("open", () => {
        console.error(`[agency-ws] Connected`);
        socket.send(JSON.stringify({ type: "register", agentId }));
      });

      socket.on("message", (data: any) => {
        try {
          const msg = JSON.parse(data.toString());
          console.error(`[agency-ws] Received: ${msg.type}`);

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

      socket.on("close", () => {
        console.error(`[agency-ws] Disconnected, reconnecting in 5s...`);
        agencyWs = null;
        setTimeout(connect, 5000);
      });

      socket.on("error", (err: any) => {
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

function createForumInput(url: string, agentId: string): AsyncGenerator<InputEvent> {
  const seenPosts = new Set<string>();

  return (async function* () {
    let ws: any = null;
    const queue: InputEvent[] = [];
    let resolver: ((value: InputEvent) => void) | null = null;

    const connect = () => {
      console.error(`[forum-ws] Connecting to ${url}...`);
      const socket: any = new WebSocket(url);
      ws = socket;

      socket.on("open", () => {
        console.error(`[forum-ws] Connected`);
        socket.send(JSON.stringify({ type: "register", agentId }));
      });

      socket.on("message", (data: any) => {
        try {
          const msg = JSON.parse(data.toString());
          if (msg.type === "connected" || msg.type === "registered" || msg.type === "pong") {
            return;
          }
          if (msg.type === "post" && msg.post?.id) {
            if (seenPosts.has(msg.post.id)) {
              return;
            }
            seenPosts.add(msg.post.id);
          }

          const event: InputEvent = {
            source: "forum",
            type: msg.type || "message",
            payload: msg.post || msg,
            timestamp: new Date().toISOString(),
          };

          if (resolver) {
            resolver(event);
            resolver = null;
          } else {
            queue.push(event);
          }
        } catch (err) {
          console.error(`[forum-ws] Parse error: ${err}`);
        }
      });

      socket.on("close", () => {
        console.error(`[forum-ws] Disconnected, reconnecting in 5s...`);
        setTimeout(connect, 5000);
      });

      socket.on("error", (err: any) => {
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
  let approvalPolicy = FUCODEX_APPROVAL_POLICY;
  let noSandbox = FUCODEX_NO_SANDBOX;
  let fucodexBin = FUCODEX_BIN;
  let codexBin = FUCODEX_CODEX_BIN;
  let disableHud = true;
  let printAgentOutput = FUCODEX_PRINT_AGENT_OUTPUT;
  let simpleMode = FUCODEX_SIMPLE_MODE;
  let forumWsUrl = FORUM_WS_URL;
  let enableForum = Boolean(FORUM_WS_URL);

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
      case "--approval-policy":
        approvalPolicy = args[++i];
        break;
      case "--no-sandbox":
        noSandbox = true;
        break;
      case "--fucodex-bin":
        fucodexBin = args[++i];
        break;
      case "--codex-bin":
        codexBin = args[++i];
        break;
      case "--hud":
        disableHud = false;
        break;
      case "--no-hud":
        disableHud = true;
        break;
      case "--simple":
        simpleMode = true;
        break;
      case "--live":
        simpleMode = false;
        break;
      case "--print-output":
        printAgentOutput = true;
        break;
      case "--no-print-output":
        printAgentOutput = false;
        break;
      case "--forum-ws":
        forumWsUrl = args[++i];
        enableForum = true;
        break;
      case "--no-forum":
        enableForum = false;
        break;
      case "--help":
        console.log(`
fucodex-peripheral - Multiplexed Codex wrapper with backpack & walkie-talkie

Usage:
  ./fucodex-peripheral.ts [options]

Options:
  --resume <id>           Resume existing fucodex session
  --agency-ws <url>       Agency WebSocket URL (default: ws://localhost:7070/agency/ws)
  --no-agency             Disable Agency connection (human-only mode)
  --approval-policy <p>   fucodex approval policy (default: never)
  --no-sandbox            Pass --no-sandbox to fucodex
  --hud                   Enable fucodex HUD (default: disabled)
  --no-hud                Disable fucodex HUD (default)
  --simple                Use codex exec directly (no lab stream)
  --live                  Use fucodex --live (default)
  --print-output          Print agent output (default)
  --no-print-output       Suppress agent output
  --forum-ws <url>        Forum WebSocket URL (optional)
  --no-forum              Disable Forum connection
  --fucodex-bin <path>    Path to fucodex script (default: ../fucodex)
  --codex-bin <path>      Path to codex binary (default: codex)
  --help                  Show this help

Environment:
  AGENCY_WS_URL, AGENCY_HTTP_URL, FUCODEX_AGENT_ID, FUCODEX_BIN,
  FUCODEX_CODEX_BIN, FUCODEX_APPROVAL_POLICY, FUCODEX_NO_SANDBOX,
  FUCODEX_PRINT_AGENT_OUTPUT, FUCODEX_IDLE_TIMEOUT_MS, FUCODEX_SIMPLE_MODE,
  MUSN_HTTP_URL, PATTERN_CATALOG_PATH, FORUM_WS_URL, FORUM_HTTP_URL
`);
        process.exit(0);
    }
  }

  const fucodex = new FucodexWrapper(resumeId, undefined, approvalPolicy, noSandbox, fucodexBin, codexBin);
  fucodex.setDisableHud(disableHud);
  fucodex.setPrintAgentOutput(printAgentOutput);
  fucodex.setSimpleMode(simpleMode);

  console.error(`[peripheral] Agent ID: ${AGENT_ID}`);
  console.error(`[peripheral] Resume: ${resumeId || "(new session)"}`);
  console.error(`[peripheral] fucodex: ${fucodexBin}`);
  console.error(`[peripheral] codex: ${codexBin}`);
  console.error(`[peripheral] repo root: ${REPO_ROOT}`);
  console.error(`[peripheral] Approval: ${approvalPolicy || "(default)"}`);
  console.error(`[peripheral] Sandbox: ${noSandbox ? "disabled" : "default"}`);
  console.error(`[peripheral] HUD: ${disableHud ? "disabled" : "enabled"}`);
  console.error(`[peripheral] Print output: ${printAgentOutput ? "enabled" : "disabled"}`);
  console.error(`[peripheral] Mode: ${simpleMode ? "simple" : "live"}`);
  console.error(`[peripheral] Agency WS: ${enableAgency ? agencyWsUrl : "disabled"}`);
  console.error(`[peripheral] Forum WS: ${enableForum ? forumWsUrl : "disabled"}`);
  console.error(`[peripheral] Ready for multiplexed input`);
  console.error("");

  const sources: AsyncGenerator<InputEvent>[] = [createHumanInput()];

  if (enableAgency) {
    sources.push(createAgencyInput(agencyWsUrl, AGENT_ID));
  }
  if (enableForum && forumWsUrl) {
    sources.push(createForumInput(forumWsUrl, AGENT_ID));
  }

  for await (const event of multiplex(...sources)) {
    console.error(`\n[${event.source}] Processing ${event.type || "input"}...`);

    // Standup bell: ack + self-report to MUSN (rendezvous handshake)
    if (event.source === "agency" && event.type === "bell"
        && (event.payload["bell-type"] || event.payload.bellType) === "standup") {
      const bellData = event.payload;
      const secretId = bellData["secret-id"] || bellData.secretId;
      const payload = bellData.payload || {};
      const room = payload.room || "standup";
      const prompt = payload.prompt || "Standup: What are you working on? Any blockers? 2-3 sentences.";
      const musnUrl = payload["musn-url"] || payload.musnUrl || MUSN_HTTP_URL;
      console.error(`[standup] Bell received. room=${room} secret=${secretId}`);

      // Step 1: Ack the bell (proves reception)
      if (secretId) {
        try {
          const secretResp = await fetch(`${AGENCY_HTTP_URL}/agency/secret/${secretId}`);
          const secretData = await secretResp.json();
          if (secretData.value) {
            await fetch(`${AGENCY_HTTP_URL}/agency/ack`, {
              method: "POST",
              headers: { "Content-Type": "application/json" },
              body: JSON.stringify({
                "secret-id": secretId,
                value: secretData.value,
                "agent-id": AGENT_ID,
              }),
            });
            console.error(`[standup] Acked bell ${secretId}`);
          }
        } catch (e) { console.error(`[standup] Ack error: ${e}`); }
      }

      // Step 2: Generate standup update
      try {
        const result = fucodex.runPromptBlocking(prompt);
        // Step 3: Post to MUSN as self
        await fetch(`${musnUrl}/musn/chat/message`, {
          method: "POST",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify({
            room,
            "msg-id": `standup-${AGENT_ID}-${Date.now()}`,
            author: { id: AGENT_ID, name: AGENT_ID },
            text: result,
          }),
        });
        console.error(`[standup] Posted to #${room} (${result.length} chars)`);
      } catch (e) { console.error(`[standup] Error: ${e}`); }
      continue;
    }

    // Whistle/page: synchronous request-response back over WebSocket
    if (event.source === "agency" && (event.type === "whistle" || event.type === "page")) {
      const msg = event.payload;
      const requestId = msg["request-id"] || msg.requestId;
      const prompt = msg.prompt || "What are you working on?";
      console.error(`[whistle] request-id=${requestId}, prompt=${prompt.slice(0, 60)}...`);
      try {
        const result = fucodex.runPromptBlocking(prompt);
        sendAgencyMessage({
          type: "whistle-response",
          "request-id": requestId,
          response: { result },
        });
        console.error(`[whistle] Response sent (${result.length} chars)`);
      } catch (e) {
        console.error(`[whistle] Error: ${e}`);
        sendAgencyMessage({
          type: "whistle-response",
          "request-id": requestId,
          response: { error: String(e) },
        });
      }
      continue;
    }

    await fucodex.handleInput(event);
  }
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
