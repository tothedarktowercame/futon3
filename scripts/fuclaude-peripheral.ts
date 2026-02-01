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

class ClaudeCodeWrapper {
  private resumeId: string | undefined;
  private inputQueue: string[] = [];
  private isProcessing = false;
  private onOutput: (text: string) => void;

  constructor(resumeId?: string, onOutput?: (text: string) => void) {
    this.resumeId = resumeId;
    this.onOutput = onOutput || ((text) => process.stdout.write(text));
  }

  private processNext(): void {
    if (this.isProcessing || this.inputQueue.length === 0) {
      return;
    }

    const input = this.inputQueue.shift()!;
    this.runClaude(input);
  }

  private runClaude(input: string): void {
    // Escape the input for shell
    const escapedInput = input.replace(/'/g, "'\\''");
    const tmpFile = path.join(os.tmpdir(), `claude-out-${Date.now()}.txt`);

    let cmd = `claude --permission-mode bypassPermissions -p '${escapedInput}' > '${tmpFile}' 2>&1`;
    if (this.resumeId) {
      cmd = `claude --resume '${this.resumeId}' --permission-mode bypassPermissions -p '${escapedInput}' > '${tmpFile}' 2>&1`;
    }

    console.error(`[claude] Running: ${cmd.slice(0, 80)}...`);
    this.isProcessing = true;

    try {
      execSync(cmd, { timeout: 120000 }); // 2 minute timeout
      const output = fs.readFileSync(tmpFile, "utf-8");
      if (output) {
        this.onOutput(output);
        this.onOutput("\n");
      }
      console.error(`[claude] Completed`);
    } catch (error: any) {
      console.error(`[claude] Error: ${error.message}`);
      // Try to read partial output
      try {
        const output = fs.readFileSync(tmpFile, "utf-8");
        if (output) {
          this.onOutput(output);
          this.onOutput("\n");
        }
      } catch {}
    } finally {
      // Cleanup temp file
      try { fs.unlinkSync(tmpFile); } catch {}
      this.isProcessing = false;
      this.processNext();
    }
  }

  async handleInput(event: InputEvent): Promise<void> {
    const prefix = `[${event.source}${event.type ? ` ${event.type}` : ""}]`;
    let message: string;

    if (event.source === "agency" && event.type === "bell") {
      // Format bell data nicely
      const bellData = event.payload;
      message = `${prefix} Bell received!\n` +
        `Type: ${bellData["bell-type"] || bellData.bellType || "unknown"}\n` +
        `Secret ID: ${bellData["secret-id"] || bellData.secretId || "none"}\n` +
        `To verify receipt, fetch: curl ${AGENCY_HTTP_URL}/agency/secret/${bellData["secret-id"] || bellData.secretId}\n` +
        `Payload: ${JSON.stringify(bellData.payload || {})}`;
    } else if (typeof event.payload === "object") {
      message = `${prefix} ${JSON.stringify(event.payload)}`;
    } else {
      message = `${prefix} ${event.payload}`;
    }

    console.error(`[peripheral] Queuing: ${message.slice(0, 80)}...`);
    this.inputQueue.push(message);
    this.processNext();
  }

  getResumeId(): string | undefined {
    return this.resumeId;
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
fuclaude-peripheral - Multiplexed Claude Code wrapper with backpack & walkie-talkie

Usage:
  ./fuclaude-peripheral.ts [options]

Options:
  --resume <id>       Resume existing Claude Code session
  --agency-ws <url>   Agency WebSocket URL (default: ws://localhost:7070/agency/ws)
  --no-agency         Disable Agency connection (human-only mode)
  --help              Show this help

Environment:
  AGENCY_WS_URL       Agency WebSocket URL
  FUCLAUDE_AGENT_ID   Agent identifier (default: fuclaude)

This wrapper multiplexes human input and Agency events into a single Claude Code
session. Use --resume to continue an existing session (keeping memory and context).

Example:
  # Start fresh session connected to Agency
  ./fuclaude-peripheral.ts

  # Resume existing session
  ./fuclaude-peripheral.ts --resume 973b4921-7efc-4d15-bb0e-3eabfe652a17

  # Human-only mode (no Agency connection)
  ./fuclaude-peripheral.ts --no-agency
`);
        process.exit(0);
    }
  }

  // Create Claude Code wrapper
  const claude = new ClaudeCodeWrapper(resumeId);

  console.error(`[peripheral] Agent ID: ${AGENT_ID}`);
  console.error(`[peripheral] Resume: ${resumeId || "(new session)"}`);
  console.error(`[peripheral] Agency WS: ${enableAgency ? agencyWsUrl : "disabled"}`);
  console.error(`[peripheral] Ready for multiplexed input`);
  console.error("");

  // Create input sources
  const sources: AsyncGenerator<InputEvent>[] = [createHumanInput()];

  if (enableAgency) {
    sources.push(createAgencyInput(agencyWsUrl, AGENT_ID));
  }

  // Process multiplexed input
  for await (const event of multiplex(...sources)) {
    console.error(`\n[${event.source}] Processing ${event.type || "input"}...`);
    await claude.handleInput(event);
  }
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
