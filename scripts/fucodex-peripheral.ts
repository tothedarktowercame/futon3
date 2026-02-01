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
 *   FUCODEX_APPROVAL_POLICY  - Approval policy (default: never)
 *   FUCODEX_NO_SANDBOX       - Set to 1/true to pass --no-sandbox
 */

import { spawn } from "child_process";
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
const scriptDir = path.dirname(process.argv[1] || ".");
const REPO_ROOT = path.resolve(scriptDir, "..");
const FUCODEX_BIN = process.env.FUCODEX_BIN || path.resolve(REPO_ROOT, "fucodex");
const FUCODEX_APPROVAL_POLICY = process.env.FUCODEX_APPROVAL_POLICY || "never";
const FUCODEX_NO_SANDBOX = ["1", "true", "yes"].includes(
  (process.env.FUCODEX_NO_SANDBOX || "").toLowerCase()
);

// ============================================================================
// Types
// ============================================================================

interface InputEvent {
  source: "human" | "agency";
  type: string;
  payload: any;
  timestamp: string;
}

// ============================================================================
// fucodex Process
// ============================================================================

class FucodexWrapper {
  private resumeId: string | undefined;
  private inputQueue: string[] = [];
  private isProcessing = false;
  private onOutput: (text: string) => void;
  private approvalPolicy: string;
  private noSandbox: boolean;
  private fucodexBin: string;
  private disableHud: boolean;

  constructor(
    resumeId?: string,
    onOutput?: (text: string) => void,
    approvalPolicy?: string,
    noSandbox?: boolean,
    fucodexBin?: string
  ) {
    this.resumeId = resumeId;
    this.onOutput = onOutput || ((text) => process.stdout.write(text));
    this.approvalPolicy = approvalPolicy || FUCODEX_APPROVAL_POLICY;
    this.noSandbox = noSandbox ?? FUCODEX_NO_SANDBOX;
    this.fucodexBin = fucodexBin || FUCODEX_BIN;
    this.disableHud = true;
  }

  setDisableHud(value: boolean): void {
    this.disableHud = value;
  }

  private processNext(): void {
    if (this.isProcessing || this.inputQueue.length === 0) {
      return;
    }

    const input = this.inputQueue.shift()!;
    this.runFucodex(input, this.disableHud);
  }

  private runFucodex(input: string, disableHud: boolean): void {
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

    const child = spawn(this.fucodexBin, args, { cwd: REPO_ROOT, env });
    child.stdout.on("data", (data) => this.onOutput(data.toString()));
    child.stderr.on("data", (data) => process.stderr.write(data.toString()));
    child.on("close", (code) => {
      if (code && code !== 0) {
        console.error(`[fucodex] Error: exited with ${code}`);
      }
      console.error(`[fucodex] Completed`);
      this.isProcessing = false;
      this.processNext();
    });
  }

  async handleInput(event: InputEvent): Promise<void> {
    const prefix = `[${event.source}${event.type ? ` ${event.type}` : ""}]`;
    let message: string;

    if (event.source === "agency" && event.type === "bell") {
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
    let ws: any = null;
    const queue: InputEvent[] = [];
    let resolver: ((value: InputEvent) => void) | null = null;

    const connect = () => {
      console.error(`[agency-ws] Connecting to ${url}...`);
      const socket: any = new WebSocket(`${url}?agent-id=${agentId}`);
      ws = socket;

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
  let disableHud = true;

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
      case "--hud":
        disableHud = false;
        break;
      case "--no-hud":
        disableHud = true;
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
  --fucodex-bin <path>    Path to fucodex script (default: ../fucodex)
  --help                  Show this help

Environment:
  AGENCY_WS_URL, AGENCY_HTTP_URL, FUCODEX_AGENT_ID, FUCODEX_BIN,
  FUCODEX_APPROVAL_POLICY, FUCODEX_NO_SANDBOX
`);
        process.exit(0);
    }
  }

  const fucodex = new FucodexWrapper(resumeId, undefined, approvalPolicy, noSandbox, fucodexBin);
  fucodex.setDisableHud(disableHud);

  console.error(`[peripheral] Agent ID: ${AGENT_ID}`);
  console.error(`[peripheral] Resume: ${resumeId || "(new session)"}`);
  console.error(`[peripheral] fucodex: ${fucodexBin}`);
  console.error(`[peripheral] repo root: ${REPO_ROOT}`);
  console.error(`[peripheral] Approval: ${approvalPolicy || "(default)"}`);
  console.error(`[peripheral] Sandbox: ${noSandbox ? "disabled" : "default"}`);
  console.error(`[peripheral] HUD: ${disableHud ? "disabled" : "enabled"}`);
  console.error(`[peripheral] Agency WS: ${enableAgency ? agencyWsUrl : "disabled"}`);
  console.error(`[peripheral] Ready for multiplexed input`);
  console.error("");

  const sources: AsyncGenerator<InputEvent>[] = [createHumanInput()];

  if (enableAgency) {
    sources.push(createAgencyInput(agencyWsUrl, AGENT_ID));
  }

  for await (const event of multiplex(...sources)) {
    console.error(`\n[${event.source}] Processing ${event.type || "input"}...`);
    await fucodex.handleInput(event);
  }
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
