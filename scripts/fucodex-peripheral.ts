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
    this.resumeId = resumeId;
    this.onOutput = onOutput || ((text) => process.stdout.write(text));
    this.approvalPolicy = approvalPolicy || FUCODEX_APPROVAL_POLICY;
    this.noSandbox = noSandbox ?? FUCODEX_NO_SANDBOX;
    this.fucodexBin = fucodexBin || FUCODEX_BIN;
    this.codexBin = codexBin || FUCODEX_CODEX_BIN;
    this.disableHud = true;
    this.printAgentOutput = true;
    this.simpleMode = FUCODEX_SIMPLE_MODE;
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
    child.stdout.on("data", (data) => this.onOutput(data.toString()));
    child.stderr.on("data", (data) => process.stderr.write(data.toString()));
    child.on("close", (code) => {
      if (code && code !== 0) {
        console.error(`[codex] Error: exited with ${code}`);
      }
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
  let codexBin = FUCODEX_CODEX_BIN;
  let disableHud = true;
  let printAgentOutput = FUCODEX_PRINT_AGENT_OUTPUT;
  let simpleMode = FUCODEX_SIMPLE_MODE;

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
  --fucodex-bin <path>    Path to fucodex script (default: ../fucodex)
  --codex-bin <path>      Path to codex binary (default: codex)
  --help                  Show this help

Environment:
  AGENCY_WS_URL, AGENCY_HTTP_URL, FUCODEX_AGENT_ID, FUCODEX_BIN,
  FUCODEX_CODEX_BIN, FUCODEX_APPROVAL_POLICY, FUCODEX_NO_SANDBOX,
  FUCODEX_PRINT_AGENT_OUTPUT, FUCODEX_IDLE_TIMEOUT_MS, FUCODEX_SIMPLE_MODE
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
