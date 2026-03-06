import { query } from "@anthropic-ai/claude-agent-sdk";

// Don't set ANTHROPIC_API_KEY - let CLI use its built-in auth
console.error("[test] Starting query (using CLI auth)...");

(async () => {
  try {
    for await (const msg of query({ prompt: "Say hello briefly", options: { maxTurns: 1 } })) {
      const type = (msg as any).type || "unknown";
      console.error(`[test] Got ${type}`);
      if ((msg as any).result) {
        console.log("Result:", (msg as any).result);
      }
    }
    console.error("[test] Done");
  } catch (e) {
    console.error("[test] Caught:", e);
  }
})();
