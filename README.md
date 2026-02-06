# futon3 — Interface Layer

futon3 is the central coordination layer of the futon stack. It hosts MUSN
(the session/transport protocol), the canonical pattern library (flexiargs +
sigils), and Agency (multi-agent coordination). All other futons route through
futon3 for pattern lookup, session management, and inter-agent communication.

> **Stack context**: futon0/1/2 provide data and experiments -> **futon3
> coordinates** -> futon4/5 consume patterns and scholia. See
> `../futon0/README.md` for the full stack diagram.

---

A modern Clojure rewrite of the vintage Monster Mountain / MUSN server. The transport provides a battle-tested ingest path plus an **optional** multi-user REPL (SAFE via SCI, ADMIN via tokenized unsafe eval) while we wait to slot in an external persistence layer. Futon3 is also the canonical interface layer: shared ChatGPT/tatami shims (e.g. `contrib/aob-chatgpt.el`), pattern libraries, and sigil tooling live here so every other futon, including Futon1 storage, depends on a single point of integration.

## Prerequisites
- JDK 11+ and `clojure` CLI tools (`brew install clojure/tools-deps` or similar)
- Network access the first time you run `make test`/`make dev` so dependencies can be cached into the repo-local `.m2/` directory

## Layout Highlights
- `src/f2/transport.clj` – WebSocket/HTTP bus, async back-pressure, optional REPL wiring
- `src/f2/repl.clj` – SAFE (SCI sandbox) + ADMIN (token-gated) evaluators
- `src/f2/ui.clj` – JSON facade (`/musn/clients`, `/musn/sessions`, `/musn/practices`, `/musn/export`)
- `src/f2/musn.clj` – system lifecycle + demo ingest hooks
- `src/f2/semantics.clj` – placeholder reasoning over transport history
- `dev/demo_events.ndjson` – sample ingest for `make demo`

## MUSN + HUD (fubar/fucodex) Quickstart
To run a MUSN-guided fucodex session with live HUD in Emacs:

1. Start MUSN HTTP (6065):
   ```bash
   clojure -M -m futon3.musn.http
   # MUSN HTTP server on 6065
   ```
2. Start drawbridge + transport/UI (6767/5050/6060):
   ```bash
   ADMIN_TOKEN=$(cat .admintoken) FUTON3_DRAWBRIDGE=1 ./scripts/dev.sh
   # Drawbridge on http://127.0.0.1:6767/repl ...
   # Transport on 5050, UI on 6060
   ```
3. In Emacs, run:
   ```elisp
   M-x fubar-musn-launch-and-view
   ;; enter your prompt/intent when prompted
   ```
   This opens a 2-up view (raw stream + HUD). HUD stays in sync with MUSN intent/session/sigils and shows pattern candidates + AIF suggestion. Click "Proceed" in the HUD to continue the run.

Port map (defaults):
- Drawbridge (nREPL over HTTP): 6767
- Transport/HUD format: 5050 (`HUD_SERVER`)
- UI + pattern-action RPC: 6060 (`FUTON3_CODEX_SERVER_URL`)
- MUSN HTTP: 6065 (`FUTON3_MUSN_URL`)
- MUSN IRC bridge: 6667 (`scripts/musn_irc_bridge.clj`)

## Quick Start
1. Install deps (first run populates `.m2/`):
   ```bash
   make test
   ```
2. Boot the stack:
   ```bash
   make dev
   ```
   - Transport (WS+HTTP) -> `http://localhost:5050`
   - UI endpoints -> `http://localhost:6060`
   - MUSN HTTP -> `http://localhost:6065`
3. (Optional) Load sample events:
   ```bash
   make demo
   # streams dev/demo_events.ndjson via the ingest path
   ```

## Fuclaude / Fucodex Usage

Use `fuclaude` and `fucodex` instead of raw `claude` / `codex` to get automatic session logging, pattern context injection, and audit trails.

### Common invocations

```bash
# Basic usage (replaces `claude -p "task"`)
./fuclaude --live -p "Fix the bug in parser.clj"

# Basic usage (replaces `codex exec "task"`)
./fucodex --live exec --prompt "Add unit tests for the API"

# With full MUSN tracking (recommended for important work)
./fuclaude --musn --live -p "Implement the new feature"
./fucodex --musn --live exec --prompt "Refactor authentication"

# Resume a previous session
./fuclaude --resume <session-id> -p "Continue from where we left off"
./fucodex --live resume --last "Continue work"

# Interactive mode with AIF pattern guidance
./fucodex --cli --with-aif
```

### What you get

- **Session logs**: `lab/raw/`, `lab/trace/`, `lab/stubs/`
- **Pattern context**: HUD injects relevant patterns into prompts
- **PSR/PUR capture**: Pattern Selection Records and Pattern Use Records
- **AIF scoring**: Adaptive scoring of pattern candidates
- **Intent extraction**: Automatic intent parsing for audit trails

### Shell aliases (optional)

Add to your `.bashrc` or `.zshrc`:

```bash
alias fc='cd /home/joe/code/futon3 && ./fuclaude --live -p'
alias fcm='cd /home/joe/code/futon3 && ./fuclaude --musn --live -p'
alias fx='cd /home/joe/code/futon3 && ./fucodex --live exec --prompt'
alias fxm='cd /home/joe/code/futon3 && ./fucodex --musn --live exec --prompt'
alias fxi='cd /home/joe/code/futon3 && ./fucodex --cli --with-aif'
```

## Elisp Package Tests
Use the package runner to target individual Elisp test files:
```bash
./scripts/test-elisp-packages.sh embedding
./scripts/test-elisp-packages.sh hud sessions
PACKAGE=bridge ./scripts/test-elisp-packages.sh
```
Supported packages: `embedding`, `hud`, `sessions`, `arxana`, `bridge`, `aob`, `all`.

## Devmap PDF (A3)
We keep the seven futon devmaps in `artifacts/devmaps.tex`. To regenerate the emoji-rich A3 PDF:
```bash
scripts/build_devmaps.sh
```
The script regenerates `holes/artifacts/devmaps.tex` via `scripts/make_devmaps_tex.py`, then runs lualatex inside `holes/`.

## Vitality Feeds (git activity)

FUTON0 collects raw git stats via `scripts/git_activity.py`. Futon3 then converts that JSON into a HUD-friendly EDN snapshot:

```bash
clojure -M:vitality/git-summary
# writes resources/vitality/git_summary.edn
```

Load the summary in code via `futon3.vitality.git` -- it exposes helpers such as `load-summary`, `daily-grid`, `streak`, and `dominant-sphere`.

## Running Tests
```bash
make test
```
Runs Datascript fixture tests, semantics checks, and transport/REPL dispatch tests.

### Emacs Test Tips
`scripts/test-elisp.sh` runs the chatgpt-shell HUD suite. To keep it gentle on memory:
- limit the scope with `ERT_SELECTOR`, e.g. `ERT_SELECTOR='futon3-prototype-*' ./scripts/test-elisp.sh`
- cap virtual memory when running the full suite: `ulimit -v 2097152 && ./scripts/test-elisp.sh`

## Additional Documentation

For detailed documentation on specific subsystems, see:

| Document | Topics |
|----------|--------|
| `README-musn-transport.md` | IRC Bridge, Chat Supervisor, WebSocket protocol, Drawbridge, HTTP ingest, UI routes |
| `README-tatami.md` | Tatami HUD, cue embeddings, intent sigil lifecycle, paramitas |
| `README-patterns.md` | Sigil adjacency builder, pattern embeddings CLI, Futon1 ingestion |
| `README-proofwork.md` | Transport contract, pattern canon, check DSL, trail instrumentation, workday bridge |
| `README-drawbridge.md` | Detailed nREPL-over-HTTP setup and remote access |

## Next Steps
- Swap the Datascript placeholder with the upcoming external graph/persistence package
- Expand the `musn.api` DSL surfaced to SAFE evals (graph queries, semantic helpers, etc.)
- Layer auth/tooling on top of the `eval` frame (CLI helpers, per-token quotas, observability)
