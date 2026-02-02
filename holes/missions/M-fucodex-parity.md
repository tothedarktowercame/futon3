# Mission: Fucodex Peripheral Parity

Verify that the `fucodex-peripheral.ts` wrapper can perform the same MUSN/Arxana operations as Claude Code, proving the peripheral infrastructure supports full agent capabilities.

## Why This Matters

Running curl commands manually proves the APIs work. Running through `fucodex-peripheral.ts` proves:
- The peripheral can spawn Codex with correct environment
- Codex inside the peripheral can reach MUSN/Arxana
- Agency/Forum integration works end-to-end
- Memory/session state travels correctly

## Architecture

```
┌─────────────────────────────────────────────────────┐
│ Codex (main instance in cloud)                      │
│                                                     │
│  "Start fucodex peripheral for parity test"         │
│           │                                         │
│           ▼                                         │
│  ./scripts/fucodex-peripheral.ts                    │
│           │                                         │
│           ├── Connects to Agency WS (7070)          │
│           ├── Connects to MUSN HTTP (6065)          │
│           ├── Spawns local `codex` process          │
│           │                                         │
│           ▼                                         │
│  ┌─────────────────────────────────────────────┐   │
│  │ fucodex (Codex running inside peripheral)   │   │
│  │                                             │   │
│  │  Has access to:                             │   │
│  │  - Bash (can curl Arxana endpoints)         │   │
│  │  - File system (can verify index.edn)       │   │
│  │  - Pattern backpack (PSR/PUR)               │   │
│  │  - MUSN activity logging                    │   │
│  └─────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────┘
           │
           ▼
    Claude verifies: anchor in lab/anchors/index.edn?
```

## Prerequisites

1. MUSN server running (ports 5050, 6065)
2. Agency server running (port 7070) - optional but recommended
3. `codex` CLI installed and in PATH
4. Working directory: `/home/joe/code/futon3`

## Parity Test Protocol

### Step 1: Start Fucodex Peripheral

From the futon3 directory:

```bash
./scripts/fucodex-peripheral.ts
```

Or with explicit config:
```bash
MUSN_HTTP_URL=http://localhost:6065 \
AGENCY_WS_URL=ws://localhost:7070/agency/ws \
./scripts/fucodex-peripheral.ts
```

### Step 2: Inside Peripheral - Create Session

Once fucodex is running, tell it:

```
Create a MUSN session called "fucodex-parity-test" and an Arxana anchor to prove peripheral parity.
```

Fucodex should:
1. Call `POST /musn/session/create` with session-id `fucodex-parity-test`
2. Call `POST /lab/anchor/create` with an insight anchor
3. Verify the anchor was written to `lab/anchors/index.edn`

### Step 3: Inside Peripheral - Create Cross-Agent Link

Tell fucodex:

```
Create a link from your anchor to Claude's anchor at claude-parity-20260202T202200Z:turn-1:artifact-1
```

This proves fucodex can reference anchors created by other agents.

### Step 4: Inside Peripheral - Generate PAR

Tell fucodex:

```
Generate a PAR for this parity test session.
```

Should call `POST /musn/par` and verify it appears in RAP.

### Step 5: Claude Verifies

Claude (or human) runs:

```bash
# Check anchor exists
grep "fucodex-parity-test" lab/anchors/index.edn

# Check link exists
grep "fucodex-parity-test" lab/links/graph.edn

# Check PAR via API
curl -s "http://localhost:6065/rap" | jq '.pars[] | select(.["session-id"] | contains("fucodex"))'
```

## Success Criteria

- [x] `fucodex-peripheral.ts` starts without errors (2026-02-02)
- [x] Fucodex creates session via MUSN (session: fucodex-parity-test)
- [x] Fucodex creates anchor that persists to `lab/anchors/index.edn` (2 anchors created)
- [x] Fucodex creates cross-agent link to Claude's anchor (link-0596ad55, link-6e7d8191)
- [x] Fucodex generates PAR visible in RAP (par-9e255cc4, par-636b4a99)
- [x] Claude can query all artifacts created by fucodex (verified via /lab/anchors, /lab/links)

## Required Environment Variables

For the fucodex peripheral to reach the network from inside Codex sandbox:

```bash
FUCODEX_NO_SANDBOX=1      # Allow network access
FUCODEX_SIMPLE_MODE=1     # Simplified execution mode
```

## Debugging

### Peripheral won't start

```bash
# Check codex is installed
which codex

# Check Node/ts-node
npx ts-node --version

# Run with debug output
DEBUG=* ./scripts/fucodex-peripheral.ts
```

### Anchors not persisting

Inside fucodex, verify connectivity:
```bash
curl -s http://localhost:5050/lab/anchors/test
curl -s http://localhost:6065/musn/dashboard | jq .
```

If these fail, the peripheral can't reach MUSN.

### Session state issues

Check peripheral session file:
```bash
ls -la /tmp/fucodex-sessions/
```

## Enhancements (Future)

1. **Direct Arxana integration**: Add `createAnchor()`, `createLink()` functions to `fucodex-peripheral.ts` instead of relying on Codex calling curl
2. **PSR/PUR flow**: Test pattern selection and use recording through peripheral
3. **Agency summons**: Test receiving bells/summons via Agency WebSocket
4. **Forum bridge**: Test posting results to Forum thread

## Related

- `scripts/fucodex-peripheral.ts` - The peripheral wrapper
- `M-codex-parity.md` - Basic API parity (curl-based)
- `M-pattern-inference-engine.md` - PSR/PUR infrastructure
- `plugins/futon-peripherals/README.md` - Peripheral model docs
