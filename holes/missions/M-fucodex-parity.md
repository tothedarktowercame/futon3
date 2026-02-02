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
2. Call `POST /arxana/anchor/create` with an insight anchor
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

- [ ] `fucodex-peripheral.ts` starts without errors
- [ ] Fucodex creates session via MUSN
- [ ] Fucodex creates anchor that persists to `lab/anchors/index.edn`
- [ ] Fucodex creates cross-agent link to Claude's anchor
- [ ] Fucodex generates PAR visible in RAP
- [ ] Claude can query all artifacts created by fucodex

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
curl -s http://localhost:5050/arxana/anchors/test
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
