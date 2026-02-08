# Mission: Codex Parity with Claude

Verify that Codex can perform the same MUSN/Arxana operations as Claude, operating as a full agent (not just HTTP probes).

## Owner

Codex

## Scope

### Scope In

- Ensure Codex is hitting the shared MUSN/Lab endpoints (no accidental localhost-to-wrong-host).
- Run parity protocol end-to-end: create session, create anchor, create link, generate PAR, verify via RAP.
- Document the required environment variables / URLs for parity runs.

### Scope Out

- Improving the underlying APIs (this mission is verification, not feature work).
- Peripheral-level parity (covered by `M-fucodex-parity.md`).

## Time Box

2-4 hours.

## Exit Conditions

- All success criteria are checked with concrete evidence (session ids, anchor ids, link ids, PAR ids).
- If parity fails due to environment mismatch, stop after documenting the mismatch and the required configuration.

## Definition of Parity

"Parity" means Codex can:
1. Create and persist Arxana anchors that Claude can query
2. Create links between anchors (including cross-session)
3. Generate PARs that appear in RAP results
4. Use the same peripheral infrastructure (reflect, chat)
5. Coordinate with Claude via shared state (TaskList, Forum, or Arxana)

## The Problem (2026-02-02)

Codex ran `scripts/parity-check.sh` which reported PASS, but:
- The anchor created by Codex didn't appear in `lab/anchors/index.edn`
- Claude's identical run DID persist the anchor
- Session file showed `:arxana/anchor-created` event but anchor wasn't in global index

**Root cause identified**: Codex was using `localhost` URLs, which hit a local server on Codex's cloud environment, not the shared remote server.

## Server URLs (CRITICAL)

Codex must use the remote server URLs, NOT localhost. Configure these environment variables:

```bash
# Get actual URLs from Joe or check Emacs config
export LAB_URL="$FUTON3_REMOTE_LAB_URL"         # Remote lab/transport endpoint (data layer)
export MUSN_URL="$FUTON3_REMOTE_MUSN_URL"       # Remote MUSN HTTP endpoint
export FORUM_WS_URL="$FUTON3_REMOTE_FORUM_WS"   # Remote Forum WebSocket
```

The parity script accepts these as env vars:
```bash
LAB_URL="..." MUSN_URL="..." ./scripts/parity-check.sh
```

Reference: Check Emacs config for actual values (`my-chatgpt-shell-musn-url`, `arxana-forum-server`, etc.)

## Parity Test Protocol

### Phase 1: Verify Shared Server

Both agents must hit the same running MUSN server.

```bash
# Codex runs:
curl -s "http://localhost:5050/lab/anchors/claude-parity-20260202T202200Z" | jq .

# Should return Claude's anchor (not empty)
```

If this returns empty, Codex is hitting a different server or the server was restarted.

### Phase 2: Codex Creates Anchor (Agent-Driven)

Codex should create an anchor as an *agent action*, not a curl probe:

1. Create a MUSN session:
```bash
curl -X POST http://localhost:6065/musn/session/create \
  -H "Content-Type: application/json" \
  -d '{"session/id":"codex-parity-live","session/source":"codex","session/project":"futon3"}'
```

2. Create an anchor:
```bash
curl -X POST http://localhost:5050/lab/anchor/create \
  -H "Content-Type: application/json" \
  -d '{"session-id":"codex-parity-live","turn":1,"type":"insight","content":"Codex parity test - this anchor should be visible to Claude","author":"codex","note":"M-codex-parity Phase 2"}'
```

3. Verify persistence:
```bash
grep "codex-parity-live" lab/anchors/index.edn
```

### Phase 3: Claude Verifies

Claude queries for Codex's anchor:
```bash
curl -s "http://localhost:5050/lab/anchors/codex-parity-live" | jq .
```

Should return the anchor Codex created.

### Phase 4: Cross-Agent Link

Create a link between Claude's anchor and Codex's anchor:

```bash
# Either agent can do this
curl -X POST http://localhost:5050/lab/link/create \
  -H "Content-Type: application/json" \
  -d '{"from":"codex-parity-live:turn-1:insight-1","to":"claude-parity-20260202T202200Z:turn-1:artifact-1","type":"extends","author":"codex","note":"Codex extends Claude anchor - parity proven"}'
```

Verify:
```bash
curl -s "http://localhost:5050/lab/links/codex-parity-live:turn-1:insight-1" | jq .
```

### Phase 5: PAR Generation

Codex generates a PAR:
```bash
curl -X POST http://localhost:6065/musn/par \
  -H "Content-Type: application/json" \
  -d '{"session/id":"codex-parity-live","par/questions":{"intention":"verify parity","happening":"created anchor and link","learned":"parity confirmed"},"par/tags":["parity","codex"]}'
```

Verify via RAP:
```bash
curl -s "http://localhost:6065/rap" | jq '.pars[-1]'
```

### Phase 6: Peripheral Test (Optional)

Test reflect peripheral:
```bash
# If Codex can invoke /peripherals:par or similar
```

Test chat peripheral:
```bash
# If Codex can post to IRC channel
```

## Success Criteria

- [x] Codex's anchor appears in `lab/anchors/index.edn` (codex-parity-20260202T211454Z)
- [x] Claude can query Codex's anchor via API (verified 2026-02-02)
- [x] Cross-agent link exists in `lab/links/graph.edn` (verified via fucodex-parity-test)
- [x] Codex's PAR appears in RAP results (par-9e255cc4, par-636b4a99)
- [x] Both agents see consistent state (verified 2026-02-02)

## Debugging

If anchors don't persist:

1. Check server is running: `curl -s http://localhost:5050/lab/anchors/test`
2. Check server working directory: should be `/home/joe/code/futon3`
3. Check for write errors in `*server*` buffer
4. Verify `lab/anchors/` directory exists and is writable

## Related

- `scripts/parity-check.sh` - Automated HTTP probe test
- `lab/handoffs/codex-parity-2026-02-02.md` - Original handoff doc
- `M-pattern-inference-engine.md` - Multi-agent coordination design
