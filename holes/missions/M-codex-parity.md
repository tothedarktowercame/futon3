# Mission: Codex Parity with Claude

Verify that Codex can perform the same MUSN/Arxana operations as Claude, operating as a full agent (not just HTTP probes).

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

**Root cause identified**: Codex was using `localhost` URLs, which hit a local server on Codex's machine (OpenAI cloud), not the shared server on Joe's Linode.

## Server URLs (CRITICAL)

Codex must use the public Linode URLs, NOT localhost:

```bash
# Arxana/Transport
ARXANA_URL="https://172-236-28-208.ip.linodeusercontent.com:5051"

# MUSN HTTP
MUSN_URL="https://172.236.28.208:6065"

# Forum WebSocket
FORUM_WS_URL="wss://172-236-28-208.ip.linodeusercontent.com:5051/forum/stream/ws"

# Stack HUD
STACK_HUD_URL="https://172.236.28.208:6066"

# Futon1 API
FUTON1_API="https://172.236.28.208:8080/api/alpha"
```

## Parity Test Protocol

### Phase 1: Verify Shared Server

Both agents must hit the same running MUSN server.

```bash
# Codex runs:
curl -s "https://172-236-28-208.ip.linodeusercontent.com:5051/arxana/anchors/claude-parity-20260202T202200Z" | jq .

# Should return Claude's anchor (not empty)
```

If this returns empty, Codex is hitting a different server or the server was restarted.

### Phase 2: Codex Creates Anchor (Agent-Driven)

Codex should create an anchor as an *agent action*, not a curl probe:

1. Create a MUSN session:
```bash
curl -X POST https://172.236.28.208:6065/musn/session/create \
  -H "Content-Type: application/json" \
  -d '{"session/id":"codex-parity-live","session/source":"codex","session/project":"futon3"}'
```

2. Create an anchor:
```bash
curl -X POST https://172-236-28-208.ip.linodeusercontent.com:5051/arxana/anchor/create \
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
curl -s "https://172-236-28-208.ip.linodeusercontent.com:5051/arxana/anchors/codex-parity-live" | jq .
```

Should return the anchor Codex created.

### Phase 4: Cross-Agent Link

Create a link between Claude's anchor and Codex's anchor:

```bash
# Either agent can do this
curl -X POST https://172-236-28-208.ip.linodeusercontent.com:5051/arxana/link/create \
  -H "Content-Type: application/json" \
  -d '{"from":"codex-parity-live:turn-1:insight-1","to":"claude-parity-20260202T202200Z:turn-1:artifact-1","type":"extends","author":"codex","note":"Codex extends Claude anchor - parity proven"}'
```

Verify:
```bash
curl -s "https://172-236-28-208.ip.linodeusercontent.com:5051/arxana/links/codex-parity-live:turn-1:insight-1" | jq .
```

### Phase 5: PAR Generation

Codex generates a PAR:
```bash
curl -X POST https://172.236.28.208:6065/musn/par \
  -H "Content-Type: application/json" \
  -d '{"session/id":"codex-parity-live","par/questions":{"intention":"verify parity","happening":"created anchor and link","learned":"parity confirmed"},"par/tags":["parity","codex"]}'
```

Verify via RAP:
```bash
curl -s "https://172.236.28.208:6065/rap" | jq '.pars[-1]'
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

- [ ] Codex's anchor appears in `lab/anchors/index.edn`
- [ ] Claude can query Codex's anchor via API
- [ ] Cross-agent link exists in `lab/links/graph.edn`
- [ ] Codex's PAR appears in RAP results
- [ ] Both agents see consistent state

## Debugging

If anchors don't persist:

1. Check server is running: `curl -s https://172-236-28-208.ip.linodeusercontent.com:5051/arxana/anchors/test`
2. Check server working directory: should be `/home/joe/code/futon3`
3. Check for write errors in `*server*` buffer
4. Verify `lab/anchors/` directory exists and is writable

## Related

- `scripts/parity-check.sh` - Automated HTTP probe test
- `lab/handoffs/codex-parity-2026-02-02.md` - Original handoff doc
- `M-pattern-inference-engine.md` - Multi-agent coordination design
