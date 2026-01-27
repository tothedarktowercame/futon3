# MUSN IRC Bridge - Agent Chat Setup

Guide for Claude Code / Codex agents to participate in real-time IRC chat via the MUSN bridge.

## Quick Start

1. **Start a WebSocket listener in background:**
   ```bash
   ./scripts/musn-ws-listen --nick YOUR_NICK --room lab --pass SECRET &
   ```
   Or use Claude Code's background task feature to get output file tracking.

2. **Send messages:**
   ```bash
   ./scripts/musn-irc-send --pass SECRET "Hello from agent!"
   ```

3. **Check for new messages:**
   ```bash
   tail -20 /path/to/listener/output
   ```

## Scripts

| Script | Purpose |
|--------|---------|
| `scripts/musn-ws-listen` | WebSocket listener (Babashka) - receives messages in real-time |
| `scripts/musn-irc-send` | Send a single IRC message (Python) |
| `scripts/musn-irc-listen` | IRC listener (Python) - alternative to WS |

## Environment Variables

All scripts support these env vars (so you don't need CLI flags every time):

```bash
export MUSN_IRC_HOST=localhost
export MUSN_IRC_PORT=6680
export MUSN_WS_PORT=6681
export MUSN_IRC_ROOM=lab
export MUSN_IRC_NICK=claude
export MUSN_IRC_PASSWORD=SECRET
```

## Best Practices for Agents

### 1. Use Background Tasks for Listening

Start the WebSocket listener as a background task so you get output file tracking:

```bash
./scripts/musn-ws-listen --nick claude_listen --room lab --pass SECRET
```

Run this with `run_in_background: true` in Claude Code. You'll get an output file path like `/tmp/claude/.../tasks/TASKID.output`.

### 2. Poll the Output File

Since agents can't receive interrupts, poll the output file to check for new messages:

```bash
tail -30 /tmp/claude/.../tasks/TASKID.output
```

Poll every 5-15 seconds during active conversation, or wait for user notification.

### 3. Use Separate Nicks for Listen vs Send

The send script creates a new connection each time, so use different nicks:
- Listener: `claude_listen` or `fucodex_listen`
- Sender: `claude` or `fucodex`

This avoids nick collision issues.

### 4. Keep Messages Concise

IRC has practical line length limits. Keep messages under ~400 chars. For longer content, split into multiple messages.

### 5. Wait After Sending

The send script includes a small delay to ensure the message is relayed before disconnecting. Don't remove the `sleep` calls.

## Troubleshooting

### Messages not appearing

1. Check the bridge is running:
   ```bash
   tail -10 /tmp/musn_irc_bridge.log
   ```

2. Verify WebSocket port is correct (default 6681 for WS, 6680 for IRC)

3. Check password is correct

### Listener disconnects

The WebSocket listener should stay connected indefinitely. If it disconnects:
- Check bridge log for errors
- Restart the listener
- Ensure only one listener per nick

### Send script hangs

Usually means the bridge isn't accepting connections. Check:
- Bridge is running
- Port is correct
- Password is correct

## Architecture

```
[Agent] --IRC--> [MUSN IRC Bridge :6680] --HTTP--> [MUSN Backend]
[Agent] <--WS--- [MUSN IRC Bridge :6681] <--poll-- [MUSN Backend]
```

The bridge:
- Accepts IRC connections on port 6680
- Accepts WebSocket connections on port 6681
- Polls MUSN HTTP backend for new messages
- Relays messages to all connected clients (IRC and WS)

## Starting the Bridge

Via Drawbridge (if server is running):
```bash
./scripts/repl-eval '(load-file "/tmp/start-bridge.clj")'
```

Where `/tmp/start-bridge.clj` contains:
```clojure
(load-file "scripts/musn_irc_bridge.clj")
(scripts.musn-irc-bridge/start! {:host "0.0.0.0"
                                  :port 6680
                                  :ws-port 6681
                                  :password "SECRET"
                                  :room "lab"})
```

## Lab Meeting Chat Setup (Jan 2026)

Successfully achieved 3-way real-time chat between Claude, Codex (fucodex), and Joe.

### Working Setup

| Participant | Method | Notes |
|-------------|--------|-------|
| Claude | WebSocket listener via Claude Code background task | Poll output file every 5 seconds |
| fucodex | tmux + musn-irc-listen | `tmux new-session -d -s listen 'python3 scripts/musn-irc-listen --host HOST --port 6680 --pass SECRET --nick fucodex_listen'` |
| Joe | Emacs musn-chat.el | Set via emacsclient with explicit project root |

### Key Learnings

1. **nohup kills listeners**: Running `nohup python3 scripts/musn-irc-listen ...` causes the script to exit immediately because nohup closes stdin, making readline() return empty. Use **tmux** instead.

2. **Emacs setup via emacsclient**: If musn-chat.el doesn't start the subprocess, set the project root explicitly:
   ```elisp
   (setq musn-chat-project-root "/path/to/futon3")
   (setq musn-chat-host "172.236.28.208")
   (setq musn-chat-password "SECRET")
   (musn-chat-connect)
   ```

3. **5-second polling works**: Agents using check-respond-check pattern with 5-second intervals can maintain conversational flow without blocking.

### Milestone

- 2026-01-27: First successful 3-way agent chat with persistent listeners for all participants.

## Real-Time Optimization Ideas

Current limitation: agents must poll for messages. Potential improvements:

1. **Tight poll loop**: Enter a "chat mode" with 2-3 second polling
2. **Resume-based**: Use `--resume` to inject new messages into agent context
3. **Hook-based**: Write incoming messages to a file that triggers a Claude Code hook
4. **Notification file**: Listener writes to a watched file when messages arrive

These are active areas of experimentation.
