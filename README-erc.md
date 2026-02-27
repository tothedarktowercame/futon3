# Connecting to MUSN IRC Bridge with ERC

The futon3 dev server includes an IRC bridge on port 6667 that connects to the MUSN chat system.

## Prerequisites

1. Start the futon3 dev server:
   ```bash
   make dev
   ```

2. Verify IRC is running:
   ```bash
   ss -tlnp | grep 6667
   # Should show: LISTEN ... 127.0.0.1:6667
   ```

## Connect from Emacs

### Interactive

```
M-x erc RET
Server: 127.0.0.1 RET
Port: 6667 RET
Nickname: yournick RET
Password: RET (leave blank)
```

Then join the lab channel:
```
/join #lab
```

### Programmatic (emacsclient)

Connect and join in one go:

```bash
# Connect to IRC
emacsclient -e '(erc :server "127.0.0.1" :port 6667 :nick "yournick")'

# Wait a moment for connection, then join #lab
emacsclient -e '(with-current-buffer "MUSNIRC" (erc-cmd-JOIN "#lab"))'

# Show the channel buffer
emacsclient -e '(pop-to-buffer "#lab")'
```

### Elisp function

Add to your init.el for quick access:

```elisp
(defun futon-irc ()
  "Connect to local MUSN IRC bridge."
  (interactive)
  (erc :server "127.0.0.1"
       :port 6667
       :nick (user-login-name)
       :full-name user-full-name)
  (run-at-time 1 nil
               (lambda ()
                 (when (get-buffer "MUSNIRC")
                   (with-current-buffer "MUSNIRC"
                     (erc-cmd-JOIN "#lab"))))))
```

Then: `M-x futon-irc RET`

## Configuration

Environment variables (set before `make dev`):

| Variable | Default | Description |
|----------|---------|-------------|
| `FUTON3_IRC_BRIDGE` | enabled | Set to `0` to disable |
| `FUTON3_IRC_HOST` | `127.0.0.1` | Bind address |
| `FUTON3_IRC_PORT` | `6667` | Port number |
| `FUTON3_IRC_PASSWORD` | (none) | Optional password |

### Remote Access (for multi-agent coordination)

By default IRC binds to localhost only. To allow remote agents (e.g., Codex) to connect, set:

```bash
FUTON3_IRC_HOST=0.0.0.0
```

This is already set in `.env.dev`. Remote agents connect using your server's IP/hostname.

**Security note**: Consider setting `FUTON3_IRC_PASSWORD` if exposing to untrusted networks.

Example with custom port:
```bash
FUTON3_IRC_PORT=6668 make dev
```

## Channels

- `#lab` - Main chat room, connected to MUSN chat supervisor

## Troubleshooting

**Connection refused**
- Check server is running: `ss -tlnp | grep 6667`
- Use IPv4 explicitly: `127.0.0.1` not `localhost`

**No such buffer "MUSNIRC"**
- The buffer name may vary; check with:
  ```bash
  emacsclient -e '(mapcar #'"'"'buffer-name (buffer-list))' | tr ',' '\n' | grep -i irc
  ```

**Messages not appearing**
- The IRC bridge polls MUSN; there may be a short delay
- Check `*Messages*` buffer for ERC errors
