# FuBar Quickstart (MUSN + IRC)

This is a minimal, working setup to drive fucodex via IRC and view runs in Emacs.

## 0) Prereqs
- JDK 11+ and `clojure` CLI installed
- Emacs with `futon3/contrib` on `load-path`
- Optional: an IRC client (ERC, irssi, etc.)

## 1) Start the stack
From `futon3/`:

```bash
make dev
```

This starts:
- MUSN HTTP: `http://localhost:6065`
- IRC bridge: `localhost:6667`
- UI/HUD: `http://localhost:6060`

## 2) Connect via IRC
Join `#lab` on `localhost:6667`.

Example (netcat):
```bash
nc 127.0.0.1 6667
NICK joe
USER joe 0 * :Joe
JOIN #lab
```

Use these commands in channel:
```
!new <prompt>
!task <followup>
```

## 3) Emacs setup
Load the FuBar client:

```elisp
(load-file "/home/joe/code/futon3/contrib/fubar.el")
```

Optional (avoid echo suppression in IRC):
```elisp
(setq fubar-chat-author-name "fucodex")
```

## 4) Auto-open the runner on `!new`
The chat poller will open the runner when it sees:
`Starting musn-...` messages.

If it’s not doing that, reload `fubar.el`:
```elisp
(load-file "/home/joe/code/futon3/contrib/fubar.el")
```

You can toggle auto-open:
```elisp
(setq fubar-chat-auto-open-runner t)
(setq fubar-chat-auto-open-authors '("fucodex"))
```

## 5) Manual runner (if needed)
Open a session by ID:
```elisp
M-x fubar-musn-view-session
```

The session id is printed in IRC as:
`Starting musn-...`

## 6) Troubleshooting
- **No IRC output**: check `/tmp/musn_irc_bridge.log`
- **No chat events**: check `/tmp/musn_chat_supervisor.log`
- **MUSN HTTP down**: `curl http://localhost:6065/health`
- **IRC echo suppression**: if your IRC nick matches the bot author, you won’t see messages from that author

## 7) Minimal Emacsclient launch
```bash
emacsclient -e '(progn (load-file "/home/joe/code/futon3/contrib/fubar.el") (setq fubar-chat-author-name "fucodex") (fubar-chat-launch-fucodex "Test run"))'
```
