#!/usr/bin/env python3
import argparse
import asyncio
import json
import sys
import urllib.parse

try:
    import websockets
except Exception as exc:
    print("websockets import failed:", exc, file=sys.stderr)
    sys.exit(2)


def build_messages(events, session_id):
    user = []
    assistant = []
    par = []
    u_idx = 1
    a_idx = 1
    timestamps = []
    for ev in events:
        ts = ev.get("timestamp")
        if ts:
            timestamps.append(ts)
        ev_type = ev.get("type")
        if ev_type == "user":
            text = ev.get("text")
            if text:
                user.append({
                    "id": f"{session_id}:u{u_idx:03d}",
                    "timestamp": ts,
                    "text": text,
                })
                u_idx += 1
        elif ev_type == "assistant":
            text = ev.get("text")
            if text:
                assistant.append({
                    "id": f"{session_id}:a{a_idx:03d}",
                    "timestamp": ts,
                    "text": text,
                })
                a_idx += 1
        elif ev_type == "par":
            par.append({
                "timestamp": ts,
                "text": ev.get("text", ""),
                "par-id": ev.get("par-id"),
                "tags": ev.get("tags"),
            })
    return user, assistant, par, timestamps


async def fetch_init(ws_url, path):
    if path:
        sep = "&" if "?" in ws_url else "?"
        ws_url = f"{ws_url}{sep}path={urllib.parse.quote(path)}"
    async with websockets.connect(ws_url, open_timeout=10) as ws:
        # First message should be init
        while True:
            raw = await ws.recv()
            msg = json.loads(raw)
            if msg.get("type") == "init":
                return msg


def main():
    ap = argparse.ArgumentParser(description="Export lab-ws session to lab/raw JSON")
    ap.add_argument("--ws-url", required=True, help="WebSocket base URL (e.g., wss://host:5057/fulab/lab/)")
    ap.add_argument("--path", required=True, help="Session JSONL path on server")
    ap.add_argument("--session-id", required=True, help="Session id")
    ap.add_argument("--project", default="", help="Project or repo root")
    ap.add_argument("--source", default="claude", help="Source label")
    ap.add_argument("--out", required=True, help="Output lab/raw JSON path")
    args = ap.parse_args()

    init = asyncio.run(fetch_init(args.ws_url, args.path))
    events = init.get("events", [])

    user_msgs, assistant_msgs, par_events, timestamps = build_messages(events, args.session_id)
    ts_start = min(timestamps) if timestamps else None
    ts_end = max(timestamps) if timestamps else None

    payload = {
        "lab/session-id": args.session_id,
        "lab/repo-root": args.project,
        "lab/timestamp-start": ts_start,
        "lab/timestamp-end": ts_end,
        "lab/user-messages": user_msgs,
        "lab/assistant-messages": assistant_msgs,
        "lab/files-touched": [],
        "lab/source": args.source,
        "lab/source-path": args.path,
    }
    if par_events:
        payload["lab/par-events"] = par_events

    with open(args.out, "w", encoding="utf-8") as f:
        json.dump(payload, f, indent=2)
    print(f"Wrote {args.out} ({len(user_msgs)} user, {len(assistant_msgs)} assistant, {len(par_events)} par)")


if __name__ == "__main__":
    main()
