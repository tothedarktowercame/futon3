#!/usr/bin/env python3
import argparse
import json
import socket
import threading
import time
import uuid
import urllib.request
import urllib.error


class MusnBridge:
    def __init__(self, host, port, musn_url, poll_interval, default_room):
        self.host = host
        self.port = port
        self.musn_url = musn_url.rstrip("/")
        self.poll_interval = poll_interval
        self.default_room = default_room
        self.client = None
        self.client_file = None
        self.nick = None
        self.user = None
        self.room = None
        self.cursor = 0
        self.running = True
        self.poll_thread = None

    def log(self, msg):
        print(f"[musn-irc] {msg}", flush=True)

    def send_line(self, line):
        if not self.client:
            return
        data = (line + "\r\n").encode("utf-8", errors="ignore")
        try:
            self.client.sendall(data)
        except Exception:
            self.running = False

    def post(self, path, payload):
        url = f"{self.musn_url}{path}"
        data = json.dumps(payload).encode("utf-8")
        req = urllib.request.Request(
            url,
            data=data,
            headers={"content-type": "application/json", "accept": "application/json"},
            method="POST",
        )
        with urllib.request.urlopen(req, timeout=5) as resp:
            body = resp.read().decode("utf-8")
            return json.loads(body) if body else {}

    def safe_post(self, path, payload):
        try:
            return self.post(path, payload)
        except urllib.error.HTTPError as exc:
            try:
                body = exc.read().decode("utf-8")
            except Exception:
                body = ""
            self.log(f"HTTP error {exc.code} {path} {body}")
        except Exception as exc:
            self.log(f"POST error {path}: {exc}")
        return None

    def ensure_registered(self):
        if self.nick and self.user:
            self.send_line(f":musn 001 {self.nick} :Welcome to MUSN IRC bridge")
            if self.default_room and not self.room:
                self.handle_join(self.default_room)

    def handle_join(self, room):
        room = room.lstrip("#")
        if not room:
            return
        self.room = room
        self.send_line(f":{self.nick}!{self.user}@musn JOIN :#{room}")
        self.send_line(f":musn 332 {self.nick} #{room} :MUSN room {room}")
        self.send_line(f":musn 353 {self.nick} = #{room} :{self.nick}")
        self.send_line(f":musn 366 {self.nick} #{room} :End of /NAMES list.")

    def handle_privmsg(self, target, text):
        if not self.room:
            self.send_line(f":musn NOTICE {self.nick} :Join a room first with /join #room")
            return
        room = target.lstrip("#")
        if room and room != self.room:
            self.send_line(f":musn NOTICE {self.nick} :Unknown room #{room}")
            return
        payload = {
            "room": self.room,
            "msg-id": str(uuid.uuid4()),
            "author": {"id": self.nick or "user", "name": self.nick or "user"},
            "text": text,
        }
        self.safe_post("/musn/chat/message", payload)

    def poll_loop(self):
        while self.running:
            if not self.room or not self.nick:
                time.sleep(self.poll_interval)
                continue
            payload = {"room": self.room}
            if self.cursor > 0:
                payload["since"] = self.cursor
            resp = self.safe_post("/musn/chat/state", payload)
            if resp and resp.get("ok"):
                self.cursor = resp.get("cursor", self.cursor)
                events = resp.get("events") or []
                for event in events:
                    etype = event.get("event/type")
                    payload = event.get("payload") or {}
                    if etype != "chat/message":
                        continue
                    author = payload.get("author") or {}
                    name = author.get("name") or author.get("id") or "anon"
                    text = payload.get("text") or ""
                    for line in text.splitlines() or [""]:
                        self.send_line(f":{name}!{name}@musn PRIVMSG #{self.room} :{line}")
            time.sleep(self.poll_interval)

    def handle_line(self, raw):
        line = raw.strip("\r\n")
        if not line:
            return
        if line.startswith("PING"):
            token = line.split(" ", 1)[-1]
            self.send_line(f"PONG {token}")
            return
        if line.startswith("CAP "):
            if "LS" in line:
                self.send_line("CAP * LS :")
            elif "END" in line:
                return
        parts = line.split()
        cmd = parts[0].upper()
        if cmd == "NICK" and len(parts) > 1:
            self.nick = parts[1].lstrip(":")
            self.ensure_registered()
            return
        if cmd == "USER" and len(parts) > 1:
            self.user = parts[1]
            self.ensure_registered()
            return
        if cmd == "JOIN" and len(parts) > 1:
            room = parts[1].lstrip(":")
            self.handle_join(room)
            return
        if cmd == "PRIVMSG" and len(parts) > 2:
            target = parts[1]
            text = line.split(" :", 1)[-1] if " :" in line else ""
            self.handle_privmsg(target, text)
            return
        if cmd == "QUIT":
            self.running = False

    def serve(self):
        with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as srv:
            srv.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
            srv.bind((self.host, self.port))
            srv.listen(1)
            self.log(f"Listening on {self.host}:{self.port}")
            self.client, addr = srv.accept()
            self.log(f"Client connected from {addr[0]}:{addr[1]}")
            with self.client:
                self.client_file = self.client.makefile("r", encoding="utf-8", errors="ignore")
                self.poll_thread = threading.Thread(target=self.poll_loop, daemon=True)
                self.poll_thread.start()
                for raw in self.client_file:
                    if not self.running:
                        break
                    self.handle_line(raw)
            self.running = False
            self.log("Client disconnected")


def main():
    parser = argparse.ArgumentParser(description="MUSN IRC bridge (single client).")
    parser.add_argument("--host", default="127.0.0.1")
    parser.add_argument("--port", type=int, default=6667)
    parser.add_argument("--musn-url", default="http://localhost:6065")
    parser.add_argument("--poll-interval", type=float, default=2.0)
    parser.add_argument("--room", default="")
    args = parser.parse_args()

    bridge = MusnBridge(
        host=args.host,
        port=args.port,
        musn_url=args.musn_url,
        poll_interval=args.poll_interval,
        default_room=args.room,
    )
    bridge.serve()


if __name__ == "__main__":
    main()
