#!/usr/bin/env python3
"""Emit a futon summary combining devmap @state with boundary data."""

from __future__ import annotations

import argparse
import datetime as dt
import re
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
DEV_DIR = ROOT / "holes"
BOUNDARY_FILE = ROOT / "resources" / "boundary.edn"


def normalized_futon_id(raw: str) -> str:
    raw = raw.strip().lower()
    if raw.startswith("futon"):
        raw = "f" + raw[5:]
    if not raw.startswith("f"):
        raw = f"f{raw}"
    return raw


def devmap_path(futon_id: str) -> Path:
    number = futon_id[1:]
    return DEV_DIR / f"futon{number}.devmap"


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Compose a futon summary for agents")
    parser.add_argument("futon", help="Futon identifier (e.g., f0, f3)")
    parser.add_argument("--boundary", type=Path, default=BOUNDARY_FILE,
                        help="boundary.edn path (default: %(default)s)")
    parser.add_argument("--format", choices=["text", "json"], default="text",
                        help="Summary output format")
    return parser.parse_args()


def load_state(futon_id: str) -> str:
    path = devmap_path(futon_id)
    if not path.exists():
        return "(devmap not found)"
    text = path.read_text(encoding="utf-8")
    match = re.search(r"@state\s*(.+?)(?:\n@|$)", text, flags=re.S)
    return match.group(1).strip() if match else "(state not found)"


def load_boundary(boundary_file: Path, futon_id: str) -> str:
    if not boundary_file.exists():
        return "(boundary snapshot missing; run scripts/devmap_readiness.py)"
    text = boundary_file.read_text(encoding="utf-8")
    match = re.search(rf"\{{:id \"{re.escape(futon_id)}\" (.+?\}})", text)
    return match.group(0) if match else "(no boundary entry)"


def main() -> None:
    args = parse_args()
    futon_id = normalized_futon_id(args.futon)
    state = load_state(futon_id)
    boundary = load_boundary(args.boundary, futon_id)
    generated = dt.datetime.now().isoformat()
    summary = (
        f"=== Futon {futon_id.upper()} summary ({generated}) ===\n"
        f"@state:\n{state}\n\nBoundary:\n{boundary}\n"
    )
    print(summary)


if __name__ == "__main__":
    main()
# Temporary summary script
