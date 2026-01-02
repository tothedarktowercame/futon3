#!/usr/bin/env python3
"""Scan FUTON devmaps and emit a boundary readiness snapshot."""

from __future__ import annotations

import argparse
import datetime as dt
import re
from pathlib import Path
from typing import Dict, List, Any

ROOT = Path(__file__).resolve().parent.parent
DEFAULT_OUTPUT = ROOT / "resources" / "boundary.edn"
DEV_DIR = ROOT / "holes"
FUTON_IDS = [f"f{i}" for i in range(8)]
PROTOTYPE_RE = re.compile(r"^!\s")
PROTOTYPE_HEADER_RE = re.compile(r"^!\s+instantiated-by:\s+Prototype\s+(\d+)\b", re.IGNORECASE)
PROTOTYPE_ANY_RE = re.compile(r"Prototype\s+(\d+)\b", re.IGNORECASE)
EVIDENCE_RE = re.compile(r"^\s*\+\s+evidence:")
TODO_RE = re.compile(r"\bTODO\b")
CONFIG_PATH = ROOT / "boundary_targets.edn"


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Generate FUTON boundary readiness state")
    parser.add_argument("--output", type=Path, default=DEFAULT_OUTPUT, help="EDN output path")
    parser.add_argument("--max-prototype", type=int,
                        help="Only count missing evidence up through this prototype number")
    return parser.parse_args()


def read_file(path: Path) -> List[str]:
    try:
        return path.read_text(encoding="utf-8").splitlines()
    except FileNotFoundError:
        return []

def load_max_prototype(path: Path) -> int | None:
    if not path.exists():
        return None
    text = path.read_text(encoding="utf-8")
    match = re.search(r":max[_-]prototype\s+(\d+)", text)
    if not match:
        return None
    try:
        return int(match.group(1))
    except ValueError:
        return None

def scan_devmap(futon_id: str, max_prototype: int | None) -> Dict[str, Any]:
    path = DEV_DIR / f"{futon_id.replace('f', 'futon')}.devmap"
    entries = {
        "id": futon_id,
        "path": str(path.relative_to(ROOT)) if path.exists() else str(path),
        "exists": path.exists(),
    }
    if not path.exists():
        entries.update({
            "prototypes": 0,
            "evidence_blocks": 0,
            "missing_evidence": 0,
            "prototypes_total": 0,
            "evidence_blocks_total": 0,
            "missing_evidence_total": 0,
            "todo_count": 0,
        })
        return entries

    lines = read_file(path)
    protos_total = 0
    protos_scope = 0
    evid_total = 0
    evid_scope = 0
    prototypes: List[Dict[str, Any]] = []
    current = None

    def start_proto(title: str, number: int | None) -> None:
        nonlocal protos_total, protos_scope, current
        protos_total += 1
        in_scope = True
        if max_prototype is not None:
            in_scope = number is not None and number <= max_prototype
            if in_scope:
                protos_scope += 1
        else:
            protos_scope += 1
        current = {"title": title, "number": number, "in_scope": in_scope, "evidence": 0}
        prototypes.append(current)

    for line in lines:
        header = PROTOTYPE_HEADER_RE.match(line)
        if header:
            number = int(header.group(1))
            title = line.strip().lstrip("!").strip()
            title = re.sub(r"^instantiated-by:\s*", "", title, flags=re.IGNORECASE)
            start_proto(title, number)
            continue
        if PROTOTYPE_RE.match(line):
            number = None
            match = PROTOTYPE_ANY_RE.search(line)
            if match:
                number = int(match.group(1))
            title = line.strip().lstrip("!").strip()
            title = re.sub(r"^instantiated-by:\s*", "", title, flags=re.IGNORECASE)
            start_proto(title, number)
            continue
        if EVIDENCE_RE.match(line):
            evid_total += 1
            if current is not None:
                current["evidence"] += 1
                if current["in_scope"]:
                    evid_scope += 1
            continue

    missing_titles = [p["title"] for p in prototypes if p["in_scope"] and p["evidence"] == 0]
    missing_titles_total = [p["title"] for p in prototypes if p["evidence"] == 0]
    protos = protos_scope if max_prototype is not None else protos_total
    evid = evid_scope if max_prototype is not None else evid_total
    todos = sum(1 for line in lines if TODO_RE.search(line))
    missing = len(missing_titles)
    missing_total = len(missing_titles_total)
    mtime = dt.datetime.fromtimestamp(path.stat().st_mtime)

    entries.update({
        "last_modified": mtime.isoformat(),
        "prototypes": protos,
        "evidence_blocks": evid,
        "missing_evidence": missing,
        "missing_evidence_titles": missing_titles,
        "prototypes_total": protos_total,
        "evidence_blocks_total": evid_total,
        "missing_evidence_total": missing_total,
        "missing_evidence_total_titles": missing_titles_total,
        "todo_count": todos,
    })
    return entries


def to_edn(value: Any) -> str:
    if isinstance(value, dict):
        inner = " ".join(f":{k} {to_edn(v)}" for k, v in value.items())
        return f"{{{inner}}}"
    if isinstance(value, list):
        return f"[{ ' '.join(to_edn(v) for v in value)}]"
    if isinstance(value, str):
        escaped = value.replace('"', '\\"')
        return f'"{escaped}"'
    if isinstance(value, bool):
        return "true" if value else "false"
    if value is None:
        return "nil"
    if isinstance(value, (int, float)):
        return str(value)
    return to_edn(str(value))


def main() -> None:
    args = parse_args()
    max_prototype = args.max_prototype
    if max_prototype is None:
        max_prototype = load_max_prototype(CONFIG_PATH)
    snapshot = {
        "generated_at": dt.datetime.now().isoformat(),
        "milestone_prototype": max_prototype,
        "futons": [scan_devmap(fid, max_prototype) for fid in FUTON_IDS],
    }
    output = args.output
    output.parent.mkdir(parents=True, exist_ok=True)
    output.write_text(to_edn(snapshot) + "\n", encoding="utf-8")
    print(f"[boundary] wrote {output}")


if __name__ == "__main__":
    main()
