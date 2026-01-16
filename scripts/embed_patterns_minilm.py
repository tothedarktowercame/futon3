#!/usr/bin/env python3
"""Embed patterns with a sentence-transformer model (MiniLM by default)."""
from __future__ import annotations

import argparse
import json
import re
from pathlib import Path
from typing import Dict, List

ARG_RE = re.compile(r"^@arg\s+(\S+)\s*$")
FLEXIARG_RE = re.compile(r"^@flexiarg\s+(\S+)\s*$")
MULTIARG_RE = re.compile(r"^@multiarg\s+(\S+)\s*$")
TITLE_RE = re.compile(r"^@title\s+(.*)\s*$")
CLAUSE_RE = re.compile(r"^\s*[+!]\s+([^:]+):\s*(.*)$")
DEV_HEADER_RE = re.compile(r"^!\s+instantiated-by:\s+Prototype\s+(\d+)\s+—\s+(.*)\s+\[(.*)\]\s*$")

ROOTS = [Path("library"), Path("holes/LDTS")]


def split_arg_blocks(text: str) -> List[str]:
    lines = text.splitlines()
    blocks: List[str] = []
    current: List[str] = []
    has_arg = False
    for line in lines:
        if line.startswith("@arg "):
            if has_arg:
                blocks.append("\n".join(current))
                current = [line]
            else:
                current = [line]
                has_arg = True
        else:
            current.append(line)
    if current:
        blocks.append("\n".join(current))
    return blocks


def parse_flexiargs() -> List[Dict[str, str]]:
    patterns: List[Dict[str, str]] = []
    for root in ROOTS:
        if not root.exists():
            continue
        for path in root.rglob("*"):
            if not path.is_file() or path.suffix.lower() not in {".flexiarg", ".multiarg"}:
                continue
            text = path.read_text()
            for block in split_arg_blocks(text):
                arg = None
                title = None
                content_lines: List[str] = []
                for line in block.splitlines():
                    m = ARG_RE.match(line)
                    if m:
                        arg = m.group(1)
                    mf = FLEXIARG_RE.match(line)
                    if mf and not arg:
                        arg = mf.group(1)
                    mm = MULTIARG_RE.match(line)
                    if mm and not arg:
                        arg = mm.group(1)
                    mt = TITLE_RE.match(line)
                    if mt:
                        title = mt.group(1).strip()
                    mc = CLAUSE_RE.match(line)
                    if mc:
                        content_lines.append(mc.group(2))
                if arg:
                    patterns.append({
                        "id": arg,
                        "title": title or arg,
                        "text": " ".join([title or "", *content_lines]),
                        "source": str(path),
                    })
    return patterns


def parse_devmaps() -> List[Dict[str, str]]:
    patterns: List[Dict[str, str]] = []
    for path in Path("holes").rglob("futon*.devmap"):
        lines = path.read_text().splitlines()
        current = None
        buffer: List[str] = []
        for line in lines + ["! instantiated-by: Prototype 0 — END [x/y]"]:
            header = DEV_HEADER_RE.match(line)
            if header:
                if current:
                    patterns.append({
                        "id": current["id"],
                        "title": current["title"],
                        "text": " ".join([current["title"], *buffer]),
                        "source": str(path),
                    })
                proto = header.group(1)
                title = header.group(2).strip()
                futon_match = re.search(r"futon(\d+)", path.name)
                futon_id = f"f{futon_match.group(1)}" if futon_match else "f?"
                current = {"id": f"{futon_id}/p{proto}", "title": title}
                buffer = []
            else:
                mc = CLAUSE_RE.match(line)
                if mc:
                    buffer.append(mc.group(2))
    return patterns


def main() -> None:
    parser = argparse.ArgumentParser(description="Embed patterns with MiniLM.")
    parser.add_argument("--model", default="sentence-transformers/all-MiniLM-L6-v2",
                        help="SentenceTransformer model name or path.")
    parser.add_argument("--out", default="resources/embeddings/minilm_pattern_embeddings.json",
                        help="Output embeddings JSON.")
    parser.add_argument("--batch-size", type=int, default=32, help="Embedding batch size.")
    parser.add_argument("--no-normalize", action="store_true",
                        help="Disable unit-length normalization.")
    args = parser.parse_args()

    try:
        from sentence_transformers import SentenceTransformer  # type: ignore
    except ImportError as exc:
        raise SystemExit("sentence-transformers is required for MiniLM embeddings") from exc

    patterns = parse_flexiargs() + parse_devmaps()
    texts = [p.get("text", "") for p in patterns]
    model = SentenceTransformer(args.model)
    vectors = model.encode(texts, batch_size=args.batch_size, normalize_embeddings=not args.no_normalize)

    output = []
    for pattern, vec in zip(patterns, vectors):
        output.append({
            "id": pattern["id"],
            "title": pattern["title"],
            "source": pattern["source"],
            "vector": [float(x) for x in vec],
        })

    out_path = Path(args.out)
    out_path.parent.mkdir(parents=True, exist_ok=True)
    out_path.write_text(json.dumps(output, indent=2))
    print(f"Wrote {len(output)} embeddings to {out_path}")


if __name__ == "__main__":
    main()
