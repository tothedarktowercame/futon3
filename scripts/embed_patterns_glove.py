#!/usr/bin/env python3
"""Embed patterns with GloVe and compute nearest neighbors."""
from __future__ import annotations

import argparse
import json
import math
import re
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Tuple

STOPWORDS = {
    "the", "and", "of", "to", "a", "in", "for", "with", "on", "by", "is",
    "are", "as", "that", "this", "be", "an", "or", "it", "at", "from",
    "not", "have", "has", "but", "if", "you", "your", "we", "our", "their",
    "they", "them", "into", "about", "can", "will", "may", "must", "should",
}

ARG_RE = re.compile(r"^@arg\s+(\S+)\s*$")
TITLE_RE = re.compile(r"^@title\s+(.*)\s*$")
CLAUSE_RE = re.compile(r"^\s*[+!]\s+([^:]+):\s*(.*)$")
DEV_HEADER_RE = re.compile(r"^!\s+instantiated-by:\s+Prototype\s+(\d+)\s+—\s+(.*)\s+\[(.*)\]\s*$")

ROOTS = [Path("library"), Path("holes/LDTS")]


def tokenize(text: str) -> List[str]:
    return [
        tok for tok in re.findall(r"[a-z0-9]+", text.lower())
        if tok not in STOPWORDS
    ]


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
        # handled by sentinel header
    return patterns


def collect_tokens(patterns: Iterable[Dict[str, str]]) -> List[str]:
    tokens: List[str] = []
    for pattern in patterns:
        tokens.extend(tokenize(pattern.get("text", "")))
    return sorted(set(tokens))


def load_glove(glove_path: Path, needed: Iterable[str]) -> Dict[str, List[float]]:
    needed_set = set(needed)
    vectors: Dict[str, List[float]] = {}
    with glove_path.open("r", encoding="utf-8") as handle:
        for line in handle:
            parts = line.strip().split()
            if len(parts) < 3:
                continue
            word = parts[0]
            if word in needed_set:
                vectors[word] = [float(x) for x in parts[1:]]
    return vectors


def mean_vector(tokens: Iterable[str], vectors: Dict[str, List[float]]) -> Optional[List[float]]:
    values = [vectors[tok] for tok in tokens if tok in vectors]
    if not values:
        return None
    length = len(values[0])
    summed = [0.0] * length
    for vec in values:
        for i, val in enumerate(vec):
            summed[i] += val
    return [val / len(values) for val in summed]


def normalize(vec: List[float]) -> List[float]:
    norm = math.sqrt(sum(v * v for v in vec))
    if norm == 0.0:
        return vec
    return [v / norm for v in vec]


def cosine(a: List[float], b: List[float]) -> float:
    return sum(x * y for x, y in zip(a, b))


def main() -> None:
    parser = argparse.ArgumentParser(description="Embed patterns with GloVe and compute neighbors.")
    parser.add_argument("--glove", required=True, help="Path to GloVe vectors (e.g. glove.6B.50d.txt)")
    parser.add_argument("--report", default="data/sigils/glove_pattern_neighbors.json",
                        help="Output report path (JSON).")
    parser.add_argument("--top", type=int, default=6, help="Number of neighbors per pattern.")
    args = parser.parse_args()

    patterns = parse_flexiargs() + parse_devmaps()
    tokens = collect_tokens(patterns)
    vectors = load_glove(Path(args.glove), tokens)

    embeds: Dict[str, List[float]] = {}
    meta: Dict[str, Dict[str, str]] = {}
    for pattern in patterns:
        toks = tokenize(pattern.get("text", ""))
        vec = mean_vector(toks, vectors)
        if vec:
            embeds[pattern["id"]] = normalize(vec)
            meta[pattern["id"]] = {
                "title": pattern["title"],
                "source": pattern["source"],
            }

    report = []
    ids = list(embeds.keys())
    for pid in ids:
        vec = embeds[pid]
        scored = []
        for other in ids:
            if other == pid:
                continue
            score = cosine(vec, embeds[other])
            scored.append((other, score))
        scored.sort(key=lambda x: x[1], reverse=True)
        neighbors = [{
            "id": other,
            "score": round(score, 6),
            "title": meta[other]["title"],
            "source": meta[other]["source"],
        } for other, score in scored[: args.top]]
        report.append({
            "id": pid,
            "title": meta[pid]["title"],
            "source": meta[pid]["source"],
            "neighbors": neighbors,
        })

    out_path = Path(args.report)
    out_path.parent.mkdir(parents=True, exist_ok=True)
    out_path.write_text(json.dumps(report, indent=2, ensure_ascii=False))
    print(f"Wrote {len(report)} entries to {out_path}")


if __name__ == "__main__":
    main()
