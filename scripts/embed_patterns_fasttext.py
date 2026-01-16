#!/usr/bin/env python3
"""Embed patterns with fastText word vectors."""
from __future__ import annotations

import argparse
import json
import math
import re
from pathlib import Path
from typing import Dict, Iterable, List, Optional

STOPWORDS = {
    "the", "and", "of", "to", "a", "in", "for", "with", "on", "by", "is",
    "are", "as", "that", "this", "be", "an", "or", "it", "at", "from",
    "not", "have", "has", "but", "if", "you", "your", "we", "our", "their",
    "they", "them", "into", "about", "can", "will", "may", "must", "should",
}

ARG_RE = re.compile(r"^@arg\s+(\S+)\s*$")
FLEXIARG_RE = re.compile(r"^@flexiarg\s+(\S+)\s*$")
MULTIARG_RE = re.compile(r"^@multiarg\s+(\S+)\s*$")
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


def mean_vector(vectors: Iterable[List[float]]) -> Optional[List[float]]:
    values = list(vectors)
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


def _iter_fasttext_text(path: Path, needed: Iterable[str]) -> Dict[str, List[float]]:
    needed_set = set(needed)
    vectors: Dict[str, List[float]] = {}
    with path.open("r", encoding="utf-8", errors="ignore") as handle:
        first = handle.readline()
        parts = first.strip().split()
        if len(parts) >= 2 and all(p.isdigit() for p in parts[:2]):
            pass
        else:
            handle.seek(0)
        for line in handle:
            items = line.rstrip().split()
            if len(items) < 3:
                continue
            word = items[0]
            if word in needed_set:
                try:
                    vectors[word] = [float(x) for x in items[1:]]
                except ValueError:
                    continue
    return vectors


def _load_fasttext_bin(path: Path):
    try:
        import fasttext  # type: ignore
    except ImportError as exc:
        raise SystemExit("fasttext is required to load .bin models") from exc
    return fasttext.load_model(str(path))


def main() -> None:
    parser = argparse.ArgumentParser(description="Embed patterns with fastText word vectors.")
    parser.add_argument("--fasttext", required=True, help="Path to fastText .vec or .bin file.")
    parser.add_argument("--out", default="resources/embeddings/fasttext_pattern_embeddings.json",
                        help="Output embeddings JSON.")
    parser.add_argument("--no-normalize", action="store_true",
                        help="Disable unit-length normalization.")
    args = parser.parse_args()

    patterns = parse_flexiargs() + parse_devmaps()
    texts = {p["id"]: tokenize(p.get("text", "")) for p in patterns}
    needed_tokens = sorted({tok for toks in texts.values() for tok in toks})

    fasttext_path = Path(args.fasttext)
    if fasttext_path.suffix == ".bin":
        model = _load_fasttext_bin(fasttext_path)
        token_vectors = {tok: model.get_word_vector(tok).tolist() for tok in needed_tokens}
    else:
        token_vectors = _iter_fasttext_text(fasttext_path, needed_tokens)

    output = []
    for pattern in patterns:
        toks = texts.get(pattern["id"], [])
        vecs = [token_vectors[tok] for tok in toks if tok in token_vectors]
        vec = mean_vector(vecs)
        if not vec:
            continue
        if not args.no_normalize:
            vec = normalize(vec)
        output.append({
            "id": pattern["id"],
            "title": pattern["title"],
            "source": pattern["source"],
            "vector": vec,
        })

    out_path = Path(args.out)
    out_path.parent.mkdir(parents=True, exist_ok=True)
    out_path.write_text(json.dumps(output, indent=2))
    print(f"Wrote {len(output)} embeddings to {out_path}")


if __name__ == "__main__":
    main()
