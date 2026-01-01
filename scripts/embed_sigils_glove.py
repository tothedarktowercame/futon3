#!/usr/bin/env python3
"""Suggest sigils using GloVe embeddings and local gloss vocab."""
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

SIGIL_BLOCK_RE = re.compile(r"\[[^\]]*[^ \[\]]+/[^ \[\]]+[^\]]*\]")
SIGIL_TOKEN_RE = re.compile(r"[^ \[\]]+/[^ \[\]]+")
ARG_RE = re.compile(r"^@arg\s+(\S+)\s*$")
SIGILS_RE = re.compile(r"^@sigils\s+\[(.*)\]\s*$")
TITLE_RE = re.compile(r"^@title\s+(.*)\s*$")
CLAUSE_RE = re.compile(r"^\s*[+!]\s+([^:]+):\s*(.*)$")
DEV_HEADER_RE = re.compile(r"^!\s+instantiated-by:\s+Prototype\s+(\d+)\s+—\s+(.*)\s+\[(.*)\]\s*$")

ROOTS = [Path("library"), Path("holes/LDTS")]


def tokenize(text: str) -> List[str]:
    return [
        tok for tok in re.findall(r"[a-z0-9]+", text.lower())
        if tok not in STOPWORDS
    ]


def load_tokipona_map(path: Path) -> Dict[str, str]:
    mapping: Dict[str, str] = {}
    if not path.exists():
        return mapping
    for line in path.read_text().splitlines():
        if not line.startswith("|") or line.startswith("|---"):
            continue
        cols = [col.strip() for col in line.split("|")][1:-1]
        if len(cols) < 2:
            continue
        emoji = cols[0]
        word = cols[1].lower()
        if emoji and word and "toki pona" not in word:
            mapping[emoji] = word
    return mapping


def load_json_map(path: Path) -> Dict[str, str]:
    if not path.exists():
        return {}
    return json.loads(path.read_text())


def parse_inline_sigils(text: str) -> List[Dict[str, str]]:
    sigils: List[Dict[str, str]] = []
    for token in SIGIL_TOKEN_RE.findall(text or ""):
        emoji, hanzi = token.split("/", 1)
        sigils.append({"emoji": emoji.strip(), "hanzi": hanzi.strip()})
    return sigils


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


def collect_tokens(patterns: Iterable[Dict[str, str]],
                   tok_gloss: Dict[str, str],
                   hanzi_gloss: Dict[str, str]) -> List[str]:
    tokens: List[str] = []
    for pattern in patterns:
        tokens.extend(tokenize(pattern.get("text", "")))
    for gloss in tok_gloss.values():
        tokens.extend(tokenize(gloss))
    for gloss in hanzi_gloss.values():
        tokens.extend(tokenize(gloss))
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


def rank_similar(target: List[float], candidates: Dict[str, List[float]], top_n: int) -> List[Tuple[str, float]]:
    scored = [(key, cosine(target, vec)) for key, vec in candidates.items()]
    scored.sort(key=lambda x: x[1], reverse=True)
    return scored[:top_n]


def build_vocab_vectors(tok_map: Dict[str, str],
                        tok_gloss: Dict[str, str],
                        hanzi_gloss: Dict[str, str],
                        vectors: Dict[str, List[float]]) -> Tuple[Dict[str, List[float]], Dict[str, List[float]]]:
    emoji_vectors: Dict[str, List[float]] = {}
    for emoji, word in tok_map.items():
        gloss = tok_gloss.get(word, "")
        text = f"{word} {gloss}"
        tokens = tokenize(text)
        vec = mean_vector(tokens, vectors)
        if vec:
            emoji_vectors[emoji] = normalize(vec)

    hanzi_vectors: Dict[str, List[float]] = {}
    for hanzi, gloss in hanzi_gloss.items():
        tokens = tokenize(gloss)
        vec = mean_vector(tokens, vectors)
        if vec:
            hanzi_vectors[hanzi] = normalize(vec)

    return emoji_vectors, hanzi_vectors


def apply_sigils(report: List[Dict[str, object]], overwrite: bool) -> int:
    choices = {}
    for entry in report:
        pairs = entry.get("pairs") or []
        if pairs:
            pair = pairs[0]
            emoji = pair.get("emoji")
            hanzi = pair.get("hanzi")
            if emoji and hanzi:
                choices[entry["id"]] = f"[{emoji}/{hanzi}]"

    changed = 0
    for root in ROOTS:
        if not root.exists():
            continue
        for path in root.rglob("*"):
            if not path.is_file() or path.suffix.lower() not in {".flexiarg", ".multiarg"}:
                continue
            text = path.read_text()
            blocks = split_arg_blocks(text)
            updated_blocks = []
            file_changed = False
            for block in blocks:
                lines = block.splitlines()
                arg_id = None
                for line in lines:
                    m = ARG_RE.match(line)
                    if m:
                        arg_id = m.group(1)
                        break
                if not arg_id or arg_id not in choices:
                    updated_blocks.append(block)
                    continue
                sigil_value = choices[arg_id]
                new_lines = []
                inserted = False
                replaced = False
                for line in lines:
                    if SIGILS_RE.match(line):
                        if overwrite:
                            new_lines.append(f"@sigils {sigil_value}")
                            replaced = True
                        else:
                            new_lines.append(line)
                        inserted = True
                    else:
                        new_lines.append(line)
                if not inserted:
                    out_lines = []
                    for line in new_lines:
                        out_lines.append(line)
                        if ARG_RE.match(line):
                            out_lines.append(f"@sigils {sigil_value}")
                            inserted = True
                    new_lines = out_lines
                if replaced or inserted:
                    file_changed = True
                updated_blocks.append("\n".join(new_lines))
            if file_changed:
                path.write_text("\n\n".join(updated_blocks).rstrip() + "\n")
                changed += 1
    return changed


def main() -> None:
    parser = argparse.ArgumentParser(description="Suggest sigils using GloVe + gloss vocab.")
    parser.add_argument("--glove", required=True, help="Path to GloVe vectors (e.g. glove.6B.50d.txt)")
    parser.add_argument("--report", default="data/sigils/glove_sigil_report.json",
                        help="Output report path (JSON).")
    parser.add_argument("--top", type=int, default=3, help="Top candidates per emoji/hanzi to report.")
    parser.add_argument("--apply", action="store_true",
                        help="Apply top-ranked sigils to flexiarg/multiarg files.")
    parser.add_argument("--overwrite", action="store_true",
                        help="Overwrite existing @sigils entries when applying.")
    args = parser.parse_args()

    patterns = parse_flexiargs() + parse_devmaps()
    tok_map = load_tokipona_map(Path("holes/tokipona.org"))
    tok_gloss = load_json_map(Path("data/tokipona_gloss.json"))
    hanzi_gloss = load_json_map(Path("data/hanzi_gloss.json"))

    tokens = collect_tokens(patterns, tok_gloss, hanzi_gloss)
    vectors = load_glove(Path(args.glove), tokens)

    emoji_vectors, hanzi_vectors = build_vocab_vectors(tok_map, tok_gloss, hanzi_gloss, vectors)

    report = []
    for pattern in patterns:
        tokens = tokenize(pattern.get("text", ""))
        vec = mean_vector(tokens, vectors)
        if not vec:
            continue
        vec = normalize(vec)
        emoji_hits = rank_similar(vec, emoji_vectors, args.top)
        hanzi_hits = rank_similar(vec, hanzi_vectors, args.top)
        pairs = []
        for emoji, es in emoji_hits:
            for hanzi, hs in hanzi_hits:
                pairs.append({
                    "emoji": emoji,
                    "hanzi": hanzi,
                    "score": round((es + hs) / 2.0, 6),
                })
        pairs.sort(key=lambda x: x["score"], reverse=True)
        report.append({
            "id": pattern["id"],
            "title": pattern["title"],
            "emoji": [{"emoji": e, "score": round(s, 6)} for e, s in emoji_hits],
            "hanzi": [{"hanzi": h, "score": round(s, 6)} for h, s in hanzi_hits],
            "pairs": pairs[: args.top],
        })

    out_path = Path(args.report)
    out_path.parent.mkdir(parents=True, exist_ok=True)
    out_path.write_text(json.dumps(report, indent=2, ensure_ascii=False))
    print(f"Wrote {len(report)} entries to {out_path}")

    if args.apply:
        changed = apply_sigils(report, args.overwrite)
        print(f"Updated {changed} files.")


if __name__ == "__main__":
    main()
