#!/usr/bin/env python3
"""Retag invalid flexiarg/multiarg sigils using TF-IDF against Tokipona/Hanzi labels."""

import json
import math
import re
from collections import Counter, defaultdict
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
TOKI_FILE = ROOT / "holes" / "tokipona.org"
HANZI_FILE = ROOT / "holes" / "256ca.el"
PATTERN_INDEX = ROOT / "resources" / "sigils" / "patterns-index.tsv"
TOKI_GLOSS_FILE = ROOT / "resources" / "tokipona_gloss.json"
HANZI_GLOSS_FILE = ROOT / "resources" / "hanzi_gloss.json"

ROOTS = [ROOT / "library", ROOT / "holes" / "LDTS"]

ARG_RE = re.compile(r"^@arg\s+(\S+)\s*$")
FLEXIARG_RE = re.compile(r"^@flexiarg\s+(\S+)\s*$")
MULTIARG_RE = re.compile(r"^@multiarg\s+(\S+)\s*$")
SIGILS_RE = re.compile(r"^@sigils\s+\[(.*)\]\s*$")
TITLE_RE = re.compile(r"^@title\s+(.*)\s*$")
CLAUSE_RE = re.compile(r"^\s*[+!]\s+([^:]+):\s*(.*)$")
SIGIL_TOKEN_RE = re.compile(r"[^ \[\]]+/[^ \[\]]+")

STOPWORDS = {
    "the", "and", "of", "to", "a", "in", "for", "with", "on", "by", "is",
    "are", "as", "that", "this", "be", "an", "or", "it", "at", "from",
    "not", "have", "has", "but", "if", "you", "your", "we", "our", "their",
    "they", "them", "into", "about", "can", "will", "may", "must", "should",
}

TRUTH_NORMALIZE = {"門": "门", "義": "义"}


def tokenize(text: str):
    return [tok for tok in re.findall(r"[a-z0-9]+", text.lower()) if tok not in STOPWORDS]


def load_tokipona_base():
    mapping = {}
    if not TOKI_FILE.exists():
        return mapping
    for line in TOKI_FILE.read_text().splitlines():
        if not line.startswith("|") or line.startswith("|---"):
            continue
        cols = [col.strip() for col in line.split("|")][1:-1]
        if len(cols) < 4:
            continue
        word = cols[1].lower()
        meaning = cols[3].lower()
        shortcode = cols[2].strip("`").replace(":", " ")
        mapping[word] = f"{word} {meaning} {shortcode}"
    return mapping


def load_hanzi_base():
    mapping = {}
    if not HANZI_FILE.exists():
        return mapping
    for line in HANZI_FILE.read_text().splitlines():
        if ';' not in line:
            continue
        prefix, comment = line.split(';', 1)
        comment = comment.split('#', 1)[0].strip()
        matches = re.findall(r'"([^"]+)"', prefix)
        if len(matches) < 2:
            continue
        char = matches[1]
        mapping[char] = f"{char} {comment}" if comment else char
    return mapping


def load_json_map(path: Path):
    if path.exists():
        return json.loads(path.read_text())
    return {}


def build_docs():
    tok_base = load_tokipona_base()
    tok_gloss = load_json_map(TOKI_GLOSS_FILE)
    hanzi_base = load_hanzi_base()
    hanzi_gloss = load_json_map(HANZI_GLOSS_FILE)

    tok_docs = defaultdict(list)
    hanzi_docs = defaultdict(list)

    for word, text in tok_base.items():
        tok_docs[word].append(text)
    for word, text in tok_gloss.items():
        tok_docs[word].append(text)
    for char, text in hanzi_base.items():
        hanzi_docs[char].append(text)
    for char, text in hanzi_gloss.items():
        hanzi_docs[char].append(text)

    if PATTERN_INDEX.exists():
        for line in PATTERN_INDEX.read_text().splitlines():
            if not line or line.startswith("#"):
                continue
            cols = line.split("\t")
            if len(cols) < 5:
                continue
            _, tok_label, truth, rationale, hotwords = cols[:5]
            tok_label = tok_label or ""
            truth = truth or ""
            info = f"{rationale} {hotwords}"
            tok_clean = tok_label.split("(")[0].strip().lower()
            if tok_clean:
                tok_docs[tok_clean].append(info)
            truth_parts = truth.split()
            if truth_parts:
                truth_char = truth_parts[0]
                norm_truth = TRUTH_NORMALIZE.get(truth_char, truth_char)
                hanzi_docs[norm_truth].append(info)

    tokipona_docs = {label: " ".join(entries) for label, entries in tok_docs.items()}
    hanzi_docs = {label: " ".join(entries) for label, entries in hanzi_docs.items()}

    return tokipona_docs, hanzi_docs


def vectorize(all_docs):
    tokens_per_doc = [tokenize(text) for text in all_docs]
    df = Counter()
    for tokens in tokens_per_doc:
        for term in set(tokens):
            df[term] += 1
    total_docs = len(tokens_per_doc)
    idf = {term: math.log((1 + total_docs) / (1 + freq)) + 1.0 for term, freq in df.items()}

    def tfidf_vector(tokens):
        counts = Counter(tokens)
        total = sum(counts.values())
        if total == 0:
            return {}
        vec = {}
        norm = 0.0
        for term, count in counts.items():
            weight = (count / total) * idf.get(term, 0.0)
            if weight > 0.0:
                vec[term] = weight
                norm += weight * weight
        if not vec or norm == 0.0:
            return {}
        norm = math.sqrt(norm)
        for term in list(vec.keys()):
            vec[term] /= norm
        return vec

    return [tfidf_vector(tokens) for tokens in tokens_per_doc]


def cosine(vec_a, vec_b):
    if not vec_a or not vec_b:
        return 0.0
    if len(vec_a) > len(vec_b):
        vec_a, vec_b = vec_b, vec_a
    return sum(weight * vec_b.get(term, 0.0) for term, weight in vec_a.items())


def load_emoji_map():
    mapping = {}
    if not TOKI_FILE.exists():
        return mapping
    for line in TOKI_FILE.read_text().splitlines():
        if not line.startswith("|") or line.startswith("|---"):
            continue
        cols = [col.strip() for col in line.split("|")][1:-1]
        if len(cols) < 2:
            continue
        emoji, word = cols[0], cols[1].lower()
        mapping[word] = emoji
    return mapping


def load_emoji_allowlist():
    return set(load_emoji_map().values())


def load_hanzi_allowlist():
    if not HANZI_FILE.exists():
        return set()
    text = HANZI_FILE.read_text()
    return set(match.group(1) for match in re.finditer(r'\("[01]{8}"\s+"([^"]+)"\s+"#[0-9a-fA-F]{6}"\)', text))


def parse_sigils(text: str):
    pairs = []
    for token in SIGIL_TOKEN_RE.findall(text or ""):
        if "/" not in token:
            continue
        emoji, hanzi = token.split("/", 1)
        pairs.append((emoji.strip(), hanzi.strip()))
    return pairs


def sigils_valid(sigils: str, emoji_allow, hanzi_allow) -> bool:
    pairs = parse_sigils(sigils)
    if not pairs:
        return False
    return all(emoji in emoji_allow and hanzi in hanzi_allow for emoji, hanzi in pairs)


def emoji_lookup(label, emoji_map):
    for part in label.split():
        emoji = emoji_map.get(part)
        if emoji:
            return emoji
    return emoji_map.get(label)


def is_arg_line(line: str) -> bool:
    return bool(ARG_RE.match(line) or FLEXIARG_RE.match(line) or MULTIARG_RE.match(line))


def extract_block_text(block_lines):
    title = ""
    clauses = []
    for line in block_lines:
        mt = TITLE_RE.match(line)
        if mt:
            title = mt.group(1).strip()
        mc = CLAUSE_RE.match(line)
        if mc:
            clauses.append(mc.group(2).strip())
    return " ".join([title, *clauses]).strip()


def collect_blocks():
    blocks = []
    files = {}
    for root in ROOTS:
        if not root.exists():
            continue
        for path in root.rglob("*"):
            if not path.is_file() or path.suffix.lower() not in {".flexiarg", ".multiarg"}:
                continue
            lines = path.read_text().splitlines()
            files[path] = lines
            i = 0
            while i < len(lines):
                line = lines[i]
                if not is_arg_line(line):
                    i += 1
                    continue
                start = i
                block_lines = [line]
                i += 1
                while i < len(lines) and not is_arg_line(lines[i]):
                    block_lines.append(lines[i])
                    i += 1
                end = i

                arg_match = ARG_RE.match(block_lines[0]) or FLEXIARG_RE.match(block_lines[0]) or MULTIARG_RE.match(block_lines[0])
                arg_id = arg_match.group(1) if arg_match else None

                sigil_idx = None
                sigil_text = ""
                title_idx = None
                for idx, bline in enumerate(block_lines):
                    if title_idx is None and TITLE_RE.match(bline):
                        title_idx = idx
                    ms = SIGILS_RE.match(bline)
                    if ms:
                        sigil_idx = idx
                        sigil_text = ms.group(1).strip()

                blocks.append({
                    "path": path,
                    "start": start,
                    "end": end,
                    "lines": lines,
                    "arg_id": arg_id,
                    "block_lines": block_lines,
                    "sigil_idx": sigil_idx,
                    "sigil_text": sigil_text,
                    "title_idx": title_idx,
                    "text": extract_block_text(block_lines),
                    "existing_pairs": parse_sigils(sigil_text),
                })
    return files, blocks


def main():
    tok_docs, hanzi_docs = build_docs()
    files, blocks = collect_blocks()

    emoji_allow = load_emoji_allowlist()
    hanzi_allow = load_hanzi_allowlist()
    emoji_map = load_emoji_map()

    flex_docs = [block["text"] for block in blocks]
    all_docs = list(tok_docs.values()) + list(hanzi_docs.values()) + flex_docs
    vectors = vectorize(all_docs)
    tok_vectors = dict(zip(tok_docs.keys(), vectors[:len(tok_docs)]))
    hanzi_vectors = dict(zip(hanzi_docs.keys(), vectors[len(tok_docs):len(tok_docs) + len(hanzi_docs)]))
    flex_vectors = vectors[len(tok_docs) + len(hanzi_docs):]

    updates = defaultdict(int)

    for block, vec in zip(blocks, flex_vectors):
        current_valid = sigils_valid(block["sigil_text"], emoji_allow, hanzi_allow)
        if current_valid:
            continue
        if not vec:
            continue
        tok_scores = sorted(((label, cosine(vec, tv)) for label, tv in tok_vectors.items() if tv),
                            key=lambda x: x[1], reverse=True)
        hanzi_scores = sorted(((label, cosine(vec, hv)) for label, hv in hanzi_vectors.items() if hv),
                              key=lambda x: x[1], reverse=True)

        desired_pairs = len(block["existing_pairs"]) if block["existing_pairs"] else 1
        desired_pairs = max(1, min(2, desired_pairs))

        tok_choices = [label for label, score in tok_scores if score > 0][:desired_pairs]
        hanzi_choices = [label for label, score in hanzi_scores if score > 0][:desired_pairs]
        if not tok_choices or not hanzi_choices:
            continue

        pairs = []
        max_pairs = min(desired_pairs, len(tok_choices), len(hanzi_choices))
        for idx in range(max_pairs):
            emoji = emoji_lookup(tok_choices[idx], emoji_map)
            hanzi = TRUTH_NORMALIZE.get(hanzi_choices[idx], hanzi_choices[idx])
            if emoji and hanzi:
                pairs.append(f"{emoji}/{hanzi}")
        if not pairs:
            continue

        new_sigil_line = f"@sigils [{' '.join(pairs)}]"
        block_lines = list(block["block_lines"])
        if block["sigil_idx"] is not None:
            block_lines[block["sigil_idx"]] = new_sigil_line
        else:
            insert_at = (block["title_idx"] + 1) if block["title_idx"] is not None else 1
            block_lines.insert(insert_at, new_sigil_line)
        block["block_lines"] = block_lines
        updates[block["path"]] += 1

    for path, lines in files.items():
        updated = False
        output = []
        idx = 0
        file_blocks = [b for b in blocks if b["path"] == path]
        file_blocks.sort(key=lambda b: b["start"])
        for block in file_blocks:
            output.extend(lines[idx:block["start"]])
            output.extend(block["block_lines"])
            idx = block["end"]
            if block["block_lines"] != lines[block["start"]:block["end"]]:
                updated = True
        output.extend(lines[idx:])
        if updated:
            path.write_text("\n".join(output) + "\n")

    if updates:
        print("Updated flexiargs:")
        for path, count in sorted(updates.items()):
            print(f"  {path} ({count} blocks)")
    else:
        print("No updates applied (no invalid sigils or zero-score matches)")


if __name__ == "__main__":
    main()
