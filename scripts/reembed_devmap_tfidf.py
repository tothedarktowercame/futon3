#!/usr/bin/env python3
"""Rewrite devmap sigils using a TF-IDF similarity against Tokipona/Hanzi labels."""

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
DEVMAP_DIR = ROOT / "holes"

DEV_MAP_GLOB = "futon*.devmap"
STOPWORDS = {
    "the", "and", "of", "to", "a", "in", "for", "with", "on", "by", "is",
    "are", "as", "that", "this", "be", "an", "or", "it", "at", "from",
    "not", "have", "has", "but", "if", "you", "your", "we", "our", "their",
    "they", "them", "into", "about", "can", "will", "may", "must", "should",
}
TRUTH_NORMALIZE = {"門": "门", "義": "义"}
SIGIL_TOKEN_RE = re.compile(r"[^ \[\]]+/[^ \[\]]+")

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


def load_hanzi_gloss():
    if HANZI_GLOSS_FILE.exists():
        return json.loads(HANZI_GLOSS_FILE.read_text())
    return {}

def load_tokipona_gloss():
    if TOKI_GLOSS_FILE.exists():
        return json.loads(TOKI_GLOSS_FILE.read_text())
    return {}


def build_tokipona_doc(label, base_map, gloss_map):
    parts = []
    for word in label.split():
        word = word.strip().lower()
        if not word:
            continue
        if word in gloss_map:
            parts.append(gloss_map[word])
        else:
            parts.append(base_map.get(word, word))
    if not parts:
        parts = [label]
    return ' '.join(parts)

def build_docs():
    tok_base = load_tokipona_base()
    tok_gloss = load_tokipona_gloss()
    hanzi_base = load_hanzi_base()
    hanzi_gloss = load_hanzi_gloss()

    tok_docs = defaultdict(list)
    hanzi_docs = defaultdict(list)

    # seed with base/gloss definitions
    for word, text in tok_base.items():
        tok_docs[word].append(text)
    for word, text in tok_gloss.items():
        tok_docs[word].append(text)
    for char, text in hanzi_base.items():
        hanzi_docs[char].append(text)
    for char, text in hanzi_gloss.items():
        hanzi_docs[char].append(text)

    pattern_entries = []
    if PATTERN_INDEX.exists():
        for line in PATTERN_INDEX.read_text().splitlines():
            if not line or line.startswith('#'):
                continue
            cols = line.split('\t')
            if len(cols) < 5:
                continue
            _, tok_label, truth, rationale, hotwords = cols[:5]
            tok_label = tok_label or ""
            truth = truth or ""
            info = f"{rationale} {hotwords}"
            tok_clean = tok_label.split('(')[0].strip().lower()
            if tok_clean:
                tok_docs[tok_clean].append(info)
            truth_parts = truth.split()
            if truth_parts:
                truth_char = truth_parts[0]
                norm_truth = TRUTH_NORMALIZE.get(truth_char, truth_char)
                hanzi_docs[norm_truth].append(info)

    tokipona_docs = {label: ' '.join(entries) if entries else build_tokipona_doc(label, tok_base, tok_gloss)
                     for label, entries in tok_docs.items()}
    hanzi_docs = {label: ' '.join(entries) if entries else hanzi_gloss.get(label, hanzi_base.get(label, label))
                  for label, entries in hanzi_docs.items()}

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

    vectors = [tfidf_vector(tokens) for tokens in tokens_per_doc]
    return vectors

def cosine(vec_a, vec_b):
    if not vec_a or not vec_b:
        return 0.0
    if len(vec_a) > len(vec_b):
        vec_a, vec_b = vec_b, vec_a
    return sum(weight * vec_b.get(term, 0.0) for term, weight in vec_a.items())

def load_devmap_blocks():
    info = []
    docs = []
    for path in sorted(DEVMAP_DIR.glob(DEV_MAP_GLOB)):
        lines = path.read_text().splitlines()
        i = 0
        while i < len(lines):
            line = lines[i]
            if line.startswith('! instantiated-by:'):
                start = i
                block = []
                while i < len(lines) and lines[i].strip():
                    block.append(lines[i])
                    i += 1
                docs.append('\n'.join(block))
                info.append({'path': path, 'line': start, 'lines': lines})
            else:
                i += 1
    return info, docs

def emoji_lookup(label, emoji_map):
    for part in label.split():
        emoji = emoji_map.get(part)
        if emoji:
            return emoji
    return emoji_map.get(label)

def load_emoji_map():
    mapping = {}
    for line in TOKI_FILE.read_text().splitlines():
        if not line.startswith('|') or line.startswith('|---'):
            continue
        cols = [col.strip() for col in line.split('|')][1:-1]
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

def main():
    tok_docs, hanzi_docs = build_docs()
    dev_infos, dev_docs = load_devmap_blocks()
    emoji_allow = load_emoji_allowlist()
    hanzi_allow = load_hanzi_allowlist()

    all_docs = list(tok_docs.values()) + list(hanzi_docs.values()) + dev_docs
    vectors = vectorize(all_docs)
    tok_vectors = dict(zip(tok_docs.keys(), vectors[:len(tok_docs)]))
    hanzi_vectors = dict(zip(hanzi_docs.keys(), vectors[len(tok_docs):len(tok_docs)+len(hanzi_docs)]))
    dev_vectors = vectors[len(tok_docs)+len(hanzi_docs):]

    emoji_map = load_emoji_map()
    updates = defaultdict(int)

    for dev_vec, info in zip(dev_vectors, dev_infos):
        tok_scores = sorted(((label, cosine(dev_vec, vec)) for label, vec in tok_vectors.items() if vec),
                            key=lambda x: x[1], reverse=True)
        hanzi_scores = sorted(((label, cosine(dev_vec, vec)) for label, vec in hanzi_vectors.items() if vec),
                              key=lambda x: x[1], reverse=True)

        tok_choices = [label for label, score in tok_scores if score > 0][:2]
        hanzi_choices = [label for label, score in hanzi_scores if score > 0][:2]

        if not tok_choices or not hanzi_choices:
            continue

        pairs = []
        max_pairs = min(len(tok_choices), len(hanzi_choices), 2)
        for idx in range(max_pairs):
            emoji = emoji_lookup(tok_choices[idx], emoji_map)
            hanzi = TRUTH_NORMALIZE.get(hanzi_choices[idx], hanzi_choices[idx])
            if emoji and hanzi:
                pairs.append(f"{emoji}/{hanzi}")
        if not pairs:
            continue

        line = info['lines'][info['line']]
        current = re.search(r'\[([^\]]*)\]', line)
        current_sigils = current.group(1) if current else ""
        if sigils_valid(current_sigils, emoji_allow, hanzi_allow):
            continue

        new_line = re.sub(r'\[[^\]]*\]', f"[{' '.join(pairs)}]", line, count=1)
        if new_line != line:
            info['lines'][info['line']] = new_line
            updates[info['path']] += 1

    for path, count in updates.items():
        path.write_text('\n'.join(next(info['lines'] for info in dev_infos if info['path'] == path)) + '\n')

    if updates:
        print("Updated devmaps:")
        for path, count in sorted(updates.items()):
            print(f"  {path} ({count} blocks)")
    else:
        print("No updates applied (TF-IDF produced zero-score matches)")

if __name__ == '__main__':
    main()
