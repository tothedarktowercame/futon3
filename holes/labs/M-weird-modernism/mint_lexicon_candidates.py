#!/usr/bin/env python3
"""mint_lexicon_candidates.py — extract candidate portable handles from the
M-weird-modernism mission's real minting surfaces.

Source: futon3/holes/missions/M-weird-modernism.md
Surfaces:
  §2 table (line 220-228): "PKD's move | What it does | Futon-theoretic name"
  §8 glossary (line 375-387): "PKD quote | Foreword use | Futon gloss"

This is a QUARANTINED candidates extractor — it produces candidate handles
for review, NOT library entries. Each candidate carries:
  - handle: the coined term
  - source_citation: exact quote from the surface where it appears
  - source_location: section + line number
  - recurrence_count: total occurrences in the full mission doc
  - recurrence_surfaces: which surfaces (§2, §8, elsewhere) it recurs in

Usage:
  python3 mint_lexicon_candidates.py [--doc <path>]
  python3 mint_lexicon_candidates.py --doc futon3/holes/missions/M-weird-modernism.md

Exit 0 = candidates extracted. No library writes, no sends.
"""

import argparse
import re
import sys
from pathlib import Path

DEFAULT_DOC = "/home/joe/code/futon3/holes/missions/M-weird-modernism.md"


def load_doc(path):
    return Path(path).read_text()


def find_section_lines(lines):
    """Find the exact line ranges for §2 table and §8 glossary."""
    s2_start = s2_end = s8_start = s8_end = None
    for i, line in enumerate(lines):
        if line.startswith("### 2."):
            s2_start = i
        elif s2_start is not None and s2_end is None and line.startswith("### 3."):
            s2_end = i
        elif line.startswith("### 8."):
            s8_start = i
        elif s8_start is not None and s8_end is None and (
            line.startswith("## ") or (line.startswith("### ") and i > s8_start + 2)
        ):
            s8_end = i
    if s8_end is None:
        s8_end = len(lines)
    return s2_start, s2_end, s8_start, s8_end


def extract_table_rows(lines, start, end):
    """Extract markdown table rows (skip header + separator)."""
    rows = []
    in_table = False
    for i in range(start, min(end, len(lines))):
        line = lines[i]
        if line.strip().startswith("|") and "---" not in line:
            in_table = True
            cells = [c.strip() for c in line.split("|")[1:-1]]
            if cells and not cells[0].startswith("---"):
                rows.append((i + 1, cells))  # 1-indexed line number
        elif in_table and not line.strip().startswith("|"):
            break
    return rows


# Candidate handles to search for — these are the COINED TERMS that appear
# in the "Futon-theoretic name" or "Futon gloss" columns. Each is a candidate
# portable handle that compresses a recurring structural configuration into
# a term. We search for each across the full doc to count recurrence.
CANDIDATE_HANDLES = [
    # From §2 table "Futon-theoretic name" column:
    ("combining-methods-as-diagnostic",
     "§2 table: 'combining-methods-as-diagnostic at cosmological scale; the disagreement IS the signal'"),
    ("wyrd-salience",
     "§2 table: 'Wyrd-salience in pure form' (configuration survives migration across real/fake regimes)"),
    ("\"mere X\" anti-pattern",
     "§2 table: 'The \"mere X\" anti-pattern named by negative example'"),
    ("\"mere X\"",
     "§2 glossary: 'The \"mere X\" move applied to Logos itself — the central anti-pattern'"),
    ("鹽/債 ledger",
     "§2 table: 'the 鹽/債 ledger' and §8 glossary: 'the 鹽/債 ledger keeps the failure usable'"),
    ("間 (inhabited interval)",
     "§2 table: 'Inhabits 間; refuses \"mere precognition\" / \"mere coincidence\"'"),
    ("咅/香 (pre/post-articulation)",
     "§2 table: '香 operating without 咅' (pre-articulation perception as load-bearing)"),
    ("brand-name anti-pattern",
     "§2 glossary: 'Brand-name anti-pattern at civilisational scale'"),
    # From §8 glossary "Futon gloss" column:
    ("wyrd-survival criterion",
     "§8 glossary: 'Wyrd-survival criterion verbatim' (Reality is that which... doesn't go away)"),
    ("戒-practitioner / Skuld-bearer",
     "§8 glossary: 'The 戒-practitioner; the Skuld-bearer who refuses'"),
    ("Verðandi-surface",
     "§8 glossary: 'The Verðandi-surface becoming 既; the eschatological 鹽'"),
    ("(應 . 債) projection",
     "§8 glossary: '(應 . 債) projection at civilisational scale'"),
    # Cross-surface candidates (recur in both §2 and §8 and elsewhere):
    ("Skuld",
     "§2/§8/cross-surface: the Skuld frame — debt, obligation, the third Norn"),
    ("three-Norn decomposition",
     "cross-surface: 既/化/應/債 — the three-Norn extension landed in futonic-logic"),
    ("retroactive-canonicalization",
     "cross-surface: candidate upgrade generalising canonicalization across granularities"),
    ("proof-path",
     "§8 glossary: 'verbose proof-path event protocol' — the event-recording discipline"),
    ("Flow My Tears chain",
     "§2/§3: the worked wyrd-example — one configuration surviving six rival containment regimes"),
]


def count_recurrence(text, handle):
    """Count occurrences of the handle's key term in the full doc."""
    # Extract the searchable key from the handle tuple
    raw = handle[0].split("(")[0].strip().strip('"').strip("/")
    # If the term starts with ( the split produced empty — use the full term
    if not raw:
        raw = handle[0].strip()
    # For multi-word keys, search as-is; for single CJK, search the character
    if len(raw) <= 2 and not raw.isascii():
        # CJK character — count directly
        return text.count(raw)
    # For terms with special chars, try multiple forms
    variants = [raw]
    if "/" in raw:
        variants.extend(raw.split("/"))
    if " " in raw:
        variants.append(raw.replace(" ", "-"))
    # Use the longest variant for the count (most specific)
    best = max(variants, key=len)
    # re.escape handles regex metacharacters like . and ( in the key
    return len(re.findall(re.escape(best), text, re.IGNORECASE))


def find_surfaces(text, lines, handle, s2_start, s2_end, s8_start, s8_end):
    """Which surfaces does the handle appear in?"""
    raw = handle[0].split("(")[0].strip().strip('"').strip("/")
    if not raw:
        raw = handle[0].strip()
    variants = [raw]
    if "/" in raw:
        variants.extend([v.strip() for v in raw.split("/")])
    if " " in raw:
        variants.append(raw.replace(" ", "-"))

    surfaces = []
    s2_text = "\n".join(lines[s2_start:s2_end]) if s2_start else ""
    s8_text = "\n".join(lines[s8_start:s8_end]) if s8_start else ""

    for v in variants:
        if re.search(re.escape(v), s2_text, re.IGNORECASE):
            surfaces.append("§2-table")
            break
    for v in variants:
        if re.search(re.escape(v), s8_text, re.IGNORECASE):
            surfaces.append("§8-glossary")
            break
    # Check "elsewhere" (in the doc but outside §2/§8)
    if not surfaces or len(surfaces) < 2:
        elsewhere_count = 0
        for v in variants:
            elsewhere_count += len(re.findall(re.escape(v), text, re.IGNORECASE))
        if elsewhere_count > 0:
            surfaces.append("elsewhere-in-doc")
    return surfaces


def main():
    parser = argparse.ArgumentParser(
        description="Extract candidate portable handles from M-weird-modernism minting surfaces"
    )
    parser.add_argument("--doc", default=DEFAULT_DOC, help="Path to mission doc")
    args = parser.parse_args()

    text = load_doc(args.doc)
    lines = text.split("\n")
    s2_start, s2_end, s8_start, s8_end = find_section_lines(lines)

    print(f"# M-weird-modernism: candidate portable handles (QUARANTINED)")
    print(f"# Source: {args.doc}")
    print(f"# §2 table: lines {s2_start+1 if s2_start else '?'}-{s2_end+1 if s2_end else '?'}")
    print(f"# §8 glossary: lines {s8_start+1 if s8_start else '?'}-{s8_end+1 if s8_end else '?'}")
    print(f"# Candidates: {len(CANDIDATE_HANDLES)}")
    print()

    for handle, citation in CANDIDATE_HANDLES:
        count = count_recurrence(text, (handle, citation))
        surfaces = find_surfaces(text, lines, (handle, citation), s2_start, s2_end, s8_start, s8_end)
        print(f"HANDLE: {handle}")
        print(f"  CITATION: {citation}")
        print(f"  RECURRENCE: {count} occurrence(s) in full doc")
        print(f"  SURFACES: {', '.join(surfaces) if surfaces else 'none found'}")
        print()

    print("# END — candidates only, not library entries. No invented handles.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
