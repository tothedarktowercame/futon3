#!/usr/bin/env python3
"""Export pattern embeddings using GloVe mean vectors."""
from __future__ import annotations

import argparse
import importlib.util
import json
import sys
from pathlib import Path
from typing import Dict, List


def _load_embed_module():
    module_path = Path(__file__).with_name("embed_patterns_glove.py")
    spec = importlib.util.spec_from_file_location("embed_patterns_glove", module_path)
    if spec is None or spec.loader is None:
        raise SystemExit(f"Cannot load {module_path}")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def export_embeddings(glove_path: Path, out_path: Path) -> None:
    mod = _load_embed_module()
    patterns = mod.parse_flexiargs() + mod.parse_devmaps()
    tokens = mod.collect_tokens(patterns)
    vectors = mod.load_glove(glove_path, tokens)

    embeds: Dict[str, List[float]] = {}
    skipped = 0
    for pattern in patterns:
        toks = mod.tokenize(pattern.get("text", ""))
        vec = mod.mean_vector(toks, vectors)
        if vec:
            embeds[pattern["id"]] = mod.normalize(vec)
        else:
            skipped += 1

    out_path.parent.mkdir(parents=True, exist_ok=True)
    out_path.write_text(json.dumps(embeds, indent=2))
    print(f"Wrote {len(embeds)} embeddings to {out_path}")
    if skipped:
        print(f"Skipped {skipped} patterns with no GloVe tokens")


def main(argv: List[str]) -> None:
    parser = argparse.ArgumentParser(description="Export pattern embeddings using GloVe.")
    parser.add_argument("--glove", required=True, help="Path to GloVe vectors (e.g. glove.6B.50d.txt)")
    parser.add_argument(
        "--out",
        default="futon3/data/glove_pattern_embeddings.json",
        help="Output JSON path for pattern-id -> vector",
    )
    args = parser.parse_args(argv)
    export_embeddings(Path(args.glove), Path(args.out))


if __name__ == "__main__":
    main(sys.argv[1:])
