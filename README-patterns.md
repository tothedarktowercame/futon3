# Pattern Library & Embeddings

This document covers the pattern library, sigil adjacency builder, embeddings CLI, and Futon1 ingestion.
For the overview and quickstart, see `README.md`.

## Sigil Adjacency Builder (emoji / hanzi)

A fake embedding is useful when Futon3 needs to "snap" a devmap clause onto the closest pattern in the standard library.

1. Annotate every new flexiarg conclusion (or equivalent top-level clause) with one or two `emoji/hanzi` sigil pairs inside brackets, e.g. `[home/work cycle/enter]`. Draw from the existing palette in `resources/sigils/index.edn` (or from the FUTON truth-table / Tokipona baselines) so the helper recognises them.

2. Save the flexiarg in `library/` (or a devmap under `holes/`) and make sure the sigils reflect its intent--these strings become the coordinates for the fake embedding.

3. Rebuild the matrices so the new sigils are part of the embedding space:

```bash
clojure -M:sigils
# scans holes/ and library/ for devmaps + flexiargs and writes resources/sigils/emoji-adjacency.csv, hanzi-adjacency.csv, and index.edn
```

The matrices are cheap to recompute, so rerun the script whenever you add or edit flexiargs/devmaps with fresh sigils.

4. When the same `[emoji/hanzi]` pair shows up repeatedly across devmaps, treat it as an alarm bell: inspect the overlapping clauses, decide whether to split/merge/refine the underlying patterns, and add/retire entries accordingly (see `library/library-coherence/pattern-differentiation-alarms`).
   The current collision list lives in `docs/sigil-collisions.md` so you can see
   which clauses are competing for the same pair before refreshing the HUD.

## Pattern Embeddings CLI (clusters, shells, tree)

The repo currently ships `resources/embeddings/glove_pattern_neighbors.json` (GloVe neighbor report) but not a raw pattern vector map. The CLI expects a file that maps `pattern-id -> [floats]`; point it at your embeddings with `--embeddings`. Generated embeddings live under `futon3/data/` (see the export script below). A tiny fixture lives at `dev/fixtures/pattern_embeddings.json`.

Dependencies: `numpy` for vector math, `scikit-learn` for agglomerative clustering (`hclust`). `hdbscan` is optional and only required if you pass `--method hdbscan`.

```bash
python -m pip install -r scripts/requirements-patterns.txt
```

Generate a GloVe embedding map for all patterns/devmaps:

```bash
python scripts/export_pattern_embeddings_glove.py \
  --glove data/glove/glove.6B.50d.txt \
  --out futon3/data/glove_pattern_embeddings.json
```

### Basic Commands

```bash
python scripts/patterns.py dry-run --embeddings dev/fixtures/pattern_embeddings.json
python scripts/patterns.py cluster --k 2 --embeddings dev/fixtures/pattern_embeddings.json --out dev/fixtures/pattern_clusters.json --include-scores
python scripts/patterns.py shell --cluster 0 --embeddings dev/fixtures/pattern_embeddings.json --clusters dev/fixtures/pattern_clusters.json --ring-size 2 --with-scores
python scripts/patterns.py tree --k 2 --max-depth 2 --embeddings dev/fixtures/pattern_embeddings.json --out dev/fixtures/pattern_tree.json
```

### Full Library Run (GloVe, ~231 patterns)

```bash
python3 -m venv .venv
. .venv/bin/activate
python -m pip install -r scripts/requirements-patterns.txt

python scripts/export_pattern_embeddings_glove.py \
  --glove data/glove/glove.6B.50d.txt \
  --out futon3/data/glove_pattern_embeddings.json

python scripts/patterns.py dry-run --embeddings futon3/data/glove_pattern_embeddings.json
python scripts/patterns.py cluster --k 32 --linkage complete \
  --embeddings futon3/data/glove_pattern_embeddings.json \
  --out futon3/data/pattern_clusters.json --include-scores
python scripts/patterns.py shell --cluster 6 \
  --embeddings futon3/data/glove_pattern_embeddings.json \
  --clusters futon3/data/pattern_clusters.json \
  --ring-size 6 --max-rings 2 --with-scores
python scripts/patterns.py tree --k 4 --max-depth 3 --min-size 6 \
  --embeddings futon3/data/glove_pattern_embeddings.json \
  --out futon3/data/pattern_tree.json
```

### Example Output

Cluster output (structure; cluster ids may differ by dataset):

```json
{
  "0": {
    "center": "beta",
    "members": ["beta", "alpha", "gamma"],
    "scores": [1.0, 0.994, 0.991]
  },
  "1": {
    "center": "delta",
    "members": ["delta", "epsilon", "zeta"],
    "scores": [1.0, 0.994, 0.97]
  }
}
```

Shell output:

```text
cluster 0 center beta
ring 0: beta(1.000) alpha(0.994)
ring 1: gamma(0.991)
```

## Ingesting New Patterns into Futon1

When you add or update flexiargs in `library/`, rebuild the sigil index and ingest into Futon1:

```bash
clojure -M -m scripts.build-pattern-index
clojure -M -m scripts.build_sigil_matrices

# Ingest into Futon1 (API must be running).
FUTON1_API=http://localhost:8080 ./scripts/ingest_patterns.sh
```

Notes:
- `scripts/ingest_patterns.sh` writes an EDN hash cache at `data/pattern-ingest-cache.edn` to skip unchanged rows on subsequent runs.
- Set `PATTERN_INGEST_CACHE` to override the cache path.
