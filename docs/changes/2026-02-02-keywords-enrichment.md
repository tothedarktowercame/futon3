# @keywords Enrichment for Pattern Search

**Date**: 2026-02-02
**Pattern Applied**: `pattern-coherence/evidence-alignment`
**Arxana Anchor**: `qa-arxana-test-2026-02-02:turn-1:artifact-2`

## Summary

Enriched 50 pattern files with `@keywords` metadata to improve semantic search quality via the MiniLM embedding pipeline.

## Changes

### Patterns Enriched

- 12 sidecar patterns (`library/sidecar/*.flexiarg`)
- 19 devmap-coherence patterns (`library/devmap-coherence/*.flexiarg`)
- 5 library-coherence patterns
- 3 pattern-coherence patterns
- Plus previously enriched: stack-coherence, code-coherence, contributing, ants namespaces

### Example

Before:
```
@flexiarg sidecar/tri-store-separation
@title Tri-Store Semantic Separation
```

After:
```
@flexiarg sidecar/tri-store-separation
@title Tri-Store Semantic Separation
@keywords facts, memes, notions, XTDB, SQLite, ANN, HNSW, trust, boundary, separation, store, layer
```

## Evidence

- Commit `fb18f4c`: Enrich 26 more coherence patterns with @keywords (50 total)
- Search query "maturity lifecycle stage" now returns `prototype-maturity-lifecycle` as #1 result (0.64 score)
- MiniLM embeddings regenerated and copied to portal

## Provenance

This change was guided by applying `pattern-coherence/evidence-alignment`:
- **Claim**: Patterns need keywords that map to search terms users would use
- **Mechanism**: @keywords field provides explicit term enrichment for embeddings
- **Confirming evidence**: Search quality improved after enrichment

Link: `link-d84dd590` (applies-pattern) connects the pattern anchor to the outcome anchor.
