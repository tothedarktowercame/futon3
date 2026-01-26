# Futon3 ↔ Futon3a Integration (Stage 1 Decision)

Status: **Decided**  
Date: 2026-01-26

## Decision

Use **Option A: Bridge** — futon3 HUD calls futon3a portal via Drawbridge.

## Rationale

- Keeps futon3a’s embedding pipeline isolated and reusable.
- Avoids repo merge churn while contracts are still moving.
- Matches the current portal‑fetch workflow (futon3 consumes candidates; futon3a ranks).

## Interface Shape

- futon3 HUD requests: query + context → futon3a portal
- futon3a returns: ranked candidates + flexiarg fields + metadata
- futon3 logs PSR/PUR and execution outcomes; futon3a logs sidecar events

## Pattern Store Decision

**Single source of truth**: futon3 patterns.  
**Derived index**: futon3a embeddings rebuilt from futon3 patterns on demand or via a sync job.

Notes:
- futon3 keeps `resources/sigils/patterns-index.tsv` authoritative.
- futon3a stores embeddings under `resources/notions/`.
- Sync is explicit (no silent drift). Rebuilds are versioned and recorded.

## Next Actions (Stage 2+)

- Implement Drawbridge call in `src/futon3/fulab/hud.clj`.
- Add a `nearest-patterns` wrapper that delegates to futon3a and falls back to token matching.
- Add a sync command: rebuild futon3a embeddings from futon3 pattern index.
