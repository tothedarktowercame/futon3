# Stack Coherence Patterns

This directory hosts flexiformal patterns for maintaining coherence across the entire FUTON stack. While `devmap-coherence/` focuses on individual devmap quality, these patterns address cross-cutting concerns: evidence trails, blocker detection, synchronization, and health checks.

## Pattern Catalog

### Evidence & Status

| Pattern | Purpose |
|---------|---------|
| `evidence-ledger` | Scan for concrete evidence backing "done" claims |
| `ready-blocked-triage` | Classify clauses as done/ready/blocked/drift |
| `maturity-evidence-audit` | Verify maturity tags match actual evidence |

### Dependency & Blocker Management

| Pattern | Purpose |
|---------|---------|
| `stack-blocker-detection` | Detect unmet cross-futon dependencies |
| `futon-bridge-health` | Verify cross-futon bridges are functional |

### Synchronization

| Pattern | Purpose |
|---------|---------|
| `readme-devmap-sync` | Diff README sections vs devmap clauses |
| `staleness-scan` | Flag docs that lag behind recent work |
| `commit-intent-alignment` | Correlate commits/PRs with devmap IDs |

### Layer-Specific Gates

| Pattern | Purpose |
|---------|---------|
| `futon1-determinism` | CI gate for futon1 storage/NLP determinism |

## Cross-Futon Bridge Inventory

Per `futon-bridge-health`, the stack has these declared bridges:

```
f0 ──vitality telemetry──→ f3a (morning review)
f1 ──graph memory──→ f2 (pivot context)
f1 ──graph memory──→ f3 (pattern store sync)
f2 ──pivot streams──→ f3 (proof trails)
f3 ──pattern canon──→ f3a (portal queries)
f3a ──audit logs──→ f0 (vitality dashboard)
f3 ──proof trails──→ f4 (Arxana nodes)
f4 ──GravPad──→ f5 (concentration practices)
f5 ──ledgers──→ f6 (Hyperreal entries)
f3 ──vetted patterns──→ f6 (argumentarium)
f2,f3,f5,f6 ──all artifacts──→ f7 (civic field)
```

## Triage Status Values

Per `ready-blocked-triage`, every devmap clause should have one of:

- **done** — complete with evidence
- **ready** — can proceed, no blockers
- **blocked** — waiting on specific dependency (must cite `blocked-by[...]`)
- **drift** — out of sync with reality (commits exist but clause says "planned")

## Relationship to devmap-coherence/

These two directories complement each other:

| Concern | devmap-coherence/ | stack-coherence/ |
|---------|-------------------|------------------|
| Scope | Single devmap quality | Cross-stack health |
| IFR checks | Per-futon IFR validation | N/A |
| Prototype structure | Required fields, maturity | Evidence validation |
| Dependencies | Bridge declaration | Bridge health verification |
| Synchronization | N/A | README↔devmap, commits↔clauses |

## Workflow Integration

1. **Weekly triage**: Run `ready-blocked-triage` + `staleness-scan` on all devmaps
2. **Release gates**: Run `futon1-determinism` + `futon-bridge-health` before releases
3. **Morning review**: Surface `maturity-evidence-audit` mismatches in futon3a digest
4. **Commit hooks**: Run `commit-intent-alignment` nightly to detect drift

## Cross-References

All patterns in this directory carry `@references` linking to related patterns in both `stack-coherence/` and `devmap-coherence/`, creating a navigable coherence graph.
