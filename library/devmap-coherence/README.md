# Devmap Coherence Checks

This directory hosts flexiformal patterns dedicated to keeping the FUTON devmaps self-consistent and structurally sound. The intent is to provide machine-checkable standards for devmap quality.

## Pattern Catalog

### IFR Bindings (per-futon coherence checks)

One pattern per futon that asserts whether the current state evidence satisfies the stated IFR:

| Pattern | Factor | Purpose |
|---------|--------|---------|
| `ifr-f0-sati` | Mindfulness (sati) | Verify vitality signals flow to telemetry |
| `ifr-f1-dhammavicaya` | Keen investigation | Verify graph memory seed + transport baseline |
| `ifr-f2-viriya` | Energy (viriya) | Verify pivot streams + graph adapters export data |
| `ifr-f3-piti` | Joy (pīti) | Verify check DSL + joy metrics for workday |
| `ifr-f3a-piti-audit` | Joy (pīti-audit) | Verify Portal/Sidecar/Compass + hub coordination |
| `ifr-f4-passaddhi` | Tranquility | Verify Arxana/GravPad integration |
| `ifr-f5-samadhi` | Concentration | Verify concentrated practice alignment |
| `ifr-f6-upekkha` | Equanimity | Verify vetted imports via F3 bridge |
| `ifr-f7-upa-upekkha` | Civic equanimity | Verify cross-futon artifacts for Hyperreal |

### Prototype Alignment (structural checks)

Parametrised patterns that check if a prototype has required structure:

| Pattern | Purpose |
|---------|---------|
| `prototype-alignment-tension` | Check for named tensions + readiness windows |
| `prototype-alignment-role` | Check for role reveal + stewardship declarations |
| `prototype-alignment-embedding` | Check patterns-index + compression log discipline |
| `prototype-alignment-bridge` | Check upstream reference + bridge artifact + downstream consumer |
| `prototype-maturity-lifecycle` | Define stub→greenfield→active→settled progression |
| `prototype-structure-checklist` | Define required fields for complete prototypes |

### Structural Discipline

| Pattern | Purpose |
|---------|---------|
| `baseline-freeze` | Freeze windows + audit hooks for baselines |
| `devmap-scope-discipline` | Keep layers scoped to own mechanisms |
| `ifr-state-convergence` | Compare IFR vs actual state |
| `next-steps-to-done` | Require explicit next steps in every clause |

## Maturity Lifecycle

Prototypes progress through four stages (defined in `prototype-maturity-lifecycle`):

1. **:stub** — idea captured, no implementation
2. **:greenfield** — implementation begun, not testable end-to-end
3. **:active** — runs end-to-end, actively used
4. **:settled** — battle-tested, stable, ready for dependents

Promotion requires evidence; demotion happens when evidence contradicts claimed level.

## Required Prototype Structure

Per `prototype-structure-checklist`, every devmap prototype should include:

```
! instantiated-by: Prototype N — Title [sigil1 sigil2]
  :maturity {:stub | :greenfield | :active | :settled}
  :depends-on [futonN/PM ...]
  :evidence-for-active [criteria]
  :evidence-for-settled [criteria]

  + context: Why this prototype exists
  + IF: The condition or need
  + HOWEVER: The obstacle or gap
  + THEN: What it does
  + BECAUSE: Why this approach

  + EVIDENCE:
    evidence[path/to/artifact]

  + NEXT-STEPS:
    next[Concrete action]

  :success-criteria
    pass[Observable success condition]
    fail[Observable failure condition]

  :psr-example "Why selected"
  :pur-template "Usage log template"
```

## Checking Workflow

1. Extract obligations from `holes/futon*.devmap` (IFR + prototypes)
2. Load devmap-coherence patterns into FUTON3 pattern store
3. For each obligation:
   - Run similarity search to shortlist candidate patterns
   - Evaluate with `check!` DSL; record status + derived actions
   - Emit proof trails (for FUTON4) and viriya deltas (for FUTON2)
   - Add `+ evidence:` lines to devmap clauses when satisfied
4. When no pattern fits, draft new coherence pattern

## Cross-References

All patterns in this directory carry `@references` to related patterns, creating a navigable graph. The IFR patterns form a chain (f0→f1→f2→f3→f3a→f4→f5→f6→f7) reflecting the stack's dependency structure.

## Related Library Folders

| Folder | Focus | Devmap touchpoints |
|--------|-------|-------------------|
| `mojo/` | Daily rhythm, micro-practices | FUTON3 joy metrics, FUTON5 concentration |
| `or/` | Obligation registry / governance | FUTON2 viriya exports, FUTON7 civic field |
| `p4ng/` | Patterns for next-generation practices | FUTON4 GravPad + FUTON5 samādhi |
| `repository-transition/` | Codebase evolution, graph hygiene | FUTON1 graph seed, FUTON3 baselines |
| `t4r/` | Training-for-Reason (flexiformal pedagogy) | FUTON6 Hyperreal on-ramp |
