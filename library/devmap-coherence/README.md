# Devmap Coherence Checks

This directory hosts flexiformal patterns dedicated to keeping the FUTON devmaps self-consistent. The intent is to reuse as much of the existing library as possible while adding lightweight glue where coverage is missing.

## Mapping the current canon

| Library folder           | Focus                                              | Likely devmap touchpoints                         |
|-------------------------|----------------------------------------------------|---------------------------------------------------|
| `mojo/`                 | Daily rhythm, micro-practices                      | FUTON3 joy metrics, FUTON5 concentration          |
| `or/`                   | Obligation registry / governance                    | FUTON2 viriya exports, FUTON7 civic field         |
| `p4ng/`                 | Patterns for next-generation practices              | FUTON4 GravPad + FUTON5 samādhi                    |
| `repository-transition/`| Codebase evolution, graph hygiene                    | FUTON1 graph seed, FUTON3 transport baselines     |
| `t4r/`                  | Training-for-Reason (flexiformal pedagogy)          | FUTON6 Hyperreal on-ramp                          |

Each existing pattern already carries sigils; once loaded into the FUTON3 pattern store they become candidates for `check!` runs.

## Planned coherence pattern families

1. **IFR bindings** – one pattern per futon that asserts whether the current state evidence satisfies the stated IFR (using bojjhaṅga cues).  e.g. `ifr-f1-dhammavicaya`, `ifr-f2-viriya`, …
2. **Prototype alignment** – parametrised pattern templates that check if a prototype has: context stated, obligation reference, export/hook to the neighbouring futon.
3. **Bojjhaṅga balance** – cross-layer checks ensuring the seven factors stay in proportion (e.g. FUTON2 energy feeding FUTON3 joy, FUTON5 concentration stabilising FUTON6 equanimity).

The first family will land under this folder as `ifr-*.flexiarg` files so we can iterate without touching the historical canon. Later families can follow the same convention.

## Abstract checking workflow (quick recap)

1. Extract obligations from `holes/futon*.devmap` (IFR + prototypes).
2. Load standard library patterns (existing + new devmap-coherence ones) into the FUTON3 pattern store with bojjhaṅga tags.
3. For each obligation:
   - run the similarity search to shortlist candidate patterns;
   - evaluate with the `check!` DSL; record status + derived actions;
   - emit proof trails (for FUTON4) and viriya deltas (for FUTON2) where applicable.
   - when an obligation is satisfied (or actively in progress), add an `+ evidence:` line to the devmap clause pointing at the specific namespaces/tests/README anchors; treat this as a standard checklist item so stack-coherence patterns can verify both intent and proof artifacts.
4. When no pattern fits, use the Pattern Creation Workbench to draft additional devmap-coherence flexiargs (and place them in this directory).

Next step: script the obligation extractor + batch checker (likely `scripts/check_devmap_coherence.clj`) so this plan becomes executable.
