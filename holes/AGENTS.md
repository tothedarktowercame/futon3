## Futon Devmap Proof Plan (FUTON3/FUTON2)

### Goal
Run routine flexiformal checks that show how the current standard library of patterns (living under `futon3/library/`) advances every FUTON devmap. Use FUTON3 as the check engine (pīti layer) and FUTON2 to translate results into viriya/energy metrics.

### Inputs
- Devmap corpus (`holes/futon[1-7].devmap`) with IFR + prototypes + bojjhaṅga metadata.
- Standard library patterns (`futon3/library/**/*.flexiarg` plus sigil embeddings in `resources/sigils`).
- Workday events / proof trails emitted through FUTON3 APIs.

### Checking Pipeline
1. **Obligation extraction**
   - Parse each devmap into `{:devmap :prototype :clauses}` records.
   - Attach IFR + bojjhaṅga metadata so checks can prefer supporting patterns (e.g., viriya patterns for FUTON2).
2. **Pattern lookup**
   - Load the standard library into the FUTON3 pattern store (Prototype 1).
   - Index by sigils, pāramitā, fruits/orbs so similarity queries run quickly.
3. **Applicability evaluation**
   - For each (devmap, prototype) clause, call the FUTON3 check DSL (`check!`) with context gathered from trails/logs.
   - Use the fake embedding (Prototype 2) to suggest top-N patterns; only run deep checks when similarity clears a threshold to manage cost.
4. **Result logging**
   - Record `:status` (applies / blocked / gap) plus derived obligations.
   - Export accepted proofs to FUTON1 (`proof->graph`) and viriya deltas to FUTON2 (`proof->energy`).
5. **Gap analysis & new pattern drafting**
   - When no existing pattern fits, funnel the evidence through the Pattern Creation Workbench (Prototype 6) to propose new “devmap coherence” patterns (e.g., `proto-ifr-binding`, `bojjhanga-alignment`).

### Efficiency Notes
- Batch checks per devmap and reuse similarity results to avoid recomputation.
- Cache embedding neighbourhoods and check outcomes so repeating the plan after small edits is incremental.
- Defer expensive trail replays: first run lightweight schema checks to ensure obligations reference known artefacts.

### Immediate Next Moves
1. Script the obligation extractor + check runner (likely under `scripts/`).
2. Populate the pattern store with the existing library and tag each pattern with the bojjhaṅga(s) it supports.
3. Define the first set of “devmap coherence” patterns (one per futon IFR) so missing coverage is explicit rather than implicit.
