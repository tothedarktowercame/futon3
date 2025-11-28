# AGENTS — Convolution Protocol for FUTON Proof Work

This note spells out how to act as a proof agent when moving between the
ledger (Org files), tactic state snapshots (PAR / PĀRAMITĀ), and the
flexiarg devmaps. Use it whenever you need to justify that a day’s work
actually advanced the ambitions.

## Stack Glossary

- **Ledgers**: `aob.org`, `orpm.org`. Treat them as accounting books —
  they list current commitments, WIP states, deadlines, and the emoji
  meta-language (🚀 ambitions, 🥨 salients, 🍐 obligations, etc.). They
  are *not* tactic states; they are the environment.
- **Devmaps**: `futon*.devmap`. Each `instantiated-by` block is a clause
  describing the obligations, indicators, and salients for a prototype or
  ambition. These clauses are the requirements / theorems we must satisfy.
- **Tactic State**: PAR/PĀRAMITĀ files (e.g., `PAR1`, `PARAMITA1`). These
  are execution traces. `PAR1` gives narrative context; `PARAMITA1`
  provides a vector-like table (obligations, pāramitās, convolution signals).
- **Memory Stack**: FUTON1 (deterministic trace) and FUTON4 (argument
  graphs) provide the history we can query like McCarthy’s
  ELEPHANT2000/Proof General.

## Symbol Rosetta

We can represent the same concept with multiple alphabets:

| Concept                     | Emoji | Hanzi | Toki Pona | Notes |
|-----------------------------|-------|-------|-----------|-------|
| Sleep baseline              | 💤     | 已    | lape      | Prototype 0 indicator |
| Tai chi / embodied practice | 🙌     | 弓    | musi?     | |
| Exercise guard              | 🏃     | 卫    | sijelo    | |
| Penmaster recordings        | 🎶     | 子    | kalama?   | |
| Work-session logs           | 📎     | 工    | lipu      | |
| Org–FUTON data pipeline     | 📁     | 上    | lipu lawa | |
| Dashboard review            | 📎     | 云    | musi ilo  | backups |
| Job-search trajectory       | 🔺     | 历    | lawa pi pali | |
| PARamita reflection         | 🌀     | 己    | pilin     | |
| Coaching / relational care  | 💟     | 女    | olin      | |
| Consulting comms            | 💬     | 口    | toki      | etc. |

Feel free to extend the table as we add clauses.

## Convolution Workflow

1. **Load the clauses** — Read the relevant `futon*.devmap` sections.
   Each `instantiated-by` block is a requirement. Prototype 8 of
   `futon0.devmap` (Stack Coverage) should flag any missing devmaps before
   you start.
2. **Review the current checklist (`JOE.md`)** — This file is the running
   list of unmet obligations from the previous pass. Snapshot it weekly
   (e.g., `JOE-2025w47.md`) so recurring items reset while the archive shows
   history.
3. **Load the tactic state** — Parse the latest `PARAMITA` (or PAR) entry
   and map narrative evidence onto the emoji/hanzi vector. Use the
   pāramitā table, convolution signals, and any explicit logs.
4. **Convolve** — Compare the tactic vector with the devmap clause vector:
   - If evidence ≥ requirement, mark the clause satisfied and tick the
     corresponding checkbox in `JOE.md`.
   - If evidence is partial, note the gaps (keep the checkbox but annotate
     context if needed).
   - If no evidence, leave the checkbox unchecked; it remains an active
     proof goal.
5. **Cross-check the ledgers** — Use `git status`/`git diff` (or similar) to
   inspect external updates to `aob.org` / `orpm.org`. Reference the
   relevant tasks when noting progress or gaps.
6. **Emit Proof Steps** — Summarise how the day advanced each ambition,
   citing devmap clauses and ledger/tactic evidence. Update `JOE.md` with
   any new obligations (e.g., Prototype 8 calling for missing devmaps).

## Example (FUTON0 ↔ PARAMITA1)

- Prototype 0 (Indicators): `🙌` missing (no tai chi), `🏃` low (exercise 1),
  `💤` strong (sleep 4), `🎶` absent, `📎` implicit (tool-testing). → mark
  sleep satisfied, exercise & recordings outstanding.
- Prototype 6 (Mojo Rhythm): `🌀` strong (life garden reflection), `💟`
  partial, `📇` absent. → reflection satisfied; schedule coaching evidence.

Unmet obligations (tai chi, recordings, job search) become the next tactics.

## Agents to Deploy

- **Convolution Agent** — Turns narratives into vectors and grades clauses.
- **Ledger Agent** — Reads Org trees to fetch supporting tasks and WIP status.
- **Memory Agent** — Queries FUTON1/FUTON4 traces when historical evidence is needed.
- **Planning Agent** — Suggests next proof steps so unmet obligations get
  scheduled.

Keep the mapping tables up to date as new devmaps and obligations appear.

## Standard Library & HAMMER

- Every flexiarg `! conclusion` now carries emoji/hanzi sigils. Treat these
  as the “HAMMER rack”: when a tactic state lights up a symbol (e.g., `[👫/女]`),
  scan the library for clauses with matching or nearby sigils to pull in the
  relevant technique.
- We can build a 256×256 association matrix using the original 8-bit encodings
  (initially Hamming distance, later refined semantically). This lets agents
  rank which library items are “close” to the current need.
- Missing devmaps (FUTON3/5/6/7) show up via Prototype 8; authoring them adds
  more tools to the rack.
