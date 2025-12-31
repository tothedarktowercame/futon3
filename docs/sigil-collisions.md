# Sigil Collision Log

`clojure -M:sigils` (2025-12-12) highlighted the following repeated `emoji/hanzi`
pairs. Each now exceeds the informal threshold enforced by
`library/library-coherence/pattern-differentiation-alarms`, so future devmap
updates should either split the clauses or reassign the pair.

| Pair | Count | Locations | Follow-up thoughts |
| --- | --- | --- | --- |
| `💤/已` | 4 | `holes/futon1.devmap`, `holes/futon2.devmap`, `holes/futon3.devmap`, `library/devmap-coherence/next-steps-to-done.flexiarg` | All four clauses talk about “Prototype done-ness.” Consider keeping `💤/已` for the Futon-level IFRs and assigning a new craft/ledger sigil (e.g. `📋/工`) to `next-steps-to-done` so stack readiness alerts stop overlapping with the layer narratives. |
| `👐/么` | 3 | `holes/futon1.devmap`, `holes/futon3.devmap`, `library/devmap-coherence/next-steps-to-done.flexiarg` | These describe “open blockers” rather than the same phenomenon. Give the Futon3 blocker triage clause a distinct pair (maybe `🪟/么` for HUD boundary) so the shared `next-steps` clause remains the only cross-layer blocker alarm. |
| `💖/弓` | 3 | `holes/futon0.devmap`, `holes/futon3.devmap`, `holes/futon7.devmap` | Prototype intents differ (embodied rhythm vs. Tatami HUD vs. civic salients). Keep `💖/弓` for Futon0’s tai-chi rhythm and rephrase the Futon3/Futon7 clauses to use joy-specific or civic sigils (e.g. `🧭/弓`, `🏛️/弓`). |
| `🙅/无` | 2 | `holes/futon1.devmap`, `holes/futon4.devmap` | Both call out “missing exports.” Decide whether Futon4’s archive guarantees should instead lean on a tooling/ledger sigil so that Futon1 retains `🙅/无` for “no session evidence.” |
| `💗/义` | 2 | `holes/futon6.devmap`, `holes/futon7.devmap` | Civic and Hyperreal care shouldn’t reuse the same balance glyph. Assign one to scholium care (F6) and the other to civic obligations (F7) to clarify future HUD groupings. |
| `💖/已` | 2 | `holes/futon1.devmap`, `holes/futon4.devmap` | This pair is already overloaded with `💖/弓`; give Futon4’s settlement clause its own completion sigil. |
| `👴/己` | 2 | `holes/futon5.devmap`, `holes/futon7.devmap` | Mentorship vs. elder civic stewards currently collide. Choose either `👴/己` for the archive caretaker motif or move Futon7 toward a civic/portal sigil before Prototype 1 freeze. |

Re-run `clojure -M:sigils` after renaming any clause so the generated
`resources/sigils/*.csv` stay in sync with this log.
