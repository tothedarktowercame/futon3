# `checks/` — derived behavioural witnesses over library edges

A flexiarg is **abstract** — a production rule at specification grain. Whether a
`@how` edge actually holds is **derived**, not stored: a relation run forward
against stated facts, in the third verification layer of
`futon2.aif.operational-witness` (*interface → structure → **behaviour***).

Run:

    clojure -Sdeps '{:paths ["checks"] :deps {org.clojure/core.logic {:mvn/version "1.0.1"}}}' \
      -M -m how-witness-snatch

**Every witness must ship with a mirror** — the same relation asked of a case
where it must *not* hold. A witness that cannot fail is not a witness.

| file | edge under test | result |
|---|---|---|
| `how_witness_snatch.clj` | `snatch/protect-the-unprotected-move` `@how` `snatch/preserve-the-right-to-abstain` | **ATTESTED**; mirror silent |
