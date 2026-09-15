# buffer-cleaner — a real adapter for G-over-cascades

Working home (Joe's directive, 2026-09-15): the worked example stops
being synthetic arithmetic and becomes a REAL adapter — real Emacs
state in, parameterized nodes per the v3 diagram, real (dry-run) kill
receipts out — feeding codex-26's G computation. The post's numbers
will come from RUNNING this, not hand-set arithmetic.

## Layout

- adapter/dump-state.el — real state via emacsclient: per-buffer
  {name, kind (the cleaner's own predicates), file?, modified,
  process-gone, display-age-seconds}; JSON out. No killing.
- src/buffer_cleaner/classify.clj — PURE: packet + wiring -> per-candidate
  classification {decisiveness, prior-needed-later, revisit-cost,
  action kill|keep} + dry-run receipts. An :execute flag is REFUSED
  (live kill needs a gate that does not exist yet).
- nodes/*.flexiarg + nodes/*.params.edn — one node per v3 diagram
  chip; the params file is the diagram made executable (decisiveness
  class, prior over needed-later, revisit cost; source labeled
  :constructed or :derived+derivation).
- wirings/*.flexiarg + wirings/*.edn — aggressive/conservative as ARG
  CHOICES over the same node library; wiring = args, not code.
- params/categories.edn — the per-category parameter table, values
  traceable to the worked example (futon2 74c298f0) where possible,
  labeled construction otherwise.
- test/ — bb clojure.test for the pure parts (bb --classpath src:test -f test/run.clj).
- adapter/run.bb — end-to-end runner: real packet + both wirings -> receipts + meters in runs/.
- runs/ — receipts and packets (gitignored-large, receipts committed).

## Gates

clj-kondo on all Clojure; futon4/dev/check-parens.el on the .el;
bb test for classify; commits cited in the post.
