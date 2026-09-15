# buffer-cleaner — flexiarg declarations (library entry)

LIBRARY CONTENT ONLY: this directory holds the pattern's parameters as
DATA — nodes/*.flexiarg + nodes/*.params.edn (one per v3 diagram chip),
wirings/*.flexiarg + wirings/*.edn (aggressive/conservative as ARG SETS
over the same nodes), params/categories.edn (the per-category value
table with :source labels — all :constructed; age histograms cannot
infer revisit rates).

The LIVE EVALUATION lives in the lab (Joe's ruling, 2026-09-15: the
library is for flexiargs only):
  futon2/holes/labs/wm-contract/buffer-cleaner-adapter/
    src/buffer_cleaner/classify.clj  — pure classifier (tests beside)
    adapter/dump-state.el            — real state via emacsclient (read-only)
    adapter/run.bb                   — end-to-end runner (reads THESE
                                       declarations; writes runs/ there)
Live kill remains refused until gated (typed refusal in execute!).

Values: from the worked example (futon2 a508aa24 / 74c298f0) where
possible; :temp is :uncertain at p=.05 (process-gone makes the kill
mechanism safe, not the future — H(.05)>0 is real); stream/http p=0
are FIXTURE PREMISES, not attestations.
