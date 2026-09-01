# library-contract worklist: one item per invocation

You are working through `futon3/holes/labs/library-contract/worklist.edn`, the
item ledger of `futon2/holes/problems/P-organise-the-library.md` (read its §0-§2
and the WAVE 2 GATED entry) and `P-validated-R5.md`. The standard for the
directives is `futon3/README-flexiarg.md` §5a. Decisions already taken (and how
to record new ones) are in `decisions.edn` beside the ledger. Do exactly ONE
item, then stop.

1. Run `bb worklist_check.bb` in the ledger directory. If it fails, fix the
   ledger only if the failure is a formatting error you introduced; otherwise
   stop and report.
2. Do what the chosen row's `:acceptance` says -- nothing more. Every claim you
   record carries a `file:line` pointer, a ledger/attestation record id, or
   "not found".
3. HARD RULES from the standard: the spider/editor lane may write `@how` and
   `@see-also` and may only PROPOSE `@why` (decisions.edn
   :spider-editorial-standing). A post-hoc why uses the interim form
   `@why-posthoc` (decisions.edn :posthoc-why-syntax). Never author an `@why`
   causal claim for a pattern you did not write. Edges are authored or
   attested, never inferred from embedding similarity (P-validated-R5 law O2).
4. Gates before committing: clj-kondo and futon4/dev/check-parens.el on any
   Clojure you touched; the relevant tests (test/library_graph_lint_test.clj,
   test/spider_runner_test.clj) under the C88 evidence pin; for wave slices,
   checks/library_graph_lint.clj over the section you touched. Capture exit
   codes to a file and test them -- never read a gate's status through a pipe.
5. Commit in futon3 with a message naming the item id. Set the row
   `:done-unreviewed` with `:evidence` (sha + pointers); a
   `:loop-mode :one-slice-per-invocation` row instead appends to `:progress`
   and stays `:open` until its last slice. Commit the ledger. Run
   `bb worklist_check.bb` again.
6. Report in three lines: item id, what changed (sha), what the reviewer should
   check. Stop.
