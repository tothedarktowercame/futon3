# library-contract worklist: REVIEW one row per invocation

You are the second reader for `futon3/holes/labs/library-contract/worklist.edn`.
Do exactly ONE review, then stop. You must not be the author of what you review:
the row's `:evidence` names the commit; if `git log -1 --format=%an <sha>` or
the row text says the work was yours, pick another row or stop and say so.

1. `cd ~/code/futon3 && git status --porcelain holes/labs/library-contract/ library checks test`
   must show nothing modified (untracked run artifacts are fine); if ledger or
   library files are modified, stop and report -- a read is against a commit.
2. Pick the FIRST row with `:status :done-unreviewed`.
3. Read the diff of every sha in its `:evidence` (`git show <sha>`). Re-run the
   verify step its `:acceptance` implies: the named tests under the C88 pin,
   `bb worklist_check.bb`, and for wave slices the section's
   `checks/library_graph_lint.clj` run. Check §5a discipline: no spider-authored
   `@why`, post-hoc edges use `@why-posthoc`, no similarity-inferred edges.
   Capture exit codes -- never pipe a gate into tail/grep and read the pipe's
   status.
4. EITHER set `:status :done :reviewed-by "<seat>" :reviewed-at "<UTC>"
   :review "<what you checked, with the numbers you reproduced>"` and
   `:covers-key :none` -- OR set `:status :open` with
   `:review-finding "<what is wrong, with a pointer>"` so the work loop repairs it.
5. Small findings you can fix in one edit: fix them, say so in `:review`.
6. `bb worklist_check.bb` must exit 0. Commit the ledger (and any fix) in futon3
   with the row id in the message. Report in three lines. Stop.
