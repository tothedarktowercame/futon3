# Hypertext demo run issues

## Step 1: register-stack
- 2026-01-06: initial run failed to write `logs/hx-register.log` because `logs/` directory did not exist. Created `logs/` and re-ran successfully.

## Step 2: extract-anchors
- 2026-01-06: regex error in Elisp defun matcher (unclosed group). Fixed regex escaping in `scripts/extract_anchors.clj`.
- 2026-01-06: `ClassCastException` due to using `(swap! seen update ...)` result as a number. Introduced `next-count!` helper and updated counters in `scripts/extract_anchors.clj`.

## Step 3: suggest-links
- 2026-01-06: syntax errors from escaped quotes in `scripts/suggest_links.clj` strings/regexes; fixed literal quoting.
- 2026-01-06: org PATTERN links unresolved in initial run; pattern registry lookup likely missing/empty. Added missing-count tracking in suggest-links output.
- 2026-01-06: org PATTERN links now parsed; one missing pattern id from futon1 registry: `transition/prototype1a-workbench` (aob.org). Logged as scope limitation.
- 2026-01-06: code scan found one pattern id in `src/f2/ui.clj` with no matching registry entry (pattern-missing=1).
## Step 4: review-links
- 2026-01-06: namespaced map destructuring in `review_links.clj` caused `No such namespace: artifact`; replaced with explicit key lookup.
