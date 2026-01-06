# Hypertext Handover (Claude)

This note summarizes the Phase 5/6 hypertext demo work and recent QA runs.

## Current state
- Hypertext store + API + router/UI + schemas implemented in futon3.
- Phase 5 demo scripts exist and have been executed once with logs captured.
- Phase 6 MVP validation uses agent self‑validation + structural checks.
- Docbooks are resolved via futon1 API (docbook endpoints), not filesystem.

## Demo scripts
- `scripts/register_stack.clj` – register futon3 artifacts
- `scripts/extract_anchors.clj` – extract Clojure + Elisp anchors
- `scripts/suggest_links.clj` – devmap/docbook/org/code link suggestions
- `scripts/review_links.clj` – QA batch review (structural checks)

## Latest run logs (committed)
- `logs/hx-register.log`
- `logs/hx-anchors.log`
- `logs/hx-suggest.log`
- `logs/hx-review.log`
- `logs/hx-run-issues.md`

## Known limitations / gaps
- Org pattern link missing: `transition/prototype1a-workbench` not found in futon1 registry (aob.org).
- Code scan found one `:pattern/id` in `src/f2/ui.clj` with no registry match.
- Runtime artifacts `futon3/futon3/logs/hypertext.edn` and `hx-review-report.edn` are ignored (not committed).

## Notes on validation
- MVP validation is self‑validation (`futon3.hx.validate/self-validation`) with optional peer sign‑off via `peer-review`.
- Structural checks are enforced in `scripts/review_links.clj`.
- LLM validation is deferred; see Phase 6 plan in `docs/plans/hypertext-porcelain.md`.

## Where to look
- Plan + checklist: `docs/plans/hypertext-porcelain.md`
- Demo steps: `docs/hx-demo-run.md`
- Inventory/evidence: `futon4/docs/evidence/system-surface-inventory.edn`

