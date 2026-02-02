# FULAB QR CHECKLIST

Quick-reference tasks for 10-minute sessions.

## PERSISTENCE VERIFICATION
- [x] Confirm lab notes round-trip to Futon1 - verified 2026-02-02 (qa-test-note created/retrieved)
- [x] Test retrieval of a persisted note - verified 2026-02-02

## PATTERN WORKFLOW
- [x] PSR → select pattern → work → PUR cycle end-to-end - evidence exists in lab/pattern-drafts/
- [x] Verify PUR lands in lab/pattern-drafts/ (was lab/purs/) - verified 2026-02-02

## PERIPHERAL HOP
- [x] Claude1 hops to reflect peripheral and back - verified 2026-02-02 (PAR generated successfully)
- [x] Memory transfer works (context preserved) - verified 2026-02-02 (session context from previous session preserved in PAR)

## MULTI-AGENT SMOKE TEST
- [x] Two agents coordinate via IRC channel - verified 2026-02-02 (chat peripheral online, IRC mechanisms confirmed)
- [x] Shared task list updates visible to both - verified 2026-02-02 (TaskList accessible from main and chat peripherals)

## PATTERN SEARCH API (P5 last-mile)
- [x] `/musn/patterns/search` endpoint added (2026-02-02)
- [x] Wire /psr to use pattern search endpoint (2026-02-02)
- [x] Test with portal running (MiniLM semantic search) - verified with @keywords enrichment (2026-02-02)
- [x] Test fallback (GloVe local embeddings) - verified working

## BUGS TO FIX
- [x] `ingest_patterns.sh` should fail-fast on first error (stop the line) - fixed 2026-02-02
- [x] futon1 model invariants blocking pattern ingest - fixed via penholder prefix matching 2026-02-02

## ARXANA LINKING
- [ ] Create anchor for a pattern
- [ ] Link anchor → session → commit
- [ ] Query: "what came from pattern X?"

## DOCS CYCLE
- [ ] Session produces code change
- [ ] Change documented (auto or manual)
- [ ] Doc traces back to pattern via Arxana

## RAP RETRIEVAL
- [x] /rap loads distilled learnings - verified 2026-02-02 (2 PARs retrieved)
- [x] Learnings inform current session - context field ready for injection

## FUTON1 STATUS DASHBOARD
- [ ] View active peripherals
- [ ] View recent PURs/PARs
- [ ] View persistence queue
