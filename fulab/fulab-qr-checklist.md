# FULAB QR CHECKLIST

Quick-reference tasks for 10-minute sessions.

## PERSISTENCE VERIFICATION
- [ ] Confirm lab notes round-trip to Futon1
- [ ] Test retrieval of a persisted note

## PATTERN WORKFLOW
- [ ] PSR → select pattern → work → PUR cycle end-to-end
- [ ] Verify PUR lands in lab/purs/

## PERIPHERAL HOP
- [ ] Claude1 hops to reflect peripheral and back
- [ ] Memory transfer works (context preserved)

## MULTI-AGENT SMOKE TEST
- [ ] Two agents coordinate via IRC channel
- [ ] Shared task list updates visible to both

## PATTERN SEARCH API (P5 last-mile)
- [x] `/musn/patterns/search` endpoint added (2026-02-02)
- [x] Wire /psr to use pattern search endpoint (2026-02-02)
- [ ] Test with portal running (MiniLM semantic search)
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
- [ ] /rap loads distilled learnings
- [ ] Learnings inform current session

## FUTON1 STATUS DASHBOARD
- [ ] View active peripherals
- [ ] View recent PURs/PARs
- [ ] View persistence queue
