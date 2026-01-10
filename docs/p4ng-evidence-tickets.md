# P4NG Evidence Gathering Tickets

Paper deadline: ~1 week (Thu Jan 15)
Lab work cutoff: Mon Jan 12

---

## Evidence Already Gathered

### Session 327e1ce7: Pattern-guided dead-code review (Jan 10)

**What happened:**
- HUD injected 4 pattern candidates including `code-coherence/dead-code-hygiene`
- Agent read pattern catalog, examined `pattern_hints.clj`, grepped for call sites
- Found 2 unused private functions: `read-lines` (line 57), `read-file` (line 65)
- Emitted FULAB-REPORT with `:applied "code-coherence/dead-code-hygiene"`

**Artifacts:**
- `lab/raw/327e1ce7-7fe5-47dd-93ae-6a1053cb2d0c.json` — tool uses, messages, timestamps
- `lab/trace/327e1ce7-7fe5-47dd-93ae-6a1053cb2d0c.org` — conversation trace
- `lab/aif/327e1ce7-7fe5-47dd-93ae-6a1053cb2d0c.edn` — AIF metadata

**Usable for:** E01 (pattern-guided refactor), E03 (vignette extraction)

---

## Infrastructure Status

| Component | Status | Notes |
|-----------|--------|-------|
| `fuclaude --hud` | ✓ Working | Injects pattern candidates into prompt |
| `fuclaude --allow-edits` | ✓ Working | Enables agent file edits |
| `fuclaude --resume/--continue` | ✓ Working | Session resumption |
| `lab-pattern-check.clj` | ✓ Exists | Needs `sessions/*.edn` (live mode only) |
| `lab-session-report.clj` | ✓ Exists | Needs `sessions/*.edn` (live mode only) |
| `fulab-pattern-check` wrapper | ✗ Missing | Trivial: 3-line bash script |
| Non-live → sessions export | ✗ Gap | Raw data exists but no events structure |

**Implication:** For full E04/E08 functionality, run sessions in `--live` mode. For paper vignettes, `raw/*.json` + `trace/*.org` suffices.

---

## Ticket Overview

| ID  | Assignee     | Summary                                            | Status |
|-----|--------------|----------------------------------------------------|--------|
| E01 | fucodex      | Pattern-guided refactor of aif_bridge.clj          | **Done** (327e1ce7) |
| E02 | fucodex      | Implement one AIF pattern next-step                | Optional |
| E03 | fuclaude     | Analyze sessions, extract vignette                 | Ready |
| E04 | fuclaude     | Validate PUR claims from E01/E02                   | Blocked (needs --live) |
| E05 | fubar.el+Joe | Live pattern-guided editing session                | Joe-blocks |
| E06 | Joe          | Review vignettes, select best for paper            | Pending E03 |
| E07 | fuclaude     | Draft Section III-A (Interactional) from vignette  | Pending E06 |
| E08 | fucodex      | Generate session reports for all evidence sessions | Blocked (needs --live) |
| E09 | Joe          | Final integration into main.tex                    | Pending E07 |
| E10 | fuclaude     | Verify paper claims against evidence trail         | Pending E09 |

**Critical path:** E03 → E06 → E07 → E09

---

## Detailed Tickets

### E01: Pattern-guided refactor ~~of aif_bridge.clj~~ ✓ DONE
**Assignee:** fucodex → **Completed by fuclaude session 327e1ce7**
**Patterns:** `code-coherence/dead-code-hygiene` (applied)
**Goal:** Demonstrate pattern selection in a real refactoring task

**What was run:**
```bash
./fuclaude --hud --intent "review pattern_hints.clj for dead code" \
  -p "List any unused private functions in src/futon3/pattern_hints.clj following the dead-code-hygiene pattern."
```

**Results:**
- HUD presented 4 candidates; agent selected `dead-code-hygiene`
- Found `read-lines` (line 57) and `read-file` (line 65) as unused
- Emitted FULAB-REPORT with applied pattern and rationale

**Success criteria:**
- [x] Pattern candidates presented (4 via HUD)
- [x] Pattern applied with reasoning
- [x] Session exported to lab/raw/, lab/trace/, lab/aif/

---

### E02: Implement one AIF pattern next-step
**Assignee:** fucodex
**Patterns:** `aif/belief-state-operational-hypotheses`, `aif/structured-observation-vector`
**Goal:** Show pattern next-steps driving actual implementation

```bash
cd /home/joe/code/futon3
fucodex --live \
  --patterns "aif/belief-state-operational-hypotheses,aif/structured-observation-vector" \
  --clock-in "aif/belief-state-operational-hypotheses" \
  -p "Implement the first next-step from aif/belief-state-operational-hypotheses: define :aif/belief-state schema in tatami_schema.clj"
```

**Success criteria:**
- [ ] Schema added to tatami_schema.clj
- [ ] Clock-in recorded linking pattern to task
- [ ] PUR captures implementation outcome

---

### E03: Analyze sessions, extract vignette — READY
**Assignee:** fuclaude
**Depends on:** E01 ✓
**Goal:** Turn raw session data into paper-ready narrative

**Updated approach** (works with non-live data):
```bash
cd /home/joe/code/futon3
fuclaude -p "Read lab/raw/327e1ce7-7fe5-47dd-93ae-6a1053cb2d0c.json and
lab/trace/327e1ce7-7fe5-47dd-93ae-6a1053cb2d0c.org.
Extract a compelling vignette showing:
1. What pattern candidates were presented (from HUD in user message)
2. What pattern the agent applied and why (from FULAB-REPORT)
3. What actions the agent took (from tool uses)
4. What the outcome was (dead code found)
5. PAR analysis: intention vs actual vs surprise
Format as a ~200 word narrative suitable for Section III-A of the paper."
```

**Success criteria:**
- [ ] Narrative references session 327e1ce7
- [ ] Shows pattern selection reasoning
- [ ] PAR framing applied (intention/actual/surprise)

---

### E04: Validate PUR claims — BLOCKED / OPTIONAL
**Assignee:** fuclaude
**Depends on:** E01, E02
**Goal:** Show verification pipeline working

**Blocker:** `lab-pattern-check.clj` needs `sessions/*.edn` which only `--live` mode creates. Session 327e1ce7 was non-live.

**Options:**
1. **Skip for paper** — manual inspection of FULAB-REPORT suffices
2. **Re-run E01 in --live mode** — creates full events structure
3. **Quick workaround** — parse FULAB-REPORT from raw JSON, verify pattern exists in catalog

**Manual verification for 327e1ce7:**
```bash
# Verify applied pattern exists
grep "dead-code-hygiene" resources/sigils/patterns-index.tsv
# Verify claimed dead code is actually unused
grep -r "read-lines\|read-file" src/futon3/pattern_hints.clj
```

**Success criteria:**
- [ ] Pattern ID verified in catalog
- [ ] Claimed findings spot-checked

---

### E05: Live pattern-guided editing (fubar.el)
**Assignee:** Joe + fubar.el
**Goal:** Experience the agent workflow firsthand, generate another evidence session

**Setup:**
```elisp
;; In Emacs with fubar.el loaded
(fubar-start-session
  :patterns '("contributing/devmap-contribution-protocol"
              "code-coherence/test-before-commit")
  :task "Add a test case for learn_or_act.clj decide function")
```

**During session:**
- Note which patterns feel applicable
- Observe when fubar suggests pattern switches
- Record surprises or friction points

**Success criteria:**
- [ ] Session trace captured
- [ ] Joe's subjective notes on workflow feel
- [ ] At least one PSR/PUR pair generated

---

### E06: Review vignettes, select best
**Assignee:** Joe
**Depends on:** E03, E05
**Goal:** Human judgment on what's paper-worthy

**Review criteria:**
- Does the vignette clearly show pattern → action → outcome?
- Is it self-contained enough for readers unfamiliar with futon?
- Does it support the paper's claims about pattern-guided agents?

**Success criteria:**
- [ ] 1-2 vignettes selected for paper
- [ ] Feedback on what to cut/expand

---

### E07: Draft Section III-A (Interactional)
**Assignee:** fuclaude
**Depends on:** E06
**Goal:** Turn selected vignette into paper prose

```bash
fuclaude -p "Using the selected vignette from E06, draft Section III-A
(Interactional layer) for the paper. Include:
- The solo workflow context
- How the pattern surfaced/was selected
- PAR/CLA diagnosis
- Lesson for refining interactional patterns
Target: 300-400 words, academic tone, cite specific evidence."
```

**Success criteria:**
- [ ] Draft ready for Joe's review
- [ ] Specific session IDs/pattern IDs referenced

---

### E08: Generate session reports
**Assignee:** fucodex
**Depends on:** E01, E02, E05
**Goal:** Structured summaries of all evidence sessions

```bash
cd /home/joe/code/futon3
for session in lab/sessions/*.edn; do
  clojure -Sdeps "$LAB_DEPS" -M dev/lab-session-report.clj --session "$session"
done
```

**Success criteria:**
- [ ] Reports generated for each session
- [ ] Summary table: sessions × patterns × outcomes

---

### E09: Final integration into main.tex
**Assignee:** Joe
**Depends on:** E07, E08
**Goal:** Put evidence into the paper

**Tasks:**
- Replace placeholder bullets in Section III with actual vignettes
- Add appendix with session summary table if useful
- Update Discussion to reference concrete evidence
- Ensure all cited session/pattern IDs are traceable

**Success criteria:**
- [ ] Section III has real content
- [ ] Discussion reflects actual findings
- [ ] No orphan references

---

### E10: Verify paper claims against evidence
**Assignee:** fuclaude
**Depends on:** E09
**Goal:** Final sanity check

```bash
fuclaude -p "Read main.tex. For each empirical claim (X sessions, Y patterns, etc.),
verify it against the evidence in lab/. Flag any unsupported claims or mismatches."
```

**Success criteria:**
- [ ] All claims verified or flagged
- [ ] Joe addresses any flags before submission

---

## Revised Schedule (as of Jan 10)

| Day | Tickets | Notes |
|-----|---------|-------|
| Fri Jan 10 | E01 ✓, E03 | E01 done; run E03 vignette extraction |
| Sat Jan 11 | E05, E06 | Joe's fubar session + vignette review |
| Sun Jan 12 | E07 | Draft Section III-A from vignette |
| Mon Jan 13 | E09, E10 | Integration + verification |
| Tue-Thu | Paper polish | Submission |

**Dropped/deferred:**
- E02 (AIF next-step implementation) — optional, only if time
- E04 (automated PUR validation) — manual spot-check suffices
- E08 (session reports) — nice-to-have for appendix

## Notes

- Session 327e1ce7 is the primary evidence session
- E05 (fubar.el live session) would add a second vignette if Joe has bandwidth
- Keep lab/ committed so evidence is versioned
- If fuclaude sessions fail, the raw/trace data is already sufficient for paper narrative
