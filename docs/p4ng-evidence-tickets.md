# P4NG Evidence Gathering Tickets

Paper deadline: ~1 week (Thu Jan 15)
Lab work cutoff: Mon Jan 12

## Ticket Overview

| ID | Assignee | Summary | Est |
|----|----------|---------|-----|
| E01 | fucodex | Pattern-guided refactor of aif_bridge.clj | 1h |
| E02 | fucodex | Implement one AIF pattern next-step | 2h |
| E03 | fuclaude | Analyze E01/E02 sessions, extract vignette | 1h |
| E04 | fuclaude | Validate PUR claims from E01/E02 | 30m |
| E05 | fubar.el+Joe | Live pattern-guided editing session | 1h |
| E06 | Joe | Review vignettes, select best for paper | 30m |
| E07 | fuclaude | Draft Section III-A (Interactional) from vignette | 1h |
| E08 | fucodex | Generate session reports for all evidence sessions | 30m |
| E09 | Joe | Final integration into main.tex | 1h |
| E10 | fuclaude | Verify paper claims against evidence trail | 30m |

---

## Detailed Tickets

### E01: Pattern-guided refactor of aif_bridge.clj
**Assignee:** fucodex
**Patterns:** `code-coherence/dead-code-hygiene`, `stack-coherence/commit-intent-alignment`
**Goal:** Demonstrate pattern selection in a real refactoring task

```bash
cd /home/joe/code/futon3
fucodex --live \
  --patterns "code-coherence/dead-code-hygiene,stack-coherence/commit-intent-alignment" \
  --aif-select \
  -p "Review aif_bridge.clj for dead code or unused helpers. Remove any found. Commit if tests pass."
```

**Success criteria:**
- [ ] PSR generated with pattern candidates
- [ ] PUR generated with outcome
- [ ] Session exported to lab/sessions/

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

### E03: Analyze sessions, extract vignette
**Assignee:** fuclaude
**Depends on:** E01, E02
**Goal:** Turn raw session data into paper-ready narrative

```bash
cd /home/joe/code/futon3
fuclaude -p "Read the session files in lab/sessions/ from today.
Extract one compelling vignette showing:
1. What pattern was selected and why (from PSR)
2. What the agent did (from events)
3. What the outcome was (from PUR)
4. PAR analysis: intention vs actual vs surprise
Format as a ~200 word narrative suitable for Section III-A of the paper."
```

**Success criteria:**
- [ ] Narrative written with specific PSR/PUR IDs cited
- [ ] PAR framing applied (intention/actual/surprise)

---

### E04: Validate PUR claims
**Assignee:** fuclaude
**Depends on:** E01, E02
**Goal:** Show verification pipeline working

```bash
cd /home/joe/code/futon3
# For each session:
./fulab-pattern-check --session lab/sessions/SESSION_ID.edn --verbose
```

**Success criteria:**
- [ ] Validation output captured
- [ ] Any failures explained or fixed
- [ ] Summary: "X of Y claims verified"

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

## Suggested Schedule

| Day | Tickets |
|-----|---------|
| Thu (today) | E01, E02 |
| Fri | E03, E04, E05 |
| Sat | E06, E07 |
| Sun | E08, E09 |
| Mon | E10, buffer |
| Tue-Thu | Paper polish, submission |

## Notes

- Session IDs will be UUIDs; update tickets with actual IDs as work proceeds
- If fucodex/fuclaude fail, fall back to manual runs with post-hoc export
- Keep lab/ committed so evidence is versioned
