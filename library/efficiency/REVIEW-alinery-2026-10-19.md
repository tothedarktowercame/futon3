# Review: Alinery read through the efficiency set (claude-1 for claude-5, 2026-10-19)

Commissioned by claude-5 (job `invoke-1792418672170-17430-e418ec1a`) for Joe's question: what
problem does Alinery solve, read through the twelve proposed patterns in this directory, and is its
Task / Playbook / Step / Artifact shape the futon mission lifecycle under other names?

## What was read, and how far

Citations are `file:line`. Abbreviations:

- **README, VISION, DESIGN, AGENTS**: `~/code/external/alinery/<name>.md`, at Alinery commit `cfc0d1e`
  ("bump Alinery to 0.22.2"), the same commit the minus40 note read.
- **GUIDE**: `docs/playbook-design-guidance.md` in that clone.
- **SD**: `alinery-app/src-tauri/playbooks/superdevelop/playbook.md`.
- **AS**: `.../playbooks/academic-survey/playbook.md`. Not on the packet's list. Added because SD has no
  `each`/`complete` input and AS is where a planning step emits a family of units for parallel workers,
  which question C turns on.
- **one-shot**, **review**: `.../playbooks/one-shot/playbook.md`, `.../playbooks/review/playbook.md`.
  The packet said "one-shot or code-review"; there is no `code-review` directory. The code-review
  playbook is `review` (title "Evidence-Backed Code Review"). One-shot was read in full, `review` for its
  step table and the lines grepped for independence.
- **LIFECYCLE**: `~/code/futon4/holes/mission-lifecycle.md`.
- **MINUS40-NOTE**: `~/code/minus40/docs/NOTE-2026-10-15-spec-gaps-and-contrast.md`.
- The twelve flexiargs at futon3 `391e673` and `PROPOSAL-2026-10-19.md`: conclusion, IF/HOWEVER/THEN and
  both evidence lines read for each.

Read in full: README, VISION, GUIDE, SD, one-shot, LIFECYCLE (phase sections and conventions), AS
(step table and the plan-search, gather, prepare-readings, evaluate-source prompts). Read in part:
DESIGN (614 lines, a UI design system; searched for fan-out, parallel, worker, merge, loop and found
only line 79 and 205, which say commenting on Markdown plans is the primary loop), AGENTS (209 lines,
engineering conventions; lines 60-70 and a grep for review/parallel), `playbook-sharing-contract.md`
(first 30 lines; it is the HTTP contract for the public playbook library and says nothing about
decomposition), `review` and `systematic-evidence-review` (step tables and greps). Nothing in the
Alinery clone was run, built or modified.

## A. The twelve patterns

Verdicts: yes / partial / no / out of scope. Counts at the end of the table.

| # | pattern | Alinery has it? | where | how it differs |
|---|---|---|---|---|
| 1 | split-by-shared-dependency | partial | SD:259 "Give each change one clear owner"; SD:260 "Divide work into independently verifiable deliverables ... Split where a reviewer could meaningfully accept one result and reject another" | Alinery cuts a plan at the boundary a reviewer can accept or reject. The pattern cuts at the facts several conclusions would each have to establish. Alinery names no shared-fact unit that goes out first. One owner per change does prevent two units editing the same thing. |
| 2 | give-the-dependent-piece-its-hypothesis | out of scope | GUIDE:121 (a session "can start once its required files are accepted, their producing sessions have exited"); GUIDE:333 (no second engine-started coding execution in the worktree) | Code units run one at a time, so unit B always has A's accepted result. Fan-out workers are independent by construction: each processes "only its assigned member" (SD:102) and cross-product joins are not allowed (GUIDE:94 "Do not invent cross-product joins"). Where one step depends on another, Alinery makes it wait for the accepted artifact, which is the opposite of the pattern, and costs nothing because nothing is meant to overlap. |
| 3 | seat-sized-units | partial | AS:183 "One source means one engine-managed agent session. Never batch multiple independent sources into a reading request"; SD:260; SD:68-73 (Build is one step, one session) | In the research playbooks the unit is one source per session, which is the pattern's rule in a non-coding domain. For code, the plan is cut into deliverables but Build is one session over all of them (SD:352), so no size bound on a unit is ever tested. Also the premise differs: Alinery sessions are interactive with a human gate (GUIDE:13), so a seat is not "one signal at the end". |
| 4 | every-leaf-states-what-the-assembly-consumes | partial | SD:104 and GUIDE:234 "never fabricate a successful handoff", "Never invent a study, selected fix, shortlist or continuation merely to leave evidence"; SD:309 "Mark them as specified but not demonstrated"; SD:261 each deliverable gets "required inputs, resulting interface or behavior" | The rule against manufactured values is present and stated as plainly as in Joe's 3bc41ccc1 ruling; Define Tests separates demonstrated red from specified-only (SD:324). What is missing is the per-leaf check "this is what the assembly consumes from it": SD:271-272 ask for interfaces between deliverables and a mapping from acceptance conditions to verification, as prose in one file. |
| 5 | the-assembly-does-not-move | partial | SD:257 "Verify which version is approved"; SD:258 "surface that change instead of burying a redesign in the plan"; SD:352 "ask the user when the design or scope must change"; GUIDE:71-75 (a task keeps its playbook definition fixed); SD:98 (do not overwrite the original ticket or another execution's outputs) | The approved design plays the registered statement's part and is protected by the human who approved it. There is no mechanical fingerprint: GUIDE:303 says "Do not add content-hash approval/revocation machinery". Unit ids are not stable in the pattern's sense either; artifact numbers are assigned by the engine before launch (GUIDE:262, 286) and existing artifacts are never renamed (GUIDE:286). |
| 6 | review-the-decomposition-before-dispatch | partial | SD:40 (Decide is human-gated); SD:51 (Plan is not); SD:84 (Prepare Review is gated); SD:263 "Review the plan against every acceptance condition"; SD:356 "If an independent reviewer is available and authorized ... otherwise label your review as self-review" | See D. The human pause is before the decomposition, on the design. The decomposition is checked by its own author-session and then built; independent review is optional and must be labelled. |
| 7 | cover-every-row-or-say-why-not | yes | AS:177 "Account for every supplied search, including empty results and access failures"; AS:183 and AS:185 (a roster, with a reason per candidate: new, duplicate, outside, unresolved); GUIDE:127 "A merge cannot silently omit a running, paused or failed worker"; SD:263, SD:272 (every acceptance condition has a verification) | Present in two forms: a ledger written by the planning session (AS), and engine enforcement that a merge waits for every expected worker (GUIDE:127), which the pattern leaves to the reviewer. The engine form covers workers, not source rows: a source row that nobody made a request for is still found by a person. |
| 8 | order-by-what-it-unlocks | no | SD:352 "in dependency order" is the only ordering; GUIDE:61 (extra ready work "remains queued") | Order is topological within a task, and queue order under the live-session cap. Nothing ranks pieces by how many consumers each closes. Across tasks, Grid/Kanban has "manual lane order" (README:111-113); ranking is left to the person. |
| 9 | name-the-piece-that-is-not-one-turn | partial | SD:121 "If the request contains several independently deliverable systems, expose that scope and seek a sensible first deliverable before planning the whole platform"; SD:273 "explicit stop conditions"; README:144-148 (sub-tasks) | The oversized piece is surfaced at Clarify and can become a sub-task with its own worktree, playbook and artifacts. There is no "cannot be one turn" section naming each oversized ask, and only one sub-task can be active at a time (README:148), so the split runs serially. |
| 10 | search-before-declaring-a-foundation-missing | yes | SD:164 (Investigate step 3) "Look for existing implementations and patterns that can satisfy the request. Distinguish reusable behavior from code that only looks similar"; SD:213 "Reuse existing mechanisms before introducing new ones"; SD:178 "Exact checks performed and their results; separately list anything not verified" | Discovery is its own step with a written reading result, ahead of the step that chooses a design. Same ordering as the pattern. The pattern asks for found-at-path or not-found-after-these-searches; SD:178 and SD:179 ask for the checks and the unverified remainder, which is the same record. |
| 11 | split-the-partial-not-retry-it | partial | SD:358 "keep the working tree and partial evidence intact"; GUIDE:337 "No automatic retry, attempts or `on_failure` policy"; GUIDE:203 the loop step "writes a fresh assigned actionable ticket containing the remaining work"; AS:140 a continuation targets "new citations, search gaps ... do not repeat the entire initial search" | The first half (bank what was done, do not rerun the whole) is present in loops and in AS continuations. The second half, the reviewer resizing the remainder into new seat-sized units, is not: recovery is a person creating a session (GUIDE:339). |
| 12 | a-false-leaf-goes-back-to-the-statement | partial | SD:258; SD:354 "change a test expectation only when evidence shows the test contradicted the approved requirement"; SD:352 | A plan or test that contradicts the approved design goes back to the human, not into a quiet edit. The grade comes from the same human who approved the design, not from someone outside the queue, and no separate record of the repair is required. |

**Counts: yes 2 (rows 7, 10); partial 8 (rows 1, 3, 4, 5, 6, 9, 11, 12); no 1 (row 8); out of scope 1 (row 2).**

## B. Alinery against the mission lifecycle

Word each side uses is given first. Where a row has no counterpart on one side, it says so.

| Alinery | Mission lifecycle | Alinery cite | Lifecycle cite | Match |
|---|---|---|---|---|
| **Task**: "One place for the work, its agent sessions, decisions, and artifacts" | **Mission**: "a scoped unit of work that moves the stack from a known state to a better one" | README:53 | LIFECYCLE:3-4 | Same role: the organising unit. A Task has one fixed playbook (GUIDE:28); a mission has no process object, its process is the lifecycle. |
| **Playbook**: "A reusable process with defined Steps and review points"; typed inputs and outputs, modes `single`/`each`/`complete` | **The lifecycle** itself: "a reusable reference — not specific to any mission" | README:54; GUIDE:91-92 | LIFECYCLE:9 | Alinery has many (14 bundled) and they are editable and executable. The futon side has one, fixed at seven phases plus optional HEAD (LIFECYCLE:3-5), and it is a checklist, not an executable graph. |
| **Step** (SD: Clarify, Investigate, Decide, Plan, Define Tests, Build, Prepare Review) with "Ready when" | **Phase** (HEAD, IDENTIFY, MAP, DERIVE, ARGUE, VERIFY, INSTANTIATE, DOCUMENT) with "Exit criterion" | SD:110-135 and following | LIFECYCLE:50-52, 68 | Rough pairing: Clarify with IDENTIFY, Investigate with MAP (LIFECYCLE:135 "Survey what exists. Don't design yet"; SD:158 "Answer the investigation questions using evidence"), Decide with DERIVE plus ARGUE, Build with INSTANTIATE. **No Alinery counterpart** for HEAD, for ARGUE's pattern cross-reference and plain-language argument (LIFECYCLE:194-231), or DOCUMENT's docbook entries (LIFECYCLE:308-333). **No lifecycle counterpart** for Plan (the lifecycle expects DERIVE to be enough to implement from, LIFECYCLE:191) or Define Tests as a separate step (nearest: VERIFY's completion-criteria pre-check, LIFECYCLE:254). |
| **Session**: "An agent or terminal session; a task can have many" | none | README:55; GUIDE:30-31 | LIFECYCLE says nothing about who or what runs a phase | **No lifecycle counterpart.** The lifecycle is silent on seats; this is the gap the efficiency set works in. |
| **Artifact**: "A numbered Markdown output to inspect, comment on, and use in later Steps"; one or more files per step, bound by the engine to later steps | **Phase section** in one mission file; "Each phase accretes", "Checkpoints are appended" | README:56; SD:47, 58, 69, 80 | LIFECYCLE:354, 388, 391 | Alinery: many files, each a typed input to named steps. Lifecycle: one growing file, read by whoever opens it. The lifecycle's exit verdict line (LIFECYCLE:373-387) has no file of its own. |
| **Worktree**: "An optional isolated Git checkout and branch for a task"; one per task, one per sub-task | none | README:57, 148; GUIDE:28 | LIFECYCLE:354 places the mission file in `<repo>/holes/missions/` and says nothing of branches | **No lifecycle counterpart.** Futon handles it outside the lifecycle (worktrees under `~/worktrees/`, one JVM per repo, in the workspace CLAUDE.md). |
| **Review point**: a step with `auto_advance_default = false`; "Human-gated steps require permission to complete" | **Exit criterion** met and recorded as `**<PHASE> exit: Met.**`; `**Gate:** operator-*` | SD:40, 84; SD:104; GUIDE:350 "Put real human review behind the completion gate, not prompt-only 'ask first' wording" | LIFECYCLE:68, 361-372, 373-387 | Both give the human a point agents cannot clear for themselves (LIFECYCLE:371-372 "Agents may propose a gate ... but may not clear one"). Alinery enforces it in the engine; futon writes it in the document and the War Machine observes it. Alinery gates at Decide and Prepare Review; the lifecycle gates every phase's exit, in principle. |
| **Sub-task** (one active child, own worktree and playbook) | **Relationship to other missions**; BLOCKED / DEFERRED states | README:144-148 | LIFECYCLE:59, 347-349 | Alinery's child returns an artifact or merged code to its parent (README:165-171). A mission names a dependency and waits. |
| **Grid / Kanban**: tasks grouped by current Playbook Step | **Mission Control** tracks status and coverage; **portfolio inference** picks the next mission | README:58, 111 | LIFECYCLE:604-605, 611 | Same job (see where everything is); Alinery's is a view the user arranges, Mission Control computes tensions. |

Reading of Joe's hypothesis, from the rows above. For the sequential spine (one scoped unit of work, staged,
human exit points, artifacts that accumulate, and a record a returning person can read) it holds: the
minus40 note already reached the same place from the other side, "a task that holds sessions, decisions
and artifacts, so a return visit shows 'what was attempted, what was learned, which decisions still
matter'" (MINUS40-NOTE:114-117, citing VISION:57). It fails in two places. The lifecycle has no execution
semantics (no session, no scheduler, no inputs bound to outputs, no parallelism), and Alinery has no
ARGUE, no pattern grounding (PSR/PUR) and no HEAD. A Playbook is closer to "the lifecycle, made
executable and made plural" than to a mission; a Task is a mission with the playbook already chosen.

## C. Can the Plan step emit a decomposition of the kind the efficiency set reviews?

**Answer: in the bundled code playbook, no. Plan emits one prose file for one next session, and Build runs
the whole thing serially in one worktree. The engine can run parallel units, but only for non-coding
work, and the planner there declares no dependencies between units, because the units are independent
by construction.**

SuperDevelop. The Plan step has one exact input set and one output, `plan.md` (SD:46-47). Its purpose line
is "Convert the approved design into an implementation plan that another Session can execute without
reconstructing the reasoning or guessing the intended interfaces" (SD:253), singular. The deliverable
is "Ordered deliverables with concrete changes and verification for each" and "Interfaces and
dependencies between deliverables" (SD:270-271); the work instruction is "Divide work into
independently verifiable deliverables ... Split where a reviewer could meaningfully accept one
result and reject another, not merely at arbitrary file or line counts" (SD:260). That sizes by
acceptability to a reviewer, not by what a seat can replace whole. The consumers are Define Tests and
Build, each with `mode = "single"` on `plan.md` (SD:57, 68), both `is_coding_step = true` (SD:61, 72).
Build then "Work[s] through the planned deliverables in dependency order" (SD:352), in the one shared
worktree: "do not create worker worktrees" (SD:100). The engine rule is that coding steps "use exact
`single` inputs only: no wildcard inputs, `each` or `complete`" (GUIDE:329) and that a second coding
execution is not started while the claim is live (GUIDE:333). The plan's deliverables are therefore
prose sections inside one file, consumed by one session in sequence.

The engine does have fan-out. `each` "Create[s] one ordinary execution per bound input occurrence"
(GUIDE:116; input modes at GUIDE:92); `complete` supplies the whole collection
to a merge, "accounting for every expected producer/worker" (GUIDE:117, 127); the live-session cap
is "Maximum live sessions, default 10" (GUIDE:61). AS uses it. Its planning step declares outputs
`search-plan.md` and `search-request-*.md` (AS:27) with `auto_advance_default = true` (AS:31) and is told
to "Write a finite `search-request-*.md` family for the meaningful searches this wave needs.
Each member carries the relevant brief, one concrete search assignment ... " (AS:144). Workers each
process "only your assigned search request" (AS:156). A later step issues "one `source-request-*.md`
member for each distinct source" (AS:183) with "Do not impose an arbitrary cap on the roster; the engine
queues sessions under the task's live-session limit" (AS:185). That is a decomposition into seat-sized
units taken in parallel. Three things distinguish it from what this set reviews:

1. The units are independent. No unit names another it depends on; the dependency is the engine's
   wait-for-all (`complete`), not a hypothesis passed between units. Pattern 2 has nothing to act on.
2. It is non-coding. A coding step "may produce wildcard outputs for non-coding fan-out" (GUIDE:331), so
   a coding Plan could emit `request-*.md` for analysis workers. Implementation workers in the same
   worktree remain excluded (GUIDE:329-333), and the guide says "do not call it non-coding to gain
   parallelism" (GUIDE:349).
3. It is not gated. AS's planning step auto-advances (AS:31), so the family goes to workers without a
   human or another seat reading it first, unless the user turns the lock on for that task.

So the efficiency set's object (seat-sized units, explicit dependencies, several agents in parallel, a reviewed
tree) appears in Alinery only for research playbooks, with independent units and no review step. The
code path is prose for one session.

## D. A counterexample: where reading Alinery through this set misleads

**Pattern 6, review-the-decomposition-before-dispatch, looks present and is doing something else.**

The README sentence is "Review the decision before starting Plan" (README:66), and the human gates in
SuperDevelop are Decide (SD:40) and Prepare Review (SD:84). That looks like a reviewed plan before Build.
The gate is on the design, upstream of the decomposition. Plan has `auto_advance_default = true`
(SD:51), and so does Define Tests (SD:62) and Build (SD:73); the person's approval of `decide.md` is the
last human check before the plan is written, tested and built. The plan is reviewed by "Review the
plan against every acceptance condition" (SD:263), which is the author-session reading its own plan, and
Build's own review is an independent reviewer "if available and authorized", else "label your review
as self-review" (SD:356). Pattern 6 asks for the opposite of that last point: the tree reviewed, once,
before any seat runs, by someone other than its author. Alinery's person can open `plan.md` and
comment on it (README:56, DESIGN:79), and may lock Plan by switching auto-advance off for the task
(GUIDE:292 "`auto_advance_default` initializes a task-level choice"); the playbook as
shipped does not ask for it. A reader who sees "human review is built into the playbook" and ticks
row 6 yes is crediting the design review for the decomposition review.

## E. What problem Alinery is solving

Alinery's own description is "A Playbook IDE for consequential work" (README:7): put "a team of agents to
work with a Playbook. Give them a process, steer the important decisions, and build on the results"
(README:23), and judges progress by "whether people can explain the decisions behind the code, catch mistakes at
useful checkpoints, and resume work without reconstructing the whole task" (VISION:97). The scarce thing it
protects is the person's attention and understanding: "how much useful, verified work a person can direct
with the attention they have" (VISION:51). The efficiency set protects a different scarce thing, the
reviewer's judgement across many seats, and gets throughput by decomposing once and reviewing the
decomposition. Alinery slows the work to keep one person aligned (README:37 "slower, deeper work with
AI"); the set speeds it by reviewing a plan so that many seats can run unattended.
