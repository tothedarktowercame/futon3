# F12 part 2: why-cycle diagnosis

2026-09-09, codex-17, for claude-1's independent review.
Discovery only; no pattern, checker, or relation has been changed.

Basis: futon3 `e0c2570bb4ed1ac3f3df7af0b53ea04d506c4035` (main).
Agency packet: `invoke-1788925682087-15941-cd88c889`.

## Finding

The constructor's actual reader reports **1,319 patterns, 508 internal authored
why edges, 9 cyclic nodes, and 5 elementary directed cycles**. Every cycle has
two nodes. Four strongly connected components contain them: three pairs and
the three-node WR-27 component, which contains two cycles.

All five are **class (a): an authoring error in relation meaning and direction**,
not line-number drift. The September 5 L14 pass added general-ruling-to-specific-
problem `@why` edges opposite existing problem-to-ruling edges. Its matching
criteria establish shared coverage or a textual mention, not the authority
relation asserted by `@why`. I found no cycle requiring class (b), genuine
mutual authority forbidden by acyclicity. This classification is an assessment
of the cited content and history, not something graph traversal alone proves.

The proposed repair is to retract the five unsupported L14 authority assertions
through a reviewed library edit, preserving the dated diagnosis and provenance.
Do not automatically replace them with `@how`, `@why-posthoc`, or `@see-also`.
Any replacement relation needs its own editorial warrant. No repair is applied
by this packet.

## Complete cycle and edge enumeration

Each row is `problem -> ruling -> same problem`. The problem's outgoing edge
is at line 4 in every case. Links name the files; the line columns identify
the exact directives at the pinned basis.

| Cycle | Problem file | Ruling file | Problem -> ruling | Ruling -> problem | L14 reverse-edge basis |
|---|---|---|---|---|---|
| 1 | [commitment-temperature-is-instrumented-as-gain](../library/problems/commitment-temperature-is-instrumented-as-gain.flexiarg) | [WR-27](../library/war-room/wr-27-a-loop-is-born-instrumented-for-its-gain.flexiarg) | line 4 | line 11 | `named` |
| 2 | [operator-turns-become-inference-observations](../library/problems/operator-turns-become-inference-observations.flexiarg) | [WR-16](../library/war-room/wr-16-operationalised-exploit-loops-are-first-class-observation-channels.flexiarg) | line 4 | line 10 | `holds` |
| 3 | [per-tick-mismatch-instruments-outer-loop-gain](../library/problems/per-tick-mismatch-instruments-outer-loop-gain.flexiarg) | [WR-27](../library/war-room/wr-27-a-loop-is-born-instrumented-for-its-gain.flexiarg) | line 4 | line 12 | `named` |
| 4 | [satisfied-rungs-are-counted-and-surfaced](../library/problems/satisfied-rungs-are-counted-and-surfaced.flexiarg) | [WR-25](../library/war-room/wr-25-good-news-gets-the-same-evidence-discipline-as-bad.flexiarg) | line 4 | line 13 | `named` |
| 5 | [tension-proposes-candidates](../library/problems/tension-proposes-candidates.flexiarg) | [WR-19](../library/war-room/wr-19-tension-must-generate-not-only-rank.flexiarg) | line 4 | line 12 | `named` |

These are ten cycle-participating edges. The reader also reports an eleventh
edge between cyclic nodes: **WR-27 -> WR-16**, at WR-27 line 7. It is not on
any cycle: there is no return path from WR-16 to WR-27. It was authored in
`3764729a` on 2026-08-22 at 15:58:35 UTC and should remain unchanged. Counting
edges whose endpoints are cyclic is not the same as enumerating cycle edges.

## Authoring history

All five **problem -> ruling** edges originated in
`2aa8d0cc8c206a679d29766ed10e43e0ec022a49`, 2026-08-25 11:08:58 UTC,
“Add library/problems: @how patterns for the five control-map red rings.”
The commit explicitly describes five new patterns, one per ring, each with
`@why` pointing at its ruling. The problem texts were written to address the
unmet ruling; these are not older implementation patterns retrospectively
assigned an explanation. This supports retaining the original direction.

All five **ruling -> problem** edges originated in
`c0b001cb8308804721dad51c01cc2f7a6e665cc3`, 2026-09-05 12:57:37 UTC,
“library: L14 edge-resolution pass -- 35 resolvable @why edges added
(holds/named bases only, no invention).” Git records Joseph Corneli as author;
the new lines identify zai-1 and the L14 pass. Git identity alone is not a
claim that Joe personally judged each relation.

Verified with `git blame -L N,N -- FILE` for all ten directives and
`git show` of the originating commits. The current
[L14 receipt](../../futon2/holes/labs/library-loop/runs/L14-edge-resolution.edn)
lists these additions at lines 25, 26, 28, and 30. The authoring script
[l14_resolve.py](../../futon2/holes/labs/library-loop/runs/l14_resolve.py)
defines `matches` at lines 39-47: shared `@holds-at` tokens, otherwise the
pattern ID appearing anywhere in the problem text. At lines 78-82 it writes
those matches as `@why`. It does not establish why the ruling would derive
authority from the matched problem.

The script was read, not executed: it edits the library. Its current receipt
and the [L14 delta](../../futon2/holes/labs/library-loop/runs/L14-delta.md)
describe a pass with 37 additions, whereas the originating commit title says
35. The individual edge origins above come from blame and the commit diff,
not an inference from either aggregate count.

## Classification and proposed disposition per edge

The directive contract is explicit in
[README-flexiarg.md](../README-flexiarg.md), lines 168-180: `@why` points toward
the general authority, `@how` toward curated specific methods, and the two are
not mechanical inverses. Lines 223-234 separately distinguish R-node coverage
from pattern relations. The checker's acyclicity contract is stated at
[library_graph_lint.clj](library_graph_lint.clj), lines 236-246.

| Pair | Original edge: assessment and proposal | L14 reverse edge: assessment and proposal |
|---|---|---|
| Commitment temperature / WR-27 | Retain. The problem's context identifies the WR-27 requirement and its THEN specifies instrumentation of one gain (problem lines 9-12, 30-35). | Class (a); propose retracting WR-27 line 11. The general birth-time instrumentation rule does not rest on this specific commitment-temperature task. The problem even declares its salience unfilled. |
| Operator turns / WR-16 | Retain. The problem was authored against the named R2/WR-16 gap and specifies an observation-channel method (problem lines 9-12, 30-44). | Class (a); propose retracting WR-16 line 10. A shared R2 location establishes coverage, not authority. WR-16 also covers independent telemetry streams and is not justified by the operator-turn task. |
| Per-tick mismatch / WR-27 | Retain. The problem applies the general instrumentation rule to the reward outer loop (problem lines 9-11, 28-39). | Class (a); propose retracting WR-27 line 12. The named reference in the problem records the rule it applies; it does not reverse that dependency. |
| Satisfied rungs / WR-25 | Retain. The specific counting/surfacing repair applies the general symmetric evidence discipline (problem lines 10-13, 27-38). | Class (a); propose retracting WR-25 line 13. WR-25's financial evidence discipline is broader than the rung-counting application. A textual reference does not make that application its authority. |
| Tension proposer / WR-19 | Retain. The task explicitly implements the ruling's unbuilt proposer (problem lines 10-12, 30-41). | Class (a); propose retracting WR-19 line 12. The task cites the ruling it seeks to satisfy. Its text does not establish that the ruling is an instance of the task. |

The original commit calls the new patterns potential `@how` methods, which
makes those reasonable editorial candidates, not automatically licensed new
edges. Several texts explicitly lack live evidence. Retraction of an incorrect
authority assertion must not silently become a claim that its proposed method
works. Likewise, `@why-posthoc` would preserve an authority-shaped explanatory
claim that the L14 matching test never warranted; using it just to remove the
cycle would be a workaround.

No acyclicity exemption, inverse-edge exception, or checker change is proposed.
If a reviewer finds evidence that any reverse edge really is authority, stop
and route that content/requirement conflict to the library lane or Joe; do not
delete whichever edge happens to make a test green.

The L14 mechanism can repeat this mistake even after the five edges are
retracted. Its authoring discipline therefore needs a separate library-lane
review: coverage/mention matching may nominate candidates, but cannot itself
certify authority or replace a whole-graph acyclicity check. That follow-up is
outside this discovery packet.

## Reproduction and completeness

Run from futon3; this only reads the library and prints results:

```sh
clojure -Sdeps '{:paths ["checks"]}' -M - <<'EOF'
(require '[construct-cascade :as c] '[find-organise :as f])
(let [r (f/read-repository c/library-root (c/library-sections c/library-root))
      nodes (set (:cycles r))
      adj (:stands-on r)
      walk (fn walk [start path]
             (mapcat (fn [n]
                       (cond
                         (= n start) [(conj path start)]
                         (some #{n} path) []
                         (neg? (compare n start)) []
                         :else (walk start (conj path n))))
                     (sort (filter nodes (get adj (peek path))))))
      cycles (vec (mapcat #(walk % [%]) (sort nodes)))]
  (prn {:patterns (count (:patterns r)) :why-edges (count (:edges r))
        :cyclic-nodes (count nodes) :simple-cycles (count cycles)})
  (doseq [cycle cycles] (prn cycle)))
EOF
```

This uses the same sections and `#{:why}` default as the constructor's
`why-repo`. The reader identifies every node reachable from itself. Every
cycle consists entirely of those nodes. The traversal enumerates simple paths
back to their start, disallows repeated interior vertices, and admits a cycle
only from its least vertex, eliminating rotational duplicates. On this basis
it enumerates precisely the five cycles above; there are no self-loops or
additional longer elementary cycles. Non-simple closed walks can repeat or
combine these cycles and are not counted as new cycles.

## Validation and limits

The census and enumeration above exited 0. All ten cycle edges were checked
against file text and blame; the extra WR-27 -> WR-16 edge was separately
checked. The five problem bodies and four ruling bodies were read in full.
The classification does not rely on generic similarity or a coverage score.

This delivery changes only this Markdown note. No executable or flexiarg is
modified, so production Clojure lint, Lisp parentheses, and pattern-authoring
checks have no changed source target. The embedded Clojure reproduction is
executed and separately checked with clj-kondo and check-parens. Local document
links and whitespace are checked. The constructor remains blocked on its
unchanged cyclic graph; no passing constructor result or applied repair is
claimed.


## Part 2b — authorization and pre-edit gate, 2026-09-09

Claude-1 accepted the diagnosis and authorized retraction of the five L14
reverse edges in Agency review `invoke-1788925976912-15951-a80745e3`, citing
README-flexiarg:168-180 as standing warrant. No relabeling is authorized.

Scope clarification: the original diagnosis's “retain” for WR-27 -> WR-16
means **unchanged in this cycle-repair packet**, not an endorsement of the
relation. Its blame is `3764729af84ddf53c2054780036b0b729aa958c7`
(2026-08-22 15:58:35 UTC), not `c0b001cb`; it is not part of the census below.

Before editing, the repository's required `futon3.chops/validate-sigil` check
returned `:valid? false`, `:unknown-emoji` for `⚖/令`, the existing sigil on
all four target WR patterns. No patterns were edited at this stage.
`resources/tokizh/tokizh.org` does not contain scales; the existing migration
catalogue `resources/sigils/sigil-migrations.edn:40` maps `⚖️` to `👯` (`sama`,
balance -> same/equal). A separate validation of `👯/令` returned `:valid? true`.
The catalogue spelling includes a variation selector while the patterns do not;
the semantic correspondence is a proposed migration, not a silently applied
normalization. The gate conflict and proposed bounded prerequisite were sent
to claude-1 as `invoke-1788926110221-15955-6325ccda`.

### Full c0b001cb edge census for the separate library review

Derived from `git show --format= --unified=0
c0b001cb8308804721dad51c01cc2f7a6e665cc3`: **35 added `@why` lines in 25 files**.
Every line names one target. All 35 exact lines remain present at the pre-edit
basis `cef70c7`; the table records their pre-edit line numbers. This is the
original commit's population, not the later L14 receipt's 37-edge population.
No verdict is asserted for the other 30 edges. Shared generator provenance
makes them candidates for review, not automatically authorized retractions.

Every source and target below is a qualified pattern ID. Its file is
`library/<id>.flexiarg`. The origin for every row is `c0b001cb`.

| Source | Target | Pre-edit source line | This packet |
|---|---|---|---|
| `aif/belief-state-operational-hypotheses` | `problems/r1-belief-state` | 10 | Separate review; unchanged |
| `aif/candidate-pattern-action-space` | `problems/r6-candidate-action-space-and-selection` | 14 | Separate review; unchanged |
| `aif/candidate-pattern-action-space` | `problems/tension-proposes-candidates` | 15 | Separate review; unchanged |
| `aif/evidence-precision-registry` | `problems/r7-evidence-channel-precision` | 13 | Separate review; unchanged |
| `aif/expected-free-energy-scorecard` | `problems/g-over-cascade-is-undefined` | 12 | Separate review; unchanged |
| `aif/expected-free-energy-scorecard` | `problems/r5-expected-free-energy-core` | 13 | Separate review; unchanged |
| `aif/expected-free-energy-scorecard` | `problems/satisfied-rungs-are-counted-and-surfaced` | 14 | Separate review; unchanged |
| `aif/free-energy-as-tick-scalar` | `problems/per-tick-mismatch-instruments-outer-loop-gain` | 14 | Separate review; unchanged |
| `aif/free-energy-as-tick-scalar` | `problems/r8-present-fit-mismatch` | 15 | Separate review; unchanged |
| `aif/grounded-actuation-not-reobservation` | `problems/r16-grounded-actuation` | 13 | Separate review; unchanged |
| `aif/hierarchical-and-temporal-depth` | `problems/r15-hierarchy-and-timescale` | 14 | Separate review; unchanged |
| `aif/hierarchical-budget-aware-action-selection` | `problems/r11-hierarchical-shared-budget` | 13 | Separate review; unchanged |
| `aif/interoceptive-tripwires` | `problems/r20-interoceptive-tripwires` | 14 | Separate review; unchanged |
| `aif/no-self-certification` | `problems/r9-no-self-certification` | 16 | Separate review; unchanged |
| `aif/policy-precision-commitment-temperature` | `problems/commitment-temperature-is-instrumented-as-gain` | 13 | Separate review; unchanged |
| `aif/policy-precision-commitment-temperature` | `problems/r14-commitment-temperature` | 14 | Separate review; unchanged |
| `aif/predictive-coding-belief-update` | `problems/r3-belief-update` | 14 | Separate review; unchanged |
| `aif/scheduled-observer-entrypoint` | `problems/r10-scheduled-entrypoint` | 13 | Separate review; unchanged |
| `aif/shared-kernel-predictive-forward-model` | `problems/r4-forward-model` | 13 | Separate review; unchanged |
| `aif/structure-learning-by-model-reduction` | `problems/r17-structure-learning` | 12 | Separate review; unchanged |
| `aif/structured-observation-vector` | `problems/operator-turns-become-inference-observations` | 13 | Separate review; unchanged |
| `aif/structured-observation-vector` | `problems/r2-structured-observation` | 14 | Separate review; unchanged |
| `aif/temporal-depth-beyond-greedy` | `problems/r13-temporal-policy-depth` | 13 | Separate review; unchanged |
| `aif/two-layer-calibration` | `problems/r12-two-layer-calibration` | 14 | Separate review; unchanged |
| `futon-theory/futonic-logic` | `problems/satisfied-rungs-are-counted-and-surfaced` | 12 | Separate review; unchanged |
| `war-room/wr-16-operationalised-exploit-loops-are-first-class-observation-channels` | `problems/operator-turns-become-inference-observations` | 10 | Retracted in part 2b |
| `war-room/wr-16-operationalised-exploit-loops-are-first-class-observation-channels` | `problems/r2-structured-observation` | 11 | Separate review; unchanged |
| `war-room/wr-19-tension-must-generate-not-only-rank` | `problems/r17-structure-learning` | 11 | Separate review; unchanged |
| `war-room/wr-19-tension-must-generate-not-only-rank` | `problems/tension-proposes-candidates` | 12 | Retracted in part 2b |
| `war-room/wr-24-a-removed-constraint-does-not-remove-the-discipline-it-supplied` | `problems/r13-temporal-policy-depth` | 11 | Separate review; unchanged |
| `war-room/wr-25-good-news-gets-the-same-evidence-discipline-as-bad` | `problems/r9-no-self-certification` | 12 | Separate review; unchanged |
| `war-room/wr-25-good-news-gets-the-same-evidence-discipline-as-bad` | `problems/satisfied-rungs-are-counted-and-surfaced` | 13 | Retracted in part 2b |
| `war-room/wr-26-a-capability-switched-off-carries-its-re-arm-condition-in-writing-at-the-switch` | `problems/r20-interoceptive-tripwires` | 10 | Separate review; unchanged |
| `war-room/wr-27-a-loop-is-born-instrumented-for-its-gain` | `problems/commitment-temperature-is-instrumented-as-gain` | 11 | Retracted in part 2b |
| `war-room/wr-27-a-loop-is-born-instrumented-for-its-gain` | `problems/per-tick-mismatch-instruments-outer-loop-gain` | 12 | Retracted in part 2b |


### Retractions applied, 2026-09-09

Authorized by claude-1 review `invoke-1788925976912-15951-a80745e3` under
README-flexiarg:168-180. These five lines were removed, not relabeled. Their
common blame is `c0b001cb8308804721dad51c01cc2f7a6e665cc3`
(2026-09-05 12:57:37 UTC); original problem -> ruling edges are unchanged.
Pointers below are the pre-edit positions preserved in the census above.

`library/war-room/wr-16-operationalised-exploit-loops-are-first-class-observation-channels.flexiarg:10` — removed verbatim:

```text
@why problems/operator-turns-become-inference-observations (L14 edge-resolution; basis: holds -- the problem node's own holds-at token covers this pattern; source: receipt runs/L14-edge-resolution.edn; zai-1, 2026-09-05)
```

`library/war-room/wr-19-tension-must-generate-not-only-rank.flexiarg:12` — removed verbatim:

```text
@why problems/tension-proposes-candidates (L14 edge-resolution; basis: named -- the problem node's own text names the pattern id covers this pattern; source: receipt runs/L14-edge-resolution.edn; zai-1, 2026-09-05)
```

`library/war-room/wr-25-good-news-gets-the-same-evidence-discipline-as-bad.flexiarg:13` — removed verbatim:

```text
@why problems/satisfied-rungs-are-counted-and-surfaced (L14 edge-resolution; basis: named -- the problem node's own text names the pattern id covers this pattern; source: receipt runs/L14-edge-resolution.edn; zai-1, 2026-09-05)
```

`library/war-room/wr-27-a-loop-is-born-instrumented-for-its-gain.flexiarg:11` — removed verbatim:

```text
@why problems/commitment-temperature-is-instrumented-as-gain (L14 edge-resolution; basis: named -- the problem node's own text names the pattern id covers this pattern; source: receipt runs/L14-edge-resolution.edn; zai-1, 2026-09-05)
```

`library/war-room/wr-27-a-loop-is-born-instrumented-for-its-gain.flexiarg:12` — removed verbatim:

```text
@why problems/per-tick-mismatch-instruments-outer-loop-gain (L14 edge-resolution; basis: named -- the problem node's own text names the pattern id covers this pattern; source: receipt runs/L14-edge-resolution.edn; zai-1, 2026-09-05)
```

### Canonical sigil migration applied, 2026-09-09

Claude-1 amended scope in `invoke-1788926110221-15955-6325ccda`, citing
`resources/sigils/sigil-migrations.edn:40`: scales -> `👯` (`sama`). The
catalogue's VS16 form `⚖️` and these files' bare `⚖` are the same glyph family;
this equivalence and its application were explicitly approved in that review.

Exactly the four files below change their line 3 from `@sigils [⚖/令]` to
`@sigils [👯/令]`; no other sigil or pattern content changes. Claude-1 reports
36 library files with legacy scales, 28 in war-room; that is owner-supplied
scope evidence, not a fresh count by this packet. The other 32 are outside
this repair. The unchanged WR-27 -> WR-16 relation is not endorsed by this edit.

- `library/war-room/wr-16-operationalised-exploit-loops-are-first-class-observation-channels.flexiarg:3`
- `library/war-room/wr-19-tension-must-generate-not-only-rank.flexiarg:3`
- `library/war-room/wr-25-good-news-gets-the-same-evidence-discipline-as-bad.flexiarg:3`
- `library/war-room/wr-27-a-loop-is-born-instrumented-for-its-gain.flexiarg:3`


### Part 2b validation result

The whole-library constructor command from futon3:

```sh
clojure -Sdeps '{:paths ["checks"]}' -M -m construct-cascade
```

exited **0**. The refusing `require-pass!` gate returned successfully: no
failure vector was raised. The committed generated output is
[construct-cascade.edn](construct-cascade.edn), regenerated by this command,
not edited by hand. The stdout log is also retained locally at
`/tmp/codex17-f12-retracted-constructor.log`.

```text
library: 100 sections, 1319 patterns, 503 authored @why edges, 535 @why+@how; read-digest 8b20e68b
find F1-F4: all true
widen-to-a-budget: 20 nodes / 3 edges; O1-O3 all true
widen-to-the-marginal-gain-floor: 41 nodes / 14 edges; O1-O3 all true
VERDICT distinguishable-from-uniform = true (without the floor arm: true)
controls: 69 citations re-read, 0 unreadable; correspondence 3/3; grain leaks 0; mutations 5 declared, 0 slipped
construct-cascade: PASS exit-convention=0-pass/1-fail
```

Checks and boundaries:

- All four actual file sigils were extracted and passed to
  `futon3.chops/validate-sigil`: each returned `:valid? true`, `:errors []`,
  input `👯/令`, decoded `sama [令]`; the assertion command exited 0.
- Each changed flexiarg was parsed with the repository's authoritative
  `contrib/flexiarg.el` parser. Assertions checked matching qualified ID,
  canonical sigil metadata, one conclusion root, context/IF/HOWEVER/THEN/BECAUSE
  children, and full parsed-body equality to its pre-edit copy. All four
  passed, exit 0. The parser emitted its existing obsolete-`when-let` warning;
  the temporary harness also lacked a lexical-binding cookie. Neither was
  suppressed or reported as a parser failure.
- Generated `checks/construct-cascade.edn`: clj-kondo 0 errors / 0 warnings;
  `futon4/dev/check-parens.el` OK. No checker implementation changed.
- Exactly 30 of the 35 original `c0b001cb` edge lines remain verbatim; the five
  removed lines are preserved above. The original forward edges and all other
  pattern metadata apart from the four sigils are unchanged.
- Local document links resolve and `git diff --check` passes.

This supersedes the earlier discovery-stage statement that the constructor
remains blocked. It does not close F12: the two math candidates' separately
observed authored-edge absences, the subsequent exemplar/Lean work, and the
broader L14/sigil reviews remain outside this packet.
