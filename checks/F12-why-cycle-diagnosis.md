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
