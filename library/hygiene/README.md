# hygiene/ — shared patterns of three automated-upkeep policies

Three policies in this stack act unattended on things that are usually, but not
always, safe to act on:

- **buffer cleaner** (`buffer-cleaner/`) — kills unneeded Emacs buffers;
- **inbox zero** (`inbox-zero/`) — keeps repositories committed, pushed and in
  step across boxes;
- **test registry** (`test-registry/`) — lets a reviewer verify an author's
  registered test run instead of rerunning it (being built, 2026-09-16).

The patterns in this directory state what the three share. Where a unit must stay equivalent after crossing a boundary rather than be cleared, see `translation/`. Each one's `@why` is
the reason that holds in all three settings; its `@how` is a checklist every
generation must satisfy; its violation signature gives one real incident from
each generation. The generation patterns carry the mechanics for their own
setting. The `apparatus/` patterns above state the general principles these
cite.

| hygiene pattern | buffer cleaner | inbox zero | test registry |
|---|---|---|---|
| observe-the-authority | observe-categories | measure-against-the-remote | bind-warrant-to-the-diff |
| classify-by-remedy-judgement | classify-staleness | classify-the-dirt, generated-by-role (remedy for untracked noise: ignore-by-kind) | rerun-when-the-warrant-fails (lanes) |
| exempt-the-in-use | exempt-the-in-use | promote-at-turn-end (in-flight edits) | rerun-when-the-warrant-fails (mandatory lanes) |
| receipt-then-gate | receipt-then-gate | push-the-declared | register-the-run, judge-adequacy |
| route-to-who-can-act | — (no gated consumer yet) | escalate-by-who-can-act | warrant-rides-the-handoff |
| settle-with-meters | yield-settled | gate-fails-loudly | meter-the-saving |

## Reading one row

Take `observe-the-authority`. The shared reason: a verdict read from a local
stand-in passes exactly when the stand-in has drifted. The three mechanisms
differ:

- the buffer cleaner's first collector read display time from the eval buffer,
  so every buffer reported the same age — fixed by reading each buffer's own
  value;
- futon-sync compared against the last-fetched ref and showed `=` while futon3
  was 27 commits behind — fixed by fetching first;
- the test registry's review refused on an environment mismatch that turned out
  to be `LC_ALL` set in one shell and not the other — fixed by declaring the test
  environment in the warrant.

One `@why`, three `@how`s, three incidents.

## Gaps kept visible

The rows do not line up perfectly, and the differences are recorded in each
pattern rather than smoothed over:

- the buffer cleaner has no routing instance, because its execution is refused
  until a gated consumer exists;
- in the test registry the exemption runs the other way: mandatory lanes are
  exempt from the saving (they must execute), not from an action;
- inbox zero needs two classification patterns, because tracked generated files
  split again by role (intermediate, rebuildable view, deliverable, evidence).

## Status

Drafts, 2026-09-16 (claude-7). `exempt-the-in-use` and `receipt-then-gate` were
promoted from `buffer-cleaner/`, where they had named themselves shared and asked
to move on the third generation's citation; the `buffer-cleaner/` files keep the
buffer-cleaner instance. The test-registry column rests partly on a feature
still being built; its evidence is from the 2026-09-14 round trip and a
2026-09-16 worked instance
(`futon2/holes/labs/wm-contract/WORKED-INSTANCES-pattern-interpretation-2026-09-16.md`).
