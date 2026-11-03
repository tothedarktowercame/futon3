# Review of HOWEVER-undercuts-proposal-2026-11-03.tsv (claude-2, 2026-11-03 16:4xZ)

Checked by script: all 31 `however-sentence` values match the HOWEVER block of
their pattern verbatim (whitespace-normalised); every `undercuts` value is a
pattern in this directory, `self`, or `none`. Job
`invoke-1793723865212-10860-0c9dc357` (codex-9), commit 903d16b69.

## Rival rows: accepted as typed undercuts

| attacker | undercuts | shown by (recorded vocabulary) | note |
|---|---|---|---|
| prepare-the-ground | what-others-wait-on-first | Giraffe row: cone > 0 and unblocks = 0 on the undercut argument's pick | both sentences (the rule and the Giraffe pilot 2 measurement) back one edge |
| finish-what-is-nearly-done | what-others-wait-on-first | unrecorded: "nearly done" is not a Giraffe, placement or header field | edge accepted; it cannot be shown at selection time until a field exists |
| take-the-hard-one-to-learn | take-the-cheap-win | unrecorded: "close sooner" and "look better on a count" are not recorded | edge accepted, unshowable today; the TSV's "status, prerequisites" does not show it |
| what-others-wait-on-first | take-the-cheap-win | Giraffe row: the cheap pick's unblocks lower than another ready candidate's | edge accepted |

## Rival row rejected

- finish-what-is-nearly-done → prepare-the-ground: the sentence attacks scores
  built on structural importance; prepare-the-ground is not a score, and its
  second clause ("staged work that grew beyond the item's scope") is self.

## Self and condition rows

Not typed as undercuts. Where the fact column names a recorded field (status,
prerequisites, placement status/action, header date/timebox), the condition is
already what the pattern's own gate excludes (serves-a-way's placement kinds,
only-what-can-start-now's prerequisites, bound-the-bet's timebox). Where it says
"no recorded fact", nothing at selection time can show it, and an undercut that
cannot be shown must not fire (absence must not read as a value).

## Rule for the frame (packet argument-C2)

Argument X undercuts argument Y when a pattern p of X carries `@undercuts q`,
q is a pattern of Y, and p is not itself a pattern of Y (a cascade that holds
both has answered the undercut inside itself, as giraffe-order does with cone
before unblocks). The attack is `:undercut` and can defeat only when its
showing predicate holds on Y's pick; otherwise it is recorded as
`:undercut-unshown` and defeats nothing.
