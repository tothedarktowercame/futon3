# TN — mission→pattern correspondence: where the survey lives, and why an ARGUE-scoped count reads low

**Status: measurement, 2026-08-23 (claude-13, laptop). Revised same day** — the
first version claimed the survey was readable only in the migration snapshot. It is
not: 991 `scope/pattern` entities are live on (L). Only the *edge* layer is missing.
§3a is new and corrects the "Zone is a stale replica" reading. Written in response to an
agent on Zone reporting **"151 patterns cited across 42 ARGUE sections"** and asking
whether that is surprisingly small. Short answer: the ARGUE-scoped number is small
for a structural reason (patterns are not cited *in* ARGUE), the corpus-wide survey
does exist and gives **297 patterns across 176 missions**, and the layer that would
normally serve that survey — the `mission-scope/pattern` hyperedges — did not
survive the futon1a→futon1b migration.

Companion to `futon1b/TN-pattern-duplication-findings.md` (2026-08-13), which
measures the same substrates from the pattern-library side.

---

## 0. What this note does and does not establish

- It **does** measure the mission→pattern correspondence over the artifacts
  available on the laptop today, with repro commands (§6).
- It **does not** reproduce Zone's `151 / 42` figure. I do not know what produced
  it — which corpus it scanned, which pattern-name form it matched, or how it
  delimited an "ARGUE section". Every comparison below to that number is
  therefore a *hypothesis about the discrepancy*, flagged as such. To close it,
  the Zone-side script and its mission checkout need to be named.
- Numbers marked **(snapshot)** come from a 2026-07-12 export, not a live store.

Environments, kept explicit because they disagree:

- **(L)** laptop — `futon1b-server --store-dir migration-store-21 --port 7073`, live.
- **(Z)** Zone (`zone-joe`, ams) — same server, same store *name*, **diverged contents**.
- **(X)** `futon1b/migration-export-full/graph-snapshot.edn` — the 2026-07-12
  futon1a export, 91,809 docs. Both (L) and (Z) were seeded from this file —
  it is byte-identical on the two machines (sha256
  `3e7e2db6…f8ce5bf`), which is what makes §3a's argument possible.
- **(D)** the mission corpus on disk — `futon*/holes/missions/*.md`, 423 files.

---

## 1. The survey exists, and it is a scope layer, not an edge layer

It was built by **D1.3(a)** of `futon3c/holes/missions/M-mission-scopes-into-substrate-2.md:260`
(ingested by codex-3 and independently reviewed by claude-3, 2026-06-09). The
detector is `futon6/scripts/mission_scope_detect.py`; the binder is
`futon3c/src/futon3c/scripts/mission_scope_ingest.clj --binder pattern`.

Detection rule (`load_pattern_index`, `mission_scope_detect.py:162`): a literal
occurrence of a `futon*/library/**/*.flexiarg` basename with **≥2 hyphens and ≥12
characters**. Scope id shape is `<mission-stem>/pattern/<flexiarg-slug>`.
Attestation is deliberately weak — "referenced", not psr/pur/pxr "used".

Two distinct artifacts come out of that run, and they have had different fates:

| artifact | type | fate |
|---|---|---|
| scope entities | `:scope/pattern` | **survived** — 934 in (X), **991 live on (L)** |
| mission→pattern edges | `mission-scope/pattern` hyperedges | **did not survive** — see §3 |

Counting the surviving scope entities in (X):

Live census on (L), 2026-08-23: `GET /api/alpha/census?entity-type=scope/pattern`
returns **991** — *more* than the snapshot, because ingest has continued since
2026-07-12. The correspondence is therefore readable live; it is only the **edge**
layer that is missing (§3). The breakdown below is computed over (X) because the
snapshot is the surface on which the per-document props can be scanned offline,
not because the live store lacks the data.

| (X), 2026-07-12 | |
|---|---|
| `:scope/pattern` docs | **934** |
| distinct `:entity/external-id` | 934 |
| distinct mission→pattern pairs | **934** (no duplicates) |
| distinct missions citing ≥1 pattern | **176** |
| distinct patterns cited | **297** |
| `:anchor/state` | 934/934 `:anchored` |

A representative document, abridged:

```clj
{:entity/id "agency-rebuild/pattern/single-routing-authority"
 :entity/name "Patterns"                       ; NOTE: the heading, not the id
 :entity/type :scope/pattern
 :entity/external-id "agency-rebuild/pattern/single-routing-authority"
 :entity/source "mission-scope-tree"
 :entity/props {:pattern/ident "single-routing-authority"
                :pattern/ref "futon3/library/agency/single-routing-authority.flexiarg"
                :mission "M-agency-rebuild"
                :mission/path "/home/joe/code/futon3/holes/missions/M-agency-rebuild.md"
                :anchor/passage "## Patterns (Normative)"
                :anchor/heading "single-routing-authority"
                :anchor/state :anchored
                :scope/binder-type "pattern"
                :scope/original-id "M-agency-rebuild:scope-003"}}
```

**Footgun for anyone querying this layer:** `:entity/name` holds the *anchoring
heading* ("Patterns"), not the scope id. Grouping by `:entity/name` collapses 934
docs to 376 groups and produces nonsense. Group by `:entity/external-id`, or by
`(:mission, :pattern/ident)` out of `:entity/props`.

---

## 2. Why an ARGUE-scoped count reads low: patterns are not cited in ARGUE

Bucketing the 934 scopes by the phase of the heading each one anchors to
(`:anchor/passage` matched against the eightfold phase names):

| anchoring section | distinct patterns | distinct missions |
|---|---|---|
| **other** (not a phase heading) | **276** | **171** |
| ARGUE | 33 | 11 |
| MAP | 22 | 13 |
| DERIVE | 19 | 13 |
| VERIFY | 16 | 14 |
| IDENTIFY | 8 | 8 |
| DOCUMENT | 8 | 7 |
| INSTANTIATE | 8 | 4 |
| HEAD | 2 | 2 |

The "other" bucket is not noise — it is a *convention*. The most common anchoring
passages are dedicated cross-reference blocks:

```
  23  - **A.6** … the 25-pattern math-reasoning atlas … (one bullet listing 25 patterns)
  20  ### Pattern Cross-Reference
  19  ### Q2: Existing patterns that apply
  17  ### Pattern Index
  15  ## Argument (Pattern-Backed)
  15  ### Pattern cross-reference (`futon3/library/` — all citations verified to exist)
  12  ### Pattern cross-reference
  11  ### Pattern cross-reference (`futon3/library/`, 989 flexiargs surveyed)
   8  ### Pólya Patterns as Agent Strategies
```

Missions collect their pattern citations into a cross-reference section rather
than inlining them into the argument. ARGUE is the *plain-language* argument phase
(see `feedback_plain_language_argue` and `mission_scope_detect.py:454`, where the
plain-language sub-scope of ARGUE is defined). So an ARGUE-scoped query is
structurally blind to roughly 90% of the correspondence. **The low number is a
property of the query, not of the corpus.**

Corroborated independently on (D), with no store involved — segmenting each of the
423 mission docs by phase heading and matching the 631 distinctive flexiarg
basenames (from the `futon*/library/**` glob) as literals:

| | |
|---|---|
| mission docs on disk | 423 |
| mission docs with an ARGUE heading at all | **64** |
| mission docs citing ≥1 pattern anywhere | 190 |
| distinct patterns cited anywhere | **257** |
| distinct patterns cited inside ARGUE sections | **68** |
| mission docs with a *citing* ARGUE section | **31** |

Distinct patterns per phase section, same scan: DERIVE 80, VERIFY 74, ARGUE 68,
MAP 64, INSTANTIATE 44, IDENTIFY 43, HEAD 27, DOCUMENT 18.

The disk scan (257 anywhere) and the snapshot (297) disagree by ~40 because they
were taken 6 weeks apart against a corpus that has moved, and because the snapshot
includes missions whose files have since been edited or relocated. They agree on
the shape, which is the load-bearing part.

### Hypotheses for the `151 / 42` discrepancy (unverified)

Zone's ARGUE slice is *larger* than both of mine (33 patterns / 11 missions in the
snapshot; 68 / 31 on disk). Candidate explanations, in rough order of likelihood:

1. **A looser ARGUE boundary.** `mission_scope_detect.py:50` aliases
   `open questions?|notes?` → `argue`, and `## Argument (Pattern-Backed)` (15
   scopes) is an ARGUE-flavoured heading that my phase matcher classified as
   "other". A scan that folds these in would gain a lot.
2. **A different pattern-name form.** I matched the 616 distinctive basenames in
   `futon3/library` (631 across all `futon*/library`). The store's deduped
   `pattern/library` listing carries **821** sigiled names in the `category/name`
   form (`iiching/exotype-157`). Matching that form, or dropping the ≥2-hyphen /
   ≥12-char filter, widens the net.
3. **A different mission corpus.** Per `futon0/README-bare-metal.md:426`, Zone was
   seeded with `futon5 futon2 futon3a` and `futon1/apps/*`. If its checkout has
   drifted from the laptop's 423 mission docs in either direction, the counts are
   not comparable at all.

Resolving this needs the Zone-side script named, not more inference from here.

---

## 3. The edge layer did not survive the migration

The June D1.3(a) run wrote **564** `mission-scope/pattern` hyperedges (282 linked /
282 detached, 232 distinct cited patterns, 94 distinct linked) against futon1a on
:7071. That figure is quoted from the mission record; :7071 is not running today,
so I could not re-verify it directly.

What *is* directly verifiable is that those edges are largely gone. Live census on
(L), 2026-08-23:

```
mission-scope/pattern    {:kind :hyperedge, :count 112}     ← vs 564 in June
mission-scope/concept    {:kind :hyperedge, :count 0}
mission-scope/source     {:kind :hyperedge, :count 0}
pattern/has-sigil        {:kind :hyperedge, :count 0}
mission/doc              {:kind :hyperedge, :count 0}
```

The cause is in the export summary itself,
`futon1b/migration-export-full/export-summary.edn`:

```clj
{:results {:graph      {:count 91809 :counts {:docs 91809 :entities 45124 :relations 46685}}
           :evidence   {:error "HTTP 500 for http://127.0.0.1:7071/api/alpha/evidence/sessions"}
           :hyperedges {:error "Read timed out"}}}          ; ← here
```

**Entities and relations migrated; hyperedges timed out and were never exported.**
The 112 edges now on (L) are a later partial re-ingest, not migrated data. The
`pattern/has-sigil` zero is the same hole seen from the pattern side — (X) holds
1780 of those relations (`futon1b/TN-pattern-duplication-findings.md` §5).

Consequence: **any consumer that reads `mission-scope/pattern` off the current
store undercounts the correspondence by roughly 5×.** The scope entities are
intact (991 live on (L)), so the edges are re-derivable without re-running
detection — see §5.

---

## 3a. (Z) is not a stale replica — it is an independent divergent line

The natural reading of §3 is "the migration was incomplete, therefore Zone is a
replica of an incomplete database." The first clause is true; the inference is not.
There is **no replication between (L) and (Z) at all**, and (Z) is not behind.

What is actually shared is the *seed*. `migration-export-full/graph-snapshot.edn` is
byte-identical on both machines:

```
3e7e2db639136ca2bc763b70523e309ba80bfa2b8b5a56a4b30825e20f8ce5bf   (L)
3e7e2db639136ca2bc763b70523e309ba80bfa2b8b5a56a4b30825e20f8ce5bf   (Z)
```

Both were built from that one 2026-07-12 export and have been written to
independently ever since. So the incomplete-hyperedge defect is **common to both** —
it is not a thing (Z) suffers and (L) escapes. Re-syncing (Z) from (L) would not
fix it; both need the same re-derivation (§5.2).

Meanwhile (Z) has diverged *upward*, not downward. Store inventory, 2026-08-23:

| | (L) Dionysus | (Z) zone-joe |
|---|---|---|
| `migration-store` | 1.5 G | 1.5 G |
| `migration-store-21` | **26 G** | **32 G** |
| `migration-store-21.merged-2026-08-17` | absent | **24 G** |
| `ams-store.retired-20260810` | absent | 400 M |
| futon1b server listening | :7073 up | **nothing on :7073** |

(Z) carries a merged store from 2026-08-17 that has no counterpart here, and its
`migration-store-21` is 6 G larger. This matches the row counts in
`futon1b/TN-pattern-duplication-findings.md` §1 — 5876 `pattern/library` rows on (Z)
against 4655 on (L), for the same 821 deduped names, i.e. roughly one extra ingest
pass on (Z) that gained no new patterns.

Three consequences worth stating plainly:

1. **"Usable replica" is the wrong model.** Nothing replicates. Two stores were
   forked from a common seed six weeks ago and have drifted under independent
   writes. Any plan that assumes one is a copy of the other will lose data in
   whichever direction it is applied.
2. **The direction of the fix is not obvious.** (Z) is not simply stale — it holds
   the `.merged-2026-08-17` work and 6 G more data. Overwriting (Z) from (L) would
   discard that; overwriting (L) from (Z) would discard whatever (L) has gained.
   Reconciliation needs a discovery pass on both before any copy is made.
3. **(Z)'s futon1b server is not running.** The 5876 figure was measured on
   2026-08-13 when it was up. Nothing on (Z) is currently serving :7073, so any
   Zone-side tool that reads the substrate over HTTP is either failing or reading
   something else — which is one more candidate explanation for the `151 / 42`
   discrepancy in §2 that should be checked before the others.

---

## 4. Coverage: most of the library has never been cited

| | |
|---|---|
| rows in `futon3/resources/sigils/patterns-index.tsv` | 1374 (+1 header) |
| `.flexiarg` files under `futon3/library/` | 1140 |
| …of those, "distinctive" under the detector's rule | 616 |
| distinct `pattern/library` names in the store (deduped) | 821 |
| **distinct patterns ever cited by a mission** | **297** |

So ~22% of the indexed library has ever been cited by a mission, under a detector
that can only see 616 of the 1140 flexiargs in the first place. Two separable
effects are tangled here and should not be conflated:

- **a real coverage gap** — most patterns genuinely have no mission citing them;
- **a detector ceiling** — the ≥2-hyphen / ≥12-char rule excludes 524 flexiargs
  from being detectable *at all*, so their citation count is structurally zero.

Neither number is trustworthy as a "pattern reuse" metric until they are separated.
This is the more interesting finding than the ARGUE count, and it is not yet
measured properly.

Most-cited patterns, by number of distinct missions citing them (snapshot):

```
  26  stop-the-line
  18  construct-an-explicit-witness
  17  structural-tension-as-observation
  16  expected-free-energy-scorecard
  15  logic-model-before-code
  15  reduce-to-known-result
  14  structured-observation-vector
  14  candidate-pattern-action-space
  12  evidence-over-assertion
  12  interest-event-vocabulary
  12  structured-events-only
  11  reachable-from-boot
  11  scope-bounded-handoff
  11  state-snapshot-witness
  10  learn-as-you-go
```

---

## 5. Open items

1. **Name the Zone-side scan** that produced `151 / 42`, and its mission checkout.
   Until then §2's discrepancy hypotheses stay hypotheses.
2. **Re-derive `mission-scope/pattern` edges from the surviving scope
   entities** (991 on (L)) into (L) and (Z). No re-detection needed — the scopes carry
   `:mission` and `:pattern/ident` in props. The binder's swap-not-add path is
   idempotent. This also relieves step 2 of `TN-pattern-duplication-findings.md` §6.
3. **Reconcile (L) and (Z) — as two divergent lines, not as replica-and-master**
   (§3a). Discovery pass on both first: what does `.merged-2026-08-17` contain,
   and what has each store gained since the common 2026-07-12 seed? No copy in
   either direction until that is answered. Deadline pressure: (L) is Dionysus,
   returned 2026-08-28.
4. **Bring (Z)'s futon1b server back up on :7073** and re-measure — nothing is
   serving it as of 2026-08-23.
5. **Separate the coverage gap from the detector ceiling** (§4) before anyone
   quotes a pattern-reuse percentage.
6. **Fix the `:entity/name` footgun** (§1) or document it at the query surface —
   it is a silent 934→376 undercount waiting to happen.

---

## 6. Repro

All of the following were run on the laptop, 2026-08-23, from `/home/joe/code`.

```bash
# --- live store: the missing edge layer (§3) ---
for t in "mission-scope/pattern" "mission-scope/concept" "mission-scope/source" \
         "pattern/has-sigil" "mission/doc"; do
  printf "%-24s " "$t"
  curl -s -m 15 "http://127.0.0.1:7073/api/alpha/census?type=$(
    python3 -c 'import urllib.parse,sys;print(urllib.parse.quote(sys.argv[1],safe=""))' "$t")"
  echo
done

# the export failure that caused it
cat futon1b/migration-export-full/export-summary.edn

# --- disk scan: phase headings and citations (§2) ---
# 1. segment each futon*/holes/missions/*.md on
#      (?mi)^\s*(?:#{1,6}\s*)?\*{0,2}(HEAD|IDENTIFY|MAP|DERIVE|ARGUE|VERIFY|INSTANTIATE|DOCUMENT)\b
# 2. collect basenames of futon*/library/**/*.flexiarg with >=2 hyphens and >=12 chars
# 3. match them as literals with (?<![A-Za-z0-9])NAME(?![A-Za-z0-9-])
# 4. count distinct matches per phase segment and over the whole file
#    -> 423 docs, 64 with an ARGUE heading, 190 citing, 257 distinct patterns,
#       68 distinct in ARGUE across 31 docs

# --- snapshot scan: the survey itself (§1, §2, §4) ---
# brace-depth scan over futon1b/migration-export-full/graph-snapshot.edn,
# select docs containing ":entity/type :scope/pattern" (934 of them), then read
# :entity/external-id, :pattern/ident, :mission, :anchor/passage, :anchor/state
# out of each. Group by (:mission, :pattern/ident) -- NOT by :entity/name.
#    -> 934 docs, 934 pairs, 176 missions, 297 patterns, 934/934 :anchored

# --- (L)/(Z) divergence: common seed, independent drift (§3a) ---
sha256sum ~/code/futon1b/migration-export-full/graph-snapshot.edn
ssh zone-joe 'sha256sum ~/code/futon1b/migration-export-full/graph-snapshot.edn'
du -sh ~/code/futon1b/*store*
ssh zone-joe 'du -sh ~/code/futon1b/*store*; ss -ltnp | grep -E "70[0-9][0-9]"'

# --- live scope layer (§1) ---
curl -s "http://127.0.0.1:7073/api/alpha/census?entity-type=scope/pattern"
#    -> {:type "scope/pattern", :kind :entity, :count 991}

# --- library sizes (§4) ---
wc -l futon3/resources/sigils/patterns-index.tsv
find futon3/library -name '*.flexiarg' | wc -l
find futon3/library -name '*.flexiarg' -printf '%f\n' | sed 's/\.flexiarg$//' \
  | awk 'length($0)>=12 && gsub(/-/,"-")>=2' | sort -u | wc -l
```

## 7. See also

- `futon3c/holes/missions/M-mission-scopes-into-substrate-2.md:260` — D1.3(a), the ingest that built this layer
- `futon1b/TN-pattern-duplication-findings.md` — the same substrates from the pattern-library side; (L)/(Z) divergence
- `futon6/scripts/mission_scope_detect.py` — the detector (`load_pattern_index:162`, phase aliases:50)
- `futon3c/src/futon3c/scripts/mission_scope_ingest.clj` — the `--binder pattern` ingest
