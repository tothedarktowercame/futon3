# Pattern Competence Pipeline (Fulab)

This pipeline records pattern competence in two linked records:
- PUR (Pattern Use Record): retrospective, auditable use of a pattern.
- PSR (Pattern Selection Record): prospective, auditable selection among patterns.

Records are stored as append-only events in `lab/sessions/<session-id>.edn` and
verified via `futon3.hx.logic` step kinds.

## Schemas (EDN examples)

### Anchor

```edn
{:anchor/type :code/edit
 :anchor/ref {:event/type :code/edit
              :file "src/f2/claude.clj"
              :fn "start-session!"}}
```

Anchors must resolve to at least one session `:events` entry.

### Pattern Use Record (PUR)

```edn
{:pur/id "pur-1"
 :session/id "claude-2026-01-01-001"
 :pattern/id "fulab/clock-in"
 :instance/id "pur-1-a"
 :fields {:context "..."
          :if "..."
          :however "..."
          :then "..."
          :because "..."
          :next-steps "..."}
 :anchors [{:anchor/type :code/edit
            :anchor/ref {:event/type :code/edit
                         :file "src/f2/claude.clj"
                         :fn "start-session!"}}]
 :decision/id "decision-1"}
```

Optional fields:
- `:outcome/tags` (keywords; see `resources/fulab-outcome-tags.edn`)
- `:delta` (before/after anchors)
- `:unresolved-tension` (string or {:ref :if/:however :note "..."})
- `:counterevidence` (vector of {:claim "..." :locus <anchor or {:locus/type ...}>})
- `:revision-proposal` ({:field <pur field> :edit "..." :support [anchors...]})

### Pattern Selection Record (PSR)

```edn
{:psr/id "psr-1"
 :session/id "claude-2026-01-01-001"
 :decision/id "decision-1"
 :candidates ["fulab/clock-in" "fulab/clock-out"]
 :chosen "fulab/clock-in"
 :context/anchors [<anchor>]
 :forecast {:benefits [{:tag :benefit/test :locus <anchor> :note "..."}]
            :risks []
            :success []
            :failure []}
 :rejections {"fulab/clock-out" {:codes [:reject/fit] :note "..."}}
 :horizon :short}
```

Optional:
- `:uncertainties` (vector of strings)
- `:override/solo?` and `:override/note` for one-candidate decisions

## Claim, Check, Report

```bash
# Write a PUR template
futon3/fulab-pattern-claim --session-id claude-2026-01-01-001 --kind pur

# Write a PSR template
futon3/fulab-pattern-claim --session-id claude-2026-01-01-001 --kind psr --decision-id decision-1

# Validate claims and append verification events
futon3/fulab-pattern-check --session-id claude-2026-01-01-001

# Report coverage
futon3/fulab-pattern-report --session-id claude-2026-01-01-001
```

Optional PSR draft helper (unverified):

```bash
futon3/fulab-pattern-suggest --session-id claude-2026-01-01-001 --decision-id decision-1
```

## hx.logic integration

New step kinds:
- `:hx/pattern-use-claimed`
- `:hx/pattern-use-verified`
- `:hx/pattern-selection-claimed`
- `:hx/pattern-selection-verified`

Validator groups:

PUR validators:
- V1 Traceability (required keys, pattern exists, field schema)
- V2 Anchors (each anchor resolves to session events)
- V3 Outcome tags (if present, must be in vocab)
- V4 Delta (if present, anchors resolve and differ)
- V5 Tension accounting (valid references)
- V6 Counterevidence (at least one resolvable locus)
- V7 Revision proposal (valid field + support anchors)

PSR validators:
- W1 Candidate integrity (chosen in candidates; patterns resolve)
- W2 Two-alternative rule (>=2 candidates unless override)
- W3 Fit anchors resolve
- W4 Forecast checkability (each entry has resolvable locus)
- W5 Rejection structure (rejected candidates have codes)

## Linking PUR to PSR

If a PUR includes `:decision/id` matching a PSR `:decision/id`,
`fulab-pattern-report` displays the linkage in the report.
