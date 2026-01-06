# Hypertext Demo Run (Phase 5)

Goal: run the devmap-first ingest to stage artifacts, anchors, and links in
FUTON3. This is staging-only; persistence and inference remain deferred, in
line with futon1 archivist guidance.

## Prereqs
- FUTON3 running with the hypertext store on disk (default log: futon3/logs/hypertext.edn)
- Scripts available in futon3/scripts

## Step 1: Register artifacts
Run the registration script to stage futon3 artifacts.

```
clojure -M -m scripts.register-stack
```

## Step 2: Extract devmap anchors
Extract anchors from `futon3/holes/*.devmap` (instantiated-by blocks only).

```
clojure -M -m scripts.extract-anchors
```

## Step 3: Suggest links from devmap references + org TODOs + docbooks + code scans
Scan devmaps for file references and suggest `:documents` links. Docbook
references are resolved via futon1's docbook endpoints (not the filesystem).
If docbook links are ingested properly, we can see in advance which docbooks
document which functions.
Scan org TODO lists for `:PATTERN:` metadata and suggest `:applies-pattern`
links. Then scan clojure + elisp forms to suggest `:uses` links based on the
CAR of each sexp.
This org TODO demo supports the human fubar contributor and provides a model
for future breakdown and planning workflows.

If Futon1 is running, the script also queries the pattern registry to add
`:applies-pattern` links from devmap titles, org `:PATTERN:` metadata, and any
`:pattern/id` literals found in code. Use `FUTON1_API_BASE` to point at the API
(default: `http://localhost:8080/api/alpha`).

```
clojure -M -m scripts.suggest-links
```

## Evidence
- Hypertext log: `futon3/logs/hypertext.edn`
- Store schema: `futon3/resources/schemas/hypertext.edn`
- Link type allowlist: `futon3/resources/schemas/hypertext-link-types.edn`
- Scripts: `futon3/scripts/register_stack.clj`, `futon3/scripts/extract_anchors.clj`, `futon3/scripts/suggest_links.clj`

## Notes
- This workflow stages data in futon3 only. It does not write to futon1.
- Future steps may export a commit-ready bundle for futon1 without changing
  the futon1 persistence model.

## Step 5.4: Review and proof chain
Review suggested links and build a proof chain that is ready for persistence.
Start with a human-approved pass, then mirror it with a machine-based pass.

Human-approved (Emacs-driven, generic):
- Open a review buffer listing `:suggested` links.
- Accept or reject each link and record rationale.
- Confirm that accepted links form a coherent chain.

Machine-mirrored (batch, generic):
- Re-run structural checks (anchors resolve, types are allowed).
- Apply the same accept/reject criteria in batch.
- Emit a summary report of accepted/rejected links and rationale.

QA demo script:
```
clojure -M -m scripts.review-links
```
Writes `futon3/logs/hx-review-report.edn` with accept/reject status and issues.
