# Logic Checks (core.logic)

This repo has two core.logic kernels that emit structured witnesses and
obligations for rule enforcement.

Logic checks are recorded to `futon3/logs/logic_audit.edn`, and MUSN session
state responses include a `:logic/summary` keyed by session id. Hypertext
structural checks are scoped to the transport client id (set as `:session/id`
for auditing) so summaries remain per-client rather than global.
Plan (later): add optional `:session/id` to the `:hx/*` schemas and propagate it
through the transport/router so audits can use real session ids instead of the
client id placeholder.

## MUSN turn logic (`src/futon3/musn/logic.clj`)

Checked during `turn/action` and `turn/end`, surfaced as `:logic` in the
responses:

- Plan-before-tool: `implement`, `update`, `off-trail`, `wide-scan` require a plan.
- Selection-before-write: `implement` and `update` require a selection.
- Cost consent: `:expensive` and `:human-contact` actions require a plan.
- Off-trail budget: exceeding the trail limit adds a warning (no halt).
- Use-requires-evidence: if selection reason mode is `:use`, the turn must have
  evidence (`write?` set via action or evidence add).

Each check yields `:musn.logic/v1` witness facts and obligations such as
`:missing-plan`, `:missing-selection`, `:missing-consent`, `:off-trail`,
`:no-write`.

## Hypertext + pattern competence (`src/futon3/hx/logic.clj`)

Checked via `futon3.hx.validate/structural-step` and used by the
pattern-competence pipeline:

- Artifact register: `:artifact/id` present; optional policies require docs or
  pattern refs.
- Anchors upsert: artifact exists, anchor ids present/unique, selectors valid.
- Link suggest: from/to artifacts and anchors resolve, link type is allowed,
  duplicates rejected when policy says so.
- Link accept/reject: link exists and has an allowed status for that decision.
- PUR (pattern use claimed): required keys, known pattern id, field shape,
  anchor resolution, outcome tags, delta/tension/counterevidence/revision checks,
  certificates.
- PSR (pattern selection claimed): candidates/chosen, horizon value, override
  note if needed, context anchors, forecast entries resolved, rejections, AIF
  G-term provenance, certificates.

Each check yields a `:hx.logic/v1` witness with facts, errors, and obligations.
