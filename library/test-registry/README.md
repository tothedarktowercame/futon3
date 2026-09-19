# test-registry/ — patterns for run-once, warrant-forever test evidence

The registry lets a reviewer rely on an author's registered test run instead
of repeating it. The economy in one line (Joe, 2026-09-19): **tests that pass
against code that doesn't change become warrants and don't need to be rerun.**
Implementation: `futon3c/src/futon3c/test_registry.clj` (mint and check) and
`futon3c/src/futon3c/test_registry/validation.clj` (subject index,
revalidation queue, conformance readout; end-to-end entry point
`clojure -M -m futon3c.test-registry.validation register <spec.edn>`).

## The patterns, as the life of one warrant

1. **register-the-run** — the author's run is registered as a sha-chained
   warrant (manifests, environment fingerprint, log, parsed results); an
   unregistered run is a claim, not evidence.
2. **outlive-the-process** — the warrant AND any typed refusal land on the
   durable evidence backend; nothing lives only in the minting JVM.
3. **warrant-only-the-wires** — subjects come from the queriable wire
   register (producer→consumer seams, as instances or type-duties), never
   from test accretion; only needed behaviour is warranted.
4. **bind-the-subject** — an append-only subject→warrant index; incidents
   open revalidation, closed only by a currently-bound warrant minted after
   the incident; the readout is evidence, never a gate.
5. **warrant-rides-the-handoff** — handoffs carry base sha and warrant ids;
   acceptance binds the warrant.
6. **bind-warrant-to-the-diff** — the reviewer checks the warrant against the
   declared scope of the diff; typed refusals name the drifted fields.
7. **judge-adequacy** — the review is a recorded adequacy judgement (with the
   proposed adversarial-fixture discriminator), not a rerun.
8. **spot-check-at-the-declared-rate** — one recorded execution as a
   record-honesty check, not a correctness verdict.
9. **rerun-when-the-warrant-fails** — execution is required exactly when the
   warrant fails, a structural lane demands it, tests are new, or a
   revalidation is open; the rerun is registered and becomes the next warrant.
10. **meter-the-saving** — whether the registry pays for itself is measured,
    including "no saving".

## Standing rulings the patterns encode (Joe, 2026-09-19 unless noted)

- It is a **registry service, not a scanner**: no cron sweeps, no history
  backfill; ledgers start empty and fill forward at mint / observation /
  acceptance time (**fill-forward-not-sweep**).
- Warrants certify **only needed behaviour, required for running the
  machine** — the wires. The 27 ARGUE'd requirements are wire-types or
  wire-instances, reviewable and queriable
  (`futon2/scripts/wire_register.bb` over
  `futon2/holes/labs/wm-contract/wire-requirements.edn` and
  `p4ng/empirics-futon/aif-conformance.edn`).
- **Verification is registered, not rerun**: reviews consume warrants and
  receipts; a fresh execution is done by another agent and registered.
- Conformance readout is **evidence, never a gate** (guards carry the burden
  of proof; nothing here halts a machine run).
- Run certificates are **uniform across every run of the machine**, self-repair
  included; external repairs not based on a run mint no run-witnesses, because
  that would be fabrication
  (`futon2/holes/labs/wm-contract/RULING-run-certificate-uniformity-2026-09-19.md`).

## Where this sits in the tetrahedral model

The registry is owned by the **verbs** vertex of the project's tetrahedral
(Sierpinski) outcome model — the vetting question it answers is verbatim the
verbs question: *"do producers and consumers perform the claimed
operations?"* (p4ng sec-case-study-vetting; vertices ruled in
RULINGS-…-09-09 Item 18b). The wire register indexes the verbs; warrants
certify them. The contract artifacts both endpoints of a wire pin are
organisation-vertex objects; the ledger receipts are the evidence vertex —
the vertex recursion holds inside the service (its own refusal traces were
the missing evidence leg, closed 2026-09-19).

Higher-level patterns shared with the other upkeep policies are in
`../hygiene/`; general principles in `../apparatus/`.

## Status

Seven patterns drafted 2026-09-16 (claude-7) against a feature being built;
updated and extended (four new patterns) 2026-09-19 (claude-12) against the
running service: registered mints, four live subject bindings, and the
durable-backend + refusal-trace fix (futon3c 224da745).
