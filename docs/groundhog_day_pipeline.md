# Groundhog Day transport pipeline (draft)

Source assets live under `../futon5/resources/demos/`:
- `groundhog_day.prompt` – system instructions for the deterministic storyteller.
- `groundhog_day_input.txt` – editable user brief describing the MUSN beat order.
- `groundhog_day_raw.json` – captured ChatGPT completion (run once without
  `--dry-run`).

## Conversion plan

1. **Transcript → NDJSON**
   - Add a helper (`scripts/groundhog_day_ingest.clj`) that reads
     `groundhog_day_raw.json`, extracts the `assistant` content, and emits a
     deterministic list of MUSN frames (hello/event/workday/check/bye) under
     `dev/groundhog_day.ndjson`.
   - Each sentence becomes either an `event` payload or a `workday` submission;
     timestamps in parentheses drive the `:t` fields.
2. **Replay script**
   - Create `scripts/run_groundhog_day.sh` that starts Futon3 with the Futon1
     adapter enabled, streams `dev/groundhog_day.ndjson` into `/musn/ingest`, and
     captures the replies for regression.
3. **Verification**
   - After ingestion, query Futon1 (`curl /api/α/entity ...`) to confirm the
     proof/workday nodes exist. Optionally run `clojure -M:groundhog/test` to
     assert the stored EDN matches fixtures under `resources/demos/`.
4. **Tutorial**
   - Document the workflow in Futon3’s README and cross-link to Futon5 so the
     provenance of the transcript is clear.

Once the adapter and scripts exist we can add a `make groundhog` target that
runs the full loop in CI (using mocked HTTP for Futon1).
