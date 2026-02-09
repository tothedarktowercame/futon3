# Mission: F6 Exploration and Ingest (Superpod Run)

Process math.stackexchange through the full NLP pipeline on Rob's superpod,
producing the annotated corpus that feeds all subsequent stages.

## Owner

Joe + Rob (superpod access)

## Scope

### Scope In

- Ship NER kernel + superpod-job.py + math.SE dump to Rob's machine
- Execute the 4-stage pipeline: parse, embeddings, LLM pattern tagging, clustering
- Add Stage 5: classical NER term spotting + scope detection using the kernel
- Retrieve output blob and validate locally
- Bootstrap loop: absorb math.SE tags into kernel, re-run term spotter

### Scope Out

- Building the evaluation framework (that's M-f6-eval)
- Agent infrastructure (that's M-f6-agents)
- Arxiv processing (that's M-f6-arxiv)

## Time Box

Superpod window: 2-4 hours for the run itself. Preparation: 1-2 sessions.
Post-processing: 1 session.

## Prerequisites

- [x] Streaming XML parser handles multi-GB dumps (tested on physics.SE)
- [x] NER kernel: 19,236 terms from 63 PlanetMath MSC repos + SE tags
- [x] Superpod batch job: scripts/superpod-job.py (4-stage, self-contained)
- [x] Pattern tagger: 25 math-informal patterns with hotword lists
- [x] Classical term spotter: scripts/spot-terms.bb
- [ ] Stage 5 integrated into superpod-job.py (NER + scope detection)
- [ ] math.stackexchange dump (3.4GB) accessible
- [ ] Rob confirms superpod availability and Llama-3 access

## Work Plan

### Outpost O-1: Pipeline Dry Run

Run the full 5-stage pipeline locally on existing physics.SE data before
touching the superpod. This proves the pipeline works end-to-end and
validates Stage 5 (NER + scope detection) on real data.

**Data**: physics.SE subset (first 1K pairs from `data/se-physics.json`)
**Flags**: `--skip-embeddings --skip-llm` (CPU-only stages)
**Validates**:
- [ ] Stage 5 NER term spotting produces `ner-terms.json` with >95% entity coverage
- [ ] Stage 5 scope detection finds Let/Define openers in >30% of answers
- [ ] Output manifest.json is well-formed and matches expected entity count
- [ ] Pipeline completes in <10 minutes on local hardware

**Depends on**: Nothing beyond what already exists (physics.SE data + NER kernel)
**Feeds**: Confidence to ship to Rob; bug fixes before they cost superpod time

**Pattern references**: `f6/bootstrap-loop`, `f6/scope-chain-tracking`

---

### Phase 1: Pre-flight (before superpod access)

1. **Add Stage 5** to superpod-job.py: classical NER + scope detection
   - Read NER kernel (terms.tsv) shipped alongside the dump
   - Spot terms in each QA pair (CPU, fast — same algorithm as spot-terms.bb)
   - Detect scope openers (Let/Define/Assume/Consider patterns from scope-patterns.edn)
   - Extract LaTeX symbol bindings from scope openers
   - Output: `ner-terms.json` (per-entity term hits) + `scopes.json` (binding records)

2. **Package the job** as a self-contained tarball:
   - `superpod-job.py` + `src/futon6/stackexchange.py`
   - `data/ner-kernel/` (terms.tsv + scope-patterns.edn)
   - `pyproject.toml` for dependency install
   - README with exact setup commands

3. **Test locally** on physics.SE subset (first 10K pairs, --skip-embeddings --skip-llm)

### Phase 2: Superpod run

4. Upload blob to superpod, extract math.SE dump
5. `pip install -e ".[gpu]"` + verify torch sees GPUs
6. Run full pipeline:
   ```
   python scripts/superpod-job.py ./math-se-raw/Posts.xml \
     --output-dir ./math-se-processed \
     --llm-model meta-llama/Meta-Llama-3-8B-Instruct \
     --site math.stackexchange
   ```
7. Monitor: expect ~200K+ QA pairs, 2-4 hours total

### Phase 3: Post-processing

8. Download output blob
9. Validate: check manifest.json, spot-check pattern tags, verify embeddings shape
10. **Bootstrap loop**: extract math.SE tags from output, merge into NER kernel,
    re-run term spotter on the processed data to measure coverage improvement
11. Update kernel: `bb scripts/build-ner-kernel.bb --se-json data/se-math.json`
12. Commit expanded kernel and results

## Expected Outputs

| Artifact | Description | Est. Size |
|----------|-------------|-----------|
| entities.json | QA pairs in entity format | ~1 GB |
| relations.json | Tag, score, answer-to-question relations | ~200 MB |
| embeddings.npy | bge-large-en-v1.5, 1024d per entity | ~800 MB |
| pattern-tags.json | LLM-assigned informal reasoning patterns | ~100 MB |
| clusters.json | HDBSCAN cluster assignments | ~20 MB |
| ner-terms.json | Per-entity term hits from NER kernel | ~200 MB |
| scopes.json | Scope/binding records (Let/Define openers) | ~50 MB |
| manifest.json | Run metadata, stats, config | ~10 KB |

## Success Criteria

- [ ] math.SE dump fully parsed (expect 200K+ QA pairs)
- [ ] All 25 patterns fire on math.SE data
- [ ] Embeddings computed for all entities
- [ ] NER term spotting achieves >95% entity coverage
- [ ] Scope detection finds Let/Define openers in >30% of answers
- [ ] Output blob downloads and validates locally
- [ ] Expanded NER kernel incorporates math.SE tags (expect 1000+ new terms)
- [ ] Total pipeline completes within 4-hour superpod window

## Failure Modes

| Failure | Mitigation |
|---------|------------|
| OOM on superpod | Shouldn't happen with 2TB RAM; streaming parser is constant-memory |
| Llama-3 not available | Use --skip-llm, run pattern tagging later or use classical tagger |
| Network issues during upload | Compress aggressively; math.SE dump is ~3.4GB → ~700MB compressed |
| Stage 5 too slow | NER spotting is CPU-bound; parallelize with pmap or multiprocessing |

## Connects To

- **Feeds**: M-f6-eval (the annotated corpus is the evaluation target)
- **Feeds**: M-f6-agents (agents need the knowledge graph to reason about)
- **Feeds**: M-f6-arxiv (establishes the pipeline pattern that Arxiv reuses)
- **Devmap**: f6/P7 (StackExchange Import)
- **Grant lineage**: AI4CI Month 1 (Corpus Preparation), LEAPQA T1-translation

## Context

This mission realises the corpus preparation phase that appears in every grant
proposal from 2015 to 2026. The difference: we have working infrastructure,
a 19K-term NER kernel, and free superpod access. The £400/month hobbyist
budget replaces the £49K AI4CI ask and delivers more because there's no
permission latency.
