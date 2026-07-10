# data-mining — patterns for LLM-over-corpus mining pipelines

Lessons distilled from the futon6 meme-mining (forward: turns → memes/methods) and goals-and-holes
(backward: turns → C-entries/belly) runs on a rented GPU box, 2026-06-25/26. They generalize to any
pipeline that runs a model over a large corpus to extract structured records.

## Quality — get the prompt right before you scale
*the prompt is the product; verify it cheaply and independently before you scale.*

1. **[classify-over-one-clean-source](classify-over-one-clean-source.flexiarg)** — first, know your corpus
   is clean. A shared source commingling provenance needs ONE shared classifier, not per-consumer filters
   that drift (a bell-phrased-as-request leaked 7% into the forward pass).
2. **[constrain-extraction-to-the-downstream-vocabulary](constrain-extraction-to-the-downstream-vocabulary.flexiarg)**
   — write the prompt so each field maps to the consumer's vocabulary (allowlist, not blocklist) and the
   rare/interesting label DEFAULTS to the common case (or it over-fires: 95% new_patterns, 88% corrections).
3. **[golden-is-curated-not-raw](golden-is-curated-not-raw.flexiarg)** — prime with a hand-verified,
   balanced golden: positives PLUS labeled negatives. Never feed raw (half-bad) output back as golden.
4. **[gates-as-code](gates-as-code.flexiarg)** — encode the quality bands as a runnable, shape-tolerant
   checker authored independently of the producer. It reviews the output AND the golden (author ≠ reviewer).
5. **[smoke-before-the-paid-run](smoke-before-the-paid-run.flexiarg)** — validate the prompt on a small
   held-out real-backend sample against the gates before paying for the whole corpus. Stub ≠ prompt test.

Tighten the instruction (heavy lifting) AND prime with curated negatives (clears the gate); gate the data
AND the golden; smoke before you spend; and remember a code-fix does not clean already-produced data
(re-run or post-filter).

## Execution — once the prompt is right, run the corpus fast, durably, and gated
*the GPU work is the irreplaceable thing; saturate it, never lose it, and judge it early.*

6. **[saturate-the-accelerator-with-concurrency](saturate-the-accelerator-with-concurrency.flexiarg)** —
   mine N items at once (thread pool / async); the server continuous-batches them. Sequential left the GPU
   idle (`Running: 1 reqs`); 8-way fan-out turned a 12-15 h run into ~2 h on the SAME box. Usually the
   single biggest throughput lever.
7. **[checkpoint-the-long-run](checkpoint-the-long-run.flexiarg)** — flush every N records (since-last
   counter, not modulo), normalize nullable model fields, build the record inside the per-item try. A late
   crash once lost ~400 GPU calls held only in memory; durability bounds loss to the last N.
8. **[the-first-checkpoint-is-a-quality-probe](the-first-checkpoint-is-a-quality-probe.flexiarg)** — once
   fast and checkpointed, run the FULL corpus and gate the first checkpoint (~200-400 records); ride to
   completion on pass, bail there on fail. Dissolves the "cap to be safe vs. process everything" dilemma.

## Training / experiment runs — when the paid run is an ablation, not an extraction
*added 2026-07-01 from E-fold-embed-pipeline (a torch GNN ablation, not LLM mining). The mining patterns
above transfer, but a comparison-of-arms run has two extra failure modes: it can be un-attributable, and it
can waste a multi-GPU box. A worked application: applying gates-as-code + smoke-before-the-paid-run to that
run's dataset — via `futon6/scripts/fold_embed/check_fold_embed_gates.py`, torch-free, on the laptop —
caught a showstopper (every training pair had an empty cascade → the ablation would have been null) and a
28% dead-edge issue, for $0, before any box was rented.*

9. **[seed-the-run-for-a-fair-ablation](seed-the-run-for-a-fair-ablation.flexiarg)** — an ablation's value is
   attributing a metric delta to the arm; seed ALL randomness (torch + numpy + python `random`), hold it
   fixed across arms, ideally sweep K seeds and report mean±sd. An unseeded ablation measures init noise, not
   the arm.
10. **[fan-out-independent-runs-across-devices](fan-out-independent-runs-across-devices.flexiarg)** — the
    training sibling of saturate-the-accelerator: right-size the box to the model, then pin each independent
    arm/seed to its own GPU (`CUDA_VISIBLE_DEVICES=i`) and run them concurrently. A 1-GPU-sized job looping
    3 arms on a 4-GPU box leaves ¾ of the rental idle; fanning out is a ~k× wall-clock (cost) cut.
