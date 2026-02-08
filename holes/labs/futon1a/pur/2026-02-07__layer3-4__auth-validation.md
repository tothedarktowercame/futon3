# PUR: Layers 3–4 Auth + Validation

context: implemented penholder authorization and model validation.
pattern: storage/error-layer-hierarchy

decision: enforce Layer 4 validation before Layer 3 auth in pipeline.

outcome: pipeline ordering tests confirm L4 → L3 precedence.

evidence link: futon1a/test/futon1a/cross_layer/pipeline_order_test.clj
