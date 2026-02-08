# PSR: Pipeline + API Write Surface

context: need an end-to-end stub write path to prove layer ordering.
pattern: storage/canonical-interface

decision: implement core pipeline and API routes that call it.

alternatives:
- direct calls from tests (rejected: no API surface)

outcome: API write handler returns tx-id or structured error by layer.

evidence link: futon1a/src/futon1a/core/pipeline.clj
