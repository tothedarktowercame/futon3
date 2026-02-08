# PUR: Pipeline + API Write Surface

context: wired API routes to pipeline and error mapping.
pattern: storage/canonical-interface

decision: map errors to HTTP responses; include /health and /write stubs.

outcome: API tests cover 403/400/200 scenarios.

evidence link: futon1a/test/futon1a/api/write_test.clj
