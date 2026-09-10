# F12 contextual preparation adapter — build for independent review

Codex-16, 2026-09-10. Implements the six contextual instances accepted in
futon2 `runs/E-pre-go-live-independent-pair-review-2026-09-10.md` under
`holes/labs/wm-contract`. **Build and diagnostics only; no primary pair run.**
Checksum observations establish byte identity only, not proof validity,
complete historical evidence currency, activation or readiness.

## Delivered execution path

`f12_preparation.clj` constructs the same six-instance repository using the
existing `find-organise/organise`, selected-only closure, and the reviewed empty
authored relation pullback. It checks selected/authored/admission bindings and
O1–O3 on that contextual carrier, without conversion to parent IDs. The instance
population is serialized in `fixtures/f12-preparation/manifest.json` and must
match the code's IDs exactly. Parent source snapshots retain their THEN 23–24
provenance; the pinned reviewed five-parent basis has no outgoing why edges.
No canonical patterns or relation directives are changed.

Each transition calls `find-organise/fire`. F and S remain independently enabled
until their own receipt exists; D requires both. Actual returned firings and
command receipts determine acting order. D constructs one disposition; summary
and score tables dispatch over exactly the six-case enumeration. There is no
second readiness authority. The checks compare expected hashes taken from the
two frozen authority documents, never hashes inferred from their target bytes.

`f12_preparation_io.py` verifies frozen sources, code and runtime pins, then runs
the exact `/usr/bin/sha256sum --check --strict -` argv on a newly allocated private
copy of the selected target. Each receipt records the canonical logical stdin,
actual private stdin, explicit path mapping, bytes digest, stdout/stderr and
hashes, exit, elapsed time, environment and manifest hash. Core dumps are disabled;
only stdin/stdout/stderr are inherited. The five-second timeout kills/waits the
checksum process, with no automatic retry. Source drift after execution retains
the command receipt as refusal evidence. The Clojure loop propagates failures
with partial state/transcript in exception data, rather than fabricating D or
silently retrying.

The private copy changes the physical target pathname, deliberately. This is
an explicit interpreted mapping from the frozen authority's canonical identity
to byte-identical private input; both logical and physical payloads are retained.
It is not a claim that the checksum command opened the canonical path during
adapter execution. The independently reviewed diagnostic qualification did open
canonical targets read-only, as authorized.

## Actual binary I/O qualification

`F-diagnostic.json`, `S-diagnostic.json`, and corresponding `.strace` files under
`fixtures/f12-preparation` are diagnostic records, explicitly nonprimary.
Both returned match/exit 0. Recorded accesses were:

- exec `/usr/bin/sha256sum` with four clean environment variables;
- `/etc/ld.so.preload` readability probe returned ENOENT;
- reads of `/etc/ld.so.cache`, libcrypto.so.3 and libc.so.6;
- stdin and the single explicitly named canonical target;
- stdout only for writes, then exit. No children, network calls or file writes
  occur in the traced syscall classes.

Static ELF inspection additionally identifies the kernel-loaded interpreter
`/lib64/ld-linux-x86-64.so.2`. There was no observed OpenSSL config/provider read
or unanticipated source path. Trace classes were `%file,%process,read,write,network`;
this is an observed file/process census, not a claim of tracing every syscall.

Strace was absent. Downloaded Ubuntu strace 6.8-0ubuntu2 and libunwind8
1.6.2-3build1.1 packages and extracted them under `/tmp/f12-trace-tool`; no system
package installation. The initial tracer launch failed for missing
libunwind-ptrace before launching a checksum child. After private extraction,
the tracer used its own LD_LIBRARY_PATH and explicitly removed that variable
from the checksum child with `-E LD_LIBRARY_PATH`. Both diagnostic records retain
the setup failure attribution; it is not a primary rerun. These tools are
qualification-only and are not adapter subprocess dependencies.

## Frozen inputs and precise isolation limits

The manifest contains nine copied input/source files: find implementation,
its independent F11 manifest, risk binding proof, its certificate, and five
parent patterns. It additionally pins both adapter files and the actual shared
find_organise source; it pins the checksum executable, loader/cache/libraries,
Python executable, Babashka executable, and Python module/cache files observed
loaded during freezing. Python launches with `-I -S -B` to exclude user-site,
PYTHONPATH and site customization and avoid bytecode writes. The checksum child
receives exactly LC_ALL=C, LANG=C, TZ=UTC, OPENSSL_CONF=/dev/null. The manifest
records resolved runtime paths and rejects changed resolution/content or a new
loader preload file before/after every action.

Frozen copies are committed evidence files; each execution rechecks hashes and
copies bytes into a private temporary directory with a mode-0444 target. No
canonical source, ledger, registry or live data is written. This is **not a
kernel filesystem/network sandbox**. The host Python/Babashka harness, kernel,
CPU and transient timing are not a hermetically reproducible system image;
module hashes and observed loader paths do not establish every possible shared
library access on every host. No expensive whole-repository freeze is used.
Qualification is specific to the inspected host and binary. Unexpected access
in a subsequent approved diagnostic must be reported and reviewed, not silently
added or described as isolated. No wrapper is installed in a live service.

`freeze` intentionally creates a new proposed manifest from current canonical
inputs. It is a build operation, not an automatic repair: never invoke it to
resolve execution refusal on a reviewed attempt. Historical expected hashes
remain tied to the copied authorities, whose values are checked independently
by the adapter.

## Budget, denominators, controls and primary gate

Fixed budget remains one primary pair, two command invocations/arm, three
firings/arm, five seconds/command, no retry. A primary arm is one episode with
three transcript firings. A coherent match OR mismatch disposition is a completed
integrity answer; no score improvement is expected. Diagnostics always have
`primary? false` and `primary-score nil`. Test law-control numbers are explicitly
synthetic and cannot become primary scores.

`reviewed-pair` is provided for use after review, but was not called. It requires
an independently authored EDN review containing reviewer `codex-17`,
`approved-primary? true`, and the exact manifest SHA256. The manifest pins adapter
source hashes, so the approval binds the code too. The function executes the
same tested path and produces all six observed O4 fields, both instance and
parent-projected O4 results, and `closure-claimed? false`. It refuses missing or
failed primary evidence. There is no unattended primary CLI or automatic retry.
The review receipt is a workflow authority, not a cryptographic identity system;
this author has not supplied it. The reviewer controls when the single approved
pair is invoked; repeated calls would be new attempts, not silently added samples.

Example AFTER independent primary approval, from futon3:

```clojure
(require '[f12-preparation :as p])
(p/reviewed-pair "checks/fixtures/f12-preparation/manifest.json"
                 "/path/to/independent-primary-review.edn")
```

Retain the returned record (or refusal exception data) in a new attempt artifact;
never overwrite a prior attempt. The reviewed approval's primary flag differs
from the manifest's build-time `primary_authorized: false`: the former is the
explicit later authorization for these exact frozen bytes, not a self-repin.

## Tests and reproduction

From futon3:

```sh
bb -cp checks checks/f12_preparation_test.clj
/usr/bin/python3 checks/f12_preparation_io_test.py
clj-kondo --lint checks/f12_preparation.clj checks/f12_preparation_test.clj
emacs -Q --batch -l /home/joe/code/futon4/dev/check-parens.el --eval '(arxana-check-parens-cli)' -- checks/f12_preparation.clj checks/f12_preparation_test.clj
```

Clojure: 2 tests, 12 assertions, zero failures/errors. Python: 4 tests, all pass.
Tests run actual checksum commands for positive, changed-expected and missing-file
cases. They exercise actual organise/fire diagnostic arms, parser rejection,
input/runtime drift, symlink rejection, selected identity refusal, total consumer
coverage, flat-effect O4 rejection and equal parent projection. Lint reports zero
errors/warnings; parentheses, Python compilation and scoped diff checks pass.
No Lean build or primary comparison was run. Test primary scores remain absent.

Remaining review decision is concrete: approve these bytes/path mapping and
host-runtime limits or identify a code finding. No further task-purpose choice
or broad freeze is proposed. After approval the gated function can execute the
single primary pair directly; it still cannot discharge universal organise
conformance or the implementation refusal by itself.
