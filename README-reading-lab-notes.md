# Reading lab notes (Arxana interface)

Futon3 owns the lab notebook data (raw JSON, stubs, traces) under `lab/`.
The Emacs UI for browsing those records lives in futon4 as part of the
Arxana client:

- `futon4/dev/arxana-lab.el` loads and parses lab sessions.
- `futon4/dev/arxana-browser-lab.el` wires lab entries into the browser UI.

The `fulab-export-musn` wrapper runs `dev/lab-export-musn.clj` and can list
available `lab/musn/*.edn` sessions before exporting.

By default the interface auto-detects a `lab/` directory by walking upward
from the current file/buffer. If the lab data live elsewhere, set
`arxana-lab-root` to the desired path.

Arxana can read `futon3/lab` directly, keeping Futon3 as the owner of lab
records while Futon4 owns the Arxana interface for reading them.
