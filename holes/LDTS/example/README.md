# Flexiarg Examples

This directory holds textual sources for the `talk4real` argument set using `flexiarg-mode`.
Each `*.flexiarg` file corresponds to a section (executive summary, rationale, etc.) and includes
metadata (`@arg`, `@audience`, `@up`, `@next`, …) plus the argument tree written with the
Fountain-style markers (`!`, `+`, `?`).

## Editing and regenerating EDN

1. Open a `.flexiarg` file in Emacs.
2. Enable `flexiarg-mode` (`M-x flexiarg-mode`).
3. Run `M-x flexiarg-show-edn` to render the buffer as the EDN structure used by `compile-summary`.
4. Copy the EDN output into the aggregate file `example/specs.edn` (it is a vector of maps).
   - If you edit multiple sections, regenerate each EDN block and replace the corresponding map.
5. Run any of the CLI helpers, e.g. `clojure -M -m compile-summary prompt t4r/exec-summary-tree`, to
   confirm that the new data loads correctly.

`compile-summary.clj` now reads from `example/specs.edn`, so updating that file (via the steps above)
keeps the command-line pipeline in sync with the flexiarg sources.

## Graph navigation metadata

Two optional directives connect argument blocks:

- `@up <id>` – identifies the parent argument (the block this section supports).
- `@next <id>` – names the sibling or subsequent block to read next.

These keys surface as `:up` and `:next` in the EDN, making it easy to traverse the whole document or
build custom outlines.
