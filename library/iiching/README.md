# iiching

This directory encodes the 256 8-bit exotypes (0-255) as explicit, named
patterns. The goal is to make exotype knowledge concrete and inspectable,
separate from the theory in `library/exotic`.

Naming and encoding
- Files use `exotype-XYZ.flexiarg` where `XYZ` is a zero-padded decimal id.
- Each exotype records its 8-bit string, hex value, and hamming weight.
- Bit order is MSB->LSB (b7..b0). For example, `0x80` is `10000000`.

Workflow
- Start by adding or updating `exotype-XYZ.flexiarg` entries.
- When semantics or parameter mappings are pulled from futon5, record the
  source path in `@futon5-ref`, keep the canonical futon5 paths in
  `@futon5-manifest-path` and `@futon5-lift-path`, and fill `@exotype-program`,
  `@exotype-lift`, and `@exotype-params`.
- If semantics are not yet known, mark the entry `blocked-by[futon5-exotypes]`
  in `NEXT-STEPS`.

Template
- Use `TEMPLATE.flexiarg` for new entries.
