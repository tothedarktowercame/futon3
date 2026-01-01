#!/usr/bin/env python3
"""Ensure flexiarg/multiarg patterns have @sigils entries."""
from __future__ import annotations

import re
from pathlib import Path
from typing import Dict, List, Optional

SIGIL_BLOCK_RE = re.compile(r"\[[^\]]*[^ \[\]]+/[^ \[\]]+[^\]]*\]")
ARG_RE = re.compile(r"^@arg\s+(\S+)\s*$")

ROOTS = [Path("library"), Path("holes/LDTS")]


def load_or_sigils() -> Dict[str, str]:
    path = Path("library/or/or.flexiarg")
    if not path.exists():
        return {}
    lines = path.read_text().splitlines()
    mapping: Dict[str, str] = {}
    current: Optional[str] = None
    buffer: List[str] = []
    for line in lines + ["@arg EOF"]:
        m = ARG_RE.match(line)
        if m:
            if current and buffer:
                for chunk in buffer:
                    match = SIGIL_BLOCK_RE.search(chunk)
                    if match:
                        mapping[current] = match.group(0)
                        break
            current = m.group(1)
            buffer = []
        else:
            if current is not None:
                buffer.append(line)
    return mapping


OR_SIGILS = load_or_sigils()

MANUAL_SIGILS: Dict[str, str] = {
    # p4ng-agent-environments
    "p4ng/institutional-drift": "[🧭/界]",
    "p4ng/norm-seed": "[🌱/序]",
    "p4ng/boundary-oscillation": "[🌲/门]",
    "p4ng/reflection-scaffold-upgrade": "[🪞/理]",
    "p4ng/proportional-load-sharing": "[⚖️/义]",
    "p4ng/legitimate-iteration": "[🧪/律]",
    "p4ng/reflect-in-layers": "[🧭/心]",
    "p4ng/disruption-traceback": "[🔦/公]",
    "p4ng/pattern-dispute-dialogue": "[💬/口]",
    "p4ng/self-patterning-mandate": "[🙌/己]",
    # p4ng-agents
    "p4ng/meta-reflection-loop-agent": "[🪞/理]",
    "p4ng/timebox-the-core-agent": "[⏱️/程]",
    "p4ng/role-reveal-agent": "[👥/仁]",
    "p4ng/pattern-activation": "[🧩/工]",
    "p4ng/tension-detection": "[⚖️/态]",
    "p4ng/candidate-move-generation": "[🧮/索]",
    "p4ng/shared-memory-architecture": "[🧵/络]",
    "p4ng/feedback-rhythms": "[🔁/更]",
    "p4ng/pattern-diffusion": "[📡/径]",
    # p4ng-simulation
    "p4ng/timebox-the-core-agent-prime": "[⏱️/程]",
    "p4ng/outcome-synthesizer": "[🧮/索]",
    "p4ng/call-a-synthesizer": "[🧩/工]",
    "p4ng/co-design-partners-agent": "[🤝/群]",
    # p4ng-workshops
    "p4ng/start-with-sense": "[👂/人]",
    "p4ng/name-the-frame": "[🧭/纲]",
    "p4ng/borrow-the-situation": "[🧩/用]",
    "p4ng/pattern-priming": "[🌱/序]",
    "p4ng/timebox-the-core": "[⏱️/程]",
    "p4ng/role-reveal": "[👥/仁]",
    "p4ng/harvest-before-close": "[📤/文]",
    "p4ng/pattern-the-play": "[🎶/人]",
    "p4ng/meta-reflection-loop": "[🪞/理]",
}


def find_inline_sigils(lines: List[str]) -> Optional[str]:
    for line in lines:
        match = SIGIL_BLOCK_RE.search(line)
        if match:
            return match.group(0)
    return None


def infer_sigils(arg_id: str, block_lines: List[str]) -> Optional[str]:
    if arg_id in MANUAL_SIGILS:
        return MANUAL_SIGILS[arg_id]
    if arg_id.startswith("p4ng/"):
        counterpart = "or/" + arg_id.split("/", 1)[1]
        if counterpart in OR_SIGILS:
            return OR_SIGILS[counterpart]
    return find_inline_sigils(block_lines)


def update_file(path: Path) -> bool:
    lines = path.read_text().splitlines()
    updated: List[str] = []
    changed = False
    idx = 0
    while idx < len(lines):
        line = lines[idx]
        m = ARG_RE.match(line)
        if not m:
            updated.append(line)
            idx += 1
            continue
        arg_id = m.group(1)
        block_lines = [line]
        idx += 1
        while idx < len(lines) and not ARG_RE.match(lines[idx]):
            block_lines.append(lines[idx])
            idx += 1
        has_sigils = any(l.startswith("@sigils") for l in block_lines)
        if not has_sigils:
            sigils = infer_sigils(arg_id, block_lines)
            if sigils:
                updated.append(line)
                updated.append(f"@sigils {sigils}")
                updated.extend(block_lines[1:])
                changed = True
                continue
        updated.extend(block_lines)
    if changed:
        path.write_text("\n".join(updated) + "\n")
    return changed


def main() -> None:
    candidates = []
    for root in ROOTS:
        if not root.exists():
            continue
        candidates.extend([p for p in root.rglob("*")
                           if p.is_file() and p.suffix.lower() in {".flexiarg", ".multiarg"}])

    changed = 0
    for path in candidates:
        if update_file(path):
            changed += 1
    print(f"Updated {changed} files.")


if __name__ == "__main__":
    main()
