#!/usr/bin/env python3
"""Generate holes/artifacts/devmaps.tex from futon*.devmap sources."""

from __future__ import annotations

import argparse
import re
from collections import defaultdict
from pathlib import Path

FLEXI_TAGS = {
    "context",
    "if",
    "however",
    "then",
    "because",
    "next",
    "next-evidence",
    "evidence",
}
FLEXI_RE = re.compile(r"^(?P<indent>\s*)\+\s*(?P<tag>[A-Za-z-]+):(?P<rest>.*)")


SPECIALS = {
    "\\": r"\textbackslash{}",
    "&": r"\&",
    "%": r"\%",
    "$": r"\$",
    "#": r"\#",
    "_": r"\_",
    "{": r"\{",
    "}": r"\}",
    "~": r"\textasciitilde{}",
    "^": r"\textasciicircum{}",
}


def normalize_quotes(text: str) -> str:
    result: list[str] = []
    backtick_open = False
    for ch in text:
        if ch == "`":
            if backtick_open:
                result.append("\u2019")
            else:
                result.append("\u2018")
            backtick_open = not backtick_open
        elif ch == "'":
            result.append("\u2019")
        else:
            result.append(ch)
    if backtick_open:
        result.append("\u2019")
    return "".join(result)


def needs_emoji_font(ch: str) -> bool:
    code = ord(ch)
    if ch == "\n":
        return False
    if code == 0xFE0F:
        return True
    if 0x1F000 <= code <= 0x1FAFF:
        return True
    if 0x2300 <= code <= 0x23FF:
        return True
    if 0x2600 <= code <= 0x27BF:
        return True
    if code == 0x3030:
        return True
    return False


def escape_line(line: str) -> str:
    result: list[str] = []
    i = 0
    while i < len(line):
        if line[i] == "’" and i + 1 < len(line) and line[i + 1] == ",":
            result.append(r"\tightquotecomma{}")
            i += 2
            continue
        ch = line[i]
        result.append(SPECIALS.get(ch, ch))
        i += 1
    return "".join(result)


def build_block(title: str, content: str, emoji_chars: set[str]) -> str:
    lines: list[str] = []
    raw_lines = content.splitlines()
    in_ifr = False
    i = 0
    total = len(raw_lines)
    while i < total:
        raw_line = raw_lines[i]
        for ch in raw_line:
            if ord(ch) >= 128 and needs_emoji_font(ch):
                emoji_chars.add(ch)
        stripped = raw_line.strip()
        if not stripped:
            in_ifr = False
            j = i + 1
            skip_par = False
            while j < total:
                candidate = raw_lines[j]
                if candidate.strip():
                    cand_body = candidate.lstrip()
                    cand_match = FLEXI_RE.match(cand_body)
                    if cand_match and cand_match.group("tag").lower() in FLEXI_TAGS:
                        skip_par = True
                    break
                j += 1
            if not skip_par:
                lines.append("\\par")
            i += 1
            continue
        indent = raw_line[: len(raw_line) - len(raw_line.lstrip())]
        body = raw_line[len(indent) :]
        body_lower = body.lstrip().lower()

        match = FLEXI_RE.match(body)
        if match and match.group("tag").lower() in FLEXI_TAGS:
            tag = match.group("tag").lower()
            rest = match.group("rest").lstrip()
            if not rest.strip():
                consumed = 0
                j = i + 1
                collected: list[str] = []
                while j < total:
                    look = raw_lines[j]
                    look_stripped = look.strip()
                    if not look_stripped:
                        break
                    look_body = look.lstrip()
                    look_lower = look_body.lower()
                    look_body_stripped = look_body.lstrip()
                    if (
                        look_body.startswith("+")
                        or look_lower.startswith("@")
                        or look_lower.startswith("! instantiated-by:")
                        or look_body_stripped.startswith("-")
                    ):
                        break
                    collected.append(look_body.strip())
                    consumed += 1
                    for ch in raw_lines[j]:
                        if ord(ch) >= 128 and needs_emoji_font(ch):
                            emoji_chars.add(ch)
                    j += 1
                if collected:
                    rest = " " + " ".join(collected)
                    i += consumed
            rest = normalize_quotes(rest)
            escaped_rest = escape_line(rest)
            lines.append(f"{indent}\\flexitag{{+{tag}:}}{escaped_rest}\\\\")
            i += 1
            continue

        code_candidate = body.lstrip()
        if code_candidate and code_candidate[0] in "{:[]}":
            lines.append(f"{indent}\\verb|{code_candidate}|\\\\")
            i += 1
            continue

        normalized_body = normalize_quotes(body)
        escaped = escape_line(normalized_body)
        make_bold = False
        if body_lower.startswith("@ifr:"):
            in_ifr = True
            make_bold = True
        elif in_ifr:
            make_bold = True
        if body_lower.startswith("! instantiated-by:"):
            make_bold = True
        if make_bold:
            escaped = rf"\textbf{{{escaped}}}"
        lines.append(f"{indent}{escaped}\\\\")
        i += 1

    block_lines = [f"% {title}"]
    block_lines.append(rf"\textbf{{{title}}}\par")
    block_lines.append("\\medskip")
    block_lines.extend(lines)
    return "\n".join(block_lines)


def extract_dependencies(current_id: int, text: str, edge_map, node_labels) -> None:
    proto_label: str | None = None
    for raw_line in text.splitlines():
        proto_match = re.match(r"!\s+instantiated-by: Prototype\s+(\d+)", raw_line)
        if proto_match:
            proto_label = f"P{proto_match.group(1)}"
        for match in re.findall(r"futon(\d+)", raw_line, flags=re.IGNORECASE):
            other = int(match)
            if other == current_id:
                continue
            edge_map[current_id][other].add(proto_label or "context")
            node_labels.setdefault(other, f"Futon{other}")


def write_mermaid(node_labels, edge_map, output_path: Path) -> None:
    lines = [
        "%%{init: { 'themeVariables': { 'fontSize': '26px', 'labelTextSize': '24px' } }}%%",
        "graph LR",
    ]
    for fid in sorted(node_labels):
        title = node_labels[fid]
        lines.append(f"  f{fid}[\"Futon {fid} — {title}\"]")
    for src in sorted(edge_map):
        for dst in sorted(edge_map[src]):
            labels = sorted(edge_map[src][dst])
            if labels:
                label = ",".join(labels[:3])
                if len(labels) > 3:
                    label += ",…"
            else:
                label = "context"
            lines.append(f"  f{src} -->|{label}| f{dst}")
    output_path.write_text("\n".join(lines) + "\n")
    print(f"Wrote {output_path}")


def main() -> None:
    root = Path(__file__).resolve().parents[1]
    default_output = root / "holes" / "artifacts" / "devmaps.tex"

    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--output",
        type=Path,
        default=default_output,
        help=f"Path to write the LaTeX file (default: {default_output})",
    )
    args = parser.parse_args()

    holes_dir = root / "holes"
    devmap_paths = [holes_dir / f"futon{i}.devmap" for i in range(0, 8)]
    missing = [str(p) for p in devmap_paths if not p.exists()]
    if missing:
        raise SystemExit("Missing devmap(s): " + ", ".join(missing))

    emoji_chars: set[str] = set()
    blocks: list[str] = []
    node_labels: dict[int, str] = {}
    edge_map = defaultdict(lambda: defaultdict(set))

    devmap_texts = []
    for path in devmap_paths:
        text = path.read_text()
        devmap_texts.append((path, text))

    for path, text in devmap_texts:
        futon_id = int(path.stem.replace("futon", ""))
        title_match = re.search(r"@title\s+(.*)", text)
        if title_match:
            node_labels[futon_id] = title_match.group(1).strip()
        else:
            node_labels[futon_id] = path.stem.upper()
        extract_dependencies(futon_id, text, edge_map, node_labels)
        blocks.append(build_block(path.stem.upper(), text, emoji_chars))

    header = r"""\documentclass[landscape]{article}
\usepackage[a3paper,margin=1cm]{geometry}
\usepackage{fontspec}
\usepackage{xcolor}
\usepackage{microtype}
\usepackage{newunicodechar}
\usepackage{paracol}
\defaultfontfeatures{Ligatures=TeX,Scale=MatchLowercase}
\setmainfont{Noto Sans CJK SC}
\setsansfont{Noto Sans CJK SC}
\setmonofont{Noto Sans Mono CJK SC}
\newfontfamily\EmojiFont{Noto Color Emoji}[Renderer=Harfbuzz]
\newcommand{\Emoji}[1]{{\EmojiFont #1}}
\setlength{\columnsep}{0.6cm}
\setlength{\columnseprule}{0.2pt}
\setlength{\parindent}{0pt}
\setlength{\parskip}{0.1\baselineskip}
\setlength{\emergencystretch}{3em}
\tolerance=2000
\hyphenpenalty=200
\pretolerance=100
\microtypesetup{protrusion=true,expansion=true}
\newcommand{\flexitag}[1]{\textcolor[gray]{0.35}{\textit{#1}}}
\newcommand{\tightquotecomma}{\textquoteright\kern-0.08em,}
"""

    emoji_lines = []
    for ch in sorted(emoji_chars, key=lambda c: ord(c)):
        code = ord(ch)
        if code == 0xFE0F:
            replacement = ""
        else:
            replacement = r"\Emoji{" + ch + "}"
        emoji_lines.append(f"\\newunicodechar{{{ch}}}{{{replacement}}}")

    extra_unicode = [
        ("\u202F", r"\,"),
        ("▸", r"\ensuremath{\triangleright}"),
    ]
    for ch, repl in extra_unicode:
        emoji_lines.append(f"\\newunicodechar{{{ch}}}{{{repl}}}")

    body_intro = "\\begin{document}\n\\scriptsize\\sffamily\n\\begin{paracol}{8}\n"
    footer = "\\end{paracol}\n\\end{document}\n"

    body_lines: list[str] = []
    for idx, block in enumerate(blocks):
        if idx:
            body_lines.append("\\switchcolumn")
        body_lines.append(block)

    content = "\n".join(
        [header] + emoji_lines + [body_intro] + body_lines + ["", footer]
    )

    output_path = args.output
    output_path.parent.mkdir(parents=True, exist_ok=True)
    output_path.write_text(content)
    print(f"Wrote {output_path} (emoji entries: {len(emoji_lines)})")

    mermaid_path = output_path.parent / "devmap-deps.mmd"
    write_mermaid(node_labels, edge_map, mermaid_path)


if __name__ == "__main__":
    main()
