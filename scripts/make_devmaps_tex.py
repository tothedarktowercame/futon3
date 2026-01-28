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
DONE_STATUS_RE = re.compile(r"status\[(?i:done)\]")
INSTANTIATED_RE = re.compile(r"^\s*!\s+instantiated-by:", re.IGNORECASE)
MATURITY_RE = re.compile(r"^\s*:maturity\s+:([A-Za-z-]+)\s*$", re.IGNORECASE)

MATURITY_ENV = {
    "active": "devmapactive",
    "settled": "devmapsettled",
    "greenfield": "devmapgreenfield",
    "green-field": "devmapgreenfield",
    "stub": "devmapstub",
}


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


CODE_BREAK_CHARS = {",", ";", ":", "|", "/", "\\", "_", "-", "=", "+", "?", "&"}


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


def escape_code_line(line: str) -> str:
    result: list[str] = []
    for ch in line:
        result.append(SPECIALS.get(ch, ch))
        if ch in CODE_BREAK_CHARS:
            result.append(r"\allowbreak{}")
    return "".join(result)


def split_sections(raw_lines: list[str]) -> list[tuple[str, list[str]]]:
    sections: list[tuple[str, list[str]]] = []
    current: list[str] = []
    kind = "preamble"
    for line in raw_lines:
        if INSTANTIATED_RE.match(line):
            if current:
                sections.append((kind, current))
            current = [line]
            kind = "prototype"
        else:
            current.append(line)
    if current:
        sections.append((kind, current))
    return sections


def section_is_done(raw_lines: list[str]) -> bool:
    return any(DONE_STATUS_RE.search(line) for line in raw_lines)


def section_maturity(raw_lines: list[str]) -> str | None:
    for line in raw_lines:
        match = MATURITY_RE.match(line)
        if match:
            return match.group(1).lower()
    return None


def render_lines(raw_lines: list[str], emoji_chars: set[str]) -> list[str]:
    lines: list[str] = []
    in_ifr = False
    i = 0
    total = len(raw_lines)
    heading_re = re.compile(r"^(?P<indent>\s*)#{2,6}\s+(?P<title>.+)$")
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

        is_heading = heading_re.match(raw_line)
        is_flexi = bool(FLEXI_RE.match(body))
        is_directive = body_lower.startswith("@")
        is_instantiated = body_lower.startswith("! instantiated-by:")
        is_bullet = body.lstrip().startswith("-")
        code_candidate = body.lstrip()
        is_codeish = bool(code_candidate and code_candidate[0] in "{:[]}")

        heading_match = heading_re.match(raw_line)
        if heading_match:
            title = heading_match.group("title").strip()
            if title:
                escaped_title = escape_line(normalize_quotes(title))
                lines.append(f"{indent}\\textbf{{{escaped_title}}}\\par")
            j = i + 1
            para_lines: list[str] = []
            while j < total:
                look = raw_lines[j]
                look_stripped = look.strip()
                if not look_stripped:
                    break
                look_body = look.lstrip()
                look_lower = look_body.lower()
                if (
                    FLEXI_RE.match(look_body)
                    or look_lower.startswith("@")
                    or look_lower.startswith("! instantiated-by:")
                    or heading_re.match(look)
                ):
                    break
                para_lines.append(look_body.strip())
                for ch in look:
                    if ord(ch) >= 128 and needs_emoji_font(ch):
                        emoji_chars.add(ch)
                j += 1
            if para_lines:
                paragraph = normalize_quotes(" ".join(para_lines))
                escaped_paragraph = escape_line(paragraph)
                lines.append(f"{indent}{escaped_paragraph}\\\\")
            i = j
            continue

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

        if is_codeish:
            escaped_code = escape_code_line(code_candidate)
            lines.append(f"{indent}\\texttt{{{escaped_code}}}\\\\")
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

        is_regular = not (is_heading or is_flexi or is_directive or is_instantiated or is_bullet or is_codeish)
        if is_regular and not make_bold:
            j = i
            para_lines: list[str] = []
            while j < total:
                look = raw_lines[j]
                look_stripped = look.strip()
                if not look_stripped:
                    break
                look_body = look.lstrip()
                look_lower = look_body.lower()
                look_code = look_body.lstrip()
                if (
                    heading_re.match(look)
                    or FLEXI_RE.match(look_body)
                    or look_lower.startswith("@")
                    or look_lower.startswith("! instantiated-by:")
                    or look_body.startswith("-")
                    or (look_code and look_code[0] in "{:[]}")
                ):
                    break
                para_lines.append(look_body.strip())
                for ch in look:
                    if ord(ch) >= 128 and needs_emoji_font(ch):
                        emoji_chars.add(ch)
                j += 1
            if para_lines:
                paragraph = normalize_quotes(" ".join(para_lines))
                escaped_paragraph = escape_line(paragraph)
                lines.append(f"{indent}{escaped_paragraph}\\\\")
                i = j
                continue

        if make_bold:
            escaped = rf"\textbf{{{escaped}}}"
        lines.append(f"{indent}{escaped}\\\\")
        i += 1

    return lines


def build_block(title: str, content: str, emoji_chars: set[str]) -> str:
    raw_lines = content.splitlines()
    sections = split_sections(raw_lines)
    block_lines = [f"% {title}"]
    block_lines.append(rf"\textbf{{{title}}}\par")
    block_lines.append("\\medskip")
    for kind, section_lines in sections:
        rendered = render_lines(section_lines, emoji_chars)
        if kind == "prototype":
            env = None
            if section_is_done(section_lines):
                env = "devmapdone"
            else:
                maturity = section_maturity(section_lines)
                env = MATURITY_ENV.get(maturity)
            if env:
                block_lines.append(rf"\begin{{{env}}}")
                block_lines.extend(rendered)
                block_lines.append(rf"\end{{{env}}}")
            else:
                block_lines.extend(rendered)
        else:
            block_lines.extend(rendered)
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
\usepackage{framed}
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
\definecolor{devmapdonebg}{RGB}{232,246,232}
\definecolor{devmapactivebg}{RGB}{255,244,214}
\definecolor{devmapsettledbg}{RGB}{221,236,255}
\definecolor{devmapgreenfieldbg}{RGB}{226,255,226}
\definecolor{devmapstubbg}{RGB}{242,242,242}
\newenvironment{devmapdone}{%
  \def\FrameCommand{\colorbox{devmapdonebg}}%
  \setlength{\fboxsep}{2pt}%
  \MakeFramed{\advance\hsize-\width \FrameRestore}%
}{\endMakeFramed}
\newenvironment{devmapactive}{%
  \def\FrameCommand{\colorbox{devmapactivebg}}%
  \setlength{\fboxsep}{2pt}%
  \MakeFramed{\advance\hsize-\width \FrameRestore}%
}{\endMakeFramed}
\newenvironment{devmapsettled}{%
  \def\FrameCommand{\colorbox{devmapsettledbg}}%
  \setlength{\fboxsep}{2pt}%
  \MakeFramed{\advance\hsize-\width \FrameRestore}%
}{\endMakeFramed}
\newenvironment{devmapgreenfield}{%
  \def\FrameCommand{\colorbox{devmapgreenfieldbg}}%
  \setlength{\fboxsep}{2pt}%
  \MakeFramed{\advance\hsize-\width \FrameRestore}%
}{\endMakeFramed}
\newenvironment{devmapstub}{%
  \def\FrameCommand{\colorbox{devmapstubbg}}%
  \setlength{\fboxsep}{2pt}%
  \MakeFramed{\advance\hsize-\width \FrameRestore}%
}{\endMakeFramed}
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
