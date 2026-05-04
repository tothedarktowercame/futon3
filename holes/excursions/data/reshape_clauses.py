#!/usr/bin/env python3
"""Reshape .flexiarg files per E-clause-vocabulary-reshape.sexp.

Operates on a single .flexiarg file. Preserves prose verbatim; relocates
clauses into the seven-component canonical frame; flags authorial cases.

Usage:
    python3 reshape_clauses.py <path-to-flexiarg> [--apply]
    python3 reshape_clauses.py --dir <library-subdir> [--apply]

Without --apply, prints the proposed reshape to stdout and writes nothing.
"""
import argparse
import os
import re
import sys
from dataclasses import dataclass, field
from typing import Optional


CLAUSE_HEADER_RE = re.compile(r'^(\s*)([!+])\s+([^:\n]+):\s*(.*)$')


@dataclass
class Clause:
    prefix: str          # '!' or '+'
    name: str            # original-case name
    name_key: str        # lower-cased / hyphenated
    indent: int          # column of prefix marker
    inline: str          # text on the same line after the colon
    body: list[str] = field(default_factory=list)   # subsequent indented body lines

    def render(self, base_indent: int = 2) -> list[str]:
        marker = ' ' * base_indent + f'{self.prefix} {self.name}:'
        first = marker + ((' ' + self.inline) if self.inline.strip() else '')
        out = [first]
        for line in self.body:
            out.append(line)
        return out


@dataclass
class Pattern:
    header_lines: list[str] = field(default_factory=list)  # @-prefixed
    blank_after_header: list[str] = field(default_factory=list)
    conclusion: Optional[Clause] = None
    clauses: list[Clause] = field(default_factory=list)
    flags: list[str] = field(default_factory=list)

    def find(self, key: str) -> Optional[Clause]:
        for c in self.clauses:
            if c.name_key == key:
                return c
        return None

    def remove(self, key: str) -> Optional[Clause]:
        for i, c in enumerate(self.clauses):
            if c.name_key == key:
                return self.clauses.pop(i)
        return None

    def remove_all(self, key: str) -> list['Clause']:
        out = []
        i = 0
        while i < len(self.clauses):
            if self.clauses[i].name_key == key:
                out.append(self.clauses.pop(i))
            else:
                i += 1
        return out


def parse(text: str) -> Pattern:
    pat = Pattern()
    lines = text.splitlines()
    i = 0
    # Header @-lines
    while i < len(lines):
        ln = lines[i]
        if ln.startswith('@'):
            pat.header_lines.append(ln)
            i += 1
        elif ln.strip() == '':
            pat.blank_after_header.append(ln)
            i += 1
        else:
            break

    # Conclusion + clauses
    current: Optional[Clause] = None
    while i < len(lines):
        ln = lines[i]
        m = CLAUSE_HEADER_RE.match(ln)
        if m:
            indent_str, prefix, name_raw, trailing = m.groups()
            name = name_raw.strip()
            name_key = re.sub(r'\s+', '-', name.lower())
            new_clause = Clause(
                prefix=prefix,
                name=name,
                name_key=name_key,
                indent=len(indent_str),
                inline=trailing,
            )
            if prefix == '!':
                pat.conclusion = new_clause
            else:
                pat.clauses.append(new_clause)
            current = new_clause
        else:
            if current is not None:
                current.body.append(ln)
            else:
                pat.header_lines.append(ln)
        i += 1

    # Trim trailing blank body lines (kept inside files but cleaner to strip)
    for c in [pat.conclusion] + pat.clauses:
        if c is None:
            continue
        while c.body and c.body[-1].strip() == '':
            c.body.pop()

    return pat


def split_tension_prose(body: list[str], inline: str) -> tuple[list[str], list[str], list[str]]:
    """Heuristically split a :tension clause body into IF and HOWEVER prose.

    Recognises the regular Axis: / Real: / Irreducible: / Active: structure.
    Returns (if_lines, however_lines, fallback_unsplit_lines).
    fallback_unsplit_lines is empty when the split succeeded; otherwise
    contains the original body so the caller can fall back to a placeholder.
    """
    full = ([inline] if inline.strip() else []) + body
    # Group prose into sub-fields by the leading "Word:" pattern at the
    # body's natural indent.
    fields: dict[str, list[str]] = {}
    current_key: Optional[str] = None
    field_re = re.compile(r'^(\s*)([A-Z][A-Za-z\-]+):\s*(.*)$')
    for ln in full:
        m = field_re.match(ln)
        if m:
            _, key, rest = m.groups()
            current_key = key.lower()
            fields[current_key] = [rest] if rest.strip() else []
        elif current_key is not None:
            fields[current_key].append(ln)
        else:
            # No leading sub-field key — fall back.
            return [], [], list(full)

    # Need at least axis + real (irreducible/active are bonus).
    if 'axis' not in fields or 'real' not in fields:
        return [], [], list(full)

    axis_text = ' '.join(s.strip() for s in fields['axis'] if s.strip())
    real_text = ' '.join(s.strip() for s in fields['real'] if s.strip())
    irreducible_text = ' '.join(s.strip() for s in fields.get('irreducible', []) if s.strip())
    active_text = ' '.join(s.strip() for s in fields.get('active', []) if s.strip())

    # IF carries the constructive-aim prose: the axis as the structural
    # commitment, plus the irreducible-statement that names what cannot be
    # had simultaneously. (The pattern's THEN names which pole the move
    # actually targets.)
    if_lines = [
        f'    The pattern operates on the axis: {axis_text}',
    ]
    if irreducible_text:
        if_lines.append(f'    Irreducible: {irreducible_text}')

    # HOWEVER carries the costs of each pole + the temporal dynamic.
    however_lines = [
        f'    Real costs: {real_text}',
    ]
    if active_text:
        however_lines.append(f'    Active dynamic: {active_text}')

    return if_lines, however_lines, []


def reshape(pat: Pattern) -> Pattern:
    """Apply E-clause-vocabulary-reshape.sexp rules to the pattern."""

    # ----- Step 1: handle TENSION → IF + HOWEVER (authorial-mergeable) ----
    tension = pat.remove('tension')
    if tension is not None:
        if_lines, however_lines, fallback = split_tension_prose(tension.body, tension.inline)
        if fallback:
            # Heuristic split failed; record the original prose as inline
            # IF and HOWEVER placeholders, flag for manual review.
            pat.flags.append('manual-tension-split')
            if_clause = Clause('+', 'IF', 'if', 2, '', list(fallback))
            however_clause = Clause('+', 'HOWEVER', 'however', 2, '', [])
        else:
            if_clause = Clause('+', 'IF', 'if', 2, '', if_lines)
            however_clause = Clause('+', 'HOWEVER', 'however', 2, '', however_lines)
        pat.clauses.insert(0, if_clause)
        pat.clauses.insert(1, however_clause)
    # else: file already has explicit IF/HOWEVER (or simply no tension clause)

    # ----- Step 2: substructure moves -------------------------------------
    # Synthesized parent stubs (used when substructure has no host).
    SYNTHESIZED_PARENT_TEXT = {
        'then':    "Operate an explicit move on the named axis (see substructure below); steward the pattern against the declared discipline.",
        'however': "The structural tension above manifests in the following concrete shapes (see substructure).",
        'because': "The warrant for the move (see substructure).",
        'if':      "The applicability scope of the pattern (see substructure).",
        'next-steps': "Operational follow-on (see substructure).",
    }

    def ensure_parent(parent_key: str) -> Clause:
        """Find an existing parent clause or synthesize a stub one."""
        existing = pat.find(parent_key)
        if existing is not None:
            return existing
        synthesized_text = SYNTHESIZED_PARENT_TEXT.get(parent_key, '')
        stub = Clause(
            prefix='+',
            name=parent_key.upper(),
            name_key=parent_key,
            indent=2,
            inline=synthesized_text,
            body=[],
        )
        pat.clauses.append(stub)
        pat.flags.append(f'synthesized-parent:{parent_key}')
        return stub

    def attach_substructure(parent_key: str, child: Clause, sub_indent: int = 4):
        """Attach `child` as a sub-clause body of `parent`.
        If parent is missing, synthesize a stub parent first.
        Re-indents the child's marker to sub_indent and shifts its body."""
        parent = ensure_parent(parent_key)
        # Extra blank line separator if parent's body has content
        sep = [''] if parent.body and parent.body[-1].strip() else []
        marker = ' ' * sub_indent + f'+ {child.name}:'
        first = marker + ((' ' + child.inline) if child.inline.strip() else '')
        # Re-indent body: original indent at child.indent → sub_indent + 2
        new_body = []
        shift = sub_indent + 2 - (child.indent + 2)  # original body indent vs target
        for line in child.body:
            if line.strip() == '':
                new_body.append(line)
            elif shift >= 0:
                new_body.append(' ' * shift + line)
            else:
                # Body had less indent than expected; preserve as-is
                new_body.append(line)
        parent.body.extend(sep + [first] + new_body)

    for parent_key, child_keys in [
        ('then',    ['compositions', 'check', 'enforcement', 'lean']),
        ('however', ['failure-modes', 'anti-patterns', 'absence-signals']),
        ('because', ['evidence', 'evidence-base', 'because->evidence',
                     'mechanism', 'counterfactual']),
        ('next-steps', ['use']),
    ]:
        for ck in child_keys:
            for child in pat.remove_all(ck):
                # Normalize the synonyms to a single substructure name
                if ck in ('evidence-base', 'because->evidence'):
                    child.name = 'EVIDENCE'
                    child.name_key = 'evidence'
                attach_substructure(parent_key, child)

    # IF substructure: does-not-apply
    dna = pat.remove('does-not-apply')
    if dna is not None:
        attach_substructure('if', dna)

    # Signals: confirmation-shape (under CHECK if it exists) vs
    # violation-shape (under HOWEVER, sibling of ABSENCE-SIGNALS /
    # FAILURE-MODES). Heuristic per the rulebook amendment 2026-05-04.
    sig = pat.remove('signals')
    if sig is not None:
        # By the time substructure-moves above have run, CHECK is
        # already inside THEN's body (not a top-level clause), so we
        # can't `pat.find('check')`. Use presence of CHECK substructure
        # text inside THEN's body as the discriminator.
        then_clause = pat.find('then')
        check_present = (then_clause is not None and
                         any('+ CHECK:' in line or '+ check:' in line
                             for line in then_clause.body))
        if check_present:
            # Append signals as a sub-sub-clause of THEN's CHECK block.
            # Simpler: append to THEN's body at sub-sub-indent (6 spaces).
            sep = [''] if then_clause.body and then_clause.body[-1].strip() else []
            marker = ' ' * 6 + f'+ {sig.name}:'
            first = marker + ((' ' + sig.inline) if sig.inline.strip() else '')
            new_body = []
            for line in sig.body:
                if line.strip() == '':
                    new_body.append(line)
                else:
                    # Indent body to 8 spaces (sub-sub-body)
                    new_body.append(' ' * 8 + line.lstrip())
            then_clause.body.extend(sep + [first] + new_body)
        else:
            # Violation-shape: route under HOWEVER (synthesizing if absent)
            attach_substructure('however', sig)

    # ----- Step 3: replaces / fold-prose-into-canonical -------------------
    # therefore → then (fold prose)
    therefore = pat.remove('therefore')
    if therefore is not None:
        target = pat.find('then')
        if target is None:
            pat.flags.append('orphaned-therefore-no-then')
        else:
            target.body.extend([''] + ([therefore.inline] if therefore.inline else []) + therefore.body)

    # applies-when → if (fold prose)
    aw = pat.remove('applies-when')
    if aw is not None:
        target = pat.find('if')
        if target is None:
            pat.flags.append('orphaned-applies-when-no-if')
        else:
            target.body.extend([''] + ([aw.inline] if aw.inline else []) + aw.body)

    # derivation → because (fold prose with "Mechanic, in short:" preamble
    # reusing the original Mechanic/Serves/Axis/Without label structure)
    derivation = pat.remove('derivation')
    if derivation is not None:
        target = pat.find('because')
        if target is None:
            pat.flags.append('orphaned-derivation-no-because')
        else:
            target.body.append('')
            preamble = '    Mechanic, in short:'
            target.body.append(preamble)
            indent = '    '
            full = ([derivation.inline] if derivation.inline.strip() else []) + derivation.body
            for ln in full:
                stripped = ln.strip()
                if stripped:
                    target.body.append(indent + stripped)

    # invariant-statement → fold into ! conclusion: (authorial — flag and
    # append as a final clause of the conclusion's prose)
    inv = pat.remove('invariant-statement')
    if inv is not None and pat.conclusion is not None:
        pat.flags.append('manual-invariant-statement-merge')
        full_inv = ' '.join(s.strip() for s in
                            ([inv.inline] if inv.inline.strip() else []) + inv.body
                            if s.strip())
        # Append the invariant prose to the conclusion line
        pat.conclusion.inline = (pat.conclusion.inline.rstrip() + '  ' + full_inv).strip()

    # ----- Step 4: metadata promotions ------------------------------------
    def promote_to_metadata(key: str, at_key: str):
        all_items = []
        for clause in pat.remove_all(key):
            full = ([clause.inline] if clause.inline.strip() else []) + clause.body
            for ln in full:
                s = ln.strip()
                if s.startswith('-'):
                    s = s[1:].strip()
                if s:
                    if ' ' in s and not (s.startswith('"') or s.startswith('[')):
                        all_items.append('"' + s.replace('"', '\\"') + '"')
                    else:
                        all_items.append(s)
        if all_items:
            pat.header_lines.append(f'@{at_key} [{" ".join(all_items)}]')

    promote_to_metadata('ancestors', 'ancestors')
    promote_to_metadata('related-patterns', 'references')
    promote_to_metadata('status', 'status')
    promote_to_metadata('governance', 'governance')
    promote_to_metadata('instantiated-by', 'instantiated-by')
    promote_to_metadata('illustrates', 'illustrates')

    # ----- Step 5: enforce canonical clause order -------------------------
    canonical_order = ['context', 'if', 'however', 'then', 'because', 'next-steps']
    order_index = {k: i for i, k in enumerate(canonical_order)}
    pat.clauses.sort(key=lambda c: (order_index.get(c.name_key, 99), c.name_key))

    # ----- Step 6: long-tail bespoke clause heuristic ----------------------
    # Per the rulebook's :* rule, long-tail clauses get per-pattern review.
    # As operator-by-proxy, route by name keyword to the canonical component
    # whose semantic role best fits, preserving the original clause as named
    # substructure. Falls back to THEN (the pattern's resolution slot) when
    # no keyword matches — that's the most permissive home and easiest to
    # tighten in a later operator pass.
    known = set(canonical_order) | {
        'conclusion',
        # universal-tolerated
        'evidence', 'failure-modes',
        # specialized
        'status', 'tension', 'check', 'compositions', 'invariant-statement',
        'governance', 'derivation', 'ancestors', 'related-patterns',
        'anti-patterns', 'because->evidence', 'absence-signals', 'lean',
        'mechanism', 'counterfactual', 'applies-when', 'does-not-apply',
        'signals', 'use',
    }

    def classify_bespoke(name_key: str) -> str:
        """Return the canonical-component target for a bespoke clause name."""
        n = name_key.lower()
        # HOWEVER-shape (failure / violation / absence / risk)
        if any(s in n for s in ['violation', 'wrong-', 'ghost-', 'instability',
                                 'failure', 'worn-away', 'running-circle',
                                 'absence', 'anti-', 'tripwire', 'broken']):
            return 'however'
        # BECAUSE-shape (warrant / evidence / derivation / mechanism)
        if any(s in n for s in ['evidence', 'mechanism', 'counterfactual',
                                 'warrant', 'derivation', 'rationale',
                                 'gain-calculation', 'because']):
            return 'because'
        # IF-shape (scope / preconditions / requirements)
        if any(s in n for s in ['requirements', 'minimum-viable',
                                 'applies-when', 'agent-types',
                                 'contract-requirements', 'not-required']):
            return 'if'
        # NEXT-STEPS-shape (escalation / reversal / follow-on)
        if any(s in n for s in ['escalation', 'reversal', 'next-',
                                 'follow-on']):
            return 'next-steps'
        # CONTEXT-shape (8FP/meditation framing layers)
        if any(s in n for s in ['mundane-content', 'transcendent-quality',
                                 'ripens-in', 'distinguishing-from-mundane',
                                 'distinguishing-from-wrong']):
            return 'context'
        # THEN-shape (everything else — implementation / procedure / move)
        return 'then'

    def metadata_target(name_key: str) -> Optional[str]:
        """Return @-metadata key when a bespoke name is structurally
        metadata rather than pattern content, else None."""
        n = name_key.lower()
        if n in ('definition', 'specifies', 'layer-position'):
            return n
        if n.startswith('relationship-to-') or n.startswith('relation-to-'):
            return 'references'  # merge into @references list
        if n in ('related',):
            return 'references'
        return None

    bespoke_clauses = [c for c in pat.clauses if c.name_key not in known]
    for c in bespoke_clauses:
        pat.flags.append(f'long-tail-bespoke:{c.name_key}')
        pat.clauses.remove(c)
        meta_key = metadata_target(c.name_key)
        if meta_key == 'references':
            # Merge clause body items into the existing or new @references list
            full = ([c.inline] if c.inline.strip() else []) + c.body
            items = []
            for ln in full:
                s = ln.strip()
                if s.startswith('-'):
                    s = s[1:].strip()
                if s:
                    items.append('"' + s.replace('"', '\\"') + '"' if ' ' in s else s)
            if items:
                pat.header_lines.append(f'@references-extra [{" ".join(items)}]')
        elif meta_key is not None:
            full = ' '.join(s.strip() for s in
                            ([c.inline] if c.inline.strip() else []) + c.body
                            if s.strip())
            pat.header_lines.append(f'@{meta_key} "{full}"')
        else:
            target = classify_bespoke(c.name_key)
            attach_substructure(target, c)

    # Re-sort clauses after potential additions from heuristic routing
    pat.clauses.sort(key=lambda c: (order_index.get(c.name_key, 99), c.name_key))

    return pat


def render(pat: Pattern) -> str:
    out = []
    out.extend(pat.header_lines)
    if pat.blank_after_header:
        out.extend(pat.blank_after_header)
    elif pat.header_lines:
        out.append('')
    if pat.conclusion is not None:
        out.append(f'! conclusion: {pat.conclusion.inline.strip()}')
        for line in pat.conclusion.body:
            out.append(line)
    for c in pat.clauses:
        out.append('')
        out.extend(c.render(base_indent=2))
    if not out[-1].endswith('\n'):
        out.append('')
    return '\n'.join(out)


def process_file(path: str, apply: bool) -> dict:
    with open(path) as fh:
        original = fh.read()
    pat = parse(original)
    pat = reshape(pat)
    new_text = render(pat)
    changed = new_text != original
    if apply and changed:
        with open(path, 'w') as fh:
            fh.write(new_text)
    return {
        'path': path,
        'changed': changed,
        'flags': pat.flags,
        'new_text': new_text if not apply else None,
    }


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('target', help='File or directory')
    ap.add_argument('--apply', action='store_true', help='Write changes to disk')
    args = ap.parse_args()

    if os.path.isfile(args.target):
        files = [args.target]
    else:
        files = []
        for dp, _, fns in os.walk(args.target):
            for f in sorted(fns):
                if f.endswith(('.flexiarg', '.multiarg')):
                    files.append(os.path.join(dp, f))

    summary = {'changed': 0, 'unchanged': 0, 'flags': {}}
    for f in files:
        result = process_file(f, args.apply)
        if result['changed']:
            summary['changed'] += 1
        else:
            summary['unchanged'] += 1
        for flag in result['flags']:
            summary['flags'][flag] = summary['flags'].get(flag, 0) + 1
        if result['flags']:
            print(f'FLAGS  {f}: {result["flags"]}', file=sys.stderr)

    print(f'\n{summary["changed"]} changed, {summary["unchanged"]} unchanged')
    print('Flag counts:')
    for flag, n in sorted(summary['flags'].items(), key=lambda kv: -kv[1]):
        print(f'  {n:5d}  {flag}')


if __name__ == '__main__':
    main()
