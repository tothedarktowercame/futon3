#!/usr/bin/env python3

"""Emit a small EDN description of Python source files for substrate-2."""

from __future__ import annotations

import ast
import json
import sys
import tokenize
from pathlib import Path
from typing import Iterable


def _edn(value):
    if value is None:
        return "nil"
    if value is True:
        return "true"
    if value is False:
        return "false"
    if isinstance(value, str):
        return json.dumps(value)
    if isinstance(value, (int, float)):
        return str(value)
    if isinstance(value, (list, tuple)):
        return "[" + " ".join(_edn(item) for item in value) + "]"
    if isinstance(value, dict):
        return "{" + " ".join(
            f"{_edn(key)} {_edn(val)}" for key, val in value.items()
        ) + "}"
    raise TypeError(f"Unsupported EDN value: {type(value)!r}")


def _read_paths(argv: list[str]) -> list[Path]:
    if argv:
        return [Path(arg).resolve() for arg in argv]
    return [Path(line.strip()).resolve() for line in sys.stdin if line.strip()]


def _topmost_package_dir(start: Path) -> Path | None:
    current = start
    top = None
    while (current / "__init__.py").exists():
        top = current
        parent = current.parent
        if parent == current:
            break
        current = parent
    return top


def _repo_root(path: Path) -> Path | None:
    for parent in [path.parent, *path.parents]:
        if (parent / "pyproject.toml").exists() or (parent / "setup.py").exists():
            return parent
    return None


def module_name(path: Path) -> str:
    package_dir = _topmost_package_dir(path.parent)
    if package_dir is not None:
        rel = path.relative_to(package_dir.parent).with_suffix("")
        return ".".join(rel.parts)
    repo_root = _repo_root(path)
    if repo_root is not None:
        rel = path.relative_to(repo_root).with_suffix("")
        return ".".join(rel.parts)
    return path.stem


def resolve_from_import(current_module: str, module: str | None, level: int) -> str:
    if level <= 0:
        return module or ""
    package_parts = current_module.split(".")[:-1]
    keep = max(0, len(package_parts) - (level - 1))
    base = package_parts[:keep]
    if module:
        base.extend(module.split("."))
    return ".".join(part for part in base if part)


def is_test_file(path: Path) -> bool:
    name = path.name
    parts = set(path.parts)
    return (
        name.startswith("test_")
        or name.endswith("_test.py")
        or "test" in parts
        or "tests" in parts
    )


class BodySymbolCollector(ast.NodeVisitor):
    def __init__(self) -> None:
        self.symbols: set[str] = set()

    def visit_Name(self, node: ast.Name) -> None:
        if isinstance(node.ctx, ast.Load):
            self.symbols.add(node.id)

    def visit_Attribute(self, node: ast.Attribute) -> None:
        if isinstance(node.ctx, ast.Load):
            root = self._attribute_root(node)
            if root:
                self.symbols.add(f"{root}/{node.attr}")
                return
        self.generic_visit(node)

    def visit_FunctionDef(self, node: ast.FunctionDef) -> None:
        return

    def visit_AsyncFunctionDef(self, node: ast.AsyncFunctionDef) -> None:
        return

    def visit_ClassDef(self, node: ast.ClassDef) -> None:
        return

    def _attribute_root(self, node: ast.Attribute) -> str | None:
        current = node.value
        while isinstance(current, ast.Attribute):
            current = current.value
        if isinstance(current, ast.Name):
            return current.id
        return None


def body_symbols(node: ast.AST) -> list[str]:
    collector = BodySymbolCollector()
    for stmt in getattr(node, "body", []):
        collector.visit(stmt)
    return sorted(collector.symbols)


def import_aliases(tree: ast.Module, current_module: str) -> dict[str, str]:
    aliases: dict[str, str] = {}
    for stmt in tree.body:
        if isinstance(stmt, ast.Import):
            for alias in stmt.names:
                if alias.name == "*":
                    continue
                local = alias.asname or alias.name.split(".")[0]
                aliases[local] = alias.name
        elif isinstance(stmt, ast.ImportFrom):
            target_module = resolve_from_import(current_module, stmt.module, stmt.level)
            for alias in stmt.names:
                if alias.name == "*":
                    continue
                local = alias.asname or alias.name
                aliases[local] = target_module
    return aliases


def top_level_defs(tree: ast.Module, current_module: str, test_file: bool) -> tuple[list[dict], list[dict]]:
    defs: list[dict] = []
    tests: list[dict] = []

    for stmt in tree.body:
        if isinstance(stmt, ast.FunctionDef):
            entry = {
                "name": stmt.name,
                "kind": "def",
                "body-syms": body_symbols(stmt),
                "has-doc": ast.get_docstring(stmt) is not None,
            }
            if stmt.name.startswith("test_"):
                tests.append({"name": stmt.name, "body-syms": entry["body-syms"]})
            else:
                defs.append(entry)
        elif isinstance(stmt, ast.AsyncFunctionDef):
            entry = {
                "name": stmt.name,
                "kind": "async-def",
                "body-syms": body_symbols(stmt),
                "has-doc": ast.get_docstring(stmt) is not None,
            }
            if stmt.name.startswith("test_"):
                tests.append({"name": stmt.name, "body-syms": entry["body-syms"]})
            else:
                defs.append(entry)
        elif isinstance(stmt, ast.ClassDef):
            defs.append(
                {
                    "name": stmt.name,
                    "kind": "class",
                    "body-syms": body_symbols(stmt),
                    "has-doc": ast.get_docstring(stmt) is not None,
                }
            )

    if test_file:
        return [], tests
    return defs, tests


def describe_file(path: Path) -> dict:
    with tokenize.open(path) as handle:
        source = handle.read()
    module = module_name(path)
    tree = ast.parse(source, filename=str(path))
    test_file = is_test_file(path)
    defs, tests = top_level_defs(tree, module, test_file)
    return {
        "path": str(path),
        "module": module,
        "imports": import_aliases(tree, module),
        "defs": defs,
        "tests": tests,
        "is-test?": test_file,
    }


def main(argv: list[str]) -> int:
    for path in _read_paths(argv):
        print(_edn(describe_file(path)))
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
