#!/usr/bin/env python3
"""Cluster and inspect pattern embeddings."""
from __future__ import annotations

import argparse
import csv
import json
import sys
from collections import OrderedDict
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Tuple

try:
    import numpy as np
except ImportError as exc:  # pragma: no cover - handled by caller
    raise SystemExit("numpy is required for patterns CLI") from exc


DEFAULT_EMBEDDINGS = Path("futon3/data/glove_pattern_embeddings.json")
DEFAULT_CLUSTER_OUT = Path("futon3/data/pattern_clusters.json")
DEFAULT_TREE_OUT = Path("futon3/data/pattern_tree.json")


def _die(message: str) -> None:
    raise SystemExit(message)


def _load_json(path: Path) -> Dict[str, List[float]]:
    payload = json.loads(path.read_text())
    if isinstance(payload, dict):
        return {str(k): list(v) for k, v in payload.items()}
    if isinstance(payload, list):
        out: Dict[str, List[float]] = {}
        for item in payload:
            if not isinstance(item, dict):
                _die(f"Unsupported JSON entry in {path}")
            pid = item.get("id") or item.get("pattern") or item.get("pattern_id")
            vec = item.get("vector") or item.get("embedding")
            if pid is None or vec is None:
                _die(f"Missing id/vector in JSON entry from {path}")
            out[str(pid)] = list(vec)
        return out
    _die(f"Unsupported JSON format in {path}")
    return {}


def _load_csv(path: Path) -> Dict[str, List[float]]:
    delimiter = "\t" if path.suffix.lower() == ".tsv" else ","
    out: Dict[str, List[float]] = {}
    with path.open(newline="") as handle:
        reader = csv.reader(handle, delimiter=delimiter)
        rows = list(reader)
    if not rows:
        _die(f"Empty embeddings file {path}")
    start_idx = 0
    if len(rows[0]) > 1:
        try:
            [float(x) for x in rows[0][1:]]
        except ValueError:
            start_idx = 1
    for row in rows[start_idx:]:
        if not row:
            continue
        pid = row[0].strip()
        if not pid:
            continue
        try:
            vec = [float(x) for x in row[1:]]
        except ValueError as exc:
            _die(f"Non-numeric vector in {path}: {exc}")
        out[pid] = vec
    return out


def _load_edn(path: Path) -> Dict[str, List[float]]:
    try:
        import edn_format  # type: ignore
    except ImportError as exc:
        _die("edn_format is required to load .edn embeddings")
    payload = edn_format.loads(path.read_text())
    if not isinstance(payload, dict):
        _die(f"Unsupported EDN format in {path}")
    return {str(k).lstrip(":"): list(v) for k, v in payload.items()}


def load_embeddings(path: Path) -> Tuple[List[str], np.ndarray]:
    if not path.exists():
        _die(
            f"Embeddings file not found: {path}. "
            "Use --embeddings to point at a JSON/CSV/TSV/EDN vector map."
        )
    suffix = path.suffix.lower()
    if suffix == ".json":
        data = _load_json(path)
    elif suffix in {".csv", ".tsv"}:
        data = _load_csv(path)
    elif suffix == ".edn":
        data = _load_edn(path)
    else:
        _die(f"Unsupported embeddings format: {path}")
    if not data:
        _die(f"No embeddings found in {path}")
    ids = sorted(data.keys())
    vectors = []
    for pid in ids:
        vec = data[pid]
        if not isinstance(vec, list):
            _die(f"Vector for {pid} is not a list in {path}")
        vectors.append(vec)
    matrix = np.array(vectors, dtype=float)
    if matrix.ndim != 2:
        _die(f"Embeddings must be a 2D array in {path}")
    return ids, matrix


def normalize_vectors(matrix: np.ndarray) -> Tuple[np.ndarray, Dict[str, int]]:
    if np.isnan(matrix).any():
        _die("Embeddings contain NaN values")
    norms = np.linalg.norm(matrix, axis=1)
    zero_count = int((norms == 0.0).sum())
    if zero_count:
        _die(f"Embeddings contain {zero_count} zero-length vectors")
    normalized = matrix / norms[:, None]
    stats = {
        "n": int(matrix.shape[0]),
        "dim": int(matrix.shape[1]),
        "zeros": zero_count,
        "nans": int(np.isnan(matrix).sum()),
    }
    return normalized, stats


def _get_clusterer(method: str, k: int, linkage: str):
    if method != "hclust":
        return None
    try:
        from sklearn.cluster import AgglomerativeClustering  # type: ignore
    except ImportError as exc:  # pragma: no cover - environment dependent
        _die("scikit-learn is required for hclust clustering")
    try:
        return AgglomerativeClustering(n_clusters=k, metric="cosine", linkage=linkage)
    except TypeError:
        return AgglomerativeClustering(n_clusters=k, affinity="cosine", linkage=linkage)


def _cluster_labels(matrix: np.ndarray, method: str, k: int, linkage: str,
                    min_cluster_size: int, min_samples: Optional[int]) -> np.ndarray:
    if method == "hclust":
        clusterer = _get_clusterer(method, k, linkage)
        labels = clusterer.fit_predict(matrix)
        return labels
    if method == "hdbscan":
        try:
            import hdbscan  # type: ignore
        except ImportError as exc:  # pragma: no cover - environment dependent
            _die("hdbscan is required for hdbscan clustering")
        # HDBSCAN relies on sklearn neighbors; use euclidean on unit vectors,
        # which preserves cosine distance ordering.
        clusterer = hdbscan.HDBSCAN(
            metric="euclidean",
            min_cluster_size=min_cluster_size,
            min_samples=min_samples,
        )
        return clusterer.fit_predict(matrix)
    _die(f"Unknown clustering method: {method}")
    return np.zeros(matrix.shape[0])


def _sorted_cluster_ids(labels: Iterable[int]) -> List[int]:
    unique = sorted(set(int(x) for x in labels), key=lambda x: (x == -1, x))
    return unique


def _pick_center(ids: List[str], vectors: np.ndarray, method: str) -> str:
    if len(ids) == 1:
        return ids[0]
    if method == "centroid":
        centroid = vectors.mean(axis=0)
        norm = np.linalg.norm(centroid)
        if norm == 0.0:
            return sorted(ids)[0]
        centroid = centroid / norm
        scores = vectors @ centroid
        best = sorted(zip(ids, scores), key=lambda x: (-x[1], x[0]))[0]
        return best[0]
    sims = vectors @ vectors.T
    distances = 1.0 - sims
    np.fill_diagonal(distances, 0.0)
    avg = distances.sum(axis=1) / max(len(ids) - 1, 1)
    best = sorted(zip(ids, avg), key=lambda x: (x[1], x[0]))[0]
    return best[0]


def _members_by_center(center_id: str, ids: List[str], vectors: np.ndarray,
                       center_vec: np.ndarray, include_scores: bool) -> Tuple[List[str], Optional[List[float]]]:
    scores = vectors @ center_vec
    scored = sorted(zip(ids, scores), key=lambda x: (-x[1], x[0]))
    members = [pid for pid, _ in scored]
    if include_scores:
        return members, [round(score, 6) for _, score in scored]
    return members, None


def cluster_embeddings(ids: List[str], matrix: np.ndarray, k: int, method: str,
                       linkage: str, centrality: str, include_scores: bool,
                       min_cluster_size: int, min_samples: Optional[int]) -> Dict[str, Dict[str, object]]:
    labels = _cluster_labels(matrix, method, k, linkage, min_cluster_size, min_samples)
    output: Dict[str, Dict[str, object]] = OrderedDict()
    for label in _sorted_cluster_ids(labels):
        member_idx = [i for i, value in enumerate(labels) if value == label]
        member_ids = [ids[i] for i in member_idx]
        member_vecs = matrix[member_idx]
        center_id = _pick_center(member_ids, member_vecs, centrality)
        center_idx = member_ids.index(center_id)
        center_vec = member_vecs[center_idx]
        members, scores = _members_by_center(center_id, member_ids, member_vecs, center_vec, include_scores)
        entry: Dict[str, object] = {"center": center_id, "members": members}
        if include_scores and scores is not None:
            entry["scores"] = scores
        output[str(label)] = entry
    return output


def emit_shell(cluster_id: str, clusters: Dict[str, Dict[str, object]], ids: List[str],
               matrix: np.ndarray, ring_size: int, max_rings: Optional[int],
               with_scores: bool) -> None:
    if cluster_id not in clusters:
        _die(f"Cluster {cluster_id} not found in cluster file")
    cluster = clusters[cluster_id]
    members = cluster.get("members")
    center_id = cluster.get("center")
    if not isinstance(members, list) or not center_id:
        _die(f"Cluster {cluster_id} missing members or center")
    member_ids = [str(x) for x in members]
    id_map = {pid: idx for idx, pid in enumerate(ids)}
    missing = [pid for pid in member_ids if pid not in id_map]
    if missing:
        _die(f"Cluster members missing from embeddings: {missing[:5]}")
    center_idx = id_map.get(center_id)
    if center_idx is None:
        _die(f"Cluster center {center_id} missing from embeddings")
    center_vec = matrix[center_idx]
    member_vecs = matrix[[id_map[pid] for pid in member_ids]]
    members_sorted, scores = _members_by_center(center_id, member_ids, member_vecs, center_vec, True)
    print(f"cluster {cluster_id} center {center_id}")
    ring = 0
    for start in range(0, len(members_sorted), ring_size):
        if max_rings is not None and ring >= max_rings:
            break
        chunk = members_sorted[start:start + ring_size]
        if with_scores:
            chunk_scores = scores[start:start + ring_size] if scores else []
            rendered = " ".join(f"{pid}({score:.3f})" for pid, score in zip(chunk, chunk_scores))
        else:
            rendered = " ".join(chunk)
        print(f"ring {ring}: {rendered}")
        ring += 1


def build_tree(ids: List[str], matrix: np.ndarray, k: int, max_depth: int,
               min_size: int, linkage: str, centrality: str, depth: int,
               node_id: str) -> Dict[str, object]:
    member_ids = ids
    member_vecs = matrix
    center_id = _pick_center(member_ids, member_vecs, centrality)
    center_idx = member_ids.index(center_id)
    center_vec = member_vecs[center_idx]
    members, _ = _members_by_center(center_id, member_ids, member_vecs, center_vec, False)
    node: Dict[str, object] = {"id": node_id, "center": center_id, "members": members}
    if depth >= max_depth or len(member_ids) < min_size or len(member_ids) < k:
        return node
    labels = _cluster_labels(member_vecs, "hclust", k, linkage, 0, None)
    children: List[Dict[str, object]] = []
    for label in _sorted_cluster_ids(labels):
        child_idx = [i for i, value in enumerate(labels) if value == label]
        child_ids = [member_ids[i] for i in child_idx]
        child_vecs = member_vecs[child_idx]
        child_node_id = f"{node_id}.{label}"
        child = build_tree(
            child_ids,
            child_vecs,
            k,
            max_depth,
            min_size,
            linkage,
            centrality,
            depth + 1,
            child_node_id,
        )
        children.append(child)
    children.sort(key=lambda x: (x.get("center") or "", x.get("id") or ""))
    node["children"] = children
    return node


def dry_run(path: Path) -> None:
    ids, matrix = load_embeddings(path)
    _, stats = normalize_vectors(matrix)
    print(f"path={path}")
    print(f"n={stats['n']} dim={stats['dim']} zeros={stats['zeros']} nans={stats['nans']}")
    if len(ids) != len(set(ids)):
        print("warning=duplicate_ids")


def parse_args(argv: List[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Cluster pattern embeddings.")
    parser.add_argument("--embeddings", default=str(DEFAULT_EMBEDDINGS),
                        help="Path to embeddings file (JSON/CSV/TSV/EDN).")
    subparsers = parser.add_subparsers(dest="command", required=True)

    cluster = subparsers.add_parser("cluster", help="Cluster embeddings.")
    cluster.add_argument("--embeddings", default=argparse.SUPPRESS,
                         help="Path to embeddings file (JSON/CSV/TSV/EDN).")
    cluster.add_argument("--k", type=int, required=True, help="Number of clusters.")
    cluster.add_argument("--method", choices=["hclust", "hdbscan"], default="hclust")
    cluster.add_argument("--linkage", choices=["average", "complete"], default="average")
    cluster.add_argument("--centrality", choices=["medoid", "centroid"], default="medoid")
    cluster.add_argument("--include-scores", action="store_true", help="Include similarity scores.")
    cluster.add_argument("--out", default=str(DEFAULT_CLUSTER_OUT), help="Output JSON path.")
    cluster.add_argument("--min-cluster-size", type=int, default=5,
                         help="HDBSCAN min cluster size.")
    cluster.add_argument("--min-samples", type=int, default=None,
                         help="HDBSCAN min samples.")

    shell = subparsers.add_parser("shell", help="Print cluster shells.")
    shell.add_argument("--embeddings", default=argparse.SUPPRESS,
                       help="Path to embeddings file (JSON/CSV/TSV/EDN).")
    shell.add_argument("--cluster", required=True, help="Cluster ID.")
    shell.add_argument("--clusters", default=str(DEFAULT_CLUSTER_OUT),
                       help="Path to cluster JSON output.")
    shell.add_argument("--ring-size", type=int, default=10)
    shell.add_argument("--max-rings", type=int, default=None)
    shell.add_argument("--with-scores", action="store_true", help="Include similarity scores.")

    tree = subparsers.add_parser("tree", help="Build recursive cluster tree.")
    tree.add_argument("--embeddings", default=argparse.SUPPRESS,
                      help="Path to embeddings file (JSON/CSV/TSV/EDN).")
    tree.add_argument("--k", type=int, required=True, help="Number of clusters per level.")
    tree.add_argument("--max-depth", type=int, required=True)
    tree.add_argument("--min-size", type=int, default=2)
    tree.add_argument("--linkage", choices=["average", "complete"], default="average")
    tree.add_argument("--centrality", choices=["medoid", "centroid"], default="medoid")
    tree.add_argument("--out", default=str(DEFAULT_TREE_OUT), help="Output JSON path.")

    dry = subparsers.add_parser("dry-run", help="Validate embeddings and print stats.")
    dry.add_argument("--embeddings", default=argparse.SUPPRESS,
                     help="Path to embeddings file (JSON/CSV/TSV/EDN).")

    return parser.parse_args(argv)


def main(argv: List[str]) -> None:
    args = parse_args(argv)
    embeddings_path = Path(args.embeddings)

    if args.command == "dry-run":
        dry_run(embeddings_path)
        return

    ids, matrix = load_embeddings(embeddings_path)
    matrix, _stats = normalize_vectors(matrix)

    if args.command == "cluster":
        if args.k <= 0:
            _die("--k must be positive")
        output = cluster_embeddings(
            ids,
            matrix,
            args.k,
            args.method,
            args.linkage,
            args.centrality,
            args.include_scores,
            args.min_cluster_size,
            args.min_samples,
        )
        out_path = Path(args.out)
        out_path.parent.mkdir(parents=True, exist_ok=True)
        out_path.write_text(json.dumps(output, indent=2))
        print(f"Wrote {len(output)} clusters to {out_path}")
        return

    if args.command == "shell":
        clusters_path = Path(args.clusters)
        if not clusters_path.exists():
            _die(f"Clusters file not found: {clusters_path}")
        clusters = json.loads(clusters_path.read_text())
        emit_shell(
            args.cluster,
            clusters,
            ids,
            matrix,
            args.ring_size,
            args.max_rings,
            args.with_scores,
        )
        return

    if args.command == "tree":
        if args.k <= 0:
            _die("--k must be positive")
        tree = build_tree(
            ids,
            matrix,
            args.k,
            args.max_depth,
            args.min_size,
            args.linkage,
            args.centrality,
            0,
            "root",
        )
        out_path = Path(args.out)
        out_path.parent.mkdir(parents=True, exist_ok=True)
        out_path.write_text(json.dumps(tree, indent=2))
        print(f"Wrote tree to {out_path}")
        return

    _die(f"Unknown command: {args.command}")


if __name__ == "__main__":
    main(sys.argv[1:])
