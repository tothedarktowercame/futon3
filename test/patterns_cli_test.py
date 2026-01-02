import json
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path
import importlib.util

ROOT = Path(__file__).resolve().parents[1]
EMBEDDINGS = ROOT / "dev" / "fixtures" / "pattern_embeddings.json"
SCRIPT = ROOT / "scripts" / "patterns.py"


def _run(args):
    result = subprocess.run(
        [sys.executable, str(SCRIPT), "--embeddings", str(EMBEDDINGS), *args],
        capture_output=True,
        text=True,
        check=False,
    )
    return result


class PatternsCliTests(unittest.TestCase):
    def test_dry_run(self):
        result = _run(["dry-run"])
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("n=6", result.stdout)
        self.assertIn("dim=3", result.stdout)

    @unittest.skipUnless(
        importlib.util.find_spec("sklearn") is not None,
        "scikit-learn not installed",
    )
    def test_cluster_smoke(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            out_path = Path(tmpdir) / "clusters.json"
            result = _run(["cluster", "--k", "2", "--out", str(out_path)])
            self.assertEqual(result.returncode, 0, result.stderr)
            data = json.loads(out_path.read_text())
            self.assertEqual(len(data), 2)
            members = []
            for entry in data.values():
                self.assertIn("center", entry)
                self.assertIn("members", entry)
                members.extend(entry["members"])
            self.assertEqual(set(members), {"alpha", "beta", "gamma", "delta", "epsilon", "zeta"})

    @unittest.skipUnless(
        importlib.util.find_spec("sklearn") is not None,
        "scikit-learn not installed",
    )
    def test_shell_smoke(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            out_path = Path(tmpdir) / "clusters.json"
            result = _run(["cluster", "--k", "2", "--out", str(out_path)])
            self.assertEqual(result.returncode, 0, result.stderr)
            clusters = json.loads(out_path.read_text())
            cluster_id = next(iter(clusters.keys()))
            result = subprocess.run(
                [
                    sys.executable,
                    str(SCRIPT),
                    "--embeddings",
                    str(EMBEDDINGS),
                    "shell",
                    "--cluster",
                    cluster_id,
                    "--clusters",
                    str(out_path),
                    "--ring-size",
                    "2",
                ],
                capture_output=True,
                text=True,
                check=False,
            )
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertIn("ring 0", result.stdout)

    @unittest.skipUnless(
        importlib.util.find_spec("sklearn") is not None,
        "scikit-learn not installed",
    )
    def test_tree_smoke(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            out_path = Path(tmpdir) / "tree.json"
            result = _run(["tree", "--k", "2", "--max-depth", "2", "--out", str(out_path)])
            self.assertEqual(result.returncode, 0, result.stderr)
            data = json.loads(out_path.read_text())
            self.assertIn("center", data)
            self.assertIn("members", data)


if __name__ == "__main__":
    unittest.main()
