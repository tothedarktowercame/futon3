import copy
import json
from pathlib import Path
import tempfile
import unittest
import f12_preparation_io as io

class Checks(unittest.TestCase):
    def test_real_commands(self):
        data = b'finite actual checksum fixture\n'
        self.assertEqual('match', io.command(data, io.digest(data), '/logical')['result'])
        self.assertEqual('mismatch', io.command(data, '0' * 64, '/logical')['result'])
        self.assertEqual('check-failed', io.command(data, io.digest(data), '/logical', 'missing')['result'])

    def test_parser(self):
        self.assertEqual('check-failed', io.parse_result('/x', {'exit': 0, 'stdout': '/x: OK\n/x: OK\n', 'stderr': ''}))
        self.assertEqual('check-failed', io.parse_result('/x', {'exit': 1, 'stdout': '', 'stderr': 'missing'}))

    def test_drift_and_symlink(self):
        with tempfile.TemporaryDirectory() as d:
            p = Path(d) / 'x'
            p.write_bytes(b'changed')
            with self.assertRaises(ValueError):
                io.verified(p, io.digest(b'original'))
            link = Path(d) / 'link'
            link.symlink_to(p)
            with self.assertRaises(ValueError):
                io.verified(link, io.digest(b'changed'))

    def test_manifest_identity(self):
        m = json.loads((io.HERE / 'fixtures/f12-preparation/manifest-serialization-successor-proposed.json').read_text())
        io.basis(m)
        changed = copy.deepcopy(m)
        changed['runtime'][0]['sha256'] = '0' * 64
        with self.assertRaises(ValueError):
            io.basis(changed)

if __name__ == '__main__':
    unittest.main()
