"""Recover the exact retained F12 attempt serialization; never execute an arm."""
import hashlib
from pathlib import Path

root = Path(__file__).resolve().parent / 'fixtures/f12-preparation'
source = root / 'primary-attempt-1-2026-09-10.edn.txt'
target = root / 'primary-attempt-1-2026-09-10.recovered.edn'
raw = source.read_bytes()
assert hashlib.sha256(raw).hexdigest() == '7e00351de0433994904c33b7355f5bf13b2873c98ff735c10b5450dab0e3f536'
paths = [b'/home/joe/code/futon3/checks/find_organise.clj',
         b'/home/joe/code/futon2/holes/labs/wm-contract/runs/separated-risk-certificate/runtime-mass-binding.lean']
replacements = [(b'#:{:' + p[1:], b'{"' + p + b'"') for p in paths]
assert raw.count(b'#:{:home/joe/') == 8
out = raw
for old, new in replacements:
    assert out.count(old) == 4
    out = out.replace(old, new)
inverse = out
for old, new in replacements:
    inverse = inverse.replace(new, old)
assert inverse == raw
if target.exists():
    assert target.read_bytes() == out, 'Refuse to overwrite conflicting recovery'
else:
    with target.open('xb') as stream:
        stream.write(out)
print(hashlib.sha256(out).hexdigest(), target.name)
