#!/usr/bin/env python3
"""Bounded byte-identity actuator. Private copies, not an OS sandbox."""
import hashlib
import json
import resource
from pathlib import Path
import subprocess
import sys
import tempfile
import time

ENV = {'LC_ALL': 'C', 'LANG': 'C', 'TZ': 'UTC', 'OPENSSL_CONF': '/dev/null'}
HERE = Path(__file__).resolve().parent


def digest(data):
    return hashlib.sha256(data).hexdigest()


def verified(path, expected):
    path = Path(path)
    if path.is_symlink():
        raise ValueError('symlink input: ' + str(path))
    data = path.read_bytes()
    if digest(data) != expected:
        raise ValueError('basis drift: ' + str(path))
    return data


def basis(manifest):
    for row in manifest['runtime']:
        p = Path(row['path'])
        if str(p.resolve()) != row['resolved']:
            raise ValueError('runtime resolution drift')
        if digest(p.read_bytes()) != row['sha256']:
            raise ValueError('runtime drift: ' + str(p))
    if Path('/etc/ld.so.preload').exists() or Path('/etc/ld.so.preload').is_symlink():
        raise ValueError('unexpected loader preload')
    for row in manifest['code']:
        verified(HERE / row['path'], row['sha256'])
    for row in manifest['sources']:
        verified(HERE / row['copy'], row['sha256'])


def parse_result(path, result):
    if result['exit'] == 0 and result['stdout'] == path + ': OK\n' and not result['stderr']:
        return 'match'
    if (result['exit'] == 1 and result['stdout'] == path + ': FAILED\n'
            and result['stderr'] == '/usr/bin/sha256sum: WARNING: 1 computed checksum did NOT match\n'):
        return 'mismatch'
    return 'check-failed'


def command(data, expected, logical, mode='normal'):
    # Diagnostic controls are explicit calls, never manifest action overrides.
    with tempfile.TemporaryDirectory(prefix='f12-byte-check-') as tmp:
        target = Path(tmp) / 'target'
        if mode != 'missing':
            target.write_bytes(data)
            target.chmod(0o444)
        payload = expected + '  ' + str(target) + '\n'
        started = time.monotonic_ns()
        try:
            r = subprocess.run(['/usr/bin/sha256sum', '--check', '--strict', '-'],
                               input=payload, text=True, capture_output=True,
                               env=ENV, cwd=tmp, timeout=5, close_fds=True,
                               preexec_fn=lambda: resource.setrlimit(resource.RLIMIT_CORE, (0, 0)))
            record = {'exit': r.returncode, 'stdout': r.stdout, 'stderr': r.stderr}
        except subprocess.TimeoutExpired as error:
            record = {'exit': None, 'stdout': str(error.stdout or ''),
                      'stderr': str(error.stderr or ''), 'timeout': True}
        record.update({'elapsed_ns': time.monotonic_ns() - started,
                       'argv': ['/usr/bin/sha256sum', '--check', '--strict', '-'],
                       'stdin': payload, 'logical_stdin': expected + '  ' + logical + '\n',
                       'cwd': tmp, 'environment': ENV,
                       'mapping': {logical: str(target)}, 'input_sha256': digest(data),
                       'kind': 'checksum-command-observation'})
        record['result'] = parse_result(str(target), record)
        for key in ['stdout', 'stderr', 'stdin']:
            record[key + '_sha256'] = digest(record[key].encode())
        return record


def action(manifest_path, name):
    raw = Path(manifest_path).read_bytes()
    manifest = json.loads(raw)
    basis(manifest)
    spec = manifest['actions'][name]
    data = verified(HERE / spec['copy'], spec['snapshot_sha256'])
    record = command(data, spec['expected'], spec['logical'])
    try:
        basis(manifest)
        if Path(manifest_path).read_bytes() != raw:
            raise ValueError('manifest drift')
        stable = True
    except ValueError as error:
        stable = False
        record['refusal'] = str(error)
    record.update({'action': name, 'manifest_sha256': digest(raw), 'basis_stable': stable})
    return record


def freeze():
    root = Path('/home/joe/code')
    directory = HERE / 'fixtures/f12-preparation/inputs'
    directory.mkdir(parents=True, exist_ok=True)
    names = ['futon3/checks/find_organise.clj',
             'futon3/checks/F11-find-comparison-manifest.edn',
             'futon2/holes/labs/wm-contract/runs/separated-risk-certificate/runtime-mass-binding.lean',
             'futon2/holes/labs/wm-contract/runs/separated-risk-certificate/certificate.edn']
    parents = ['one-authority-per-question', 'evidence-to-disposition-once',
               'pin-moves-with-the-population', 'replayable-not-precious', 'done-is-observed-running']
    names += ['futon3/library/apparatus/' + p + '.flexiarg' for p in parents]
    sources = []
    for index, name in enumerate(names):
        data = (root / name).read_bytes()
        target = directory / (str(index) + '.txt')
        target.write_bytes(data)
        sources.append({'logical': str(root / name), 'copy': str(target.relative_to(HERE)),
                        'sha256': digest(data)})
    runtime = []
    for path in ['/usr/bin/sha256sum', '/etc/ld.so.cache',
                 '/lib64/ld-linux-x86-64.so.2', '/lib/x86_64-linux-gnu/libcrypto.so.3',
                 '/lib/x86_64-linux-gnu/libc.so.6', '/usr/bin/python3', '/usr/local/bin/bb']:
        p = Path(path)
        runtime.append({'path': path, 'resolved': str(p.resolve()), 'sha256': digest(p.read_bytes())})
    runtime_paths = {row['path'] for row in runtime}
    for module in list(sys.modules.values()):
        for attr in ['__file__', '__cached__']:
            path = getattr(module, attr, None)
            if path and path.startswith('/usr/lib/python3') and Path(path).is_file() and path not in runtime_paths:
                p = Path(path)
                runtime.append({'path': path, 'resolved': str(p.resolve()), 'sha256': digest(p.read_bytes())})
                runtime_paths.add(path)
    actions = {}
    for name, index, expected in [('F', 0, '64c3abb4a5a8655736ffc6391cbf7cbae17a76cdc3e026cfc3e6adcf0da1d9ce'),
                                  ('S', 2, '305222dea73f92c92bf89166be7423e12d1eae04eb3b40e5067b338911fc6659')]:
        row = sources[index]
        actions[name] = {'logical': row['logical'], 'copy': row['copy'],
                         'snapshot_sha256': row['sha256'], 'expected': expected}
    m = {'schema': 'f12-preparation-build-v1', 'primary_authorized': False,
         'scope': 'byte identity only; not proof or readiness validation',
         'sources': sources, 'runtime': runtime, 'actions': actions,
         'code': [{'path': p, 'sha256': digest((HERE / p).read_bytes())}
                  for p in ['f12_preparation_io.py', 'f12_preparation.clj', 'find_organise.clj']],
         'instances': [['apparatus/one-authority-per-question', 'find-source-pin'],
                       ['apparatus/one-authority-per-question', 'risk-binding-pin'],
                       ['apparatus/evidence-to-disposition-once', 'preparation-integrity'],
                       ['apparatus/pin-moves-with-the-population', 'supporting'],
                       ['apparatus/replayable-not-precious', 'supporting'],
                       ['apparatus/done-is-observed-running', 'supporting']],
         'then_spans': [23, 24], 'authored_stands_on': [], 'admissions': [],
         'score': {'primary_denominator_per_arm': 1, 'transcript_firings': 3,
                   'rule': 'one coherent complete disposition; checksum mismatch is valid evidence',
                   'diagnostics_primary_score': None},
         'environment': ENV, 'baseline': [1, 2, 3, 4, 5, 6],
         'intervention': [2, 1, 3, 4, 5, 6],
         'budget': {'primary_pairs': 1, 'commands_per_arm': 2, 'firings_per_arm': 3,
                    'timeout_seconds': 5, 'retries': 0},
         'isolation': 'private read-only target copies, clean child env, no OS sandbox; runtime drift refuses'}
    (HERE / 'fixtures/f12-preparation/manifest.json').write_text(json.dumps(m, indent=2) + '\n')


if __name__ == '__main__':
    if sys.argv[1:] == ['freeze']:
        freeze()
    elif len(sys.argv) == 4 and sys.argv[1] == 'diagnostic':
        print(json.dumps(action(sys.argv[2], sys.argv[3])))
    else:
        raise SystemExit('Only freeze or diagnostic MANIFEST F|S is enabled; no primary CLI before review')
