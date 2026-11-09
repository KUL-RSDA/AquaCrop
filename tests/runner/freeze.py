#!/usr/bin/env python3
"""Run cases and write their OUTP_REF — this is what *defines* the reference.

    tests/runner/freeze.py A01           # one case
    tests/runner/freeze.py F             # a group
    tests/runner/freeze.py --all         # everything (asks first)

Refreezing a case that already has an OUTP_REF needs --force, because
overwriting a reference silently is how a regression gets blessed by accident.
The binary's identity is recorded in tests/REFERENCE.txt.
"""
from __future__ import annotations

import argparse
import concurrent.futures
import datetime
import hashlib
import pathlib
import shutil
import subprocess
import sys

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))
import harness as H

GREEN, RED, DIM, OFF = '\033[32m', '\033[31m', '\033[2m', '\033[0m'


def binary_identity(exe: pathlib.Path) -> dict:
    """What produced this binary: its checksum, and the code it was built from.

    The code is the folder holding the binary, not the folder the suite runs
    from: with --exe they are different, and it is the code that the stored
    output belongs to.
    """
    sha = hashlib.sha256(exe.read_bytes()).hexdigest()[:16]
    code = exe.resolve().parent                              # .../src
    try:
        git = subprocess.run(['git', '-C', str(code), 'rev-parse', 'HEAD'],
                             capture_output=True, text=True).stdout.strip()
        branch = subprocess.run(['git', '-C', str(code), 'rev-parse',
                                 '--abbrev-ref', 'HEAD'],
                                capture_output=True, text=True).stdout.strip()
        dirty = subprocess.run(['git', '-C', str(code), 'status', '--porcelain',
                                '--', '.'], capture_output=True, text=True).stdout.strip()
    except Exception:                                        # noqa: BLE001
        git, branch, dirty = '?', '?', ''
    return {'sha256': sha, 'commit': git or '?', 'branch': branch or '?',
            'code': str(code),
            'src_dirty': bool(dirty),
            'mtime': datetime.datetime.fromtimestamp(exe.stat().st_mtime).isoformat(' ', 'seconds')}


def main():
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument('cases', nargs='*')
    ap.add_argument('--all', action='store_true')
    ap.add_argument('--exe', type=lambda p: pathlib.Path(p).resolve(), default=H.EXE)
    ap.add_argument('--work', type=pathlib.Path, default=H.ROOT / 'work')
    ap.add_argument('--force', action='store_true',
                    help='overwrite an OUTP_REF that already exists')
    ap.add_argument('-j', '--jobs', type=int, default=1,
                    help='cases to freeze at once; each owns its working tree')
    ap.add_argument('-y', '--yes', action='store_true', help='skip the confirmation')
    a = ap.parse_args()

    if not a.cases and not a.all:
        sys.exit('name some cases, or pass --all')
    cases = H.discover(a.cases if a.cases else ['all'])

    ident = binary_identity(a.exe)
    print(f'freezing {len(cases)} case(s)')
    print(f"  binary   {a.exe}")
    print(f"  code     {ident['code']}  ({ident['branch']})")
    print(f"  sha256   {ident['sha256']}   commit {ident['commit'][:12]}"
          f"{'  (src/ DIRTY)' if ident['src_dirty'] else ''}")
    existing = [c for c in cases if (c / 'OUTP_REF').is_dir()]
    if existing and not a.force:
        print(f'\n{RED}{len(existing)} case(s) already have a reference{OFF} — '
              f'pass --force to overwrite:\n  ' + ' '.join(c.name for c in existing[:10]))
        sys.exit(1)
    if not a.yes:
        if input('\nproceed? [y/N] ').strip().lower() not in ('y', 'yes'):
            sys.exit('aborted')

    a.work.mkdir(parents=True, exist_ok=True)

    def freeze_one(c):
        """Known-defect cases are skipped: there is no correct output to freeze."""
        """Returns (name, ok, message). Each case owns its working tree, so
        this is safe to run concurrently."""
        try:
            spec = H.load_case(c)
            if spec['known_defect']:
                return c.name, True, f"skipped -- known defect: {spec['known_defect']}"
            if spec['expect_error']:
                return c.name, True, "skipped -- expected to stop with an error"
            work = H.stage(spec, a.work / spec['id'])
            proc = H.run(work, a.exe)
            if proc.returncode != spec['expect_exit']:
                tail = (proc.stdout + proc.stderr).strip().splitlines()[-3:]
                return c.name, False, (f'exit {proc.returncode}'
                                       + ('  |  ' + ' / '.join(tail) if tail else ''))
            produced = sorted(f for f in (work / 'OUTP').iterdir() if f.is_file())
            if not produced:
                return c.name, False, 'produced no output'
            ref = c / 'OUTP_REF'
            shutil.rmtree(ref, ignore_errors=True)
            ref.mkdir()
            for f in produced:
                shutil.copyfile(f, ref / f.name)
            size = sum(f.stat().st_size for f in ref.iterdir())
            return c.name, True, f'{len(produced)} files, {size/1024:.0f} KB'
        except Exception as e:                               # noqa: BLE001
            return c.name, False, f'{type(e).__name__}: {e}'

    ok = bad = 0
    total_bytes = 0

    def report(name, good, msg):
        nonlocal ok, bad
        if good:
            ok += 1
            print(f'{GREEN}+{OFF}  {name:<38} {DIM}{msg}{OFF}')
        else:
            bad += 1
            print(f'{RED}!{OFF}  {name:<38} {msg}')

    if a.jobs > 1:
        with concurrent.futures.ThreadPoolExecutor(a.jobs) as pool:
            for fut in concurrent.futures.as_completed(
                    [pool.submit(freeze_one, c) for c in cases]):
                report(*fut.result())
    else:
        for c in cases:
            report(*freeze_one(c))

    total_bytes = sum(f.stat().st_size for c in cases
                      if (c / 'OUTP_REF').is_dir()
                      for f in (c / 'OUTP_REF').iterdir() if f.is_file())

    stamp = H.ROOT / 'REFERENCE.txt'
    stamp.write_text(
        f"AquaCrop test suite reference\n"
        f"frozen   {datetime.datetime.now().isoformat(' ', 'seconds')}\n"
        f"binary   {a.exe}\n"
        f"sha256   {ident['sha256']}\n"
        f"code     {ident['code']}\n"
        f"branch   {ident['branch']}\n"
        f"commit   {ident['commit']}\n"
        f"src dirty at freeze time: {ident['src_dirty']}\n"
        f"binary mtime: {ident['mtime']}\n")
    print(f'\nfroze {ok}, failed {bad}   references total {total_bytes/1024/1024:.1f} MB'
          f'   (stamp written to {stamp.relative_to(H.REPO)})')
    sys.exit(1 if bad else 0)


if __name__ == '__main__':
    main()
