#!/usr/bin/env python3
"""Run AquaCrop test cases and compare their output against the frozen reference.

    tests/runner/run_tests.py                 # every case
    tests/runner/run_tests.py A01 F14         # named cases
    tests/runner/run_tests.py F               # every case in group F
    tests/runner/run_tests.py --tier T0 T1    # by tier

Exit status is 0 only if every selected case passes.
"""
from __future__ import annotations

import argparse
import concurrent.futures
import datetime
import hashlib
import json
import pathlib
import shutil
import sys
import time

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))
import harness as H
import invariants as INV

GREEN, YELLOW, RED, DIM, OFF = '\033[32m', '\033[33m', '\033[31m', '\033[2m', '\033[0m'
MARK = {'pass': (GREEN, '='), 'close': (YELLOW, '~'),
        'fail': (RED, 'x'), 'error': (RED, '!'),
        'known': (YELLOW, 'K'), 'fixed': (GREEN, 'F')}


def _digest(outp: pathlib.Path, skip: int) -> str:
    """Hash of every output file with the timestamp header removed."""
    import hashlib
    h = hashlib.sha256()
    for f in sorted(outp.iterdir()):
        if not f.is_file():
            continue
        h.update(f.name.encode())
        h.update('\n'.join(f.read_text(errors='replace')
                            .splitlines()[skip:]).encode())
    return h.hexdigest()[:16]


def run_one(case_dir: pathlib.Path, work_root: pathlib.Path, exe, rtol=None, ulp=0.0,
            repeat=1):
    """Returns (id, verdict, seconds, messages)."""
    import shutil
    t0 = time.time()
    try:
        spec = H.load_case(case_dir)
        work = H.stage(spec, work_root / spec['id'])
        proc = H.run(work, exe)
        if spec['known_defect']:
            # Documents a defect: failing is the expected outcome.
            if proc.returncode != 0 or not (case_dir / 'OUTP_REF').is_dir():
                return (spec['id'], 'known', time.time() - t0,
                        [f"  known defect: {spec['known_defect']}",
                         f'  exit {proc.returncode}'])
            return (spec['id'], 'fixed', time.time() - t0,
                    [f"  this case is marked as a known defect but now runs "
                     f"clean -- re-check and drop the marker:",
                     f"    {spec['known_defect']}"])
        if spec['expect_error']:
            out = proc.stdout + proc.stderr
            problems = []
            if proc.returncode == 0:
                problems.append('  exit 0, but AquaCrop should have stopped')
            elif (spec['expect_exit'] is not None
                  and proc.returncode != spec['expect_exit']):
                problems.append(f'  exit {proc.returncode}, '
                                f'expected {spec["expect_exit"]}')
            if spec['expect_error'] not in out:
                problems.append(f'  message not found: {spec["expect_error"]}')
            if problems:
                return (spec['id'], 'fail', time.time() - t0, problems
                        + ['  ' + l for l in out.strip().splitlines()[-12:]])
            return spec['id'], 'pass', time.time() - t0, []
        if proc.returncode != spec['expect_exit']:
            return (spec['id'], 'error', time.time() - t0,
                    [f'  exit {proc.returncode}, expected {spec["expect_exit"]}']
                    + ['  ' + l for l in (proc.stdout + proc.stderr).strip()
                       .splitlines()[-12:]])
        if repeat > 1:
            # Z13: the same inputs must give the same bytes every time
            first = _digest(work / 'OUTP', spec['skip_lines'])
            for i in range(2, repeat + 1):
                w2 = H.stage(spec, work_root / f"{spec['id']}__r{i}")
                p2 = H.run(w2, exe)
                if p2.returncode != spec['expect_exit']:
                    return (spec['id'], 'fail', time.time() - t0,
                            [f'  repeat {i}: exit {p2.returncode}'])
                d2 = _digest(w2 / 'OUTP', spec['skip_lines'])
                shutil.rmtree(w2, ignore_errors=True)
                if d2 != first:
                    return (spec['id'], 'fail', time.time() - t0,
                            [f'  not deterministic: run 1 digest {first}, '
                             f'run {i} digest {d2}'])
        ref = case_dir / 'OUTP_REF'
        if not ref.is_dir():
            return (spec['id'], 'error', time.time() - t0,
                    ['  no OUTP_REF — run freeze.py for this case first'])
        verdict, msgs = H.compare(work / 'OUTP', ref,
                                  rtol if rtol is not None else spec['rtol'],
                                  spec['skip_lines'], ulp)
        stages_ppn = any(a.upper().endswith(('.PPN', '.PP1'))
                         for a in spec.get('stage', []))
        # a .MAN declaring non-zero bunds can leave water on the surface
        bunded = False
        for a in spec.get('stage', []):
            if a.upper().endswith('.MAN'):
                f = H.ASSETS / 'man' / a
                if f.is_file():
                    for ln in f.read_text().splitlines():
                        if 'height (m) of soil bunds' in ln:
                            try:
                                bunded = float(ln.split(':')[0]) > 0.001
                            except ValueError:
                                pass
        viol = INV.check(work / 'OUTP', expect_ppn=stages_ppn, bunded=bunded,
                         skip=tuple(spec['skip_invariants']),
                         storage_in=float(spec['surface_storage_in']))
        if viol:
            msgs += ['  invariant violated:'] + viol
            verdict = 'fail'
        if spec['expect_decade']:
            extra = H.check_decade(work / 'OUTP', spec['expect_decade'])
            if extra:
                msgs += extra
                verdict = 'fail'
        if spec['expect_gdd']:
            extra = H.check_gdd(work / 'OUTP', spec['expect_gdd'])
            if extra:
                msgs += extra
                verdict = 'fail'
        pred0 = spec.get('predict') or {}
        if pred0.get('thicknesses'):
            sol = next((a for a in spec.get('stage', [])
                        if a.upper().endswith('.SOL') and 'DEFAULT' not in a), None)
            if sol:
                extra = H.check_profile_sum(work / 'OUTP',
                                            pred0['thicknesses'], sol)
                extra += H.check_compartment_bounds(work / 'OUTP',
                                                    pred0['thicknesses'], sol)
                if pred0.get('soil_rootmax') is not None:
                    extra += H.check_root_zone(work / 'OUTP',
                                               pred0['soil_rootmax'])
                if extra:
                    msgs += extra
                    verdict = 'fail'
        if spec['expect_compartments'] is not None:
            pred = spec.get('predict') or {}
            mids, acc = [], 0.0
            for t in pred.get('thicknesses', []):
                mids.append(acc + t / 2); acc += t
            extra = H.check_compartments(work / 'OUTP', spec['expect_compartments'],
                                         pred.get('total_depth'), mids)
            if extra:
                msgs += extra
                verdict = 'fail'
        return spec['id'], verdict, time.time() - t0, msgs
    except Exception as e:                                   # noqa: BLE001
        return case_dir.name, 'error', time.time() - t0, [f'  {type(e).__name__}: {e}']


def main():
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument('cases', nargs='*', help='case ids, group letters, or nothing for all')
    ap.add_argument('--tier', nargs='+', help='only cases at these tiers')
    ap.add_argument('--exe', type=lambda p: pathlib.Path(p).resolve(), default=H.EXE)
    ap.add_argument('--rtol', type=float, help='override every case tolerance')
    ap.add_argument('--ulp', type=float, default=0.0,
                    help='also accept a difference of this many units in the last printed '
                         'decimal. For comparing two BUILDS of the same code: a value near a '
                         'rounding boundary prints 0.183 from one and 0.182 from the other, '
                         'which no relative tolerance can tell from a real change when the '
                         'value is small. Leave it off when comparing code against code')
    ap.add_argument('--work', type=pathlib.Path, default=H.ROOT / 'work')
    ap.add_argument('--keep', action='store_true', help='keep working trees after a pass')
    ap.add_argument('-j', '--jobs', type=int, default=1,
                    help='cases to run at once (each has its own working tree)')
    ap.add_argument('-q', '--quiet', action='store_true', help='only show failures')
    ap.add_argument('--repeat', type=int, default=1, metavar='N',
                    help='run each case N times and require byte-identical '
                         'output every time (Z13: determinism)')
    ap.add_argument('--shuffle', action='store_true',
                    help='randomise case order; results must not depend on it '
                         '(Z14: no cross-case state leakage)')
    a = ap.parse_args()

    cases = H.discover(a.cases)
    if a.shuffle:
        import random
        random.shuffle(cases)
    if a.tier:
        want = {t.upper() for t in a.tier}
        cases = [c for c in cases if H.load_case(c)['tier'] in want]
    if not cases:
        sys.exit('no cases selected')

    a.work.mkdir(parents=True, exist_ok=True)
    print(f'running {len(cases)} case(s) against {a.exe}')
    print('=' * 60)

    tally, failures = {'pass': 0, 'close': 0, 'fail': 0, 'error': 0,
                       'known': 0, 'fixed': 0}, []
    close_names = []
    records = []
    t0 = time.time()

    def emit(cid, verdict, secs, msgs):
        records.append({'case': cid, 'verdict': verdict, 'seconds': round(secs, 2),
                        'messages': [m.rstrip() for m in msgs]})
        tally[verdict] += 1
        if verdict in ('fail', 'error'):
            failures.append(cid)
        if verdict == 'close':
            close_names.append(cid)
        if a.quiet and verdict in ('pass', 'close', 'known'):
            return
        col, ch = MARK[verdict]
        print(f'{col}{ch}{OFF}  {cid:<38} {DIM}{secs:5.1f}s{OFF}')
        for m in msgs:
            print(m)

    if a.jobs > 1:
        with concurrent.futures.ThreadPoolExecutor(a.jobs) as pool:
            futs = {pool.submit(run_one, c, a.work, a.exe, a.rtol, a.ulp, a.repeat): c
                    for c in cases}
            for f in concurrent.futures.as_completed(futs):
                emit(*f.result())
    else:
        for c in cases:
            emit(*run_one(c, a.work, a.exe, a.rtol, a.ulp, a.repeat))

    if not a.keep:
        for c in cases:
            if c.name not in failures:
                shutil.rmtree(a.work / c.name, ignore_errors=True)

    # A machine-readable record of the run, for tests/compare_suite.ipynb. Passing
    # cases lose their working tree unless --keep, so this is the only place that
    # distinguishes "passed" from "was never run".
    exe = pathlib.Path(a.exe)
    report = {
        'finished': datetime.datetime.now().isoformat(timespec='seconds'),
        'exe': str(exe),
        'exe_sha256': (hashlib.sha256(exe.read_bytes()).hexdigest()[:16]
                       if exe.is_file() else None),
        'rtol_override': a.rtol, 'ulp': a.ulp,
        'kept_passing_trees': bool(a.keep),
        'tally': tally,
        'cases': sorted(records, key=lambda r: r['case']),
    }
    a.work.mkdir(parents=True, exist_ok=True)
    (a.work / 'results.json').write_text(json.dumps(report, indent=1))

    print('=' * 60)
    print(f"pass {tally['pass']}   within-tol {tally['close']}   "
          f"known-defect {tally['known']}   FAIL {tally['fail']}   "
          f"ERROR {tally['error']}   ({time.time() - t0:.1f}s)")
    if close_names:
        print('\nwithin tolerance: ' + ' '.join(close_names))
    if tally['fixed']:
        print(f"{GREEN}{tally['fixed']} known-defect case(s) now pass{OFF} "
              f"-- verify and remove the known_defect marker")
    print(f'report written to {a.work / "results.json"}')
    if failures:
        print('\nfailed: ' + ' '.join(failures))
        print(f'working trees kept under {a.work}')
        sys.exit(1)


if __name__ == '__main__':
    main()
