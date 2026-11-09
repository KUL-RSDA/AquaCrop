#!/usr/bin/env python3
"""Compare cases against each other, for properties no single reference can hold.

    tests/runner/check_equivalence.py

Z15 -- a project of N runs, uncoupled by KeepSWC, must produce exactly what N
separate single-run projects produce. The seasonal totals of run i of the
multi-run project are compared against the whole output of standalone project i.
"""
from __future__ import annotations

import pathlib
import sys

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))
import harness as H                                            # noqa: E402

GREEN, RED, OFF = '\033[32m', '\033[31m', '\033[0m'

#: name -> (multi-run case id, [single-run case ids in run order], tolerance)
PAIRS = {
    'Z15 multi-run equals separate runs': ('Z15a', ['Z15b', 'Z15c'], 0.05),
}

SKIP_COLS = {'RunNr'}


def season_rows(case_id: str):
    d = next(pathlib.Path(H.CASES).glob(f'{case_id}_*'), None)
    if d is None or not (d/'OUTP_REF').is_dir():
        return None, f'{case_id}: no frozen reference'
    f = next((d/'OUTP_REF').glob('*season.OUT'), None)
    if f is None:
        return None, f'{case_id}: no season output'
    lines = f.read_text().splitlines()
    hdr, rows = None, []
    for line in lines:
        t = line.split()
        if t[:1] == ['RunNr']:
            hdr = t
            continue
        if hdr and t and t[0].startswith(('Tot', 'Run')) and len(t) >= len(hdr):
            rows.append(dict(zip(hdr, t)))
    return rows, None


def main():
    bad = 0
    for label, (multi, singles, tol) in PAIRS.items():
        mrows, err = season_rows(multi)
        if err:
            print(f'{RED}!{OFF} {label}: {err}'); bad += 1; continue
        if len(mrows) != len(singles):
            print(f'{RED}x{OFF} {label}: {multi} has {len(mrows)} run rows, '
                  f'{len(singles)} single-run cases given'); bad += 1; continue
        diffs = []
        for i, (mrow, sid) in enumerate(zip(mrows, singles), 1):
            srows, err = season_rows(sid)
            if err:
                diffs.append(err); continue
            for k, mv in mrow.items():
                if k in SKIP_COLS or k not in srows[0]:
                    continue
                sv = srows[0][k]
                try:
                    a, b = float(mv), float(sv)
                except ValueError:
                    if mv != sv:
                        diffs.append(f'run {i} {k}: {mv!r} vs {sv!r}')
                    continue
                denom = max(abs(a), abs(b), 1e-9)
                if abs(a - b)/denom > tol:
                    diffs.append(f'run {i} {k}: {a} vs {b}')
        if diffs:
            bad += 1
            print(f'{RED}x{OFF} {label}')
            for x in diffs[:6]:
                print(f'    {x}')
            if len(diffs) > 6:
                print(f'    ... (+{len(diffs)-6} more)')
        else:
            print(f'{GREEN}={OFF} {label}')
    sys.exit(1 if bad else 0)


if __name__ == '__main__':
    main()
