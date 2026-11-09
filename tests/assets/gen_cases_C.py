#!/usr/bin/env python3
"""Generate the group C cases: GDD vs calendar days.

Every case enables daily output 2, which carries the GD column, and asserts that
series against `runner/gdd_check.py` -- a hand port of DegreesDay
(global.f90:2449). That checks the arithmetic itself rather than only that
today's run matches yesterday's.

This independent check was validated against the A01 reference before any group C case
existed: method 3 reproduces all 892 of its GD values exactly, while methods 1
and 2 diverge on 15 and 165 days respectively.
"""
from __future__ import annotations

import json
import pathlib
import sys

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent.parent / 'runner'))
from gdd_check import crop_params, cumulative, day_reaching   # noqa: E402

ROOT = pathlib.Path(__file__).resolve().parent.parent
CROPS = ROOT / 'assets' / 'crops'
CLIM = ROOT / 'assets' / 'climate'
CASES = ROOT / 'cases'

GDD_METHOD_LINE = 21      # ".PPn: Default method for the calculation of GDD"

BASE = ['Ottawa.CLI', 'Ottawa.ETo', 'Ottawa.PLU', 'MaunaLoa.CO2',
        'Ottawa.PPn', 'DEFAULT.CRO', 'DEFAULT.SOL', 'Ottawa.SOL']

MAY = ('2014-05-21', '2014-10-31')      # the standard Ottawa maize season
APR = ('2014-04-01', '2014-10-31')      # a cold start: many days with Tmax < Tbase

# id, crop, tnx, season, GDD method, tier, what it is for, crop-file patches
CASES_C: list[tuple] = [
    ('C01', 'MaizeCalwpy.CRO',  'Ottawa.Tnx',      MAY, 3, 'T1',
     'annual maize by calendar days', {}),
    ('C02', 'MaizeGDDwpy.CRO',  'Ottawa.Tnx',      MAY, 3, 'T1',
     'the same maize by growing degree days', {}),
    ('C03', 'AlfOttawaGDD.CRO', 'Ottawa.Tnx',      MAY, 3, 'T1',
     'perennial forage by GDD, single run', {}),
    ('C06', 'MaizeGDDwpy.CRO',  'Ottawa.Tnx',      APR, 1, 'T1',
     'GDD method 1 on a cold-start season', {}),
    ('C07', 'MaizeGDDwpy.CRO',  'Ottawa.Tnx',      APR, 2, 'T1',
     'GDD method 2 on a cold-start season', {}),
    ('C08', 'MaizeGDDwpy.CRO',  'Ottawa.Tnx',      APR, 3, 'T1',
     'GDD method 3 on a cold-start season', {}),
    ('C09', 'MaizeGDDwpy.CRO',  'Ottawa.Tnx',      MAY, 1, 'T2',
     'GDD method 1 on the standard season', {}),
    ('C10', 'MaizeGDDwpy.CRO',  'Ottawa.Tnx',      MAY, 2, 'T2',
     'GDD method 2 on the standard season', {}),
    ('C14', 'MaizeGDDwpy.CRO',  'Ottawa.Tnx',      MAY, 3, 'T1',
     'crop needs more GDD than the season supplies', {10: 5000}),
    ('C16', 'MaizeGDDwpy.CRO',  'Ottawa.Tnx',      ('2014-05-21', '2014-08-15'), 3, 'T1',
     'simulation ends mid-cycle', {}),
    ('C17', 'AlfOttawaGDD.CRO', 'OttawaConst.Tnx', MAY, 3, 'T1',
     'constant 12/28 degC: exactly 15 GDD per day', {}),
    ('C18', 'MaizeGDDwpy.CRO',  'OttawaConst.Tnx', MAY, 3, 'T1',
     'constant 12/28 degC with Tbase 8: exactly 12 GDD per day', {}),
]


def slug(t):
    return ''.join(c if c.isalnum() else '_' for c in t.lower())[:44].strip('_')


def main():
    CASES.mkdir(parents=True, exist_ok=True)
    rows = []
    for cid, crop, tnx, season, method, tier, why, cpatch in CASES_C:
        cp = crop_params(CROPS / crop)
        tb, tu = cp['tbase'], cp['tupper']
        target = cpatch.get(10, cp['gdd_harvest'])
        total = cumulative(CLIM / tnx, season[0], season[1], tb, tu, method)
        mat, acc = (None, 0.0)
        if cp['mode'] == 'GDD':
            mat, acc = day_reaching(CLIM / tnx, season[0], target, tb, tu, method)
            # a crop that never matures inside the window is the point of C14
            if mat and mat.isoformat() > season[1]:
                mat = None

        name = f'{cid}_{slug(why)}'
        d = CASES / name
        d.mkdir(exist_ok=True)

        patches = {'Ottawa.PPn': {GDD_METHOD_LINE:
                   f'      {method}         : Default method for the calculation '
                   f'of growing degree days'}}
        if cpatch:
            patches[crop] = {k: f'  {v}         : Total length of crop cycle in '
                                f'growing degree-days' for k, v in cpatch.items()}

        patch_yaml = ''
        for fn, edits in patches.items():
            patch_yaml += f'  {fn}:\n'
            for ln, txt in edits.items():
                patch_yaml += f'    {ln}: {json.dumps(txt)}\n'

        (d / 'case.yml').write_text(f"""id: {name}
desc: >
  {why}.
  Crop {crop} ({cp['mode']} mode, Tbase {tb} / Tupper {tu} degC),
  climate {tnx}, {season[0]} to {season[1]}, GDD method {method}.
tier: {tier}

stage:
{chr(10).join('  - ' + a for a in BASE)}
  - {crop}
  - {tnx}

project:
  name: {cid}
  desc: {json.dumps(f'{cid} - {why}')}
  runs:
    - year: 1
      sim: [{season[0]}, {season[1]}]
      cli: Ottawa.CLI
      tnx: {tnx}
      eto: Ottawa.ETo
      plu: Ottawa.PLU
      co2: MaunaLoa.CO2
      cro: {crop}
      sol: Ottawa.SOL

patch:
{patch_yaml}
# Daily output 2 carries the GD column (run.f90:4124).
daily: [2]
particular: []
aggregate: 0

# Checked against runner/gdd_check.py, independently of the frozen reference.
expect_gdd:
  tnx: {tnx}
  tbase: {tb}
  tupper: {tu}
  method: {method}

predict:
  cycle_mode: {cp['mode']}
  gdd_target: {target}
  season_gdd_total: {round(total, 1)}
  matures_on: {mat.isoformat() if mat else 'never within the window'}

rtol: 1.0e-3
""")
        rows.append((cid, crop.replace('.CRO', ''), tnx.replace('.Tnx', ''),
                     season[0][5:], method, round(total), target,
                     mat.isoformat() if mat else 'never'))

    print(f'wrote {len(rows)} group-C cases\n')
    print(f"{'id':<5} {'crop':<15} {'clim':<12} {'sown':<6} {'M':>2} "
          f"{'seasonGDD':>10} {'target':>7}  matures")
    print('-' * 82)
    for r in rows:
        print(f'{r[0]:<5} {r[1]:<15} {r[2]:<12} {r[3]:<6} {r[4]:>2} '
              f'{r[5]:>10} {r[6]:>7}  {r[7]}')


if __name__ == '__main__':
    main()
