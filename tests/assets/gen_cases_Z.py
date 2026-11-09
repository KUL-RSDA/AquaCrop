#!/usr/bin/env python3
"""Group Z equivalence pairs: properties checked by comparing cases, not
against a frozen reference.

Z15 asserts that a project of N runs, with no KeepSWC to couple them, produces
exactly what N separate single-run projects produce. Nothing in a reference diff
can express that -- it is a relationship between cases.
"""
from __future__ import annotations

import json
import pathlib

ROOT = pathlib.Path(__file__).resolve().parent.parent
CASES = ROOT / 'cases'
BASE = ['Ottawa.CLI', 'Ottawa.Tnx', 'Ottawa.ETo', 'Ottawa.PLU', 'MaunaLoa.CO2',
        'Ottawa.PPn', 'DEFAULT.CRO', 'DEFAULT.SOL', 'Ottawa.SOL', 'MaizeGDD.CRO']
SEASONS = (('2014-05-21', '2014-10-31'), ('2015-05-21', '2015-10-31'))


def emit(cid, why, ptype, runs, tier='T1'):
    name = f'{cid}_{"".join(c if c.isalnum() else "_" for c in why.lower())[:44].strip("_")}'
    d = CASES / name
    d.mkdir(parents=True, exist_ok=True)
    rl = []
    for i, (a, b) in enumerate(runs, 1):
        rl += [f'    - year: {i}', f'      sim: [{a}, {b}]', f'      crop: [{a}, {b}]',
               '      cli: Ottawa.CLI', '      tnx: Ottawa.Tnx', '      eto: Ottawa.ETo',
               '      plu: Ottawa.PLU', '      co2: MaunaLoa.CO2',
               '      cro: MaizeGDD.CRO', '      sol: Ottawa.SOL']
    (d/'case.yml').write_text(f"""id: {name}
desc: >
  {why}.
tier: {tier}

stage:
{chr(10).join('  - ' + a for a in BASE)}

project:
  name: {cid}
  type: {ptype}
  desc: {json.dumps(f'{cid} - {why}')}
  runs:
{chr(10).join(rl)}

daily: [1, 2]
particular: []
aggregate: 0
rtol: 1.0e-3
""")
    return name


def main():
    n = [emit('Z15a', 'two seasons as one project of two runs', 'PRM', SEASONS),
         emit('Z15b', 'the first season on its own', 'PRO', (SEASONS[0],)),
         emit('Z15c', 'the second season on its own', 'PRO', (SEASONS[1],))]
    print('wrote 3 equivalence cases: ' + ', '.join(x.split('_')[0] for x in n))


if __name__ == '__main__':
    main()
