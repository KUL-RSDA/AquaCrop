#!/usr/bin/env python3
"""Group A: project structure, including the error paths.

Half of these deliberately hand AquaCrop something wrong -- a project file that
is not there, a crop file it cannot open, an empty project list. What it does
in response has not been observed before, so their expected exit status is not
known in advance: they are built, frozen, and whichever ones report an error
cleanly keep an `expect_exit`, while any that crash become findings and are
removed. That is the same loop that produced D2, D6, D9 and D10.
"""
from __future__ import annotations

import json
import pathlib

ROOT = pathlib.Path(__file__).resolve().parent.parent
CASES = ROOT / 'cases'
CORE = ['Ottawa.CLI', 'Ottawa.Tnx', 'Ottawa.ETo', 'Ottawa.PLU', 'MaunaLoa.CO2',
        'Ottawa.PPn', 'DEFAULT.CRO', 'DEFAULT.SOL', 'Ottawa.SOL', 'MaizeGDD.CRO']
S = ('2014-05-21', '2014-10-31')


def run(sim, crop=None, year=1, cro='MaizeGDD.CRO', sol='Ottawa.SOL', **kw):
    d = dict(year=year, sim=sim, crop=crop or sim, cli='Ottawa.CLI',
             tnx='Ottawa.Tnx', eto='Ottawa.ETo', plu='Ottawa.PLU',
             co2='MaunaLoa.CO2', cro=cro, sol=sol)
    d.update(kw)
    return d


# id, tier, runs, type, list_projects, extra stage, expect_exit, desc
#: removed -- each crashes rather than reporting the problem, see D11 and D12.
#: The harness supports them (list_projects, and a project naming an unstaged
#: file), so each is one row away from returning once the guard is added.
#: Revived after BUG-11/12/13: A09 and A10 now skip the project, and A07
#: stops with a warning. In the table below, a text in the exit column means
#: AquaCrop must stop and print that text (expect_error).
RETIRED_CRASH: list[tuple] = []

A: list[tuple] = [
    ('A07', 'T3', [run(S)], 'PRM', '\n', [],
     'is empty. AquaCrop cannot read a project list with empty lines',
     'an empty project list'),
    ('A09', 'T3', [run(S, cro='Ghost.CRO')], 'PRM', None, [], 0,
     'a project referencing a crop file that is not there'),
    ('A10', 'T3', [run(S, sol='Ghost.SOL')], 'PRM', None, [], 0,
     'a project referencing a soil file that is not there'),
    ('A04', 'T2', [run((f'{y}-05-21', f'{y}-10-31'), year=i + 1)
                   for i, y in enumerate((2014, 2015, 2016))] * 3 +
     [run(('2016-05-21', '2016-10-31'), year=10)], 'PRM', None, [], 0,
     'a ten-run project across the whole climate record'),
    ('A05', 'T2', [run(S)], 'PRM', 'absent', [], 0,
     'no project list: AquaCrop discovers the project itself'),
    ('A08', 'T3', [run(S)], 'PRM', 'Ghost.PRM\nA08.PRM\n', [], 0,
     'a project list naming a file that is not there'),
    ('A16', 'T3', [run(('2014-05-20', '2014-11-01'), crop=S)], 'PRM', None, [], 0,
     'a simulation period one day longer at each end'),
    ('A19', 'T3', [run(('2016-02-29', '2016-08-31'))], 'PRM', None, [], 0,
     'a run starting on the leap day'),
    ('A20', 'T3', [run(('2014-05-21', '2014-05-21'))], 'PRM', None, [], 0,
     'a run of exactly one day'),
    ('A22', 'T2', [run(('2014-05-21', '2014-10-31'), cro='AlfOttawaGDD.CRO'),
                   run(('2015-05-01', '2015-10-24'), year=2, cro='MaizeGDD.CRO')],
     'PRM', None, ['AlfOttawaGDD.CRO'], 0,
     'a second run using a different crop file'),
    ('A23', 'T3', [run(('2015-05-21', '2015-10-31'), year=1),
                   run(('2014-05-21', '2014-10-31'), year=2)], 'PRM', None, [], 0,
     'runs listed out of chronological order'),
    ('A24', 'T2', [run(('2014-05-21', '2014-07-31'), year=1),
                   run(('2014-09-15', '2014-10-31'), year=2)], 'PRM', None, [], 0,
     'two runs with a gap between them'),
]


def slug(t):
    return ''.join(c if c.isalnum() else '_' for c in t.lower())[:44].strip('_')


def main():
    CASES.mkdir(parents=True, exist_ok=True)
    for cid, tier, runs, ptype, lp, extra, exit_code, why in A:
        name = f'{cid}_{slug(why)}'
        d = CASES / name
        d.mkdir(exist_ok=True)
        stage = sorted(set(CORE + extra))
        rl = []
        for r in runs:
            rl.append(f"    - year: {r['year']}")
            rl.append(f"      sim: [{r['sim'][0]}, {r['sim'][1]}]")
            rl.append(f"      crop: [{r['crop'][0]}, {r['crop'][1]}]")
            for k in ('cli', 'tnx', 'eto', 'plu', 'co2', 'cro', 'sol'):
                rl.append(f"      {k}: {r[k]}")
        exit_line = (f'expect_error: {json.dumps(exit_code)}'
                     if isinstance(exit_code, str) else f'expect_exit: {exit_code}')
        lp_line = ''
        if lp is not None:
            lp_line = (f'list_projects: {json.dumps(lp)}\n' if lp != 'absent'
                       else 'list_projects: absent\n')
        (d / 'case.yml').write_text(f"""id: {name}
desc: >
  {why}.
tier: {tier}

stage:
{chr(10).join('  - ' + a for a in stage)}

project:
  name: {cid}
  type: {ptype}
  desc: {json.dumps(f'{cid} - {why}')}
  runs:
{chr(10).join(rl)}

{lp_line}daily: [1]
particular: []
aggregate: 0
{exit_line}
rtol: 1.0e-3
""")
    print(f'wrote {len(A)} group-A cases')


if __name__ == '__main__':
    main()
