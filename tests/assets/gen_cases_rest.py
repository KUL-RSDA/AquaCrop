#!/usr/bin/env python3
"""The remaining buildable plan rows, after the audit of 2026-09-10.

Not here, and why -- each is recorded in TESTPLAN.md against the row:

  covered already   C30 by J33 (AlfOttawaGDD is GDD mode, and the .MAN already
                    generates on a GDD interval), N17 by N16a (EvapZmin is the
                    parameter 15, so N16a's EvapZmax of 15 *is* the equal case),
                    X06 by D28, I20 by I11, N28 by every case that stages a
                    .PPn -- the lookup has one level, not two.
  not drivable      C22 needs the coupled temperature arrays, F33 needs finer
                    input than the .CRO's f9.2, Y15/Y16 set WithBeta in code.
  not portable      A12 would put this machine's absolute paths in a reference.
  blocked           D21 by BUG-16, A11 pending a look at path concatenation.
  built, then removed after the freeze of 2026-09-10:
    D23  a one-day climate record. The run hangs before writing any output:
         run.f90 searches the 31-entry climate datasets for the wanted day with
         fourteen unbounded `do while` loops, and a one-day record never
         satisfies them. Written up as BUG-6, which this generalised from a
         decadal-reader problem to the search itself. Revive when the loops are
         bounded; the asset OneDay.{CLI,Tnx,ETo,PLU} is kept for that.
    N02  a .PPn cut short after five records. Aborts at startunit.F90:815 with
         'End of file': the loader does 25 list reads and none carries an
         iostat. Written up as BUG-21. Revive when the reads fall back to the
         built-in defaults; the asset param/Truncated.PPn is kept for that.
  no such input     N27. IniAbstract has no record in the .PPn at all -- the
                    reader jumps from the capillary shape factor straight to the
                    default temperatures and just calls SetSimulParam_IniAbstract(5)
                    (global.f90:8219). The one file that does hold the record,
                    SIMUL/Soil.PAR, has it overwritten with 5 on the next line
                    (global.f90:3279). No input can move it.
"""
from __future__ import annotations

import json
import pathlib

ROOT = pathlib.Path(__file__).resolve().parent.parent
CASES = ROOT / 'cases'
S = ('2014-05-21', '2014-10-31')
BASE = ['MaunaLoa.CO2', 'Ottawa.PPn', 'DEFAULT.CRO', 'DEFAULT.SOL']

CRO = {10: 'Total length of crop cycle in growing degree-days',
       34: 'Calibrated response (%) of stomata stress to ECsw (Range: 0 (none) to +200 (extreme))',
       43: 'Soil surface covered by an individual seedling at 90 % emergence (cm2)',
       72: 'GDDays: from sowing to maturity (length of crop cycle)'}
#: verified against param/Ottawa.PPn record by record, 2026-09-10. Record 14 is
#: SimulParam_RunoffDepth (global.f90:8205); record 16 is the salt diffusion
#: factor, which is NOT what E14 wants.
PPN = {13: 'Depth [cm] of soil profile affected by water extraction by soil evaporation',
       14: 'Considered depth (m) of soil profile for calculation of mean soil water content for CN adjustment'}

# id, tier, cli, soil, crop, slots, cro patch, ppn patch, daily, part, season, desc
K: list[tuple] = [
    # ---- B: phenology -------------------------------------------------
    # B14 already sets a premature end that bites (day 250, against maturity
    # on 28 October). The branch nothing reaches is a premature end that is set
    # but falls after the run ends, so the crop keeps its own last day
    # (global.f90:4812) -- every case without the record takes that same else
    # with the value undefined instead.
    ('B15','T2','Ottawa.CLI','Ottawa.SOL','MaizePremEndLate.CRO',{},{},{},[2],[],S,
     'a premature end dated after the run already stopped'),
    # ---- C: the GDD boundary -----------------------------------------
    # 1332.2 GDD are available over the standard season at Tbase 8 / Tupper 30
    # by method 3; the stock crop asks for 1210. Asking for exactly 1332 puts
    # maturity on the final day of the run.
    ('C15','T1','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{10:'1332',72:'1332'},{},
     [2,7],[],S,'a crop needing exactly the growing degrees the season provides'),
    # ---- D: climate ---------------------------------------------------
    ('D16','T2','(None)','Ottawa.SOL','MaizeCalwpy.CRO',{},{},{},[7],[],S,
     'a project naming no climate file at all'),
    ('D22','T2','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{},{},[7],[],
     ('2015-05-21','2015-10-31'),'a cropping year shifted onto a later year of the record'),
    # ---- E: runoff ----------------------------------------------------
    ('E14','T2','Storm.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{},{14:'0.10'},[1],[],S,
     'runoff judged on the top tenth of a metre'),
    # ---- J: cuttings --------------------------------------------------
    ('J31','T3','Ottawa.CLI','Ottawa.SOL','AlfOttawaGDD.CRO',{'man':'MAN_cut_day1.MAN'},
     {},{},[2],[1],S,'a cut falling on the first day of the cycle'),
    ('J37','T3','Ottawa.CLI','Ottawa.SOL','AlfOttawaGDD.CRO',{'man':'MAN_cut_never.MAN'},
     {},{},[2],[1],S,'a cutting criterion the season never meets'),
    ('J39','T2','Ottawa.CLI','Ottawa.SOL','AlfOttawaGDD.CRO',{'man':'MAN_cut_window30.MAN'},
     {},{},[2],[1],S,'a cutting window clipped to thirty days'),
    ('J43a','T2','Ottawa.CLI','Ottawa.SOL','AlfOttawaGDD.CRO',{'man':'MAN_cut_cgc0.MAN'},
     {},{},[2],[1],S,'regrowth with no increase of the canopy growth coefficient'),
    ('J43b','T2','Ottawa.CLI','Ottawa.SOL','AlfOttawaGDD.CRO',{'man':'MAN_cut_cgc50.MAN'},
     {},{},[2],[1],S,'regrowth with the canopy growth coefficient raised by half'),
    # ---- L: salinity --------------------------------------------------
    ('L17a','T2','Ottawa.CLI','Ottawa.SOL','MaizeSalinity.CRO',{'sw0':'SW0_ece8.SW0'},
     {34:'0'},{},[2,4],[],S,'no stomatal response to the salinity of the soil water'),
    ('L17b','T2','Ottawa.CLI','Ottawa.SOL','MaizeSalinity.CRO',{'sw0':'SW0_ece8.SW0'},
     {34:'200'},{},[2,4],[],S,'an extreme stomatal response to the salinity of the soil water'),
    # ---- W: canopy ----------------------------------------------------
    ('W17','T2','Ottawa.CLI','Ottawa.SOL','AlfOttawaGDD.CRO',
     {'man':'MAN_cut_beforeclosure.MAN'},{},{},[2],[1],S,
     'a cut taken while the canopy is still expanding'),
    ('W28','T1','Ottawa.CLI','Ottawa.SOL','TuberLongLag.CRO',{},{43:'25.00'},{},[2],[],S,
     'a transplanted crop whose seedlings already cover the ground'),
]


def slug(t):
    return ''.join(c if c.isalnum() else '_' for c in t.lower())[:44].strip('_')


def emit(cid, tier, cli, soil, crop, slots, cpatch, ppatch, daily, part,
         season, why, extra='', stage_over=None, ppn='Ottawa.PPn'):
    name = f'{cid}_{slug(why)}'
    d = CASES / name
    d.mkdir(parents=True, exist_ok=True)
    if cli.startswith('('):
        tnx = eto = plu = '(None)'
        clifiles: list[str] = []
    else:
        tnx, eto, plu = (ROOT/'assets'/'climate'/cli).read_text().splitlines()[2:5]
        clifiles = [cli] + [x for x in (tnx, eto, plu) if not x.startswith('(')]
    base = [b for b in BASE if not b.endswith('.PPn')] + ([ppn] if ppn else [])
    stage = stage_over if stage_over is not None else sorted(set(
        base + [soil, crop] + clifiles + list(slots.values())
        + [slots.get('co2', 'MaunaLoa.CO2')]))
    co2 = slots.pop('co2', 'MaunaLoa.CO2')
    slot_lines = ''.join(f'\n      {k}: {v}' for k, v in sorted(slots.items()))
    blocks = []
    if cpatch:
        blocks.append(f'  {crop}:\n' + ''.join(
            f'    {ln}: {json.dumps(f"{v:>10}      : {CRO[ln]}")}\n'
            for ln, v in sorted(cpatch.items())))
    if ppatch:
        blocks.append(f'  {ppn}:\n' + ''.join(
            f'    {ln}: {json.dumps(f"{v:>6}      : {PPN[ln]}")}\n'
            for ln, v in sorted(ppatch.items())))
    patch = ('\npatch:\n' + ''.join(blocks)) if blocks else ''
    (d/'case.yml').write_text(f"""id: {name}
desc: >
  {why}.
  {crop} on {soil}, climate {cli.replace('.CLI','')}, {season[0]} to {season[1]}.
tier: {tier}

stage:
{chr(10).join('  - ' + a for a in stage)}

project:
  name: {cid}
  desc: {json.dumps(f'{cid} - {why}')}
  runs:
    - year: 1
      sim: [{season[0]}, {season[1]}]
      crop: [{season[0]}, {season[1]}]
      cli: {cli}
      tnx: {tnx}
      eto: {eto}
      plu: {plu}
      co2: {co2}
      cro: {crop}
      sol: {soil}{slot_lines}
{patch}
daily: {daily}
particular: {part}
aggregate: 0
rtol: 1.0e-3
{extra}""")
    return name


def two_run_keepswc(cid, tier, why, soils, crops, daily):
    """F27/F28: two runs where the second keeps the first run's profile."""
    name = f'{cid}_{slug(why)}'
    d = CASES / name
    d.mkdir(parents=True, exist_ok=True)
    stage = sorted(set(BASE + ['Ottawa.CLI', 'Ottawa.Tnx', 'Ottawa.ETo',
                               'Ottawa.PLU'] + list(soils) + list(crops)))
    runs = ''
    for i, (sol, cro, season) in enumerate(zip(soils, crops,
                                               [('2014-05-21', '2014-10-31'),
                                                ('2015-05-21', '2015-10-31')])):
        sw0 = '\n      sw0: KeepSWC' if i else ''
        runs += f"""    - year: {i+1}
      sim: [{season[0]}, {season[1]}]
      crop: [{season[0]}, {season[1]}]
      cli: Ottawa.CLI
      tnx: Ottawa.Tnx
      eto: Ottawa.ETo
      plu: Ottawa.PLU
      co2: MaunaLoa.CO2
      cro: {cro}
      sol: {sol}{sw0}
"""
    (d/'case.yml').write_text(f"""id: {name}
desc: >
  {why}.
  Two runs over Ottawa 2014 and 2015; the second keeps the profile the first
  left behind.
tier: {tier}

stage:
{chr(10).join('  - ' + a for a in stage)}

project:
  name: {cid}
  desc: {json.dumps(f'{cid} - {why}')}
  runs:
{runs}
daily: {daily}
particular: []
aggregate: 0
rtol: 1.0e-3
""")
    return name


def main():
    CASES.mkdir(parents=True, exist_ok=True)
    made = [emit(*row) for row in K]

    # ---- N01: no program parameters at all, so the built-in defaults apply.
    # Nothing is staged into PARAM/, so the harness has nothing to rename and
    # the .PPn invariant does not apply (run_tests derives it from the stage
    # list). ListProjectsLoaded.OUT will say the defaults were used.
    made.append(emit('N01', 'T1', 'Ottawa.CLI', 'Ottawa.SOL', 'MaizeGDD.CRO',
                     {}, {}, {}, [1, 2], [], S,
                     'no program parameters, so the built-in defaults are used',
                     ppn=None))

    # ---- F27 / F28: KeepSWC across two runs.
    made.append(two_run_keepswc(
        'F27', 'T2', 'a kept profile whose rooting depth fits the soil',
        ('Ottawa.SOL', 'Ottawa.SOL'), ('MaizeGDD.CRO', 'MaizeGDD.CRO'), [3, 5]))
    made.append(two_run_keepswc(
        'F28', 'T1', 'a kept profile across runs asking for different depths',
        ('GEOM_1p20m.SOL', 'GEOM_1p20m.SOL'),
        ('MaizeGDD.CRO', 'AlfOttawaGDD.CRO'), [3, 5]))

    made.append(mixed_project_list())

    # ---- cases where AquaCrop must stop with a message (revived after the
    # fixes of BUG-6, BUG-16 and BUG-21; they used to hang or crash)
    made.append(emit('D20', 'T3', 'Ottawa.CLI', 'Ottawa.SOL', 'MaizeCalwpy.CRO',
                     {}, {}, {}, [7], [], ('2016-10-01', '2017-02-15'),
                     'a simulation period ending after the record',
                     extra='expect_error: "after the end of the climate file"\n'))
    # D21: simulation and crop both after the record (Ottawa ends 31/12/2016)
    made.append(emit('D21', 'T3', 'Ottawa.CLI', 'Ottawa.SOL', 'MaizeCalwpy.CRO',
                     {}, {}, {}, [7], [], ('2018-05-21', '2018-10-31'),
                     'a simulation period entirely after the record',
                     extra='expect_error: "after the end of the climate file"\n'))
    made.append(emit('D23', 'T3', 'OneDay.CLI', 'Ottawa.SOL', 'MaizeCalwpy.CRO',
                     {}, {}, {}, [7], [], S, 'a one-day climate record',
                     extra='expect_error: "after the end of the climate file"\n'))
    made.append(emit('N02', 'T3', 'Ottawa.CLI', 'Ottawa.SOL', 'MaizeGDD.CRO',
                     {}, {}, {}, [1], [], S, 'a .PPn cut short after five records',
                     ppn='Truncated.PPn',
                     extra='expect_error: "program parameters instead of"\n'))

    print(f'wrote {len(made)} cases')
    for n in made:
        print('  ' + n)




def mixed_project_list():
    """A06: one ListProjects.txt naming a .PRO and a .PRM.

    The case supplies both project files itself, in its own LIST/ directory,
    because the harness only generates a project when LIST/ is empty. Both
    projects run the same season, so the two output sets are comparable; the
    point of the case is the loop in InitializeProject handling the two
    extensions one after the other.
    """
    import sys
    sys.path.insert(0, str(ROOT / 'runner'))
    import harness as H

    why = 'a project list naming a PRO and a PRM together'
    d = CASES / f'A06_{slug(why)}'
    (d / 'LIST').mkdir(parents=True, exist_ok=True)
    run = {'year': 1, 'sim': list(S), 'cli': 'Ottawa.CLI', 'tnx': 'Ottawa.Tnx',
           'eto': 'Ottawa.ETo', 'plu': 'Ottawa.PLU', 'co2': 'MaunaLoa.CO2',
           'cro': 'MaizeGDD.CRO', 'sol': 'Ottawa.SOL'}
    for kind in ('PRO', 'PRM'):
        spec = {'id': f'A06{kind}',
                'project': {'name': f'A06{kind}', 'type': kind,
                            'desc': f'A06 - the {kind} of the mixed list',
                            'runs': [dict(run)]}}
        name, text = H.render_project(spec)
        (d / 'LIST' / name).write_text(text)

    (d / 'case.yml').write_text(f"""id: A06_{slug(why)}
desc: >
  {why}.
  Both projects run MaizeGDD.CRO on Ottawa.SOL over {S[0]} to {S[1]}; the case
  pins that AquaCrop loads a .PRO and a .PRM from one list in a single pass.
  Both project files are carried in this case's own LIST/ directory.
tier: T2

stage:
  - DEFAULT.CRO
  - DEFAULT.SOL
  - MaizeGDD.CRO
  - MaunaLoa.CO2
  - Ottawa.CLI
  - Ottawa.ETo
  - Ottawa.PLU
  - Ottawa.PPn
  - Ottawa.SOL
  - Ottawa.Tnx

list_projects: "A06PRO.PRO\\nA06PRM.PRM\\n"

daily: [1]
particular: []
aggregate: 0
rtol: 1.0e-3
""")
    return d.name

if __name__ == '__main__':
    main()
