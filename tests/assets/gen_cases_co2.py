#!/usr/bin/env python3
"""The CO2 file a run ends up using.

A project run names a CO2 file or says (None). What (None) means is decided in
two places: `InitializeSettings` (`initialsettings.f90`) sets a default once, at
start-up, and `LoadSimulationRunProject` (`tempprocessing.f90` 1.4) reads each
run's own entry. Nothing in the suite exercised that between them, so these
cases pin it down:

  D29  one run, no CO2 file named            -> the default record is used
  D30  two runs, the first names a file,     -> what the second run uses is the
       the second names none                    question: the default, or the
                                                file the first run left behind

D30 is the one that discriminates. FlatCO2 is a constant 369.41 ppm, the
reference concentration at which AquaCrop makes no CO2 adjustment at all, while
MaunaLoa is around 398 ppm in 2014 - so whichever record run 2 ends up on is
visible in its biomass, not merely in the run's description line.

  D31  the default record absent from SIMUL/,  -> the run should not care: it
       while the run names its own file          named a file of its own

D31 guards the start-up read itself (BUG-25, fixed by PR #379 and merged
2026-10-13). Before that fix it did not fail, it died: AquaCrop read
SIMUL/MaunaLoa.CO2 before reading the project, with no iostat, so a run naming
its own CO2 file still needed the default record to be present.
"""
from __future__ import annotations

import pathlib

HERE = pathlib.Path(__file__).resolve().parent
CASES = HERE.parent / 'cases'

CORE = ['MaunaLoa.CO2', 'Ottawa.PPn', 'DEFAULT.CRO', 'DEFAULT.SOL']
CLIM = ['Ottawa.CLI', 'Ottawa.Tnx', 'Ottawa.ETo', 'Ottawa.PLU']
CROP, SOIL = 'MaizeGDD.CRO', 'Ottawa.SOL'


def slug(text):
    out = ''.join(c if c.isalnum() else '_' for c in text.lower())
    return out[:44].rstrip('_')


def run_block(year, y1, co2):
    """One run of the project; co2=None writes no file, which renders as (None)."""
    lines = [f'    - year: {year}',
             f'      sim: [{y1}-05-21, {y1}-10-31]',
             f'      crop: [{y1}-05-21, {y1}-10-31]',
             '      cli: Ottawa.CLI',
             '      tnx: Ottawa.Tnx',
             '      eto: Ottawa.ETo',
             '      plu: Ottawa.PLU']
    if co2:
        lines.append(f'      co2: {co2}')
    lines += [f'      cro: {CROP}', f'      sol: {SOIL}']
    return '\n'.join(lines)


def emit(cid, tier, why, runs, extra_assets=(), without=()):
    """`without` leaves an asset out of the staging list, so the run meets a
    SIMUL/ that does not hold it."""
    name = f'{cid}_{slug(why)}'
    d = CASES / name
    d.mkdir(parents=True, exist_ok=True)
    stage = sorted(set(CORE + CLIM + [CROP, SOIL] + list(extra_assets)) - set(without))
    (d / 'case.yml').write_text(f"""id: {name}
desc: >
  {why}.
  {CROP} on {SOIL}, climate Ottawa, 2014 onwards.
tier: {tier}

stage:
{chr(10).join('  - ' + a for a in stage)}

project:
  name: {cid}
  type: PRM
  desc: "{cid} - {why}"
  runs:
{chr(10).join(runs)}

daily: [2]
particular: []
aggregate: 0
rtol: 1.0e-3
""")
    return name


def main():
    made = [
        emit('D29', 'T2', 'a run naming no CO2 file',
             [run_block(1, 2014, None)]),
        emit('D30', 'T1', 'a run naming no CO2 file after one that did',
             [run_block(1, 2014, 'FlatCO2.CO2'), run_block(2, 2015, None)],
             extra_assets=['FlatCO2.CO2']),
        emit('D31', 'T2', 'the default CO2 record absent while the run names its own',
             [run_block(1, 2014, 'FlatCO2.CO2')], extra_assets=['FlatCO2.CO2'],
             without=['MaunaLoa.CO2']),
    ]
    print('wrote ' + ' '.join(made))


if __name__ == '__main__':
    main()
