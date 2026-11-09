#!/usr/bin/env python3
"""Generate groups A (project structure), B (crop types), D (climate records),
M (calendar) and P (stress paths).

These are variations on assets that already exist, so each case is a short
declaration. The interesting ones:

  A  .PRO vs .PRM, multi-run with KeepSWC, simulation period wider than the
     cropping period, a season inside the 2016 leap year, a run crossing a
     calendar-year boundary
  B  all four subkinds and both planting types, using the donated crop files
  D  the 10-daily and monthly aggregations, which are the only cases in the
     suite touching datatype_decadely and datatype_monthly
  M  the .CAL is never opened by the standalone (LoadCropCalendar has no
     callers), so these three cases pin that it is accepted and ignored
  P  stress paths reachable with existing assets
"""
from __future__ import annotations

import json
import pathlib

ROOT = pathlib.Path(__file__).resolve().parent.parent
CASES = ROOT / 'cases'
SIM = ('2014-05-21', '2014-10-31')
COMMON = ['Ottawa.CLI', 'Ottawa.Tnx', 'Ottawa.ETo', 'Ottawa.PLU', 'MaunaLoa.CO2',
          'Ottawa.PPn', 'DEFAULT.CRO', 'DEFAULT.SOL', 'Ottawa.SOL']

# id, tier, crop, extra assets, runs (list of dicts), daily, particular, project type, desc
#: removed because they hang -- see D6. The 10-daily temperature reader has an
#: unbounded search loop; decadal ETo and rain, and monthly temperature, are
#: fine, so only the two cases staging OttawaDec.Tnx are affected.
#: D02 and D08 are back since BUG-6 (the monthly reference climate of a
#: 10-daily temperature record) is fixed.
RETIRED_HANGS: list[tuple] = []

CASES: list[tuple] = [
    # ---- A: project structure --------------------------------------------
    ('A02', 'T1', 'MaizeGDD.CRO', [], [dict(sim=SIM)], [1], [], 'PRO',
     'a single-run project as a .PRO'),
    ('A03', 'T1', 'MaizeGDD.CRO', [], [dict(sim=SIM),
                                       dict(sim=('2015-05-21', '2015-10-31'), year=2)],
     [1], [], 'PRM', 'a two-run project over consecutive years'),
    ('A15', 'T1', 'MaizeGDD.CRO', [], [dict(sim=SIM, crop=SIM)], [1], [], 'PRM',
     'simulation period equal to the cropping period'),
    ('A13', 'T1', 'MaizeGDD.CRO', [], [dict(sim=('2014-04-01', '2014-11-30'), crop=SIM)],
     [1], [], 'PRM', 'simulation period wider than the cropping period at both ends'),
    ('A18', 'T1', 'MaizeGDD.CRO', [], [dict(sim=('2016-05-21', '2016-10-31'), year=1)],
     [1], [], 'PRM', 'a season inside the 2016 leap year'),
    ('A17', 'T2', 'AlfOttawaGDD.CRO', [], [dict(sim=('2014-11-01', '2015-05-31'))],
     [1], [], 'PRM', 'a run crossing a calendar-year boundary'),
    ('A21', 'T1', 'AlfOttawaGDD.CRO', [], [dict(sim=('2014-05-21', '2014-10-31')),
                                           dict(sim=('2014-11-01', '2015-10-24'),
                                                crop=('2015-05-01', '2015-10-24'),
                                                year=2, sw0='KeepSWC')],
     [1], [], 'PRM', 'perennial over two runs carrying the profile forward'),
    # ---- B: crop type and planting ---------------------------------------
    ('B01', 'T1', 'veg.CRO', [], [dict(sim=SIM)], [2], [], 'PRM',
     'a leafy vegetable: subkind vegetative, transplanted'),
    ('B03', 'T1', 'tuber.CRO', [], [dict(sim=SIM)], [2], [], 'PRM',
     'a potato: subkind tuber, transplanted'),
    ('B03b', 'T2', 'tuberwpy.CRO', [], [dict(sim=SIM)], [2], [], 'PRM',
     'the same tuber with WP reduced during yield formation'),
    ('B06', 'T1', 'rice_test.CRO', [], [dict(sim=SIM)], [2], [], 'PRM',
     'transplanted rice: subkind grain, fertility calibrated at 60 %'),
    ('B09', 'T1', 'Maize_EUirr_cal.CRO', [], [dict(sim=SIM)], [2], [], 'PRM',
     'an annual with no calibrated fertility response'),
    ('B09b', 'T1', 'Maize_EUirr_GDD.CRO', [], [dict(sim=SIM)], [2], [], 'PRM',
     'the same crop calibrated at 30 % fertility stress'),
    ('B26', 'T2', 'MaizeSalinity.CRO', [], [dict(sim=SIM)], [2], [], 'PRM',
     'a salinity-sensitive crop: ECn 0, ECx 6, distortion 75 %'),
    ('B26b', 'T2', 'MaizeSalinityGDD.CRO', [], [dict(sim=SIM)], [2], [], 'PRM',
     'its GDD twin'),
    # ---- D: climate record types ------------------------------------------
    ('D02', 'T1', 'MaizeGDD.CRO', ['OttawaDec.Tnx'], [dict(sim=SIM, tnx='OttawaDec.Tnx')],
     [7], [], 'PRM', '10-daily temperature records'),
    ('D08', 'T2', 'MaizeGDD.CRO', ['OttawaDec.Tnx', 'OttawaMon.ETo', 'OttawaDec.PLU'],
     [dict(sim=SIM, tnx='OttawaDec.Tnx', eto='OttawaMon.ETo', plu='OttawaDec.PLU')],
     [7], [], 'PRM', 'a mix of daily, 10-daily and monthly records'),
    ('D03', 'T1', 'MaizeGDD.CRO', ['OttawaMon.Tnx'], [dict(sim=SIM, tnx='OttawaMon.Tnx')],
     [7], [], 'PRM', 'monthly temperature records'),
    ('D04', 'T1', 'MaizeGDD.CRO', ['OttawaDec.ETo'], [dict(sim=SIM, eto='OttawaDec.ETo')],
     [7], [], 'PRM', '10-daily reference ET records'),
    ('D05', 'T1', 'MaizeGDD.CRO', ['OttawaMon.ETo'], [dict(sim=SIM, eto='OttawaMon.ETo')],
     [7], [], 'PRM', 'monthly reference ET records'),
    ('D06', 'T1', 'MaizeGDD.CRO', ['OttawaDec.PLU'], [dict(sim=SIM, plu='OttawaDec.PLU')],
     [7], [], 'PRM', '10-daily rainfall records'),
    ('D07', 'T1', 'MaizeGDD.CRO', ['OttawaMon.PLU'], [dict(sim=SIM, plu='OttawaMon.PLU')],
     [7], [], 'PRM', 'monthly rainfall records'),
    ('D08b', 'T2', 'MaizeGDD.CRO', ['OttawaMon.Tnx', 'OttawaMon.ETo', 'OttawaMon.PLU'],
     [dict(sim=SIM, tnx='OttawaMon.Tnx', eto='OttawaMon.ETo', plu='OttawaMon.PLU')],
     [7], [], 'PRM', 'every record monthly'),
    # ---- M: calendar --------------------------------------------------------
    ('M01', 'T2', 'MaizeGDD.CRO', ['21May.CAL'], [dict(sim=SIM, cal='21May.CAL')],
     [1], [], 'PRM', 'a .CAL referenced in the project - accepted and never opened'),
    ('M02', 'T2', 'MaizeGDD.CRO', [], [dict(sim=SIM)], [1], [], 'PRM',
     'no calendar file in the project'),
    # ---- P: stress paths ----------------------------------------------------
    ('P07', 'T1', 'MaizeGDD.CRO', [], [dict(sim=('2014-04-01', '2014-10-31'),
                                            crop=('2014-04-01', '2014-10-31'))],
     [2], [], 'PRM', 'sowing in early April: cold stress on a young canopy'),
    ('P16', 'T2', 'MaizeGDD.CRO', [], [dict(sim=('2014-08-01', '2014-10-31'),
                                            crop=('2014-08-01', '2014-10-31'))],
     [2], [], 'PRM', 'a late August sowing that cannot finish the cycle'),
    ('P11', 'T1', 'MaizeGDD.CRO', ['KSAT_5.SOL'], [dict(sim=SIM, sol='KSAT_5.SOL')],
     [1], [], 'PRM', 'near-impermeable soil: waterlogging and aeration stress'),
]


def slug(t):
    return ''.join(c if c.isalnum() else '_' for c in t.lower())[:44].strip('_')


def main():
    CASES_DIR = ROOT / 'cases'
    CASES_DIR.mkdir(parents=True, exist_ok=True)
    for cid, tier, crop, extra, runs, daily, part, ptype, why in CASES:
        name = f'{cid}_{slug(why)}'
        d = CASES_DIR / name
        d.mkdir(exist_ok=True)
        stage = COMMON + [crop] + extra
        rlines = []
        for r in runs:
            sim = r['sim']
            crp = r.get('crop', sim)
            rlines.append(f"    - year: {r.get('year', 1)}")
            rlines.append(f"      sim: [{sim[0]}, {sim[1]}]")
            rlines.append(f"      crop: [{crp[0]}, {crp[1]}]")
            for k, dflt in (('cli', 'Ottawa.CLI'), ('tnx', 'Ottawa.Tnx'),
                            ('eto', 'Ottawa.ETo'), ('plu', 'Ottawa.PLU'),
                            ('co2', 'MaunaLoa.CO2'), ('cro', crop),
                            ('sol', 'Ottawa.SOL')):
                rlines.append(f"      {k}: {r.get(k, dflt)}")
            for k in ('cal', 'sw0'):
                if k in r:
                    rlines.append(f"      {k}: {r[k]}")
        (d / 'case.yml').write_text(f"""id: {name}
desc: >
  {why}.
  {crop} on the Ottawa record, Ottawa.SOL.
tier: {tier}

stage:
{chr(10).join('  - ' + a for a in stage)}

project:
  name: {cid}
  type: {ptype}
  desc: {json.dumps(f'{cid} - {why}')}
  runs:
{chr(10).join(rlines)}

daily: {daily}
particular: {part}
aggregate: 0
rtol: 1.0e-3
""")
    print(f'wrote {len(CASES)} cases: ' + ' '.join(
        f'{g}={sum(1 for c in CASES if c[0][0] == g)}' for g in 'ABDMP'))


if __name__ == '__main__':
    main()
