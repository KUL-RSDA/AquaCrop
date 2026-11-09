#!/usr/bin/env python3
"""Fill out groups O (output), J (management), D (climate) and B (crop).

Group A is handled separately: most of its remaining rows are error paths whose
exit status has to be observed before a case can assert it.
"""
from __future__ import annotations

import json
import pathlib

ROOT = pathlib.Path(__file__).resolve().parent.parent
CASES = ROOT / 'cases'
SEASON = ('2014-05-21', '2014-10-31')
CORE = ['MaunaLoa.CO2', 'Ottawa.PPn', 'DEFAULT.CRO', 'DEFAULT.SOL']

CRO = {26: 'DayNr Premature end (counting from 1 January of planting year)',
       56: 'Calendar Days: from sowing to flowering',
       57: 'Length of the flowering stage (days)',
       58: 'Crop determinancy linked with flowering',
       46: 'Canopy growth coefficient (CGC): Increase in canopy cover (fraction soil cover per day)',
       47: 'Number of years at which CCx declines to 90 % of its value due to self-thinning',
       48: 'Shape factor of the decline of CCx over the years due to self-thinning'}

#: cases where AquaCrop must stop: id -> text its console output must contain
EXPECT_ERROR = {
    'D18': 'the climate data are not linked to a specific year',
}

# id, tier, cli, soil, crop, slots, cro-patch, daily, particular, aggregate,
# season, raw-SIM, obs, co2, desc
C: list[tuple] = [
    # ================== O: output and reporting =========================
    ('O11','T2','Ottawa.CLI','Ottawa.SOL','AlfOttawaGDD.CRO',{},{},[5,6],[],0,SEASON,None,None,None,
     'per-compartment water and salinity with twelve compartments'),
    ('O12','T2','Ottawa.CLI','GEOM_0p30m.SOL','MaizeGDD.CRO',{},{},[5,6],[],0,SEASON,None,None,None,
     'per-compartment water and salinity with three compartments'),
    ('O13','T3','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{},[],[],0,SEASON,
     ' 1 : Various parameters of the soil water balance\n'
     ' 1 : Various parameters of the soil water balance\n'
     ' 2 : Crop development and production\n',None,None,
     'a daily-output selection listing the same line twice'),
    ('O14','T3','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{},[],[],0,SEASON,
     ' 1 : Various parameters of the soil water balance\n'
     ' 9 : out of range\n 0 : also out of range\n',None,None,
     'a daily-output selection with numbers outside 1-8'),
    ('O19','T3','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{},[1],[],2,
     ('2014-05-21','2014-05-27'),None,None,None,
     'ten-daily aggregation over a season shorter than a decade'),
    ('O20','T2','Ottawa.CLI','Ottawa.SOL','AlfOttawaGDD.CRO',{},{},[1],[],3,
     ('2014-11-01','2015-03-31'),None,None,None,
     'monthly aggregation across a calendar-year boundary'),
    ('O24','T3','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{},[2],[1],0,SEASON,None,None,None,
     'the cuttings report requested for a crop that has none'),
    ('O25','T1','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{},[2],[2],0,SEASON,None,
     'ObsCC.OBS',None,'evaluation against canopy-cover observations'),
    ('O27','T1','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{},[3],[2],0,SEASON,None,
     'ObsSWC.OBS',None,'evaluation against soil-water observations'),
    ('O28','T1','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{},[2,3],[2],0,SEASON,None,
     'ObsAll.OBS',None,'evaluation against canopy, biomass and soil water together'),
    ('O29','T3','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{},[2],[2],0,SEASON,None,
     'ObsOutside.OBS',None,'observations falling outside the simulation period'),
    ('O30','T3','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{},[2],[2],0,SEASON,None,
     'ObsSingle.OBS',None,'a single observation: degenerate statistics'),
    ('O31','T3','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{},[2],[2],0,SEASON,None,
     'ObsMissing.OBS',None,'observations present but every value absent'),
    ('O32','T3','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{},[2],[2],0,SEASON,None,None,None,
     'evaluation requested with no field data supplied'),
    # ================== J: management remainder ==========================
    ('J06','T3','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{'man':'MAN_mulch_no_effect.MAN'},{},
     [1],[],0,SEASON,None,None,None,'full mulch cover with no effect on evaporation'),
    ('J08','T1','Ottawa.CLI','Ottawa.SOL','Maize_EUirr_GDD.CRO',{'man':'MAN_fert0.MAN'},{},
     [2],[],0,SEASON,None,None,None,'soil fertility stress explicitly set to zero'),
    ('J16','T1','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{'man':'MAN_runoff_off_nobunds.MAN'},{},
     [1],[],0,SEASON,None,None,None,'runoff prevented on a field with no bunds'),
    ('J20','T2','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{'man':'MAN_cn_minus10.MAN'},{},
     [1],[],0,SEASON,None,None,None,'field practices lowering the curve number by 10'),
    ('J24','T2','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{'man':'MAN_weeds_negdelta.MAN'},{},
     [2],[],0,SEASON,None,None,None,'weed cover decreasing through mid-season'),
    ('J26','T2','Ottawa.CLI','Ottawa.SOL','AlfOttawaGDD.CRO',{'man':'MAN_weedadj0.MAN'},{},
     [2],[],0,SEASON,None,None,None,'weeds not replacing self-thinned perennial canopy'),
    ('J33','T1','Ottawa.CLI','Ottawa.SOL','AlfOttawaGDD.CRO',{'man':'MAN_cut_intgdd_full.MAN'},{},
     [2],[1],0,SEASON,None,None,None,'cuttings generated on a growing-degree interval'),
    ('J34','T1','Ottawa.CLI','Ottawa.SOL','AlfOttawaGDD.CRO',{'man':'MAN_cut_dryb_full.MAN'},{},
     [2],[1],0,SEASON,None,None,None,'cuttings generated on accumulated dry biomass'),
    ('J35','T1','Ottawa.CLI','Ottawa.SOL','AlfOttawaGDD.CRO',{'man':'MAN_cut_dryy_full.MAN'},{},
     [2],[1],0,SEASON,None,None,None,'cuttings generated on accumulated dry yield'),
    ('J36','T1','Ottawa.CLI','Ottawa.SOL','AlfOttawaGDD.CRO',{'man':'MAN_cut_freshy_full.MAN'},{},
     [2],[1],0,SEASON,None,None,None,'cuttings generated on accumulated fresh yield'),
    ('J38','T2','Ottawa.CLI','Ottawa.SOL','AlfOttawaGDD.CRO',{'man':'MAN_cut_intday_late.MAN'},{},
     [2],[1],0,SEASON,None,None,None,'a cutting window starting on day 60'),
    ('J41','T2','Ottawa.CLI','Ottawa.SOL','AlfOttawaGDD.CRO',{'man':'MAN_cut_harvestend.MAN'},{},
     [2],[1],0,SEASON,None,None,None,'a final harvest taken at crop maturity'),
    ('J42','T2','Ottawa.CLI','Ottawa.SOL','AlfOttawaGDD.CRO',{'man':'MAN_cut_cc10.MAN'},{},
     [2],[1],0,SEASON,None,None,None,'only 10 per cent canopy left after each cut'),
    # ================== D: climate remainder =============================
    ('D13','T2','NoRain.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{},[1,7],[],0,SEASON,None,None,None,
     'a climate file naming no rainfall record'),
    ('D14','T2','NoEto.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{},[1,7],[],0,SEASON,None,None,None,
     'a climate file naming no reference-ET record'),
    ('D17','T2','Agnostic1y.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{},[7],[],0,
     ('1901-05-21','1901-10-31'),None,None,None,
     'a year-agnostic record with a project dated in 1901 to match'),
    # BUG-10: the same record with real dates must be refused
    ('D18','T2','Agnostic1y.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{},[7],[],0,
     ('2016-05-21','2016-10-31'),None,None,None,
     'a year-agnostic record used in a 2016 run'),
    ('D25','T2','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{},[2,7],[],0,SEASON,None,None,
     'FlatCO2.CO2','a flat CO2 record at the 369.41 ppm reference'),
    ('D27','T3','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{},[2,7],[],0,SEASON,None,None,
     'ShortCO2.CO2','a simulation year past the end of the CO2 record'),
    ('D28','T1','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{},[2,7],[],0,SEASON,None,None,
     'FlatCO2.CO2','CO2 exactly at the reference: no adjustment of WP'),
    # ================== B: crop remainder ================================
    ('B10','T2','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{58:'1'},[2],[],0,SEASON,None,None,None,
     'a determinant crop, linked with flowering'),
    ('B11','T2','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{58:'0'},[2],[],0,SEASON,None,None,None,
     'an indeterminant crop, unlinked from flowering'),
    ('B12','T3','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{57:'0'},[2],[],0,SEASON,None,None,None,
     'a flowering stage of zero length'),
    ('B13','T3','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{56:'1'},[2],[],0,SEASON,None,None,None,
     'flowering starting on the first day of the cycle'),
    ('B23','T2','Ottawa.CLI','Ottawa.SOL','AlfOttawaGDD.CRO',{},{47:'3',48:'1.00'},[2],[],0,
     SEASON,None,None,None,'rapid perennial self-thinning: CCx at 90 % after 3 years'),
    ('B25','T3','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{46:'0.50000'},[2],[],0,
     SEASON,None,None,None,'a canopy that closes within days'),
]


#: cases that must be .PRO rather than .PRM. D9: for a single-run .PRM the
#: evaluation reader looks for EvalData1.OUT while the writer produced
#: EvalData.OUT, so any single-run .PRM asking for evaluation dies. The .PRO
#: branch leaves StrNr empty on both sides and works.
AS_PRO = {'O25', 'O27', 'O28', 'O29', 'O30', 'O31'}


def slug(t):
    return ''.join(c if c.isalnum() else '_' for c in t.lower())[:44].strip('_')


def main():
    CASES.mkdir(parents=True, exist_ok=True)
    for (cid, tier, cli, soil, crop, slots, cpatch, daily, part, agg, season,
         raw, obs, co2, why) in C:
        name = f'{cid}_{slug(why)}'
        d = CASES / name
        d.mkdir(exist_ok=True)
        tnx, eto, plu = (ROOT/'assets'/'climate'/cli).read_text().splitlines()[2:5]
        co2f = co2 or 'MaunaLoa.CO2'
        stage = sorted(set(CORE + [cli, soil, crop, co2f]
                           + [x for x in (tnx, eto, plu) if not x.startswith('(')]
                           + ([obs] if obs else []) + sorted(slots.values())))
        slot_lines = ''.join(f'\n      {k}: {v}' for k, v in sorted(slots.items()))
        if obs:
            slot_lines += f'\n      obs: {obs}'
        patch = ''
        if cpatch:
            patch = (f'\npatch:\n  {crop}:\n' + ''.join(
                f'    {ln}: {json.dumps(f"{v:>10}      : {CRO[ln]}")}\n'
                for ln, v in sorted(cpatch.items())))
        raw_block = ''
        if raw:
            raw_block = 'daily_raw: |\n' + ''.join(f'  {l}\n' for l in raw.splitlines())
        (d/'case.yml').write_text(f"""id: {name}
desc: >
  {why}.
  {crop} on {soil}, climate {cli.replace('.CLI','')}, {season[0]} to {season[1]}.
tier: {tier}

stage:
{chr(10).join('  - ' + a for a in stage)}

project:
  name: {cid}
  type: {'PRO' if cid in AS_PRO else 'PRM'}
  desc: {json.dumps(f'{cid} - {why}')}
  runs:
    - year: 1
      sim: [{season[0]}, {season[1]}]
      crop: [{season[0]}, {season[1]}]
      cli: {cli}
      tnx: {tnx}
      eto: {eto}
      plu: {plu}
      co2: {co2f}
      cro: {crop}
      sol: {soil}{slot_lines}
{patch}
{raw_block}daily: {daily}
particular: {part}
aggregate: {agg}
rtol: 1.0e-3
""" + (f'expect_error: {json.dumps(EXPECT_ERROR[cid])}\n'
       if cid in EXPECT_ERROR else ''))
    from collections import Counter
    n = Counter(x[0][0] for x in C)
    print(f'wrote {len(C)} cases: ' + ' '.join(f'{g}={k}' for g, k in sorted(n.items())))


if __name__ == '__main__':
    main()
