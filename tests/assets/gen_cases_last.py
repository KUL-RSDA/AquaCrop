#!/usr/bin/env python3
"""The last buildable rows: A, B, C, D, E, F, G, H, I, J, K, L, N, O, W, X, Y.

What is deliberately NOT here: rows blocked by an open defect (decadal
temperature by D6, Tbase == Tupper by D15), rows marked unreachable from inputs,
and group Z, whose remaining rows are runner features rather than cases.
"""
from __future__ import annotations

import json
import pathlib

ROOT = pathlib.Path(__file__).resolve().parent.parent
CASES = ROOT / 'cases'
S = ('2014-05-21', '2014-10-31')

#: cases where AquaCrop must stop and say why (BUG-24: a period starting before
#: the climate record used to run on weather shifted by the days it was short)
EXPECT_ERROR = {'D19': 'before the start of the climate'}
WIDE = ('2014-04-01', '2014-11-30')
# crosses the 2016 leap February; the crop grows Ottawa's season in 2016, inside
# the simulation (a crop in 2014 against a 2016 simulation is not a real set-up)
WINTER = ('2016-01-15', '2016-10-31', '2016-05-21', '2016-10-31')
BASE = ['MaunaLoa.CO2', 'Ottawa.PPn', 'DEFAULT.CRO', 'DEFAULT.SOL']

CRO = {11: 'Soil water depletion factor for canopy expansion (p-exp) - Upper threshold',
       12: 'Soil water depletion factor for canopy expansion (p-exp) - Lower threshold',
       36: 'Decline of crop coefficient (%/day) as a result of ageing',
       42: 'Effect of canopy cover in reducing soil evaporation in late season stage',
       45: 'Number of plants per hectare', 50: 'Maximum canopy cover (CCx) in fraction soil cover',
       51: 'Canopy decline coefficient (CDC): Decrease in canopy cover (in fraction per day)',
       61: 'Water Productivity normalized for ETo and CO2 (WP*) (gram/m2)'}
PPN = {3: 'Threshold for green CC below which HI can no longer increase (% cover)',
       11: 'Decrease of p(sen) once early canopy senescence is triggered (% of p(sen))',
       14: 'Considered depth (m) of soil profile for calculation of mean soil water content for CN adjustment',
       15: 'CN is adjusted to Antecedent Moisture Class',
       24: 'Number of showers in a decade for run-off estimate'}

# id, tier, cli, soil, crop, slots, cro, ppn, daily, part, season, desc
#: removed -- a simulation period past the end of the climate record crashes
#: rather than reporting it; see D16. D19, the leading edge, is fine and stays.
RETIRED_D16: list[tuple] = []   # D20 is revived in gen_cases_rest.py (BUG-16)

#: removed -- the .CRO internal calendar block (dormancy onset and end criteria)
#: is read by LoadCrop and never consumed: GetPerennialPeriod_* appears only in
#: global.f90, with no reference in run.f90, simul.f90 or tempprocessing.f90.
#: The standalone takes its dates from the .PRM. Cases varying that block pass
#: without asserting anything, which is worse than not having them.
RETIRED_DEAD = [
    ('B17', 'dormancy break by mean air temperature'),
    ('B19', 'end of growth by mean air temperature'),
    ('B20', 'dormancy break requiring three occurrences'),
]

K: list[tuple] = [
    # ---- C ----------------------------------------------------------
    ('C04','T1','Ottawa.CLI','Ottawa.SOL','AlfOttawaCal.CRO',{},{},{},[2],[],S,
     'a perennial whose cycle is set by calendar days'),
    ('C19','T3','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{},{},[2,7],[],
     ('2014-01-01','2014-06-30'),'a cropping period starting on the first day of the record'),
    # In GDD mode AdjustCalendarCrop -> AdjustCalendarDays overwrites the
    # calendar stages from the GDD ones (tempprocessing.f90:1602, run.f90:8070).
    # C25 runs through that; C24 does not, because a calendar crop never calls
    # it. The pair pins the difference, which is what the GDD refactor changes.
    ('C24','T1','Ottawa.CLI','Ottawa.SOL','MaizeCalwpy.CRO',{},{},{},[2],[],S,
     'a calendar crop, whose stages are used as written'),
    ('C25','T1','Ottawa.CLI','Ottawa.SOL','MaizeGDDwpy.CRO',{},{},{},[2],[],S,
     'a GDD crop, whose calendar stages are derived at run time'),
    # ---- D ----------------------------------------------------------
    ('D09','T2','Dec11.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{},{},[7],[],S,
     'ten-daily records starting on day 11 of a month'),
    ('D10','T2','Dec21.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{},{},[7],[],S,
     'ten-daily records starting on day 21 of a month'),
    ('D11','T2','LeapDec.CLI','Ottawa.SOL','AlfOttawaGDD.CRO',{},{},{},[7],[],WINTER,
     'a ten-daily record across the 2016 leap February'),
    ('D12','T2','LeapMon.CLI','Ottawa.SOL','AlfOttawaGDD.CRO',{},{},{},[7],[],WINTER,
     'a monthly record across the 2016 leap February'),
    ('D15','T2','NoTnx.CLI','Ottawa.SOL','MaizeCalwpy.CRO',{},{},{},[7],[],S,
     'a climate file naming no temperature record'),
    ('D19','T3','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{},{},[7],[],
     ('2013-11-01','2014-08-31'),'a simulation period starting before the record'),
    ('D26','T3','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{'co2':'LateCO2.CO2'},{},{},[2,7],[],S,
     'a CO2 record beginning after the simulation year'),
    # ---- B ----------------------------------------------------------
    ('B02','T1','Ottawa.CLI','Ottawa.SOL','Maize_EUirr_GDD.CRO',{},{},{},[2],[],S,
     'a fruit and grain crop with a full flowering block'),
    ('B08','T1','Ottawa.CLI','Ottawa.SOL','TuberLongLag.CRO',{},{},{},[2],[],S,
     'a transplanted crop with a 35-day recovery'),
    ('B14','T2','Ottawa.CLI','Ottawa.SOL','MaizePremEnd.CRO',{},{},{},[2],[],S,
     'an annual crop with a premature end date'),
    ('W01','T1','Wet.CLI','Ottawa.SOL','MaizeCalwpy.CRO',{},{},{},[2],[],S,
     'an unstressed canopy driven by calendar days'),
    ('W03','T1','Wet.CLI','Ottawa.SOL','MaizeGDDwpy.CRO',{},{},{},[2],[],S,
     'the same canopy driven by growing degrees'),
    ('W14','T2','Sparse.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{51:'0.30000'},{},[2],[],S,
     'a canopy declining to nothing before maturity'),
    ('W16','T1','Ottawa.CLI','Ottawa.SOL','AlfOttawaGDD.CRO',{'man':'MAN_cut_intday_full.MAN'},
     {},{},[2],[1],S,'cuttings following one another closely'),
    ('W19','T2','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{'man':'MAN_weeds_delta.MAN'},{},{},
     [2],[],S,'weed cover increasing through mid-season'),
    ('W20','T2','Ottawa.CLI','Ottawa.SOL','AlfOttawaGDD.CRO',{'man':'MAN_weeds25.MAN'},{},{},
     [2],[],S,'weeds replacing self-thinned perennial canopy'),
    ('W21','T2','Ottawa.CLI','Ottawa.SOL','AlfOttawaGDD.CRO',{},{},{},[2],[],
     ('2014-05-21','2016-10-31'),'perennial self-thinning across three seasons'),
    # ---- F ----------------------------------------------------------
    ('F30','T1','Ottawa.CLI','Ottawa.SOL','AlfOttawaGDD.CRO',{'sw0':'SW0_atWP.SW0'},{},{},
     [3,5],[],S,'compartment expansion with an initial profile supplied'),
    ('F31','T1','Ottawa.CLI','Ottawa.SOL','AlfOttawaGDD.CRO',{'gwt':'GWT_const_1p0.GWT'},{},{},
     [3,5],[],S,'compartment expansion with a water table present'),
    ('F32','T3','Ottawa.CLI','GEOM_1p20m.SOL','MaizeGDD.CRO',{},{},{},[3,5],[],S,
     'a rooting depth differing from the tiled depth by under a millimetre'),
    # ---- I / J / K / L / N / O / E / G / H / X / Y --------------------
    ('I09','T3','Dry.CLI','Ottawa.SOL','MaizeGDD.CRO',{'irr':'IRR_manual_dense.IRR'},{},{},
     [1],[],WIDE,'irrigation events falling outside the cropping period'),
    ('I19','T2','Wet.CLI','Ottawa.SOL','MaizeGDD.CRO',{'irr':'IRR_gen_allraw.IRR'},{},{},
     [1],[],S,'a refill to field capacity on an already wet profile'),
    ('I36','T1','Storm.CLI','KSAT_5.SOL','MaizeGDD.CRO',{'irr':'IRR_manual_dense.IRR'},{},{},
     [1],[],S,'rain and irrigation together exceeding the infiltration rate'),
    ('J17','T3','Storm.CLI','Ottawa.SOL','MaizeGDD.CRO',{'man':'MAN_bund_tiny.MAN'},{},{},
     [1],[],S,'a bund height below the one-millimetre test'),
    ('J25','T2','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{'man':'MAN_weedshape50.MAN'},{},{},
     [2],[],S,'a mid-range weed shape factor'),
    ('J30','T2','Ottawa.CLI','Ottawa.SOL','AlfOttawaGDD.CRO',{'man':'MAN_cuts_single.MAN'},{},{},
     [2],[1],S,'a cutting schedule with a single cut'),
    ('K15','T2','Ottawa.CLI','Ottawa.SOL','AlfOttawaGDD.CRO',{'offseason':'OFF_cover_both.OFF'},
     {},{},[1],[],('2014-05-21','2015-10-24'),'a perennial with an off-season between two seasons'),
    ('L06','T3','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{'sw0':'SW0_ece25.SW0'},{},{},[4,6],[],S,
     'an initial salinity beyond the crop tolerance range'),
    ('L16','T2','Ottawa.CLI','Ottawa.SOL','MaizeSalinity.CRO',{'sw0':'SW0_ece8.SW0'},{},{},
     [2,4],[],S,'a strongly distorted canopy response to salinity'),
    ('N05','T2','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{},{3:'20'},[2],[],S,
     'a high canopy threshold for harvest-index build-up'),
    ('N14','T2','Sparse.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{},{11:'30'},[2],[],S,
     'a large decrease of p(sen) once senescence starts'),
    ('N18','T2','Storm.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{},{14:'1.00'},[1],[],S,
     'curve-number adjustment over the top metre'),
    ('O35','T3','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{},{},[1],[],S,
     'a project name longer than eight characters'),
    ('E06','T2','Dec11.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{},{24:'5'},[1],[],S,
     'daily rainfall estimated from a ten-daily record'),
    ('E16','T2','Storm.CLI','KSAT_5.SOL','MaizeGDD.CRO',{},{},{15:'0'},[1],[],S,
     'a curve number left unadjusted for antecedent moisture'),
    ('G08','T3','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{'gwt':'GWT_above.GWT'},{},{},[1,3],[],S,
     'a water table standing above the soil surface'),
    ('H23','T1','Ottawa.CLI','Ottawa.SOL','AlfOttawaGDD.CRO',{'sw0':'SalineSoil.SW0'},{},{},
     [4,6],[],('2014-05-21','2015-10-24'),'salt carried across a run boundary by KeepSWC'),
    ('X05','T1','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{61:'20.0'},{},[2],[],S,
     'a lower water productivity against the same CO2 record'),
    ('X22','T2','Ottawa.CLI','Ottawa.SOL','AlfOttawaGDD.CRO',{},{},{},[2],[1],S,
     'fresh yield reported alongside dry yield'),
    ('Y09','T1','Sparse.CLI','Ottawa.SOL','Maize_EUirr_GDD.CRO',{'man':'MAN_fert50.MAN'},{},{},
     [2],[],S,'fertility stress recomputed against a stressed season'),
    ('Y14','T1','EtoHigh.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{11:'0.15',12:'0.65'},{},[1,2],[],S,
     'stomatal thresholds adjusted across a high demand season'),
]


def slug(t):
    return ''.join(c if c.isalnum() else '_' for c in t.lower())[:44].strip('_')


def main():
    CASES.mkdir(parents=True, exist_ok=True)
    made = 0
    for cid, tier, cli, soil, crop, slots, cpatch, ppatch, daily, part, season, why in K:
        name = f'{cid}_{slug(why)}'
        d = CASES / name
        d.mkdir(exist_ok=True)
        tnx, eto, plu = (ROOT/'assets'/'climate'/cli).read_text().splitlines()[2:5]
        stage = sorted(set(BASE + [cli, soil, crop]
                           + [x for x in (tnx, eto, plu) if not x.startswith('(')]
                           + list(slots.values()) + [slots.get('co2', 'MaunaLoa.CO2')]))
        co2 = slots.pop('co2', 'MaunaLoa.CO2')
        slot_lines = ''.join(f'\n      {k}: {v}' for k, v in sorted(slots.items()))
        blocks = []
        if cpatch:
            blocks.append(f'  {crop}:\n' + ''.join(
                f'    {ln}: {json.dumps(f"{v:>10}      : {CRO[ln]}")}\n'
                for ln, v in sorted(cpatch.items())))
        if ppatch:
            blocks.append('  Ottawa.PPn:\n' + ''.join(
                f'    {ln}: {json.dumps(f"{v:>6}      : {PPN[ln]}")}\n'
                for ln, v in sorted(ppatch.items())))
        patch = ('\npatch:\n' + ''.join(blocks)) if blocks else ''
        pname = 'LongProjectName' if cid == 'O35' else cid
        (d/'case.yml').write_text(f"""id: {name}
desc: >
  {why}.
  {crop} on {soil}, climate {cli.replace('.CLI','')}, {season[0]} to {season[1]}.
tier: {tier}

stage:
{chr(10).join('  - ' + a for a in stage)}

project:
  name: {pname}
  desc: {json.dumps(f'{cid} - {why}')}
  runs:
    - year: 1
      sim: [{season[0]}, {season[1]}]
      crop: [{(season[2:] or S)[0]}, {(season[2:] or S)[1]}]
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
""" + (f'expect_error: {json.dumps(EXPECT_ERROR[cid])}\n'
       if cid in EXPECT_ERROR else ''))
        made += 1
    print(f'wrote {made} cases')


if __name__ == '__main__':
    main()
