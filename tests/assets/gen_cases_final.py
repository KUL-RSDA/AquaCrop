#!/usr/bin/env python3
"""The remaining rows of C, D, E, F, G, H, I, J, K, M, N, W, X and Y.

Nothing structurally new here: these are the variations left over once each
group's mechanisms were covered. The interesting ones are C20/C21 and N21/N22,
which run with no temperature record at all so the model falls back to the
default Tmin/Tmax in the .PPn, and the .SW0 geometry cases (H04-H08) which
exercise TranslateIniLayersToSWProfile against compartment edges it does not
line up with.
"""
from __future__ import annotations

import json
import pathlib

ROOT = pathlib.Path(__file__).resolve().parent.parent
CASES = ROOT / 'cases'
S = ('2014-05-21', '2014-10-31')
WIDE = ('2014-04-01', '2014-11-30')
BASE = ['MaunaLoa.CO2', 'Ottawa.PPn', 'DEFAULT.CRO', 'DEFAULT.SOL']

CRO = {8: 'Base temperature (degC) below which crop development does not progress',
       9: 'Upper temperature (degC) above which crop development no longer increases',
       10: 'Total length of crop cycle in growing degree-days',
       7: 'Soil water depletion factors (p) are adjusted by ETo',
       11: 'Soil water depletion factor for canopy expansion (p-exp) - Upper threshold',
       12: 'Soil water depletion factor for canopy expansion (p-exp) - Lower threshold',
       26: 'DayNr Premature end (counting from 1 January of planting year)',
       37: 'Minimum effective rooting depth (m)',
       38: 'Maximum effective rooting depth (m)',
       39: 'Shape factor describing root zone expansion',
       42: 'Effect of canopy cover in reducing soil evaporation in late season stage'}
PPN = {5: 'Maximum allowable root zone expansion (fixed at 5 cm/day)',
       6: 'Shape factor for effect water stress on root zone expansion',
       7: 'Required soil water content in top soil for germination (% TAW)',
       13: 'Depth [cm] of soil profile affected by water extraction by soil evaporation',
       19: 'Default minimum temperature (degC) if no temperature file is specified',
       20: 'Default maximum temperature (degC) if no temperature file is specified',
       21: 'Default method for the calculation of growing degree days',
       22: 'Daily rainfall is estimated by USDA-SCS procedure',
       23: 'Percentage of effective rainfall'}

#  id, tier, cli, soil, crop, slots, cro, ppn, daily, particular, season, desc
#: Revived after the fixes of BUG-14 and BUG-15 (they used to hang or crash).
#: G13 now runs; C13 and N22 must stop with the message below.
RETIRED_HANG: list[tuple] = []

#: cases where AquaCrop must stop: id -> text its console output must contain
EXPECT_ERROR = {
    'C13': 'the upper temperature is not above the base temperature',
    'N22': 'temperatures in the program parameters are too low',
}

C: list[tuple] = [
    # ---- C: GDD vs calendar ------------------------------------------
    ('C05','T1','Ottawa.CLI','Ottawa.SOL','MaizeCalwpy.CRO',{},{},{},[2],[],S,
     'the calendar twin of the GDD maize, same season'),
    ('C11','T2','Hot.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{},{},[2,7],[],S,
     'air minimum above the crop upper temperature'),
    ('C12','T1','Cold.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{},{21:'2'},[2,7],[],S,
     'Tmin below Tbase below Tmax, where methods 2 and 3 diverge'),
    ('C20','T1','NoTnx.CLI','Ottawa.SOL','MaizeCalwpy.CRO',{},{},{},[2,7],[],S,
     'no temperature record: the calendar crop on default air temperatures'),
    ('C21','T1','NoTnx.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{},{},[2,7],[],S,
     'no temperature record: a GDD crop on default air temperatures'),
    ('C26','T3','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{10:'400'},{},[2],[],S,
     'a GDD target far below the sum of the crop stages'),
    ('C29','T2','Cold.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{},{},[1,2],[],S,
     'a cold season where the transpiration GDD floor binds'),
    # ---- N: program parameters ---------------------------------------
    ('N21','T1','NoTnx.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{},{19:'12.0',20:'28.0'},[2,7],[],S,
     'the default 12/28 degC air temperatures, stated explicitly'),
    ('N23','T1','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{},{21:'1'},[2],[],S,
     'growing-degree method 1 selected in the program parameters'),
    ('N25','T1','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{7:'0'},{},[1,2],[],S,
     'depletion factors not adjusted by reference ET'),
    ('N10','T3','Dry.CLI','Ottawa.SOL','MaizeGDD.CRO',{'sw0':'SW0_atWP.SW0'},{},{7:'95'},[2],[],S,
     'a germination threshold that can never be met'),
    # ---- E: rainfall ---------------------------------------------------
    ('E01','T2','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{},{22:'0',23:'100'},[1],[],S,
     'effective rainfall taken in full'),
    ('E03','T2','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{},{23:'40'},[1],[],S,
     'effective rainfall at 40 per cent'),
    ('E11','T2','Storm.CLI','CN30.SOL','MaizeGDD.CRO',{},{},{},[1],[],S,
     'a curve number of 30: very little runoff'),
    ('E13','T1','Storm.CLI','CN90.SOL','MaizeGDD.CRO',{},{},{},[1],[],S,
     'a curve number of 90: most rainfall runs off'),
    ('E15','T2','Dry.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{},{},[1],[],S,
     'no rainfall for the whole season'),
    # ---- G: groundwater ------------------------------------------------
    ('G07','T3','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{'gwt':'GWT_const_0p0.GWT'},{},{},[1,3],[],S,
     'a water table exactly at the soil surface'),
    ('G10','T2','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{'gwt':'GWT_var_12obs.GWT'},{},{},[1,3],[],S,
     'a water table given by twelve monthly observations'),
    ('G11','T3','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{'gwt':'GWT_var_1obs.GWT'},{},{},[1,3],[],S,
     'a variable water table declared with a single observation'),
    ('G14','T3','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{'gwt':'GWT_var_early.GWT'},{},{},[1,3],[],S,
     'water-table observations ending before the season'),
    ('G18','T2','Ottawa.CLI','PEN_zero_at_0p60.SOL','MaizeGDD.CRO',
     {'gwt':'GWT_const_1p0.GWT'},{},{},[1,3],[],S,
     'a water table below an impermeable horizon'),
    # ---- H: initial conditions ------------------------------------------
    ('H04','T2','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{'sw0':'SW0_1loc.SW0'},{},{},[3],[],S,
     'initial water given as a single location'),
    ('H05','T2','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{'sw0':'SW0_12loc.SW0'},{},{},[3,5],[],S,
     'initial water given at twelve depths'),
    ('H06','T1','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{'sw0':'SW0_offset.SW0'},{},{},[3,5],[],S,
     'initial water at depths that miss every compartment edge'),
    ('H07','T3','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{'sw0':'SW0_shallow.SW0'},{},{},[3,5],[],S,
     'initial water given only for the top of the profile'),
    ('H08','T3','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{'sw0':'SW0_deep.SW0'},{},{},[3,5],[],S,
     'initial water given deeper than the profile'),
    ('H13','T1','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{'sw0':'SW0_ccini.SW0'},{},{},[2],[],S,
     'an initial canopy cover carried in on its own'),
    ('H14','T1','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{'sw0':'SW0_bini.SW0'},{},{},[2],[],S,
     'an initial biomass carried in on its own'),
    ('H15','T1','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{'sw0':'SW0_zrini.SW0'},{},{},[2,3],[],S,
     'an initial rooting depth carried in on its own'),
    # ---- I: irrigation --------------------------------------------------
    ('I02','T2','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{'irr':'IRR_mode0.IRR'},{},{},[1],[],S,
     'an irrigation file declaring rainfed cropping'),
    ('I05','T2','Dry.CLI','Ottawa.SOL','MaizeGDD.CRO',{'irr':'IRR_daily.IRR'},{},{},[1],[],S,
     'an irrigation event on every day of a month'),
    ('I07','T2','Dry.CLI','Ottawa.SOL','MaizeGDD.CRO',{'irr':'IRR_v73.IRR'},{},{},[1],[],S,
     'a manual schedule in the version 7.3 format'),
    ('I11','T2','Dry.CLI','Ottawa.SOL','MaizeGDD.CRO',{'irr':'IRR_gen_fixint1.IRR'},{},{},[1],[],S,
     'generated irrigation on a one-day interval'),
    ('I13','T2','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{'irr':'IRR_gen_never.IRR'},{},{},[1],[],S,
     'a depletion threshold that is never reached'),
    ('I16','T2','Ottawa.CLI','KSAT_5.SOL','MaizeGDD.CRO',
     {'irr':'IRR_gen_bunds.IRR','man':'MAN_bund30.MAN'},{},{},[1],[],S,
     'generated irrigation keeping water between bunds'),
    ('I35','T1','Dry.CLI','KSAT_5.SOL','MaizeGDD.CRO',{'irr':'IRR_daily.IRR'},{},{},[1],[],S,
     'daily irrigation exceeding the layer-1 infiltration rate'),
    ('I37','T1','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',
     {'irr':'IRR_manual_dense.IRR','man':'MAN_bund30.MAN'},{},{},[1],[],S,
     'irrigation applied to a bunded field'),
    ('I38','T1','DryConst.CLI','Ottawa.SOL','MaizeGDD.CRO',{'irr':'IRR_gen_lastday10.IRR'},{},{},
     [1,2],[],S,'last 10 days unirrigated, GDD, constant 12/28 degC'),
    ('I39','T1','DryEto5.CLI','Ottawa.SOL','MaizeGDD.CRO',{'irr':'IRR_gen_lastday10.IRR'},{},{},
     [1,2],[],S,'last 10 days unirrigated, GDD, Ottawa temperatures'),
    ('I40','T1','DryEto5.CLI','Ottawa.SOL','MaizeCalwpy.CRO',{'irr':'IRR_gen_lastday10.IRR'},{},{},
     [1,2],[],S,'last 10 days unirrigated, calendar crop'),
    # ---- K: off-season ---------------------------------------------------
    ('K07','T2','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{'offseason':'OFF_5before.OFF'},{},{},
     [1],[],WIDE,'five irrigation events before the season'),
    ('K09','T2','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{'offseason':'OFF_5after.OFF'},{},{},
     [1],[],WIDE,'five irrigation events after the season'),
    ('K12','T2','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{'offseason':'OFF_post_ecw.OFF'},{},{},
     [1,4],[],WIDE,'saline irrigation water after the season'),
    # ---- F: rooting and expansion ----------------------------------------
    ('F46','T2','Ottawa.CLI','GEOM_0p30m.SOL','MaizeGDD.CRO',{},{37:'0.50',38:'0.50'},{},[1,3],[],S,
     'a minimum rooting depth deeper than the profile'),
    ('F47','T3','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{37:'0.80',38:'0.80'},{},[1,3],[],S,
     'no root expansion: Zrmin equals Zrmax'),
    ('F50','T2','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{39:'25'},{},[1,3],[],S,
     'a steep root-expansion shape factor'),
    ('F51','T1','Sparse.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{},{6:'6'},[1,3],[],S,
     'root expansion limited by water stress'),
    ('F52','T2','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{},{5:'1.00'},[1,3],[],S,
     'root expansion capped at 1 cm per day'),
    # ---- W / X / Y remainder ---------------------------------------------
    ('W04','T1','Dry.CLI','Ottawa.SOL','MaizeGDD.CRO',{'sw0':'DryTopSoil.SW0'},{},{},[2],[],S,
     'emergence delayed by a dry seedbed'),
    ('W07','T1','Ottawa.CLI','Ottawa.SOL','MaizeSalinity.CRO',{'sw0':'SW0_ece8.SW0'},{},{},[2,4],[],S,
     'canopy expansion limited by salinity'),
    ('W10','T1','Sparse.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{11:'0.10',12:'0.20'},{},[2],[],S,
     'early senescence triggered by water stress'),
    ('W12','T1','Storm.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{11:'0.10',12:'0.20'},{},[2],[],S,
     'senescence interrupted by heavy rewetting'),
    ('W29','T2','Wet.CLI','KSAT_5.SOL','MaizeGDD.CRO',{'sw0':'SW0_atSAT.SW0'},{},{},[2],[],S,
     'canopy development under aeration stress'),
    ('X01','T1','Wet.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{},{},[2],[],S,
     'unstressed biomass accumulation'),
    ('X04','T1','Ottawa.CLI','Ottawa.SOL','MaizeSalinity.CRO',{'sw0':'SW0_ece8.SW0'},{},{},[2,4],[],S,
     'biomass accumulation under salinity stress'),
    ('X12','T1','Ottawa.CLI','Ottawa.SOL','tuber.CRO',{},{},{},[2],[],S,
     'the harvest index of a tuber crop'),
    ('X13','T1','Ottawa.CLI','Ottawa.SOL','veg.CRO',{},{},{},[2],[],S,
     'the harvest index of a leafy vegetable'),
    ('X23','T2','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{26:'250'},{},[2],[],S,
     'biomass accumulation cut short by a premature end date'),
    ('X24','T2','Dry.CLI','Ottawa.SOL','MaizeGDD.CRO',{'sw0':'SW0_atWP.SW0'},{},{},[2],[],S,
     'a crop that produces no biomass at all'),
    ('Y01','T1','Ottawa.CLI','Ottawa.SOL','Maize_EUirr_GDD.CRO',{'man':'MAN_fert0.MAN'},{},{},
     [2],[],S,'the stress engine with fertility stress at zero'),
    ('Y11','T1','Ottawa.CLI','Ottawa.SOL','MaizeSalinity.CRO',
     {'man':'MAN_fert75.MAN','sw0':'SW0_ece8.SW0'},{},{},[2,4],[],S,
     'fertility and salinity stress driving the engine together'),
    ('Y12','T1','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{},{},[1,2],[],S,
     'depletion thresholds adjusted at a low evaporative demand'),
    ('Y13','T1','EtoHigh.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{},{},[1,2],[],S,
     'depletion thresholds adjusted at a high evaporative demand'),
    ('Y18','T3','Sparse.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{11:'0.50',12:'0.50'},{},[2],[],S,
     'a degenerate expansion band: upper equals lower threshold'),
    # ---- revived (BUG-14, BUG-15) ----------------------------------------
    ('G13','T3','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{'gwt':'GWT_var_late.GWT'},{},{},[1,3],[],S,
     'water-table observations beginning after the season'),
    ('C13','T3','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{9:'8.0'},{},[2],[],S,
     'Tbase equals Tupper, so the crop cannot develop'),
    ('N22','T3','NoTnx.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{},{19:'2.0',20:'6.0'},[2,7],[],S,
     'default air temperatures below Tbase'),
    # ---- M ---------------------------------------------------------------
    ('M03','T3','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{'cal':'Ghost.CAL'},{},{},[1],[],S,
     'a calendar file named but not present - never opened, so harmless'),
]


def slug(t):
    return ''.join(c if c.isalnum() else '_' for c in t.lower())[:44].strip('_')


def main():
    CASES.mkdir(parents=True, exist_ok=True)
    for cid, tier, cli, soil, crop, slots, cpatch, ppatch, daily, part, season, why in C:
        name = f'{cid}_{slug(why)}'
        d = CASES / name
        d.mkdir(exist_ok=True)
        tnx, eto, plu = (ROOT/'assets'/'climate'/cli).read_text().splitlines()[2:5]
        stage = sorted(set(BASE + [cli, soil, crop]
                           + [x for x in (tnx, eto, plu) if not x.startswith('(')]
                           + [v for v in slots.values() if not v.startswith('Ghost')]))
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
      crop: [{S[0]}, {S[1]}]
      cli: {cli}
      tnx: {tnx}
      eto: {eto}
      plu: {plu}
      co2: MaunaLoa.CO2
      cro: {crop}
      sol: {soil}{slot_lines}
{patch}
daily: {daily}
particular: {part}
aggregate: 0
rtol: 1.0e-3
""" + (f'expect_error: {json.dumps(EXPECT_ERROR[cid])}\n'
       if cid in EXPECT_ERROR else ''))
    from collections import Counter
    n = Counter(x[0][0] for x in C)
    print(f'wrote {len(C)} cases: ' + ' '.join(f'{g}={k}' for g, k in sorted(n.items())))


if __name__ == '__main__':
    main()
