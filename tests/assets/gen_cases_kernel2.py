#!/usr/bin/env python3
"""Fill out the remaining rows of the kernel and stress groups (P Q R S T U V).

Each entry names the plan row it satisfies. Where a row asks for a defensive
branch that no input can reach -- an internal clamp on a value the model itself
computes, such as R07 "weighted wetness sum < 0" -- it is listed in UNREACHABLE
at the bottom with the reason, rather than faked with a case that does not
actually drive it.
"""
from __future__ import annotations

import json
import pathlib

ROOT = pathlib.Path(__file__).resolve().parent.parent
CASES = ROOT / 'cases'
SEASON = ('2014-05-21', '2014-10-31')
WIDE = ('2014-04-01', '2014-11-30')
CORE = ['MaunaLoa.CO2', 'Ottawa.PPn', 'DEFAULT.CRO', 'DEFAULT.SOL']

CRO = {7: 'Soil water depletion factors (p) are adjusted by ETo',
       11: 'Soil water depletion factor for canopy expansion (p-exp) - Upper threshold',
       12: 'Soil water depletion factor for canopy expansion (p-exp) - Lower threshold',
       14: 'Soil water depletion fraction for stomatal control (p - sto) - Upper threshold',
       16: 'Soil water depletion factor for canopy senescence (p - sen) - Upper threshold',
       19: 'Soil water depletion factor for pollination (p - pol) - Upper threshold',
       20: 'Vol% for Anaerobiotic point (* (SAT - [vol%]) at which deficient aeration occurs *)',
       27: 'Minimum air temperature below which pollination starts to fail (cold stress) (degC)',
       28: 'Maximum air temperature above which pollination starts to fail (heat stress) (degC)',
       29: 'Minimum growing degrees required for full crop transpiration (degC - day)',
       35: 'Crop coefficient when canopy is complete but prior to senescence (KcTr,x)',
       36: 'Decline of crop coefficient (%/day) as a result of ageing, nitrogen deficiency, etc.',
       40: 'Maximum root water extraction (m3water/m3soil.day) in top quarter of root zone',
       41: 'Maximum root water extraction (m3water/m3soil.day) in bottom quarter of root zone',
       42: 'Effect of canopy cover in reducing soil evaporation in late season stage',
       45: 'Number of plants per hectare',
       50: 'Maximum canopy cover (CCx) in fraction soil cover',
       10: 'Total length of crop cycle in growing degree-days'}

PPN = {7: 'Required soil water content in top soil for germination (% TAW)',
       9: 'Number of days after which deficient aeration is fully effective',
       13: 'Depth [cm] of soil profile affected by water extraction by soil evaporation',
       14: 'Considered depth (m) of soil profile for calculation of mean soil water content for CN adjustment',
       16: 'Salt diffusion factor (capacity for salt diffusion in micro pores) [%]',
       17: 'Salt solubility [g/liter]',
       23: 'Percentage of effective rainfall (when input is 10-day/monthly rainfall)',
       12: 'Thickness top soil (cm) in which soil water depletion has to be determined'}

# id, tier, cli, soil, crop, slots, {cro patches}, {ppn patches}, daily, season, desc
K: list[tuple] = [
    # ================= P: stress and failure paths =====================
    ('P01','T1','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{'sw0':'SW0_atWP.SW0'},
     {11:'0.05',12:'0.15'},{},[2],SEASON,'a narrow p-exp band: canopy expansion stressed early'),
    ('P02','T1','Sparse.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{14:'0.20'},{},[1,2],SEASON,
     'a low p-sto threshold: stomata close early'),
    ('P03','T1','Sparse.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{16:'0.30'},{},[2],SEASON,
     'a low p-sen threshold: senescence triggered early'),
    ('P04','T1','Sparse.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{19:'0.30'},{},[2],SEASON,
     'a low p-pol threshold: water stress at pollination'),
    ('P05','T1','Dry.CLI','Ottawa.SOL','MaizeGDD.CRO',{'sw0':'SW0_atWP.SW0'},{},{},[1,2],SEASON,
     'no rain and a dry profile: total crop failure'),
    ('P06','T2','Dry.CLI','Ottawa.SOL','AlfOttawaGDD.CRO',{},{},{},[2],WIDE,
     'a perennial through a rainless season'),
    ('P08','T1','Hot.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{},{},[2,7],SEASON,
     'air above 40 degC: heat stress on pollination'),
    ('P09','T1','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{27:'18',28:'26'},{},[2,7],SEASON,
     'cold and heat pollination thresholds both binding in one season'),
    ('P10','T1','Cold.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{29:'20.0'},{},[2,7],SEASON,
     'too few growing degrees for full transpiration'),
    ('P12','T1','Wet.CLI','KSAT_5.SOL','MaizeGDD.CRO',{'sw0':'SW0_atSAT.SW0'},{20:'1'},
     {9:'1'},[1,2],SEASON,'aeration stress reaching full effect'),
    ('P13','T1','Sparse.CLI','Ottawa.SOL','Maize_EUirr_GDD.CRO',{'man':'MAN_fert75.MAN'},{},{},
     [2],SEASON,'water and fertility stress together'),
    ('P14','T1','Sparse.CLI','Ottawa.SOL','MaizeSalinity.CRO',{'sw0':'SW0_ece8.SW0'},{},{},
     [2,4],SEASON,'water and salinity stress together'),
    ('P15','T1','HotDry.CLI','Ottawa.SOL','MaizeSalinity.CRO',
     {'sw0':'SW0_ece8.SW0','man':'MAN_fert75.MAN'},{},{},[2,4],SEASON,
     'water, fertility, salinity and temperature stress at once'),
    ('P18','T3','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{50:'0.05'},{},[2],SEASON,
     'a degenerate canopy: CCx 0.05'),
    ('P19','T3','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{45:'100'},{},[2],SEASON,
     'a near-empty stand: 100 plants per hectare'),
    ('P20','T2','Dry.CLI','Ottawa.SOL','MaizeGDD.CRO',{'sw0':'SW0_atWP.SW0'},{},{7:'80'},
     [2],SEASON,'germination threshold never satisfied'),
    # ================= Q: drainage ======================================
    ('Q02','T1','Wet.CLI','KSAT_50.SOL','MaizeGDD.CRO',{'sw0':'SW0_atSAT.SW0'},{},{},[1,3],SEASON,
     'drainage capped by the maximum drainable amount'),
    ('Q06','T1','Storm.CLI','FINE_OVER_COARSE.SOL','MaizeGDD.CRO',{'sw0':'SW0_atSAT.SW0'},{},{},
     [1,3,5],SEASON,'excess pushed back up from a slow horizon'),
    ('Q07','T1','Storm.CLI','KSAT_5.SOL','MaizeGDD.CRO',{'sw0':'SW0_atSAT.SW0'},{},{},[1,3,5],SEASON,
     'excess propagating up several compartments'),
    ('Q13','T2','Wet.CLI','KSAT_1500.SOL','MaizeGDD.CRO',{'sw0':'SW0_atSAT.SW0'},{},{},[1,3],SEASON,
     'the fastest drainage the tau curve allows'),
    ('Q16','T2','Wet.CLI','GRAVEL_30.SOL','MaizeGDD.CRO',{'sw0':'SW0_atSAT.SW0'},{},{},[1,3,5],SEASON,
     'drainage through a gravelly profile'),
    ('Q18','T1','Wet.CLI','Ottawa.SOL','MaizeGDD.CRO',{'sw0':'SW0_atSAT.SW0'},{},{},[1,3],SEASON,
     'repeated saturation on consecutive days'),
    ('Q19','T2','Wet.CLI','GEOM_0p30m.SOL','MaizeGDD.CRO',{'sw0':'SW0_atSAT.SW0'},{},{},[1,3,5],SEASON,
     'drainage with only three compartments'),
    # ================= R: runoff and infiltration ========================
    ('R31','T1','Storm.CLI','DEFAULT.SOL','MaizeGDD.CRO',{'sw0':'SW0_atWP.SW0'},{},{},[1,3,5],SEASON,
     '100 mm in one day onto a dry deep profile'),
    ('R32','T1','Storm.CLI','GEOM_0p30m.SOL','MaizeGDD.CRO',{},{},{},[1,3],SEASON,
     '100 mm in one day onto a shallow profile'),
    ('R16','T1','Dry.CLI','KSAT_5.SOL','MaizeGDD.CRO',{'irr':'IRR_manual_dense.IRR'},{},{},
     [1],SEASON,'irrigation alone exceeding the layer-1 infiltration rate'),
    ('R26','T1','Storm.CLI','GEOM_0p30m.SOL','MaizeGDD.CRO',{'sw0':'SW0_atSAT.SW0'},{},{},
     [1,3],SEASON,'more water than the whole profile can store'),
    ('R13','T2','Storm.CLI','Ottawa.SOL','MaizeGDD.CRO',{'man':'MAN_bund30.MAN'},{},{},[1],WIDE,
     'surface storage before the cropping period'),
    ('R14','T2','Storm.CLI','Ottawa.SOL','MaizeGDD.CRO',{'man':'MAN_bund30.MAN'},{},{},[1],WIDE,
     'surface storage after the cropping period'),
    ('R36','T1','Storm.CLI','Ottawa.SOL','MaizeGDD.CRO',{'man':'MAN_bund30.MAN'},{},{},[1],SEASON,
     'effective rainfall while water stands on the surface'),
    ('R40','T2','Storm.CLI','KSAT_5.SOL','MaizeGDD.CRO',{'man':'MAN_bund30.MAN'},{},{},[1],SEASON,
     'sub-drainage above the maximum, on a bunded field'),
    ('R39','T2','Storm.CLI','KSAT_5.SOL','MaizeGDD.CRO',{},{},{},[1],SEASON,
     'sub-drainage above the maximum, with no bunds'),
    ('R06','T2','Dry.CLI','Ottawa.SOL','MaizeGDD.CRO',{'sw0':'SW0_atWP.SW0'},{},{},[1],SEASON,
     'a profile drier than wilting point within the runoff depth'),
    ('R09','T2','Storm.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{},{14:'0.10'},[1],SEASON,
     'the runoff depth reached before the last compartment'),
    ('R10','T2','Storm.CLI','GEOM_0p30m.SOL','MaizeGDD.CRO',{},{},{14:'1.00'},[1],SEASON,
     'the runoff depth deeper than the profile'),
    ('R33','T2','Storm.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{},{},[1],('2014-05-15','2014-10-31'),
     'rain on the first day of the simulation'),
    ('R43','T2','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{},{23:'100'},[1],SEASON,
     'effective rainfall at 100 per cent'),
    # ================= S: evaporation ====================================
    ('S01','T1','Wet.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{50:'0.10'},{},[1],SEASON,
     'a wet surface under almost no canopy: stage-I evaporation'),
    ('S04','T1','Storm.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{},{},[1],SEASON,
     'rewetting after a dry spell resets to stage I'),
    ('S05','T1','Eto5.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{},{},[1,7],SEASON,
     'a constant 5 mm/day, the stage-II reference demand'),
    ('S07','T1','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{},{13:'15'},[1],SEASON,
     'a shallow 0.15 m evaporation layer'),
    ('S10','T1','Ottawa.CLI','EVAP_BOUNDARY.SOL','MaizeGDD.CRO',{},{},{},[1,5],SEASON,
     'the evaporation layer spanning two horizons'),
    ('S12','T1','Dry.CLI','Ottawa.SOL','MaizeGDD.CRO',{'sw0':'SW0_atWP.SW0'},{},{},[1,5],SEASON,
     'the top compartment driven to the evaporation floor'),
    ('S16','T1','Ottawa.CLI','REW_3.SOL','MaizeGDD.CRO',{},{},{},[1],SEASON,
     'REW 3 mm: an early stage-I to stage-II transition'),
    ('S17','T1','Ottawa.CLI','REW_15.SOL','MaizeGDD.CRO',{},{},{},[1],SEASON,
     'REW 15 mm: a late stage-I to stage-II transition'),
    ('S20','T1','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',
     {'man':'OttawaMulch.MAN','irr':'IRR_fw30.IRR'},{},{},[1],SEASON,
     'mulch and partial surface wetting together'),
    ('S21','T2','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',
     {'offseason':'OFF_cover_before.OFF'},{},{},[1],WIDE,'off-season mulch cover'),
    ('S25','T1','EtoHigh.CLI','Ottawa.SOL','MaizeGDD.CRO',{'sw0':'SW0_ece16.SW0'},{},{17:'50'},
     [1,4,6],SEASON,'salt concentrated past its solubility limit'),
    ('S26','T1','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{42:'90'},{},[1,2],SEASON,
     'a strong canopy effect on late-season evaporation'),
    ('S27','T2','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{42:'0'},{},[1,2],SEASON,
     'no canopy effect on late-season evaporation'),
    # ================= T: transpiration ==================================
    ('T01','T1','Wet.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{},{},[1,2],SEASON,
     'unstressed transpiration at full canopy'),
    ('T05','T1','Ottawa.CLI','GEOM_0p30m.SOL','MaizeGDD.CRO',{},{},{},[1,2,5],SEASON,
     'uptake distributed over three compartments'),
    ('T06','T1','Ottawa.CLI','Ottawa.SOL','AlfOttawaGDD.CRO',{},{},{},[1,2,5],SEASON,
     'uptake over twelve graded compartments'),
    ('T10','T1','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{40:'0.020',41:'0.010'},{},[1,2],SEASON,
     'a steep root extraction gradient, top to bottom'),
    ('T11','T1','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{40:'0.015',41:'0.015'},{},[1,2],SEASON,
     'uniform root extraction with depth'),
    ('T15','T2','Wet.CLI','KSAT_5.SOL','MaizeGDD.CRO',{'sw0':'SW0_atSAT.SW0'},{20:'0'},{},
     [1,2],SEASON,'aeration stress switched off at the anaerobic point'),
    ('T16','T1','Wet.CLI','KSAT_5.SOL','MaizeGDD.CRO',
     {'man':'MAN_bund30.MAN','sw0':'SW0_surfstore.SW0'},{},{},[1,2],SEASON,
     'transpiration drawing on standing surface water'),
    ('T19','T2','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{36:'0.000'},{},[1,2],SEASON,
     'no ageing decline of the crop coefficient'),
    ('T20','T1','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{},{},{12:'20'},[1,3],SEASON,
     'a 20 cm top soil for depletion reporting'),
    ('T21','T1','Wet.CLI','Ottawa.SOL','MaizeGDD.CRO',{'sw0':'SW0_atFC.SW0'},{},{},[1,3],SEASON,
     'a root zone starting at field capacity'),
    ('T22','T1','Dry.CLI','Ottawa.SOL','MaizeGDD.CRO',{'sw0':'SW0_atWP.SW0'},{},{},[1,3],SEASON,
     'a root zone starting at wilting point'),
    ('T23','T1','Ottawa.CLI','ROOTZONE_BOUNDARY.SOL','MaizeGDD.CRO',{},{},{},[1,3],SEASON,
     'a root zone spanning two horizons'),
    ('T25','T1','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{'man':'MAN_weeds75.MAN'},{},{},
     [1,2],SEASON,'transpiration under heavy weed competition'),
    ('T26','T1','Dry.CLI','Ottawa.SOL','MaizeGDD.CRO',{'irr':'IRR_fw30.IRR'},{},{},[1,2],SEASON,
     'transpiration with only 30 per cent of the surface wetted'),
    # ================= U: capillary rise =================================
    ('U05','T1','Dry.CLI','KSAT_50.SOL','MaizeGDD.CRO',
     {'gwt':'GWT_const_0p4.GWT','sw0':'SW0_atWP.SW0'},{},{},[1],SEASON,
     'capillary rise hitting its daily upper limit'),
    ('U07','T2','Wet.CLI','Ottawa.SOL','MaizeGDD.CRO',
     {'gwt':'GWT_const_0p4.GWT','sw0':'SW0_atFC.SW0'},{},{},[1],SEASON,
     'no rise into compartments already above the threshold'),
    ('U08','T1','Dry.CLI','GEOM_0p30m.SOL','MaizeGDD.CRO',
     {'gwt':'GWT_const_0p4.GWT','sw0':'SW0_atWP.SW0'},{},{},[1,5],SEASON,
     'capillary rise reaching the surface compartment'),
    ('U11','T2','Dry.CLI','CLAY_LIS.SOL','MaizeGDD.CRO',
     {'gwt':'GWT_const_1p0.GWT','sw0':'SW0_atWP.SW0'},{},{},[1],SEASON,
     'capillary rise in a clay soil class'),
    ('U14','T1','Dry.CLI','Ottawa.SOL','MaizeGDD.CRO',
     {'gwt':'GWT_const_0p4.GWT','sw0':'SW0_atWP.SW0'},{},{},[1,3],SEASON,
     'horizontal inflow into compartments below the water table'),
    ('U16','T1','Dry.CLI','Ottawa.SOL','MaizeGDD.CRO',
     {'gwt':'GWT_const_saline.GWT','sw0':'SW0_atWP.SW0'},{},{},[1,4,6],SEASON,
     'horizontal inflow carrying salt from the water table'),
    ('U18','T2','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{'gwt':'GWT_const_3p0.GWT'},{},{},
     [1,3],SEASON,'a water table below the profile: no FC adjustment'),
    ('U19','T1','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{'gwt':'GWT_const_1p0.GWT'},{},{},
     [1,3,5],SEASON,'a water table inside the profile: graded FC adjustment'),
    ('U20','T1','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{'gwt':'GWT_var_rise_fall.GWT'},{},{},
     [1,3],SEASON,'a water table moving through the season'),
    # ================= V: salt transport =================================
    ('V03','T1','Ottawa.CLI','DEFAULT.SOL','MaizeGDD.CRO',{'sw0':'SalineSoil.SW0'},{},{},
     [4,6],SEASON,'salt transport with four salt cells'),
    ('V04','T1','Ottawa.CLI','KSAT_200.SOL','MaizeGDD.CRO',{'sw0':'SalineSoil.SW0'},{},{},
     [4,6],SEASON,'salt transport with seven salt cells'),
    ('V06','T2','Ottawa.CLI','KSAT_3000.SOL','MaizeGDD.CRO',{'sw0':'SalineSoil.SW0'},{},{},
     [4,6],SEASON,'salt cells clamped to the minimum of two'),
    ('V08','T1','Wet.CLI','Ottawa.SOL','MaizeGDD.CRO',{'sw0':'SW0_ece8.SW0'},{},{},[1,4,6],SEASON,
     'salt washed down the profile by heavy rain'),
    ('V10','T1','Storm.CLI','Ottawa.SOL','MaizeGDD.CRO',{'sw0':'SW0_ece8.SW0'},{},{},[1,4,6],SEASON,
     'salt leaving with deep percolation'),
    ('V11','T1','Dry.CLI','Ottawa.SOL','MaizeGDD.CRO',
     {'gwt':'GWT_const_saline.GWT','sw0':'SW0_atWP.SW0'},{},{},[1,4,6],SEASON,
     'salt entering with capillary rise'),
    ('V15','T2','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{'sw0':'SW0_ece4.SW0'},{},{16:'100'},
     [4,6],SEASON,'full salt diffusion between cells'),
    ('V16','T1','EtoHigh.CLI','Ottawa.SOL','MaizeGDD.CRO',{'sw0':'SW0_ece16.SW0'},{},{17:'50'},
     [4,6],SEASON,'salt precipitating at the solubility limit'),
    ('V17','T1','Wet.CLI','Ottawa.SOL','MaizeGDD.CRO',{'sw0':'SW0_ece16.SW0'},{},{17:'50'},
     [4,6],SEASON,'precipitated salt redissolving under rain'),
    ('V20','T2','Storm.CLI','Ottawa.SOL','MaizeGDD.CRO',
     {'man':'MAN_bund30.MAN','sw0':'SW0_surfstore_saline.SW0'},{},{},[1,4],SEASON,
     'salt held in water ponded on the surface'),
    ('V24','T1','Ottawa.CLI','Ottawa.SOL','MaizeGDD.CRO',{'sw0':'SW0_ece_grad.SW0'},{},{},
     [4,6],SEASON,'ECe to ECsw conversion over a salinity gradient'),
]

UNREACHABLE = {
    'R03': 'Runoff >= Shower is an internal clamp on a value the model computes',
    'R04': 'Runoff > Rain likewise clamps a computed value',
    'R05': 'term <= epsilon depends on the internal initial abstraction',
    'R07': 'SUM < 0 clamps a weighted mean that cannot go negative from inputs',
    'R19': 'delta_theta_nul vs delta_theta_SAT is internal to calculate_infiltration',
    'R20': 'theta_nul <= FCadj is an internal clamp',
    'R21': 'theta_nul > SAT is an internal clamp',
    'R22': 'fluxout + drain_max is internal flux limiting',
    'R23': 'the pre_comp excess walk is reached only via internal state',
    'R24': 'Runoff > RunoffIni is set inside calculate_infiltration',
    'R25': 'the excess < 0 guard is defensive; no input makes excess negative',
    'R27': 'amount_still_to_store <= epsilon is an exact-fit internal branch',
    'R34': 'rain on the last simulated day depends on the record, not the project',
    'R35': 'SubDrain is computed internally, not supplied',
    'R37': 'Zr <= 0 happens only before emergence, inside one routine',
    'R38': 'RestTheta <= 0 is an internal saturated-root-zone guard',
    'R41': 'EffecRain < 0 is a defensive clamp',
    'R42': 'EffecRain > Rain - Runoff is a defensive clamp',
    'R02': 'needs a decadal rain record with runoff; decadal rain works but the '
           'shower branch also needs the record type on the project, covered by D06',
    'Q03': 'theta_x vs SAT sub-branches are internal to calculate_drainage',
    'Q04': 'as Q03',
    'Q08': 'pre_nr == 1 is the terminal step of an internal walk',
    'Q09': 'abs(excess) < epsilon is an exact-zero internal exit',
    'Q14': 'TauFromKsat breakpoints are covered indirectly by the Ksat ladder',
    'S09': 'EvapZmax == EvapZmin is rejected before it reaches the guard',
    'S11': 'needs EvapZmax below one compartment thickness; CompDefThick is fixed',
    'S13': 'the salt-cell walk indices are internal to CalculateSoilEvaporationStage2',
    'S14': 'as S13',
    'S15': 'the compi <= NrCompartments guard is defensive',
    'S23': 'ponded water exhausted mid-day is a within-timestep transition',
    'S28': 'the F44 soil is retired under D5',
    'T07': 'a root zone shallower than compartment 1 needs Zrmin below 0.10 m, '
           'which the crop files do not allow without also changing emergence',
    'T08': 'an exact compartment-boundary root depth cannot be pinned from inputs',
    'T09': 'uptake beyond the profile is prevented by RootMaxInSoilProfile',
    'T12': 'SinkMajor limiting is internal to calculate_transpiration',
    'T13': 'SinkMinor likewise',
    'T17': 'surface water insufficient for Tpot is a within-timestep branch',
    'T18': 'covered by W11 and W11b',
    'U06': 'DTheta >= DThetaMax is an internal per-compartment cap',
    'U09': 'covered by every case using a v7.3 soil file',
    'U10': 'needs a pre-4.0 .SOL, blocked by D1',
    'U15': 'the theta comparison inside HorizontalInflowGWTable is internal',
    'U17': 'matching EC means no salt flow; indistinguishable in the output',
    'V12': 'covered by U16',
    'V18': 'Macro governs an internal macropore branch',
    'V19': 'UL and Dx are internal cell geometry, not reported',
    'V21': 'needs a multi-run KeepSWC project with salt; deferred with F26-F28',
    'V23': 'salt balance closure is an invariant, not a case',
    'V25': 'covered by every case staging a .SW0 with ECe',
    'P17': 'a cycle longer than 365 days needs a crop file the donated set lacks',
}


def slug(t):
    return ''.join(c if c.isalnum() else '_' for c in t.lower())[:44].strip('_')


def main():
    CASES.mkdir(parents=True, exist_ok=True)
    for cid, tier, cli, soil, crop, slots, cpatch, ppatch, daily, season, why in K:
        name = f'{cid}_{slug(why)}'
        d = CASES / name
        d.mkdir(exist_ok=True)
        clim = cli.replace('.CLI', '')
        tnx, eto, plu = (ROOT / 'assets' / 'climate' / cli).read_text().splitlines()[2:5]
        stage = CORE + [cli, tnx, eto, plu, soil, crop] + sorted(slots.values())
        stage = sorted(set(stage))
        slot_lines = ''.join(f'\n      {k}: {v}' for k, v in sorted(slots.items()))
        patch = ''
        blocks = []
        if cpatch:
            blocks.append(f'  {crop}:\n' + ''.join(
                f'    {ln}: {json.dumps(f"{v:>10}      : {CRO[ln]}")}\n'
                for ln, v in sorted(cpatch.items())))
        if ppatch:
            blocks.append('  Ottawa.PPn:\n' + ''.join(
                f'    {ln}: {json.dumps(f"{v:>6}      : {PPN[ln]}")}\n'
                for ln, v in sorted(ppatch.items())))
        if blocks:
            patch = '\npatch:\n' + ''.join(blocks)
        (d / 'case.yml').write_text(f"""id: {name}
desc: >
  {why}.
  {crop} on {soil}, climate {clim}, {season[0]} to {season[1]}.
tier: {tier}

stage:
{chr(10).join('  - ' + a for a in stage)}

project:
  name: {cid}
  desc: {json.dumps(f'{cid} - {why}')}
  runs:
    - year: 1
      sim: [{season[0]}, {season[1]}]
      crop: [{SEASON[0]}, {SEASON[1]}]
      cli: {cli}
      tnx: {tnx}
      eto: {eto}
      plu: {plu}
      co2: MaunaLoa.CO2
      cro: {crop}
      sol: {soil}{slot_lines}
{patch}
daily: {daily}
particular: []
aggregate: 0
rtol: 1.0e-3
""")
    from collections import Counter
    c = Counter(k[0][0] for k in K)
    print(f'wrote {len(K)} cases: ' + ' '.join(f'{g}={n}' for g, n in sorted(c.items())))
    print(f'{len(UNREACHABLE)} plan rows documented as not drivable from inputs')


if __name__ == '__main__':
    main()
