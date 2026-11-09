#!/usr/bin/env python3
"""Generate the kernel groups Q-Y: cases aimed at one simul.f90 routine each.

These differ from the input groups in intent. An input case asks "is this file
read correctly"; a kernel case sets up the conditions a routine needs and then
records what it does. Most reuse assets that already exist -- the new part is
the combination and the daily output that makes the routine observable:

  Q drainage            output 1+3   fluxes and profile water
  R runoff/infiltration output 1     rain, runoff, infiltration, drainage
  S evaporation         output 1     Ex/E and the E/Ex ratio
  T transpiration       output 1+2   Trx/Tr and the stress coefficients
  U capillary rise      output 1     the CR and Zgwt columns
  V salt transport      output 4+6   root-zone and per-compartment salinity
  W canopy              output 2     CC, CCw and the stress factors
  X biomass and yield   output 2     biomass, HI, yield
  Y stress engine       output 2     StExp, StSto, StSen, StSalt
"""
from __future__ import annotations

import json
import pathlib

ROOT = pathlib.Path(__file__).resolve().parent.parent
CASES = ROOT / 'cases'
SEASON = ('2014-05-21', '2014-10-31')
CORE = ['Ottawa.CLI', 'Ottawa.Tnx', 'Ottawa.ETo', 'Ottawa.PLU', 'MaunaLoa.CO2',
        'Ottawa.PPn', 'DEFAULT.CRO', 'DEFAULT.SOL']

# id, tier, soil, crop, slots {sw0/gwt/irr/man}, daily, ppn patch (line,value,label), desc
K: list[tuple] = [
    # ---- Q: drainage and internal redistribution --------------------------
    ('Q01', 'T1', 'Ottawa.SOL', 'MaizeGDD.CRO', {'sw0': 'SW0_atSAT.SW0'}, [1, 3], None,
     'a saturated profile draining freely'),
    ('Q05', 'T1', 'KSAT_5.SOL', 'MaizeGDD.CRO', {'sw0': 'SW0_atSAT.SW0'}, [1, 3], None,
     'saturated start on a near-impermeable soil'),
    ('Q10', 'T1', 'FINE_OVER_COARSE.SOL', 'MaizeGDD.CRO', {'sw0': 'SW0_atSAT.SW0'}, [1, 3], None,
     'clay over sand draining from saturation'),
    ('Q11', 'T1', 'COARSE_OVER_FINE.SOL', 'MaizeGDD.CRO', {'sw0': 'SW0_atSAT.SW0'}, [1, 3], None,
     'sand over clay draining from saturation'),
    ('Q12', 'T2', 'KSAT_50.SOL', 'MaizeGDD.CRO', {'sw0': 'SW0_atSAT.SW0'}, [1, 3], None,
     'slow drainage at Ksat 50'),
    ('Q15', 'T1', 'Ottawa.SOL', 'MaizeGDD.CRO',
     {'sw0': 'SW0_atSAT.SW0', 'gwt': 'GWT_const_1p0.GWT'}, [1, 3], None,
     'drainage against an adjusted field capacity from a water table'),
    ('Q17', 'T2', 'Ottawa.SOL', 'MaizeGDD.CRO', {'sw0': 'SW0_atWP.SW0'}, [1, 3], None,
     'a profile below field capacity: nothing to drain'),
    ('Q20', 'T2', 'NARROW_DRAINABLE.SOL', 'MaizeGDD.CRO', {'sw0': 'SW0_atSAT.SW0'}, [1, 3], None,
     'only 3 vol% of drainable porosity'),
    # ---- R: runoff, surface storage and infiltration -----------------------
    ('R08', 'T1', 'Ottawa.SOL', 'MaizeGDD.CRO', {'man': 'MAN_cn_plus10.MAN'}, [1], None,
     'raised curve number: more runoff'),
    ('R11', 'T1', 'Ottawa.SOL', 'MaizeGDD.CRO', {'man': 'MAN_bund10.MAN'}, [1], None,
     'rain ponding behind 0.10 m bunds'),
    ('R12', 'T1', 'Ottawa.SOL', 'MaizeGDD.CRO', {'man': 'MAN_bund30.MAN'}, [1], None,
     'rain ponding behind 0.30 m bunds'),
    ('R15', 'T1', 'KSAT_5.SOL', 'MaizeGDD.CRO', {}, [1], None,
     'rainfall exceeding the layer-1 infiltration rate'),
    ('R17', 'T1', 'KSAT_5.SOL', 'MaizeGDD.CRO', {'irr': 'IRR_manual_dense.IRR'}, [1], None,
     'rain and irrigation together exceeding Ksat'),
    ('R28', 'T2', 'Ottawa.SOL', 'MaizeGDD.CRO', {'sw0': 'SW0_atSAT.SW0'}, [1], None,
     'infiltration into a profile with no storage capacity left'),
    ('R29', 'T1', 'COARSE_OVER_FINE.SOL', 'MaizeGDD.CRO', {}, [1], None,
     'perching at a coarse-over-fine boundary'),
    ('R30', 'T1', 'Ottawa.SOL', 'MaizeGDD.CRO', {'gwt': 'GWT_const_0p4.GWT'}, [1], None,
     'infiltration with a shallow water table reducing capacity'),
    # ---- S: soil evaporation ----------------------------------------------
    ('S02', 'T1', 'REW_3.SOL', 'MaizeGDD.CRO', {}, [1], None,
     'a short stage I: REW 3 mm'),
    ('S03', 'T1', 'REW_15.SOL', 'MaizeGDD.CRO', {}, [1], None,
     'a long stage I: REW 15 mm'),
    ('S06', 'T2', 'Ottawa.SOL', 'MaizeGDD.CRO', {}, [1],
     (2, '1.30', 'Ke(x) Soil evaporation coefficient for fully wet and non-shaded soil surface'),
     'a raised maximum soil evaporation coefficient'),
    ('S18', 'T1', 'Ottawa.SOL', 'MaizeGDD.CRO', {'man': 'MAN_mulch100.MAN'}, [1], None,
     'full mulch cover suppressing evaporation'),
    ('S19', 'T1', 'Ottawa.SOL', 'MaizeGDD.CRO', {'irr': 'IRR_fw30.IRR'}, [1], None,
     'drip irrigation wetting only 30 % of the surface'),
    ('S22', 'T1', 'Ottawa.SOL', 'MaizeGDD.CRO',
     {'man': 'MAN_bund30.MAN', 'sw0': 'SW0_surfstore.SW0'}, [1], None,
     'evaporation from water ponded between bunds'),
    ('S24', 'T1', 'Ottawa.SOL', 'MaizeGDD.CRO', {'sw0': 'SalineSoil.SW0'}, [1, 4], None,
     'salts concentrating in the evaporation layer'),
    # ---- T: transpiration and uptake ---------------------------------------
    ('T02', 'T1', 'Ottawa.SOL', 'MaizeGDD.CRO', {'sw0': 'SW0_atWP.SW0'}, [1, 2], None,
     'transpiration under severe water stress'),
    ('T03', 'T1', 'Ottawa.SOL', 'MaizeGDD.CRO', {'sw0': 'SalineSoil.SW0'}, [1, 2], None,
     'transpiration under salinity stress'),
    ('T04', 'T1', 'Ottawa.SOL', 'MaizeSalinity.CRO', {'sw0': 'SalineSoil.SW0'}, [1, 2], None,
     'a salt-sensitive crop on a saline profile'),
    ('T14', 'T1', 'KSAT_5.SOL', 'MaizeGDD.CRO', {'sw0': 'SW0_atSAT.SW0'}, [1, 2], None,
     'aeration stress from a waterlogged profile'),
    ('T24', 'T1', 'Ottawa.SOL', 'MaizeGDD.CRO', {'gwt': 'GWT_const_0p4.GWT'}, [1, 2], None,
     'uptake from a root zone reaching the water table'),
    # ---- U: capillary rise and groundwater inflow ---------------------------
    ('U02', 'T1', 'Ottawa.SOL', 'MaizeGDD.CRO',
     {'gwt': 'GWT_const_2p0.GWT', 'sw0': 'SW0_atWP.SW0'}, [1], None,
     'capillary rise into a dry profile from 2.0 m'),
    ('U03', 'T1', 'Ottawa.SOL', 'MaizeGDD.CRO',
     {'gwt': 'GWT_const_1p0.GWT', 'sw0': 'SW0_atWP.SW0'}, [1], None,
     'capillary rise from 1.0 m'),
    ('U04', 'T1', 'Ottawa.SOL', 'MaizeGDD.CRO',
     {'gwt': 'GWT_const_0p4.GWT', 'sw0': 'SW0_atWP.SW0'}, [1], None,
     'capillary rise from 0.4 m into the evaporation layer'),
    ('U12', 'T2', 'Ottawa.SOL', 'MaizeGDD.CRO',
     {'gwt': 'GWT_const_1p0.GWT', 'sw0': 'SW0_atWP.SW0'}, [1],
     (18, '32', 'Shape factor for effect of soil water content gradient on capillary rise'),
     'capillary rise with a doubled gradient shape factor'),
    ('U13', 'T1', 'Ottawa.SOL', 'MaizeGDD.CRO',
     {'gwt': 'GWT_const_saline.GWT', 'sw0': 'SW0_atWP.SW0'}, [1, 4], None,
     'salt carried upward with capillary rise'),
    # ---- V: salt transport ---------------------------------------------------
    ('V02', 'T1', 'Ottawa.SOL', 'MaizeGDD.CRO', {'sw0': 'SalineSoil.SW0'}, [4, 6], None,
     'salt transport with 2 salt cells (Ksat 1200)'),
    ('V05', 'T1', 'KSAT_112.SOL', 'MaizeGDD.CRO', {'sw0': 'SalineSoil.SW0'}, [4, 6], None,
     'salt transport with 11 salt cells (Ksat 112)'),
    ('V07', 'T1', 'KSAT_mixed.SOL', 'MaizeGDD.CRO', {'sw0': 'SalineSoil.SW0'}, [4, 6], None,
     'a different salt-cell count in each horizon'),
    ('V09', 'T1', 'Ottawa.SOL', 'MaizeGDD.CRO', {'irr': 'IRR_manual_saline.IRR'}, [4, 6], None,
     'salt arriving with saline irrigation water'),
    ('V22', 'T1', 'Ottawa.SOL', 'MaizeGDD.CRO',
     {'sw0': 'SalineSoil.SW0', 'irr': 'IRR_gen_allraw.IRR'}, [4, 6], None,
     'leaching a saline profile with fresh irrigation'),
    ('V13', 'T2', 'Ottawa.SOL', 'MaizeGDD.CRO', {'sw0': 'SalineSoil.SW0'}, [4, 6],
     (16, '0', 'Salt diffusion factor (capacity for salt diffusion in micro pores) [%]'),
     'salt transport with diffusion switched off'),
    # ---- W / X / Y: canopy, biomass, stress ---------------------------------
    ('W05', 'T1', 'Ottawa.SOL', 'MaizeGDD.CRO', {'sw0': 'SW0_atWP.SW0'}, [2], None,
     'canopy expansion limited by water stress'),
    ('W06', 'T1', 'Ottawa.SOL', 'Maize_EUirr_GDD.CRO', {'man': 'MAN_fert75.MAN'}, [2], None,
     'canopy limited by fertility stress on a calibrated crop'),
    ('W18', 'T1', 'Ottawa.SOL', 'MaizeGDD.CRO', {'man': 'MAN_weeds75.MAN'}, [2], None,
     'crop canopy against total canopy under heavy weeds'),
    ('X02', 'T1', 'Ottawa.SOL', 'MaizeGDD.CRO', {'sw0': 'SW0_atWP.SW0'}, [2], None,
     'biomass accumulation under water stress'),
    ('X10', 'T1', 'Ottawa.SOL', 'MaizeGDDwpy.CRO', {}, [2], None,
     'water productivity reduced during yield formation'),
    ('Y03', 'T1', 'Ottawa.SOL', 'Maize_EUirr_GDD.CRO', {'man': 'MAN_fert25.MAN'}, [2], None,
     'fertility stress below the crop calibration point'),
    ('Y04', 'T1', 'Ottawa.SOL', 'Maize_EUirr_GDD.CRO', {'man': 'MAN_fert75.MAN'}, [2], None,
     'fertility stress above the crop calibration point'),
    ('Y10', 'T1', 'Ottawa.SOL', 'MaizeSalinity.CRO', {'sw0': 'SalineSoil.SW0'}, [2], None,
     'the stress engine driven by salinity alone'),
]


def slug(t):
    return ''.join(c if c.isalnum() else '_' for c in t.lower())[:44].strip('_')


def main():
    CASES.mkdir(parents=True, exist_ok=True)
    for cid, tier, soil, crop, slots, daily, ppn, why in K:
        name = f'{cid}_{slug(why)}'
        d = CASES / name
        d.mkdir(exist_ok=True)
        stage = CORE + [soil, crop] + sorted(slots.values())
        slot_lines = ''.join(f'\n      {k}: {v}' for k, v in sorted(slots.items()))
        # SW0_surfstore declares 20 mm ponded; it is only retained where the
        # management file declares bunds, and it is an input to the balance
        storage = ('surface_storage_in: 20.0\n'
                   if slots.get('sw0', '').startswith('SW0_surfstore')
                   and 'bund' in slots.get('man', '') else '')
        patch = ''
        if ppn:
            line, val, label = ppn
            patch = ('\npatch:\n  Ottawa.PPn:\n'
                     f'    {line}: {json.dumps(f"{val:>6}      : {label}")}\n')
        (d / 'case.yml').write_text(f"""id: {name}
desc: >
  {why}.
  {crop} on {soil}, Ottawa 2014, sown {SEASON[0]}.
tier: {tier}

stage:
{chr(10).join('  - ' + a for a in stage)}

project:
  name: {cid}
  desc: {json.dumps(f'{cid} - {why}')}
  runs:
    - year: 1
      sim: [{SEASON[0]}, {SEASON[1]}]
      cli: Ottawa.CLI
      tnx: Ottawa.Tnx
      eto: Ottawa.ETo
      plu: Ottawa.PLU
      co2: MaunaLoa.CO2
      cro: {crop}
      sol: {soil}{slot_lines}
{patch}
daily: {daily}
particular: []
aggregate: 0
{storage}rtol: 1.0e-3
""")
    from collections import Counter
    c = Counter(k[0][0] for k in K)
    print(f'wrote {len(K)} kernel cases: ' + ' '.join(f'{g}={n}' for g, n in sorted(c.items())))


if __name__ == '__main__':
    main()
