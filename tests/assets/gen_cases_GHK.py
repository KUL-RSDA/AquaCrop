#!/usr/bin/env python3
"""Generate groups G (groundwater), H (initial conditions) and K (off-season).

All three share one base: maize on the Ottawa 2014 record, Ottawa.SOL, sown
21 May. Only the extra input file varies, so any difference in the output is
attributable to it.

Group K needs a simulation period wider than the cropping period, or there are
no off-season days for the file to act on -- that is the whole point of an .OFF.
"""
from __future__ import annotations

import json
import pathlib

ROOT = pathlib.Path(__file__).resolve().parent.parent
CASES = ROOT / 'cases'

CROP = 'MaizeGDD.CRO'
BASE = ['Ottawa.CLI', 'Ottawa.Tnx', 'Ottawa.ETo', 'Ottawa.PLU', 'MaunaLoa.CO2',
        'Ottawa.PPn', 'DEFAULT.CRO', 'DEFAULT.SOL', 'Ottawa.SOL', CROP]

SEASON = ('2014-05-21', '2014-10-31')
WIDE = ('2014-04-01', '2014-11-30')      # room either side for the off-season

#  id, slot, file, tier, daily outputs, description
CASES_GHK: list[tuple] = [
    # ---- G: groundwater -------------------------------------------------
    ('G02', 'gwt', 'GWT_none.GWT',          'T2', [1], 'a .GWT file declaring no water table'),
    ('G03', 'gwt', 'GWT_const_3p0.GWT',     'T2', [1], 'constant table at 3.0 m, below the profile'),
    ('G04', 'gwt', 'GWT_const_2p0.GWT',     'T1', [1], 'constant table at 2.0 m'),
    ('G05', 'gwt', 'GWT_const_1p0.GWT',     'T1', [1], 'constant table at 1.0 m, inside the profile'),
    ('G06', 'gwt', 'GWT_const_0p4.GWT',     'T1', [1], 'shallow constant table at 0.4 m'),
    ('G09', 'gwt', 'GWT_var_2obs.GWT',      'T1', [1], 'variable table, two observations'),
    ('G12', 'gwt', 'GWT_var_rise_fall.GWT', 'T2', [1], 'variable table rising then falling'),
    ('G16', 'gwt', 'GWT_const_saline.GWT',  'T1', [1], 'saline constant table, EC 5 dS/m'),
    ('G17', 'gwt', 'GWT_var_saline.GWT',    'T2', [1], 'variable table with varying salinity'),
    ('G15', 'gwt', 'Var4.GWT',              'T2', [1], 'the donated v4.0 variable table, year-agnostic'),
    # ---- H: initial conditions -------------------------------------------
    ('H02', 'sw0', 'SW0_at_depths.SW0',     'T1', [3], 'water content given at depths'),
    ('H03', 'sw0', 'SW0_dry_top.SW0',       'T1', [3], 'water content given per layer, dry top'),
    ('H09', 'sw0', 'SW0_ece_grad.SW0',      'T1', [3], 'initial salinity increasing with depth'),
    ('H10', 'sw0', 'SalineSoil.SW0',        'T1', [3], 'uniformly saline profile, ECe 3.0'),
    ('H11', 'sw0', 'SW0_surfstore.SW0',     'T2', [3], '20 mm ponded between bunds at the start'),
    ('H12', 'sw0', 'SW0_surfstore_saline.SW0', 'T2', [3], 'saline ponded water at the start'),
    ('H16', 'sw0', 'SW0_midseason.SW0',     'T1', [3], 'mid-season restart: CCini, Bini and Zrini'),
    ('H17', 'sw0', 'SW0_atWP.SW0',          'T1', [3], 'profile starting at wilting point'),
    ('H18', 'sw0', 'SW0_atSAT.SW0',         'T2', [3], 'profile starting at saturation'),
    ('H19', 'sw0', 'SW0_atFC.SW0',          'T1', [3], 'profile at field capacity, stated explicitly'),
    ('H20', 'sw0', 'DryTopSoil.SW0',        'T1', [3], 'dry profile below the germination threshold'),
    ('H21', 'sw0', 'SalineSoilMild.SW0',    'T2', [3], 'mildly saline profile, ECe 1.0'),
    ('H22', 'sw0', 'WPSandLoam.SW0',        'T2', [3], 'the donated v3.2 wilting-point file'),
    # ---- K: off-season ----------------------------------------------------
    ('K02', 'offseason', 'OFF_cover_before.OFF',  'T1', [1], 'mulch cover before the season'),
    ('K03', 'offseason', 'OFF_cover_after.OFF',   'T1', [1], 'mulch cover after the season'),
    ('K04', 'offseason', 'OFF_cover_both.OFF',    'T2', [1], 'full mulch cover on both sides'),
    ('K05', 'offseason', 'OFF_effect_100.OFF',    'T2', [1], 'mulch with full evaporation suppression'),
    ('K06', 'offseason', 'OFF_irr_before.OFF',    'T1', [1], 'irrigation events before the season'),
    ('K08', 'offseason', 'OFF_irr_after.OFF',     'T1', [1], 'irrigation events after the season'),
    ('K10', 'offseason', 'OFF_irr_both.OFF',      'T2', [1], 'irrigation on both sides of the season'),
    ('K11', 'offseason', 'OFF_saline_irr.OFF',    'T2', [1], 'off-season irrigation with saline water'),
    ('K13', 'offseason', 'OFF_partial_wet.OFF',   'T2', [1], 'off-season irrigation wetting 40 % of the surface'),
    ('K14', 'offseason', 'example_offseason.OFF', 'T1', [1], 'the donated off-season file'),
]


def slug(t):
    return ''.join(c if c.isalnum() else '_' for c in t.lower())[:44].strip('_')


def main():
    CASES.mkdir(parents=True, exist_ok=True)
    for cid, slot, fname, tier, daily, why in CASES_GHK:
        wide = cid.startswith('K')
        sim = WIDE if wide else SEASON
        name = f'{cid}_{slug(why)}'
        d = CASES / name
        d.mkdir(exist_ok=True)
        (d / 'case.yml').write_text(f"""id: {name}
desc: >
  {why}.
  Maize on the Ottawa 2014 record, Ottawa.SOL, sown {SEASON[0]};
  the only variable is {fname}.
tier: {tier}

stage:
{chr(10).join('  - ' + a for a in BASE)}
  - {fname}

project:
  name: {cid}
  desc: {json.dumps(f'{cid} - {why}')}
  runs:
    - year: 1
      sim: [{sim[0]}, {sim[1]}]
      crop: [{SEASON[0]}, {SEASON[1]}]
      cli: Ottawa.CLI
      tnx: Ottawa.Tnx
      eto: Ottawa.ETo
      plu: Ottawa.PLU
      co2: MaunaLoa.CO2
      cro: {CROP}
      sol: Ottawa.SOL
      {slot}: {fname}

daily: {daily}
particular: []
aggregate: 0
rtol: 1.0e-3
""")
    print(f'wrote {len(CASES_GHK)} cases: '
          f"G={sum(1 for c in CASES_GHK if c[0][0] == 'G')} "
          f"H={sum(1 for c in CASES_GHK if c[0][0] == 'H')} "
          f"K={sum(1 for c in CASES_GHK if c[0][0] == 'K')}")


if __name__ == '__main__':
    main()
