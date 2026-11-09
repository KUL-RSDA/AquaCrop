#!/usr/bin/env python3
"""Generate group L: salinity configuration.

Salinity reaches the profile three ways -- initial condition, irrigation water
and groundwater -- and its effect is shaped by two program parameters (diffusion
between salt cells, solubility) and two crop calibrations (CC distortion,
stomatal response to ECsw). This group varies each in turn against a fixed
maize/Ottawa base, with daily outputs 4 and 6 so root-zone and per-compartment
salinity are both visible.

MaizeGDD tolerates ECe 2 (ECn) to 10 (ECx); MaizeSalinity is the sensitive twin
at 0 to 6 with 75 % CC distortion.
"""
from __future__ import annotations

import json
import pathlib

ROOT = pathlib.Path(__file__).resolve().parent.parent
CASES = ROOT / 'cases'
SEASON = ('2014-05-21', '2014-10-31')
CORE = ['Ottawa.CLI', 'Ottawa.Tnx', 'Ottawa.ETo', 'Ottawa.PLU', 'MaunaLoa.CO2',
        'Ottawa.PPn', 'DEFAULT.CRO', 'DEFAULT.SOL', 'Ottawa.SOL']

# id, tier, crop, slots, ppn patch, description
L: list[tuple] = [
    ('L01', 'T1', 'MaizeGDD.CRO', {}, None, 'a non-saline profile as the reference'),
    ('L02', 'T1', 'MaizeGDD.CRO', {'sw0': 'SW0_ece2.SW0'}, None,
     'initial ECe 2, exactly at the crop threshold'),
    ('L03', 'T1', 'MaizeGDD.CRO', {'sw0': 'SW0_ece4.SW0'}, None, 'initial ECe 4'),
    ('L04', 'T1', 'MaizeGDD.CRO', {'sw0': 'SW0_ece8.SW0'}, None, 'initial ECe 8'),
    ('L05', 'T1', 'MaizeGDD.CRO', {'sw0': 'SW0_ece16.SW0'}, None,
     'initial ECe 16, beyond the crop tolerance limit'),
    ('L07', 'T1', 'MaizeGDD.CRO', {'sw0': 'SW0_ece_grad.SW0'}, None,
     'salinity increasing with depth'),
    ('L08', 'T1', 'MaizeGDD.CRO', {'sw0': 'SW0_ece_inverse.SW0'}, None,
     'salinity decreasing with depth, a leached surface'),
    ('L09', 'T1', 'MaizeSalinity.CRO', {'sw0': 'SW0_ece4.SW0'}, None,
     'a salt-sensitive crop at ECe 4'),
    ('L10', 'T1', 'MaizeGDD.CRO', {'sw0': 'SW0_ece4.SW0'},
     (16, '0', 'Salt diffusion factor (capacity for salt diffusion in micro pores) [%]'),
     'no diffusion between salt cells'),
    ('L12', 'T1', 'MaizeGDD.CRO', {'sw0': 'SW0_ece4.SW0'},
     (16, '100', 'Salt diffusion factor (capacity for salt diffusion in micro pores) [%]'),
     'full diffusion between salt cells'),
    ('L13', 'T1', 'MaizeGDD.CRO', {'sw0': 'SW0_ece8.SW0'},
     (17, '50', 'Salt solubility [g/liter]'), 'salt solubility halved: earlier precipitation'),
    # 127 is the ceiling: SaltSolub is integer(int8), so 128 or more aborts the
    # run with an integer-overflow read error (see D8)
    ('L15', 'T2', 'MaizeGDD.CRO', {'sw0': 'SW0_ece8.SW0'},
     (17, '127', 'Salt solubility [g/liter]'),
     'salt solubility at the highest representable value'),
    ('L18', 'T1', 'MaizeGDD.CRO',
     {'sw0': 'SW0_ece4.SW0', 'gwt': 'GWT_const_saline.GWT'}, None,
     'a saline profile over a saline water table'),
    ('L19', 'T1', 'MaizeGDD.CRO',
     {'sw0': 'SW0_ece8.SW0', 'irr': 'IRR_gen_allraw.IRR'}, None,
     'leaching a saline profile with fresh irrigation'),
    ('L19b', 'T1', 'MaizeGDD.CRO', {'irr': 'IRR_gen_saline.IRR'}, None,
     'salt accumulating from saline irrigation on a clean profile'),
    ('L20', 'T1', 'MaizeGDD.CRO', {'sw0': 'SW0_ece4.SW0'}, None,
     'the salinity output columns on a saline profile'),
]


def slug(t):
    return ''.join(c if c.isalnum() else '_' for c in t.lower())[:44].strip('_')


def main():
    CASES.mkdir(parents=True, exist_ok=True)
    for cid, tier, crop, slots, ppn, why in L:
        name = f'{cid}_{slug(why)}'
        d = CASES / name
        d.mkdir(exist_ok=True)
        stage = CORE + [crop] + sorted(slots.values())
        slot_lines = ''.join(f'\n      {k}: {v}' for k, v in sorted(slots.items()))
        patch = ''
        if ppn:
            line, val, label = ppn
            patch = ('\npatch:\n  Ottawa.PPn:\n'
                     f'    {line}: {json.dumps(f"{val:>6}      : {label}")}\n')
        (d / 'case.yml').write_text(f"""id: {name}
desc: >
  {why}.
  {crop} on Ottawa.SOL, Ottawa 2014, sown {SEASON[0]}.
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
      sol: Ottawa.SOL{slot_lines}
{patch}
# outputs 4 and 6: root-zone salinity and per-compartment ECe
daily: [4, 6]
particular: []
aggregate: 0
rtol: 1.0e-3
""")
    print(f'wrote {len(L)} group-L cases')


if __name__ == '__main__':
    main()
