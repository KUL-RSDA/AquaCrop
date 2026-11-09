#!/usr/bin/env python3
"""Generate groups W (canopy), X (biomass and yield) and Y (the stress engine).

These were the thinnest part of the suite -- 8 cases against 78 planned rows --
and they are where the crop physics lives, so they are the most valuable place
to be thorough.

Almost every parameter here lives in the .CRO file, which is positional. Line
numbers for MaizeGDD.CRO, confirmed against the file:

    7 p adjusted by ETo          43 seedling size        61 WP*
   13 shape, expansion stress    45 plants per hectare   62 WP during yield formation
   15 shape, stomatal stress     46 CGC                  63 CO2 sink strength
   17 shape, senescence stress   50 CCx                  64 HIo
   22 fertility -> expansion     51 CDC                  65 HI increase, pre-flowering
   23 fertility -> CCx           27 cold pollination      66 HI, restricted veg growth
   24 fertility -> WP            28 heat pollination      67 HI, stomatal closure
   25 fertility -> CC decline    29 GDD for full Tr       68 HI max increase
   35 KcTr,x                     36 Kc decline

A crop patch is applied to the staged crop file by name, so each case carries
its own edited copy and the shared asset is untouched.
"""
from __future__ import annotations

import json
import pathlib

ROOT = pathlib.Path(__file__).resolve().parent.parent
CASES = ROOT / 'cases'
SEASON = ('2014-05-21', '2014-10-31')
CORE = ['Ottawa.CLI', 'Ottawa.Tnx', 'Ottawa.ETo', 'Ottawa.PLU', 'MaunaLoa.CO2',
        'Ottawa.PPn', 'DEFAULT.CRO', 'DEFAULT.SOL', 'Ottawa.SOL']

L = {  # crop-file line -> its label, so patches keep the file readable
    7: 'Soil water depletion factors (p) are adjusted by ETo',
    13: 'Shape factor for water stress coefficient for canopy expansion (0.0 = straight line)',
    15: 'Shape factor for water stress coefficient for stomatal control (0.0 = straight line)',
    17: 'Shape factor for water stress coefficient for canopy senescence (0.0 = straight line)',
    22: 'Shape factor for the response of canopy expansion to soil fertility stress',
    23: 'Shape factor for the response of maximum canopy cover to soil fertility stress',
    24: 'Shape factor for the response of crop Water Productivity to soil fertility stress',
    25: 'Shape factor for the response of decline of canopy cover to soil fertility stress',
    27: 'Minimum air temperature below which pollination starts to fail (cold stress) (degC)',
    28: 'Maximum air temperature above which pollination starts to fail (heat stress) (degC)',
    29: 'Minimum growing degrees required for full crop transpiration (degC - day)',
    35: 'Crop coefficient when canopy is complete but prior to senescence (KcTr,x)',
    36: 'Decline of crop coefficient (%/day) as a result of ageing, nitrogen deficiency, etc.',
    43: 'Soil surface covered by an individual seedling at 90 % emergence (cm2)',
    45: 'Number of plants per hectare',
    46: 'Canopy growth coefficient (CGC): Increase in canopy cover (fraction soil cover per day)',
    50: 'Maximum canopy cover (CCx) in fraction soil cover',
    51: 'Canopy decline coefficient (CDC): Decrease in canopy cover (in fraction per day)',
    61: 'Water Productivity normalized for ETo and CO2 (WP*) (gram/m2)',
    63: 'Sink strength (%) quatifying biomass response to elevated atmospheric CO2 concentration',
    64: 'Reference Harvest Index (HIo) (%)',
    65: 'Possible increase (%) of HI due to water stress before flowering',
    66: 'Coefficient describing positive impact on HI of restricted vegetative growth during yield formation',
    67: 'Coefficient describing negative impact on HI of stomatal closure during yield formation',
    68: 'Allowable maximum increase (%) of specified HI',
}

CROP = 'MaizeGDD.CRO'
#  id, tier, {line: value} crop patch, slots, description
W: list[tuple] = [
    # ---- W: canopy development ------------------------------------------
    ('W08', 'T1', {50: '0.30'}, {}, 'a low canopy ceiling: CCx 0.30'),
    ('W09', 'T2', {50: '0.99'}, {}, 'a near-complete canopy: CCx 0.99'),
    ('W22', 'T1', {45: '7500'}, {}, 'a sparse stand: 7 500 plants per hectare'),
    ('W24', 'T1', {45: '200000'}, {}, 'a dense stand: 200 000 plants per hectare'),
    ('W25', 'T2', {43: '1.00'}, {}, 'a small seedling: 1 cm2 at emergence'),
    ('W26', 'T1', {46: '0.05000'}, {}, 'slow canopy expansion: CGC 0.05'),
    ('W26b', 'T1', {46: '0.25000'}, {}, 'fast canopy expansion: CGC 0.25'),
    ('W27', 'T1', {51: '0.03000'}, {}, 'slow canopy decline: CDC 0.03'),
    ('W27b', 'T1', {51: '0.20000'}, {}, 'fast canopy decline: CDC 0.20'),
    ('W11', 'T2', {35: '0.80'}, {}, 'a low crop coefficient: KcTr,x 0.80'),
    ('W11b', 'T2', {35: '1.30'}, {}, 'a high crop coefficient: KcTr,x 1.30'),
    ('W13', 'T2', {36: '1.000'}, {}, 'a steep ageing decline of the crop coefficient'),
    ('W30', 'T1', {}, {'sw0': 'SW0_midseason.SW0'},
     'a canopy carried in from an initial condition'),
    # ---- X: biomass, harvest index and yield ------------------------------
    ('X09', 'T1', {61: '15.0'}, {}, 'a C3 water productivity: WP* 15'),
    ('X09b', 'T2', {61: '45.0'}, {}, 'a very high water productivity: WP* 45'),
    ('X08', 'T1', {63: '0'}, {}, 'no biomass response to elevated CO2'),
    ('X08b', 'T2', {63: '100'}, {}, 'full biomass response to elevated CO2'),
    ('X11', 'T1', {64: '20'}, {}, 'a low reference harvest index: HIo 20 %'),
    ('X11b', 'T1', {64: '80'}, {}, 'a high reference harvest index: HIo 80 %'),
    ('X15', 'T1', {65: '30'}, {}, 'harvest index raised by pre-flowering water stress'),
    ('X17', 'T1', {66: '20.0'}, {}, 'a strong positive HI response to restricted growth'),
    ('X16', 'T1', {67: '15.0'}, {}, 'a strong negative HI response to stomatal closure'),
    ('X18', 'T2', {68: '0'}, {}, 'no allowable increase of the harvest index'),
    ('X19', 'T1', {27: '20'}, {}, 'cold pollination failure below 20 degC'),
    ('X20', 'T1', {28: '25'}, {}, 'heat pollination failure above 25 degC'),
    ('X21', 'T2', {27: '20', 28: '25'}, {}, 'both pollination thresholds binding'),
    ('X25', 'T1', {}, {'sw0': 'SW0_midseason.SW0'},
     'biomass carried in from an initial condition'),
    # ---- Y: the stress engine ---------------------------------------------
    ('Y05', 'T1', {22: '1.00'}, {'man': 'MAN_fert50.MAN'},
     'a weak fertility response of canopy expansion'),
    ('Y06', 'T1', {23: '1.00'}, {'man': 'MAN_fert50.MAN'},
     'a weak fertility response of maximum canopy cover'),
    ('Y07', 'T1', {24: '2.00'}, {'man': 'MAN_fert50.MAN'},
     'a positive fertility response of water productivity'),
    ('Y08', 'T1', {25: '1.00'}, {'man': 'MAN_fert50.MAN'},
     'a weak fertility response of canopy decline'),
    ('Y17', 'T1', {7: '0'}, {}, 'depletion factors not adjusted by ETo'),
    ('Y19', 'T1', {13: '0.0', 15: '0.0', 17: '0.0'}, {'sw0': 'SW0_atWP.SW0'},
     'linear water-stress coefficients: every shape factor 0'),
    ('Y21', 'T1', {13: '6.0', 15: '6.0', 17: '6.0'}, {'sw0': 'SW0_atWP.SW0'},
     'strongly curved water-stress coefficients'),
    ('Y22', 'T1', {29: '20.0'}, {}, 'a high growing-degree requirement for full transpiration'),
]


def slug(t):
    return ''.join(c if c.isalnum() else '_' for c in t.lower())[:44].strip('_')


def main():
    CASES.mkdir(parents=True, exist_ok=True)
    for cid, tier, cpatch, slots, why in W:
        name = f'{cid}_{slug(why)}'
        d = CASES / name
        d.mkdir(exist_ok=True)
        stage = CORE + [CROP] + sorted(slots.values())
        slot_lines = ''.join(f'\n      {k}: {v}' for k, v in sorted(slots.items()))
        patch = ''
        if cpatch:
            lines = ''.join(
                f'    {ln}: {json.dumps(f"{val:>10}      : {L[ln]}")}\n'
                for ln, val in sorted(cpatch.items()))
            patch = f'\npatch:\n  {CROP}:\n{lines}'
        (d / 'case.yml').write_text(f"""id: {name}
desc: >
  {why}.
  {CROP} on Ottawa.SOL, Ottawa 2014, sown {SEASON[0]}.
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
      cro: {CROP}
      sol: Ottawa.SOL{slot_lines}
{patch}
# output 2 carries CC, CCw, the stress factors, biomass, HI and yield
daily: [2]
particular: []
aggregate: 0
rtol: 1.0e-3
""")
    from collections import Counter
    c = Counter(w[0][0] for w in W)
    print(f'wrote {len(W)} cases: ' + ' '.join(f'{g}={n}' for g, n in sorted(c.items())))


if __name__ == '__main__':
    main()
