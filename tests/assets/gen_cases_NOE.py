#!/usr/bin/env python3
"""Generate groups N (program parameters), O (output selection) and E (rainfall).

All three vary configuration rather than physical inputs, so they share one
base: maize on the Ottawa 2014 record, Ottawa.SOL, sown 21 May.

Group N patches individual .PPn records. Line numbers are positional -- the
loader reads them in order with no keys -- so they are listed once here:

   1 evaporation decline factor      14 depth for CN adjustment
   2 Ke(x)                           15 CN adjusted to AMC
   3 CC threshold for HI             16 salt diffusion factor
   4 root expansion start depth      17 salt solubility
   5 max root zone expansion         18 capillary-rise shape factor
   6 shape factor, stress on roots   19 default Tmin
   7 germination TAW threshold       20 default Tmax
   8 p adjustment by ETo             21 GDD method
   9 days to full aeration effect    22 daily rainfall estimation
  10 senescence exponent             23 percentage effective rainfall
  11 p(sen) decrease                 24 showers per decade
  12 top-soil thickness              25 soil-evaporation reduction
  13 evaporation depth
"""
from __future__ import annotations

import json
import pathlib

ROOT = pathlib.Path(__file__).resolve().parent.parent
CASES = ROOT / 'cases'
SEASON = ('2014-05-21', '2014-10-31')
CROP = 'MaizeGDD.CRO'
BASE = ['Ottawa.CLI', 'Ottawa.Tnx', 'Ottawa.ETo', 'Ottawa.PLU', 'MaunaLoa.CO2',
        'Ottawa.PPn', 'DEFAULT.CRO', 'DEFAULT.SOL', 'Ottawa.SOL', CROP]

PPN_LABEL = {
    1: 'Evaporation decline factor for stage II',
    2: 'Ke(x) Soil evaporation coefficient for fully wet and non-shaded soil surface',
    3: 'Threshold for green CC below which HI can no longer increase (% cover)',
    4: 'Starting depth of root zone expansion curve (% of Zmin)',
    5: 'Maximum allowable root zone expansion (fixed at 5 cm/day)',
    6: 'Shape factor for effect water stress on root zone expansion',
    7: 'Required soil water content in top soil for germination (% TAW)',
    8: 'Adjustment factor for FAO-adjustment soil water depletion (p) by ETo',
    9: 'Number of days after which deficient aeration is fully effective',
    10: 'Exponent of senescence factor adjusting drop in photosynthetic activity of dying crop',
    11: 'Decrease of p(sen) once early canopy senescence is triggered (% of p(sen))',
    12: 'Thickness top soil (cm) in which soil water depletion has to be determined',
    13: 'Depth [cm] of soil profile affected by water extraction by soil evaporation',
    14: 'Considered depth (m) of soil profile for calculation of mean soil water content for CN adjustment',
    15: 'CN is adjusted to Antecedent Moisture Class',
    16: 'Salt diffusion factor (capacity for salt diffusion in micro pores) [%]',
    17: 'Salt solubility [g/liter]',
    18: 'Shape factor for effect of soil water content gradient on capillary rise',
    19: 'Default minimum temperature (degC) if no temperature file is specified',
    20: 'Default maximum temperature (degC) if no temperature file is specified',
    21: 'Default method for the calculation of growing degree days',
    22: 'Daily rainfall is estimated by USDA-SCS procedure (when input is 10-day/monthly rainfall)',
    23: 'Percentage of effective rainfall (when input is 10-day/monthly rainfall)',
    24: 'Number of showers in a decade for run-off estimate (when input is 10-day/monthly rainfall)',
    25: 'Parameter for reduction of soil evaporation (when input is 10-day/monthly rainfall)',
}

# id, PPn line, value, tier, description
N_CASES = [
    ('N03a', 1, 1, 'T2', 'evaporation decline factor 1'),
    ('N03b', 1, 8, 'T2', 'evaporation decline factor 8'),
    ('N04a', 2, 1.00, 'T2', 'Ke(x) 1.00'),
    ('N04b', 2, 1.20, 'T2', 'Ke(x) 1.20'),
    ('N05a', 3, 0, 'T2', 'no CC threshold for harvest index'),
    ('N05b', 3, 20, 'T2', 'CC threshold for harvest index at 20 %'),
    ('N06a', 4, 50, 'T2', 'root expansion starting at 50 % of Zmin'),
    ('N06b', 4, 100, 'T2', 'root expansion starting at 100 % of Zmin'),
    ('N07a', 5, 1.00, 'T2', 'root zone expansion capped at 1 cm/day'),
    ('N08a', 6, 0, 'T2', 'no water-stress effect on root expansion'),
    ('N08b', 6, 6, 'T2', 'positive shape factor for stress on root expansion'),
    ('N09a', 7, 5, 'T1', 'germination at 5 % TAW in the top soil'),
    ('N09b', 7, 50, 'T1', 'germination at 50 % TAW in the top soil'),
    ('N11a', 8, 0.5, 'T2', 'p adjusted by ETo with factor 0.5'),
    ('N11b', 8, 2.0, 'T2', 'p adjusted by ETo with factor 2.0'),
    ('N12a', 9, 1, 'T2', 'deficient aeration fully effective after 1 day'),
    ('N12b', 9, 7, 'T2', 'deficient aeration fully effective after 7 days'),
    ('N13a', 10, 0.5, 'T2', 'senescence exponent 0.5'),
    ('N13b', 10, 2.0, 'T2', 'senescence exponent 2.0'),
    ('N14a', 11, 0, 'T2', 'no decrease of p(sen) after senescence starts'),
    ('N14b', 11, 30, 'T2', 'p(sen) decreased by 30 %'),
    ('N15a', 12, 5, 'T2', 'top soil 5 cm'),
    ('N15b', 12, 20, 'T2', 'top soil 20 cm'),
    ('N16a', 13, 15, 'T1', 'evaporation depth 15 cm'),
    ('N16b', 13, 45, 'T2', 'evaporation depth 45 cm'),
    ('N18a', 14, 0.10, 'T2', 'CN adjustment over the top 0.10 m'),
    ('N18b', 14, 1.00, 'T2', 'CN adjustment over the top 1.00 m'),
    ('N19a', 16, 0, 'T2', 'no salt diffusion between cells'),
    ('N19b', 16, 100, 'T2', 'full salt diffusion between cells'),
    ('N19c', 17, 50, 'T2', 'salt solubility 50 g/l'),
    ('N20a', 18, 8, 'T2', 'capillary-rise shape factor 8'),
    ('N20b', 18, 32, 'T2', 'capillary-rise shape factor 32'),
    ('E10',  15, 0, 'T1', 'CN not adjusted to antecedent moisture class'),
    ('E04a', 23, 50, 'T2', 'effective rainfall 50 %'),
    ('E04b', 23, 100, 'T2', 'effective rainfall 100 %'),
    ('E05a', 24, 1, 'T2', 'one shower per decade'),
    ('E05b', 24, 5, 'T2', 'five showers per decade'),
    ('E07',  22, 0, 'T2', 'daily rainfall not estimated by the USDA-SCS procedure'),
    ('E08',  25, 2, 'T2', 'soil-evaporation reduction parameter 2'),
]

# id, daily, particular, aggregate, tier, extra staged file, description
O_CASES = [
    ('O01', [],  [], 0, 'T1', None, 'no daily output at all'),
    ('O02', [1], [], 0, 'T1', None, 'daily output 1 only: the water balance'),
    ('O03', [2], [], 0, 'T1', None, 'daily output 2 only: crop development'),
    ('O04', [3], [], 0, 'T1', None, 'daily output 3 only: profile and root-zone water'),
    ('O05', [4], [], 0, 'T1', None, 'daily output 4 only: profile and root-zone salinity'),
    ('O06', [5], [], 0, 'T1', None, 'daily output 5 only: water content per compartment'),
    ('O07', [6], [], 0, 'T1', None, 'daily output 6 only: salinity per compartment'),
    ('O08', [7], [], 0, 'T1', None, 'daily output 7 only: climate inputs'),
    ('O09', [8], [], 0, 'T1', 'IRR_manual_dense.IRR', 'daily output 8 only: irrigation events'),
    ('O10', [1, 2, 3, 4, 5, 6, 7, 8], [], 0, 'T1', 'IRR_manual_dense.IRR', 'all eight daily outputs together'),
    ('O16', [1], [], 1, 'T1', None, 'intermediate results aggregated daily'),
    ('O17', [1], [], 2, 'T1', None, 'intermediate results aggregated per decade'),
    ('O18', [1], [], 3, 'T1', None, 'intermediate results aggregated monthly'),
    ('O21', [1], [], 0, 'T2', None, 'no particular results requested'),
]


def slug(t):
    return ''.join(c if c.isalnum() else '_' for c in t.lower())[:44].strip('_')


def emit(cid, why, tier, stage_extra, daily, part, agg, patch=''):
    name = f'{cid}_{slug(why)}'
    d = CASES / name
    d.mkdir(exist_ok=True)
    extra = f'\n  - {stage_extra}' if stage_extra else ''
    slot = f'\n      irr: {stage_extra}' if stage_extra else ''
    (d / 'case.yml').write_text(f"""id: {name}
desc: >
  {why}.
  {CROP} on the Ottawa 2014 record, Ottawa.SOL, sown {SEASON[0]}.
tier: {tier}

stage:
{chr(10).join('  - ' + a for a in BASE)}{extra}

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
      sol: Ottawa.SOL{slot}
{patch}
daily: {daily}
particular: {part}
aggregate: {agg}
rtol: 1.0e-3
""")


def main():
    CASES.mkdir(parents=True, exist_ok=True)
    for cid, line, val, tier, why in N_CASES:
        txt = f'{val:>6}      : {PPN_LABEL[line]}'
        patch = ('\npatch:\n  Ottawa.PPn:\n'
                 f'    {line}: {json.dumps(txt)}\n')
        emit(cid, why, tier, None, [1, 2], [], 0, patch)
    for cid, daily, part, agg, tier, extra, why in O_CASES:
        emit(cid, why, tier, extra, daily, part, agg)
    print(f'wrote {len(N_CASES) + len(O_CASES)} cases: '
          f"N={sum(1 for c in N_CASES if c[0][0] == 'N')} "
          f"E={sum(1 for c in N_CASES if c[0][0] == 'E')} O={len(O_CASES)}")


if __name__ == '__main__':
    main()
