#!/usr/bin/env python3
"""Generate the group F cases (soil geometry, compartments, rooting depth).

Every case is the same maize season on the Ottawa record -- sown 21 May 2014,
matures 24 September, 163 days of simulation -- so the soil profile and the
crop's Zrmax are the only things that vary. Daily outputs 3 and 5 are enabled
because Out5CompWC writes one column per compartment, which is what makes the
compartment arithmetic observable at all.

Each case records the prediction of the independent check (runner/soil_check.py) (branch, compartment count, total
depth) in its case.yml. `expect_compartments` is asserted by the runner
independently of the frozen reference.
"""
from __future__ import annotations

import json
import pathlib
import sys

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent.parent / 'runner'))
from soil_check import layers_from_sol, predict, salt_cells   # noqa: E402

ROOT = pathlib.Path(__file__).resolve().parent.parent
SOILS = ROOT / 'assets' / 'soils'
CASES = ROOT / 'cases'

CROP = 'MaizeGDD.CRO'
ZRMAX_LINE = 38          # "Maximum effective rooting depth (m)"
ZRMIN_LINE = 37
SEASON = ['2014-05-21', '2014-10-31']

BASE_STAGE = ['Ottawa.CLI', 'Ottawa.Tnx', 'Ottawa.ETo', 'Ottawa.PLU',
              'MaunaLoa.CO2', 'Ottawa.PPn', 'DEFAULT.CRO', 'DEFAULT.SOL']

# id, soil, Zrmax, tier, what the case is for
#: cases that document a defect rather than a property. Keyed by case id.
#: Empty by policy -- the suite does not carry cases that are known to fail.
#: Defects are written up in TESTPLAN.md instead. The machinery stays in the
#: runner for the case where a regression needs quarantining mid-investigation.
KNOWN_DEFECTS: dict[str, str] = {}

#: cases where AquaCrop must stop: id -> text its console output must contain
EXPECT_ERROR = {
    'F56': 'soil horizons, but AquaCrop can handle at most',
}

#: Cases removed because they cannot pass until AquaCrop changes. Move a row
#: back into CASES_F once its defect is fixed, then regenerate and freeze.
#: The soils they need are all still produced by gen_soils.py.
RETIRED: list[tuple] = [
    # BUG-1 is fixed; these variants are not needed, F54f and F54g cover it
    # (the '-' spacer column is not a real file format)
    ('F54a', 'V30_1L_spacer',    1.00, 'T1', 'v3.0 one horizon WITH the spacer column'),
    ('F54b', 'V30_3L_spacer',    1.00, 'T1', 'v3.0 three horizons with the spacer column'),
    ('F54c', 'V30_1L_nospacer',  1.00, 'T1', 'v3.0 one horizon, no spacer, one-word description'),
    ('F54d', 'V30_1L_twoword',   1.00, 'T1', 'v3.0 one horizon, no spacer, two-word description'),
    ('F54e', 'V45_1L_spacer',    1.00, 'T1', 'v5.0 one horizon WITH the spacer column'),
]

CASES_F: list[tuple] = [
    # BUG-2: more horizons than max_SoilLayers is reported before the run stops
    ('F56', 'LAYERS_6',          1.00, 'T3', 'six horizons: one past max_SoilLayers'),
    # revived after BUG-5: a 0.10 m first horizon no longer ends the crop
    ('F44', 'PEN_in_evap_layer', 3.00, 'T2', 'restrictive horizon inside the evaporation layer'),
    # revived after BUG-1: legacy .SOL layer lines (v3.0 and v5.0) now load
    ('F54f', 'V45_1L_nospacer',  1.00, 'T1', 'v5.0 one horizon, no spacer, one-word description'),
    ('F54g', 'YoloClayLoam6.SOL', 1.00, 'T1', 'a real v3.0 file with three horizons and no spacer'),
    # revived after BUG-4: one compartment now gives one output column
    ('F07', 'GEOM_0p05m',        0.05, 'T3', 'profile thinner than one compartment'),
    # --- F.1 compartment tiling -------------------------------------------
    ('F01', 'GEOM_0p30m',        0.30, 'T1', '0.30 m profile tiles as exactly 3 compartments'),
    ('F02', 'GEOM_0p55m',        0.55, 'T1', '0.55 m profile: 5 x 0.10 plus a ragged 0.05'),
    ('F03', 'GEOM_1p20m',        1.00, 'T1', '1.20 m profile hits max_No_compartments exactly'),
    ('F04', 'GEOM_1p25m',        1.00, 'T2', '1.25 m profile: 0.05 m is never represented'),
    ('F05', 'Ottawa.SOL',        1.00, 'T1', '1.50 m Ottawa profile, 0.30 m unrepresented'),
    ('F06', 'GEOM_4p00m',        1.00, 'T2', '4.00 m profile, 2.80 m unrepresented'),
    ('F11', 'GEOM_ragged_layers', 0.80, 'T1', 'layer edges never fall on a compartment boundary'),
    ('F12', 'GEOM_thin_top',     1.00, 'T2', '0.04 m top layer: one compartment spans two layers'),
    # --- F.2 compartment expansion ----------------------------------------
    ('F13', 'Ottawa.SOL',        1.00, 'T1', 'B3: Zrx below TotDepth, no adjustment at all'),
    ('F14', 'Ottawa.SOL',        3.00, 'T1', 'B4: the A01 path, step-4 regrade to 3.05 m'),
    ('F15', 'GEOM_0p30m',        1.00, 'T1', 'step 3 extends 3 compartments to 10'),
    ('F16', 'GEOM_0p30m',        1.20, 'T1', 'step 3 reaches exactly 12 compartments'),
    ('F17', 'GEOM_0p30m',        1.30, 'T1', 'step 3 caps at 12, then step 4 regrades'),
    ('F18', 'GEOM_0p60m',        0.60, 'T2', 'Zrx equals the tiled depth: nothing to add'),
    ('F19', 'Ottawa.SOL',        1.20, 'T1', 'Zrx exactly at the 12 x CompDefThick boundary'),
    ('F20', 'Ottawa.SOL',        1.21, 'T1', 'one millimetre past the boundary: step 4 fires'),
    ('F21', 'Ottawa.SOL',        1.25, 'T2', 'step-4 regrade with the smallest useful fAdd'),
    ('F22', 'Ottawa.SOL',        2.18, 'T1', 'step-4 regrade OVERSHOOTS: the shrink branch'),
    ('F23', 'Ottawa.SOL',        3.15, 'T1', 'step-4 regrade overshoots by two 0.05 steps'),
    ('F24', 'Ottawa.SOL',        6.00, 'T3', 'extreme regrade, thickest compartments'),
    ('F25', 'GEOM_0p40m',        3.00, 'T1', 'step 3 then step 4 in the same call'),
    ('F26', 'GEOM_4p00m',        3.00, 'T2', 'deep profile regraded to 3.05 m'),
    # --- F.3 restrictive and impermeable layers ---------------------------
    ('F35', 'PEN_zero_at_0p30',  3.00, 'T1', 'B6: impermeable at 0.30 m, no adjustment despite a 3 m crop'),
    ('F36', 'PEN_zero_at_0p60',  3.00, 'T1', 'B6: impermeable third horizon at 0.60 m'),
    ('F37', 'PEN_zero_layer1',   3.00, 'T3', 'impermeable from the surface: Soil_RootMax = 0'),
    ('F38', 'PEN_zero_last',     3.00, 'T2', 'impermeable last horizon: the layi == NrSoilLayers exit'),
    ('F39', 'PEN_50',            3.00, 'T1', 'B5: half-penetrable, adjust to Soil_RootMax'),
    ('F40', 'PEN_graded',        3.00, 'T1', 'penetrability graded 100/50/25/0 over four horizons'),
    ('F41', 'PEN_25',            3.00, 'T1', 'quarter-penetrable: roots stop mid-layer'),
    ('F42', 'PEN_below_zrmax',   1.00, 'T2', 'restrictive horizon below Zrmax: no restriction applies'),
    ('F43', 'PEN_99',            3.00, 'T3', 'penetrability 99 %: rounding at the "< 100" test'),
    # --- F.4 rooting depth vs profile -------------------------------------
    ('F45', 'GEOM_0p40m',        3.00, 'T1', 'Zrmax far exceeds a 0.40 m profile'),
    ('F48', 'GEOM_1p20m',        1.20, 'T2', 'Zrmax exactly equals the profile depth'),
    ('F49', 'GEOM_0p30m',        0.30, 'T2', 'shallow soil and shallow roots together'),
    # --- F.5 hydraulic properties and layer count -------------------------
    ('F54', 'YOLO_3L_v73',       1.00, 'T1', 'three horizons, a coarse lens between two fine ones'),
    ('F55', 'LAYERS_5',          1.00, 'T1', 'five horizons: the max_SoilLayers boundary'),
    ('F57', 'COARSE_OVER_FINE',  1.00, 'T1', 'sand over clay: perching at the boundary'),
    ('F58', 'FINE_OVER_COARSE',  1.00, 'T1', 'clay over sand: a capillary barrier'),
    ('F59', 'Ottawa.SOL',        1.00, 'T1', 'Ksat 1200 -> 2 salt cells'),
    ('F60', 'DEFAULT.SOL',       1.00, 'T1', 'Ksat 500 -> 4 salt cells'),
    ('F61', 'KSAT_330',          1.00, 'T1', 'Ksat 330 -> 5 salt cells'),
    ('F62', 'KSAT_200',          1.00, 'T1', 'Ksat 200 -> 7 salt cells'),
    ('F63', 'KSAT_112',          1.00, 'T1', 'Ksat 112: the "<= 112" boundary -> 11 salt cells'),
    ('F64', 'KSAT_50',           1.00, 'T1', 'Ksat 50: slow drainage'),
    ('F65', 'KSAT_5',            1.00, 'T1', 'Ksat 5: near-impermeable, waterlogging'),
    ('F66', 'KSAT_mixed',        1.00, 'T1', 'three horizons with different salt-cell counts'),
    ('F67', 'GRAVEL_15',         1.00, 'T1', '15 % gravel by mass'),
    ('F68', 'GRAVEL_30',         1.00, 'T1', '30 % gravel by mass'),
    ('F69', 'GRAVEL_60',         1.00, 'T2', '60 % gravel by mass'),
    ('F70', 'REW_3',             1.00, 'T2', 'REW 3 mm: a short stage I'),
    ('F71', 'REW_15',            1.00, 'T2', 'REW 15 mm: a long stage I'),
    ('F72', 'NARROW_DRAINABLE',  1.00, 'T2', 'SAT - FC of 3 vol%: little drainable porosity'),
    ('F73', 'NARROW_TAW',        1.00, 'T2', 'FC - WP of 3 vol%: little available water'),
]


def slug(text):
    return ''.join(c if c.isalnum() else '_' for c in text.lower())[:44].strip('_')


def main():
    CASES.mkdir(parents=True, exist_ok=True)
    written, rows = 0, []
    for cid, soil, zrmax, tier, why in CASES_F:
        sol = soil if soil.endswith('.SOL') else f'{soil}.SOL'
        layers = layers_from_sol(SOILS / sol)
        p = predict(layers, zrmax)
        cells = sorted({salt_cells(L['ksat']) for L in layers})

        defect = KNOWN_DEFECTS.get(cid)
        name = f'{cid}_{slug(why)}'
        d = CASES / name
        d.mkdir(exist_ok=True)
        (d / 'case.yml').write_text(f"""id: {name}
desc: >
  {why}.
  Soil {sol} ({len(layers)} horizon(s), {sum(L['thickness'] for L in layers):.2f} m),
  maize Zrmax {zrmax:.2f} m, Ottawa 2014 season.
tier: {tier}

stage:
{chr(10).join('  - ' + a for a in BASE_STAGE)}
  - {CROP}
  - {sol}

project:
  name: {cid}
  desc: {json.dumps(f"{cid} - {why}")}
  runs:
    - year: 1
      sim: [{SEASON[0]}, {SEASON[1]}]
      cli: Ottawa.CLI
      tnx: Ottawa.Tnx
      eto: Ottawa.ETo
      plu: Ottawa.PLU
      co2: MaunaLoa.CO2
      cro: {CROP}
      sol: {sol}

patch:
  {CROP}:
    {ZRMAX_LINE}: "     {zrmax:.2f}      : Maximum effective rooting depth (m)"

# Out5CompWC writes one WC column per compartment, so the daily output is what
# makes the compartment arithmetic observable.
daily: [3, 5]
particular: []
aggregate: 0

# Predicted independently by tests/runner/soil_check.py, asserted by the runner.
expect_compartments: {p['n']}
predict:
  branch: {json.dumps(p["branch"])}
  total_depth: {p['total']}
  soil_rootmax: {p['soil_rootmax']}
  salt_cells: {cells}
  thicknesses: [{', '.join(f'{t:.2f}' for t in p['thicknesses'])}]

rtol: 1.0e-3
""" + (f'\nknown_defect: {json.dumps(defect)}\n' if defect else '')
  + (f'expect_error: {json.dumps(EXPECT_ERROR[cid])}\n'
     if cid in EXPECT_ERROR else ''))
        written += 1
        rows.append((cid, sol, zrmax, p['n'], p['total'], p['soil_rootmax'],
                     p['branch'], cells))

    print(f'wrote {written} group-F cases to {CASES}\n')
    print(f"{'id':<5} {'soil':<20} {'Zrx':>5} {'nc':>3} {'depth':>6} {'SRmax':>6} "
          f"{'cells':<8} branch")
    print('-' * 118)
    for cid, sol, zx, n, tot, srm, br, cells in rows:
        print(f'{cid:<5} {sol:<20} {zx:5.2f} {n:3d} {tot:6.2f} {srm:6.3f} '
              f'{str(cells):<8} {br}')


if __name__ == '__main__':
    main()
