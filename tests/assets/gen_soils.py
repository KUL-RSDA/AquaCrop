#!/usr/bin/env python3
"""Generate the .SOL profiles the test plan needs, into tests/assets/soils/.

Geometry variants deliberately keep a single texture (the Ottawa sandy loam) so
that compartment arithmetic is the only thing changing between them. Texture
sweeps are a separate axis and live in the hydraulic block at the bottom.

Layer lines are read list-directed (global.f90:LoadProfile), so column
alignment is cosmetic; token order is what matters:

    v >= 6.0 : thickness SAT FC WP Ksat penetrability gravel CRa CRb description
    v 4.0-5.x: thickness SAT FC WP Ksat CRa CRb <blank> description
    v <  4.0 : thickness SAT FC WP Ksat <blank> description
"""
from __future__ import annotations

import pathlib

OUT = pathlib.Path(__file__).resolve().parent / 'soils'

# texture presets: SAT, FC, WP, Ksat, CRa, CRb, CN, REW, label
SANDY_LOAM = (46.0, 29.0, 13.0, 1200.0, -0.390600, 1.255639, 46, 7, 'sandy_loam')
LOAM       = (50.0, 30.0, 10.0,  500.0, -0.453600, 0.837340, 61, 9, 'loam')
SILT_LOAM  = (46.0, 33.0, 13.0,  150.0, -0.485100, 0.262082, 61, 11, 'silt_loam')
CLAY       = (51.1, 42.0, 27.0,  250.0, -0.436600, 1.983308, 77, 14, 'clay')


def layer(thickness, texture=SANDY_LOAM, ksat=None, pen=100, gravel=0, desc=None):
    sat, fc, wp, k, cra, crb, _cn, _rew, label = texture
    return dict(thickness=thickness, sat=sat, fc=fc, wp=wp,
                ksat=k if ksat is None else ksat, pen=pen, gravel=gravel,
                cra=cra, crb=crb, desc=desc or label)


def render(desc, layers, cn, rew, version='7.3', note='(January 2026)'):
    v10 = round(float(version) * 10)
    out = [desc,
           f'        {version}                 : AquaCrop Version {note}',
           f'       {cn:2d}                   : CN (Curve Number)',
           f'       {rew:2d}                   : Readily evaporable water from top layer (mm)',
           f'        {len(layers)}                   : number of soil horizons',
           f'       -9                   : variable no longer applicable']
    if v10 >= 60:
        out += ['  Thickness  Sat   FC    WP     Ksat   Penetrability  Gravel  CRa       CRb           description',
                '  ---(m)-   ----(vol %)-----  (mm/day)      (%)        (%)    -----------------------------------------']
        for L in layers:
            out.append(f"    {L['thickness']:4.2f}    {L['sat']:4.1f}  {L['fc']:4.1f}  "
                       f"{L['wp']:4.1f}  {L['ksat']:6.1f}        {L['pen']:3d}        "
                       f"{L['gravel']:3d}     {L['cra']:9.6f}  {L['crb']:8.6f}   {L['desc']}")
    elif v10 >= 40:
        out += ['  Thickness  Sat   FC    WP     Ksat   CRa       CRb           description',
                '  ---(m)-   ----(vol %)-----  (mm/day) -----------------------------------------']
        for L in layers:
            out.append(f"    {L['thickness']:4.2f}    {L['sat']:4.1f}  {L['fc']:4.1f}  "
                       f"{L['wp']:4.1f}  {L['ksat']:6.1f}  {L['cra']:9.6f}  {L['crb']:8.6f}"
                       f"   -   {L['desc']}")
    else:
        out += ['  Thickness  Sat   FC    WP     Ksat        description',
                '  ---(m)-   ----(vol %)-----  (mm/day)  ---------------------------------']
        for L in layers:
            out.append(f"    {L['thickness']:4.2f}    {L['sat']:4.1f}  {L['fc']:4.1f}  "
                       f"{L['wp']:4.1f}  {L['ksat']:6.1f}   -   {L['desc']}")
    return '\n'.join(out) + '\n'


CN, REW = SANDY_LOAM[6], SANDY_LOAM[7]

SOILS: dict[str, dict] = {}


def add(name, desc, layers, cn=CN, rew=REW, **kw):
    SOILS[name] = dict(desc=desc, layers=layers, cn=cn, rew=rew, **kw)


# ---- F.1 compartment tiling -------------------------------------------------
# 12 compartments x CompDefThick (0.10 m, hardcoded) => 1.20 m is the ceiling
for t, why in [(0.05, 'thinner than one compartment'),
               (0.30, 'exactly 3 compartments'),
               (0.40, '4 compartments'),
               (0.55, '5 x 0.10 + a ragged 0.05'),
               (0.60, 'exactly 6 compartments'),
               (1.20, 'exactly 12 compartments - the max_No_compartments boundary'),
               (1.25, '12 compartments reach 1.20; 0.05 m is not represented'),
               (3.00, 'far past what 12 default compartments can tile'),
               (4.00, 'as deep as DEFAULT.SOL but the baseline texture')]:
    add(f'GEOM_{t:.2f}m'.replace('.', 'p'),
        f'sandy loam, {t:.2f} m single horizon - {why}', [layer(t)])

add('GEOM_ragged_layers',
    'sandy loam in 3 horizons whose edges never fall on a compartment boundary',
    [layer(0.13), layer(0.27), layer(0.41)])
add('GEOM_thin_top',
    'sandy loam with a 0.04 m top horizon - one compartment spans two layers',
    [layer(0.04, desc='thin_top'), layer(1.46)])

# ---- F.3 restrictive and impermeable layers ---------------------------------
add('PEN_zero_at_0p30', 'impermeable layer starting at 0.30 m',
    [layer(0.30), layer(1.20, pen=0, desc='impermeable')])
add('PEN_zero_at_0p60', 'impermeable third horizon starting at 0.60 m',
    [layer(0.30), layer(0.30), layer(0.90, pen=0, desc='impermeable')])
add('PEN_zero_layer1', 'impermeable from the surface - degenerate ZrOUT = 0',
    [layer(0.30, pen=0, desc='impermeable'), layer(1.20)])
add('PEN_zero_last', 'impermeable last horizon - the layi == NrSoilLayers exit',
    [layer(0.60), layer(0.90, pen=0, desc='impermeable')])
add('PEN_50', 'half-penetrable second horizon',
    [layer(0.30), layer(1.20, pen=50, desc='half_pen')])
add('PEN_25', 'quarter-penetrable second horizon - roots stop mid-layer',
    [layer(0.30), layer(1.20, pen=25, desc='quarter_pen')])
add('PEN_graded', 'penetrability graded 100 / 50 / 25 / 0 over four horizons',
    [layer(0.30), layer(0.30, pen=50), layer(0.30, pen=25),
     layer(0.60, pen=0, desc='impermeable')])
add('PEN_below_zrmax', 'restrictive horizon below Zrmax - the Zsoil < ZmaxCrop guard',
    [layer(1.50), layer(1.50, pen=25, desc='deep_restrict')])
add('PEN_99', 'penetrability 99 % - rounding at the "< 100" test',
    [layer(0.30), layer(1.20, pen=99, desc='near_full')])
add('PEN_in_evap_layer', 'restrictive horizon inside the 0-0.30 m evaporation layer',
    [layer(0.10), layer(1.40, pen=50, desc='shallow_restrict')])

# ---- F.5 hydraulic properties and layer count -------------------------------
# Ksat drives the salt-cell count: <=112 -> 11 cells, else round(1.6 + 1000/Ksat)
for k, cells in [(112.0, 11), (50.0, 11), (5.0, 11), (200.0, 7), (330.0, 5)]:
    add(f'KSAT_{k:.0f}', f'sandy loam at Ksat {k:.0f} mm/day -> {cells} salt cells',
        [layer(1.50, ksat=k)])
# Ksat drives tau (drainage rate) and the salt-cell count; 1500 sits at the
# tau ceiling and 3000 clamps SCP1 to its minimum of 2
add('KSAT_1500', 'sandy loam at Ksat 1500 mm/day - tau at its ceiling',
    [layer(1.50, ksat=1500.0)])
add('KSAT_3000', 'sandy loam at Ksat 3000 mm/day - salt cells clamped to 2',
    [layer(1.50, ksat=3000.0)])
add('EVAP_BOUNDARY', 'a horizon boundary at 0.15 m, inside the evaporation layer',
    [layer(0.15, desc='top'), layer(1.35, LOAM)])
add('ROOTZONE_BOUNDARY', 'a horizon boundary at 0.20 m, inside the root zone',
    [layer(0.20, desc='top'), layer(1.30, CLAY)])
add('KSAT_mixed', 'three horizons with different Ksat -> different salt-cell counts',
    [layer(0.50, ksat=1200.0), layer(0.50, ksat=200.0), layer(0.50, ksat=50.0)])

for g in (15, 30, 60):
    add(f'GRAVEL_{g}', f'sandy loam with {g} % gravel by mass', [layer(1.50, gravel=g)])
for r in (3, 15):
    add(f'REW_{r}', f'sandy loam with REW {r} mm', [layer(1.50)], rew=r)

add('LAYERS_5', 'five horizons - the max_SoilLayers boundary',
    [layer(0.30), layer(0.30, LOAM), layer(0.30, SILT_LOAM),
     layer(0.30, CLAY), layer(0.30)])
add('LAYERS_6', 'six horizons - one past max_SoilLayers',
    [layer(0.25), layer(0.25, LOAM), layer(0.25, SILT_LOAM),
     layer(0.25, CLAY), layer(0.25), layer(0.25, LOAM)])
add('YOLO_3L_v73',
    'Yolo geometry on a v7.3 file: clay loam / sandy loam lens / clay loam',
    [dict(layer(1.80), sat=51.0, fc=33.0, wp=13.8, ksat=100.0, desc='YoloClayLoam'),
     dict(layer(0.30), sat=35.0, fc=20.0, wp=10.0, ksat=150.0, desc='YoloSandyLoam'),
     dict(layer(0.90), sat=51.0, fc=33.0, wp=13.8, ksat=100.0, desc='YoloClayLoam')],
    cn=60, rew=8)
add('COARSE_OVER_FINE', 'sand over clay - perching at the horizon boundary',
    [layer(0.50), layer(1.00, CLAY)])
add('FINE_OVER_COARSE', 'clay over sand - a capillary barrier',
    [layer(0.50, CLAY), layer(1.00)])
add('NARROW_DRAINABLE', 'SAT - FC only 3 vol% - very little drainable porosity',
    [dict(layer(1.50), sat=32.0, fc=29.0)])
add('NARROW_TAW', 'FC - WP only 3 vol% - very little available water',
    [dict(layer(1.50), fc=29.0, wp=26.0)])
add('INVALID_WP_GT_FC', 'WP above FC - an invalid profile, for the error path',
    [dict(layer(1.50), fc=20.0, wp=30.0)])


# ---- v3.0 read-path probes ---------------------------------------------------
# LoadProfile's "v < 4.0" branch (global.f90:7756) reads SEVEN values:
#   thickness SAT FC WP Ksat <blank> description
# A record with only six tokens makes the list-directed read run on into the
# next record, and eventually off the end of the file. These four files
# triangulate whether that is the reader's fault or the file's.
def _v30(desc, layers):
    return dict(desc=desc, layers=layers, cn=60, rew=8, version='3.0')

SOILS['V30_1L_spacer'] = _v30(
    'v3.0, one horizon, spacer column present - should load',
    [layer(1.50, desc='sandy_loam')])
SOILS['V30_3L_spacer'] = _v30(
    'v3.0, three horizons, spacer column present - should load',
    [layer(0.50), layer(0.50, LOAM), layer(0.50, CLAY)])
SOILS['V30_1L_nospacer'] = _v30(
    'v3.0, one horizon, NO spacer and a one-word description - 6 tokens',
    [layer(1.50, desc='sandy_loam')])
SOILS['V30_1L_twoword'] = _v30(
    'v3.0, one horizon, no spacer but a TWO-WORD description - 7 tokens',
    [layer(1.50, desc='sandy loam')])

# The v4.0-5.x branch (global.f90:7772) has the same shape: it reads NINE
# values, thickness SAT FC WP Ksat CRa CRb <blank> description, so a file
# written without the spacer supplies eight and fails the same way.
def _v45(desc, layers):
    return dict(desc=desc, layers=layers, cn=60, rew=8, version='5.0')

SOILS['V45_1L_spacer'] = _v45(
    'v5.0, one horizon, spacer column present - 9 tokens, should load',
    [layer(1.50, desc='sandy_loam')])
SOILS['V45_1L_nospacer'] = _v45(
    'v5.0, one horizon, NO spacer and a one-word description - 8 tokens',
    [layer(1.50, desc='sandy_loam')])


# ---- a texture ladder for sweep SW01 ----------------------------------------
# Constructed to span all four NumberSoilClass branches and a wide Ksat range,
# not transcribed from the FAO texture table. CRa/CRb are computed with the
# ported DetermineParametersCR, which reproduces the CRa/CRb of every shipped
# .SOL exactly (Ottawa, DEFAULT, CLAY_LIS, SILT_LOAM_LIS).
import sys as _sys
_sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent.parent / 'runner'))
from soil_check import number_soil_class as _cls, parameters_cr as _cr  # noqa: E402

LADDER = [
    ('sand',            36.0, 13.0,  6.0, 3000.0),
    ('loamy_sand',      38.0, 16.0,  8.0, 2200.0),
    ('sandy_loam',      41.0, 22.0, 10.0, 1200.0),
    ('loam',            46.0, 31.0, 15.0,  500.0),
    ('silt_loam',       46.0, 33.0, 13.0,  575.0),
    ('silt',            43.0, 33.0,  9.0,  500.0),
    ('sandy_clay_loam', 47.0, 32.0, 20.0,  225.0),
    ('clay_loam',       50.0, 39.0, 23.0,  125.0),
    ('silty_clay_loam', 52.0, 44.0, 23.0,  150.0),
    ('sandy_clay',      50.0, 39.0, 27.0,   35.0),
    ('silty_clay',      54.0, 50.0, 32.0,   15.0),
    ('clay',            55.0, 54.0, 39.0,    2.0),
]
for _n, _sat, _fc, _wp, _k in LADDER:
    _c = _cls(_sat, _fc, _wp, _k)
    _a, _b = _cr(_c, _k)
    SOILS[f'TEX_{_n}'] = dict(
        desc=f'{_n.replace("_", " ")} - soil class {_c}, Ksat {_k:.0f} mm/day',
        cn=61, rew=9,
        layers=[dict(thickness=3.00, sat=_sat, fc=_fc, wp=_wp, ksat=_k,
                     pen=100, gravel=0, cra=_a, crb=_b, desc=_n)])


def main():
    OUT.mkdir(parents=True, exist_ok=True)
    for name, s in sorted(SOILS.items()):
        text = render(s['desc'], s['layers'], s['cn'], s['rew'],
                      s.get('version', '7.3'))
        if 'nospacer' in name or 'twoword' in name:
            text = text.replace('   -   ', '   ')      # drop the spacer column
        (OUT / f'{name}.SOL').write_text(text)
    print(f'wrote {len(SOILS)} .SOL files to {OUT}')
    for name, s in sorted(SOILS.items()):
        tot = sum(L['thickness'] for L in s['layers'])
        print(f"  {name:<22} {len(s['layers'])}L  {tot:5.2f} m   {s['desc']}")


if __name__ == '__main__':
    main()
