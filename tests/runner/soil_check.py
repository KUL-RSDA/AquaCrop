#!/usr/bin/env python3
"""Independent check: a prediction of AquaCrop's soil-compartment geometry.

Ported by hand from the Fortran so that group F can assert what the *source
says* should happen, instead of only diffing against a frozen run. If the model
and this check disagree, one of them is wrong and that is worth knowing --
a self-referential freeze would hide it.

Mirrors, in call order:
  DetermineNrandThicknessCompartments   global.f90:4109
  ZrAdjustedToRestrictiveLayers         global.f90:1157
  RootMaxInSoilProfile                  global.f90:1123
  AdjustCompartments                    run.f90:6758   (the six-way guard)
  AdjustSizeCompartments                global.f90:6689
"""
from __future__ import annotations

import math
import struct
import sys

COMP_DEF_THICK = 0.10       # initialsettings.f90:223 -- hardcoded, no input exposes it
MAX_COMPARTMENTS = 12       # global.f90:22
MAX_SOIL_LAYERS = 5         # global.f90:21


def roundc(x: float) -> int:
    """Fortran roundc (utils.f90:91), which follows Pascal's Round.

    An exact half goes to the EVEN neighbour (banker's rounding: 2.5 -> 2,
    3.5 -> 4); anything else rounds to nearest, halves away from zero (nint).
    The difference matters: with the fix of 537d957 a crop Zrmax of 3.15 m makes
    the first regraded compartment exactly 2.5 * 0.05 m, which is 0.10 m here and
    was 0.15 m with a half-away rounding.
    """
    f = math.floor(x)
    if abs(x - f - 0.5) < sys.float_info.epsilon:
        even = abs(int(x)) % 2 == 0                  # int() truncates, like trunc()
        if x > 0:
            return f if even else math.ceil(x)
        return math.ceil(x) if even else f
    return math.floor(x + 0.5) if x >= 0 else math.ceil(x - 0.5)


def f32(x: float) -> float:
    """x rounded to single precision, the model's only real kind. Rounding the double
    result of one +, -, * or / of two single-precision values gives exactly the
    single-precision result, so step 4 below can follow the Fortran operation by
    operation."""
    return struct.unpack('f', struct.pack('f', x))[0]


EPS32 = 2.0 ** -23              # epsilon(0._sp)
AC_ZERO_THRESHOLD = f32(1e-6)   # global.f90 ac_zero_threshold


def roundc32(x: float) -> int:
    """roundc on a single-precision value: as roundc, with the tie window
    epsilon(0._sp). A sum like 6.50000048 is then not a tie and rounds up."""
    f = math.floor(x)
    if abs(f32(f32(x - f) - 0.5)) < EPS32:
        even = abs(int(x)) % 2 == 0
        if x > 0:
            return f if even else math.ceil(x)
        return math.ceil(x) if even else f
    return math.floor(x + 0.5) if x >= 0 else math.ceil(x - 0.5)


def tile(layers) -> list[float]:
    """DetermineNrandThicknessCompartments: fill with CompDefThick slices."""
    total = sum(L['thickness'] for L in layers)
    th, tot = [], 0.0
    while True:
        dz = total - tot
        th.append(COMP_DEF_THICK if dz > COMP_DEF_THICK else dz)
        tot += th[-1]
        if len(th) == MAX_COMPARTMENTS or abs(tot - total) < 1e-4:
            break
    return th


def zr_adjusted_to_restrictive(zr_in: float, layers) -> float:
    """ZrAdjustedToRestrictiveLayers."""
    n = len(layers)
    layi, zsoil = 0, layers[0]['thickness']
    zr_adj, zr_remain, delta_z = 0.0, zr_in, zsoil
    while True:
        zr_test = zr_adj + zr_remain * (layers[layi]['pen'] / 100.0)
        if (layi == n - 1 or layers[layi]['pen'] == 0
                or roundc(zr_test * 10000) <= roundc(zsoil * 10000)):
            return zr_test
        zr_adj = zsoil
        zr_remain -= delta_z / (layers[layi]['pen'] / 100.0)
        layi += 1
        zsoil += layers[layi]['thickness']
        delta_z = layers[layi]['thickness']


def root_max_in_soil_profile(zmax_crop: float, layers) -> float:
    """RootMaxInSoilProfile."""
    zmax, zsoil, layi = zmax_crop, 0.0, 0
    while layi < len(layers) and zmax > 0:
        if (layers[layi]['pen'] < 100
                and roundc(zsoil * 1000) < roundc(zmax_crop * 1000)):
            zmax = -9.0
        zsoil += layers[layi]['thickness']
        layi += 1
    if zmax < 0:
        zmax = zr_adjusted_to_restrictive(zmax_crop, layers)
    return zmax


def predict(layers, crop_zrmax: float, keep_swc_zrx: float | None = None) -> dict:
    """Full prediction: compartment thicknesses, count, and which branch ran."""
    th = tile(layers)
    tot = sum(th)
    soil_rootmax = root_max_in_soil_profile(crop_zrmax, layers)

    # AdjustCompartments (run.f90:6758)
    if keep_swc_zrx is not None:
        if roundc(keep_swc_zrx * 1000) > roundc(tot * 1000):
            target, branch = keep_swc_zrx, 'B1 KeepSWC, ConstZrx > TotDepth'
        else:
            return _done(th, 'B2 KeepSWC, ConstZrx <= TotDepth: no adjustment',
                         soil_rootmax)
    elif roundc(crop_zrmax * 1000) <= roundc(tot * 1000):
        return _done(th, 'B3 Zrx <= TotDepth: no adjustment', soil_rootmax)
    elif roundc(soil_rootmax * 1000) == roundc(crop_zrmax * 1000):
        target, branch = crop_zrmax, 'B4 no restrictive layer: adjust to Crop_RootMax'
    elif roundc(soil_rootmax * 1000) > roundc(tot * 1000):
        target, branch = soil_rootmax, 'B5 restrictive: adjust to Soil_RootMax'
    else:
        return _done(th, 'B6 restrictive, Soil_RootMax <= TotDepth: no adjustment',
                     soil_rootmax)

    # AdjustSizeCompartments (global.f90:6689).
    # Section 3 keeps the 1e-6 padding on the target; section 4 does not -- it
    # works from the true CropZx with the +/-1e-5 tolerance the surrounding
    # code already uses. Before fix 537d957 both used the padded value, which
    # turned an exact hit on CropZx into "still too shallow" and added a
    # further 0.05 m to the last compartment.
    steps, zx = [], target + 1e-6
    if len(th) < MAX_COMPARTMENTS:                                    # step 3
        while True:
            th.append(COMP_DEF_THICK if (zx - tot) > COMP_DEF_THICK else (zx - tot))
            tot += th[-1]
            if len(th) == MAX_COMPARTMENTS or (tot + 1e-5) >= zx:
                break
        steps.append('step3-extend')
    zx = target                                                       # step 4
    if (tot + 1e-5) < zx:
        # in single precision, operation by operation: the regrade lands on exact
        # halves of 0.05 m for some depths (Zrmax 3.15 m: fAdd is exactly 0.25 in
        # single precision, 0.24999999999999994 in double), and which way those
        # round is decided by the last bit
        zx = f32(zx)
        fadd = f32(f32(f32(zx / f32(0.1)) - 12.0) / 78.0)
        th = [f32(f32(0.05) * roundc32(f32(f32(f32(0.1) * f32(1.0 + f32(i * fadd))) * 20.0)))
              for i in range(1, 13)]
        tot = 0.0
        for t in th:
            tot = f32(tot + t)
        # the same +/-1e-5 tolerance the surrounding Fortran uses: a bare
        # `tot < zx` grows one step too far when the sum of 0.05 steps lands a
        # few ulp below the target (F39: 1.6499999... against 1.65)
        if f32(tot + f32(1e-5)) < zx:
            n = 0
            while f32(tot + f32(1e-5)) < zx:
                th[11] = f32(th[11] + f32(0.05)); tot = f32(tot + f32(0.05)); n += 1
            steps.append(f'step4-regrade+grow{n}')
        else:
            n = 0
            while f32(f32(tot - f32(0.05)) + AC_ZERO_THRESHOLD) >= zx:   # global.f90:7190
                th[11] = f32(th[11] - f32(0.05)); tot = f32(tot - f32(0.05)); n += 1
            steps.append(f'step4-regrade' + (f'+shrink{n}' if n else ''))
    return _done(th, branch + ' | ' + ' '.join(steps), soil_rootmax)


def _done(th, branch, soil_rootmax):
    return {'thicknesses': [round(t, 4) for t in th], 'n': len(th),
            'total': round(sum(th), 4), 'branch': branch,
            'soil_rootmax': round(soil_rootmax, 4)}


def layers_from_sol(path) -> list[dict]:
    """Parse the layer block of a .SOL file (list-directed, so split on space)."""
    lines = [l for l in open(path).read().splitlines()]
    version = float(lines[1].split(':')[0])
    nlayers = int(lines[4].split(':')[0])
    v10 = roundc(version * 10)
    out = []
    for raw in lines[8:8 + nlayers]:
        t = raw.split()
        if v10 >= 60:
            out.append(dict(thickness=float(t[0]), sat=float(t[1]), fc=float(t[2]),
                            wp=float(t[3]), ksat=float(t[4]), pen=int(t[5]),
                            gravel=int(t[6])))
        else:                       # v < 6.0 has no penetrability/gravel columns
            out.append(dict(thickness=float(t[0]), sat=float(t[1]), fc=float(t[2]),
                            wp=float(t[3]), ksat=float(t[4]), pen=100, gravel=0))
    return out


def gravel_volume(sat_pct: float, gravel_mass_pct: float) -> float:
    """FromGravelMassToGravelVolume (global.f90:1551): mass % -> volume %."""
    if gravel_mass_pct <= 0:
        return 0.0
    mineral_bd = 2.65                                   # Mg/m3
    matrix_bd = mineral_bd * (1.0 - sat_pct / 100.0)
    soil_bd = 100.0 / (gravel_mass_pct / mineral_bd
                       + (100.0 - gravel_mass_pct) / matrix_bd)
    return gravel_mass_pct * (soil_bd / mineral_bd)


def compartment_layer(thicknesses, layers):
    """Which soil layer each compartment sits in (by its midpoint)."""
    edges, acc = [], 0.0
    for L in layers:
        acc += L['thickness']
        edges.append(acc)
    out, depth = [], 0.0
    for t in thicknesses:
        mid = depth + t / 2.0
        depth += t
        idx = next((i for i, e in enumerate(edges) if mid <= e + 1e-9),
                   len(layers) - 1)
        out.append(idx)
    return out


def number_soil_class(sat: float, fc: float, wp: float, ksat: float) -> int:
    """NumberSoilClass (global.f90): 1 sandy, 2 loamy, 3 sandy-clayey,
    4 silty-clayey."""
    if sat <= 55.0:
        if wp >= 20.0:
            if sat >= 49.0 and fc >= 40.0:
                return 4
            return 3
        if fc < 23.0:
            return 1
        if wp > 16.0 and ksat < 100.0:
            return 3
        if wp < 6.0 and fc < 28.0 and ksat > 750.0:
            return 1
        return 2
    if wp > 20.0:
        return 4
    return 3


def parameters_cr(soil_class: int, ksat: float):
    """DetermineParametersCR (global.f90:4080): the capillary-rise a/b."""
    if round(ksat * 1000) <= 0:
        return -9.0, -9.0
    if soil_class == 1:
        return -0.3112 - ksat / 100000.0, -1.4936 + 0.2416 * math.log(ksat)
    if soil_class == 2:
        return -0.4986 + 9.0 * ksat / 100000.0, -2.1320 + 0.4778 * math.log(ksat)
    if soil_class == 3:
        return -0.5677 - 4.0 * ksat / 100000.0, -3.7189 + 0.5922 * math.log(ksat)
    return -0.6366 + 8.0 * ksat / 10000.0, -1.9165 + 0.7063 * math.log(ksat)


def salt_cells(ksat: float) -> int:
    """LoadProfileProcessing: SCP1 from the infiltration rate."""
    return 11 if ksat <= 112.0 else max(2, roundc(1.6 + 1000.0 / ksat))
