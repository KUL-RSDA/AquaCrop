#!/usr/bin/env python3
"""An independent prediction of AquaCrop's growing-degree-day arithmetic.

Hand-ported from `DegreesDay` (global.f90:2449) so group C can assert what the
source implies rather than only diffing a freeze -- the same approach that
validated the compartment geometry in group F.

The three methods differ in how Tmin and Tmax are clamped before averaging, and
they disagree exactly when Tmin < Tbase < Tmax, which is the common case in a
temperate spring. Method 3 is AquaCrop's default.
"""
from __future__ import annotations

import datetime
import pathlib

EPOCH = datetime.date(1900, 12, 31)


def degrees_day(tmin: float, tmax: float, tbase: float, tupper: float,
                method: int = 3) -> float:
    """DegreesDay(): growing degrees for one day."""
    if method == 1:
        # no adjustment of Tmax/Tmin before averaging
        tavg = (tmax + tmin) / 2.0
        tavg = min(tavg, tupper)
        tavg = max(tavg, tbase)
    elif method == 2:
        # both bounds clamped into [Tbase, Tupper] first
        star_max = min(max(tmax, tbase), tupper)
        star_min = min(max(tmin, tbase), tupper)
        tavg = (star_max + star_min) / 2.0
    else:
        # method 3 (default): Tmax clamped both ways, Tmin only from above,
        # then the average is floored at Tbase
        star_max = min(max(tmax, tbase), tupper)
        star_min = min(tmin, tupper)          # note: NOT floored at Tbase here
        tavg = (star_max + star_min) / 2.0
        tavg = max(tavg, tbase)
    return tavg - tbase


def read_tnx(path) -> tuple[datetime.date, list[tuple[float, float]]]:
    """First record date and the (Tmin, Tmax) series of a .Tnx file."""
    lines = pathlib.Path(path).read_text().splitlines()
    day = int(lines[2].split()[0])
    month = int(lines[3].split()[0])
    year = int(lines[4].split()[0])
    rows = []
    for line in lines[8:]:
        if line.strip():
            a, b = line.split()[:2]
            rows.append((float(a), float(b)))
    return datetime.date(year, month, day), rows


def series(tnx, start, end, tbase, tupper, method=3) -> list[float]:
    """Daily growing degrees over [start, end] inclusive."""
    first, rows = read_tnx(tnx)
    if isinstance(start, str):
        start = datetime.date.fromisoformat(start)
    if isinstance(end, str):
        end = datetime.date.fromisoformat(end)
    i0 = (start - first).days
    i1 = (end - first).days
    if i0 < 0 or i1 >= len(rows):
        raise ValueError(f'{start}..{end} outside the record '
                         f'{first}..{first + datetime.timedelta(days=len(rows) - 1)}')
    return [degrees_day(tn, tx, tbase, tupper, method)
            for tn, tx in rows[i0:i1 + 1]]


def cumulative(tnx, start, end, tbase, tupper, method=3) -> float:
    return sum(series(tnx, start, end, tbase, tupper, method))


def day_reaching(tnx, start, target, tbase, tupper, method=3):
    """Date on which the cumulative GDD first reaches `target`, or None."""
    first, rows = read_tnx(tnx)
    if isinstance(start, str):
        start = datetime.date.fromisoformat(start)
    acc = 0.0
    for k, (tn, tx) in enumerate(rows[(start - first).days:]):
        acc += degrees_day(tn, tx, tbase, tupper, method)
        if acc >= target:
            return start + datetime.timedelta(days=k), acc
    return None, acc


def crop_params(cro) -> dict:
    """Tbase, Tupper, GDDaysToHarvest and the cycle mode from a .CRO file."""
    L = pathlib.Path(cro).read_text().splitlines()
    num = lambda i: float(L[i].split(':')[0])
    return {'mode': 'GDD' if int(num(5)) == 0 else 'calendar',
            'tbase': num(7), 'tupper': num(8),
            'gdd_harvest': num(9),
            'zrmin': num(36), 'zrmax': num(37)}
