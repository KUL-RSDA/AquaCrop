#!/usr/bin/env python3
"""An independent prediction of AquaCrop's 10-daily climate interpolation.

Hand-ported from GetDecadeEToDataSet and its inner GetParameters/GetSetofThree
(climprocessing.f90:325). A 10-daily record is not simply repeated across the
decade: each decade is turned into a piecewise-linear daily curve through three
control points derived from the previous, current and next decade values, so the
daily series is smooth across decade boundaries and its mean over the decade
returns the decade value.

    UL  = (C1+C2)/2        the value at the start of the decade
    LL  = (C2+C3)/2        the value at its end
    Mid = 2*C2 - (UL+LL)/2 the mid-decade value that preserves the mean

At the first and last decade of the record the missing neighbour is
extrapolated as C2 + (C2 - other)/4.
"""
from __future__ import annotations

import calendar
import datetime
import pathlib

EPOCH = datetime.date(1900, 12, 31)


def read_decadal(path):
    """(first date, [values]) from a 10-daily climate file."""
    L = pathlib.Path(path).read_text().splitlines()
    d, m, y = (int(L[i].split()[0]) for i in (2, 3, 4))
    vals = [float(x.split()[0]) for x in L[8:] if x.strip()]
    return datetime.date(y, m, d), vals


def decade_of(day: datetime.date):
    """(decade number 1-3, first day of that decade, days in it)."""
    if day.day > 20:
        last = calendar.monthrange(day.year, day.month)[1]
        return 3, 21, last - 21 + 1
    if day.day > 10:
        return 2, 11, 10
    return 1, 1, 10


def decade_index(first: datetime.date, day: datetime.date) -> int:
    """Position of `day`'s decade in a record starting at `first`."""
    months = (day.year - first.year) * 12 + (day.month - first.month)
    d0, _, _ = decade_of(first)
    d1, _, _ = decade_of(day)
    return months * 3 + (d1 - d0)


def set_of_three(vals, idx):
    """C1, C2, C3 with the record's end neighbours extrapolated."""
    c2 = vals[idx]
    if idx == 0:
        c3 = vals[1]
        c1 = c2 + (c2 - c3) / 4.0
    elif idx == len(vals) - 1:
        c1 = vals[idx - 1]
        c3 = c2 + (c2 - c1) / 4.0
    else:
        c1, c3 = vals[idx - 1], vals[idx + 1]
    return c1, c2, c3


def parameters(c1, c2, c3):
    ul = (c1 + c2) / 2.0
    ll = (c2 + c3) / 2.0
    mid = 2.0 * c2 - (ul + ll) / 2.0
    return ul, ll, mid


def daily(path, day: datetime.date) -> float:
    """The interpolated daily value for `day`."""
    first, vals = read_decadal(path)
    idx = decade_index(first, day)
    if not 0 <= idx < len(vals):
        raise ValueError(f'{day} outside the record')
    c1, c2, c3 = set_of_three(vals, idx)
    _, first_day, ni = decade_of(day)
    nri = day.day - first_day + 1
    if abs(c2) < 1e-300:
        return 0.0
    ul, ll, mid = parameters(c1, c2, c3)
    if nri <= ni / 2.0 + 0.01:
        v = (2.0 * ul + (mid - ul) * (2.0 * nri - 1.0) / (ni / 2.0)) / 2.0
    elif ni in (11, 9) and nri < (ni + 1.01) / 2:
        v = mid
    else:
        v = (2.0 * mid + (ll - mid) * (2.0 * nri - (ni + 1.0)) / (ni / 2.0)) / 2.0
    return max(v, 0.0)


def daily_rain(path, day: datetime.date) -> float:
    """Decadal rain is NOT interpolated: GetDecadeRainDataSet spreads the
    decade total evenly over its days (`Param = C/ni`, climprocessing.f90:515).
    """
    first, vals = read_decadal(path)
    idx = decade_index(first, day)
    if not 0 <= idx < len(vals):
        raise ValueError(f'{day} outside the record')
    _, _, ni = decade_of(day)
    return vals[idx] / ni


def series(path, start, end):
    if isinstance(start, str):
        start = datetime.date.fromisoformat(start)
    if isinstance(end, str):
        end = datetime.date.fromisoformat(end)
    out, d = [], start
    while d <= end:
        out.append((d, daily(path, d)))
        d += datetime.timedelta(days=1)
    return out
