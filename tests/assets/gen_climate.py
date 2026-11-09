#!/usr/bin/env python3
"""Aggregate the daily Ottawa records into 10-daily and monthly ones.

Purpose is to exercise `datatype_decadely` and `datatype_monthly` in
climprocessing.f90, which are otherwise untouched by the whole suite -- those
paths interpolate between periods (GetDecadeEToDataSet, GetMonthlyEToDataSet)
and are entirely separate code from the daily reader.

Aggregation: temperature and ETo are means over the period, rain is a total.
**Confirmed against the source**, not merely read off the column labels --
GetDecadeRainDataSet spreads the decade value flat (`Param = C/ni`), so it is a
total, while GetDecadeEToDataSet builds a piecewise-linear daily curve whose
mean over the decade returns the decade value, so that one is a mean. Both are
asserted by decade_check.py on cases D04 and D06.

Decades are days 1-10, 11-20, and 21 to the end of the month, matching
GetDecadeEToDataSet (climprocessing.f90:325).
"""
from __future__ import annotations

import calendar
import datetime
import pathlib

ROOT = pathlib.Path(__file__).resolve().parent / 'climate'


def read(path):
    L = path.read_text().splitlines()
    start = datetime.date(int(L[4].split()[0]), int(L[3].split()[0]),
                          int(L[2].split()[0]))
    head = L[:8]
    rows = [l.split() for l in L[8:] if l.strip()]
    return head, start, rows


def periods(start, n, monthly):
    """[(index range, label date)] over n daily records."""
    out, i = [], 0
    d = start
    while i < n:
        if monthly:
            last = calendar.monthrange(d.year, d.month)[1]
            take = min(last - d.day + 1, n - i)
        else:
            edge = 11 if d.day < 11 else (21 if d.day < 21 else
                                          calendar.monthrange(d.year, d.month)[1] + 1)
            take = min(edge - d.day, n - i)
        out.append((i, i + take, d))
        i += take
        d += datetime.timedelta(days=take)
    return out


def aggregate(src: pathlib.Path, dst: pathlib.Path, monthly: bool, how: str):
    head, start, rows = read(src)
    ncol = len(rows[0])
    out = []
    for a, b, _ in periods(start, len(rows), monthly):
        block = rows[a:b]
        vals = []
        for c in range(ncol):
            xs = [float(r[c]) for r in block]
            vals.append(sum(xs) if how == 'sum' else sum(xs) / len(xs))
        out.append('  '.join(f'{v:8.2f}' for v in vals))
    head = list(head)
    head[1] = f'     {3 if monthly else 2}  : ' + (
        'Monthly records (1=daily, 2=10-daily and 3=monthly data)' if monthly
        else '10-daily records (1=daily, 2=10-daily and 3=monthly data)')
    dst.write_text('\n'.join(head + out) + '\n')
    return len(out)


def main():
    jobs = [('Ottawa.Tnx', 'mean'), ('Ottawa.ETo', 'mean'), ('Ottawa.PLU', 'sum')]
    for name, how in jobs:
        src = ROOT / name
        stem, ext = name.split('.')
        for monthly, tag in ((False, 'Dec'), (True, 'Mon')):
            dst = ROOT / f'{stem}{tag}.{ext}'
            n = aggregate(src, dst, monthly, how)
            print(f'  {dst.name:<20} {n:4d} records ({how})')
    # a .CLI naming each aggregated set
    for tag in ('Dec', 'Mon'):
        (ROOT / f'Ottawa{tag}.CLI').write_text(
            f'Ottawa, Canada - {"monthly" if tag == "Mon" else "10-daily"} records\n'
            f' 7.3  : AquaCrop Version (January 2026)\n'
            f'Ottawa{tag}.Tnx\nOttawa{tag}.ETo\nOttawa{tag}.PLU\nMaunaLoa.CO2\n')
        print(f'  Ottawa{tag}.CLI')


if __name__ == '__main__':
    main()
