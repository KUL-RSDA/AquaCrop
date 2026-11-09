#!/usr/bin/env python3
"""Synthetic climate records for conditions the Ottawa data never reaches.

Groups P (stress paths) and R (runoff/infiltration) need extremes the real
record does not contain: a 100 mm day, a rainless season, air above 40 degC,
air below a crop's base temperature. Each file keeps Ottawa's header geometry
(daily, from 1 January 2014, 1096 records) so it drops in without touching the
project dates.
"""
from __future__ import annotations

import datetime
import pathlib

OUT = pathlib.Path(__file__).resolve().parent / 'climate'
N = 1096                                   # 2014-01-01 .. 2016-12-31
START = datetime.date(2014, 1, 1)


def head(desc, kind):
    label = {'Tnx': '  Tmin (C)   TMax (C)', 'ETo': '  Average ETo (mm/day)',
             'PLU': '  Total Rain (mm)'}[kind]
    return [desc, '     1  : Daily records (1=daily, 2=10-daily and 3=monthly data)',
            '     1  : First day of record (1, 11 or 21 for 10-day or 1 for months)',
            '     1  : First month of record',
            '  2014  : First year of record (1901 if not linked to a specific year)',
            '', label, '========================']


def write(name, kind, desc, values):
    body = [f'{v:8.2f}' if kind != 'Tnx' else f'{v[0]:8.2f}{v[1]:10.2f}'
            for v in values]
    (OUT / f'{name}.{kind}').write_text('\n'.join(head(desc, kind) + body) + '\n')


def read_ottawa(kind):
    L = (OUT / f'Ottawa.{kind}').read_text().splitlines()[8:]
    rows = [x.split() for x in L if x.strip()]
    return ([(float(a), float(b)) for a, b in rows] if kind == 'Tnx'
            else [float(r[0]) for r in rows])


def main():
    tnx, eto, plu = read_ottawa('Tnx'), read_ottawa('ETo'), read_ottawa('PLU')

    # --- rainfall -------------------------------------------------------
    write('Dry', 'PLU', 'no rain at all - forces terminal water stress',
          [0.0] * N)
    write('Sparse', 'PLU', 'a tenth of the Ottawa rainfall',
          [r / 10.0 for r in plu])
    # one 100 mm day per month, nothing else: drives runoff clamps and a
    # full-profile wetting front
    storm = [0.0] * N
    for i, d in enumerate(START + datetime.timedelta(days=k) for k in range(N)):
        if d.day == 15:
            storm[i] = 100.0
    write('Storm', 'PLU', 'a single 100 mm event on the 15th of each month', storm)
    write('Wet', 'PLU', 'three times the Ottawa rainfall, every day',
          [r * 3.0 for r in plu])

    # --- temperature -----------------------------------------------------
    write('Hot', 'Tnx', 'Ottawa shifted +15 degC - pollination heat stress',
          [(a + 15.0, b + 15.0) for a, b in tnx])
    write('Cold', 'Tnx', 'Ottawa shifted -8 degC - cold stress and stalled GDD',
          [(a - 8.0, b - 8.0) for a, b in tnx])

    # --- reference ET ----------------------------------------------------
    write('Eto5', 'ETo', 'a constant 5.0 mm/day - the stage-II reference value',
          [5.0] * N)
    write('EtoHigh', 'ETo', 'a constant 12.0 mm/day - high evaporative demand',
          [12.0] * N)

    # a .CLI for each combination used by a case
    combos = {
        'Dry':     ('Ottawa.Tnx', 'Ottawa.ETo', 'Dry.PLU'),
        'Sparse':  ('Ottawa.Tnx', 'Ottawa.ETo', 'Sparse.PLU'),
        'Storm':   ('Ottawa.Tnx', 'Ottawa.ETo', 'Storm.PLU'),
        'Wet':     ('Ottawa.Tnx', 'Ottawa.ETo', 'Wet.PLU'),
        'Hot':     ('Hot.Tnx', 'Ottawa.ETo', 'Ottawa.PLU'),
        'Cold':    ('Cold.Tnx', 'Ottawa.ETo', 'Ottawa.PLU'),
        'Eto5':    ('Ottawa.Tnx', 'Eto5.ETo', 'Ottawa.PLU'),
        'EtoHigh': ('Ottawa.Tnx', 'EtoHigh.ETo', 'Ottawa.PLU'),
        'HotDry':  ('Hot.Tnx', 'EtoHigh.ETo', 'Dry.PLU'),
        'DryEto5': ('Ottawa.Tnx', 'Eto5.ETo', 'Dry.PLU'),
        'DryConst': ('OttawaConst.Tnx', 'Eto5.ETo', 'Dry.PLU'),
    }
    for name, (t, e, p) in combos.items():
        (OUT / f'{name}.CLI').write_text(
            f'synthetic climate: {name}\n'
            f' 7.3  : AquaCrop Version (January 2026)\n{t}\n{e}\n{p}\nMaunaLoa.CO2\n')
    print(f'  wrote {len(combos)} .CLI and 8 record files into {OUT.name}/')


if __name__ == '__main__':
    main()
