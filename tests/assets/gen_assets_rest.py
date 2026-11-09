#!/usr/bin/env python3
"""Assets for the last buildable plan rows.

Covers D23 (a one-record climate file), the five remaining cutting variants,
and a truncated .PPn.
"""
from __future__ import annotations

import pathlib

HERE = pathlib.Path(__file__).resolve().parent
CLIM, MAN, PARAM = HERE / 'climate', HERE / 'man', HERE / 'param'


def head(desc, kind, first_year=2014):
    label = {'Tnx': '  Tmin (C)   TMax (C)', 'ETo': '  Average ETo (mm/day)',
             'PLU': '  Total Rain (mm)'}[kind]
    rule = {'Tnx': '=' * 24, 'ETo': '=' * 23, 'PLU': '=' * 18}[kind]
    return [desc,
            '     1  : Daily records (1=daily, 2=10-daily and 3=monthly data)',
            '     1  : First day of record (1, 11 or 21 for 10-day or 1 for months)',
            '     1  : First month of record',
            f'  {first_year}  : First year of record (1901 if not linked to a specific year)',
            '', label, rule]


def write(path, lines):
    path.write_text('\n'.join(lines) + '\n')


def one_day():
    """D23: a record holding a single day."""
    for kind, val in (('Tnx', '10.0\t22.0'), ('ETo', '4.5'), ('PLU', '0.0')):
        write(CLIM / f'OneDay.{kind}',
              head('a climate record of one single day', kind) + [val])
    write(CLIM / 'OneDay.CLI',
          ['a climate record of one single day',
           ' 7.3  : AquaCrop Version (January 2026)',
           'OneDay.Tnx', 'OneDay.ETo', 'OneDay.PLU', 'MaunaLoa.CO2'])


#: the cutting records of a .MAN, by line number in the file
CUT = {13: 'Multiple cuttings are considered',
       14: 'Canopy cover (%) after cutting',
       15: 'Increase (%) of Canopy Growth Coefficient (CGC) after cutting',
       16: 'First day of window for multiple cuttings',
       17: 'Number of days in window for multiple cuttings',
       18: 'Multiple cuttings schedule is generated',
       19: 'Time criterion for generating cuttings',
       20: 'final harvest at crop maturity is not considered'}


def man(name, desc, records, block):
    """A .MAN built from MAN_cut_intday_full, with cutting records replaced."""
    lines = (MAN / 'MAN_cut_intday_full.MAN').read_text().splitlines()
    lines[0] = desc
    for ln, val in records.items():
        text = CUT[ln] if ln in CUT else lines[ln - 1].split(': ', 1)[1]
        if ln == 18 and val == 0:
            text = 'Multiple cuttings schedule is specified'
        elif ln == 19 and val == 0:
            text = 'Time criterion: Not Applicable'
        lines[ln - 1] = f'{val:>6}         : {text}'
    write(MAN / name, lines[:22] + block)


#: header + rule for each kind of cutting data block
GEN = [' From day   Criterion value', '=' * 29]
FIX = [' Harvest Day', '=' * 14]


def cuttings():
    # J31 -- a cut on the very first day of the cycle
    man('MAN_cut_day1.MAN', 'a specified cut on day 1 of the cycle',
        {18: 0, 19: 0}, FIX + ['     1'])
    # W17 -- a cut while the canopy is still expanding (closure is near day 60)
    man('MAN_cut_beforeclosure.MAN', 'a specified cut before canopy closure',
        {18: 0, 19: 0}, FIX + ['    20'])
    # J37 -- generated on dry biomass the season never reaches
    man('MAN_cut_never.MAN', 'a cutting criterion the season never meets',
        {19: 3}, GEN + ['     1     999.0'])
    # J39 -- the generation window clipped to 30 days
    man('MAN_cut_window30.MAN', 'a cutting window of thirty days',
        {17: 30}, GEN + ['     1     40'])
    # J43 -- regrowth vigour at the ends of its range (20 % is the default)
    man('MAN_cut_cgc0.MAN', 'no increase of CGC after cutting',
        {15: 0}, GEN + ['     1     40'])
    man('MAN_cut_cgc50.MAN', 'a 50 % increase of CGC after cutting',
        {15: 50}, GEN + ['     1     40'])


def late_premature_end():
    """B15: a premature-end date the run never reaches.

    Record 26 of a .CRO carries Crop_PrematureEnd (global.f90:5009), despite
    the stock comment calling it a dummy no longer required. When it is set and
    the run ends first, global.f90:4812 takes the else and the crop keeps its
    own last day -- the one sub-branch no other case reaches, because every
    case without the record hits the same else with the value undefined.

    Day 320 is 16 November; the standard season stops on 31 October.
    """
    rows = (HERE / 'crops' / 'MaizePremEnd.CRO').read_text().splitlines()
    rows[0] = 'Generic maize with a premature end on day-of-year 320'
    rows[25] = '       320      : dummy - Parameter no Longer required'
    write(HERE / 'crops' / 'MaizePremEndLate.CRO', rows)


def truncated_ppn():
    """N02: a .PPn holding only its first five records."""
    rows = (PARAM / 'Ottawa.PPn').read_text().splitlines()
    write(PARAM / 'Truncated.PPn', rows[:5])


def main():
    one_day()
    cuttings()
    truncated_ppn()
    late_premature_end()
    print('assets: OneDay, 6 cutting files, Truncated.PPn, MaizePremEndLate')


if __name__ == '__main__':
    main()
