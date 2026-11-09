#!/usr/bin/env python3
"""Field-observation (.OBS) and CO2 files for group O and group D.

Ottawa.OBS carries biomass only -- its canopy-cover and soil-water columns are
all -9 -- so the evaluation paths for CC (typeObsSim_ObsSimCC) and SWC
(typeObsSim_ObsSimSWC) have never been exercised. Format follows Ottawa.OBS:
five header records, a blank, two column-title rows, a rule, then
`day  CC mean  CC std  B mean  B std  SWC mean  SWC std` with -9 for absent.
"""
from __future__ import annotations

import pathlib

OBS = pathlib.Path(__file__).resolve().parent / 'obs'
SIM = pathlib.Path(__file__).resolve().parent / 'simul'


def obs(desc, rows, depth=1.00, first=(1, 1, 2014)):
    L = [desc, '     7.3  : AquaCrop Version (January 2026)',
         f'     {depth:.2f}  : depth of sampled soil profile',
         f'     {first[0]}     : first day of observations',
         f'     {first[1]}     : first month of observations',
         f'  {first[2]}     : first year of observations (1901 if not linked to a specific year)',
         '',
         '   Day    Canopy cover (%)    dry Biomass (ton/ha)    Soil water content (mm)',
         '            Mean     Std         Mean       Std           Mean      Std',
         '=============================================================================']
    for d, cc, ccs, b, bs, w, ws in rows:
        L.append(f'   {d:4d}   {cc:7.1f} {ccs:7.1f}   {b:11.3f} {bs:7.1f}   {w:12.1f} {ws:7.1f}')
    return '\n'.join(L) + '\n'


N = -9.0
CC_ROWS = [(150, 12.0, 2.0, N, N, N, N), (170, 48.0, 5.0, N, N, N, N),
           (190, 88.0, 4.0, N, N, N, N), (220, 92.0, 3.0, N, N, N, N),
           (250, 61.0, 8.0, N, N, N, N)]
SWC_ROWS = [(150, N, N, N, N, 290.0, 12.0), (180, N, N, N, N, 245.0, 15.0),
            (210, N, N, N, N, 268.0, 11.0), (240, N, N, N, N, 302.0, 14.0)]
ALL_ROWS = [(150, 12.0, 2.0, 0.30, 0.05, 290.0, 12.0),
            (180, 62.0, 6.0, 3.90, 0.40, 245.0, 15.0),
            (210, 91.0, 4.0, 9.80, 0.90, 268.0, 11.0),
            (240, 74.0, 7.0, 14.20, 1.30, 302.0, 14.0)]

FILES = {
    'ObsCC':      obs('canopy-cover observations only', CC_ROWS),
    'ObsSWC':     obs('soil-water-content observations only', SWC_ROWS),
    'ObsAll':     obs('canopy cover, biomass and soil water together', ALL_ROWS),
    'ObsSingle':  obs('a single observation - degenerate statistics',
                      [(190, 88.0, 4.0, 9.80, 0.90, 268.0, 11.0)]),
    'ObsMissing': obs('every value absent',
                      [(150, N, N, N, N, N, N), (200, N, N, N, N, N, N)]),
    'ObsOutside': obs('observations outside the simulation period',
                      [(20, 10.0, 2.0, N, N, N, N), (350, 40.0, 5.0, N, N, N, N)]),
}


def main():
    OBS.mkdir(parents=True, exist_ok=True)
    for n, t in sorted(FILES.items()):
        (OBS / f'{n}.OBS').write_text(t)
    print(f'  obs/: {len(FILES)} files')
    # a flat CO2 record: two points at the same concentration, so any year in
    # between interpolates to exactly 369.41 -- the WP reference value
    SIM.mkdir(parents=True, exist_ok=True)
    (SIM / 'FlatCO2.CO2').write_text(
        'Constant atmospheric CO2 at the 369.41 ppm reference\n'
        'Year     CO2 (ppm)\n'
        '====================\n'
        '  1900    369.41\n'
        '  2100    369.41\n')
    (SIM / 'ShortCO2.CO2').write_text(
        'CO2 record covering 2000-2010 only - a 2014 run falls past its end\n'
        'Year     CO2 (ppm)\n'
        '====================\n'
        '  2000    369.52\n'
        '  2010    389.90\n')
    print('  simul/: FlatCO2.CO2, ShortCO2.CO2')


if __name__ == '__main__':
    main()
