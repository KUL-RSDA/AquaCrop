#!/usr/bin/env python3
"""Generate the .GWT, .SW0 and .OFF inputs for groups G, H and K.

Record layouts follow the loaders exactly; each reads a fixed number of records
and skips exactly three before the data block, so the blank/heading/rule lines
are structural, not decoration:

  LoadGroundWater        global.f90:6107   mode; dates only when variable
  LoadInitialConditions  global.f90:6612   CC/B/Zr only at version >= 4.1
  LoadOffSeason          global.f90:5895   pre/post ECw only at version >= 3.2
"""
from __future__ import annotations

import pathlib

ROOT = pathlib.Path(__file__).resolve().parent
V = '7.3'


# ---------------------------------------------------------------- groundwater
def gwt(desc, mode, obs=None, first=(1, 1, 1901)):
    """mode 0 none, 1 constant (one observation), 2 variable (several)."""
    L = [desc, f'     {V}   : AquaCrop Version',
         f'     {mode}     : ' + {0: 'no groundwater table',
                                  1: 'constant groundwater table',
                                  2: 'variable groundwater table'}[mode]]
    if mode == 2:
        L += [f'     {first[0]}     : first day of observations',
              f'     {first[1]}     : first month of observations',
              f'  {first[2]}     : first year of observations '
              f'(1901 if not linked to a specific year)']
    if mode > 0:
        L += ['', '   Day    Depth (m)    ECw (dS/m)',
              '====================================']
        for day, z, ec in (obs or []):
            L.append(f'   {day:4d}      {z:4.2f}          {ec:.1f}')
    return '\n'.join(L) + '\n'


GWT = {
    'GWT_none':        gwt('no groundwater table present', 0),
    'GWT_const_3p0':   gwt('constant water table at 3.0 m - below the profile', 1,
                           [(1, 3.00, 0.0)]),
    'GWT_const_2p0':   gwt('constant water table at 2.0 m', 1, [(1, 2.00, 0.0)]),
    'GWT_const_1p0':   gwt('constant water table at 1.0 m', 1, [(1, 1.00, 0.0)]),
    'GWT_const_0p4':   gwt('shallow constant water table at 0.4 m', 1, [(1, 0.40, 0.0)]),
    'GWT_const_saline': gwt('constant water table at 1.0 m, EC 5 dS/m', 1,
                            [(1, 1.00, 5.0)]),
    'GWT_var_2obs':    gwt('variable water table, two observations', 2,
                           [(1, 2.00, 0.0), (300, 1.00, 0.0)]),
    'GWT_var_rise_fall': gwt('variable water table rising then falling', 2,
                             [(1, 2.00, 0.0), (120, 0.80, 0.0),
                              (240, 1.20, 0.0), (360, 2.00, 0.0)]),
    'GWT_var_saline':  gwt('variable water table with varying salinity', 2,
                           [(1, 1.50, 1.0), (120, 0.90, 3.0), (300, 1.80, 1.0)]),
}


# --------------------------------------------------------- initial conditions
def sw0(desc, locs, at_depths=False, ccini=-9.0, bini=0.0, zrini=-9.0,
        surf=0.0, ecsurf=0.0):
    kind = 'depths' if at_depths else 'soil layers'
    L = [desc, f'    {V}   : AquaCrop Version',
         f'   {ccini:6.2f} : initial canopy cover',
         f'   {bini:6.3f} : biomass (ton/ha) produced before the simulation period',
         f'   {zrini:6.2f} : initial effective rooting depth (m)',
         f'   {surf:6.1f} : water layer (mm) stored between soil bunds',
         f'   {ecsurf:6.2f} : electrical conductivity (dS/m) of that water layer',
         f'    {1 if at_depths else 0}     : soil water content specified for {kind}',
         f'    {len(locs)}     : number of {kind} considered', '',
         '     Thickness (m)      Water content (vol%)     ECe (dS/m)',
         '==============================================================']
    for z, vol, ece in locs:
        L.append(f'         {z:4.2f}                {vol:5.2f}                  {ece:4.2f}')
    return '\n'.join(L) + '\n'


# Ottawa.SOL: SAT 46.0, FC 29.0, WP 13.0, 1.50 m
SW0 = {
    'SW0_atFC':      sw0('profile at field capacity', [(1.50, 29.00, 0.00)]),
    'SW0_atWP':      sw0('profile at wilting point', [(1.50, 13.00, 0.00)]),
    'SW0_atSAT':     sw0('profile at saturation', [(1.50, 46.00, 0.00)]),
    'SW0_dry_top':   sw0('dry top, wet subsoil - specified per layer',
                         [(0.30, 15.00, 0.00), (1.20, 29.00, 0.00)]),
    'SW0_at_depths': sw0('water content given at depths, not per layer',
                         [(0.10, 20.00, 0.00), (0.50, 26.00, 0.00),
                          (1.00, 29.00, 0.00), (1.50, 32.00, 0.00)],
                         at_depths=True),
    'SW0_ece_grad':  sw0('salinity increasing with depth',
                         [(0.30, 29.00, 1.00), (0.60, 29.00, 3.00),
                          (1.50, 29.00, 6.00)]),
    'SW0_surfstore': sw0('20 mm ponded between bunds at the start',
                         [(1.50, 29.00, 0.00)], surf=20.0),
    'SW0_surfstore_saline': sw0('20 mm of saline water ponded at the start',
                                [(1.50, 29.00, 0.00)], surf=20.0, ecsurf=4.0),
    # a ladder of uniform initial salinities for group L. Ottawa.SOL is at
    # FC 29.0 vol%; MaizeGDD tolerates ECe 2 (ECn) to 10 (ECx), so the ladder
    # spans below-threshold, mid-range, at-limit and beyond.
    'SW0_ece2':      sw0('uniformly saline profile, ECe 2 dS/m (at the crop threshold)',
                         [(1.50, 29.00, 2.00)]),
    'SW0_ece4':      sw0('uniformly saline profile, ECe 4 dS/m', [(1.50, 29.00, 4.00)]),
    'SW0_ece8':      sw0('uniformly saline profile, ECe 8 dS/m', [(1.50, 29.00, 8.00)]),
    'SW0_ece16':     sw0('uniformly saline profile, ECe 16 dS/m (beyond the crop limit)',
                         [(1.50, 29.00, 16.00)]),
    'SW0_ece_inverse': sw0('salinity decreasing with depth - a leached surface',
                           [(0.30, 29.00, 6.00), (0.60, 29.00, 3.00),
                            (1.50, 29.00, 1.00)]),
    'SW0_midseason': sw0('mid-season restart: canopy, biomass and roots carried in',
                         [(1.50, 29.00, 0.00)], ccini=35.0, bini=2.500, zrini=0.80),
}


# ---------------------------------------------------------------- off-season
def off(desc, before=0, after=0, effect=50, ev_before=(), ev_after=(),
        ecw_pre=0.0, ecw_post=0.0, fw=100):
    L = [desc, f'     {V}       : AquaCrop Version',
         f'    {before:2d}         : percentage (%) of ground surface covered by '
         f'mulches BEFORE growing period',
         f'    {after:2d}         : percentage (%) of ground surface covered by '
         f'mulches AFTER growing period',
         f'    {effect:2d}         : effect (%) of mulches on reduction of soil evaporation',
         f'     {len(ev_before)}         : number of irrigation events BEFORE growing period',
         f'     {ecw_pre:.1f}       : quality of irrigation water BEFORE growing period (dS/m)',
         f'     {len(ev_after)}         : number of irrigation events AFTER growing period',
         f'     {ecw_post:.1f}       : quality of irrigation water AFTER growing period (dS/m)',
         f'    {fw:2d}         : percentage (%) of soil surface wetted by off-season irrigation']
    if ev_before or ev_after:
        L += ['', '   Day    Depth(mm)    When', '   =================================']
        for d, mm in ev_before:
            L.append(f'   {d:4d}      {mm:3d}       before season')
        for d, mm in ev_after:
            L.append(f'   {d:4d}      {mm:3d}       after season')
    return '\n'.join(L) + '\n'


OFF = {
    'OFF_cover_before': off('50 % mulch cover before the season only', before=50),
    'OFF_cover_after':  off('60 % mulch cover after the season only', after=60),
    'OFF_cover_both':   off('full mulch cover on both sides', before=100, after=100),
    'OFF_effect_100':   off('50/50 cover with full evaporation suppression',
                            before=50, after=50, effect=100),
    'OFF_irr_before':   off('two irrigation events before the season',
                            ev_before=((5, 25), (15, 30))),
    'OFF_irr_after':    off('two irrigation events after the season',
                            ev_after=((5, 25), (20, 30))),
    'OFF_irr_both':     off('irrigation on both sides of the season',
                            ev_before=((5, 25),), ev_after=((10, 30),)),
    'OFF_saline_irr':   off('off-season irrigation with saline water',
                            ev_before=((5, 25),), ev_after=((10, 30),),
                            ecw_pre=3.0, ecw_post=3.0),
    'OFF_partial_wet':  off('off-season irrigation wetting 40 % of the surface',
                            ev_after=((10, 30),), fw=40),
}


def main():
    for sub, table, ext in (('gwt', GWT, 'GWT'), ('sw0', SW0, 'SW0'),
                            ('off', OFF, 'OFF')):
        d = ROOT / sub
        d.mkdir(parents=True, exist_ok=True)
        for name, text in sorted(table.items()):
            (d / f'{name}.{ext}').write_text(text)
        print(f'  {sub}/: {len(table)} files')


if __name__ == '__main__':
    main()
