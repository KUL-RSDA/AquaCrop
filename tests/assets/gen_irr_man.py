#!/usr/bin/env python3
"""Generate the .IRR and .MAN inputs for groups I and J.

Record layouts follow LoadIrriScheduleInfo (global.f90:2891) and LoadManagement
(global.f90:3389). Codes, from the source rather than from the comments in the
sample files:

  method  1 sprinkler  2 basin  3 border  4 furrow  else drip
  mode    0 rainfed    1 manual schedule  2 generate  else net requirement
  time    1 fixed interval  2 depletion (mm)  3 depletion (% RAW)  4 between bunds
  depth   1 back to FC      else fixed application

Version 7.2 is used for the generate/manual files so the record layout matches
the donated IrriGen.IRR and schedule_Tarn.IRR exactly. A generate file with
`lastday` is written as 7.3, which adds the IrriInfoLastDay record after the
depth criterion: no irrigation in the last `lastday` days of the crop cycle.
"""
from __future__ import annotations

import pathlib

ROOT = pathlib.Path(__file__).resolve().parent
MET = {1: 'Sprinkler irrigation', 2: 'Basin irrigation', 3: 'Border irrigation',
       4: 'Furrow irrigation', 5: 'Drip irrigation'}


def manual(desc, method=1, fw=100, events=((30, 20, 0.0),), firstday=-9):
    L = [desc, '   7.2   : AquaCrop Version',
         f'   {method}     : {MET[method]}',
         f' {fw:3d}     : Percentage of soil surface wetted by irrigation',
         '   1     : Irrigation schedule',
         f'      {firstday} : Day 1 is first day of growing period',
         '   Day    Depth (mm)   ECw (dS/m)',
         '====================================']
    for d, mm, ec in events:
        L.append(f'   {d:5d}     {mm:5d}       {ec:5.1f}')
    return '\n'.join(L) + '\n'


def generate(desc, method=1, fw=100, time=3, depth=1, periods=((1, 50, 0, 0.0),),
             lastday=None):
    version = '7.2' if lastday is None else '7.3'
    L = [desc, f'   {version}   : AquaCrop Version',
         f'   {method}     : {MET[method]}',
         f' {fw:3d}     : Percentage of soil surface wetted by irrigation',
         '   2     : Generate irrigation schedule',
         f'   {time}     : Time criterion',
         f'   {depth}     : Depth criterion']
    if lastday is not None:
        L.append(f' {lastday:3d}     : Last irrigation event {lastday} days '
                 'before the end of the crop cycle')
    L += ['',
         '  From day    Depleted RAW (%)   Back to FC (+/- mm)       ECw (dS/m)',
         '=========================================================================']
    for d, a, b, ec in periods:
        L.append(f'      {d:5d}          {a:5d}              {b:6d}              {ec:5.1f}')
    return '\n'.join(L) + '\n'


def inet(desc, perc_raw=50):
    return '\n'.join([
        desc, '   7.2   : AquaCrop Version',
        '   1     : irrigation method - not considered',
        ' 100     : soil surface wetted by irrigation - not considered',
        '   3     : Determination of Net Irrigation requirement',
        f' {perc_raw:3d}     : Allowable depletion of RAW (%)']) + '\n'


SCHED = tuple((d, 25, 0.0) for d in range(30, 121, 15))          # 7 events, 25 mm

IRR = {f'IRR_method_{m}': manual(f'manual schedule, {MET[m].lower()}', method=m)
       for m in MET}
IRR.update({
    'IRR_manual_saline':   manual('manual schedule with saline water, ECw 4 dS/m',
                                  events=tuple((d, 25, 4.0) for d in range(30, 121, 15))),
    'IRR_manual_dense':    manual('manual schedule, 7 events of 25 mm', events=SCHED),
    'IRR_manual_single':   manual('a single irrigation event', events=((60, 40, 0.0),)),
    'IRR_fw30':            manual('manual schedule wetting 30 % of the surface', fw=30),
    'IRR_fw50':            manual('manual schedule wetting 50 % of the surface', fw=50),
    'IRR_gen_fixint':      generate('generate on a fixed 7-day interval', time=1,
                                    periods=((1, 7, 0, 0.0),)),
    'IRR_gen_alldepl':     generate('generate at 40 mm depletion', time=2,
                                    periods=((1, 40, 0, 0.0),)),
    'IRR_gen_allraw':      generate('generate at 50 % RAW depletion', time=3,
                                    periods=((1, 50, 0, 0.0),)),
    'IRR_gen_allraw_100':  generate('generate at 100 % RAW depletion', time=3,
                                    periods=((1, 100, 0, 0.0),)),
    'IRR_gen_fixdepth':    generate('generate at 50 % RAW, fixed 30 mm application',
                                    time=3, depth=0, periods=((1, 50, 30, 0.0),)),
    'IRR_gen_multiperiod': generate('generate with the threshold changing mid-season',
                                    time=3, periods=((1, 20, 0, 0.0), (45, 60, 0, 0.0),
                                                     (90, 40, 0, 0.0))),
    'IRR_gen_saline':      generate('generated schedule with saline water',
                                    time=3, periods=((1, 50, 0, 4.0),)),
    'IRR_gen_lastday10':   generate('generate 6 mm every day, none in the last 10 days',
                                    time=1, depth=0, periods=((1, 1, 6, 0.0),),
                                    lastday=10),
    'IRR_inet_50':         inet('net irrigation requirement, 50 % RAW', 50),
    'IRR_inet_100':        inet('net irrigation requirement, 100 % RAW', 100),
})


# ------------------------------------------------------------------ .MAN
def man(desc, mulch=0, effect=50, fert=0, bund=0.0, runoff_affected=0, cn=0,
        weed_rc=0, weed_drc=0, weed_shape=-0.01, weed_adj=100, cuts=None):
    """cuts: None for no cuttings, else a list of harvest day numbers."""
    L = [desc, '     7.3       : AquaCrop Version (January 2026)',
         f'    {mulch:2d}         : percentage (%) of ground surface covered by mulches IN growing period',
         f'    {effect:2d}         : effect (%) of mulches on reduction of soil evaporation',
         f'    {fert:2d}         : Degree of soil fertility stress (%) - Effect is crop specific',
         f'     {bund:.2f}      : height (m) of soil bunds',
         f'     {runoff_affected}         : surface runoff '
         + ('affected by field surface practices' if runoff_affected else
            'NOT affected by field surface practices'),
         f'    {cn:2d}         : effect on CN of field surface practices',
         f'    {weed_rc:2d}         : relative cover of weeds at canopy closure (%)',
         f'    {weed_drc:2d}         : increase of relative cover of weeds in mid-season (+%)',
         f'   {weed_shape:6.2f}      : shape factor of the CC expansion function in a weed infested field',
         f'   {weed_adj:3d}         : replacement (%) by weeds of the self-thinned part of the CC']
    if cuts is None:
        L += ['     0         : Multiple cuttings are not considered',
              '    30         : Canopy cover (%) after cutting - not considered',
              '    -9         : parameter no longer considered',
              '     1         : First day of window for multiple cuttings',
              '    -9         : Number of days in window for multiple cuttings',
              '    -9         : Timing of multiple cuttings: Not Applicable',
              '     0         : Time criterion: Not Applicable',
              '     0         : final harvest at crop maturity is not considered',
              '    -9         : Start of the growing cycle is Day 1 in list of cuttings']
    else:
        L += ['     1         : Multiple cuttings are considered',
              '    25         : Canopy cover (%) after cutting',
              '    20         : Increase (%) of Canopy Growth Coefficient (CGC) after cutting',
              '     1         : First day of window for multiple cuttings',
              '    -9         : Number of days in window for multiple cuttings',
              '     0         : Multiple cuttings schedule is specified',
              '     0         : Time criterion: Not Applicable',
              '     0         : final harvest at crop maturity is not considered',
              ' 41414         : dayNr for Day 1 of list of cuttings', '',
              ' Harvest Day', '==============']
        L += [f'   {d}' for d in cuts]
    return '\n'.join(L) + '\n'


MAN = {
    'MAN_plain':        man('no specific field management'),
    'MAN_mulch50':      man('50 % mulch cover', mulch=50),
    'MAN_mulch100':     man('full mulch cover', mulch=100),
    'MAN_mulch_full_effect': man('50 % cover with full evaporation suppression',
                                 mulch=50, effect=100),
    'MAN_fert25':       man('soil fertility stress 25 %', fert=25),
    'MAN_fert50':       man('soil fertility stress 50 %', fert=50),
    'MAN_fert75':       man('soil fertility stress 75 %', fert=75),
    'MAN_fert100':      man('soil fertility stress 100 %', fert=100),
    'MAN_bund10':       man('0.10 m soil bunds', bund=0.10),
    'MAN_bund30':       man('0.30 m soil bunds', bund=0.30),
    'MAN_runoff_off':   man('surface runoff prevented by field practices',
                            runoff_affected=1, cn=-10),
    'MAN_cn_plus10':    man('field practices raising CN by 10',
                            runoff_affected=1, cn=10),
    'MAN_weeds25':      man('weeds covering 25 % at canopy closure',
                            weed_rc=25, weed_shape=100.0),
    'MAN_weeds75':      man('heavy weed infestation, 75 % at closure',
                            weed_rc=75, weed_shape=100.0),
    'MAN_weeds_delta':  man('weeds increasing by 50 % in mid-season',
                            weed_rc=25, weed_drc=50, weed_shape=100.0),
    'MAN_mulch_fert':   man('50 % mulch together with 50 % fertility stress',
                            mulch=50, fert=50),
    'MAN_mulch_no_effect': man('full mulch cover with no effect on evaporation',
                               mulch=100, effect=0),
    'MAN_fert0':        man('soil fertility stress explicitly zero', fert=0),
    'MAN_runoff_off_nobunds': man('runoff prevented with no bunds present',
                                  bund=0.0, runoff_affected=1, cn=0),
    'MAN_cn_minus10':   man('field practices lowering CN by 10',
                            runoff_affected=1, cn=-10),
    'MAN_weeds_negdelta': man('weed cover decreasing through mid-season',
                              weed_rc=50, weed_drc=-40, weed_shape=100.0),
    'MAN_weedadj0':     man('weeds not replacing self-thinned perennial canopy',
                            weed_rc=25, weed_shape=100.0, weed_adj=0),
    'MAN_cuts_list':    man('forage with a fixed list of cutting days',
                            fert=50, cuts=(194, 243)),
}


# ---- the SW09 surface-management grid ---------------------------------------
# weeds x mulch x bunds, generated so the sweep has one file per cell
for _w in (0, 25, 75):
    for _m in (0, 50, 100):
        for _b in (0, 10):
            MAN[f'MAN_sweep_w{_w}_m{_m}_b{_b}'] = man(
                f'weeds {_w} %, mulch {_m} %, bunds 0.{_b:02d} m',
                mulch=_m, weed_rc=_w, bund=_b / 100.0,
                weed_shape=100.0 if _w else -0.01)

# ---- cutting schedules for sweep SW10 ---------------------------------------
# TimeCuttings: 1 interval in days, 2 interval in GDD, 3 dry biomass,
# 4 dry yield, 5 fresh yield (global.f90:88). Generated cuttings need
# Cuttings_Generate true and a criterion, rather than a day list.
def man_cuts_generated(desc, criterion, value, day1=1, nrdays=-9, fert=50,
                       first_daynr=41414):
    """A .MAN declaring a GENERATED cutting schedule.

    The record order matters and is not obvious. LoadManagement
    (global.f90:3467) reads nine cuttings records -- considered, CC after cut,
    CGC increase, window first day, window length, generate, time criterion,
    final harvest, dayNr of list day 1 -- and there is **no** record holding the
    criterion threshold. The threshold lives in the data rows at the end, which
    OpenHarvestInfo (run.f90:5955) reaches by skipping 2 + 10 + 12 records: the
    description and version, ten management records, then the nine cuttings
    records plus the blank/title/rule lines that follow them.

    Each data row is `FromDay  value`, the value being interval days, interval
    GDD, or a mass in ton/ha depending on the criterion.
    """
    fmt = '%d' if criterion in (1, 2) else '%.1f'
    L = [desc, '     7.3       : AquaCrop Version (January 2026)',
         '     0         : percentage (%) of ground surface covered by mulches IN growing period',
         '    50         : effect (%) of mulches on reduction of soil evaporation',
         f'    {fert:2d}         : Degree of soil fertility stress (%) - Effect is crop specific',
         '     0.00      : height (m) of soil bunds',
         '     0         : surface runoff NOT affected by field surface practices',
         '     0         : effect on CN of field surface practices',
         '     0         : relative cover of weeds at canopy closure (%)',
         '     0         : increase of relative cover of weeds in mid-season (+%)',
         '    -0.01      : shape factor of the CC expansion function in a weed infested field',
         '   100         : replacement (%) by weeds of the self-thinned part of the CC',
         '     1         : Multiple cuttings are considered',
         '    25         : Canopy cover (%) after cutting',
         '    20         : Increase (%) of Canopy Growth Coefficient (CGC) after cutting',
         f'    {day1:2d}         : First day of window for multiple cuttings',
         f'    {nrdays:2d}         : Number of days in window for multiple cuttings',
         '     1         : Multiple cuttings schedule is generated',
         f'     {criterion}         : Time criterion for generating cuttings',
         '     0         : final harvest at crop maturity is not considered',
         f' {first_daynr}         : dayNr for Day 1 of list of cuttings',
         '',
         ' From day   Criterion value',
         '=============================',
         '     1     ' + (fmt % value)]
    return '\n'.join(L) + '\n'


for _crit, _name, _val in ((1, 'intday', 40), (2, 'intgdd', 500),
                           (3, 'dryb', 4.0), (4, 'dryy', 3.0), (5, 'freshy', 15.0)):
    for _w, _d1, _nd in (('full', 1, -9), ('late', 60, -9)):
        MAN[f'MAN_cut_{_name}_{_w}'] = man_cuts_generated(
            f'generated cuttings on criterion {_crit} ({_name}), {_w} window',
            _crit, _val, day1=_d1, nrdays=_nd)


MAN['MAN_cut_harvestend'] = man_cuts_generated(
    'generated cuttings with a final harvest at maturity', 1, 40).replace(
    '     0         : final harvest at crop maturity is not considered',
    '     1         : final harvest at crop maturity is considered')
MAN['MAN_cut_cc10'] = man_cuts_generated(
    'generated cuttings leaving only 10 % canopy', 1, 40).replace(
    '    25         : Canopy cover (%) after cutting',
    '    10         : Canopy cover (%) after cutting')


def main():
    for sub, table, ext in (('irr', IRR, 'IRR'), ('man', MAN, 'MAN')):
        d = ROOT / sub
        d.mkdir(parents=True, exist_ok=True)
        for name, text in sorted(table.items()):
            (d / f'{name}.{ext}').write_text(text)
        print(f'  {sub}/: {len(table)} generated')


if __name__ == '__main__':
    main()
