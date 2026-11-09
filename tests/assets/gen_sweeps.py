#!/usr/bin/env python3
"""Generate the Part IV sweep families: factorial grids over axes that interact.

Sweeps are deliberately cheap. Every case takes **season output only** (~3 KB of
reference), because a sweep's value is in the combination, not in day-resolution
detail -- the hand-designed groups already cover that. The whole of Part IV
costs under a megabyte.

Combinations known to hang are excluded: SW07 omits 10-daily temperature
records entirely (defect D6), which removes 9 of its 27 cells.

Every case still gets the full invariant suite, which is the point -- 250 extra
reference-diffs on their own would add little, but 250 extra runs each checked
for mass balance, bounds and NaN is genuine coverage.
"""
from __future__ import annotations

import itertools
import json
import pathlib

ROOT = pathlib.Path(__file__).resolve().parent.parent
CASES = ROOT / 'cases'
CORE = ['Ottawa.CLI', 'MaunaLoa.CO2', 'Ottawa.PPn', 'DEFAULT.CRO', 'DEFAULT.SOL']
MAY = ('2014-05-21', '2014-10-31')

TEX = ['sand', 'loamy_sand', 'sandy_loam', 'loam', 'silt_loam', 'silt',
       'sandy_clay_loam', 'clay_loam', 'silty_clay_loam', 'sandy_clay',
       'silty_clay', 'clay']
PPN_LABELS = None   # filled from gen_cases_NOE to keep the labels in one place


def crop_patch(**lines):
    return lines


CASES_SW: list[dict] = []


def add(fam, cid, desc, *, crop='MaizeGDD.CRO', soil='Ottawa.SOL', tnx='Ottawa.Tnx',
        eto='Ottawa.ETo', plu='Ottawa.PLU', slots=None, season=MAY,
        cropdates=None, patch=None, tier='T2'):
    CASES_SW.append(dict(fam=fam, cid=cid, desc=desc, crop=crop, soil=soil,
                         tnx=tnx, eto=eto, plu=plu, slots=slots or {},
                         season=season, cropdates=cropdates,
                         patch=patch or {}, tier=tier))


ZR_LINE, MODE_LINE, GDD_LINE = 38, 6, 10
ZR_LABEL = 'Maximum effective rooting depth (m)'

# ---- SW01  texture x rooting depth ------------------------------------------
for t, zr in itertools.product(TEX, (0.30, 0.80, 1.50, 3.00)):
    add('SW01', f"SW01_{t}_zr{str(zr).replace('.','p')}",
        f'{t.replace("_"," ")} with Zrmax {zr:.2f} m',
        soil=f'TEX_{t}.SOL',
        patch={'MaizeGDD.CRO': {ZR_LINE: f'     {zr:.2f}      : {ZR_LABEL}'}})

# ---- SW02  compartment geometry ---------------------------------------------
for depth, zr in itertools.product(('0p30', '0p55', '1p20', '1p25', '3p00', '4p00'),
                                   (0.30, 0.60, 1.20, 1.30, 3.00)):
    add('SW02', f'SW02_soil{depth}_zr{str(zr).replace(".","p")}',
        f'soil {depth.replace("p",".")} m with Zrmax {zr:.2f} m',
        soil=f'GEOM_{depth}m.SOL', tier='T1',
        patch={'MaizeGDD.CRO': {ZR_LINE: f'     {zr:.2f}      : {ZR_LABEL}'}})

# ---- SW03  GDD formulation ---------------------------------------------------
GDD_M = 'Default method for the calculation of growing degree days'
for m, onset in itertools.product((1, 2, 3), ('04-15', '05-21', '06-15', '07-01')):
    add('SW03', f'SW03_gdd_m{m}_{onset.replace("-","")}',
        f'GDD method {m}, sown 2014-{onset}', crop='MaizeGDDwpy.CRO', tier='T1',
        season=(f'2014-{onset}', '2014-10-31'),
        patch={'Ottawa.PPn': {21: f'      {m}         : {GDD_M}'}})
for onset in ('04-15', '05-21', '06-15', '07-01'):
    add('SW03', f'SW03_cal_{onset.replace("-","")}',
        f'calendar-day cycle, sown 2014-{onset}', crop='MaizeCalwpy.CRO',
        tier='T1', season=(f'2014-{onset}', '2014-10-31'))

# ---- SW04  irrigation regime -------------------------------------------------
for f in ('IRR_method_1', 'IRR_method_2', 'IRR_method_3', 'IRR_method_4',
          'IRR_method_5'):
    add('SW04', f'SW04_manual_{f[-1]}', f'manual schedule, method {f[-1]}',
        slots={'irr': f'{f}.IRR'})
for f, lbl in (('IRR_gen_fixint', 'fixed interval'), ('IRR_gen_alldepl', 'depletion mm'),
               ('IRR_gen_allraw', 'depletion % RAW'), ('IRR_gen_fixdepth', 'fixed depth')):
    add('SW04', f'SW04_gen_{f.split("_")[-1]}', f'generated schedule, {lbl}',
        slots={'irr': f'{f}.IRR'})
for f in ('IRR_inet_50', 'IRR_inet_100', 'Inet'):
    add('SW04', f'SW04_{f.lower()}', f'net irrigation requirement, {f}',
        slots={'irr': f'{f}.IRR'})

# ---- SW05  fertility x water -------------------------------------------------
for fert, water in itertools.product(('MAN_plain', 'MAN_fert25', 'MAN_fert50',
                                      'MAN_fert75', 'MAN_fert100'),
                                     (None, 'IRR_gen_allraw', 'IRR_manual_dense',
                                      'IRR_gen_fixint')):
    slots = {'man': f'{fert}.MAN'}
    if water:
        slots['irr'] = f'{water}.IRR'
    add('SW05', f'SW05_{fert.replace("MAN_","")}_{water or "rainfed"}',
        f'{fert.replace("MAN_","")} with {water or "rainfed"}',
        crop='Maize_EUirr_GDD.CRO', slots=slots, tier='T1')

# ---- SW06  salinity ----------------------------------------------------------
for ece, irr in itertools.product(('SW0_atFC', 'SW0_ece2', 'SW0_ece4', 'SW0_ece8'),
                                  (None, 'IRR_gen_allraw', 'IRR_gen_saline')):
    slots = {'sw0': f'{ece}.SW0'}
    if irr:
        slots['irr'] = f'{irr}.IRR'
    add('SW06', f'SW06_{ece.replace("SW0_","")}_{irr or "rainfed"}',
        f'{ece.replace("SW0_","")} with {irr or "rainfed"}', slots=slots)

# ---- SW07  climate record types (no 10-daily temperature: defect D6) --------
for tn, et, pl in itertools.product(('Ottawa', 'OttawaMon'),
                                    ('Ottawa', 'OttawaDec', 'OttawaMon'),
                                    ('Ottawa', 'OttawaDec', 'OttawaMon')):
    add('SW07', f'SW07_{tn[6:] or "day"}_{et[6:] or "day"}_{pl[6:] or "day"}',
        f'temperature {tn[6:] or "daily"}, ETo {et[6:] or "daily"}, '
        f'rain {pl[6:] or "daily"}',
        tnx=f'{tn}.Tnx', eto=f'{et}.ETo', plu=f'{pl}.PLU')

# ---- SW08  groundwater x texture --------------------------------------------
for g, t in itertools.product(('GWT_none', 'GWT_const_3p0', 'GWT_const_2p0',
                               'GWT_const_1p0', 'GWT_const_0p4', 'GWT_var_2obs'),
                              ('sand', 'loam', 'silt_loam', 'clay')):
    add('SW08', f'SW08_{g.replace("GWT_","")}_{t}',
        f'{g.replace("GWT_"," ").strip()} on {t.replace("_"," ")}',
        soil=f'TEX_{t}.SOL', slots={'gwt': f'{g}.GWT'})

# ---- SW09  surface management ------------------------------------------------
for w, mu, bd in itertools.product(('0', '25', '75'), ('0', '50', '100'), ('0', '10')):
    key = {('0','0','0'): 'MAN_plain'}.get((w,mu,bd))
    name = key or f'MAN_sweep_w{w}_m{mu}_b{bd}'
    add('SW09', f'SW09_w{w}_m{mu}_b{bd}',
        f'weeds {w} %, mulch {mu} %, bunds 0.{bd.zfill(2)} m',
        slots={'man': f'{name}.MAN'})

# ---- SW10  cutting schedules (forage only) ----------------------------------
for name in ('intday', 'intgdd', 'dryb', 'dryy', 'freshy'):
    for win in ('full', 'late'):
        add('SW10', f'SW10_{name}_{win}',
            f'generated cuttings on {name}, {win} window',
            crop='AlfOttawaGDD.CRO', slots={'man': f'MAN_cut_{name}_{win}.MAN'},
            tier='T1')

# ---- SW11  one .PPn parameter at a time --------------------------------------
PPN_SWEEP = {
    1: ('2', 'Evaporation decline factor for stage II'),
    2: ('1.05', 'Ke(x) Soil evaporation coefficient for fully wet and non-shaded soil surface'),
    3: ('10', 'Threshold for green CC below which HI can no longer increase (% cover)'),
    4: ('60', 'Starting depth of root zone expansion curve (% of Zmin)'),
    5: ('3.00', 'Maximum allowable root zone expansion (fixed at 5 cm/day)'),
    6: ('-3', 'Shape factor for effect water stress on root zone expansion'),
    7: ('30', 'Required soil water content in top soil for germination (% TAW)'),
    8: ('1.5', 'Adjustment factor for FAO-adjustment soil water depletion (p) by ETo'),
    9: ('5', 'Number of days after which deficient aeration is fully effective'),
    10: ('1.50', 'Exponent of senescence factor adjusting drop in photosynthetic activity of dying crop'),
    11: ('20', 'Decrease of p(sen) once early canopy senescence is triggered (% of p(sen))'),
    12: ('15', 'Thickness top soil (cm) in which soil water depletion has to be determined'),
    13: ('25', 'Depth [cm] of soil profile affected by water extraction by soil evaporation'),
    14: ('0.20', 'Considered depth (m) of soil profile for calculation of mean soil water content for CN adjustment'),
    15: ('0', 'CN is adjusted to Antecedent Moisture Class'),
    16: ('40', 'Salt diffusion factor (capacity for salt diffusion in micro pores) [%]'),
    17: ('80', 'Salt solubility [g/liter]'),
    18: ('12', 'Shape factor for effect of soil water content gradient on capillary rise'),
    21: ('2', 'Default method for the calculation of growing degree days'),
    22: ('0', 'Daily rainfall is estimated by USDA-SCS procedure (when input is 10-day/monthly rainfall)'),
    23: ('60', 'Percentage of effective rainfall (when input is 10-day/monthly rainfall)'),
    24: ('3', 'Number of showers in a decade for run-off estimate (when input is 10-day/monthly rainfall)'),
    25: ('4', 'Parameter for reduction of soil evaporation (when input is 10-day/monthly rainfall)'),
}
for line, (val, label) in sorted(PPN_SWEEP.items()):
    add('SW11', f'SW11_ppn{line:02d}', f'.PPn record {line} set to {val}',
        patch={'Ottawa.PPn': {line: f'{val:>6}      : {label}'}})

# ---- SW12  crop x sowing date ------------------------------------------------
for crop, onset in itertools.product(
        ('MaizeGDD.CRO', 'MaizeCalwpy.CRO', 'MaizeGDDwpy.CRO', 'MaizeSalinity.CRO',
         'MaizeSalinityGDD.CRO', 'Maize_EUirr_GDD.CRO', 'Maize_EUirr_cal.CRO',
         'tuber.CRO', 'veg.CRO', 'rice_test.CRO'),
        ('05-01', '06-10')):
    add('SW12', f'SW12_{crop.replace(".CRO","").lower()}_{onset.replace("-","")}',
        f'{crop.replace(".CRO","")} sown 2014-{onset}', crop=crop, tier='T1',
        season=(f'2014-{onset}', '2014-10-31'))


def main():
    CASES.mkdir(parents=True, exist_ok=True)
    from collections import Counter
    n = Counter()
    for c in CASES_SW:
        d = CASES / c['cid']
        d.mkdir(exist_ok=True)
        stage = CORE + [c['crop'], c['soil'], c['tnx'], c['eto'], c['plu']]
        stage += sorted(c['slots'].values())
        stage = list(dict.fromkeys(stage))
        _irr = (c.get('slots') or {}).get('irr', '')
        skip = ('# Inet reports a requirement, not applied water -- see O2.\n'
                'skip_invariants: [surface_balance, daily_soil_balance,\n'
                '                  daily_surface_balance]\n'
                if 'inet' in _irr.lower() else '')
        slot_lines = ''.join(f'\n      {k}: {v}' for k, v in sorted(c['slots'].items()))
        patch = ''
        if c['patch']:
            patch = '\npatch:\n'
            for fn, edits in c['patch'].items():
                patch += f'  {fn}:\n'
                for ln, txt in sorted(edits.items()):
                    patch += f'    {ln}: {json.dumps(txt)}\n'
        cd = c['cropdates'] or c['season']
        (d / 'case.yml').write_text(f"""id: {c['cid']}
desc: >
  Sweep {c['fam']}: {c['desc']}.
tier: {c['tier']}

stage:
{chr(10).join('  - ' + a for a in stage)}

project:
  name: {c['cid']}
  desc: {json.dumps(f"{c['fam']} - {c['desc']}")}
  runs:
    - year: 1
      sim: [{c['season'][0]}, {c['season'][1]}]
      crop: [{cd[0]}, {cd[1]}]
      cli: Ottawa.CLI
      tnx: {c['tnx']}
      eto: {c['eto']}
      plu: {c['plu']}
      co2: MaunaLoa.CO2
      cro: {c['crop']}
      sol: {c['soil']}{slot_lines}
{patch}
# sweeps take season output only; the invariants still run on every one
daily: []
particular: []
aggregate: 0
{skip}rtol: 1.0e-3
""")
        n[c['fam']] += 1
    print(f'wrote {sum(n.values())} sweep cases')
    for f, k in sorted(n.items()):
        print(f'  {f}: {k}')


if __name__ == '__main__':
    main()
