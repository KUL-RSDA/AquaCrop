#!/usr/bin/env python3
"""Generate group I (irrigation) and group J (field management).

Same base as G/H/K: maize on the Ottawa 2014 record, Ottawa.SOL, sown 21 May,
with one input file varying. Irrigation cases take daily output 1, whose Irri
column shows what was actually applied; management cases take output 2, where
fertility, weed and mulch effects show up in canopy and biomass.

The cuttings case uses the forage crop, since multiple cuttings are only
meaningful for subkind_Forage, and adds particular output 1 for the harvests
file.
"""
from __future__ import annotations

import json
import pathlib

ROOT = pathlib.Path(__file__).resolve().parent.parent
CASES = ROOT / 'cases'
SEASON = ('2014-05-21', '2014-10-31')
BASE = ['Ottawa.CLI', 'Ottawa.Tnx', 'Ottawa.ETo', 'Ottawa.PLU', 'MaunaLoa.CO2',
        'Ottawa.PPn', 'DEFAULT.CRO', 'DEFAULT.SOL', 'Ottawa.SOL']

#  id, slot, file, crop, tier, daily, particular, description
CASES_IJ: list[tuple] = [
    # ---- I: irrigation ----------------------------------------------------
    ('I03', 'irr', 'IRR_manual_dense.IRR',   'MaizeGDD.CRO', 'T1', [1], [], 'manual schedule, seven 25 mm events'),
    ('I04', 'irr', 'IRR_manual_single.IRR',  'MaizeGDD.CRO', 'T2', [1], [], 'a single irrigation event'),
    ('I06', 'irr', 'schedule_Tarn.IRR',      'MaizeGDD.CRO', 'T1', [1], [], 'the donated manual sprinkler schedule'),
    ('I08', 'irr', 'IRR_manual_saline.IRR',  'MaizeGDD.CRO', 'T1', [1], [], 'manual schedule with saline water, ECw 4'),
    ('I10', 'irr', 'IRR_gen_fixint.IRR',     'MaizeGDD.CRO', 'T1', [1], [], 'generate on a fixed 7-day interval'),
    ('I12', 'irr', 'IRR_gen_alldepl.IRR',    'MaizeGDD.CRO', 'T1', [1], [], 'generate at 40 mm depletion'),
    ('I14', 'irr', 'IRR_gen_allraw.IRR',     'MaizeGDD.CRO', 'T1', [1], [], 'generate at 50 % RAW depletion'),
    ('I15', 'irr', 'IRR_gen_allraw_100.IRR', 'MaizeGDD.CRO', 'T2', [1], [], 'generate at 100 % RAW depletion'),
    ('I17', 'irr', 'IRR_gen_allraw.IRR',     'MaizeGDD.CRO', 'T1', [1], [], 'generate, refill back to field capacity'),
    ('I18', 'irr', 'IRR_gen_fixdepth.IRR',   'MaizeGDD.CRO', 'T1', [1], [], 'generate, fixed 30 mm application'),
    ('I21', 'irr', 'IRR_gen_multiperiod.IRR', 'MaizeGDD.CRO', 'T2', [1], [], 'generate with the threshold changing mid-season'),
    ('I22', 'irr', 'Inet.IRR',               'MaizeGDD.CRO', 'T1', [1], [], 'net irrigation requirement, the donated file'),
    ('I23', 'irr', 'IRR_inet_50.IRR',        'MaizeGDD.CRO', 'T2', [1], [], 'net irrigation requirement at 50 % RAW'),
    ('I24', 'irr', 'IRR_inet_100.IRR',       'MaizeGDD.CRO', 'T2', [1], [], 'net irrigation requirement at 100 % RAW'),
    ('I25', 'irr', 'IRR_method_1.IRR',       'MaizeGDD.CRO', 'T1', [1], [], 'sprinkler irrigation'),
    ('I26', 'irr', 'IRR_method_2.IRR',       'MaizeGDD.CRO', 'T2', [1], [], 'basin irrigation'),
    ('I27', 'irr', 'IRR_method_3.IRR',       'MaizeGDD.CRO', 'T2', [1], [], 'border irrigation'),
    ('I28', 'irr', 'IRR_method_4.IRR',       'MaizeGDD.CRO', 'T2', [1], [], 'furrow irrigation'),
    ('I29', 'irr', 'IRR_method_5.IRR',       'MaizeGDD.CRO', 'T1', [1], [], 'drip irrigation'),
    ('I30', 'irr', 'IRR_fw30.IRR',           'MaizeGDD.CRO', 'T2', [1], [], 'irrigation wetting 30 % of the surface'),
    ('I31', 'irr', 'IRR_fw50.IRR',           'MaizeGDD.CRO', 'T2', [1], [], 'irrigation wetting 50 % of the surface'),
    ('I32', 'irr', 'IRR_gen_saline.IRR',     'MaizeGDD.CRO', 'T1', [1], [], 'generated schedule with saline water'),
    ('I33', 'irr', 'IrriGen.IRR',            'MaizeGDD.CRO', 'T1', [1], [], 'the donated generated schedule, ECw 4'),
    ('I34', 'irr', 'IrriGenFw.IRR',          'MaizeGDD.CRO', 'T2', [1], [], 'the same at 50 % surface wetting'),
    # ---- J: field management ----------------------------------------------
    ('J01', 'man', 'MAN_plain.MAN',          'MaizeGDD.CRO', 'T1', [2], [], 'a management file with nothing set'),
    ('J03', 'man', 'MAN_mulch50.MAN',        'MaizeGDD.CRO', 'T1', [2], [], '50 % mulch cover'),
    ('J04', 'man', 'MAN_mulch100.MAN',       'MaizeGDD.CRO', 'T1', [2], [], 'full mulch cover'),
    ('J05', 'man', 'MAN_mulch_full_effect.MAN', 'MaizeGDD.CRO', 'T2', [2], [], 'mulch with full evaporation suppression'),
    ('J07', 'man', 'OttawaMulch.MAN',        'MaizeGDD.CRO', 'T2', [2], [], 'the donated mulch file'),
    ('J09', 'man', 'MAN_fert25.MAN',         'MaizeGDD.CRO', 'T1', [2], [], 'soil fertility stress 25 %'),
    ('J10', 'man', 'MAN_fert50.MAN',         'MaizeGDD.CRO', 'T1', [2], [], 'soil fertility stress 50 %'),
    ('J11', 'man', 'MAN_fert75.MAN',         'MaizeGDD.CRO', 'T1', [2], [], 'soil fertility stress 75 %'),
    ('J12', 'man', 'MAN_fert100.MAN',        'MaizeGDD.CRO', 'T2', [2], [], 'soil fertility stress 100 %'),
    ('J13', 'man', 'TarnFert.MAN',           'MaizeGDD.CRO', 'T1', [2], [], 'the donated 40 % fertility file'),
    ('J14', 'man', 'MAN_bund10.MAN',         'MaizeGDD.CRO', 'T1', [1], [], '0.10 m soil bunds'),
    ('J15', 'man', 'MAN_bund30.MAN',         'MaizeGDD.CRO', 'T1', [1], [], '0.30 m soil bunds'),
    ('J18', 'man', 'MAN_runoff_off.MAN',     'MaizeGDD.CRO', 'T1', [1], [], 'surface runoff prevented by field practices'),
    ('J19', 'man', 'MAN_cn_plus10.MAN',      'MaizeGDD.CRO', 'T2', [1], [], 'field practices raising CN by 10'),
    ('J21', 'man', 'MAN_weeds25.MAN',        'MaizeGDD.CRO', 'T1', [2], [], 'weeds covering 25 % at canopy closure'),
    ('J22', 'man', 'MAN_weeds75.MAN',        'MaizeGDD.CRO', 'T2', [2], [], 'heavy weed infestation at 75 %'),
    ('J23', 'man', 'MAN_weeds_delta.MAN',    'MaizeGDD.CRO', 'T2', [2], [], 'weeds increasing by 50 % in mid-season'),
    ('J27', 'man', 'MAN_mulch_fert.MAN',     'MaizeGDD.CRO', 'T2', [2], [], 'mulch and fertility stress together'),
    ('J28', 'man', 'Ottawa2.MAN',            'MaizeGDD.CRO', 'T2', [2], [], 'the donated 21 % fertility file'),
    ('J29', 'man', 'MAN_cuts_list.MAN',      'AlfOttawaGDD.CRO', 'T1', [2], [1], 'forage with a fixed list of cutting days'),
]


def slug(t):
    return ''.join(c if c.isalnum() else '_' for c in t.lower())[:44].strip('_')


def main():
    CASES.mkdir(parents=True, exist_ok=True)
    for cid, slot, fname, crop, tier, daily, part, why in CASES_IJ:
        # Under IrriMode_Inet the reported Irri is a net requirement that never
        # crosses the soil surface, so the surface-balance invariant does not
        # apply (observation O2).
        skip = ('\n# Inet reports a requirement, not applied water -- see O2.\n'
                'skip_invariants: [surface_balance, daily_soil_balance,\n'
                '                  daily_surface_balance]\n'
                if 'net' in fname.lower() or 'Inet' in fname else '')
        name = f'{cid}_{slug(why)}'
        d = CASES / name
        d.mkdir(exist_ok=True)
        (d / 'case.yml').write_text(f"""id: {name}
desc: >
  {why}.
  {crop} on the Ottawa 2014 record, Ottawa.SOL, sown {SEASON[0]};
  the only variable is {fname}.
tier: {tier}

stage:
{chr(10).join('  - ' + a for a in BASE)}
  - {crop}
  - {fname}

project:
  name: {cid}
  desc: {json.dumps(f'{cid} - {why}')}
  runs:
    - year: 1
      sim: [{SEASON[0]}, {SEASON[1]}]
      cli: Ottawa.CLI
      tnx: Ottawa.Tnx
      eto: Ottawa.ETo
      plu: Ottawa.PLU
      co2: MaunaLoa.CO2
      cro: {crop}
      sol: Ottawa.SOL
      {slot}: {fname}

daily: {daily}
particular: {part}
aggregate: 0
rtol: 1.0e-3
{skip}""")
    print(f"wrote {len(CASES_IJ)} cases: "
          f"I={sum(1 for c in CASES_IJ if c[0][0] == 'I')} "
          f"J={sum(1 for c in CASES_IJ if c[0][0] == 'J')}")


if __name__ == '__main__':
    main()
