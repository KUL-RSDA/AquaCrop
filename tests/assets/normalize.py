#!/usr/bin/env python3
"""Normalise donated AquaCrop input files into tests/assets/.

Three fixes, all confined to text AquaCrop never parses:
  * CRLF -> LF, including a trailing orphan CR (the Tarn .CLI files end
    "MaunaLoa.CO2\r" with no final newline at all)
  * degree sign -> "deg", in both encodings the donated files use: UTF-8
    (C2 B0, the tmp/testcase pool) and ISO-8859-1 (bare B0, the tests/DATA
    pool). UTF-8 must be replaced first or it leaves a stray C2 behind. The
    sign only ever appears in the comment after the ':' on a line, which
    list-directed reads discard.
  * guarantee a final newline

Originals are kept byte-for-byte under assets/raw/ so the odd encodings stay
available as their own robustness cases.
"""
import pathlib, shutil, sys

ROOT = pathlib.Path(__file__).resolve().parent
SRC = pathlib.Path('/data/leuven/336/vsc33669/src/AquaCrop/tests/DATA')
SRC2 = pathlib.Path('/data/leuven/336/vsc33669/src/tmp/testcase/DATA')

# donated file -> (destination subdir, new name or None to keep)
PLAN = {
    'Alessandria7.CLI': ('climate', None), 'Alessandria7.Tnx': ('climate', None),
    'Alessandria7.ETo': ('climate', None), 'Alessandria7.PLU': ('climate', None),
    'Tarn-13-2019.CLI': ('climate', None), 'Tarn-13-2019.Tnx': ('climate', None),
    'Tarn-13-2019.ETo': ('climate', None), 'Tarn-13-2019.PLU': ('climate', None),
    'Tarn-22-2016.CLI': ('climate', None), 'Tarn-22-2016.Tnx': ('climate', None),
    'Tarn-22-2016.ETo': ('climate', None), 'Tarn-22-2016.PLU': ('climate', None),
    'Maize_EUirr_cal.CRO': ('crops', None),
    'Maize_EUirr_GDD.CRO': ('crops', None),
    'CLAY_LIS.SOL': ('soils', None),
    'SILT_LOAM_LIS.SOL': ('soils', None),
    'test.SOL': ('soils', 'SILT_LOAM_wideTAW.SOL'),   # == SILT_LOAM_LIS but WP 6.0
    'YoloClayLoam6.SOL': ('soils', None),
    'Inet.IRR': ('irr', None),
    'schedule_Tarn.IRR': ('irr', None),
    'TarnFert.MAN': ('man', None),
    '14-22_1px_Crop4.PPn': ('param', None),
    'rice_test.CRO': ('crops', None),          # transplanted grain, v7.3, fert. calib. 60 %
    'Var4.GWT': ('gwt', None),                 # variable table, 4 obs, EC varies, v4.0
    'WPSandLoam.SW0': ('sw0', None),           # wilting point, v3.2 -> the "< 41" gates
    'example_offseason.OFF': ('off', None),    # mulch before/after + 2 post-season events
}

# second donated pool: the files built during the GDD refactor
PLAN2 = {
    'MaizeGDD.CRO': ('crops', None),           # sown grain, GDD
    'MaizeGDDwpy.CRO': ('crops', None),        # == MaizeGDD but WPy 90 %
    'MaizeCalwpy.CRO': ('crops', None),        # the GUI's calendar twin of MaizeGDDwpy
    'MaizeSalinity.CRO': ('crops', None),      # salinity-sensitive: ECn 0, ECx 6, distortion 75 %
    'MaizeSalinityGDD.CRO': ('crops', None),   # its GDD twin
    'tuber.CRO': ('crops', None),              # transplanted tuber, GDD, fert. calib. 50 %
    'tuberwpy.CRO': ('crops', None),           # == tuber but WPy 90 %
    'veg.CRO': ('crops', None),                # transplanted leafy vegetable, GDD
    'IrriGen.IRR': ('irr', None),              # Generate/AllRAW/FixDepth, 4 periods, ECw 4
    'IrriGenFw.IRR': ('irr', None),            # same but 50 % surface wetted
    'DryTopSoil.SW0': ('sw0', None),           # 14 vol% -> below the germination threshold
    'SalineSoil.SW0': ('sw0', None),           # ECe 3.0 at FC
    'SalineSoilMild.SW0': ('sw0', None),       # ECe 1.0 at FC
    'Ottawa2.MAN': ('man', None),              # fertility 21 %, no cuttings
    'OttawaMulch.MAN': ('man', None),          # mulch 50 %, fertility 21 %
    'OttawaConst.Tnx': ('climate', None),      # constant 12/28 degC -> 15 GDD/day exactly
}

# already in the repo under testcase/
REPO = {
    '../../testcase/DATA/Ottawa.CLI': 'climate', '../../testcase/DATA/Ottawa.Tnx': 'climate',
    '../../testcase/DATA/Ottawa.ETo': 'climate', '../../testcase/DATA/Ottawa.PLU': 'climate',
    '../../testcase/DATA/AlfOttawaGDD.CRO': 'crops', '../../testcase/SIMUL/DEFAULT.CRO': 'crops',
    '../../testcase/DATA/Ottawa.SOL': 'soils', '../../testcase/SIMUL/DEFAULT.SOL': 'soils',
    '../../testcase/DATA/Ottawa.MAN': 'man', '../../testcase/PARAM/Ottawa.PPn': 'param',
    '../../testcase/DATA/21May.CAL': 'cal', '../../testcase/OBS/Ottawa.OBS': 'obs',
    '../../testcase/SIMUL/MaunaLoa.CO2': 'simul',
    '../../testcase/SIMUL/DailyResults.SIM': 'simul',
    '../../testcase/SIMUL/DailyResultsFullList.SIM': 'simul',
    '../../testcase/SIMUL/ParticularResults.SIM': 'simul',
    '../../testcase/SIMUL/ParticularResultsFullList.SIM': 'simul',
    '../../testcase/SIMUL/AggregationResults.SIM': 'simul',
    '../../testcase/SIMUL/EToData.SIM': 'simul',
    '../../testcase/SIMUL/RainData.SIM': 'simul',
    '../../testcase/SIMUL/TempData.SIM': 'simul',
}


def normalise(raw: bytes) -> bytes:
    out = raw.replace(b'\r\n', b'\n').replace(b'\r', b'\n')
    out = out.replace(b'\xc2\xb0', b'deg').replace(b'\xb0', b'deg')
    if out and not out.endswith(b'\n'):
        out += b'\n'
    return out


def main():
    n_new = n_repo = n_touched = 0
    for name, (sub, rename) in list(PLAN.items()) + list(PLAN2.items()):
        src = (SRC if name in PLAN else SRC2) / name
        if not src.exists():
            print(f'  MISSING {name}'); continue
        raw = src.read_bytes()
        (ROOT / 'raw' / name).write_bytes(raw)
        clean = normalise(raw)
        if clean != raw:
            n_touched += 1
        (ROOT / sub / (rename or name)).write_bytes(clean)
        n_new += 1
    for rel, sub in REPO.items():
        src = (ROOT / rel).resolve()
        if not src.exists():
            print(f'  MISSING {rel}'); continue
        shutil.copyfile(src, ROOT / sub / src.name)
        n_repo += 1
    print(f'staged {n_new} donated ({n_touched} needed normalising) '
          f'+ {n_repo} from testcase/')


if __name__ == '__main__':
    main()
