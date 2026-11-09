#!/usr/bin/env python3
"""Staging, execution and comparison for the AquaCrop test suite.

AquaCrop resolves its directories relative to the current working directory
(`SetPathNameOutp('OUTP/')` and friends in startunit.F90), so every case runs in
a working tree of its own. That is also what makes cases independent of each
other: no shared state beyond the read-only asset pool.
"""
from __future__ import annotations

import datetime
import pathlib
import re
import shutil
import subprocess
import sys

import yaml

ROOT = pathlib.Path(__file__).resolve().parent.parent      # tests/
REPO = ROOT.parent                                          # repo root
ASSETS = ROOT / 'assets'
CASES = ROOT / 'cases'
EXE = REPO / 'src' / 'aquacrop'

#: pool subdirectory -> working-tree subdirectory
DEST = {'climate': 'DATA', 'crops': 'DATA', 'soils': 'DATA', 'irr': 'DATA',
        'man': 'DATA', 'gwt': 'DATA', 'sw0': 'DATA', 'off': 'DATA',
        'cal': 'DATA', 'obs': 'OBS', 'param': 'PARAM', 'simul': 'SIMUL'}

#: AquaCrop reads these from SIMUL/ unconditionally (defaultcropsoil.f90:287,
#: global.f90:6806), but a project may ALSO name one as its own crop or soil, in
#: which case the .PRM points at DATA/. So they are staged to both places.
DEST_EXTRA = {'DEFAULT.CRO': 'SIMUL', 'DEFAULT.SOL': 'SIMUL'}

WORKDIRS = ('LIST', 'DATA', 'PARAM', 'SIMUL', 'OBS', 'OUTP')

EPOCH = datetime.date(1900, 12, 31)     # AquaCrop day number 0

DAILY_LABELS = {
    1: 'Various parameters of the soil water balance',
    2: 'Crop development and production',
    3: 'Soil water content in the soil profile and root zone',
    4: 'Soil salinity in the soil profile and root zone',
    5: 'Soil water content at various depths of the soil profile',
    6: 'Soil salinity at various depths of the soil profile',
    7: 'Climate input parameters',
    8: 'Irrigation events and intervals',
}
PARTICULAR_LABELS = {
    1: 'Biomass and Yield at Multiple cuttings (for herbaceous forage crops)',
    2: 'Evaluation of simulation results (when Field Data)',
}


def daynr(d) -> int:
    """AquaCrop day number for a date or an ISO date string."""
    if isinstance(d, str):
        d = datetime.date.fromisoformat(d)
    if isinstance(d, datetime.datetime):
        d = d.date()
    return d.toordinal() - EPOCH.toordinal()


def to_date(n: int) -> datetime.date:
    return datetime.date.fromordinal(n + EPOCH.toordinal())


# --------------------------------------------------------------------------
# asset pool


def build_pool() -> dict[str, tuple[pathlib.Path, str]]:
    """basename -> (source path, working-tree subdirectory)."""
    pool: dict[str, tuple[pathlib.Path, str]] = {}
    for sub, dest in DEST.items():
        d = ASSETS / sub
        if not d.is_dir():
            continue
        for f in sorted(d.iterdir()):
            if not f.is_file():
                continue
            if f.name in pool:
                raise SystemExit(f'duplicate asset name in the pool: {f.name}')
            pool[f.name] = (f, dest)
    return pool


POOL = build_pool()


# --------------------------------------------------------------------------
# case definition


class CaseError(Exception):
    pass


def load_case(case_dir: pathlib.Path) -> dict:
    f = case_dir / 'case.yml'
    if not f.is_file():
        raise CaseError(f'{case_dir.name}: no case.yml')
    spec = yaml.safe_load(f.read_text()) or {}
    spec.setdefault('id', case_dir.name)
    spec.setdefault('tier', 'T2')
    spec.setdefault('stage', [])
    spec.setdefault('patch', {})
    spec.setdefault('rtol', 1.0e-3)
    spec.setdefault('skip_lines', 1)      # the "Output created on <date>" header
    # A case where AquaCrop must stop and say why: the run passes when it ends
    # with a non-zero exit status and its console output contains this text.
    # There is no reference to compare. An explicit expect_exit is still checked.
    spec.setdefault('expect_error', None)
    spec.setdefault('expect_exit', None if spec['expect_error'] else 0)
    spec.setdefault('daily', [])
    # verbatim DailyResults.SIM, for cases about malformed selections
    spec.setdefault('daily_raw', None)
    # LIST/ListProjects.txt: omitted when 'absent', written verbatim when a
    # string, generated from the staged project files when None (the default)
    spec.setdefault('list_projects', None)
    spec.setdefault('particular', [])
    spec.setdefault('aggregate', 0)
    spec.setdefault('expect_compartments', None)
    # A case that documents a defect in AquaCrop rather than a property of it.
    # It is expected to fail; the runner reports it separately and does not let
    # it fail the suite, but DOES shout if it starts passing.
    spec.setdefault('known_defect', None)
    spec.setdefault('expect_gdd', None)
    spec.setdefault('skip_invariants', [])
    # water ponded between bunds at the start; an input to the water balance
    # that appears in no season-output column
    spec.setdefault('surface_storage_in', 0.0)
    spec.setdefault('expect_decade', None)     # {'eto': file} / {'rain': file}
    spec['daily'] = effective_daily(spec)
    spec['dir'] = case_dir
    return spec


# Every case writes the water balance (1) and crop (2) daily blocks on top of
# its own selection, so each run can be compared day by day and the water
# balance invariants see it. Left alone: the sweeps (season totals over many
# runs), the O family (its point is the output selection itself), cases that
# must stop, verbatim selections, and any case that sets `daily_core: false`.
CORE_DAILY = (1, 2)


def effective_daily(spec: dict) -> list:
    core = spec.get('daily_core')
    if core is None:
        core = not (str(spec['id']).startswith(('SW', 'O'))
                    or spec['expect_error'] or spec['daily_raw'] is not None)
    daily = list(spec['daily'] or [])
    return sorted(set(daily) | set(CORE_DAILY)) if core else daily


# --------------------------------------------------------------------------
# staging


def _apply_patch(path: pathlib.Path, edits: dict) -> None:
    """Rewrite individual 1-indexed lines of a staged file."""
    lines = path.read_text().splitlines()
    for lineno, text in edits.items():
        i = int(lineno) - 1
        if not 0 <= i < len(lines):
            raise CaseError(f'{path.name}: patch line {lineno} out of range '
                            f'(file has {len(lines)} lines)')
        lines[i] = text
    path.write_text('\n'.join(lines) + '\n')


def stage(spec: dict, work: pathlib.Path) -> pathlib.Path:
    """Build a complete AquaCrop working tree for one case."""
    if work.exists():
        shutil.rmtree(work)
    for d in WORKDIRS:
        (work / d).mkdir(parents=True)

    # 1. assets named in the case
    for name in spec['stage']:
        if name not in POOL:
            raise CaseError(f"{spec['id']}: unknown asset {name!r}")
        src, dest = POOL[name]
        shutil.copyfile(src, work / dest / name)
        if name in DEST_EXTRA:
            shutil.copyfile(src, work / DEST_EXTRA[name] / name)

    # 2. files carried by the case directory itself (project files, bespoke
    #    inputs); these override anything staged from the pool
    for d in WORKDIRS:
        local = spec['dir'] / d
        if local.is_dir():
            for f in sorted(local.iterdir()):
                if f.is_file():
                    shutil.copyfile(f, work / d / f.name)

    # 3. generated project file, if the case did not supply one
    if not any((work / 'LIST').iterdir()):
        if 'project' not in spec:
            raise CaseError(f"{spec['id']}: no LIST/ file and no project: block")
        name, text = render_project(spec)
        (work / 'LIST' / name).write_text(text)

    # 4. ListProjects.txt
    lp = work / 'LIST' / 'ListProjects.txt'
    if spec.get('list_projects') == 'absent':
        lp.unlink(missing_ok=True)          # AquaCrop then scans LIST/ itself
    elif isinstance(spec.get('list_projects'), str):
        lp.write_text(spec['list_projects'])
    elif not lp.is_file():
        projects = sorted(f.name for f in (work / 'LIST').iterdir()
                          if f.suffix.upper() in ('.PRM', '.PRO'))
        lp.write_text('\n'.join(projects) + '\n')

    # 5. output selection
    write_sim_files(spec, work / 'SIMUL')

    # 6. line patches, before the .PPn is renamed so the rename carries them
    for fname, edits in spec['patch'].items():
        hits = [p for d in WORKDIRS for p in (work / d).glob(fname)]
        if not hits:
            raise CaseError(f"{spec['id']}: patch target {fname!r} was not staged")
        for p in hits:
            _apply_patch(p, edits)

    # 7. program parameters must be named after the project.
    #    ComposeFileForProgramParameters (startunit.F90:749) derives
    #    PARAM/<project basename>.PPn for a .PRM and .PP1 for a .PRO, so a file
    #    staged under any other name is silently ignored and the built-in
    #    defaults are used instead -- with no warning of any kind.
    projects = [f for f in (work / 'LIST').iterdir()
                if f.suffix.upper() in ('.PRM', '.PRO')]
    params = [f for f in (work / 'PARAM').iterdir()
              if f.suffix.upper() in ('.PPN', '.PP1')]
    if projects and params:
        wanted = set()
        for proj in projects:
            want = proj.stem + ('.PP1' if proj.suffix.upper() == '.PRO' else '.PPn')
            wanted.add(want)
            if not (work / 'PARAM' / want).is_file():
                shutil.copyfile(params[0], work / 'PARAM' / want)
        # drop the source file if it is not itself one of the names AquaCrop
        # will look for, so the working tree shows exactly what is read
        for f in params:
            if f.name not in wanted:
                f.unlink()

    return work


def write_sim_files(spec: dict, simul: pathlib.Path) -> None:
    """Materialise the output-control .SIM files from the case's own settings.

    Cases carry `daily:`, `particular:` and `aggregate:` rather than shipping
    .SIM files, so the default (no daily output, season only) costs nothing.
    """
    if spec.get('daily_raw') is not None:
        simul.joinpath('DailyResults.SIM').write_text(spec['daily_raw'])
        part_raw = spec['particular']
        if part_raw:
            simul.joinpath('ParticularResults.SIM').write_text(
                ''.join(f' {n} : {PARTICULAR_LABELS[n]}\n' for n in part_raw))
        else:
            simul.joinpath('ParticularResults.SIM').unlink(missing_ok=True)
        simul.joinpath('AggregationResults.SIM').write_text(
            f"{spec['aggregate']} :  Time aggregation for intermediate results "
            f"(0 = none ; 1 = daily; 2 = 10-daily; 3 = monthly)\n")
        return

    daily = spec['daily']
    if daily:
        simul.joinpath('DailyResults.SIM').write_text(
            ''.join(f' {n} : {DAILY_LABELS[n]}\n' for n in daily))
    else:
        simul.joinpath('DailyResults.SIM').unlink(missing_ok=True)

    part = spec['particular']
    if part:
        simul.joinpath('ParticularResults.SIM').write_text(
            ''.join(f' {n} : {PARTICULAR_LABELS[n]}\n' for n in part))
    else:
        simul.joinpath('ParticularResults.SIM').unlink(missing_ok=True)

    simul.joinpath('AggregationResults.SIM').write_text(
        f"{spec['aggregate']} :  Time aggregation for intermediate results "
        f"(0 = none ; 1 = daily; 2 = 10-daily; 3 = monthly)\n")


# --------------------------------------------------------------------------
# project file generation

_SECTIONS = [
    ('-- 1. Climate (CLI) file', 'cli', 'DATA'),
    ('   1.1 Temperature (Tnx or TMP) file', 'tnx', 'DATA'),
    ('   1.2 Reference ET (ETo) file', 'eto', 'DATA'),
    ('   1.3 Rain (PLU) file', 'plu', 'DATA'),
    ('   1.4 Atmospheric CO2 concentration (CO2) file', 'co2', 'SIMUL'),
    ('-- 2. Calendar (CAL) file', 'cal', 'DATA'),
    ('-- 3. Crop (CRO) file', 'cro', 'DATA'),
    ('-- 4. Irrigation management (IRR) file', 'irr', 'DATA'),
    ('-- 5. Field management (MAN) file', 'man', 'DATA'),
    ('-- 6. Soil profile (SOL) file', 'sol', 'DATA'),
    ('-- 7. Groundwater table (GWT) file', 'gwt', 'DATA'),
    ('-- 8. Initial conditions (SW0) file', 'sw0', 'DATA'),
    # NB: the key is 'offseason', not 'off' -- YAML 1.1 parses a bare `off`
    # as the boolean False, which silently drops the file from the project.
    ('-- 9. Off-season conditions (OFF) file', 'offseason', 'DATA'),
    ('-- 10. Field data (OBS) file', 'obs', 'OBS'),
]


def render_project(spec: dict) -> tuple[str, str]:
    """Render a .PRM (or .PRO) from the case's `project:` block.

    project:
      name: F14_deep_soil          # -> F14_deep_soil.PRM
      desc: 4 m profile, 3 m roots
      version: '7.3'
      runs:
        - year: 1
          sim: [2014-05-21, 2014-10-31]
          crop: [2014-05-21, 2014-10-31]      # defaults to sim
          cli: Ottawa.CLI
          tnx: Ottawa.Tnx
          ...
          sw0: KeepSWC                        # the literal keeps the profile
    """
    p = spec['project']
    runs = p['runs']
    for run in runs:
        stray = [k for k in run if isinstance(k, bool)]
        if stray:
            raise CaseError(
                f"{spec['id']}: a run key parsed as a YAML boolean ({stray}). "
                f"Quote it, or use 'offseason' rather than 'off'.")
    ext = 'PRO' if p.get('type', 'PRM').upper() == 'PRO' else 'PRM'
    name = f"{p.get('name', spec['id'])}.{ext}"

    out = [p.get('desc', spec['id']),
           f"      {p.get('version', '7.3')}  : AquaCrop Version "
           f"{p.get('version_note', '(January 2026)')}"]

    for run in runs:
        sim = run['sim']
        crop = run.get('crop', sim)
        label = ('Seeding/planting year' if run.get('year', 1) == 1
                 else 'Non-seeding/planting year')
        out.append(f"      {run.get('year', 1)}         "
                   f": Year number of cultivation ({label})")
        for tag, day in (('First day of simulation period', sim[0]),
                         ('Last day of simulation period', sim[1]),
                         ('First day of cropping period', crop[0]),
                         ('Last day of cropping period', crop[1])):
            n = daynr(day)
            d = to_date(n)
            out.append(f"  {n:5d}         : {tag} - {d.day} {d:%B %Y}")
        for header, key, folder in _SECTIONS:
            out.append(header)
            val = run.get(key)
            if val is None or (isinstance(val, str) and val.startswith('(')):
                out += ['   (None)', '   (None)']
            elif val == 'KeepSWC':
                out += ['   KeepSWC', '   Keep soil water profile of previous run']
            else:
                out += [f'   {val}', f"   './{folder}/'"]

    return name, '\n'.join(out) + '\n'


# --------------------------------------------------------------------------
# execution


def run(work: pathlib.Path, exe: pathlib.Path = EXE, timeout: int = 600):
    if not exe.is_file():
        raise CaseError(f'executable not found: {exe} — run `make -C src` first')
    proc = subprocess.run([str(exe)], cwd=work, capture_output=True,
                          text=True, timeout=timeout)
    # Keep the whole console transcript next to the working tree; a Fortran
    # runtime error prints a backtrace far longer than any summary line.
    if proc.returncode != 0:
        (work / 'RUN.log').write_text(
            f'$ {exe}\n# cwd {work}\n# exit {proc.returncode}\n\n'
            f'--- stdout ---\n{proc.stdout}\n--- stderr ---\n{proc.stderr}\n')
    return proc


# --------------------------------------------------------------------------
# structural assertions


# run.f90:4168 writes 'WC01' for the first compartment, then 'WC'//trim(Str1)
# where Str1 comes from write(Str1,'(i2)') -- which leaves a LEADING blank that
# trim() does not strip. So compartments 2-9 are labelled 'WC 2'..'WC 9', and
# only 1 and 10+ are zero-padded. Matching \d{2} alone silently reports 1
# compartment for any profile with 9 or fewer.
_WC_COL = re.compile(r'WC\s*(\d+)')


def count_compartments(outp: pathlib.Path):
    """Number of compartments AquaCrop actually used, from the daily output.

    Out5CompWC writes one WC<nn> column per compartment (run.f90:4168), so the
    header encodes NrCompartments directly. Returns None if the case did not
    request daily output 5.
    """
    for f in sorted(outp.glob('*day.OUT')):
        for line in f.read_text().splitlines()[:12]:
            cols = _WC_COL.findall(line)
            if cols:
                return max(int(c) for c in cols)
    return None


def check_gdd(outp: pathlib.Path, spec_gdd: dict):
    """Assert the daily GD column against an independent GDD computation.

    `gdd_check.degrees_day` is a hand port of global.f90:2449; comparing the
    model's own GD series against it checks the arithmetic itself, not just that
    today's run matches yesterday's.
    """
    import gdd_check as G
    tol = spec_gdd.get('tol', 0.051)
    files = sorted(outp.glob('*day.OUT'))
    if not files:
        return ['  expect_gdd set but the case produced no daily output '
                '(is daily output 2 enabled?)']
    got = daily_column(files[0], 'GD')
    if not got:
        return ['  expect_gdd set but no GD column found in the daily output']
    tnx = ASSETS / 'climate' / spec_gdd['tnx']
    bad = []
    for d, model in got:
        try:
            o = G.series(tnx, d, d, spec_gdd['tbase'], spec_gdd['tupper'],
                         spec_gdd.get('method', 3))[0]
        except ValueError:
            continue                      # outside the record: nothing to check
        if abs(model - o) > tol:
            bad.append((d, model, round(o, 3)))
    if bad:
        head = ', '.join(f'{d} model {m} expected {o}' for d, m, o in bad[:4])
        return [f'  GD differs from the independent check on {len(bad)} of {len(got)} days: {head}']
    return []


def check_profile_sum(outp: pathlib.Path, thicknesses, sol_name):
    """Assert the per-compartment water content reconstructs the profile total.

    Out5CompWC reports each compartment in vol% of the SOIL MATRIX, so the
    profile depth in mm is

        sum_i  WC_i/100 * thickness_i * 1000 * (1 - GravelVol_i/100)

    Without the gravel term the 60 %-gravel case is out by 168 mm; with it,
    0.17 mm. This ties the reported water content to the independent check's thickness
    vector, so a compartment geometry that differed from the prediction fails
    here even when the compartment count matches.
    """
    import re as _re
    import soil_check as SO
    files = sorted(outp.glob('*day.OUT'))
    if not files:
        return []
    f = files[0]
    hdr = next((l for l in f.read_text().splitlines()
                if l.strip().startswith('Day ')), '')
    m = _re.search(r'WC\([0-9.]+\)', hdr)
    if not m or 'WC01' not in hdr:
        return []
    layers = SO.layers_from_sol(ASSETS / 'soils' / sol_name)
    owner = SO.compartment_layer(thicknesses, layers)
    fac = [1.0 - SO.gravel_volume(layers[o]['sat'], layers[o]['gravel']) / 100.0
           for o in owner]
    prof = daily_column(f, m.group())
    comps = []
    for i in range(1, len(thicknesses) + 1):
        lbl = f'WC{i:02d}' if (i == 1 or i >= 10) else f'WC {i}'
        c = daily_column(f, lbl)
        if not c:
            return []
        comps.append(c)
    n = min(len(prof), min(len(c) for c in comps))
    if n < 1:
        return []
    tol = 0.5 * sum(thicknesses) + 0.5          # vol% printed to one decimal
    worst = (0, 0.0)
    for k in range(n):
        s_ = sum(comps[i][k][1] / 100.0 * thicknesses[i] * 1000.0 * fac[i]
                 for i in range(len(thicknesses)))
        dv = s_ - prof[k][1]
        if abs(dv) > abs(worst[1]):
            worst = (k, dv)
    if abs(worst[1]) > tol:
        return [f'  per-compartment water does not reconstruct the profile: '
                f'off by {worst[1]:+.2f} mm on day {worst[0]+1} (tolerance {tol:.2f})']
    return []


def check_compartment_bounds(outp: pathlib.Path, thicknesses, sol_name):
    """No compartment may hold more water than its layer's porosity, or less
    than none. Out5CompWC is vol% of the soil matrix and SAT is quoted the same
    way, so the comparison is direct.
    """
    import re as _re
    import soil_check as SO
    files = sorted(outp.glob('*day.OUT'))
    if not files:
        return []
    f = files[0]
    hdr = next((l for l in f.read_text().splitlines()
                if l.strip().startswith('Day ')), '')
    if 'WC01' not in hdr:
        return []
    layers = SO.layers_from_sol(ASSETS / 'soils' / sol_name)
    owner = SO.compartment_layer(thicknesses, layers)
    out = []
    for i in range(1, len(thicknesses) + 1):
        lbl = f'WC{i:02d}' if (i == 1 or i >= 10) else f'WC {i}'
        col = daily_column(f, lbl)
        if not col:
            break
        sat = layers[owner[i - 1]]['sat']
        hi = max(col, key=lambda t: t[1])
        lo = min(col, key=lambda t: t[1])
        if hi[1] > sat + 0.05:                      # 0.05 = print rounding
            out.append(f'  compartment {i} exceeds its layer porosity on '
                       f'{hi[0]}: {hi[1]} vol% vs SAT {sat}')
        if lo[1] < -0.05:
            out.append(f'  compartment {i} is negative on {lo[0]}: {lo[1]} vol%')
    return out[:4]


def check_root_zone(outp: pathlib.Path, soil_rootmax: float, tol: float = 0.011):
    """The daily header's Wr(x) names the depth water is accounted over.

    It must equal Soil_RootMax -- the crop's maximum rooting depth after
    ZrAdjustedToRestrictiveLayers -- which the soil check (soil_check.py) predicts
    independently. Note this is NOT the same as the Z column, which reports an
    unrestricted depth and can exceed it on a restrictive profile (see O5).
    """
    import re as _re
    files = sorted(outp.glob('*day.OUT'))
    if not files:
        return []
    hdr = next((l for l in files[0].read_text().splitlines()
                if l.strip().startswith('Day ')), '')
    m = _re.search(r'Wr\(([0-9.]+)\)', hdr)
    if not m:
        return []
    got = float(m.group(1))
    if abs(got - soil_rootmax) > tol:
        return [f'  root zone depth: model accounts over {got} m, '
                f'independent check gives Soil_RootMax {soil_rootmax} m']
    return []


def check_decade(outp: pathlib.Path, spec_dec: dict, tol: float = 0.051):
    """Assert an interpolated 10-daily climate series against decade_check.

    ETo is turned into a piecewise-linear daily curve preserving the decade
    mean; rain is spread flat. Both are checked against an independent port.
    """
    import decade_check as D
    files = sorted(outp.glob('*day.OUT'))
    if not files:
        return ['  expect_decade set but the case produced no daily output']
    out = []
    for kind, col, fn in (('eto', 'ETo', D.daily), ('rain', 'Rain', D.daily_rain)):
        name = spec_dec.get(kind)
        if not name:
            continue
        src = ASSETS / 'climate' / name
        got = daily_column(files[0], col)
        if not got:
            out.append(f'  expect_decade[{kind}] set but no {col} column found')
            continue
        bad = []
        for d, model in got:
            try:
                o = fn(src, d)
            except ValueError:
                continue
            if abs(model - o) > tol:
                bad.append((d, model, round(o, 3)))
        if bad:
            head = ', '.join(f'{d} model {m} expected {o}' for d, m, o in bad[:3])
            out.append(f'  {col} differs from the independent check on '
                       f'{len(bad)} of {len(got)} days: {head}')
    return out


# --------------------------------------------------------------------------
# comparison


def compare(outp: pathlib.Path, ref: pathlib.Path, rtol: float, skip: int, ulp: float = 0.0):
    """Compare an OUTP tree against a reference tree.

    Returns (verdict, lines). Verdict is 'pass', 'close' or 'fail'.
    """
    cmp_py = pathlib.Path(__file__).parent / 'compare_numeric.py'
    msgs, worst = [], 0
    ref_files = sorted(f for f in ref.iterdir() if f.is_file())
    if not ref_files:
        return 'fail', [f'reference {ref} is empty']

    for rf in ref_files:
        of = outp / rf.name
        if not of.is_file():
            msgs.append(f'  missing in OUTP: {rf.name}')
            worst = max(worst, 2)
            continue
        r = subprocess.run([sys.executable, str(cmp_py), str(rf), str(of),
                            '--rtol', str(rtol), '--skip', str(skip),
                            '--ulp', str(ulp)],
                           capture_output=True, text=True)
        if r.returncode:
            worst = max(worst, r.returncode)
            tag = '~' if r.returncode == 1 else 'x'
            msgs.append(f'  {tag} {rf.name}')
            msgs += ['    ' + l for l in r.stdout.strip().splitlines() if l.strip()]

    extra = sorted(f.name for f in outp.iterdir()
                   if f.is_file() and not (ref / f.name).is_file())
    if extra:
        msgs.append('  extra in OUTP: ' + ', '.join(extra))
        worst = max(worst, 2)

    return {0: 'pass', 1: 'close'}.get(worst, 'fail'), msgs


_WC_TOTAL = re.compile(r'WC\(([0-9.]+)\)')


def compartment_geometry(outp: pathlib.Path):
    """(count, total depth, mid-depths) as the model reports them.

    The daily header's first row names one WC column per compartment and the
    second row carries each compartment's mid-depth; `WC(x.xx)` in the first
    row is the total profile depth the compartments cover.
    """
    for f in sorted(outp.glob('*day.OUT')):
        lines = f.read_text().splitlines()
        for i, line in enumerate(lines[:12]):
            cols = _WC_COL.findall(line)
            if not cols:
                continue
            n = max(int(c) for c in cols)
            m = _WC_TOTAL.search(line)
            total = float(m.group(1)) if m else None
            mids = [float(x) for x in lines[i + 1].split()[-n:]] \
                if i + 1 < len(lines) else []
            return n, total, mids
    return None, None, []


def daily_runs(path: pathlib.Path, name: str):
    """daily_column split into contiguous date segments.

    A multi-run day file concatenates its runs. Unless the project carries the
    profile forward with KeepSWC, soil water restarts at the first day of each
    run, so a difference taken across that boundary is not a flux and no balance
    identity applies to it. Splitting wherever the date is not the next calendar
    day isolates each run, and leaves a KeepSWC project -- whose runs are
    date-contiguous -- as a single segment, which is correct: its profile really
    does carry over.
    """
    series = daily_column(path, name)
    if not series:
        return []
    runs, cur = [], [series[0]]
    for prev, item in zip(series, series[1:]):
        if (item[0] - prev[0]).days != 1:
            runs.append(cur)
            cur = [item]
        else:
            cur.append(item)
    runs.append(cur)
    return runs


#: how far a value's right edge may sit from its header label's right edge.
#: Labels can be wider than their values, so this is not symmetric slack: it is
#: the widest label-minus-value overhang in any output block ('Salt(3.05)' at 10
#: characters over '26.938' at 6, which is 4).
_EDGE_SLACK = 5


def daily_column(path: pathlib.Path, name: str, strict: bool = True):
    """[(date, value)] for one named column of a daily output file.

    The header cannot be split on whitespace: compartment columns are labelled
    'WC 2' .. 'WC 9' and 'ECe 2' .. 'ECe 9' (see D3), so a 12-compartment header
    tokenises to 114 fields over 98 actual columns. Columns are right-aligned in
    fixed width, so the label's right edge in the header is matched against each
    value's right edge in the data rows.
    """
    import datetime
    out, edge, hdr_len, seen_label = [], None, None, False
    for line in pathlib.Path(path).read_text().splitlines():
        stripped = line.strip()
        if stripped.startswith('Day ') or stripped.startswith('Day\t'):
            m = re.search(r'(?<![A-Za-z0-9(])' + re.escape(name) + r'(?![A-Za-z0-9.)])',
                          line)
            edge = m.end() if m else None
            seen_label = seen_label or edge is not None
            hdr_len = len(line)
            continue
        if edge is None:
            continue
        t = line.split()
        if len(t) < 3 or not (t[0].isdigit() and t[1].isdigit() and t[2].isdigit()):
            continue
        try:
            d = datetime.date(int(t[2]), int(t[1]), int(t[0]))
        except ValueError:
            continue
        # the value whose right edge sits closest to the header label's
        best, bestd = None, 1 << 30
        for m in re.finditer(r'\S+', line):
            dist = abs(m.end() - edge)
            if dist < bestd:
                best, bestd = m.group(), dist
        if best is not None and bestd <= _EDGE_SLACK:
            try:
                out.append((d, float(best)))
            except ValueError:
                pass
    return out


def check_compartments(outp: pathlib.Path, expected: int,
                       expect_depth=None, expect_mids=None, tol=0.011):
    """Assert the model's compartment geometry against the independent check (soil_check.py).

    Mid-depths are compared with a tolerance: 0.175 and 2.425 are exact ties at
    two decimals, and Fortran's output formatting rounds them the opposite way
    from Python, so demanding equality would fail on rounding rather than on
    geometry.
    """
    got, total, mids = compartment_geometry(outp)
    if got is None:
        return ['  expect_compartments set but no WC columns in the daily output '
                '(is daily output 5 enabled?)']
    out = []
    if got != expected:
        out.append(f'  compartments: model used {got}, independent check predicted {expected}')
    if expect_depth is not None and total is not None \
            and abs(total - expect_depth) > tol:
        out.append(f'  profile depth: model {total}, independent check {expect_depth}')
    if expect_mids and mids and len(mids) == len(expect_mids):
        bad = [(i + 1, m, e) for i, (m, e) in enumerate(zip(mids, expect_mids))
               if abs(m - e) > tol]
        if bad:
            out.append('  compartment mid-depths differ from the independent check: ' + ', '.join(
                f'#{i} model {m} expected {e}' for i, m, e in bad[:4]))
    return out


def _unused_check_compartments(outp: pathlib.Path, expected: int):
    """Assert the model used the compartment count the independent check predicted."""
    got = count_compartments(outp)
    if got is None:
        return ['  expect_compartments set but no WC columns in the daily output '
                '(is daily output 5 enabled?)']
    if got != expected:
        return [f'  compartments: model used {got}, independent check predicted {expected}']
    return []


# --------------------------------------------------------------------------


def discover(selectors) -> list[pathlib.Path]:
    """Resolve case selectors: exact ids, group letters (F), or 'all'."""
    all_cases = sorted(d for d in CASES.iterdir()
                       if d.is_dir() and (d / 'case.yml').is_file())
    if not selectors or 'all' in selectors:
        return all_cases
    picked, seen = [], set()
    for sel in selectors:
        hits = [d for d in all_cases
                if d.name == sel or d.name.split('_')[0] == sel
                or (sel.isalpha() and d.name[:len(sel)] == sel
                    and d.name[len(sel):len(sel) + 2].isdigit())]
        if not hits:
            raise SystemExit(f'no case matches {sel!r}')
        for h in hits:
            if h.name not in seen:
                seen.add(h.name)
                picked.append(h)
    return picked
