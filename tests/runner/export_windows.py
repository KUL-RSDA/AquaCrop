#!/usr/bin/env python3
"""Package test cases so they can be opened in the Windows AquaCrop GUI.

    python3 tests/runner/export_windows.py Q15
    python3 tests/runner/export_windows.py Q15 G05 --data 'C:\\AquaCrop\\DATA'
    python3 tests/runner/export_windows.py F14 F21 S10 --bundle geometry

For each case this writes tests/export/<ID>/ and tests/export/<ID>.zip holding:

  <ID>.PRM   the project, every path pointing at the GUI's DATA folder
  <ID>.*     every input the project names, renamed after the case (Q15.SOL,
             Q15.CRO, ...) so nothing overwrites a file of the same name
             already in that folder; the .CLI is rewritten to match
  <ID>.PPn   the program parameters, if the case has any
  README.txt where each file goes

Files are written with Windows line endings in cp1252. The inputs are those
the suite stages, so any patch a case applies is already in them.
"""
from __future__ import annotations

import argparse
import pathlib
import shutil
import sys
import tempfile
import zipfile

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))
import harness as H
import soil_check as SO

WIN_DATA = r'C:\Workdir\Programs\GUI_AC7.3\DATA'


def _write(path: pathlib.Path, lines):
    path.write_bytes(('\r\n'.join(lines) + '\r\n').encode('cp1252'))


def expected_geometry(spec, work):
    """Compartments the soil check predicts for a single-run project, as README lines.

    The soil check (tests/runner/soil_check.py) mirrors the compartment sizing of the
    Fortran on the branch it sits on. The GUI prints the compartment mid-depths on the
    line under the header of daily output block 5, so these can be checked by eye.
    """
    runs = spec['project']['runs']
    r = runs[0]
    if len(runs) > 1 or not r.get('sol') or not r.get('cro') or str(r['sol']).startswith('('):
        return []
    layers = SO.layers_from_sol(work / 'DATA' / r['sol'])
    zr = float((work / 'DATA' / r['cro']).read_text().splitlines()[37].split()[0])
    p = SO.predict(layers, zr)
    owner = SO.compartment_layer(p['thicknesses'], layers)
    # The same arithmetic as the depths AquaCrop prints under block 5 (run.f90, D5:
    # NodeD grows by half of each neighbouring thickness), so a two-decimal tie such
    # as 0.475 rounds the same way. Regraded thicknesses are whole multiples of 0.05 m.
    th = [0.05 * round(t / 0.05) if abs(t / 0.05 - round(t / 0.05)) < 1e-6 else t
          for t in p['thicknesses']]
    mids = [th[0] / 2.0]
    for i in range(1, len(th)):
        mids.append(mids[-1] + th[i - 1] / 2.0 + th[i] / 2.0)
    return [
        'Expected compartments (the Fortran on this branch, from the soil check):',
        f'  {p["n"]} compartments, profile {p["total"]:.2f} m   (WC({p["total"]:.2f}) in block 3)',
        '  mid-depths (m): ' + ' '.join(f'{m:.2f}' for m in mids),
        '  thickness  (m): ' + ' '.join(f'{t:.2f}' for t in p['thicknesses']),
        '  soil layer    : ' + ' '.join(f'{o + 1:>4}' for o in owner),
        f'  crop Zrmax {zr:.3f} m, root zone limited to {p["soil_rootmax"]:.3f} m',
        '',
    ]


def export(case_dir: pathlib.Path, out_root: pathlib.Path, win_data: str) -> pathlib.Path:
    spec = H.load_case(case_dir)
    if 'project' not in spec:
        raise SystemExit(f'{case_dir.name}: its project files are hand-written (LIST/), '
                         f'not generated -- export those by hand')
    cid = spec['id'].split('_')[0]
    with tempfile.TemporaryDirectory() as tmp:
        work = H.stage(spec, pathlib.Path(tmp) / cid)
        prm = next(p for p in (work / 'LIST').iterdir()
                   if p.suffix.upper() in ('.PRM', '.PRO'))
        lines = prm.read_text().splitlines()

        out = out_root / cid
        if out.exists():
            shutil.rmtree(out)
        out.mkdir(parents=True)

        # every file line in the project is followed by its path line
        renamed, taken = {}, set()
        for i in range(len(lines) - 1):
            name = lines[i].strip()
            path = lines[i + 1].strip().strip("'")
            if not path.startswith('./') or name.startswith('('):
                continue
            src = work / path[2:] / name
            if name not in renamed:
                ext = pathlib.Path(name).suffix
                new, k = f'{cid}{ext}', 2
                while new.upper() in taken:              # two crops in one project
                    new, k = f'{cid}_{k}{ext}', k + 1
                taken.add(new.upper())
                renamed[name] = (new, src)
            lines[i] = f'   {renamed[name][0]}'
            lines[i + 1] = f'   {win_data.rstrip(chr(92))}\\'

        for name, (new, src) in renamed.items():
            text = src.read_text().splitlines()
            if new.upper().endswith('.CLI'):
                # lines 3-6 name the temperature, ETo, rain and CO2 files
                text = [renamed[t.strip()][0] if t.strip() in renamed else t
                        for t in text]
            _write(out / new, text)

        ext = '.PP1' if prm.suffix.upper() == '.PRO' else '.PPn'
        params = work / 'PARAM' / f'{prm.stem}{ext}'
        _write(out / f'{cid}{prm.suffix.upper()}', lines)
        if params.is_file():
            _write(out / f'{cid}{ext}', params.read_text().splitlines())

        listing = [f'  {new:<14} was {name}' for name, (new, _) in renamed.items()]
        geometry = expected_geometry(spec, work)
        _write(out / 'README.txt', [
            f'{spec["id"]}',
            '',
            (spec.get('desc') or '').strip(),
            '',
            f'Copy the input files below into {win_data}\\',
            f'The project {cid}{prm.suffix.upper()} points every file at that folder.',
            '',
            *listing,
            '',
            *geometry,
            'In the GUI, switch on the daily output you want to compare. The output',
            'selection is a GUI setting and is not stored in the project. This case',
            f'was run with daily blocks {spec["daily"]}; blocks 3 and 5 show the',
            'profile depth and the per-compartment water contents with their mid-depths.',
            '',
            f'{cid}{prm.suffix.upper()} goes wherever the GUI keeps its projects (LIST).'
            if not params.is_file() else
            f'{cid}{prm.suffix.upper()} and {cid}{ext} go together where the GUI keeps '
            f'its projects (LIST); {cid}{ext} holds the program parameters this case '
            f'was run with and must keep the project\'s name to be picked up.',
            '',
            'Generated by tests/runner/export_windows.py.',
        ])

    zip_path = out_root / f'{cid}.zip'
    with zipfile.ZipFile(zip_path, 'w', zipfile.ZIP_DEFLATED) as z:
        for f in sorted(out.iterdir()):
            z.write(f, f'{cid}/{f.name}')
    return out


def main():
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument('cases', nargs='+', help='case ids, e.g. Q15 G05')
    ap.add_argument('--data', default=WIN_DATA, help='DATA folder on the Windows side')
    ap.add_argument('--out', type=pathlib.Path, default=H.ROOT / 'export')
    ap.add_argument('--bundle', metavar='NAME',
                    help='also write export/NAME.zip holding all the cases together')
    a = ap.parse_args()
    done = []
    for sel in a.cases:
        dirs = [d for d in sorted((H.ROOT / 'cases').iterdir())
                if d.name.split('_')[0] == sel or d.name == sel]
        if not dirs:
            raise SystemExit(f'no case {sel!r}')
        out = export(dirs[0], a.out, a.data)
        done.append((dirs[0].name, out))
        print(f'{dirs[0].name}\n  -> {out}  ({len(list(out.iterdir()))} files)'
              f'\n  -> {out.with_suffix(".zip")}')
    if a.bundle:
        zip_path = a.out / f'{a.bundle}.zip'
        with zipfile.ZipFile(zip_path, 'w', zipfile.ZIP_DEFLATED) as z:
            z.writestr('README.txt', '\r\n'.join(
                [f'{len(done)} AquaCrop test cases, one folder each.',
                 'Every input is named after its case, so all of them can be copied',
                 'into the same DATA folder without overwriting each other.', '']
                + [f'  {name}' for name, _ in done]) + '\r\n')
            for name, out in done:
                for f in sorted(out.iterdir()):
                    z.write(f, f'{out.name}/{f.name}')
        print(f'bundle -> {zip_path}')


if __name__ == '__main__':
    main()
