#!/usr/bin/env python3
"""Properties that must hold for every case, checked instead of frozen.

Group Z of the test plan. These cost no reference storage and apply to every
case in the suite, including ones added later -- so they catch a whole class of
problem without anyone writing a case for it. A frozen reference only says
"the same as last time"; an invariant says "this is not physically possible".
"""
from __future__ import annotations

import math
import pathlib
import re
import sys

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))
from harness import daily_column, daily_runs                  # noqa: E402


def _season_rows(path: pathlib.Path):
    """[(header, values)] for each Tot(n)/run row of a season output file."""
    lines = path.read_text().splitlines()
    hdr = None
    for line in lines:
        t = line.split()
        if t[:1] == ['RunNr']:
            hdr = t
            continue
        # the data row carries a trailing project-file name with no header
        # label, so it is one token LONGER than the header
        if hdr and t and re.match(r'^(Tot|Run)\(?\d*\)?$', t[0]) and len(t) >= len(hdr):
            yield hdr, t


def _num(x):
    try:
        return float(x)
    except ValueError:
        return None


def check(outp: pathlib.Path, tol: float = 0.25, expect_ppn: bool = False,
          skip: tuple = (), storage_in: float = 0.0,
          bunded: bool = False) -> list[str]:
    """Returns a list of violations; empty means every invariant held."""
    bad: list[str] = []

    # A project's .PPn must be named after the project or it is silently
    # ignored (ComposeFileForProgramParameters, startunit.F90:749).
    # ListProjectsLoaded.OUT is the only place that says which happened, so a
    # case that stages parameters asserts they were actually read.
    if expect_ppn:
        f = outp / 'ListProjectsLoaded.OUT'
        if f.is_file() and 'default setting of program parameters' in f.read_text():
            bad.append('  program parameters were NOT read -- the .PPn must be '
                       'named after the project (see the note under group N)')

    # Z17 -- no NaN, Inf or overflowed field in any output cell
    for f in sorted(outp.glob('*.OUT')):
        text = f.read_text()
        for token in ('NaN', 'nan', 'Infinity', '*****'):
            if token in text:
                bad.append(f'  {f.name}: contains {token!r}')
                break

    for f in sorted(outp.glob('*season.OUT')):
        for hdr, t in _season_rows(f):
            col = dict(zip(hdr, t))

            def g(name):
                return _num(col.get(name, ''))

            rain, irri = g('Rain'), g('Irri')
            infilt, ro = g('Infilt'), g('Runoff')
            # Z01 -- everything arriving at the surface either infiltrates or
            # runs off. Not applicable under IrriMode_Inet, where the reported
            # Irri is a computed net requirement that never crosses the surface:
            # Infilt accounts for rain alone (see observation O2).
            # Only valid when no water stands on the surface at either end,
            # i.e. an unbunded field: with bunds, whatever is still ponded on
            # the last day is missing from Infilt + Runoff.
            if ('surface_balance' not in skip and not bunded
                    and None not in (rain, irri, infilt, ro)):
                # water ponded between bunds at the start of the simulation is
                # an input to the balance too, but appears in no season column
                lhs, rhs = rain + irri + storage_in, infilt + ro
                if abs(lhs - rhs) > tol:
                    extra = f' + stored {storage_in}' if storage_in else ''
                    bad.append(f'  {f.name} {t[0]}: surface balance off by '
                               f'{lhs - rhs:+.2f} mm '
                               f'(Rain {rain} + Irri {irri}{extra} '
                               f'!= Infilt {infilt} + RO {ro})')

            # Z04/Z05 -- fluxes cannot be negative
            for name in ('Rain', 'Irri', 'Infilt', 'Runoff', 'Drain', 'E', 'Tr', 'ETo'):
                v = g(name)
                if v is not None and v < -1e-9:
                    bad.append(f'  {f.name} {t[0]}: {name} is negative ({v})')

            # Ratios and stress levels are percentages. -9 is undef_int, which
            # the model writes when the ratio is undefined -- Tr/Trx with no
            # transpiration at all, for instance -- so it is a valid value, not
            # an out-of-range one.
            for name in ('E/Ex', 'Tr/Trx', 'SaltStr', 'FertSt'):
                v = g(name)
                if v is not None and abs(v + 9.0) > 1e-9 \
                        and not (-0.5 <= v <= 100.5):
                    bad.append(f'  {f.name} {t[0]}: {name} outside 0-100 ({v})')

            # Z -- a cycle cannot be negative
            cyc = g('Cycle')
            if cyc is not None and abs(cyc + 9.0) > 1e-9 and cyc < 0:
                bad.append(f'  {f.name} {t[0]}: negative cycle length ({cyc})')

    # ---- daily mass balance -------------------------------------------
    # Both identities were derived from the output rather than assumed, and
    # verified across every case that reports a full daily balance: residuals
    # are at most 0.20 mm, which is the print precision of six one-decimal
    # terms. They are far stronger than the season-level check, and the surface
    # one handles ponded water natively because Surf is a reported column.
    # Z03 -- salt in the profile changes by exactly what arrives less what
    # leaves: d(Salt) == SaltIn + SaltUp - SaltOut, per run segment, in ton/ha.
    #
    # This closes to the print precision (worst 0.007 over 45 run segments)
    # EXCEPT where salt precipitates. Dissolved salt lives in
    # Compartment%Salt and precipitated salt in Compartment%Depo, and only the
    # first is reported: 'Depo' appears 50 times in simul.f90 and never once in
    # run.f90, which writes every output file. So salt crossing from solution
    # into deposit leaves Salt(x) without passing through SaltOut, and the
    # reported balance cannot close. The cases where that happens carry
    # skip_invariants: [salt_balance] and are listed under O9.
    if 'salt_balance' not in skip:
        for f in sorted(outp.glob('*day.OUT')):
            hdr = next((l for l in f.read_text().splitlines()
                        if l.strip().startswith('Day ')), '')
            prof = next(iter(re.findall(r'Salt\([0-9.]+\)', hdr)), None)
            if not prof or 'SaltIn' not in hdr:
                continue
            try:
                runs = {k: daily_runs(f, k)
                        for k in (prof, 'SaltIn', 'SaltOut', 'SaltUp')}
            except Exception:                                    # noqa: BLE001
                continue
            if not runs[prof] or any(len(v) != len(runs[prof])
                                     for v in runs.values()):
                continue
            for seg in range(len(runs[prof])):
                cols = [[v for _, v in runs[k][seg]]
                        for k in (prof, 'SaltIn', 'SaltOut', 'SaltUp')]
                m = min(map(len, cols))
                if m < 3:
                    continue
                salt, sin, sout, sup = (c[:m] for c in cols)
                d = ((salt[-1] - salt[0])
                     - (sum(sin[1:]) + sum(sup[1:]) - sum(sout[1:])))
                if abs(d) > 0.05:
                    bad.append(
                        f'  {f.name}: run {seg + 1} salt balance off by '
                        f'{d:+.3f} ton/ha -- d(Salt)={salt[-1] - salt[0]:+.3f} '
                        f'but In+Up-Out={sum(sin[1:]) + sum(sup[1:]) - sum(sout[1:]):+.3f}')

    for f in sorted(outp.glob('*day.OUT')):
        hdr = next((l for l in f.read_text().splitlines()
                    if l.strip().startswith('Day ')), '')
        if 'Infilt' not in hdr or 'Drain' not in hdr:
            continue
        wc = next(iter(re.findall(r'WC\([0-9.]+\)', hdr)), None)
        if not wc:
            continue
        try:
            cols = [daily_runs(f, x)
                    for x in (wc, 'Rain', 'Irri', 'Surf', 'Infilt', 'RO',
                              'Drain', 'CR', 'E', 'Tr')]
        except Exception:                                    # noqa: BLE001
            continue
        if not cols[0] or any(len(c) != len(cols[0]) for c in cols):
            continue
        segments = []
        for seg in range(len(cols[0])):
            vals = [[v for _, v in c[seg]] for c in cols]
            m = min(map(len, vals))
            if m >= 3:
                segments.append([v[:m] for v in vals])
        if not segments:
            continue
        # The soil profile and the surface pond must be balanced TOGETHER.
        # Splitting them fails on a bunded field: evaporation from ponded water
        # is inside the reported total E but never touched the soil, so a soil
        # balance over-counts by exactly what a surface balance under-counts.
        # Verified across 201 cases; worst residual 0.20 mm, the print precision
        # of seven one-decimal terms.
        if 'daily_water_balance' not in skip:
            for si, (W, R, I_, S, INF, RO, DR, CR, E, TR) in enumerate(segments, 1):
                n = len(W)
                off = [(k, ((W[k] + S[k]) - (W[k-1] + S[k-1]))
                        - (R[k] + I_[k] + CR[k] - RO[k] - DR[k] - E[k] - TR[k]))
                       for k in range(1, n)]
                worst = max(off, key=lambda t: abs(t[1]))
                if abs(worst[1]) > 0.25:
                    where = (f'day {worst[0]+1}' if len(segments) == 1
                             else f'run {si} day {worst[0]+1}')
                    bad.append(f'  {f.name}: water balance off by '
                               f'{worst[1]:+.2f} mm on {where} '
                               f'(d(WC+Surf) != Rain + Irri + CR - RO - Drain - E - Tr)')
                    break

    for f in sorted(outp.glob('*day.OUT')):
        text = f.read_text().splitlines()
        hdr = None
        for line in text:
            st = line.strip()
            if st.startswith('Day ') and 'Month' in st:
                hdr = line
                continue
            if hdr is None:
                continue
            t = line.split()
            if len(t) < 5 or not (t[0].isdigit() and t[1].isdigit()):
                continue
            # Z10 -- canopy cover is a percentage
            m = re.search(r'(?<![A-Za-z])CC(?![A-Za-z0-9(])', hdr)
            if m:
                for mm in re.finditer(r'\S+', line):
                    if abs(mm.end() - m.end()) <= 3:
                        v = _num(mm.group())
                        if v is not None and not (-0.5 <= v <= 100.5):
                            bad.append(f'  {f.name}: CC outside 0-100 ({v}) on '
                                       f'{t[0]}/{t[1]}/{t[2]}')
                        break
            if len(bad) > 40:
                return bad + ['  ... (more suppressed)']
    return bad
