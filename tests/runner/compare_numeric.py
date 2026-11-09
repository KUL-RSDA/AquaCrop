#!/usr/bin/env python3
"""Numeric-tolerant comparison of two AquaCrop output files.

Compares two column-formatted output files token by token. Text tokens (project
names, "Tot(1)", ...) must match exactly. Numeric tokens are allowed to differ by
at most --rtol in RELATIVE terms (default 0.001 = 0.1%), and optionally by --ulp
units in the last printed decimal as well:

    reldiff = |ref - out| / max(|ref|, |out|)

so a genuine phenology shift (which moves values by whole percent) still shows up
as a real difference, while last-digit rounding stays under the bar. When both
values are zero the reldiff is 0; when only one is zero it is 1.0 (a real diff).

Exit codes:
  0  exact match (after skipping header lines)
  1  differences, but all within the relative tolerance
  2  real differences (some token exceeds tolerance / text mismatch)
  3  structural mismatch (line or column count) or I/O error
"""
import argparse
import sys


def last_digit(tok):
    """One unit in the last printed decimal of a token: 0.001 for "0.183".

    None for a token written without decimals. A whole number in this output is a
    count or a code - the day, the growth stage - where a difference of one is a
    difference, not rounding.
    """
    return 10.0 ** -len(tok.split('.')[1]) if '.' in tok else None


def parse_num(tok):
    try:
        return float(tok)
    except ValueError:
        return None


def reldiff(rn, on):
    """Relative difference, safe for zeros."""
    denom = max(abs(rn), abs(on))
    if denom == 0.0:
        return 0.0
    return abs(rn - on) / denom


def main():
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument('ref')
    ap.add_argument('out')
    ap.add_argument('--skip', type=int, default=1,
                    help='header lines to skip in each file (default 1: timestamp)')
    ap.add_argument('--rtol', type=float, default=0.001,
                    help='allowed relative difference per numeric token (default 0.001 = 0.1%%)')
    ap.add_argument('--ulp', type=float, default=0.0,
                    help='also accept a difference of this many units in the last printed '
                         'decimal (default 0: off). A value written to three decimals can '
                         'sit on a rounding boundary, where two builds of the same code '
                         'print 0.183 and 0.182 for the same number; --ulp 1 lets that pass '
                         'while a real difference, which moves whole percent, still fails')
    ap.add_argument('--show', type=int, default=8,
                    help='max offending cells to print (default 8)')
    a = ap.parse_args()

    try:
        with open(a.ref) as f:
            ref = f.read().splitlines()[a.skip:]
        with open(a.out) as f:
            out = f.read().splitlines()[a.skip:]
    except OSError as e:
        print(f"  I/O error: {e}")
        sys.exit(3)

    if len(ref) != len(out):
        print(f"  line count differs: ref={len(ref)} out={len(out)}")
        sys.exit(3)

    any_diff = False
    offenders = []      # (line, col, reftok, outtok, reldiff|None)
    max_rel = 0.0
    n_within = 0        # numeric cells that differ but stay within tolerance

    for i, (rl, ol) in enumerate(zip(ref, out), start=a.skip + 1):
        if rl == ol:
            continue
        any_diff = True
        rt, ot = rl.split(), ol.split()
        if len(rt) != len(ot):
            offenders.append((i, '-', f'{len(rt)} cols', f'{len(ot)} cols', None))
            continue
        for c, (rtok, otok) in enumerate(zip(rt, ot), start=1):
            if rtok == otok:
                continue
            rn, on = parse_num(rtok), parse_num(otok)
            if rn is None or on is None:
                offenders.append((i, c, rtok, otok, None))   # text mismatch
                continue
            rel = reldiff(rn, on)
            max_rel = max(max_rel, rel)
            unit = last_digit(rtok)
            near = (a.ulp > 0 and unit is not None
                    and abs(rn - on) <= a.ulp * unit * (1 + 1e-9))
            if rel > a.rtol and not near:
                offenders.append((i, c, rtok, otok, rel))
            else:
                n_within += 1

    if not any_diff:
        sys.exit(0)

    if not offenders:
        # only sub-tolerance differences
        print(f"  within tolerance: {n_within} cell(s) differ by <= {a.rtol:.3%} "
              f"(max {max_rel:.3%})")
        sys.exit(1)

    for (ln, col, rtok, otok, rel) in offenders[:a.show]:
        extra = f'  (rel {rel:.3%})' if rel is not None else '  (text)'
        print(f"  line {ln} col {col}: {rtok} vs {otok}{extra}")
    if len(offenders) > a.show:
        print(f"  ... (+{len(offenders) - a.show} more cells)")
    sys.exit(2)


if __name__ == '__main__':
    main()
