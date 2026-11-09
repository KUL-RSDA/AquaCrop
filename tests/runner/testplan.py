"""Read tests/TESTPLAN.md into the case list the run explorer shows.

TESTPLAN.md is the source of truth; this only reformats it. The cases on disk say what
exists, the plan says what is planned: a row is "Tested" once a case of its own exists with
a stored reference. `load_plan()` returns plain data (with the plan's markdown already turned
into escaped HTML), which build_explorer.py writes into the page's data.
"""
from __future__ import annotations

import html
import pathlib
import re

#: the states a plan row can be in, in the words the page uses. Every row is in exactly one.
ST_LABEL = {'built': 'Tested', 'partial': 'Partly tested',
            'covered': 'Covered elsewhere', 'invariant': 'Checked by rule',
            'blocked': 'Blocked', 'unreachable': 'Not testable',
            'todo': 'Not written', 'staged': 'Awaiting freeze'}

ST_HELP = {
    'built': 'A test case of its own. Its output is frozen and compared on every run.',
    'partial': 'The cases named on the row exercise only part of what it describes.',
    'covered': 'No case of its own — the cases named on the row already exercise this exact path.',
    'invariant': 'No case of its own — a rule in group Z checks it on every run instead.',
    'blocked': 'Cannot pass until a defect is fixed. The bug is named on the row.',
    'unreachable': 'No input file can reach it — the value is fixed in the source.',
    'todo': 'Still to write. Nothing is stopping it.',
    'staged': 'The case exists but has no stored output yet. Run freeze.py.',
}

PART_NAMES = {'I': 'Input & option coverage', 'II': 'Kernel coverage',
              'III': 'Version compatibility', 'IV': 'Generated sweeps'}

SEV_ORDER = ('crit', 'high', 'med', 'low', 'harness', 'fixed')
SEV_NAME = {'crit': 'Stops the run', 'high': 'Undefined behaviour',
            'med': 'Wrong or missing output', 'low': 'Minor',
            'harness': 'Test code (already fixed)',
            'fixed': 'Fixed on fix/7.4_fixes_testsuite'}


def md(s: str) -> str:
    s = html.escape(s)
    s = re.sub(r'`([^`]+)`', r'<code>\1</code>', s)
    s = re.sub(r'\*\*([^*]+)\*\*', r'<strong>\1</strong>', s)
    return s.replace('→', '&rarr;').replace('×', '&times;').replace('≤', '&le;').replace('≥', '&ge;')


def covered_by(ex: str) -> list[str]:
    """The case ids after "covered by" (or "partly covered by") in a row's text, up to
    the first ':' or '(': "— covered by A01, C02: why" gives ['A01', 'C02']. A row with
    no case of its own names the ones that exercise it."""
    m = re.search(r'covered by ([^:(]+)', ex)
    ids = re.findall(r'\b[A-Z]{1,2}\d{2}[a-z]?\b', m.group(1)) if m else []
    return list(dict.fromkeys(ids))


def severity_of(meta: str, title: str) -> str:
    """The class of a defect, from its recorded severity line."""
    # whole words only: "the model c-hang-es" is not a hang, and "be-low" is
    # not a low severity. Read the recorded Severity: clause first, since the
    # title often names the symptom of a defect whose severity is elsewhere.
    t = (meta + ' ' + title).lower()

    def has(*words):
        return any(re.search(r'\b' + w + r'\b', t) for w in words)

    if has('harness'):
        return 'harness'
    if has('cosmetic'):
        return 'low'
    if has('hangs?', 'hanging', 'infinite'):
        return 'crit'
    if has('segfaults?', 'crash(es|ed)?', 'aborts?'):
        return 'crit'
    if has('undefined') or 'out-of-bounds' in t or has('out of bounds'):
        return 'high'
    if has('floating-point', 'sigfpe', 'exceptions?'):
        return 'high'
    if re.search(r'severity:\s*low\b', t):
        return 'low'
    return 'med'


def _on_disk(cases_dir: pathlib.Path) -> tuple[dict[str, bool], list[str]]:
    """Case id -> has a stored reference, and the case folders that have one."""
    on_disk: dict[str, bool] = {}
    frozen_dirs: list[str] = []
    for d in sorted(cases_dir.iterdir()) if cases_dir.is_dir() else []:
        if d.is_dir() and (d / 'case.yml').is_file():
            cid = d.name.split('_')[0]
            # a case that must stop with a message has no reference by design
            frozen = ((d / 'OUTP_REF').is_dir()
                      or 'expect_error:' in (d / 'case.yml').read_text())
            if frozen:
                frozen_dirs.append(d.name)
            on_disk[cid] = frozen
            # several plan rows are covered by a lettered family of cases
            # (N09a/N09b for plan row N09); a row counts as built once any
            # member of its family is frozen
            base = re.match(r'^([A-Z]{1,2}\d{2})[a-z]$', cid)
            if base:
                on_disk[base.group(1)] = on_disk.get(base.group(1), False) or frozen
    return on_disk, frozen_dirs


def _blocked(retired: list) -> dict[str, str]:
    """Every case id named in the retired table, expanded from "D02, D08" and "F54a-F54d"
    into the individual ids, mapped to the defect holding it up."""
    blocked: dict[str, str] = {}
    for cases, _inp, bug, _when in retired:
        for part in re.split(r',\s*', cases):
            part = part.strip().strip('`')
            m = re.match(r'^([A-Z]{1,2})(\d{2})([a-z])?[–-]([A-Z]{1,2})?(\d{2})?([a-z])?$', part)
            if m and m.group(3) and m.group(6):          # F54a-F54d
                for c in range(ord(m.group(3)), ord(m.group(6)) + 1):
                    blocked[f'{m.group(1)}{m.group(2)}{chr(c)}'] = bug
                blocked[f'{m.group(1)}{m.group(2)}'] = bug
            elif part:
                blocked[part] = bug
                b = re.match(r'^([A-Z]{1,2}\d{2})[a-z]$', part)
                if b:
                    blocked.setdefault(b.group(1), bug)
    return blocked


def load_plan(tests: pathlib.Path) -> dict:
    src = (tests / 'TESTPLAN.md').read_text()
    on_disk, frozen_dirs = _on_disk(tests / 'cases')

    ret = re.search(r'^## Retired cases$(.+?)(?=^## )', src, re.M | re.S)
    retired = re.findall(r'^\| (\S.*?) \| (.*?) \| (BUG-\d+) \| (.*?) \|$',
                         ret.group(1) if ret else '', re.M)
    blocked = _blocked(retired)
    fix = re.search(r'^## Fixed on the fix branch$(.+?)(?=^## )', src, re.M | re.S)
    fixed = dict(re.findall(r'^\| (BUG-\d+) \| (.+?) \|$', fix.group(1) if fix else '', re.M))

    def live_status(cid: str, planned: str) -> str:
        """Filesystem truth beats the plan."""
        if planned in ('invariant', 'unreachable'):
            return planned            # not a case; the plan is authoritative
        if planned == 'retired' or (planned == 'todo' and cid in blocked):
            return 'blocked'          # a defect is in the way; the plan says which
        if cid in on_disk:
            return 'built' if on_disk[cid] else 'staged'
        return {'built': 'covered'}.get(planned, planned)

    # ------------------------------------------------------------ plan rows, by group
    groups: list[dict] = []
    cur = None
    part = 'I'
    for line in src.splitlines():
        m = re.match(r'^# Part ([IV]+) ', line)
        if m:
            part = m.group(1)
            continue
        m = re.match(r'^### ([A-Z]{1,2})\. (.+?)\s*$', line)
        if m:
            name, anchor = m.group(2), ''
            a = re.search(r'\(`(.+)`\)\s*⭐?\s*$', name)
            if a:
                anchor = a.group(1)
                name = name[:a.start()].strip()
            cur = {'letter': m.group(1), 'name': name.replace('⭐', '').strip(),
                   'anchor': anchor, 'part': part, 'rows': []}
            groups.append(cur)
            continue
        if re.match(r'^\| ID \| Family \|', line):        # the sweep table
            cur = {'letter': 'SW', 'name': 'Generated sweep families',
                   'anchor': 'runner/make_inputs.py', 'part': 'IV', 'rows': []}
            groups.append(cur)
            continue
        m = re.match(r'^\|\s*(SW\d{2})\s*\|(.+?)\|(.+?)\|\s*(\d+)\s*\|\s*(T[0-3])\s*\|\s*\[([x~ ])\]\s*\|\s*$', line)
        if m and cur and cur['letter'] == 'SW':
            cur['rows'].append({
                'id': m.group(1), 'case': md(m.group(2).strip()), 'ex': md(m.group(3).strip()),
                'tier': m.group(5), 'st': {'x': 'built', '~': 'partial', ' ': 'todo'}[m.group(6)],
                'bug': '', 'n': int(m.group(4))})
            continue
        m = re.match(r'^\|\s*([A-Z]{1,2}\d{2}[a-z]?)\s*\|(.+?)\|(.+?)\|\s*(T[0-3])\s*\|\s*\[([x~ i—n])\]\s*\|\s*$', line)
        if m and cur and cur['letter'] != 'SW':
            cur['rows'].append({
                'id': m.group(1), 'case': md(m.group(2).strip()), 'ex': md(m.group(3).strip()),
                'tier': m.group(4),
                'st': live_status(m.group(1),
                                  {'x': 'built', '~': 'partial', ' ': 'todo',
                                   'i': 'invariant', '—': 'retired',
                                   'n': 'unreachable'}[m.group(5)]),
                'bug': blocked.get(m.group(1), ''), 'n': 1,
                'by': covered_by(m.group(3))})

    rows_all = [r for g in groups for r in g['rows']]
    enum = [r for g in groups if g['letter'] != 'SW' for r in g['rows']]
    used = [st for st in ('built', 'blocked', 'covered', 'invariant', 'partial',
                          'unreachable', 'todo', 'staged')
            if any(r['st'] == st for r in enum)]

    # ------------------------------------------------------------------ defects
    # ordered so the worst is first, each saying what it breaks and which cases wait on it
    waiting: dict[str, list[str]] = {}
    for c, bug in blocked.items():
        if re.match(r'^[A-Z]{1,2}\d{2}$', c):
            waiting.setdefault(bug, []).append(c)
    defects = []
    for bid, title, meta in re.findall(r'^### (BUG-\d+) — (.+?)$\n\n\*(.+?)\*', src, re.M | re.S):
        sev = 'fixed' if bid in fixed else severity_of(meta, title)
        note = re.sub(r'\s+', ' ', meta.split('Severity:')[-1]).strip(' .*')
        if bid in fixed:
            note = 'Fixed: ' + fixed[bid]
        defects.append({'id': bid, 'n': int(bid.split('-')[1]), 'title': md(title),
                        'sev': sev, 'note': md(note), 'waiting': sorted(waiting.get(bid, []))})
    defects.sort(key=lambda d: (SEV_ORDER.index(d['sev']), d['n']))

    # ------------------------------------------------------------- observations
    # recorded behaviour that is not a defect to fix
    obs = []
    for m in re.finditer(r'^#{2,3} (O\d+) — (.+?)$\n\n\*?(.*?)\*?\n\n(.+?)(?=\n\n)',
                         src, re.M | re.S):
        body = re.sub(r'\s+', ' ', m.group(4)).strip()
        if len(body) > 340:                       # cut on a word, not mid-token
            body = body[:340].rsplit(' ', 1)[0]
            if body.count('**') % 2:              # never leave a bold half-open
                body = body[:body.rfind('**')].rstrip()
            if body.count('`') % 2:
                body = body[:body.rfind('`')].rstrip()
            body += '…'
        retracted = 'RETRACTED' in m.group(2).upper()
        obs.append({'id': m.group(1), 'title': md(m.group(2).replace('RETRACTED: ', '')),
                    'body': md(body), 'retracted': retracted})
    obs.sort(key=lambda o: int(o['id'][1:]))

    return {
        'groups': groups, 'parts': PART_NAMES,
        'label': ST_LABEL, 'help': ST_HELP, 'used': used,
        'sevname': SEV_NAME, 'defects': defects, 'obs': obs,
        'retired': [{'cases': c, 'input': html.escape(i), 'bug': b, 'when': md(w)}
                    for c, i, b, w in retired],
        'cases': frozen_dirs,
        'counts': {'frozen': len(frozen_dirs), 'rows': sum(r['n'] for r in rows_all),
                   'enum': len(enum), 'gen': sum(r['n'] for r in rows_all) - len(enum),
                   'tiers': {t: sum(r['n'] for r in rows_all if r['tier'] == t)
                             for t in ('T0', 'T1', 'T2', 'T3')}},
    }


if __name__ == '__main__':
    import json
    p = load_plan(pathlib.Path(__file__).resolve().parent.parent)
    st: dict[str, int] = {}
    for g in p['groups']:
        for r in g['rows']:
            st[r['st']] = st.get(r['st'], 0) + 1
    print(json.dumps({**p['counts'], 'states': st, 'defects': len(p['defects']),
                      'obs': len(p['obs']), 'retired': len(p['retired']),
                      'groups': len(p['groups'])}))
