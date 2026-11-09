# AquaCrop test suite

A set of 856 small AquaCrop simulations with their expected output. Run them
after you change the code, and they tell you what your change did to the
results.

The suite is its own branch, `test/test_suite`, and everything it needs is in
the folder `tests/`. The branch also carries the **code** the reference output 
was generated from, in `src/`. `tests/REFERENCE.txt` from which code version the
reference was generated, down to the branch and the commit.
Running the suite with a new code version shows what the edits changed in the output;
section 4 explains how to read such differences.


## In short

You need the suite (`tests/`) and a build of AquaCrop (`src/aquacrop`)

```bash
# Point the test runs at your new build. If you are in your AquaCrop clone.
# Creates a seperate testsuite folder. (do it only once)
git worktree add ../AquaCrop_suite origin/test/test_suite
# build YOUR code.
make -C ./src/
# Go to the testsuite
cd ../AquaCrop_suite
# Run the testsuite.
python3 tests/runner/run_tests.py -j 8 --exe ../AquaCrop/src/aquacrop
```

In your terminal, you'll see the testcases being executed and diagnosed.
If every case passes, your change did not alter any result. If some fail, the
rest of this page explains how to find out whether that was intended.

---

## 1. What you need

- **Python 3** with **PyYAML** (`pip install pyyaml`). This is all the runner
  needs and is available on the default HPC python (or base env).
- A build of AquaCrop at `src/aquacrop` (`make`).
- _(Optional because a webpage can be generated as an alternative)_
  For the results notebook: **pandas**, **numpy** and **matplotlib**, and
  optionally **ipywidgets** for the drop-down menus. The simplest way to get
  them is a Python virtual environment, made once. It needs **Python 3.9 or
  newer**: check with `python3 --version`. If a conda environment is active
  (your prompt starts with `(base)`), `python3` is conda's Python, which may be
  older; run `conda deactivate` first, or start from `/usr/bin/python3`.

  On the VSC clusters, put it in your data folder. `$VSC_DATA` is already set
  to it (`/data/leuven/xxx/vscxxxxx`), so the commands work for everyone as
  they are:

  ```bash
  python3 -m venv $VSC_DATA/venvs/aquacrop-tests
  source $VSC_DATA/venvs/aquacrop-tests/bin/activate
  pip install -r tests/requirements.txt
  python -m ipykernel install --user --name aquacrop-tests --display-name "aquacrop_testing"
  ```

  The last line registers the environment as a Jupyter kernel called
  *aquacrop_testing*. On another machine, use any folder instead of
  `$VSC_DATA/venvs`.

  **Using it in VS Code.** Open `tests/compare_suite.ipynb`, click
  *Select Kernel* at the top right, then *Jupyter Kernel...*, and pick
  *AquaCrop tests*. If it is not in the list yet, reload the window once
  (Ctrl+Shift+P, *Developer: Reload Window*). Alternatively, choose
  *Python Environments...*, then *Enter interpreter path...*, and type the
  full path, for example
  `/data/leuven/xxx/vscxxxxx/venvs/aquacrop-tests/bin/python`
  (VS Code does not expand `$VSC_DATA` there; `echo $VSC_DATA` shows yours).

The runs are small, but there are many. Use several cores: with 36 cores the
whole suite takes about a minute. The stored output is about 70M; a run with
`--keep` adds a few hundred M under `tests/work/`, which git ignores.


## 2. Prepare

The runner needs two things: the folder `tests/`, which it is run from, and a
built `aquacrop`.

The suite is the branch `test/test_suite` of the group repository,
[lbusschaert/AquaCrop](https://github.com/lbusschaert/AquaCrop). The commands below
Check with `git remote -v`. If `origin` is your own fork, add the group repository once
and use its name instead of `origin` everywhere on this page:

```bash
git remote add kul git@github.com:KUL-RSDA/AquaCrop.git
git fetch kul
```

This is your own code or branch you are working and will build form and test.
Now the testsuite can easily be added in another folder. 

```bash
git fetch origin
git worktree add ../AquaCrop_suite origin/test/test_suite   # once
```

Then build your code as usual, and run the suite from its own folder, telling
it which binary to use:

```bash
make -C src   # in your own working folder (-C allows to point to a directory and make)
cd ../AquaCrop_suite
python3 tests/runner/run_tests.py -j 8 --exe ../AquaCrop/src/aquacrop
```

`--exe` takes a relative or an absolute path and is accepted by `run_tests.py`
and `freeze.py` alike. Without it, both use `src/aquacrop` of the folder they
are run from — on the suite branch that is the build its references were frozen
from, which passes every case.

## 3. Run it

From the top of the repository:

```bash
python3 tests/runner/run_tests.py -j 8 \
        --exe ../AquaCrop/src/aquacrop
# it always requires the --exe to test your new build. Also needed in all cases below. 
python3 tests/runner/run_tests.py -j 8           # all cases, 8 at a time
python3 tests/runner/run_tests.py -j 8 G U       # only groups G and U
python3 tests/runner/run_tests.py Q15 F14        # only these cases
python3 tests/runner/run_tests.py -j 8 -q        # only print what goes wrong
python3 tests/runner/run_tests.py -j 8 --keep    # keep the output of passing cases too
python3 tests/runner/run_tests.py -j 8 --rtol 0  # every difference is a failure
python3 tests/runner/run_tests.py -j 8 --ulp 1   # comparing two builds: see below
```

Each case runs in its own folder under `tests/work/`, so running many at once
is safe. The folder of a failing case is always kept, so you can look at its
output. Use `--keep` if you want to plot the results afterwards (section 5):
passing cases are useful to compare against.

At the end you get a summary like

```
pass 853   within-tol 0   known-defect 0   FAIL 3   ERROR 0
```

and a report is written to `tests/work/results.json`. The exit status is 0
only if every case selected passed, so the suite can be used in a script.

### What the results mean

| mark | result | meaning |
|---|---|---|
| `=` | **pass** | the output is the same as the stored reference |
| `~` | **within tolerance** | numbers differ, but by less than the case's tolerance (normally 0.1 %) |
| `x` | **fail** | the output differs, or a check below was broken |
| `!` | **error** | AquaCrop stopped with an error |

The first line of every output file (the date and time of the run) is always
ignored.

### The tolerance

Text (project names, dates, `Tot(1)`) has to match exactly. Numbers may differ
by the case's `rtol`, 0.1 % by default, before the case fails. Such a
difference is never hidden: the case is marked `~`, counted as *within-tol* in
the summary, and the message says how many values differed and by how much.

The same binary on the same machine reproduces its output exactly, so on your
own runs `~` means your change did move numbers, if only slightly. The
tolerance is there for comparisons where equality to the last digit is not a
fair test: another compiler or machine, or another build of the same code, for
example the unoptimised build of section 9, which reorders floating-point
arithmetic. A real change of behaviour moves numbers by whole percent and
fails whatever the tolerance.

To change it for a whole run, use `--rtol` (`--rtol 0` fails on any
difference); to change it for one case, edit `rtol:` in its `case.yml`.

### Expected failures

**Ten cases are meant to fail to run.** They hand AquaCrop something wrong —
a project list with an empty line, a soil file with too many horizons, a
simulation period the climate record does not cover — and AquaCrop has to
refuse it with a message. Their `case.yml` says which message
(`expect_error:`, see section 8), and they pass when the run stops and prints
it. They have no stored output. If one of them starts running "successfully",
or stops with a different message, it fails.

### Invariants (rules)

Besides comparing with the reference, every case is also checked against
rules that must always hold, whatever the code does:

- the water balance closes: what comes in equals what goes out plus the change
  in storage, every day;
- the salt balance closes in the same way;
- no compartment holds more water than its porosity, or less than none;
- percentages stay between 0 and 100, and no output is `NaN` or `Inf`.

So a case can fail even if the reference was never updated: if your change
breaks the water balance, you will know.

### Independent checks

For some cases the expected value is also **recomputed in Python**, by a hand
port of the Fortran that does not depend on any stored reference:

- `runner/soil_check.py`: the compartment sizes and the root zone depth (group F);
- `runner/gdd_check.py`: the daily growing degree days (group C);
- `runner/decade_check.py`: daily values interpolated from ten-day climate
  records (group D).

The explorer reports a mismatch as *independent check disagrees*. Then either
the Fortran or the Python port is wrong, and comparing the two shows which.


## 4. Some cases failed. Now what?

A failing case is not automatically a problem. It means **something changed**,
and you have to decide whether that change is the one you wanted.

1. **Look at what moved.** Open the notebook (section 5), or compare the files
   directly:

   ```bash
   diff tests/cases/Q15_*/OUTP_REF/Q15PRMseason.OUT \
        tests/work/Q15_*/OUTP/Q15PRMseason.OUT
   ```

2. **Ask whether it makes sense.** A fix to capillary rise should move the
   cases with a water table (groups G and U). If cases move that have nothing 
   to do with your change, look closer.

3. **If an error or a broken rule shows up**, your change has introduced a bug.

4. **If the changes are what you intended**, the stored output has to be
   updated so the suite stays in step with the code (see below). Do this on
   the suite branch, and say in the commit message which change moved which
   cases.

### Updating the references

Only after you have checked that the changes are intended:

```bash
python3 tests/runner/freeze.py Q15 G05 -j 8        # only these cases
python3 tests/runner/freeze.py --all --force -j 36 # every case
```

`freeze.py` asks before it writes and refuses to overwrite an existing
reference unless you give `--force`. It takes `--exe` like `run_tests.py`, and
records the build it used in `tests/REFERENCE.txt`. Cases that are meant to
fail to run are skipped: there is nothing to store for them. Then:

```bash
python3 tests/runner/run_tests.py -j 8     # everything should pass now
git add tests/cases tests/REFERENCE.txt
git commit -m "TST: Update test references after <your change>"
```

Write in the commit message which change moved which cases, so the new values
can always be traced back.

If your change deliberately changes **how compartments are sized**, the
expected sizes stored in some `case.yml` files (the `predict:` blocks) and
`tests/runner/soil_check.py` must follow too. The geometry checks will tell
you which cases.


## 5. Analyze the differences

### The run explorer (user friendly)

To look at a run without the notebook, in an interactive web page, build the explorer from it:

```bash
python3 tests/runner/run_tests.py -j 8 --keep   # keep the passing cases too
python3 tests/runner/build_explorer.py          # writes tests/work/explorer/
```

Open `tests/work/explorer/index.html` in a browser; keep the `data/` folder next
to it. GitHub shows `.html` files as source code, not as a page, so to get a link
you can open or share, ask Claude Code to publish the explorer as an artifact. It
gives you a `claude.ai` link, private until you share it from the page's share
menu. After the next run, ask it to publish again to the same link.

<img width="1880" height="710" alt="image" src="https://github.com/user-attachments/assets/e26d74c1-f53a-4da0-be0a-67b5270c4dfe" />

It has five tabs:

| view | what it shows |
|---|---|
| Case list | the test plan: every case, grouped by what it tests, with its status; next to it, the verdict each case got in the run the page shows, which can be older than the plan; then the defects and observations the suite has found |
| Overview | how many cases pass, per group; how the others differ; whether every calendar-mode case is unchanged; a searchable list of all cases |
| Daily | reference against new for one daily variable, one dot per case, run and day; zoom, and click a dot or a ranking row to see that case's time series — every run of the project end to end, with the run boundaries marked (the crop and water variables are always included; Wr is drawn with its FC, PWP and SAT levels) |
| Season | the same for the season totals, including the cases without daily output |
| Case | one case in depth: its description, runs, input files and changed lines, what the run reported, its season totals, and every daily column as a time series, the columns that moved first |

Without `--keep` the passing cases have no working tree, and the explorer can
only show their reference. The page needs the internet only for the plotting
library and the fonts.

The case list is read from `tests/TESTPLAN.md` and the cases on disk each time
the explorer is built, so after adding a case or editing the plan, build it
again.

<img width="1500" height="823" alt="image" src="https://github.com/user-attachments/assets/678acdad-2af3-4f6f-b5f8-bc74e75b6284" />


### Notebook

`tests/compare_suite.ipynb` shows the outcome of a run:

1. **Overview**: which groups have cases that do not pass, and how they fail
   (the run stopped, a balance broke, numbers moved, or only text changed).
2. **Scatter plots**: reference against new value for a set of cases you pick,
   one panel per output variable. Points on the diagonal did not move.
3. **One case over time**: reference and new, the difference underneath, and
   the first day they differ. `plot_case(case, columns="crop")` shows canopy,
   biomass and yield; `columns="water"` shows the soil water, with the root
   zone water at saturation, field capacity and wilting point drawn along it.

Run the suite with `--keep` first, then open the notebook and run it from the
top. To look at a run that sits somewhere else, set the environment variable
`AQUACROP_WORK` to that `work` folder before starting Jupyter.

## 6. Check a case in the Windows GUI

To compare a case with the AquaCrop GUI, you can export the case for Windows:

```bash
python3 tests/runner/export_windows.py Q15
  # bundles the test case Q15
python3 tests/runner/export_windows.py F14 F23 S10 --bundle geometry
  # bundle several cases (here called geometry because that was the main check)
python3 tests/runner/export_windows.py Q15 --data 'D:\AquaCrop\DATA'
  # give the path of your GUI DATA folder
```

This writes `tests/export/Q15/` and `tests/export/Q15.zip` (with `--bundle`,
also one zip holding all the cases). Inside:

- every input file, **renamed after the case** (`Q15.SOL`, `Q15.CRO`, ...), so
  nothing overwrites a file already in the GUI's folder;
- the project `Q15.PRM`, with every path pointing at the GUI's DATA folder
  (`C:\Workdir\Programs\GUI_AC7.3\DATA` unless you give `--data`);
- `Q15.PPn`, the program parameters the case was run with;
- a `README.txt` saying where each file goes, and for soil cases the
  compartment sizes to expect.

All files have Windows line endings. On the Windows side:

1. Copy the input files into the GUI's DATA folder.
2. In the GUI, open the project folder (e.g. `Q15.PRM`), the corresponding data
   should be loaded.
3. Run the project and compare. Either visually within the GUI or export the output.


## 7. What is in `tests/`

| path | what it is |
|---|---|
| `cases/<ID>_<name>/` | one folder per case: `case.yml` (the inputs) and `OUTP_REF/` (the expected output) |
| `assets/` | the input files the cases use (climate, crops, soils, ...) and the scripts that generated them |
| `runner/` | the tools: `run_tests.py`, `freeze.py`, `build_explorer.py`, the checks and the Python re-computations (`*_check.py`) |
| `TESTPLAN.md` | the full plan: every case, what it covers, and the problems found so far; the explorer's case list shows it |
| `REFERENCE.txt` | which build produced the stored references |
| `compare_suite.ipynb` | the results notebook |
| `requirements.txt` | the Python packages the notebook needs |
| `work/`, `export/` | output of your runs and exports; not stored in git |

### Groups

The letters at the start of a case name say what the case is about.

| | | | |
|---|---|---|---|
| **A** | project and run structure | **N** | program parameters |
| **B** | crop type, planting, phenology | **O** | output and reporting |
| **C** | calendar days vs growing degree days | **P** | stress and crop failure |
| **D** | climate input | **Q** | drainage |
| **E** | rainfall and runoff settings | **R** | runoff and infiltration |
| **F** | soil profile, compartments, rooting depth | **S** | soil evaporation |
| **G** | groundwater table | **T** | transpiration and root water uptake |
| **H** | initial conditions | **U** | capillary rise |
| **I** | irrigation | **V** | salt transport |
| **J** | field management | **W** | canopy development |
| **K** | off-season | **X** | biomass, harvest index, yield |
| **L** | salinity | **Y** | stress |
| **M** | growing-season calendar | **Z** | water and salt balance, overall checks |
| **SW** | generated sweeps (one setting varied over its range) | | |

## 8. Adding a case

**Most cases are written by a script.** The generators in `tests/assets/`
(`gen_cases_*.py`) hold one line per case and write its `case.yml`. If your
case belongs to a group one of them produces, add your line there and run the
script, otherwise the next person who runs it will not have your case. Check
with `git status` that it only wrote what you expected.

For a one-off case, the easiest way is to copy a case that is close to what
you want:

```bash
cp -r tests/cases/Q15_drainage_against_an_adjusted_field_capacity \
      tests/cases/Q30_my_new_case
rm -r tests/cases/Q30_my_new_case/OUTP_REF
```

Edit `case.yml` in the new folder:

- `id`: the folder name;
- `stage`: the input files it needs, by name, from `tests/assets/`;
- `project`: dates and which file is used for climate, crop, soil, water
  table, and so on;
- `patch`: change single lines of an input file, as `line number: new text`;
- `daily`: which daily output blocks to write (1 to 8). Blocks 1 (water
  balance) and 2 (crop) are always added, except for the sweeps (`SW`), the
  output-selection cases (`O`), cases that must stop, and a case that sets
  `daily_core: false`;
- `rtol`: the relative tolerance (0.001 is 0.1 %);
- `expect_error`: for a case with wrong input, where AquaCrop must stop. The
  case passes when AquaCrop stops (exit status not 0) and its terminal output
  contains this text. Such a case has no `OUTP_REF`, and `freeze.py` skips it.

Then store its output and check it:

```bash
python3 tests/runner/freeze.py Q30
python3 tests/runner/run_tests.py Q30
```

New input files go in the matching folder under `tests/assets/`.

**The suite never keeps a case that is known to fail.** If a new case shows a
bug in AquaCrop, write the problem down in `TESTPLAN.md` (under *Defects found
by the suite*) and remove the case. It is added back once the bug is fixed. That
way a failing run always means something new.

## 9. Extra checks

| command | what it checks |
|---|---|
| `tests/runner/check_builds.sh fpe` | runs the suite on a build that stops on invalid arithmetic (division by zero, `NaN`) |
| `tests/runner/check_builds.sh o0` | runs the suite on an unoptimised build, to catch results that depend on the compiler |
| `tests/runner/check_builds.sh snan N16a S07` | runs the named cases on a build that stops where an uninitialised value is used |
| `python3 tests/runner/check_equivalence.py` | a project with several runs gives the same result as those runs done separately |

`check_builds.sh` compiles the code twice with other compiler options, runs the
suite against each build, and puts your original `src/aquacrop` back when it is
done, also if it stops half way. It lists every case as it goes, the passing
ones included, and compares with `--ulp 1`: these passes hold one *build*
against another, where a value on a rounding boundary is printed `0.183` by one
and `0.182` by the other. A difference that matters moves whole percent and
still fails. Because it compiles, it needs the code itself,
not just a binary: `--exe` is not enough. By default it builds the `src/` next
to the cases; if the suite is a separate checkout, point it at your working
folder:

```bash
tests/runner/check_builds.sh fpe --src ../AquaCrop
```

It prints which code and which cases it is using, so you can check before it
starts. Mind that these builds use `DEBUG=1` (`-O0 -fcheck=all`), which is
slow: allow more time than for a normal run.

Naming cases at the end runs only those, which is what you want for `snan`:
that build treats every uninitialised real as an error, and AquaCrop leaves
variables unset on purpose in places, so on the whole suite it reports more
than you can read. The backtrace of a case is in `tests/work/<case>/RUN.log`.
