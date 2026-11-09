# AquaCrop test suite — case plan

Branch `test/test_suite`. A standing regression suite that touches most execution
paths of the Fortran code, so a development branch (GDD refactor, bug fix, LIS
coupling) can be checked against a frozen reference in one command.

Target: ~1000 cases. 732 are enumerated individually below; a further 300 come
from the generated sweep families in Part IV.

Status legend: `[x]` built · `[i]` enforced by an invariant rather than a case
· `[n]` not drivable from inputs, see "Branches unreachable from inputs"
· `[~]` partially covered · `[—]` retired, see the defect · `[ ]` to do.

---

## 0. Conventions

### 0.1 Reference build

References are produced by the **current `main` build** (`src/aquacrop`, AquaCrop
v7.3 as it sits on this branch). The freeze is a deliberate, human-run step:

```
make -C src                       # you run this
tests/runner/freeze.py --case F07 # you run this; writes OUTP_REF/ and records
                                  # the binary's git SHA in tests/REFERENCE.txt
```

Nothing regenerates references automatically. A case whose reference changes is
either a bug or an intended physics change, and either way it needs a human to
say so.

### 0.2 Output storage policy

**Measured on the group F freeze, 2026-09-01** — the earlier estimate here was
5x too low, so these are actuals, not projections:

| Class | Daily output | Sim length | Size/case |
|---|---|---|---|
| Season only | none | 1 season | **1.5 KB** |
| Probe | outputs 3 + 5, 12 compartments | 164 d | **206 KB** |
| Full | all 8 outputs | 164 d | ~450 KB (est.) |

Group F costs 11.5 MB for 57 cases and that is fine. But **1028 cases at the
probe rate would be 207 MB**, which is not. The rules that keep the suite inside
~30 MB:

- **Season output is the default.** A case earns daily output by being *about*
  something only visible day by day. Compartment geometry qualifies, because
  `Out5CompWC` writes one column per compartment; a salt-cell count or a gravel
  fraction does not — those show up in season totals.
- Keep a case to **one season of <= 180 days** unless multi-year behaviour is the
  point (groups A, B perennials, D).
- Never store `.OUT` gzipped: opaque diffs defeat the purpose.

Group F could itself drop to ~8 MB by moving F59-F73 (salt cells, gravel, REW --
all load-time or season-scale quantities) to season-only output. Worth doing on
the next re-freeze, not worth churning working references for.

### 0.3 Case anatomy

```
tests/
  TESTPLAN.md              source of truth for the matrix
  REFERENCE.txt            git SHA + build flags of the binary that froze OUTP_REF
  runner/
    run_tests.py           stage inputs, run ../src/aquacrop, diff OUTP vs OUTP_REF
    freeze.py              same, but writes OUTP_REF instead of comparing
    compare_numeric.py     token-wise numeric comparison with a relative tolerance
    make_inputs.py         materialises generated cases from case.yml
  assets/
    climate/  crops/  soils/  irr/  gwt/  sw0/  off/  man/  cal/  simul/
  cases/
    F07_soil_4m_deep/
      case.yml             description, staged assets, patches, tolerance, exit code
      OUTP_REF/            frozen reference output
```

`case.yml` carries a `patch:` block that rewrites individual lines of a staged
asset, so a case that only changes soil thickness does not need its own `.SOL`:

```yaml
id: F07_soil_4m_deep
desc: 4 m loamy profile, 3 m rooting crop — exercises AdjustSizeCompartments step 4
tier: T1
stage: {cli: Ottawa, cro: MaizeGDD, sol: DEFAULT, ppn: Ottawa}
patch:
  SOL: {9: "    4.00    50.0  30.0  10.0   500.0        100         0     -0.453600  0.837340   Loamy"}
outputs: [season]
rtol: 1.0e-3
```

Case IDs are stable. Never renumber, only append.

### 0.4 Tiers

| Tier | Meaning | When run |
|---|---|---|
| T0 | Frozen byte-exact regression, must never change | every commit |
| T1 | Core physics/feature coverage, 1e-3 relative tolerance | every commit |
| T2 | Broad option coverage | pre-merge |
| T3 | Edge cases, error paths, robustness | nightly / on demand |

### 0.5 Hard limits worth knowing

From `global.f90`: `max_SoilLayers = 5`, `max_No_compartments = 12`,
default `CompDefThick = 0.10 m`. A profile deeper than 1.2 m therefore cannot be
tiled by default-thickness compartments, and `AdjustSizeCompartments` has to
regrade them. Much of group F is built around that arithmetic.

---

# Part I — Input and option coverage

### A. Project & run structure  (`startunit.F90`, `project_input.f90`)

| ID | Case | Exercises | Tier | St |
|---|---|---|---|---|
| A01 | Ottawa 3-run PRM, perennial alfalfa GDD | current `testcase/`, full reference | T0 | [x] |
| A02 | Season 1 only, as a `.PRO` single-run project | `typeproject_typepro` | T1 | [ ] |
| A03 | PRM with 2 runs | multi-run loop | T1 | [ ] |
| A04 | PRM with 10 runs over the 3 climate years | long multi-run stability | T2 | [ ] |
| A05 | `ListProjects.txt` absent | auto-discovery via `grep -E ".*.PR[O,M]$"` | T2 | [ ] |
| A06 | `ListProjects.txt` listing a PRO **and** a PRM | mixed project loop | T2 | [ ] |
| A07 | `ListProjects.txt` empty | "does not contain ANY project file" branch | T3 | [ ] |
| A08 | Listed project file missing on disk | error branch → `ListProjectsLoaded.OUT` | T3 | [ ] |
| A09 | Project file referencing a missing `.CRO` | load failure reporting | T3 | [ ] |
| A10 | Project file referencing a missing `.SOL` | load failure reporting | T3 | [ ] |
| A13 | Sim period starts before the cropping period | `AdjustSimPeriod`, pre-season balance | T1 | [~] |
| A14 | Sim period ends after the cropping period | post-season evaporation/drainage — partly covered by R14: its simulation runs a month past the crop, but it is the only case that does | T1 | [~] |
| A15 | Sim period == cropping period exactly | `DetermineLinkedSimDay1` | T1 | [ ] |
| A16 | Sim period one day longer at each end | off-by-one at both boundaries | T3 | [ ] |
| A17 | Run crossing a calendar-year boundary | day-number arithmetic | T1 | [~] |
| A18 | Run inside a leap year (2016) | `DetermineDate`/`DetermineDayNr` Feb 29 | T1 | [~] |
| A19 | Run starting on 29 Feb 2016 | leap-day start | T3 | [ ] |
| A20 | Run of exactly 1 day | degenerate period | T3 | [ ] |
| A21 | Perennial, year 1 (seeding) vs year >1 | `AdjustYearPerennials`, `Sown1stYear` | T1 | [~] |
| A22 | Perennial run 2 with a *different* crop file | crop switch between runs | T2 | [ ] |
| A23 | Runs in non-chronological order in the PRM | run ordering assumptions | T3 | [ ] |
| A24 | Two runs with a gap between them | non-contiguous sim periods | T2 | [ ] |

### B. Crop type, planting & phenology  (`LoadCrop`, `DetermineLengthGrowthStages`)

| ID | Case | Exercises | Tier | St |
|---|---|---|---|---|
| B01 | subkind = Vegetative | `subkind_Vegetative`, HI build-up from sowing | T1 | [ ] |
| B02 | subkind = Grain | `subkind_Grain`, flowering block | T1 | [ ] |
| B03 | subkind = Tuber | `subkind_Tuber` | T1 | [ ] |
| B04 | subkind = Forage (alfalfa) | `subkind_Forage` — covered by A01 | T0 | [x] |
| B05 | Planting = sown from seed | `plant_Seed`, germination check — covered by A01, C02 | T1 | [x] |
| B06 | Planting = transplanted | `plant_Transplant`, CCo at transplant | T1 | [ ] |
| B07 | Planting = regrowth | `plant_Regrowth`, CCini from re-growth — partly covered by A01: runs 2 and 3 of the Ottawa project are re-growth years | T1 | [~] |
| B08 | Transplanted crop with a long establishment lag | days to recovered transplant | T2 | [ ] |
| B09 | Annual crop, not perennial | non-perennial branch throughout | T1 | [ ] |
| B10 | Determinant crop (linked to flowering) | `Crop_DeterminancyLinked = true` | T2 | [ ] |
| B11 | Indeterminant crop | `Crop_DeterminancyLinked = false` | T2 | [ ] |
| B12 | Flowering length 0 (no flowering stage) | degenerate flowering window | T3 | [ ] |
| B13 | Flowering starting on day 1 of the cycle | boundary of the flowering window | T3 | [ ] |
| B14 | Crop with a premature-end date set | `DayNr Premature end` for annuals | T2 | [ ] |
| B15 | Premature end before maturity | early termination | T2 | [ ] |
| B16 | Perennial: fixed dormancy onset & end dates | `GenerateOnset = .false.` — partly covered by A01: the standalone takes these dates from the .PRM; the .CRO dormancy block is read but never used (see B17–B20 under retired) | T1 | [~] |
| B23 | Perennial self-thinning, 9-year CCx decline | years-to-90% CCx, shape factor | T2 | [ ] |
| B25 | Crop with CGC so large CC closes in days | rapid canopy closure | T3 | [ ] |
| B26 | Crop with CCx = CCo (no expansion) | degenerate canopy growth | T3 | [ ] |

> B24 (a perennial in its 12th year) was removed on 2026-09-22: it was one
> cropping period of eleven years, which is not a physical set-up, since a
> perennial runs one season per project run. B23 covers the self-thinning decline.

### C. Crop-cycle mode: GDD vs calendar days  ⭐ priority for the GDD refactor

| ID | Case | Exercises | Tier | St |
|---|---|---|---|---|
| C01 | Annual crop, `modeCycle_CalendarDays` | calendar-day path end to end | T1 | [ ] |
| C02 | Annual crop, `modeCycle_GDDays` | GDD path end to end | T1 | [ ] |
| C03 | Perennial, GDD (alfalfa) | current baseline | T0 | [x] |
| C04 | Perennial, calendar days | perennial × calendar-day combination | T1 | [ ] |
| C05 | Same crop authored both ways, same season | GDD/calendar equivalence at the margin | T1 | [ ] |
| C06 | GDD method 1 (no Tmax/Tmin adjustment) | `DegreesDay` case 1 | T1 | [ ] |
| C07 | GDD method 2 (Tbase adjustment first) | `DegreesDay` case 2 | T1 | [ ] |
| C08 | GDD method 3 (default) | `DegreesDay` default branch | T0 | [x] |
| C09 | GDD method sweep on an identical season | isolates the three formulations | T1 | [ ] |
| C10 | Tmax < Tbase for a long stretch | zero-GDD days, stalled phenology | T1 | [ ] |
| C11 | Tmin > Tupper for a stretch | capping at Tupper | T2 | [ ] |
| C12 | Tmin < Tbase < Tmax (method 2 vs 3 diverge) | the case the methods disagree on | T1 | [ ] |
| C13 | Tbase == Tupper | degenerate temperature window | T3 | [ ] |
| C14 | Crop needing **more GDD than the record supplies** | truncation, no look-ahead | T1 | [ ] |
| C15 | Crop needing exactly the GDD available | boundary of the record | T1 | [ ] |
| C16 | Sim period ends mid-cycle | unfinished season output | T1 | [ ] |
| C17 | Crop day 1 == last day of the temperature record | T-record boundary | T3 | [ ] |
| C18 | Cropping period entirely outside the T record | `AdjustClimRecordTo` / defaults | T3 | [ ] |
| C19 | Cropping period starting one day before the record | leading-edge clipping | T3 | [ ] |
| C20 | No `.Tnx` — constant default Tmin/Tmax from PPn | `TemperatureFile == '(None)'` | T1 | [ ] |
| C21 | No `.Tnx`, GDD crop | GDD from constant 12/28 °C defaults | T1 | [ ] |
| C23 | GDD-based stage lengths (emergence → maturity) | `GDDaysTo*` conversions — covered by C02, A02 | T1 | [x] |
| C24 | Calendar→GDD conversion of a crop file on load | `DetermineLengthGrowthStages` | T1 | [ ] |
| C25 | GDD→calendar conversion on load | reverse conversion | T1 | [ ] |
| C26 | GDD crop whose stages sum past `GDDaysToHarvest` | inconsistent crop file | T3 | [ ] |
| C27 | Perennial GDD across a dormancy gap | GDD accumulation reset over winter — partly covered by A01: runs 2 and 3 start in November and pass the winter before growth restarts | T1 | [~] |
| C28 | Perennial `SumCalendarDays` walk over the T record | remaining T-record read in perennials — partly covered by A01: the only perennial in GDD mode over a multi-year record | T1 | [~] |
| C29 | GDD crop with `GDtranspLow` binding | minimum GDD for full transpiration | T2 | [ ] |
| C30 | Cuttings scheduled by GDD interval | `TimeCuttings_IntGDD` × GDD mode — covered by J33: AlfOttawaGDD is GDD mode and the .MAN already generates on a GDD interval | T1 | [x] |

### D. Climate input  (`LoadClim`, `LoadClimate`, `climprocessing.f90`)

> **The two 10-daily readers do different things.** `GetDecadeEToDataSet`
> (`climprocessing.f90:325`) turns each decade into a piecewise-linear daily
> curve through three control points derived from the previous, current and next
> decade values, so the series is smooth across decade boundaries and its mean
> over the decade returns the decade value. `GetDecadeRainDataSet` (`:515`)
> simply spreads the decade total flat (`Param = C/ni`). So a decadal ETo record
> holds means and a decadal rain record holds totals. `runner/decade_check.py`
> ports both and D04/D06 assert against it.

| ID | Case | Exercises | Tier | St |
|---|---|---|---|---|
| D01 | Daily Tnx / ETo / PLU | `datatype_daily` — covered by A01 | T0 | [x] |
| D02 | 10-daily Tnx | `datatype_decadely` + interpolation | T1 | [ ] |
| D03 | Monthly Tnx | `datatype_monthly` + interpolation | T1 | [ ] |
| D04 | 10-daily ETo | decadal ETo splitting | T1 | [ ] |
| D05 | Monthly ETo | monthly ETo splitting | T1 | [ ] |
| D06 | 10-daily rain | decadal rain → daily | T1 | [ ] |
| D07 | Monthly rain | monthly rain → daily | T1 | [ ] |
| D08 | Mixed: daily T, monthly ETo, 10-daily rain | independent record types | T2 | [ ] |
| D09 | Record starting on day 11 of a month | 10-daily records starting mid-month | T2 | [ ] |
| D10 | Record starting on day 21 of a month | third decade start | T2 | [ ] |
| D11 | Decadal record spanning a leap February | 29-day decade handling | T2 | [ ] |
| D12 | Monthly record spanning a leap February | 29-day month | T2 | [ ] |
| D13 | `.CLI` with `(None)` for rain | no-rain default | T2 | [ ] |
| D14 | `.CLI` with `(None)` for ETo | default ETo path | T2 | [ ] |
| D15 | `.CLI` with `(None)` for temperature | pairs with C20 | T2 | [ ] |
| D16 | No `.CLI` at all | all-default climate | T2 | [ ] |
| D17 | Record not linked to a year (first year 1901) | year-agnostic climate | T2 | [ ] |
| D18 | Year-agnostic record used in a 2016 run | year mapping | T2 | [ ] |
| D19 | Sim period starts before the climate record | must stop with a message (BUG-24) | T3 | [x] |
| D20 | Sim period ends after the climate record | record exhaustion | T3 | [ ] |
| D21 | Sim period entirely outside the record | must stop with a message (BUG-16) | T3 | [x] |
| D22 | Crop year shifted onto the climate file | `AdjustCropYearToClimFile` | T2 | [ ] |
| D23 | Single-day climate record | degenerate record — must stop with a message (BUG-6, BUG-16) | T3 | [ ] |
| D24 | CO2 = MaunaLoa | interpolation in the CO2 record — covered by A01 | T0 | [x] |
| D25 | Constant CO2 file (single value) | flat CO2 | T2 | [ ] |
| D26 | Simulation year before the CO2 record | leading extrapolation | T3 | [ ] |
| D27 | Simulation year beyond the CO2 record | trailing extrapolation | T3 | [ ] |
| D28 | CO2 at 369.41 ppm (the reference concentration) | no CO2 adjustment of WP | T1 | [ ] |
| D29 | A run naming no CO2 file | `(None)` falls back to the default record | T2 | [x] |
| D30 | A run naming none after one that did | whether the default is resolved per run | T1 | [x] |
| D31 | The default CO2 record absent, the run naming its own | the start-up read of `SIMUL/MaunaLoa.CO2` (BUG-25) | T2 | [x] |

### E. Rainfall configuration & runoff settings

| ID | Case | Exercises | Tier | St |
|---|---|---|---|---|
| E01 | Effective rain method = full | `EffectiveRainMethod_full` | T2 | [ ] |
| E02 | Effective rain method = USDA-SCS | `EffectiveRainMethod_usda` — covered by D06, D07 | T1 | [x] |
| E03 | Effective rain method = percentage | `EffectiveRainMethod_percentage` | T2 | [ ] |
| E04 | % effective rainfall 50 / 70 / 100 | PPn `PercEffRain` | T2 | [ ] |
| E05 | Showers per decade 1 / 2 / 5 / 10 | PPn `ShowersInDecade` | T2 | [ ] |
| E06 | Daily-rainfall estimation from decadal, USDA on | `Method_usda` for record splitting | T2 | [ ] |
| E07 | Daily-rainfall estimation from decadal, off | `Method_full` | T2 | [ ] |
| E08 | Soil-evaporation reduction parameter for decadal rain | PPn `EvapZmax`-adjacent parameter | T2 | [ ] |
| E09 | CN adjusted to antecedent moisture class, on | `DetermineCNIandIII` — covered by A01 | T1 | [x] |
| E10 | CN adjustment off | `SimulParam_CNcorrection = false` | T1 | [ ] |
| E11 | CN 30 (low runoff) | CN sensitivity floor | T2 | [ ] |
| E12 | CN 46 (Ottawa baseline) | covered by A01 | T0 | [x] |
| E13 | CN 90 (high runoff) | CN sensitivity ceiling | T1 | [ ] |
| E14 | `Soil.PAR` runoff depth 0.10 vs 0.30 m | `SimulParam_RunoffDepth` | T2 | [ ] |
| E15 | Zero rainfall over the whole season | dry extreme | T2 | [ ] |
| E16 | `CN_default` derived from infiltration rate | `DetermineCN_default` | T2 | [ ] |

### F. Soil profile, compartments & rooting geometry  ⭐

**68 of these are built.** `assets/gen_soils.py` writes 38 profiles and
`assets/gen_cases_F.py` writes the 58 geometry and soil-parameter cases (the
others come from `gen_cases_rest.py`, `gen_cases_last.py` and
`gen_cases_final.py`); `runner/soil_check.py` is an
independent hand-port of the compartment arithmetic, so each case asserts the
count the *source* implies rather than only diffing a freeze. This check was
validated against the A01 reference before any group-F run: it predicts
12 compartments totalling 3.00 m for Ottawa.SOL with a 3 m crop, and the frozen
`OttawaPRMday.OUT` header reads `WC(3.00)` (3.05 m before the compartment fix
537d957, which the case descriptions of F14 and F26 still quote).

Branch coverage of the six-way `AdjustCompartments` guard: **B3** F01&ndash;F13,
**B4** F14&ndash;F26, **B5** F39/F43, **B6** F35&ndash;F44, **B2** F27 (two runs,
`KeepSWC`). B1, the `KeepSWC` arm that extends the profile, has no case of its own.

`DetermineNrandThicknessCompartments` tiles the profile with `CompDefThick`
(0.10 m) slices, capped at `max_No_compartments = 12` — so a profile deeper than
1.20 m is initially only represented down to 1.20 m. `AdjustCompartments`
(`run.f90:6758`) then has a six-way decision tree, and `AdjustSizeCompartments`
either extends the compartment count (step 3) or regrades all twelve with
`fAdd = (CropZx/0.1 − 12)/78` (step 4). The Ottawa baseline (1.50 m soil, 3.00 m
alfalfa roots, no restrictive layer) only exercises one of those six paths.

**Step 4 is computed in single precision, and the case F23 depends on it.**
With the switch to single precision (2026-10-15), `soil_check.py` follows step 4
operation by operation in single precision. For a crop Zrmax of 3.15 m (F23),
`fAdd` is exactly 0.25 in single precision but 0.24999999999999994 in double, so
`0.1·(1 + i·fAdd)·20` lands on exact halves of a 0.05 m step and `roundc`
rounds them to the even neighbour (2.5 → 2, 3.5 → 4, 5.5 → 6), except
6.50000048, which is just outside the tie window and rounds up. In double every
one of them sat just below the half and rounded down. The single-precision
regrade is `0.10 0.15 0.20 0.20 0.20 0.25 0.30 0.30 0.35 0.35 0.40 0.40` (3.20 m),
trimmed by one 0.05 m step to 3.15 m. The trim needed `global.f90:7190` to test
`TotDepthC − 0.05 + ac_zero_threshold` instead of `TotDepthC − 0.04999999`: the
1e-8 margin is below single precision at 3 m, so the overshoot was never removed.
The double-precision Pascal GUI keeps the old layout for this depth.

**`CompDefThick` is not testable from inputs.** It is set once, to 0.10 m, at
`initialsettings.f90:223`, and no input file exposes it. The three cases that
varied it (and N24) have been removed; changing it needs a rebuild. Everything
below assumes 0.10 m, which is what makes 1.20 m the ceiling a default tiling
can reach.

**Every group F case also bounds and reconstructs its profile.** No compartment
may hold more water than its layer's porosity or less than none -- checked for
every compartment on every day, about 100 000 comparisons across the group, with
no violation. Both checks are discriminating: forcing one compartment to
99.9 vol% is caught by the bound (naming the compartment and its SAT) and
independently by the reconstruction below (a 70.9 mm discrepancy on that day).

**Every group F case also reconstructs its profile.** `Out5CompWC` reports each
compartment in vol% **of the soil matrix**, so the profile depth in mm is

    sum_i  WC_i/100 * thickness_i * 1000 * (1 - GravelVol_i/100)

with `GravelVol` from `FromGravelMassToGravelVolume` (`global.f90:1551`). The
check ties the reported water content to the thickness vector *predicted by soil_check.py*, so a
geometry that differed from the prediction fails here even when the compartment
count matches. Verified on all 52 applicable cases.

The gravel term is not optional: without it the 60 %-gravel case F69 is out by
**168 mm**, and 15 / 30 / 60 % gravel by mass give 8.7 / 18.8 / 44.8 % by volume.
That the residual then drops to 0.17 mm is what establishes the vol% is per unit
matrix rather than per unit bulk volume.

**F.1 — Compartment tiling** (`DetermineNrandThicknessCompartments`)

| ID | Case | Exercises | Tier | St |
|---|---|---|---|---|
| F01 | Soil 0.30 m | 3 compartments × 0.10, exact tiling | T1 | [x] |
| F02 | Soil 0.55 m | 5 × 0.10 + 1 × 0.05, ragged last compartment | T1 | [x] |
| F03 | Soil 1.20 m exactly | exactly 12 compartments, both exit conditions true | T1 | [x] |
| F04 | Soil 1.25 m | 12 compartments = 1.20 m, 0.05 m of profile unrepresented | T2 | [x] |
| F05 | Soil 1.50 m (Ottawa) | 0.30 m unrepresented before adjustment | T0 | [x] |
| F06 | Soil 4.00 m (DEFAULT.SOL) | 2.80 m unrepresented before adjustment | T2 | [x] |
| F07 | Soil 0.05 m (thinner than one compartment) | single sub-default compartment (BUG-4) | T3 | [x] |
| F11 | Layer boundaries not on compartment boundaries (0.13/0.27/0.41) | compartment→layer mapping | T1 | [x] |
| F12 | Layer thinner than one compartment (0.04 m top layer) | a compartment spanning two layers | T2 | [x] |

**F.2 — Compartment expansion** (`AdjustCompartments`, `AdjustSizeCompartments`)

| ID | Case | Exercises | Tier | St |
|---|---|---|---|---|
| F13 | Zrx ≤ TotDepth, no restrictive layer | branch 3: no adjustment at all | T1 | [x] |
| F14 | Soil 1.50 m, Zrx 3.00 m, no restrictive layer | branch 4: step-4 regrade, fAdd = 0.2308 | T0 | [x] |
| F15 | Soil 0.30 m (3 comps), Zrx 1.00 m | branch 4 via step 3: extend 3 → 10 compartments | T1 | [x] |
| F16 | Soil 0.30 m, Zrx 1.20 m | step 3 reaches exactly 12 compartments | T1 | [x] |
| F17 | Soil 0.30 m, Zrx 1.30 m | step 3 caps at 12, step 4 then regrades | T1 | [x] |
| F18 | Soil 0.60 m, Zrx 0.60 m | step 3 entered but nothing to add | T2 | [x] |
| F19 | Zrx 1.20 m exactly (== 12 × CompDefThick) | the step-3/step-4 boundary | T1 | [x] |
| F20 | Zrx 1.21 m, one millimetre past the 1.20 m boundary | step 4 fires, then `loop2` grows one 0.05 m step | T1 | [x] |
| F21 | Zrx 1.25 m | step-4 regrade with the smallest useful `fAdd`, one grow step | T2 | [x] |
| F22 | Zrx 2.18 m: the regrade overshoots | the shrink branch, one 0.05 m step removed | T1 | [x] |
| F23 | Zrx 3.15 m: `fAdd` exactly 0.25 | regrade on exact halves of 0.05 m, then one shrink step (single precision, see above) | T1 | [x] |
| F24 | Zrx 6.00 m (`fAdd` large) | extreme regrade, thickest compartments | T3 | [x] |
| F25 | Soil 0.40 m, Zrx 3.00 m | step 3 extends, then step 4 regrades, in the same call | T1 | [x] |
| F26 | Soil 4.00 m, Zrx 3.00 m | a deep profile regraded to 3.00 m | T2 | [x] |
| F27 | Multi-run `KeepSWC`, `ConstZrx ≤ TotDepth` | branch 2: no adjustment | T2 | [x] |
| F28 | Multi-run `KeepSWC` with differing Zrx per run | `CheckForKeepSWC` picking the max | T1 | [x] |
| F29 | Expansion with `IniSWC_AtFC` true | `ResetSWCToFC` after the regrade — covered by F14 | T1 | [x] |
| F30 | Expansion with `IniSWC_AtFC` false (`.SW0` given) | `AdjustThetaInitial` remap instead | T1 | [x] |
| F31 | Expansion with a groundwater table present | `CalculateAdjustedFC` after the regrade | T1 | [x] |
| F32 | Zrx differing from TotDepth by < 1 mm | `roundc(·*1000)` comparison boundary | T3 | [x] |

**F.3 — Restrictive and impermeable layers** (`ZrAdjustedToRestrictiveLayers`, `RootMaxInSoilProfile`)

| ID | Case | Exercises | Tier | St |
|---|---|---|---|---|
| F34 | Penetrability 100 % throughout | no restriction, `Zmax == ZmaxCrop` — covered by A01 | T0 | [x] |
| F35 | Impermeable layer (Penetrability 0) at 0.30 m | hard stop, `Penetrability == 0` exit | T1 | [x] |
| F36 | Impermeable layer at 0.60 m, 3 layers | stop in a middle layer | T1 | [x] |
| F37 | Penetrability 0 in **layer 1** | degenerate `ZrOUT = 0` | T3 | [x] |
| F38 | Penetrability 0 in the **last** layer | the `layi == TheNrSoilLayers` short-circuit | T2 | [x] |
| F39 | Penetrability 50 % in one layer | partial penetration arithmetic | T1 | [x] |
| F40 | Penetrability graded 100 / 50 / 25 / 0 over 4 layers | the full walk, one layer at a time | T1 | [x] |
| F41 | Penetrability 25 %, roots stopping mid-layer | `ZrTest ≤ Zsoil` exit | T1 | [x] |
| F42 | Penetrability < 100 in a layer *below* Zrmax | the `Zsoil < ZmaxCrop` guard — no restriction | T2 | [x] |
| F43 | Penetrability 99 % (barely restrictive) | rounding at the `< 100` test | T3 | [x] |
| F44 | Restrictive layer inside the evaporation layer (0–0.30 m) | interaction with `EvapZmax` | T2 | [x] |

**F.4 — Rooting depth vs profile**

| ID | Case | Exercises | Tier | St |
|---|---|---|---|---|
| F45 | Zrmax > soil depth (3 m crop, 0.40 m soil) | root depth clipped to the profile | T1 | [x] |
| F46 | Zrmin > soil depth | clipping at emergence, not just maturity | T2 | [x] |
| F47 | Zrmin == Zrmax (no root expansion) | degenerate root growth | T3 | [x] |
| F48 | Zrmax exactly == soil depth | boundary | T2 | [x] |
| F49 | Shallow soil + shallow-rooted crop | both within one compartment | T2 | [x] |
| F50 | Root expansion shape factor −6 vs 15 vs 25 | `rootunit.f90` expansion curve | T2 | [x] |
| F51 | Root expansion limited by water stress | shape factor for stress on expansion | T1 | [x] |
| F52 | Max root-zone expansion rate 1 vs 5 cm/day | PPn cap on daily expansion | T2 | [x] |

**F.5 — Hydraulic properties & layer count**

| ID | Case | Exercises | Tier | St |
|---|---|---|---|---|
| F53 | 1 horizon | baseline — covered by A01 | T0 | [x] |
| F54 | 3 horizons | multi-layer water balance | T1 | [x] |
| F55 | 5 horizons (`max_SoilLayers` boundary) | maximum supported layer count | T1 | [x] |
| F56 | 6 horizons declared in the file | overflow of `max_SoilLayers` — must stop with a message (BUG-2) | T3 | [x] |
| F57 | Coarse over fine (sand over clay) | perched water, infiltration limit at a boundary | T1 | [x] |
| F58 | Fine over coarse (clay over sand) | capillary barrier | T1 | [x] |
| F59 | Ksat 1200 mm/day (Ottawa) | `SCP1 = 2` salt cells | T0 | [x] |
| F60 | Ksat 500 mm/day | `SCP1 = 4` salt cells | T1 | [x] |
| F61 | Ksat 330 mm/day | `SCP1 = 5` salt cells | T1 | [x] |
| F62 | Ksat 200 mm/day | `SCP1 = 7` salt cells | T1 | [x] |
| F63 | Ksat 112 mm/day (the `≤ 112` boundary) | `SCP1 = 11` salt cells | T1 | [x] |
| F64 | Ksat 50 mm/day | `SCP1 = 11`, slow drainage | T1 | [x] |
| F65 | Ksat 5 mm/day (near-impermeable) | waterlogging, `tau` floor | T1 | [x] |
| F66 | Different Ksat per layer → different `SCP1` | per-layer salt-cell counts (2, 7, 11) | T1 | [x] |
| F67 | Gravel 15 % by mass | `GravelVol`, effective water holding | T1 | [x] |
| F68 | Gravel 30 % by mass | `GravelVol`, effective water holding | T1 | [x] |
| F69 | Gravel 60 % by mass | `GravelVol`, effective water holding | T2 | [x] |
| F70 | REW 3 mm | a short stage-I evaporation | T2 | [x] |
| F71 | REW 15 mm | a long stage-I evaporation | T2 | [x] |
| F72 | SAT − FC of 3 vol% (poorly drained) | narrow drainable porosity | T2 | [x] |
| F73 | FC − WP of 3 vol% (low TAW) | narrow available water | T2 | [x] |

---

## Defects found by the suite

**The suite does not carry cases that are known to fail.** Findings are written
up here and the case is removed, so a red run always means a real regression.
(`run_tests.py` still supports a `known_defect:` marker for quarantining
something mid-investigation; no case uses it.)

### BUG-1 — legacy `.SOL` files fail on a token count, not a layer count

*Found by F54, characterised by F54a–F54d, 2026-09-01. Severity: backwards
compatibility. **Recorded, not fixed** — the suite now uses current-format files,
and F54 was repointed at a v7.3 restatement of the same three-horizon geometry.*

Both legacy read paths in `LoadProfile` expect a `<blank>` spacer column before
the layer description, and neither tolerates its absence:

| file version | read at | values wanted | record without a spacer supplies | |
|---|---|---|---|---|
| `v < 4.0` | `global.f90:7756` | 7 — `thickness SAT FC WP Ksat <blank> description` | 6 | **fails** |
| `v 4.0–5.x` | `global.f90:7772` | 9 — `… CRa CRb <blank> description` | 8 | **fails** |
| `v >= 6.0` | `global.f90:7793` | 10 — no `<blank>` | 10 | ok |

When a value is missing, Fortran list-directed input continues into the
following record to find it, walks through the remaining layer lines, and
reaches EOF:

    At line 7757 of file global.f90
    Fortran runtime error: End of file

`YoloClayLoam6.SOL`, a real v3.0 file, hits this. The failure turns on
something nobody would think mattered — whether the description happens to be
**one word or two**: one word gives six tokens and dies, two words give seven
and parse (with the description itself mangled into the spacer).

The probes settle it, and rule out layer count as a factor:

| case | file | tokens | result |
|---|---|---|---|
| F54a | v3.0, 1 layer, spacer | 7 | loads |
| F54b | v3.0, **3 layers**, spacer | 7 | loads |
| F54c | v3.0, 1 layer, no spacer, one-word description | 6 | **fails** |
| F54d | v3.0, 1 layer, no spacer, **two-word** description | 7 | loads |
| F54e | v5.0, 1 layer, spacer | 9 | to run |
| F54f | v5.0, 1 layer, no spacer | 8 | to run — expected to fail |

**Fix:** at `global.f90:7756` and `:7772`, read the record into a string and take
the first five (or seven) numbers, letting the remainder be the description,
instead of demanding a token whose presence is a formatting accident.

**To revive the cases after fixing:** the probe soils are still generated
(`V30_1L_spacer`, `V30_3L_spacer`, `V30_1L_nospacer`, `V30_1L_twoword`,
`V45_1L_spacer`, `V45_1L_nospacer` in `assets/gen_soils.py`). Re-add the six
rows to `RETIRED` in `assets/gen_cases_F.py` as F54a–F54f, regenerate, freeze.

### BUG-2 — a `.SOL` declaring more than five horizons segfaults

*Found 2026-09-01. Severity: robustness — malformed input. **Recorded only** —
the case has been removed from the suite, since it cannot pass until the model
changes.*

`LoadProfile` loops `do i = 1, GetSoil_NrSoilLayers()` (`global.f90:7753`)
writing into `soillayer`, dimensioned `max_SoilLayers = 5`, with no check that
the declared count fits. A profile claiming six horizons writes out of bounds:

    Program received signal SIGSEGV: Segmentation fault - invalid memory reference.

Malformed input should be reported, not dereferenced. Note that F55 — five
horizons, the boundary — loads fine, so the limit itself works; only exceeding
it is unguarded.

**Fix:** bound-check the declared horizon count against `max_SoilLayers` after
reading it at `global.f90:7746`, and stop with a message rather than looping
past the array.

**To revive the case after fixing:** `LAYERS_6.SOL` is still generated. Re-add
the `F56` row from `RETIRED` in `assets/gen_cases_F.py`, regenerate, freeze —
and decide what the expected behaviour is (a clean error exit, which the case
would assert via `expect_exit`, or clamping to 5 with a warning).

---

### G. Groundwater table  (`LoadGroundWater`, `CalculateAdjustedFC`, `HorizontalInflowGWTable`)

| ID | Case | Exercises | Tier | St |
|---|---|---|---|---|
| G01 | No `.GWT` file | undefined water table — covered by A01 | T0 | [x] |
| G02 | `.GWT` present, mode 0 (no table) | mode-0 branch, `ConstGwt = true` | T2 | [ ] |
| G03 | Constant GWT at 3.0 m (below the profile) | present but never reached | T2 | [ ] |
| G04 | Constant GWT at 2.0 m | `ConstGwt = true`, FC adjustment deep | T1 | [ ] |
| G05 | Constant GWT at 1.0 m | FC adjustment mid-profile | T1 | [ ] |
| G06 | Constant GWT at 0.4 m (shallow) | strong FC adjustment, capillary rise | T1 | [ ] |
| G07 | Constant GWT at 0.0 m (at the surface) | degenerate depth | T3 | [ ] |
| G08 | GWT above the soil surface (negative depth) | `Zi ≥ DepthGWTmeter` for every compartment | T3 | [ ] |
| G09 | Variable GWT, 2 observations | linear interpolation between them | T1 | [ ] |
| G10 | Variable GWT, 12 monthly observations | repeated interpolation | T1 | [ ] |
| G11 | Variable GWT, single observation | `iostat_end` → constant-table branch | T3 | [ ] |
| G12 | Variable GWT rising then falling | non-monotonic table | T2 | [ ] |
| G13 | Observations starting after the sim period | leading extrapolation | T3 | [ ] |
| G14 | Observations ending before the sim period | trailing extrapolation | T3 | [ ] |
| G15 | Observation year not linked (1901) | year-agnostic groundwater | T2 | [ ] |
| G16 | Saline groundwater, EC 5 dS/m | salt entering from below | T1 | [ ] |
| G17 | Saline groundwater, EC varying with the table | time-varying `ECdSm` | T2 | [ ] |
| G18 | GWT with a restrictive layer above it | FC adjustment across a barrier | T2 | [ ] |

### H. Initial conditions  (`LoadInitialConditions`, `AdjustThetaInitial`, `ResetSWCToFC`)

| ID | Case | Exercises | Tier | St |
|---|---|---|---|---|
| H01 | No `.SW0` — profile at FC | `DeclareInitialCondAtFCandNoSalt` — covered by A01 | T0 | [x] |
| H02 | `.SW0` specified at depths | `IniSWC_AtDepths = true` | T1 | [ ] |
| H03 | `.SW0` specified per compartment | `IniSWC_AtDepths = false` | T1 | [ ] |
| H04 | `.SW0` with 1 location | minimum location count | T2 | [ ] |
| H05 | `.SW0` with 12 locations | `max_No_compartments` locations | T2 | [ ] |
| H06 | `.SW0` locations not matching compartment edges | `AdjustThetaInitial` interpolation | T1 | [ ] |
| H07 | `.SW0` shallower than the profile | undefined below the last location | T2 | [ ] |
| H08 | `.SW0` deeper than the profile | truncation | T3 | [ ] |
| H09 | `.SW0` with ECe per location | initial salinity profile | T1 | [ ] |
| H10 | `.SW0` with an ECe gradient | per-cell salt initialisation | T1 | [ ] |
| H11 | `.SW0` with initial surface storage | ponded start | T2 | [ ] |
| H12 | `.SW0` with surface storage + ECStorage | saline ponded start | T2 | [ ] |
| H13 | `.SW0` with CCini set | mid-season canopy restart | T1 | [ ] |
| H14 | `.SW0` with Bini set | biomass carried in | T1 | [ ] |
| H15 | `.SW0` with Zrini set | rooting depth carried in | T1 | [ ] |
| H16 | `.SW0` with CCini + Bini + Zrini together | full mid-season restart | T1 | [ ] |
| H17 | Profile initialised at wilting point | dry start, delayed germination | T1 | [ ] |
| H18 | Profile initialised at saturation | waterlogged start | T2 | [ ] |
| H19 | Profile initialised above saturation | clamping | T3 | [ ] |
| H20 | `KeepSWC` between runs | Ottawa runs 2 and 3 | T0 | [x] |
| H21 | `KeepSWC` with a different soil in the next run | `AdjustThetaInitial` remapping | T2 | [ ] |
| H22 | `KeepSWC` with a different Zrx in the next run | compartment regrade + remap (see F28) | T1 | [ ] |
| H23 | `KeepSWC` with salt carried over | salt continuity between runs | T1 | [ ] |

### I. Irrigation  (`LoadIrriScheduleInfo`, `Calculate_irrigation`, `.IRR`)

| ID | Case | Exercises | Tier | St |
|---|---|---|---|---|
| I01 | No `.IRR` — rainfed | `IrriMode_NoIrri` — covered by A01 | T0 | [x] |
| I02 | `.IRR` with mode 0 (rainfed, file present) | file read, mode 0 branch | T2 | [ ] |
| I03 | Manual schedule, days + mm | `IrriMode_Manual` | T1 | [ ] |
| I04 | Manual schedule, a single event | minimal schedule | T2 | [ ] |
| I05 | Manual schedule, one event per day | dense schedule | T2 | [ ] |
| I06 | Manual schedule with `IrriFirstDayNr` (v7.0 field) | version-gated read at `>= 70` | T2 | [ ] |
| I07 | Manual schedule with `IrriInfoLastDay` (v7.3 field) | version-gated read at `>= 73` | T2 | [ ] |
| I08 | Manual schedule with per-event ECw | irrigation water salinity | T1 | [ ] |
| I09 | Manual events outside the cropping period | events before day 1 / after DayN | T3 | [ ] |
| I10 | Generate, time = fixed interval 7 d | `GenerateTimeMode_FixInt` | T1 | [ ] |
| I11 | Generate, time = fixed interval 1 d | daily irrigation | T2 | [ ] |
| I12 | Generate, time = allowable depletion (mm) | `GenerateTimeMode_AllDepl` | T1 | [ ] |
| I13 | Generate, `AllDepl` threshold never reached | no events generated | T2 | [ ] |
| I14 | Generate, time = allowable depletion (% RAW) | `GenerateTimeMode_AllRAW` | T1 | [ ] |
| I15 | Generate, `AllRAW` at 100 % | irrigation at RAW exhaustion | T2 | [ ] |
| I16 | Generate, time = water between bunds (paddy) | `GenerateTimeMode_WaterBetweenBunds` | T2 | [ ] |
| I17 | Generate, depth = back to FC | `GenerateDepthMode_ToFC` | T1 | [ ] |
| I18 | Generate, depth = fixed application 30 mm | `GenerateDepthMode_FixDepth` | T1 | [ ] |
| I19 | Generate, `ToFC` when the profile is already at FC | zero-depth event, `Irrigation < 0` clamp | T2 | [ ] |
| I20 | Generate with `TargetTimeVal == 1` | the special-case branch in `Calculate_irrigation` — covered by I11 | T2 | [x] |
| I21 | Generate, schedule changing mid-season | multi-period irrigation parameters | T2 | [ ] |
| I22 | Net irrigation requirement mode | `IrriMode_Inet` | T1 | [ ] |
| I23 | Inet with `PercRAW` 50 % | partial refill | T2 | [ ] |
| I24 | Inet with `PercRAW` 100 % | full refill to FC | T2 | [ ] |
| I25 | Method = sprinkler | `IrriMethod_MSprinkler` | T1 | [ ] |
| I26 | Method = basin | `IrriMethod_MBasin` | T2 | [ ] |
| I27 | Method = border | `IrriMethod_MBorder` | T2 | [ ] |
| I28 | Method = furrow | `IrriMethod_MFurrow` | T2 | [ ] |
| I29 | Method = drip | `IrriMethod_MDrip` (default branch) | T1 | [ ] |
| I30 | Fraction soil wetted 30 / 50 / 100 % | `SimulParam_IrriFwInSeason` | T2 | [ ] |
| I31 | Fraction soil wetted 1 % | extreme localised wetting | T3 | [ ] |
| I32 | Saline irrigation water, ECw 2 dS/m | mild salt build-up | T1 | [ ] |
| I33 | Saline irrigation water, ECw 8 dS/m | strong salt build-up | T1 | [ ] |
| I34 | Over-irrigation → leaching | salt removal via deep percolation | T2 | [ ] |
| I35 | Irrigation exceeding layer-1 Ksat in a day | `calculate_Extra_runoff` from irrigation | T1 | [ ] |
| I36 | Irrigation + rain together exceeding Ksat | combined extra-runoff branch | T1 | [ ] |
| I37 | Irrigation onto a bunded field | routed through surface storage | T1 | [ ] |
| I38 | Generate 6 mm/day, none in the last 10 days; GDD crop, constant 12/28 degC, no rain | `InLastDaysOfCropCycle`, GDD: 12 GDD/day, maturity 2014-08-29, last event 2014-08-19 | T1 | [ ] |
| I39 | Same, Ottawa temperatures | GDD with the Tnx reference: last event about 10 days before maturity | T1 | [ ] |
| I40 | Same, calendar crop (160 days) | calendar rule `Crop_DayN - 10 + 1`: last event 2014-10-17 | T1 | [ ] |

### J. Field management  (`LoadManagement`, `.MAN`)

| ID | Case | Exercises | Tier | St |
|---|---|---|---|---|
| J01 | No `.MAN` file | `NoManagement` defaults | T1 | [ ] |
| J02 | Mulches 0 % | baseline — covered by A01 | T0 | [x] |
| J03 | Mulches 50 %, effect 50 % | `Management_Mulch`, `EffectMulchInS` | T1 | [ ] |
| J04 | Mulches 100 %, effect 50 % | full cover | T1 | [ ] |
| J05 | Mulches 50 %, effect 100 % | maximum evaporation suppression | T2 | [ ] |
| J06 | Mulches 100 %, effect 0 % | cover with no effect | T3 | [ ] |
| J07 | Mulch combined with irrigation wetting | `AdjustEpotMulchWettedSurface` both terms | T1 | [ ] |
| J08 | Soil fertility stress 0 % | no-stress branch | T1 | [ ] |
| J09 | Soil fertility stress 25 % | below the calibration point | T1 | [ ] |
| J10 | Soil fertility stress 50 % | at the crop's calibration point | T0 | [x] |
| J11 | Soil fertility stress 75 % | above the calibration point | T1 | [ ] |
| J12 | Soil fertility stress 100 % | maximum stress | T2 | [ ] |
| J13 | Fertility shape factors from a different crop | `CropStressParametersSoilFertility` sensitivity | T2 | [ ] |
| J14 | Soil bunds 0.10 m | surface storage | T1 | [ ] |
| J15 | Soil bunds 0.30 m + heavy rain | ponding to the bund height, then overflow | T1 | [ ] |
| J16 | Soil bunds 0.00 m with `RunoffOn` false | runoff prevented without bunds | T1 | [ ] |
| J17 | Bund height below the 0.001 m test | `Bundheight < 0.001` branch in effective rain | T3 | [ ] |
| J18 | Runoff prevented by surface practices | `Management_RunoffOn = false` | T1 | [ ] |
| J19 | CN correction +10 by field practices | `Management_CNcorrection` positive | T2 | [ ] |
| J20 | CN correction −10 | `Management_CNcorrection` negative | T2 | [ ] |
| J21 | Weeds: RC 25 % at canopy closure | `Management_WeedRC` | T1 | [ ] |
| J22 | Weeds: RC 75 % | heavy infestation | T2 | [ ] |
| J23 | Weeds: ΔRC +50 % in mid-season | `WeedDeltaRC` positive | T2 | [ ] |
| J24 | Weeds: ΔRC −50 % | `WeedDeltaRC` negative | T2 | [ ] |
| J25 | Weeds: shape factor 100 vs 50 vs −0.01 | `WeedShape` including the default | T2 | [ ] |
| J26 | Weeds: `WeedAdj` 0 / 50 / 100 % for perennials | self-thinning replacement | T2 | [ ] |
| J27 | Weeds combined with fertility stress | interacting canopy reductions | T2 | [ ] |
| J28 | Multiple cuttings NOT considered | forage without cuttings | T1 | [ ] |
| J29 | Cuttings: fixed harvest-day list | current Ottawa `.MAN` | T0 | [x] |
| J30 | Cuttings: a single cut | minimal schedule | T2 | [ ] |
| J31 | Cuttings: a cut on the first day of the cycle | boundary | T3 | [ ] |
| J32 | Cuttings generated, interval in days | `TimeCuttings_IntDay` — covered by J38, W16 | T1 | [x] |

> **A generated cutting schedule needs a data block, and there is no threshold
> record.** `LoadManagement` (`global.f90:3467`) reads nine cuttings records --
> considered, CC after cut, CGC increase, window first day, window length,
> generate, time criterion, final harvest, dayNr of list day 1 -- and none of
> them holds the criterion threshold. That lives in data rows at the end of the
> `.MAN`, which `OpenHarvestInfo` (`run.f90:5955`) reaches by skipping
> 2 + 10 + 12 records: description and version, ten management records, then the
> nine cuttings records **plus** the blank, title and rule lines after them. Each
> row is `FromDay  value`. Inserting a plausible-looking "threshold" record
> instead shifts every later read by one and the run dies with
> `Bad integer for item 1 in list input` at `run.f90:3655`.
| J33 | Cuttings generated, interval in GDD | `TimeCuttings_IntGDD` | T1 | [ ] |
| J34 | Cuttings generated on dry biomass | `TimeCuttings_DryB` | T2 | [ ] |
| J35 | Cuttings generated on dry yield | `TimeCuttings_DryY` | T2 | [ ] |
| J36 | Cuttings generated on fresh yield | `TimeCuttings_FreshY` | T2 | [ ] |
| J37 | Cutting criterion never met | no cuts generated | T3 | [ ] |
| J38 | Cutting window `Day1` > 1 | window start clipping | T2 | [ ] |
| J39 | Cutting window `NrDays` restricted | window end clipping | T2 | [ ] |
| J40 | Cutting window `NrDays = -9` (whole cycle) | the default window — covered by A01 | T0 | [x] |
| J41 | Final harvest at maturity ON | `Cuttings_HarvestEnd` | T2 | [ ] |
| J42 | CC after cut 10 / 25 / 50 % | regrowth starting canopy | T2 | [ ] |
| J43 | CGC increase after cut 0 / 20 / 50 % | regrowth vigour | T2 | [ ] |
| J44 | Cuttings across a multi-run project | `FirstDayNr` continuity between runs — covered by A01 | T1 | [x] |

### K. Off-season  (`LoadOffSeason`, `.OFF`)

| ID | Case | Exercises | Tier | St |
|---|---|---|---|---|
| K01 | No `.OFF` file | baseline — covered by A01 | T0 | [x] |
| K02 | Soil cover before the season | `SoilCoverBefore` | T1 | [ ] |
| K03 | Soil cover after the season | `SoilCoverAfter` | T1 | [ ] |
| K04 | Soil cover both sides, 100 % | full off-season mulch | T2 | [ ] |
| K05 | Mulch effect off-season 25 / 50 / 100 % | `EffectMulchOffS` | T2 | [ ] |
| K06 | 1 irrigation event BEFORE the growing period | `IrriBeforeSeason` | T1 | [ ] |
| K07 | 5 irrigation events before (the array maximum) | `IrriBeforeSeason` full | T2 | [ ] |
| K08 | 1 irrigation event AFTER the growing period | `IrriAfterSeason` | T1 | [ ] |
| K09 | 5 irrigation events after | `IrriAfterSeason` full | T2 | [ ] |
| K10 | Events both before and after | both title-skip branches | T2 | [ ] |
| K11 | Pre-season irrigation water quality | `IrriECw_PreSeason` | T2 | [ ] |
| K12 | Post-season irrigation water quality | `IrriECw_PostSeason` | T2 | [ ] |
| K13 | Off-season fraction soil wetted | `IrriFwOffSeason` | T2 | [ ] |
| K14 | Off-season with a sim period == cropping period | no off-season days to apply it to | T3 | [ ] |
| K15 | Multi-run perennial with off-season between runs | interaction with `KeepSWC` | T2 | [ ] |

### L. Salinity configuration

| ID | Case | Exercises | Tier | St |
|---|---|---|---|---|
| L01 | Non-saline baseline | — | T0 | [x] |
| L02 | Initial ECe 2 dS/m uniform | just at the crop's lower threshold | T1 | [ ] |
| L03 | Initial ECe 4 dS/m uniform | moderate salt stress | T1 | [ ] |
| L04 | Initial ECe 8 dS/m uniform | strong salt stress | T1 | [ ] |
| L05 | Initial ECe 16 dS/m (== the crop's ECmax) | upper salinity bound, no growth | T2 | [ ] |
| L06 | Initial ECe 25 dS/m (above ECmax) | beyond the tolerance range | T3 | [ ] |
| L07 | ECe increasing with depth | salt profile gradient | T1 | [ ] |
| L08 | ECe decreasing with depth | leached-surface profile | T2 | [ ] |
| L09 | Crop ECn == ECx (step response) | degenerate tolerance range | T3 | [ ] |
| L10 | Salt diffusion factor 0 % | no diffusion between cells | T2 | [ ] |
| L11 | Salt diffusion factor 20 % | Ottawa default — covered by L03 | T0 | [x] |
| L12 | Salt diffusion factor 100 % | full mixing | T2 | [ ] |
| L13 | Salt solubility 50 g/l | early precipitation | T2 | [ ] |
| L14 | Salt solubility 100 g/l | Ottawa default — covered by L03 | T0 | [x] |
| L15 | Salt solubility 500 g/l | no precipitation | T2 | [ ] |
| L16 | Calibrated CC distortion 0 / 25 / 100 % | `CalibratedCCdistortion` | T2 | [ ] |
| L17 | Stomatal response to ECsw 0 / 100 / 200 % | `ResponseECsw` | T2 | [ ] |
| L18 | Salinity with a shallow saline water table | G16 × L03 interaction | T1 | [ ] |
| L19 | Salinity with saline irrigation and leaching | I33 × I34 interaction | T1 | [ ] |
| L20 | Salt outputs enabled (`Out4Salt`, `Out6CompEC`) | salinity report writers | T1 | [ ] |

### M. Growing-season calendar  (`.CAL`)

**`LoadCropCalendar` has zero call sites in the standalone.** The `.CAL` file is
a GUI artefact: the interface uses it to *generate* the onset date, then writes
the resulting fixed dates into the `.PRM`, which is all the standalone reads.
The 17 onset-generation cases originally planned here (rainfall criteria 1–4,
air-temperature criteria 11–14, search-window edges) tested unreachable code and
have been removed. They come back if that logic is ever ported into the
standalone. The perennial dormancy onset/end criteria in the `.CRO` file are a
*different* mechanism and are live — those are B17–B22.

| ID | Case | Exercises | Tier | St |
|---|---|---|---|---|
| M01 | `.CAL` referenced in the `.PRM` | stored in `CalendarFile`, never opened | T2 | [x] |
| M02 | `(None)` for the calendar file | the no-calendar project | T2 | [ ] |
| M03 | Referenced `.CAL` missing from disk | should be harmless — nothing opens it | T3 | [ ] |

### N. Program parameters  (`.PPn`, `LoadProgramParametersProject`, `*.PAR`)

> **The `.PPn` must be named after the project.**
> `ComposeFileForProgramParameters` (`startunit.F90:749`) strips the project's
> extension and looks for `PARAM/<basename>.PPn` (or `.PP1` for a `.PRO`). A
> parameter file under any other name is **silently ignored** and the built-in
> defaults from `initialsettings.f90` are used instead — no warning, no log line.
> The harness now renames whatever `.PPn` a case stages to match its project;
> before that fix, group C's GDD-method patches had no effect and every case ran
> with the default method 3. Worth remembering when reading someone's project
> that "ignores" its parameters.
>
> For the record, `Ottawa.PPn` is value-for-value identical to the built-in
> defaults, which is why the group F references were unaffected.

| ID | Case | Exercises | Tier | St |
|---|---|---|---|---|
| N01 | No project `.PPn` | built-in defaults | T1 | [ ] |
| N02 | `.PPn` present but short (truncated file) | partial read — must stop with a message (BUG-21) | T3 | [ ] |
| N03 | Evaporation decline factor 1 / 4 / 8 | stage-II evaporation decline | T2 | [ ] |
| N04 | Kex 1.00 / 1.10 / 1.20 | maximum soil evaporation coefficient | T2 | [ ] |
| N05 | CC threshold for HI 0 / 5 / 20 % | HI cut-off on a senescing canopy | T2 | [ ] |
| N06 | Root expansion start depth 50 / 70 / 100 % of Zmin | `rootunit.f90` curve start | T2 | [ ] |
| N07 | Max root-zone expansion 1 / 5 cm/day | daily expansion cap | T2 | [ ] |
| N08 | Shape factor for water stress on root expansion | −6 vs 0 vs +6 | T2 | [ ] |
| N09 | Germination TAW threshold 5 / 20 / 50 % | `CheckGermination` | T1 | [ ] |
| N10 | Germination threshold never met | germination never triggered | T3 | [ ] |
| N11 | p-adjustment by ETo factor 0.5 / 1.0 / 2.0 | `AdjustpLeafToETo` and friends | T2 | [ ] |
| N12 | Days to full aeration effect 1 / 3 / 7 | `DelayLowOxygen` | T2 | [ ] |
| N13 | Senescence exponent 0.5 / 1.0 / 2.0 | drop in photosynthetic activity | T2 | [ ] |
| N14 | p(sen) decrease 0 / 12 / 30 % | early-senescence trigger | T2 | [ ] |
| N15 | Top-soil thickness 5 / 10 / 20 cm | top-soil depletion reporting | T2 | [ ] |
| N16 | Evaporation depth `EvapZmax` 15 / 30 cm | evaporation-layer extent | T1 | [ ] |
| N17 | `EvapZmax` == `EvapZmin` | the `EvapZmax > EvapZmin` guard — covered by N16a: EvapZmin is the parameter 15, so N16a IS the equal case | T3 | [x] |
| N18 | Depth for CN adjustment 0.10 / 0.30 m | AMC determination depth | T2 | [ ] |
| N19 | Salt diffusion factor via `Soil.PAR` | `ReadSoilSettings` | T2 | [ ] |
| N20 | Capillary-rise shape factor 8 / 16 / 32 | `RootNrDF` | T2 | [ ] |
| N21 | Default Tmin/Tmax 12/28 °C (no T file) | pairs with C20 | T1 | [ ] |
| N22 | Default Tmin/Tmax set below Tbase | no GDD accumulation at all | T3 | [ ] |
| N23 | GDD method parameter 1 / 2 / 3 | `ReadTemperatureSettingsParameters` | T1 | [ ] |
| N25 | `pMethod_NoCorrection` (crop-file flag 0) | p not adjusted by ETo | T1 | [ ] |
| N26 | `pMethod_FAOCorrection` (flag 1) | Ottawa default — covered by A01 | T0 | [x] |
| N28 | Project `.PPn` overriding a global default | precedence of project over global — covered by A01 and every other case staging a .PPn: the lookup has one level, not two | T1 | [x] |

### O. Output & reporting  (`run.f90`, `inforesults.f90`)

| ID | Case | Exercises | Tier | St |
|---|---|---|---|---|
| O01 | No `DailyResults.SIM` | no daily output at all | T1 | [ ] |
| O02 | Daily output 1 only (water balance) | `Out1Wabal` | T1 | [ ] |
| O03 | Daily output 2 only (crop) | `Out2Crop` | T1 | [ ] |
| O04 | Daily output 3 only (profile / root-zone WC) | `Out3Prof` | T1 | [ ] |
| O05 | Daily output 4 only (salinity) | `Out4Salt` | T1 | [ ] |
| O06 | Daily output 5 only (WC at depths) | `Out5CompWC` | T1 | [ ] |
| O07 | Daily output 6 only (EC at depths) | `Out6CompEC` | T1 | [ ] |
| O08 | Daily output 7 only (climate) | `Out7Clim` | T1 | [ ] |
| O09 | Daily output 8 only (irrigation events) | `Out8Irri` — needs an `.IRR` case | T1 | [ ] |
| O10 | All 8 daily outputs together | full `*day.OUT` | T1 | [~] |
| O11 | Daily outputs 5 and 6 with 12 compartments | widest possible daily record | T2 | [ ] |
| O12 | Daily outputs 5 and 6 with 3 compartments | narrowest record | T2 | [ ] |
| O13 | `DailyResults.SIM` listing a line twice | duplicate selection | T3 | [ ] |
| O14 | `DailyResults.SIM` listing an out-of-range number | invalid selection | T3 | [ ] |
| O15 | Aggregation 0 (none) | `OutputAggregate = 0` — covered by A01 | T0 | [x] |
| O16 | Aggregation 1 (daily) | `OutputAggregate = 1` | T1 | [ ] |
| O17 | Aggregation 2 (10-daily) | `OutputAggregate = 2` | T1 | [ ] |
| O18 | Aggregation 3 (monthly) | `OutputAggregate = 3` | T1 | [ ] |
| O19 | Aggregation 2 on a season shorter than a decade | partial aggregation period | T3 | [ ] |
| O20 | Aggregation 3 across a year boundary | monthly bins crossing years | T2 | [ ] |
| O21 | No `ParticularResults.SIM` | neither harvests nor evaluation | T2 | [ ] |
| O22 | Particular 1 only (multiple cuttings) | `Part1Mult` → `*harvests.OUT` — covered by J33 | T1 | [x] |
| O23 | Particular 2 only (evaluation) | `Part2Eval` → `*evaluation.OUT` — covered by O25 | T1 | [x] |
| O24 | Particular 1 on a crop with no cuttings | empty harvests file | T3 | [ ] |
| O25 | `.OBS` with canopy-cover observations | `typeObsSim_ObsSimCC` | T1 | [ ] |
| O26 | `.OBS` with biomass observations | `typeObsSim_ObsSimB` — covered by A01 | T1 | [x] |
| O27 | `.OBS` with soil-water-content observations | `typeObsSim_ObsSimSWC` | T1 | [ ] |
| O28 | `.OBS` with all three, several seasons | per-run evaluation files | T1 | [~] |
| O29 | `.OBS` with observations outside the sim period | out-of-range observations | T3 | [ ] |
| O30 | `.OBS` with a single observation | degenerate statistics (R², RMSE) | T3 | [ ] |
| O31 | `.OBS` with all values −9 (missing) | all-missing statistics | T3 | [ ] |
| O32 | No `.OBS` but `Part2Eval` requested | graceful skip | T3 | [ ] |
| O33 | Season output for a multi-run project | `*season.OUT` totals — covered by A01 | T0 | [x] |
| O34 | `AllDone.OUT` and `ListProjectsLoaded.OUT` content | run-completion reporting — covered by A01 | T0 | [x] |
| O35 | Output file names > 8 characters | project-name truncation in output names | T3 | [ ] |

### P. Stress & failure paths

| ID | Case | Exercises | Tier | St |
|---|---|---|---|---|
| P01 | Mild water stress on canopy expansion | `p-exp` upper/lower thresholds | T1 | [ ] |
| P02 | Water stress closing stomata | `p-sto` | T1 | [ ] |
| P03 | Water stress triggering early senescence | `p-sen` | T1 | [ ] |
| P04 | Water stress at pollination | `p-pol` | T2 | [ ] |
| P05 | Full crop failure from drought | zero yield | T2 | [ ] |
| P06 | Perennial permanently wilted in dormancy | `SumETo` dormancy threshold (600) | T2 | [ ] |
| P07 | Cold stress on pollination (Tmin < 8 °C) | pollination failure | T1 | [ ] |
| P08 | Heat stress on pollination (Tmax > 40 °C) | pollination failure | T1 | [ ] |
| P09 | Cold and heat stress in the same season | both branches in one run | T2 | [ ] |
| P10 | Insufficient GDD for full transpiration | `GDtranspLow` | T2 | [ ] |
| P11 | Deficient aeration (waterlogging) | `DayAnaero`, `AnaeroPoint` | T1 | [ ] |
| P12 | Aeration stress lasting past `DelayLowOxygen` | full aeration effect | T1 | [ ] |
| P13 | Water + fertility stress together | stress combination | T2 | [ ] |
| P14 | Water + salinity stress together | stress combination | T2 | [ ] |
| P15 | Water + fertility + salinity + temperature | all four at once | T2 | [ ] |
| P16 | Very short cycle (~20 days) | short-season arithmetic | T3 | [ ] |
| P18 | CCx tiny (0.05) | degenerate canopy | T3 | [ ] |
| P19 | Zero plant density | degenerate CCo | T3 | [ ] |
| P20 | Crop never emerging | germination never satisfied | T3 | [ ] |

---

# Part II — Kernel coverage

One group per `simul.f90` routine family. These are designed the other way
round from Part I: pick the routine, read its branches, and construct the
smallest input that drives each one. Most are one season, season output only.

### Q. Drainage & internal redistribution  (`calculate_drainage`, `calculate_delta_theta`, `calculate_theta`)

| ID | Case | Exercises | Tier | St |
|---|---|---|---|---|
| Q01 | Uniform profile draining from saturation | the ordinary `drainability` path | T1 | [ ] |
| Q02 | Drainage capped by `drainmax` | `drainsum <= drainmax` false | T1 | [ ] |
| Q05 | `theta_x > SAT` (low-tau layer) | the whole `theta_x > SAT` block | T1 | [ ] |
| Q06 | Excess generated, pushed up one compartment | `excess > 0`, `pre_nr < compi` | T1 | [ ] |
| Q07 | Excess propagating up several compartments | the `pre_nr` walk | T1 | [ ] |
| Q10 | Fine layer under a coarse layer | drainage limited at a boundary | T1 | [ ] |
| Q11 | Coarse layer under a fine layer | free drainage below a slow layer | T1 | [ ] |
| Q12 | Ksat 5 mm/day throughout (tau near zero) | slowest drainage | T1 | [ ] |
| Q13 | Ksat 1500 mm/day (tau at its ceiling) | fastest drainage | T2 | [ ] |
| Q15 | Drainage with a groundwater table raising FCadj | `FCadj` in place of FC | T1 | [ ] |
| Q16 | Drainage with gravel reducing pore volume | gravel × drainage | T2 | [ ] |
| Q17 | Profile starting below FC (no drainage) | the no-op path | T2 | [ ] |
| Q18 | Repeated saturation over consecutive days | day-to-day drainage continuity | T2 | [ ] |
| Q19 | Drainage with 3 compartments | minimal compartment count | T2 | [ ] |
| Q20 | Drainage with 12 graded compartments | thickness-weighted redistribution | T1 | [ ] |

### R. Runoff, surface storage & infiltration  (`calculate_runoff`, `calculate_surfacestorage`, `calculate_Extra_runoff`, `calculate_infiltration`)

`calculate_infiltration` is the largest kernel in the model — 414 lines and 83
branch lines — so it gets the most cases.

| ID | Case | Exercises | Tier | St |
|---|---|---|---|---|
| R01 | Daily rain record, CN runoff | `datatype_daily` branch of `calculate_runoff` — covered by A01 | T0 | [x] |
| R06 | Profile drier than WP in the runoff depth | the `theta < WP` weighting branch | T2 | [ ] |
| R08 | Weighted wetness sum > 1 | `SUM > 1` clamp | T3 | [ ] |
| R09 | `MaxDepth` reached before the last compartment | the `CumDepth >= MaxDepth` exit | T2 | [ ] |
| R10 | `MaxDepth` deeper than the profile | the `compi == NrCompartments` exit | T2 | [ ] |
| R11 | Rain onto bunds, below the bund height | surface storage accumulating | T1 | [ ] |
| R12 | Rain onto bunds, overtopping | `SurfaceStorage > BundHeight*1000` | T1 | [ ] |
| R13 | Surface storage before the cropping period | `dayi < Crop_Day1` | T2 | [ ] |
| R14 | Surface storage after the cropping period | `dayi > Crop_DayN` | T2 | [ ] |
| R15 | Rain exceeding layer-1 Ksat | `Sum > InfRate` branch | T1 | [ ] |
| R16 | Irrigation alone exceeding layer-1 Ksat | `calculate_Extra_runoff` irrigation branch | T1 | [ ] |
| R17 | Rain + irrigation exceeding Ksat together | the combined branch | T1 | [ ] |
| R18 | Infiltration into an unsaturated profile | the ordinary store path — covered by A01 | T0 | [x] |
| R26 | Water still to store after the whole profile | `amount_still_to_store > 0` at the end | T1 | [ ] |
| R28 | Infiltration into a profile already at SAT | zero storage capacity | T2 | [ ] |
| R29 | Infiltration through a sand-over-clay boundary | perching at a layer interface | T1 | [ ] |
| R30 | Infiltration with a shallow groundwater table | FCadj reducing capacity | T1 | [ ] |
| R31 | 100 mm in one day onto a dry deep profile | full-profile wetting front | T1 | [ ] |
| R32 | 100 mm in one day onto a shallow profile | immediate deep percolation | T1 | [ ] |
| R33 | Rain on the first day of the simulation | day-1 initialisation of the balance | T2 | [ ] |
| R36 | Effective rain with surface storage present | `SurfaceStorage > 0` branch | T2 | [ ] |
| R39 | Effective rain, `SubDrain > DrainMax`, no bunds | the `Bundheight < 0.001` branch | T2 | [ ] |
| R40 | Effective rain, `SubDrain > DrainMax`, with bunds | the bunded branch | T2 | [ ] |
| R43 | USDA effective rain with `RainMonth <= 0.1` | the dry-month guard | T3 | [ ] |

### S. Soil evaporation  (`PrepareStage1/2`, `CalculateSoilEvaporationStage1/2`, `ExtractWaterFromEvapLayer`, `WCEvapLayer`, `AdjustEpotMulchWettedSurface`, `CalculateEvaporationSurfaceWater`, `ConcentrateSalts`)

| ID | Case | Exercises | Tier | St |
|---|---|---|---|---|
| S01 | Bare soil, wet, stage-I evaporation | `CalculateSoilEvaporationStage1` | T1 | [ ] |
| S02 | Drying soil crossing into stage II | `PrepareStage2`, the REW threshold | T1 | [ ] |
| S03 | Long dry-down, deep into stage II | `CalculateSoilEvaporationStage2` | T1 | [ ] |
| S04 | Rewetting resetting to stage I | `PrepareStage1` | T1 | [ ] |
| S05 | ETo exactly 5 mm/day | the `abs(ETo - 5) > 0.01` reference branch | T2 | [ ] |
| S06 | ETo 1 vs 5 vs 12 mm/day | the ETo scaling of stage II | T1 | [ ] |
| S07 | `EvapZmax` 0.15 m | shallow evaporation layer | T1 | [ ] |
| S08 | `EvapZmax` 0.30 m (Ottawa) | covered by A01 | T0 | [x] |
| S10 | Evaporation layer spanning two soil layers | `WCEvapLayer` across a boundary | T1 | [ ] |
| S12 | Top compartment reaching WP/2 | the `theta <= WP/200` floor | T2 | [ ] |
| S16 | REW 3 mm (short stage I) | early transition | T1 | [ ] |
| S17 | REW 15 mm (long stage I) | late transition | T1 | [ ] |
| S18 | Mulch 50 % reducing Epot | `AdjustEpotMulchWettedSurface` mulch term | T1 | [ ] |
| S19 | Drip irrigation wetting 30 % of the surface | the wetted-surface term | T1 | [ ] |
| S20 | Mulch and partial wetting together | both terms in one call | T1 | [ ] |
| S21 | Off-season mulch (`SoilCoverBefore`) | the off-season mulch branch | T2 | [ ] |
| S22 | Evaporation from ponded water on bunds | `CalculateEvaporationSurfaceWater` | T1 | [ ] |
| S24 | Salts concentrating as water evaporates | `ConcentrateSalts` | T1 | [ ] |
| S25 | Concentration past the solubility limit | precipitation into `Depo` | T1 | [ ] |
| S26 | Evaporation under full canopy | `EffectCanopyCoverInReducingSoilEvap` at 60 % | T1 | [ ] |
| S27 | Evaporation under a senescing canopy | late-season evaporation increase | T2 | [ ] |

### T. Transpiration & root water uptake  (`calculate_transpiration`, `surface_transpiration`, `calculate_weighting_factors`, `DetermineRootZoneWC`)

| ID | Case | Exercises | Tier | St |
|---|---|---|---|---|
| T01 | Unstressed transpiration at full canopy | the reference path | T1 | [ ] |
| T02 | Transpiration with water stress (Ks < 1) | the stomatal Ks factor | T1 | [ ] |
| T03 | Transpiration with salinity stress | `Coeffb0Salt`/`b1`/`b2` | T1 | [ ] |
| T04 | Transpiration with both water and salt stress | combined coefficients | T1 | [ ] |
| T05 | Uptake distributed over 3 compartments | `calculate_weighting_factors` | T1 | [ ] |
| T06 | Uptake distributed over 12 graded compartments | thickness-weighted uptake | T1 | [ ] |
| T10 | `Smax` top vs bottom quarter (0.020 / 0.010) | the extraction-rate gradient | T1 | [ ] |
| T11 | `Smax` equal top and bottom | uniform extraction | T2 | [ ] |
| T14 | Anaerobic compartments past `DelayLowOxygen` | `surface_transpiration` aeration branch | T1 | [ ] |
| T15 | `AnaeroPoint = 0` (no aeration stress) | the `AnaeroPoint > 0` guard | T2 | [ ] |
| T16 | Transpiration from surface water (paddy) | `SurfaceStorage > KsReduction*Part*Tpot` | T1 | [ ] |
| T19 | Kc ageing decline 0 / 11 / 30 % | the ageing term | T2 | [ ] |
| T20 | `DetermineRootZoneWC` with `ZtopSWCconsidered` | top-soil water content reporting | T1 | [ ] |
| T21 | Root zone water content at FC | the no-depletion reference | T1 | [ ] |
| T22 | Root zone water content at WP | full depletion | T1 | [ ] |
| T23 | Root zone spanning two soil layers | layer-weighted TAW/RAW | T1 | [ ] |
| T24 | Transpiration with a groundwater table in the root zone | uptake from a saturated zone | T1 | [ ] |
| T25 | Transpiration under weed competition | `WeedRC` reducing crop Tr | T2 | [ ] |
| T26 | Transpiration with a partially wetted surface | drip irrigation × Tr | T2 | [ ] |

### U. Capillary rise & groundwater inflow  (`calculate_CapillaryRise`, `HorizontalInflowGWTable`, `CalculateAdjustedFC`)

| ID | Case | Exercises | Tier | St |
|---|---|---|---|---|
| U01 | No water table — no capillary rise | the disabled path — covered by A01 | T0 | [x] |
| U02 | Table at 2.0 m, dry profile above | ordinary capillary rise | T1 | [ ] |
| U03 | Table at 0.6 m | strong rise into the root zone | T1 | [ ] |
| U04 | Table at 0.3 m | rise into the evaporation layer | T1 | [ ] |
| U05 | Rise limited by `MaxMM > LimitMM` | the upper limit branch (both occurrences) | T1 | [ ] |
| U07 | Compartment already above `ThetaThreshold` | no rise into that compartment | T2 | [ ] |
| U08 | Rise walking to compartment 1 | the `compi < 1` loop exit | T1 | [ ] |
| U11 | Capillary rise per soil class (12 textures) | `NumberSoilClass` → CR parameters | T2 | [ ] |
| U12 | Shape factor `RootNrDF` 8 / 16 / 32 | the water-content gradient effect | T2 | [ ] |
| U13 | Saline capillary rise (`CRsalt`) | salt carried up with the water | T1 | [ ] |
| U14 | Horizontal inflow, compartment below the table | `Zi >= DepthGWTmeter` | T1 | [ ] |
| U16 | Horizontal inflow with an EC difference | `ECeComp != ECiAqua` | T1 | [ ] |
| U18 | `CalculateAdjustedFC` with the table below the profile | no FC adjustment | T2 | [ ] |
| U19 | `CalculateAdjustedFC` with the table inside the profile | graded FC adjustment | T1 | [ ] |
| U20 | Table rising during the season | FC adjustment changing daily | T1 | [ ] |

### V. Salt transport  (`calculate_saltcontent`, salt cells, `Calculate_SaltMobility`)

| ID | Case | Exercises | Tier | St |
|---|---|---|---|---|
| V01 | Non-saline profile, salt balance closes at zero | the trivial path — covered by A01, L01 | T0 | [x] |
| V02 | 2 salt cells (Ksat 1200) | `SCP1 = 2`, coarsest discretisation | T0 | [x] |
| V03 | 4 salt cells (Ksat 500) | `SCP1 = 4` | T1 | [ ] |
| V04 | 7 salt cells (Ksat 200) | `SCP1 = 7` | T1 | [ ] |
| V05 | 11 salt cells (Ksat ≤ 112) | `SCP1 = 11`, finest discretisation | T1 | [ ] |
| V06 | `SCP1` clamped to its minimum of 2 | very high Ksat | T3 | [ ] |
| V07 | Different `SCP1` per layer | cell-count change across a boundary | T1 | [ ] |
| V08 | Salt moving down with infiltrating rain | downward advection | T1 | [ ] |
| V09 | Salt moving down with irrigation | advection from irrigation water | T1 | [ ] |
| V10 | Salt leaving with deep percolation | `SaltOut` | T1 | [ ] |
| V11 | Salt entering with capillary rise | `CRSalt` | T1 | [ ] |
| V13 | Salt diffusion between cells, 0 % | `Calculate_SaltMobility` at zero | T2 | [ ] |
| V14 | Salt diffusion between cells, 20 % | Ottawa default — covered by V02 | T0 | [x] |
| V15 | Salt diffusion between cells, 100 % | full mobility | T2 | [ ] |
| V16 | Salt precipitating at the solubility limit | `Depo` filling | T1 | [ ] |
| V17 | Precipitated salt redissolving | `Depo` emptying | T1 | [ ] |
| V20 | Salt with surface storage present | salt in ponded water | T2 | [ ] |
| V22 | Full leaching to a non-saline profile | complete salt removal | T1 | [ ] |
| V24 | ECe → ECsw conversion in the root zone | `DetermineRootZoneSaltContent` | T1 | [ ] |

### W. Canopy development  (`DetermineCCi`, `DetermineCCiGDD`, `FeedbackCC`, `GetCDCadjustedNoStressNew`)

| ID | Case | Exercises | Tier | St |
|---|---|---|---|---|
| W01 | Unstressed canopy, calendar days | `DetermineCCi` reference path | T1 | [ ] |
| W02 | Unstressed canopy, GDD | `DetermineCCiGDD` reference path — covered by T01, I22 | T1 | [x] |
| W03 | Same canopy both ways | calendar/GDD canopy equivalence | T1 | [ ] |
| W04 | Emergence delayed by dry soil | `CheckGermination` gating CC start | T1 | [ ] |
| W05 | Canopy expansion limited by water stress | `StressLeaf` | T1 | [ ] |
| W06 | Canopy expansion limited by fertility stress | `CCxAdjusted`, `CCoAdjusted` | T1 | [ ] |
| W07 | Canopy expansion limited by salinity | salt effect on CC | T1 | [ ] |
| W08 | Canopy never reaching CCx | truncated expansion | T1 | [ ] |
| W09 | Canopy reaching CCx exactly at senescence | boundary | T2 | [ ] |
| W10 | Early senescence triggered by water stress | `TimeSenescence`, `WithBeta` | T1 | [ ] |
| W11 | Senescence with an adjusted CDC | `GetCDCadjustedNoStressNew` | T1 | [ ] |
| W12 | Senescence recovery after rewetting | the un-senescence path | T1 | [ ] |
| W13 | `FeedbackCC` correcting CC | the feedback branch | T2 | [ ] |
| W14 | CC decline to zero before maturity | full canopy loss | T2 | [ ] |
| W15 | Canopy after a cut (forage regrowth) | `CCcut`, CGC increase — covered by A01 | T1 | [x] |
| W16 | Two cuts in quick succession | repeated regrowth | T2 | [ ] |
| W17 | Cut before canopy closure | regrowth from a partial canopy | T2 | [ ] |
| W18 | Weed-infested canopy, `WeedRC` at closure | total vs crop CC | T1 | [ ] |
| W19 | Weeds with `WeedDeltaRC` in mid-season | time-varying weed cover | T2 | [ ] |
| W20 | Weeds replacing self-thinned perennial CC | `WeedAdj` | T2 | [ ] |
| W21 | Perennial self-thinning across years | CCx decline with age | T2 | [ ] |
| W22 | CCo from plant density | `SizeSeedling`, plants/ha | T1 | [ ] |
| W23 | CCo from re-growth canopy size | `SizePlant` for perennials — covered by A01 | T1 | [x] |
| W24 | Plant density 10× the crop default | dense stand | T2 | [ ] |
| W25 | Plant density 1/10 the crop default | sparse stand | T2 | [ ] |
| W26 | CGC 0.05 vs 0.18 vs 0.30 | expansion rate sensitivity | T2 | [ ] |
| W27 | CDC 0.01 vs 0.036 vs 0.10 | decline rate sensitivity | T2 | [ ] |
| W28 | Transplanted crop starting above CCo | transplant canopy | T1 | [ ] |
| W29 | Canopy under aeration stress | waterlogging reducing CC | T2 | [ ] |
| W30 | Canopy with `CCini` from `.SW0` | mid-season canopy restart | T1 | [ ] |

### X. Biomass, harvest index & yield  (`DeterminePotentialBiomass`, `DetermineBiomassAndYield`)

| ID | Case | Exercises | Tier | St |
|---|---|---|---|---|
| X01 | Unstressed biomass accumulation | the WP × Tr/ETo reference path | T1 | [ ] |
| X02 | Biomass under water stress | reduced Tr → reduced B | T1 | [ ] |
| X03 | Biomass under fertility stress | `FracBiomassPotSF` — covered by A01 | T1 | [x] |
| X04 | Biomass under salinity stress | salt-limited B | T1 | [ ] |
| X05 | WP adjusted for CO2 below 369.41 ppm | downward CO2 adjustment | T1 | [ ] |
| X06 | WP at exactly 369.41 ppm | no adjustment — covered by D28 | T1 | [x] |
| X07 | WP adjusted for CO2 above 369.41 ppm | upward CO2 adjustment — covered by A01 | T1 | [x] |
| X08 | CO2 at 800 ppm (far future) | strong CO2 effect | T2 | [ ] |
| X09 | WP* for a C3 vs a C4 crop | the WP parameter itself | T1 | [ ] |
| X10 | WP change during yield formation | the post-anthesis WP factor | T1 | [ ] |
| X11 | HI reference, grain crop | `subkind_Grain` HI build-up | T1 | [ ] |
| X12 | HI reference, tuber crop | `subkind_Tuber` | T1 | [ ] |
| X13 | HI reference, vegetative crop | `subkind_Vegetative` | T1 | [ ] |
| X14 | HI for forage at each cut | `subkind_Forage` yield per cut — covered by A01 | T1 | [x] |
| X15 | HI increased by water stress before flowering | positive HI adjustment | T2 | [ ] |
| X16 | HI reduced by stress during yield formation | negative HI adjustment | T1 | [ ] |
| X17 | HI reduced by pre-anthesis canopy loss | the `HIadj` vegetative term | T2 | [ ] |
| X18 | HI blocked by low green CC | the PPn CC threshold for HI | T1 | [ ] |
| X19 | HI with pollination failure (cold) | `KsPolC` | T1 | [ ] |
| X20 | HI with pollination failure (heat) | `KsPolH` | T1 | [ ] |
| X21 | HI with both pollination stresses | combined pollination failure | T2 | [ ] |
| X22 | Yield with a fresh/dry mass conversion | forage fresh yield reporting | T2 | [ ] |
| X23 | Biomass with a premature end | truncated accumulation | T2 | [ ] |
| X24 | Zero biomass (crop failure) | the degenerate yield path | T2 | [ ] |
| X25 | `Bini` from `.SW0` carried in | biomass restart | T1 | [ ] |
| X26 | Potential vs actual vs unlimited biomass | `BiomassPot`, `BiomassUnlim`, `BiomassTot` — covered by A01 | T1 | [x] |

### Y. Stress engine  (`EffectSoilFertilitySalinityStress`, `CropStressParametersSoilFertility`, `Adjustp*ToETo`)

| ID | Case | Exercises | Tier | St |
|---|---|---|---|---|
| Y01 | Fertility stress 0 % | the no-stress shortcut | T1 | [ ] |
| Y02 | Fertility stress at the crop's calibration point | the calibrated response — covered by A01 | T0 | [x] |
| Y03 | Fertility stress below the calibration point | interpolation downward | T1 | [ ] |
| Y04 | Fertility stress above the calibration point | interpolation upward | T1 | [ ] |
| Y05 | Fertility shape factor for canopy expansion | `ShapeCGC` | T1 | [ ] |
| Y06 | Fertility shape factor for CCx | `ShapeCCX` | T1 | [ ] |
| Y07 | Fertility shape factor for WP | `ShapeWP` (negative in the alfalfa file) | T1 | [ ] |
| Y08 | Fertility shape factor for CC decline | `ShapeCDecline` | T1 | [ ] |
| Y09 | `StressSFadjNEW` recomputed mid-season | the dynamic adjustment path | T1 | [ ] |
| Y10 | Salinity stress alone driving the stress engine | `Coeffb0Salt` from salinity only | T1 | [ ] |
| Y11 | Fertility and salinity stress together | the combined `EffectStress` | T1 | [ ] |
| Y12 | `AdjustpLeafToETo` at ETo 1 mm/day | low-ETo p adjustment | T1 | [ ] |
| Y13 | `AdjustpLeafToETo` at ETo 12 mm/day | high-ETo p adjustment | T1 | [ ] |
| Y14 | `AdjustpStomatalToETo` across the ETo range | stomatal p adjustment | T1 | [ ] |
| Y17 | `pMethod_NoCorrection` — no ETo adjustment at all | the disabled path | T1 | [ ] |
| Y18 | p-exp upper == lower threshold | degenerate stress range | T3 | [ ] |
| Y19 | Stress shape factor 0.0 (straight line) | the linear Ks branch | T1 | [ ] |
| Y20 | Stress shape factor 3.0 (Ottawa) | the curved Ks branch — covered by A01 | T0 | [x] |
| Y21 | Stress shape factor 6.0 | strongly curved Ks | T2 | [ ] |
| Y22 | Temperature stress on biomass (`KsTr`) | `GDtranspLow` on cool days | T2 | [ ] |

### Z. Balance integrity & suite invariants  (`CheckWaterSaltBalance`)

These are not feature tests — they are properties that must hold for *every*
case, checked by the runner rather than by an `OUTP_REF` diff. They cost no
reference storage and apply to cases added later without anyone writing them.

The daily balance was **derived from the output, not assumed**: candidate
identities were tested against a real reference and only the ones that closed
were kept. Across the 137 cases that report a full daily balance the residual is
at most 0.20 mm — the print precision of six one-decimal terms — with the sole
exception of the Inet cases, which is what surfaced observation O2.

| ID | Case | Exercises | Tier | St |
|---|---|---|---|---|
| Z01 | Surface balance closes over the season | `Rain + Irri + stored == Infilt + Runoff` | T1 | [i] |
| Z02 | Water balance closes day by day | `d(WC+Surf) == Rain + Irri + CR − RO − Drain − E − Tr`, ≤ 0.25 mm | T1 | [i] |
| Z03 | Salt balance closes over the season | `SaltIn + SaltUp − SaltOut` vs the change in `Salt(x)`, per run segment, 0.05 ton/ha — closes to print precision except where salt precipitates (O9) — covered by L03, V09 (and every other case with salt) | T1 | [x] |
| Z04 | No negative soil water content in any compartment | theta ≥ 0 | T1 | [i] |
| Z05 | No theta above SAT in any compartment | theta ≤ SAT | T1 | [i] |
| Z06 | No negative salt content in any cell | salt ≥ 0 | T1 | [i] |
| Z07 | Rooting depth never exceeds the profile | Zr ≤ soil depth | T1 | [i] |
| Z08 | Rooting depth never exceeds `Soil_RootMax` | restrictive-layer respect | T1 | [i] |
| Z09 | CC never exceeds CCx | canopy bound | T1 | [i] |
| Z10 | CC never negative | canopy floor | T1 | [i] |
| Z11 | Compartment thicknesses sum to the represented depth | compartment bookkeeping | T1 | [i] |
| Z12 | Compartment count never exceeds 12 | `max_No_compartments` | T1 | [i] |
| Z13 | Bit-identical output on a repeat run | determinism | T1 | [i] |
| Z14 | Bit-identical output regardless of case order | no cross-case state leakage | T1 | [i] |
| Z15 | A PRM of N runs == N separate PRO runs (no `KeepSWC`) | run independence | T1 | [i] |
| Z17 | No NaN or Inf in any output cell | numeric hygiene | T1 | [i] |
| Z18 | No uninitialised-value warnings under `-ffpe-trap` | build-level: needs a rebuild, not a case — earlier runs 2026-09-04 and 2026-09-10 gave BUG-17, BUG-18, BUG-19; re-run 2026-09-25 against the GDD-native references: 845 pass, 4 within tolerance, 1 within tolerance under `--ulp 1` (D08b), **0 error** — nothing traps | T2 | [x] |
| Z19 | Same results at `-O0` and `-O2` within tolerance | build-level: needs two builds, not a case — run 2026-09-25: 845 pass, 4 within tolerance, 1 last-digit flip (D08b, see below); retired O8 | T2 | [x] |
| Z20 | Every `.OUT` file the run should produce exists | output completeness | T1 | [i] |

---

# Part III — Input-file version compatibility

> **Dropped, 2026-09-17.** The suite targets current-format files. Only the
> current-format rows (covered by the baseline) are kept, plus two legacy soil
> files in group F (F54f, F54g, see BUG-1). The rows for older versions of
> each file (VC02-VC09, VC11-VC14, VC16-VC32) were removed: each needed a
> hand-made old-format file, for formats that are rarely used any more.

Every loader is gated on the version number in line 2 of the file. These
branches are invisible in normal use and are exactly what breaks when a field is
added. Each case is the Ottawa baseline with **one** file downgraded to an older
version header and its fields removed accordingly.

| ID | Case | Exercises | Tier | St |
|---|---|---|---|---|
| VC01 | `.CRO` at v7.3 | current format — covered by A01 | T0 | [x] |
| VC10 | `.MAN` at v7.3 | current format — covered by A01 | T0 | [x] |
| VC15 | `.SOL` at v7.3 | current format — covered by A01 | T0 | [x] |

---

# Part IV — Generated sweep families

The groups above are hand-designed: each case exists because a specific branch
needs driving. The families below are the opposite — factorial grids over axes
that interact, generated by `runner/make_inputs.py` from a few lines of YAML.
They are cheap to add, cheap to store (season output only), and they catch
interaction bugs that single-axis cases miss.

Family IDs are `SWnn`; generated case IDs are `SWnn_<factor>_<factor>`.

| ID | Family | Factors | Cases | Tier | St |
|---|---|---|---|---|---|
| SW01 | Soil texture × rooting depth | 12 textures × Zrmax {0.3, 0.8, 1.5, 3.0} m | 48 | T2 | [x] |
| SW02 | Compartment geometry | soil depth {0.30, 0.55, 1.20, 1.25, 3.00, 4.00} × Zrx {0.3, 0.6, 1.2, 1.3, 3.0} | 30 | T1 | [x] |
| SW03 | GDD formulation | GDD method {1,2,3} × 4 sowing dates, plus the calendar twin at each | 16 | T1 | [x] |
| SW04 | Irrigation regime | 5 methods manual, 4 generate criteria, 3 net-requirement | 12 | T2 | [x] |
| SW05 | Fertility × water | fertility {0,25,50,75,100} % × {rainfed, 3 irrigation regimes} | 20 | T1 | [x] |
| SW06 | Salinity | initial ECe {0,2,4,8} × {rainfed, fresh irrigation, saline irrigation} | 12 | T2 | [x] |
| SW07 | Climate record types | temperature {daily, monthly} × ETo {d, dec, mon} × rain {d, dec, mon} | 18 | T2 | [x] |
| SW08 | Groundwater × texture | 6 table configurations × 4 textures | 24 | T2 | [x] |
| SW09 | Surface management | weeds {0,25,75} % × mulch {0,50,100} % × bunds {0, 0.10} m | 18 | T2 | [x] |
| SW10 | Cutting schedules | 5 generated criteria × 2 windows, on the forage crop | 10 | T1 | [x] |
| SW11 | PPn one-at-a-time | 23 program-parameter records perturbed alone | 23 | T2 | [x] |
| SW12 | Crop × sowing date | 10 crop files × 2 sowing dates | 20 | T1 | [x] |

**Sweep total: 251 generated cases.** Fewer than the 300 originally planned, for
two reasons, both deliberate. SW07 drops 10-daily *temperature* entirely, which
removes 9 of its 27 cells, because that reader hangs (defect BUG-6). And several
grids were trimmed where a cell was not a distinct code path -- a GDD method has
no meaning for a calendar-day crop, and an irrigation method has none in
net-requirement mode.

Every sweep case takes **season output only**, about 3 KB of reference, so the
whole of Part IV costs under a megabyte. They still run the full invariant
suite, which is the real point: 251 extra reference-diffs would add little on
their own, but 251 extra runs each checked for mass balance, compartment bounds
and numeric hygiene is genuine coverage.

## Suite totals

| Part | Groups | Cases | T0 | T1 | T2 | T3 |
|---|---|---|---|---|---|---|
| I — Input and option coverage | A–P (16 groups) | 440 | 29 | 178 | 156 | 77 |
| II — Kernel coverage | Q–Z (10 groups) | 260 | 9 | 164 | 75 | 12 |
| III — Version compatibility | VC | 32 | 3 | 15 | 12 | 2 |
| IV — Generated sweeps | SW01–SW12 | 300 | — | 74 | 226 | — |
| **Total** | | **1032** | | | | |

Of the 732 enumerated cases, **58 are already covered** by the existing Ottawa
testcase and 12 more partially. The sweeps are the cheap axis if we want more.

---

# Part V — Build order and division of labour

## What I generate

All of the input files are deterministic text in documented formats, so they get
written by script rather than by hand:

| Script | Produces |
|---|---|
| `assets/gen_soils.py` | the 12 USDA texture `.SOL` files, layered profiles, gravel and penetrability variants, the F-group geometry profiles |
| `assets/gen_climate.py` | decadal and monthly aggregations of the Ottawa daily records, the constant-climate record, the truncated and year-agnostic records |
| `assets/gen_crops.py` | crop variants derived from `DEFAULT.CRO` and `AlfOttawaGDD.CRO` — subkinds, planting types, GDD/calendar pairs, the version-downgraded files for group VC |
| `assets/gen_management.py` | the `.IRR`, `.MAN`, `.GWT`, `.SW0`, `.OFF` and `.CAL` files for groups G–K and M |
| `runner/make_inputs.py` | materialises every `cases/*/` from `case.yml` plus the asset pool |

## Asset pool (as of 2026-09-01)

`tests/assets/` holds 63 staged inputs (1.5 MB). `assets/normalize.py`
regenerates the pool from the donated originals; untouched originals stay in
`assets/raw/`.

| Kind | n | Files |
|---|---|---|
| Climate | 17 | Ottawa (3 yr), **OttawaConst** (constant 12/28 degC, 3 yr), Alessandria7 (8 yr), Tarn-13-2019 and Tarn-22-2016 (11 yr) |
| Crops | 13 | see the matrix below |
| Soils | 6 | `Ottawa` (Ksat 1200), `DEFAULT` (500), `CLAY_LIS` (250), `SILT_LOAM_LIS` (150), `SILT_LOAM_wideTAW` (150, WP 6), `YoloClayLoam6` (**3 layers, v3.0, fine/coarse/fine**) → `SCP1` = 2, 4, 6, 8, 8, 11 |
| Irrigation | 4 | `schedule_Tarn` (manual, sprinkler), `Inet` (Inet, 20 % RAW), `IrriGen` (Generate/AllRAW/FixDepth, 4 periods, ECw 4), `IrriGenFw` (same, 50 % wetted) |
| Management | 4 | `Ottawa` (cuttings, fert. 50 %), `Ottawa2` (fert. 21 %, no cuttings), `OttawaMulch` (mulch 50 %), `TarnFert` (fert. 40 %) |
| Groundwater | 1 | `Var4` (variable, 4 obs, depth **and** EC varying, year-agnostic, v4.0) |
| Initial cond. | 4 | `DryTopSoil` (14 vol%, below germination), `SalineSoilMild` (ECe 1.0), `SalineSoil` (ECe 3.0), `WPSandLoam` (WP, **v3.2**) |
| Off-season | 1 | `example_offseason` (mulch 50/60 %, effect 75 %, 2 post-season events, 80 % wetted) |
| Calendar / OBS / PPn | 4 | `21May.CAL`, `Ottawa.OBS` (biomass only), `Ottawa.PPn`, `14-22_1px_Crop4.PPn` (byte-identical to Ottawa's) |

### Crop pool

| File | subkind | planting | mode | fertility calib. | notes |
|---|---|---|---|---|---|
| `AlfOttawaGDD` | forage | sown (perennial) | GDD | 50 % | the T0 baseline |
| `MaizeGDD` | grain | sown | GDD | 21 %-era | |
| `MaizeGDDwpy` | grain | sown | GDD | 21 %-era | == `MaizeGDD` but WPy 90 % |
| `MaizeCalwpy` | grain | sown | calendar | 21 %-era | the GUI's own calendar twin of `MaizeGDDwpy` |
| `MaizeSalinity` | grain | sown | calendar | — | salinity-sensitive: ECn 0, ECx 6, distortion 75 % |
| `MaizeSalinityGDD` | grain | sown | GDD | — | its GDD twin |
| `Maize_EUirr_GDD` | grain | sown | GDD | **30 %** | |
| `Maize_EUirr_cal` | grain | sown | calendar | **none** ("not considered") | the uncalibrated control |
| `rice_test` | grain | **transplanted** | GDD | **60 %** | v7.3 |
| `tuber` | **tuber** | transplanted | GDD | 50 % | potato, Lima |
| `tuberwpy` | tuber | transplanted | GDD | 50 % | == `tuber` but WPy 90 % |
| `veg` | **vegetative** | transplanted | GDD | 50 % | leafy |
| `DEFAULT` | grain | sown | calendar | — | the built-in fallback |

All four subkinds, both cycle modes, sown and transplanted, and four distinct
fertility calibration points (30 / 50 / 60 % plus uncalibrated) are covered.
Three matched cycle-mode pairs exist — `MaizeGDDwpy`↔`MaizeCalwpy`,
`MaizeSalinity`↔`MaizeSalinityGDD`, `MaizeGDD`↔`MaizeGDDwpy` for WPy — so C05,
C24, C25 and W03 no longer need a derived pair. The `wpy` variants differ from
their twins in exactly one field (WP during yield formation, 100 → 90 %), which
makes X10 a clean single-variable comparison.

`OttawaConst.Tnx` is the analytic anchor: at a constant 12/28 degC with method 3,
Tbase 5 and Tupper 30, every day yields exactly 15 GDD, so the GDD arithmetic in
group C can be checked by hand rather than against a reference.

**Still missing, and generated rather than requested** — no physics is being
invented in any of these:

| Script | Produces |
|---|---|
| `assets/gen_soils.py` | penetrability and gravel variants, 5-layer profiles, the Ksat = 112 boundary, low-Ksat and F-group geometry profiles |
| `assets/gen_climate.py` | decadal and monthly aggregations of the daily records, truncated and year-agnostic records |
| `assets/gen_crops.py` | sown variants of the tuber/vegetative/rice crops, calendar twins where a pair is still missing, the version-downgraded files for group VC |
| `assets/gen_management.py` | the remaining `.IRR`, `.GWT`, `.SW0`, `.OFF` and `.MAN` variants the plan calls for |
| `runner/make_inputs.py` | materialises every `cases/*/` from `case.yml` plus the asset pool |

Nothing further is needed from you on the input side. You still run
`make -C src` and `tests/runner/freeze.py`, since I do not run the compiler or
the binary on this branch.

## Suggested order

1. **Runner first** — `run_tests.py`, `freeze.py`, `make_inputs.py`, and the
   `case.yml` schema, validated against A01 (which already has a reference).
2. **Group F** — soil geometry and compartments. Highest density of untested
   arithmetic, needs only generated `.SOL` files and crop-file `Zrmax` patches,
   and it is where the "expand the compartments" bugs live.
3. **Group C** — GDD vs calendar days. The paths the look-ahead removal touched.
4. **Group Z** — the invariant checks, which then run against every case added
   afterwards and catch a whole class of problems without new references.
5. **Groups G–K** — the input files that do not exist yet.
6. Everything else, then the sweeps.
### BUG-3 — the compartment-count assertion was reading the wrong header (harness bug)

*Found by the first `run_tests.py F` replay, 2026-09-01. Not an AquaCrop defect.*

Five shallow-profile cases failed their `expect_compartments` check while their
output diffs passed. The cause was in `harness.count_compartments`, not the
model: `run.f90:4168` writes `WC01` for the first compartment and then
`'WC'//trim(Str1)` where `Str1` comes from `write(Str1,'(i2)')`, which leaves a
**leading** blank that `trim()` does not strip. Compartments 2–9 are therefore
labelled `WC 2` … `WC 9`, and only 1 and 10+ are zero-padded:

    WC01       WC 2       WC 3       WC 4  ...  WC 9       WC10       WC11       WC12

Matching `\bWC(\d{2})\b` found only `WC01` on any profile with nine or fewer
compartments, reporting 1. The twelve-compartment cases passed *by luck* — the
regex caught `WC10`/`WC11`/`WC12` and took the maximum. Fixed to `WC\s*(\d+)`.

Worth recording because of what it says about the assertion: a check that passes
can still be broken, and it was only the cases with an unusual expected value
that exposed it. All 56 group-F references now agree with the independent check across
compartment counts of 1, 3, 6, 9, 10 and 12.


### BUG-4 — a single-compartment profile writes a duplicate output column

*Found by F07, 2026-09-01. Severity: cosmetic — degenerate input. Recorded only;
the case has been removed since it cannot pass until the model changes.*

The daily-output header writer (`run.f90:4168`, and identically for `Out6CompEC`)
emits the first column unconditionally, loops over `2 .. NrCompartments-1`, then
emits column `NrCompartments`:

    call fDaily_write(trim('       WC01'), .false.)
    do Compi = 2, (GetNrCompartments()-1)  ...  end do
    write(Str1,'(i2)') GetNrCompartments()
    call fDaily_write('       WC'// trim(Str1))

With `NrCompartments = 1` the loop is empty and the final write repeats
compartment 1, so a 0.05 m profile produces two columns — `WC01` and `WC 1` —
for one compartment, with a second mid-depth of 0.53 m that corresponds to
nothing in the profile.

Only reachable with a soil profile thinner than `CompDefThick` (0.10 m), which
is not an agronomically meaningful input, hence cosmetic.

Two incidental notes on the same header, both of which the suite now handles:
compartments 2–9 are labelled `WC 2` … `WC 9` because `write(Str1,'(i2)')`
leaves a leading blank that `trim()` does not strip (see BUG-3); and the second
header row carries each compartment's mid-depth, which is what lets a case
assert the whole geometry rather than just the count.

**Fix:** at `run.f90:4168` (and identically for `Out6CompEC` just below), write
the first column only when `NrCompartments > 1`, or restructure the loop as
`do Compi = 1, NrCompartments` with the trailing-newline decision made after it.

**To revive the case after fixing:** `GEOM_0p05m.SOL` is still generated. Re-add
the `F07` row from `RETIRED` in `assets/gen_cases_F.py`, regenerate, freeze.

---

### BUG-5 — a thin first horizon terminates the crop, and `SaltStr` reads 1000 %

*Found by F44, 2026-09-01. Severity: unclear — needs a physicist's eye. Case
retired; the input is one line in `RETIRED` in `assets/gen_cases_F.py`.*

A profile whose **first horizon is 0.10 m** with a less penetrable horizon below
terminates the crop at DAP 5 and transpires nothing for the rest of the season:

    DAP 1 Stage 1 Z 0.20      Cycle     1        Tr        0.0
    DAP 2 Stage 1 Z 0.21      Rain    487.5      Tr/Trx    100
    DAP 3 Stage 1 Z 0.23      E       341.7      BioMass   0.002
    DAP 4 Stage 2 Z 0.25      TempStr  44        Y(dry)    0.000
    DAP 5 Stage -9 Z 0.29

It is **not** a question of how restrictive the lower horizon is: the case was
first written with 25 % penetrability and then softened to 50 %, and both
terminate identically. `F41` — the same 25 % penetrability but a **0.30 m**
first horizon — runs the full season normally. The distinguishing feature is the
thin top horizon, which clips the crop's `Zrmin` (0.30 m) down to the adjusted
0.20 m via `ZrAdjustedToRestrictiveLayers`.

Note also that `Z` keeps increasing after `Stage` goes to −9, and that `Tr/Trx`
reads 100 % while `Tr` is 0.0.

**Second symptom, possibly the same root cause:** the season output reports
`SaltStr = 1000` with `SaltIn`, `SaltOut`, `SaltUp` and `SaltProf` all zero.
`StrSalt` is written at `run.f90:7665`:

    if (GetRootZoneSalt_KsSalt() < 0._dp) then
        StrSalt = undef_int                       ! -9
    else
        StrSalt = roundc(100._dp * (1._dp - GetRootZoneSalt_KsSalt()), mold=1)
    end if

1000 implies `KsSalt = -9`, which should have taken the *first* branch and
printed −9. The two do not reconcile on a reading of this routine alone; the
likeliest explanation is that `KsSalt` is left at a sentinel when the root zone
is never properly evaluated, which would tie it to the termination above. Worth
confirming rather than assuming.

This is the only case in the suite that violates a group Z invariant, which is
how it was found — no one wrote a case looking for it.

### O2 — under `IrriMode_Inet` the reported `Irri` never crosses the soil surface

*Found by I22/I23 via the group Z surface-balance invariant, 2026-09-01.
Behaviour, not a defect — but a trap when reading output.*

In net-irrigation-requirement mode the season output's `Irri` column is a
**computed requirement**, not water that entered the profile. `Infilt` and
`Runoff` account for rainfall alone:

| case | % RAW | Rain | Irri | Infilt | Runoff | Rain+Irri | Infilt+RO |
|---|---|---|---|---|---|---|---|
| I22 | 20 | 487.5 | 76.8 | 473.9 | 13.6 | 564.3 | **487.5** |
| I23 | 50 | 487.5 | 16.9 | 475.5 | 12.0 | 504.4 | **487.5** |
| I24 | 100 | 487.5 | 0.0 | 475.3 | 12.2 | 487.5 | 487.5 |

I24 satisfies the balance only because its requirement happens to be zero, which
is a good illustration of why one passing case proves nothing.

Anyone totalling `Rain + Irri` across a set of runs to get water input will
over-count wherever a run used Inet mode.

**The daily balances say more.** Once `daily_soil_balance` and
`daily_surface_balance` were added, I22 and I23 were the *only* two cases out of
137 to fail either — and they fail **both**. So in Inet mode the water is not
merely absent from `Infilt`: it reaches the root zone without appearing in the
soil-water balance at all, leaving `dWC != Infilt + CR - Drain - E - Tr` by
about 5 mm. Every other irrigation mode closes to within print precision.

The three Inet cases carry
`skip_invariants: [surface_balance, daily_soil_balance, daily_surface_balance]`
with this note attached; every other mode is checked on all three.

### BUG-6 — an unbounded search over the climate dataset runs off the array or hangs

*Found by D02/D08, 2026-09-01. Severity: **hang**. Cases removed.*

Freezing the two cases staging a 10-daily `.Tnx` never returns; the other 26 in
the same batch finish in seconds. Decadal ETo and rain are fine, and so is
monthly temperature.

`TminDataSet`/`TmaxDataSet` are **module-scope** arrays of 31 entries
(`tempprocessing.f90:332`) that persist between calls and between runs. Sixteen
sites look a day up in them like this:

    case (datatype_decadely)
        if (RunningDay > TminDataSet(31)%DayNr) then
            call GetDecadeTemperatureDataSet(RunningDay, TminDataSet, TmaxDataSet)
        end if
        i = 1
        do while (TminDataSet(i)%DayNr /= RunningDay)
            i = i+1
        end do

Two things combine:

1. **The refill guard is one-sided.** It reloads only when `RunningDay` is
   *past* the loaded period. A request for an *earlier* day leaves whatever was
   loaded before in place.
2. **The search is unbounded.** No `i <= 31` test. If the requested day is not
   in the loaded window, `i` walks straight off the end of a 31-element array
   and keeps comparing whatever memory follows until something happens to equal
   `RunningDay`.

`GetDecadeTemperatureDataSet` fills slots `1..ni` with the decade's days and
pads `ni+1..31` with the decade's *last* day, so `TminDataSet(31)%DayNr` is the
end of the loaded decade and the window is only `ni` days wide.

**That is why monthly survives and decadal does not.** The same one-sided guard
is used for both, so both tolerate a backward jump only as far as the loaded
window reaches — about 30 days for a month, but only 8 to 11 for a decade. A
backward step that a monthly dataset absorbs falls outside a decadal one.

The bug is in the access pattern, not in the file: any 10-daily temperature
record is exposed, and the generated `OttawaDec.Tnx` is not implicated. It
presents as a hang rather than a crash because the walk is a linear scan through
adjacent memory, not a wild pointer.

**Fix:** bound every one of the sixteen searches (`i <= 31`, else refill and
fail loudly), and make the guard two-sided:

    if (RunningDay > TminDataSet(31)%DayNr .or. RunningDay < TminDataSet(1)%DayNr)

**Generalised by D23, 2026-09-10.** This was written up as a decadal-reader
problem. It is not: the defect is the search itself, and a *daily* record
reaches it too. D23 gives a climate file holding one single day and a
simulation period of that same day; the run hangs before writing a byte of
output — all three output files sat at zero bytes — and had to be killed.

There are **fourteen** of these loops in `run.f90`, and not one bounds `i`:

    run.f90:3797, 3835                     Tmin dataset
    run.f90:5648, 5657, 5695, 5706         ETo dataset
    run.f90:5748, 5757, 5795, 5806         Rain dataset
    run.f90:5856, 5869, 5918, 5934         Tmin dataset

Each has the shape `do while (GetXDataSet_DayNr(i) /= TargetDay)` with `i`
incremented inside. The datasets hold 31 entries. When the day being sought is
not among them — because the record is shorter than the window, or because the
window has moved past it — the loop either spins forever or walks off the end,
depending on what follows in memory.

**Fix:** bound every one of the fourteen by the dataset size and fail with a
message naming the day that could not be found.


### BUG-9 — evaluation against field data crashes on a single-run `.PRM`

*Found by O25–O31, 2026-09-03. Severity: crash on a supported configuration.*

A project that requests evaluation (`ParticularResults.SIM` line 2) and supplies
an `.OBS` file dies with:

    At line 147 of file inforesults.f90
    Fortran runtime error: Cannot open file 'SIMUL/EvalData1.OUT': No such file

The writer and the reader disagree about the filename. `CreateEvalData`
(`run.f90:5494`) suffixes it with the run number only for a genuine multi-run
project:

    if (GetSimulation_MultipleRun() .and. (GetSimulation_NrRuns() > 1)) then
        write(StrNr, '(i0)') NrRun
    else
        StrNr = ''
    end if                            ! -> writes SIMUL/EvalData.OUT

`CloseEvalDataPerformEvaluation` (`run.f90:4339`) starts with the same test but
then **unconditionally overwrites** `StrNr` inside the `.PRM` branch:

    StrNr = ''
    if (GetSimulation_MultipleRun() .and. (GetSimulation_NrRuns() > 1)) then
        write(StrNr, '(i0)') NrRun
    end if
    select case (TheProjectType)
    case(typeproject_typeprm)
        write(StrNr, '(i0)') NrRun    ! <- back to '1'
    end select                        ! -> reads SIMUL/EvalData1.OUT

So the mismatch appears for exactly one configuration: a `.PRM` with a single
run. `A01` survives because it has three runs and both sides then agree; a
`.PRO` survives because its branch leaves `StrNr` empty on both sides.

**Fix:** delete the unconditional `write(StrNr, ...)` in the `typeproject_typeprm`
branch, or give `WriteAssessmentSimulation` the same suffix the writer used.

The six evaluation cases are built as `.PRO` projects so they can test what they
are named for. A case pinning BUG-9 itself would have to fail, so there isn't one.

### BUG-10 — a year-agnostic climate record segfaults when the project names real years

*Found by D17/D18, 2026-09-03. Severity: segfault on a malformed but plausible
project. Mechanism identified; the fix is a judgement call.*

A climate record whose first year is 1901 — the documented marker for "not
linked to a specific year" — segfaults when the `.PRM` states its simulation
period in a real year. Both a three-year and a **canonical 365-day** 1901 record
crash, so record length is not the issue, as first supposed.

The design intends the crop to be moved onto the record's year.
`AdjustCropYearToClimFile` (`global.f90:6573`) rewrites the crop dates with
`yeari = GetClimRecord_FromY()`, which is 1901, and `AdjustClimRecordTo`
(`global.f90:6289`) then extends the record's end to 31 December 1901. But the
**simulation period** comes from the project file and is left alone. After the
adjustment the crop and the climate sit in 1901 while the simulation window is
still in 2014 — about 41 000 day-numbers away. Indexing a 365-value record over
that span reads far outside it.

So a year-agnostic record carries an undocumented requirement: the project must
be dated in 1901 as well. The GUI presumably writes such projects itself, which
is why this has not surfaced before.

**The defect is the missing check, not the coupling.** A project whose dates do
not match a year-agnostic record should be rejected with a message rather than
running into an out-of-range read. `AdjustSimPeriod` is the natural place, and
the same guard would cover the reverse mistake.

D17 is rebuilt with a matching 1901 project to confirm the supported combination
works; D18 (a year-agnostic record used for a 2016 season) is dropped, since it
is precisely the unsupported combination this defect describes.

### BUG-11 — a project list containing a blank line crashes

*Found by A07, 2026-09-04. Severity: crash on malformed input.*

`LIST/ListProjects.txt` holding nothing but a blank line ends the run with:

    At line 508 of file startunit.F90
    Fortran runtime error: End of file

The reader makes two passes (`startunit.F90:498-510`). The first counts records
with `read(fhandle, *, iostat=rc)` and no I/O list, which simply advances over a
blank record, so a file of one blank line counts as **one project**. The second
pass then does `read(fhandle, *) buffer` — a list-directed read of a character,
which skips blank records looking for a value, runs off the end of the file, and
has no `iostat` to catch it.

**Fix:** count only records that yield a non-empty name, or give the second read
an `iostat` and stop with a message.

### BUG-12 — four of six input loaders open their file without checking it exists

*Found by A09 and A10, 2026-09-04. Severity: crash on a missing input file.*

A project naming a crop or soil file that is not on disk dies with a bare
Fortran error:

    At line 4900 of file global.f90
    Fortran runtime error: Cannot open file './DATA/Ghost.CRO': No such file or directory

    At line 7738 of file global.f90
    Fortran runtime error: Cannot open file './DATA/Ghost.SOL': No such file or directory

The codebase already has the right pattern — it is just applied inconsistently:

| loader | `inquire(file=...)` before opening |
|---|---|
| `LoadOffSeason` | **yes** |
| `LoadGroundWater` | **yes** |
| `LoadCrop` | no |
| `LoadProfile` | no |
| `LoadIrriScheduleInfo` | no |
| `LoadManagement` | no |

`ListProjectsLoaded.OUT` already has a vocabulary for reporting missing
environment files, so the graceful path exists too; the four unguarded loaders
simply never reach it.

**Fix:** add the same `inquire` guard the other two use, and report through
`ListProjectsLoaded.OUT`.

### BUG-13 — a listed project that does not exist still produces output files

*Found by A08, 2026-09-04. Severity: spurious output; the run itself completes.*

A `ListProjects.txt` naming `Ghost.PRM` alongside a real project does **not**
crash — that path is handled. But the run leaves two extra files behind, named
with an empty project prefix:

    PRMday.OUT       91 bytes
    PRMseason.OUT   797 bytes

Both contain a header and no data rows. AquaCrop has partially initialised the
output for a project it could not load, rather than skipping the entry. Anything
collecting `OUTP/*.OUT` afterwards picks up two empty files whose names identify
no project.

A08 is kept as a passing case: it pins the current behaviour, so if the spurious
files stop being written the reference will move and say so.

### BUG-14 — the year-undefined groundwater branch reads past end of file

*Found by G13, 2026-09-04. Severity: crash. Cases removed.*

A `.GWT` with an undefined year (1901) whose observations begin after the
simulation period ends the run with:

    At line 6226 of file global.f90
    Fortran runtime error: End of file

The loop cannot terminate as written (`global.f90:6222-6231`):

    do while (rc /= iostat_end)
        read(fhandle, '(a)') StringREAD      ! <- no iostat= clause
        call SplitStringInThreeParams(StringREAD, DayDouble, Z1, EC1)
        DayNr1 = DayNr1Gwt + roundc(DayDouble, mold=1) - 1
    end do

The condition tests `rc`, but the `read` inside never assigns it — there is no
`iostat=` on that statement. `rc` keeps whatever value it had before the loop,
so the only way out is for the read itself to hit end-of-file, which is a fatal
error rather than an exit. The loop is written as though it were draining the
file, and instead it always overruns it.

**Fix:** add `iostat=rc` to the read, which is what the surrounding code does
everywhere else in the same routine.

### BUG-15 — a crop that can never accumulate growing degrees hangs

*Found by C13 and N22, 2026-09-04. Severity: hang. Cases removed.*

Two configurations make daily GDD identically zero, and both spin:

| case | how | result |
|---|---|---|
| C13 | `Tbase` = `Tupper` = 20 degC | hang |
| N22 | no temperature file, default air 2/6 degC against `Tbase` 8 | hang |

The model already knows this is a hazard. `SumCalendarDays`
(`tempprocessing.f90:1146`) guards the no-temperature-file branch explicitly:

    DayGDD = DegreesDay(...)
    if (abs(DayGDD) < epsilon(1._dp)) then
        NrCDays = -9

but the branches that walk a temperature record do not. Their loop is

    do while ((RemainingGDDays > 0) &
        .and. ((DayNri < GetTemperatureRecord_ToDayNr()) .or. AdjustDayNri))

and the `.or. AdjustDayNri` disjunct **disables the record-end bound**. When
`DayGDD` is always zero `RemainingGDDays` never decreases, so with the bound
disabled there is no exit at all.

**Not fully pinned:** N22 goes through the `(None)` path that carries the guard,
so its hang is somewhere the guard does not cover, and that exact site has not
been identified. C13's mechanism is clear.

**Fix:** apply the same zero-GDD test the `(None)` branch already uses before
entering any accumulation loop, and bound every loop by the record length
regardless of `AdjustDayNri`. This is the third unbounded search in the
temperature code, after BUG-6.

### BUG-16 — running past the end of the climate record crashes

*Found by D20, 2026-09-04. Severity: crash. Case removed.*

A simulation period extending beyond the last day of the climate record ends
with:

    At line 5685 of file run.f90
    Fortran runtime error: End of file

Starting *before* the record does not crash, so at the time this looked like the
trailing edge alone. It is not: D19 ran, but on weather shifted by 61 days. See
BUG-24.

The reads that skip the file header all carry an `iostat`, and the two that read
the actual value do not (`run.f90:5676-5686`):

    read(fETo, *, iostat=rc) ! day
    read(fETo, *, iostat=rc) ! month
    read(fETo, *, iostat=rc) ! year
    read(fETo, *, iostat=rc)
    read(fETo, *, iostat=rc)
    read(fETo, *, iostat=rc)
    read(fETo, *) ETo_temp          ! <- no iostat
    ...
    read(fETo, *) ETo_temp          ! <- no iostat

This is the same shape as BUG-14: the status is captured everywhere except where
running out of data is actually possible.

**Fix:** give both value reads an `iostat` and stop with a message naming the
record's last day. Better still, reject a simulation period that extends past
the record when the project is loaded, which is where the user can act on it.

### BUG-17 — `LoadOffSeason` reads into an unallocated deferred-length string

*Found by Z18, 2026-09-04; reconfirmed by the full Z19 run, 2026-09-10.
Severity: undefined behaviour that the production build survives by luck.*

Every off-season case — K06, K07, K08, K09, K10, K11, K12, K13, K14 — dies under
a checked build at `tempprocessing.f90:2483`, which is the call to
`LoadOffSeason`. The cause is in the loader (`global.f90`):

    character(len=:), allocatable :: ParamString      ! :5900, never allocated
    ...
    read(fhandle, '(a)') ParamString                  ! :5959
    read(fhandle, '(a)') ParamString                  ! :5968

Reading into an unallocated deferred-length character is undefined in Fortran.
The production build happens not to fall over; `-fcheck=all` reports it
immediately. Those two reads are the irrigation-event lines, so any `.OFF` file
declaring events goes through them.

All nine cases failed again in the complete Z19 run (`DEBUG=1`, which is
`-O0 -fcheck=all`), which makes this the largest single group of failures in the
suite under any non-production build. It is nine of the ten errors in that run;
the tenth is BUG-19.

**Fix:** declare it `character(len=1025)` like `StringREAD` in the neighbouring
loaders, or allocate before reading.

### BUG-18 — floating-point exceptions in the daily loop

*Found by Z18 (`-ffpe-trap=invalid,zero,overflow`), 2026-09-04. Severity:
latent; production silently absorbs these.*

Sixteen cases raise SIGFPE under a trapping build while passing in production.
Eleven of the sixteen are one bug.

| trigger | cases | faulting site |
|---|---|---|
| capillary rise from a water table | U02, U03, U04, U05, U08, U12, U13, U14, U16, V11 | `simul.f90:2167`, `calculate_CapillaryRise` |
| a cold season, GDD floor binding | C29, P10 | below `simul.f90:3455` (callee inlined) |
| `Soil_RootMax` = 0 | F37 | below `run.f90:6621`, `AdjustedRootingDepth` (callee inlined) |

**Root cause of the capillary-rise group.** The driving-force guard in
`calculate_CapillaryRise` (`simul.f90:2157`) admits equality:

```fortran
if ((GetCompartment_theta(compi) >= GetSoilLayer_WP(...)/100._dp) .and. ...) then
    DrivingForce = 1._dp - (exp(n * log(theta - WP/100._dp)) / exp(n * log(FCadj/100._dp - WP/100._dp)))
else
    DrivingForce = 1._dp
end if
```

When a compartment sits *exactly* at wilting point, `log(0.0)` raises
divide-by-zero. That is precisely what these cases set up: U02 stages
`SW0_atWP.SW0` on a soil whose WP is 13.0 vol %, so `theta - WP/100` is
identically zero on day 1.

*Correction, 2026-09-10:* Q16 was listed here as an eleventh case in this group.
It does not belong. Its frame `#2` is `simul.f90:5545`, which is the second line
of the `calculate_saltcontent` call, not the `calculate_CapillaryRise` call four
lines above it — a misreading on my part. Q16 is a separate out-of-bounds read,
written up as BUG-19.

**The fix is `>=` → `>`, and it cannot move a number.** At `theta == WP` the
formula evaluates to `1 - exp(-inf)/D` = `1 - 0/D` = `1`, which is exactly what
the `else` branch already assigns. Strict inequality routes the degenerate case
to the branch that was always producing its value anyway.

*Second hazard in the same expression, unfixed:* the denominator
`log(FCadj/100 - WP/100)` is unguarded. A soil with `FCadj == WP` gives
`exp(-inf)` = 0 and then a genuine division by zero — and unlike the numerator,
that one does not recover. No case in the suite constructs it; it is reachable
from a hand-written `.SOL` with `FC == WP`.

**These do not fail in production.** An invalid or divide-by-zero operation
produces a NaN or an infinity that later arithmetic absorbs, and the group Z
invariants confirm the published numbers still balance. That is what makes them
worth fixing rather than urgent: the results are not visibly wrong, but they
depend on IEEE fallback behaviour rather than on the model deciding what to do.

**Next step:** rebuild with `-ffpe-trap` and run one case under `gdb` to capture
the frames below `run.f90:7141`. The capillary-rise group is the place to start,
since ten of the sixteen sit there.

*Update, 2026-09-18 (fix branch).* Three of the four groups are fixed, and the
backtraces came from `check_builds.sh fpe --src`, without gdb:

* **C29, P10** — `DetermineGDDCGCadjusted` (`simul.f90`) computes the depletion
  of the root zone as `(FC - actual)/(FC - WP)`. Before the crop has roots all
  three are zero, so this is 0/0. The four places that compute it (canopy growth
  and senescence, in the GDD and the calendar version) now share one function,
  `RelativeDepletion`, which returns 0 when the zone holds no water, exactly as
  `DetermineRootZoneWC` already did. No number changes: in that situation the
  value was never used, the NaN was simply carried along.
* **F37** — `AdjustedRootingDepth` (`rootunit.f90`) corrects root water
  extraction with `ZiMax/Zi`, and `Zi` is zero in a soil that is impermeable
  from the surface. With no roots there is nothing to correct, so the factor
  stays 1.
* **The capillary-rise group (U02-U16, V11)** is unchanged here: PR #384 fixes
  it (`theta == WP`, see above).

A full `fpe` run on the fix branch now reports those ten cases and nothing
else.


### BUG-25 — the default CO2 record is read at start-up, and is stale across runs

*Found while considering PR #379, 2026-09-25. Severity: one silently wrong
result, one crash. Fixed by PR #379, merged 2026-10-13.*

`InitializeSettings` (`initialsettings.f90` 5.4) sets the CO2 file to
`MaunaLoa.CO2` once, at start-up, and reads it there to build its description.
`LoadSimulationRunProject` (`tempprocessing.f90` 1.4) then overwrites it only
when a run *names* a file; a run naming `(None)` changes nothing. Two
consequences:

**1. A run inherits the previous run's CO2 record.** D30 is a two-run project:
run 1 names `FlatCO2.CO2`, run 2 names none. Run 2 should fall back to the
default, MaunaLoa at about 400.8 ppm for 2015. It does not - it keeps
FlatCO2 at 369.41 ppm, which is exactly the reference concentration at which
AquaCrop applies no CO2 adjustment at all. So the run quietly loses its CO2
effect, and only the season output's CO2 column shows it:

        run 1   CO2 369.41 ppm   biomass 28.091
        run 2   CO2 369.41 ppm   biomass 26.797     <- should be MaunaLoa 2015

D29 shows the single-run case is fine: with no CO2 named at all, the default is
used (398.82 ppm for 2014).

**2. A missing default record kills a run that does not want it.** Because the
read happens at start-up, a project naming its own CO2 file still dies when
`SIMUL/MaunaLoa.CO2` is absent - `GenerateCO2Description` (`global.f90`:3249)
opens it with `status='old'` and no `iostat`:

        At line 3249 of file global.f90
        Fortran runtime error: Cannot open file 'SIMUL/MaunaLoa.CO2'

Same shape as BUG-12 and BUG-16: the loader reads a file it has not checked,
and the user gets a backtrace rather than a message.

**Fix:** PR #379 sets `(None)` at start-up and resolves the default per run in
`LoadSimulationRunProject`, which addresses both halves. It was merged into
`main` on 2026-10-13 and reached the suite with the rebase of
`feature/lis_external`.

**Both cases are now live.** D30 moved exactly as predicted when the PR was
tested in September - run 2 goes from the stale FlatCO2 369.41 ppm to the
default record's 401.02, dry yield 50.1 to 52.5 - and was re-frozen on
2026-10-13. It is the only case in the suite the fix moves. D31 was held back
while the defect was open, because without the fix it does not fail, it dies;
it is now generated and frozen, and guards the start-up read: a run naming its
own CO2 file must not need `SIMUL/MaunaLoa.CO2` to exist (369.41 ppm, biomass
28.091, the same as D25).

### BUG-24 — a period starting before the climate record runs on shifted weather

*Found by D19 while reading the GDD-native run, 2026-09-23. Severity: silently
wrong results. Fixed on `fix/7.4_bug_fixes` (05271db).*

`CheckClimateRecordsCoverSimPeriod` (`global.f90`) guarded only the trailing
edge, the one BUG-16 was about. A run whose simulation or cropping period starts
*before* the record's first day does not crash: it starts reading at that first
day, so every day of the run is served the weather of a day later in the year, by
however much the period was short.

D19 simulates from 1 November 2013 against an Ottawa record that begins on
1 January 2014 — 61 days short. Its maize is sown on 21 May and grows, flowers
and fills grain on the record's weather from 21 July onwards. By 11 August it is
being fed 11 October (Tmin 2.4, Tmax 11.9 against the day's real 15.8 / 27.4),
and on 31 August, its last day, −0.5 / 7.3. Growing degrees fall to zero in
midsummer and the crop finishes in a frost that is two months away.

Nothing in the output says so. The case was frozen, passed for weeks, and its
reference records the shifted run.

**Fix:** the leading edge now stops with the same kind of message as the trailing
one, naming the day the period starts and the day the record does. D19 becomes a
must-stop case; its reference is deleted.

### BUG-23 — values read before they are set

*Found by a sweep of the whole suite on a build with `-finit-real=snan`
(`check_builds.sh snan`), 2026-09-18. Severity: undefined behaviour that the
production build survives because the memory happens to hold zero.*

With every uninitialised real filled with a signalling NaN, 840 of the 851
cases stopped. They came from four places only:

| where | cases | value |
|---|---|---|
| `simul.f90`, water balance of the day | 770 | `TactWeedInfested` |
| `simul.f90`, generating an irrigation event | 38 | `Epot`, `Tpot` |
| `simul.f90`, USDA effective rainfall | 21 | `Epot`, `Tpot` |
| `simul.f90`, upward salt transport in the evaporation layer | 1 | `ThetaIniEvap` |
| `calculate_CapillaryRise` | 10 | BUG-18, fixed by PR #384 |

`Eact`, `Epot`, `Tact`, `Tpot` and `TactWeedInfested` are module variables
with no initial value, read on a day before anything sets them: the water
balance of a day before the crop, or an irrigation event generated before the
day's evaporation and transpiration are known. They read as zero only because
the compiler puts them in zeroed memory. They are now declared with 0.

`ThetaIniEvap` holds the water content of each compartment in the evaporation
layer before evaporation. Its slots were filled by a loop that stops at the
bottom of that layer, while the salt-transport step below compares against a
slot the loop may never have reached. Every slot is now filled first.

**Nothing changed in the output** (the whole suite passes unchanged), which is
what the values being zero in practice predicts. Fixed on
`fix/7.4_fixes_testsuite`; a `snan` sweep now reports only the ten
capillary-rise cases of BUG-18.

### BUG-22 — soil evaporation uses an unset reduction coefficient at a 15 cm evaporation layer

*Found by N16a/S07 through the `snan` build (`-finit-real=snan`), 2026-09-18.
Severity: wrong output; the results of such a project depend on the build.
This is the cause of what O8 saw and withdrew.*

`CalculateSoilEvaporationStage2` (`simul.f90`) computes the evaporation
reduction coefficient `Kr` **inside** the test that deepens the evaporation
layer:

```fortran
if (GetSimulParam_EvapZmax() > EvapZmin) then          ! EvapZmin = 15 cm
    do while (...)            ! deepen the layer by 1 mm at a time
    end do
    Kr = SoilEvaporationReductionCoefficient(Wrel, ...)
end if
...
Elost = Kr * (Eremaining/NrOfStepsInDay)
```

With the evaporation depth set to its lowest value, 15 cm, the layer cannot
deepen, the branch is skipped, and `Kr` is used without ever being given a
value. The production build takes whatever is in memory, so the run is not
reproducible across builds: N16a and S07 passed under `-O0` and failed under
`-O0` plus `-ffpe-trap`, and the `snan` build stops at the multiplication
itself (`simul.f90:4542`).

**It is a translation slip.** In the Pascal (`Simul.pas:3165-3175`) the
`IF (SimulParam.EvapZmax > EvapZmin) THEN` has no `BEGIN…END`, so it guards
only the `WHILE` loop; the `Kr :=` line follows it and always runs. Nothing to
report for the GUI.

**Fix:** move the `Kr` line out of the `if`, as in the Pascal. A deeper
evaporation layer is unaffected (`Kr` is computed from the same `Wrel` after
the loop). With the fix, N16a and S07 give season `E` 110.3 mm and `Drain`
87.3 mm, the same as the 25 cm (SW11) and 45 cm (N16b) cases, instead of
63.9 / 130.8. Their references were re-frozen.

### BUG-19 — the salt-cell loop walks off the bottom of the array

*Found by Z19 (`DEBUG=1`, which is `-O0 -fcheck=all`), 2026-09-10. Severity:
out-of-bounds read; production returns whatever sits before the array.*

`Q16` (drainage through a gravelly profile) dies under bounds checking:

    Fortran runtime error: Index '0' of dimension 1 of array
    'compartment...%salt' below lower bound of 1
    at global.f90:15278, from calculate_saltcontent (simul.f90:2507)

`calculate_saltcontent` walks down the salt cells of a compartment while
draining it (`simul.f90:2499`):

```fortran
do while (DeltaTheta > 0._dp)
    if (celi < GetSoilLayer_SCP1(...)) then
        limit = (celi-1._dp)*Dx
    ...
        SaltOut = SaltOut + GetCompartment_Salt(compi, celi) + ...   ! :2506
    ...
        celi = celi - 1                                              ! :2522
```

The loop condition is on `DeltaTheta`, and `celi` is decremented with **no lower
bound**. Once `celi` reaches 1 and is decremented again, the next iteration
reads cell 0. It also makes `limit` = `-Dx`, i.e. negative, so `DeltaTheta` need
not have been exhausted and the loop can keep going.

The authors knew this variable can reach zero. Fifty lines earlier there is a
guard with their own note on it (`simul.f90:2447`):

```fortran
if (celi == 0) then
    celi = 1  ! XXX would be best to avoid celi=0 to begin with
end if
```

That guard covers the entry path only; the decrement in the drainage loop
escapes it. The natural fix is to add `celi > 1` to the loop condition, or to
repeat the clamp after the decrement — but unlike BUG-18's capillary-rise fix,
**this one may move numbers**, because the out-of-bounds value currently feeds
into `SaltOut`. It needs the frozen references re-checked, not assumed.

### BUG-20 — a wide column label silently produced an empty series (harness bug)

*Found while implementing Z03, 2026-09-10. Severity: harness only, but it is
exactly the shape of BUG-3 -- an assertion that passes because it never ran.*

`daily_column` locates a column by matching the right edge of its header label
against the right edge of each value, and accepted a match only within three
characters. `Salt(3.05)` is ten characters wide over values like `26.938`,
which is six: the label's edge sits four characters right of the value's, so
every row was rejected and the function returned an **empty list** rather than
failing. Any check built on it would have passed vacuously.

Two fixes, in `harness.py`:

* `_EDGE_SLACK` is 5, the widest label-minus-value overhang in any output
  block. Re-extracting every column of all 561 frozen daily references at the
  old and new slack gives 3517 series identical, 2 newly populated and **0
  changed**, so nothing that already worked moved.
* `daily_column` now raises when the label IS in the header but no value lines
  up with it. A column genuinely absent from the header still returns empty,
  because that means the case did not request that output block -- the
  difference between a check that cannot run and a check that silently passes.

### BUG-21 — the program-parameter loader reads 25 values without `iostat`

*Found by N02 during the freeze of 2026-09-10. Severity: a short `.PPn` aborts
the run with a Fortran runtime error instead of a message.*

A `.PPn` holding only its first five records kills the run:

    At line 815 of file startunit.F90
    Fortran runtime error: End of file

`LoadProgramParametersProjectPlugIn` (`startunit.F90:794-920`) performs **25
list reads, none of which carries an `iostat`**. Line 815 is the sixth,
`read(f0, *) simul_SFR`, so a file one record short of complete is enough. The
loader has already established the file exists; what it never establishes is
that the file is long enough.

This is the same family as BUG-12 (loaders that open without checking) and
BUG-16 (value reads without `iostat`): the file is trusted once opened.

**Fix:** an `iostat` on each read, falling back to the built-in default for any
record not present — which is exactly what the `else` branch of this same
routine already does when the file is missing altogether.

*Noticed in passing:* record 5 is read into `simul_RZEma` and then discarded on
the next line by `SetSimulParam_MaxRootZoneExpansion(5.00_dp)`
(`startunit.F90:811-813`), the same dead-input pattern as `IniAbstract` under
N27. Two of the 25 records in a `.PPn` cannot influence a run.

### BUG-8 — salt solubility above 127 g/l aborts the run

*Found by L15, 2026-09-01. Severity: input range — a physically ordinary value
cannot be expressed.*

`SimulParam%SaltSolub` is declared `integer(int8)` (`global.f90:537`), so it can
hold 127 at most. Setting the `.PPn` salt-solubility record to 500 aborts on the
read at `startunit.F90:862`:

    At line 862 of file startunit.F90
    Fortran runtime error: Integer overflow while reading item 1

The default is 100 g/l, comfortably inside the range, which is presumably why
this has never surfaced. But the solubility of NaCl is about **360 g/l**, so a
user entering a realistic figure for a sodium-dominated soil gets a raw Fortran
runtime error with no indication that the field has a ceiling.

Of the six `int8` fields in `rep_simulparam` — `IrriFwInSeason`,
`IrriFwOffSeason`, `SaltDiff`, `SaltSolub`, `RootNrDF`, `IniAbstract` — this is
the only one whose natural units run past 127; the rest are percentages or small
shape factors.

**Fix:** widen `SaltSolub` to `int32`, or validate the record on read and stop
with a message naming the limit.

L15 now uses 127, the highest representable value, which both keeps the case's
intent and pins the ceiling.

### BUG-7 — `C2Max = C2Max` in the decadal temperature reader

*Found while reading the code for BUG-6. Severity: low; reachable only when the
record holds a single observation.*

`tempprocessing.f90:544`, in the `NrObs == 0` branch of `GetSetofThree`:

    C2Min = C1Min
    C2Max = C2Max      ! <- self-assignment; every sibling reads C1Max
    C3Min = C1Min
    C3Max = C1Max

`C2Max` is left uninitialised. The same block at `:561`, `:574`, `:750` and
`:768` reads `C2Max = C1Max`, so the intent is unambiguous.

### O3 — initial surface storage is discarded unless the field has bunds

*Found by S22 via the group Z surface-balance invariant, 2026-09-01. Behaviour,
not a defect.*

The `.SW0` record "water layer (mm) stored between soil bunds (if present)" is
only honoured when the management file actually declares bunds. Three cases
stage the same `SW0_surfstore.SW0`, which specifies 20 mm:

| case | bunds | initial 20 mm | Infilt | Rain+Irri vs Infilt+RO |
|---|---|---|---|---|
| S22 | 0.30 m | **retained** | 507.5 = Rain + 20 | −20.00 |
| H11 | none | **discarded** | 476.6 | 0.00 |
| H12 | none | **discarded** | 476.6 | 0.00 |

The parenthesis in the field's own label says as much, and it is physically
reasonable — water cannot pond on an unbunded field. But nothing in the output
reports that 20 mm of declared input was dropped, so a project can silently lose
it.

Two consequences for the suite. The surface-balance invariant needed a third
input term: ponded water at the start appears in no season-output column, so
cases carry `surface_storage_in` and the check becomes
`Rain + Irri + stored == Infilt + Runoff`. With that, S22 balances to the cent.
And H11/H12, which were written to test ponded initial conditions, in fact
document the *discarded* path — worth keeping, but not what their names suggest.

## Why the references moved on 2026-10-19 - single precision

`ac_kinds` now defines one real kind, `sp` (real32). Every real in the model is single
precision, as in LIS. All 853 references were re-frozen against that build.

**How it was checked.** The run used `--ulp 1`, which accepts a cell that differs by at most
one unit in its last printed digit. A single-precision operation is about 1e-7 relative, so one
operation can never move a column printed to 0.1: only accumulation or a flipped branch can.
Result: 67 cases bit-identical, 761 within one digit, 25 beyond. Those 25:

| cases | what differs | cause |
|---|---|---|
| D07, I07, Q05, T14, F51, N14, P04, W14, Y09, V13, P01 | one integer percentage out by 1 (`E/Ex`, `ET/ETx`, `Tr/Trx`, `StSto`, `StSen`) | the column is `roundc(100*ratio)`; a value at x.5 rounds the other way. Integer tokens get no tolerance from `--ulp`, so these show as failures |
| D11, D12, F30, F23, V03 | a flag flipping between -9 and 0, or a cell beyond one digit | the only seed is a last-digit difference in `GD` or in an interpolated temperature, which then accumulates |
| G17 | day 1: `CR 1.2 -> 0.0`, `Drain 0.0 -> 0.1` | its bottom compartment starts exactly at field capacity, so the sign of `FCadj - theta` decides between capillary rise and drainage |
| S10 | day 1: profile water content 355.6 -> 354.6 mm | the layer a compartment belongs to, where its midpoint sits on a horizon boundary (`depthi <= depth + ac_zero_threshold`) |
| U02, U03, U04, U05, U08, U12, U14 | capillary rise starts 9 days earlier; season `Upflow` 42.6 -> 93.4 mm | see below |

**The capillary-rise group.** `calculate_CapillaryRise` gives a compartment sitting exactly at
wilting point a minimal conductivity, because `Krel` is zero there and a zero transfer would
stop the rise from ever starting. That seed is self-amplifying: next day
`Krel = (theta-WP)/(theta_thresh-WP)`, so for Ottawa.SOL (WP 13 %, FC 29 %, 0.1 m
compartments, `MaxMM` 99 mm) it grows about 13-fold a day. The seed was 1.0e-16, which is
seven units in the last place of a double but is lost entirely when added to a single-precision
theta of 0.13, so it is now `ac_zero_threshold` (1e-6). The start of capillary rise follows the
size of the seed:

        1e-16 -> 0.08 : log(8e14)/log(13.4) = 13 days   (the old references)
        1e-6  -> 0.08 : log(8e4) /log(13.4) =  4 days   (now)

Both numbers are numerical, not physical. The alternative - dropping `Krel = 0` at wilting
point, so the rise starts at the rate the soil allows on day 1 - would make the result the same
in either precision, but it contradicts the v7.3 reference manual, so the formulation stays as
it is and the references record the four-day start.

Which cases this can touch is bounded by the test itself: the seed needs theta exactly at
wilting point, a water table within reach, and fresh water. Eleven cases start at wilting point
with a water table; the seven above are the ones with a fresh table and a soil quick enough to
bootstrap inside the season. U13, U16 and V11 have a saline table and U11 is clay, and all four
have no capillary rise at all in either build.

**One systematic change beyond the kinds.** The 88 `epsilon()` comparisons in the model are now
`epsilon(0._sp)`, about 1e9 looser than before. That is what makes tests like "theta is exactly
at wilting point" work at all in single precision, but it loosens every one of them, not only
the six places where a tolerance was set to `ac_zero_threshold` deliberately
(`KsTemperature`, the root-extraction shape, the compartment regrade loop, the layer
assignment, and the two in capillary rise).

**From now on** the references are single precision, so runs compare exactly: `--ulp` was for
this migration only.

## Why the references moved on 2026-09-30

The soil fertility - biomass relationship is now calibrated on **eleven** stress
levels (0, 10, ... 100 %) instead of eight (0 ... 70), and the daily adjustment of
that stress may reach 99 % instead of stopping at 80. Both come from the RSDA LIS
branch, where the relationship has to answer for a relative biomass anywhere in
its range: a curve fitted on 0-70 % was extrapolating below that, and the stress a
run is given is read straight off it. `StressBiomassRelationshipForTnxReference`
(`preparefertilitysalinity.f90`) and `SFadj_max` (`simul.f90`).

**27 references moved, and every one of them is alfalfa** - `AlfOttawaGDD.CRO` at
50 % soil fertility stress: A01, J29-J43b, the ten SW10 cutting sweeps, W16, W17.
No annual case moved at all, not even within tolerance: J11 (maize, 75 %), P13,
P15, SW05_fert75 and the fert25/50/100 family are bit-identical. The reason is the
crop file: alfalfa's shape factor for the response of water productivity to soil
fertility stress is **-0.16**, near-linear, so a small shift in the stress read off
the curve passes undiminished into the WP reduction. Maize's is -1.47, convex
enough to damp the same shift below 1e-3.

The extended curve returns a **higher** stress for a given relative biomass, so
these runs are slightly more stressed than they were:

| what moved | how far |
|---|---|
| WP (daily, col 39) | 8.1 -> 7.7, at worst 4.94 % - the largest change anywhere |
| WPet (SW10 seasons) | <= 1.1 % |
| cut yields, season biomass | 0.1 - 0.2 % |
| `StExp` flipping 0 <-> -9 | 0-3 lines per case |

The `StExp` flip is a flag, not a number, which is why the comparison reports it
as 100 %: the canopy now stops at its stress ceiling (`CCiActual > CCxSFCD` in
`DetermineCCi`) instead of climbing through it, and a canopy at its ceiling has no
expansion stress to report. In W16 that is CC 55.0 -> 55.7 rising against CC 54.9
-> 54.7 clipped.

**Nothing structural moved**: every cut in all 17 cutting cases falls on the same
day as before, and every season row starts on the same date. `SFadj_max` is not
visible anywhere in the suite - the daily adjustment is clamped to the management
file's own stress (50 %) long before it could reach 80, let alone 99.

The fpe build was run on the new code before freezing, because the eleven-point
loop calls `Bnormalized` at 80, 90 and 100 % stress - where the canopy is reduced
away entirely and `SumKcTopSF` is zero - and no run had ever reached that path
before. It traps nothing; the report lists the same 27 cases and the five known
monthly-record cases within tolerance.

**The Pascal GUI has not followed**: `PrepareFertilitySalinity.pas` still declares
`StressMatrix : ARRAY[0..7]` and `Simul.pas` still caps at 80, in 7.3 and in the
August 2026 copy. The two will disagree on any fertility-stressed run until FAO
makes the same change (noted in `GUI_bug_reports.md`).

## Why the references moved on 2026-09-25

The references were re-frozen against the GDD-native code (`test/test_suite`'s own
`src/`, commit 4267b18). Nearly every case changed. This is what changed and why
each change is the right one, so that a diff of old against new references does
not have to be re-derived.

**Six defects were fixed on the way**, each found by reading a case in the run
explorer:

| what | where | seen in |
|---|---|---|
| a sown seed grew a canopy while waiting for a wet enough soil, and a crop that emerged within a day of germinating was declared finished on day 1 | `CheckGermination`, `GerminationDay`, canopy step | F30, H17, H20 |
| the off-season hung off the declared end of the cropping period, not the day the crop finished | `IrriOutSeason`, `GetIrriParam`, the irrigation report | K09 and the K family |
| a canopy declared "no crop" kept a value of a few hundredths of a percent, so ETpot, the stress columns and the season's day count still saw a crop | step 7 of `DetermineCCiGDD` | F51, N14, P04, W14, SW03, P19 |
| the flowering window dropped its last partial day, losing 8-18 % of the crop's flowers | `FractionFlowering` and its gate | X19 |
| `SumGDDCuts` was declared `integer(int32)` with `real(dp)` accessors, truncating up to a degree a day | `global.f90` | J33 |
| a simulation period starting before the climate record ran on weather shifted by the days it was short (BUG-24, fixed on `fix/7.4_bug_fixes`) | `CheckClimateRecordsCoverSimPeriod` | D19 |

**The rest of the movement is the clock itself**, and falls into four groups:

1. **Harvest index builds per degree, not per day** (`HarvestIndexDay`, commit
   6ba523d). v7.3 spread HIo over a day count obtained by walking the record from
   flowering until the crop's degrees had accumulated — a walk that, for a late
   sowing or a long-filling crop, ran into the winter and returned absurd spans
   (338 days for Y09's 1150 GDD, where the crop file says 80). The rate was then
   far too slow. Where the season completes the fill both clocks reach HIo and the
   yields agree to under a percent; where it does not, the new HI tracks the share
   of the fill window the season delivers (99 % → 47.4 of a 48 cap, 74 % → 33.5,
   40 % → 15.0) while the old one tracked nothing. 35 case-runs moved by more than
   20 % in yield for this reason alone.

2. **Flowering is a thermal window**, so pollination stress is sampled on the days
   that window actually spans. X19 (cold) falls, X20 and P09 (heat) rise.

3. **The cycle ends on the GDD clock**, so a season that cannot deliver the crop's
   degrees no longer ends at a date the look-ahead invented. v7.3 extended the
   simulation past the project's own end to reach that date — 18 cases ran 186-195
   days beyond it, into the following May, with the crop finished by October and
   the canopy frozen (H22, P01, R31, U02-U14, Q17, T02, W05, X02, Y19, Y21). In a
   season too cold to reach maturity it did the opposite and ended the crop at
   emergence (C29, P10).

4. **Post-flowering water stress is step-weighted** (`DetermineBiomassAndYield`
   2.5-2.7): the correction is a step-weighted mean, so a stressed warm day counts
   more than a stressed cool one. Late-season stress therefore costs less than it
   did. Y09 is the clearest case: water stress costs 24 % of its HI where it used
   to cost 54 %.

**Two cases were decided rather than fixed**: 100 % soil fertility stress now
leaves no crop (O11), and a stand too sparse to register a canopy is no crop at
all rather than 162 days of one (P19).

**Screened before freezing**: all 850 cases for impossible values — no HI above
100 %, no negative biomass or fluxes, no yield above biomass, no cycle longer than
its simulation, no canopy above 100 %, no biomass going backwards. The only flag
was root-zone water a few mm below wilting point in 8 cases, bit-identical in
v7.3 (see O5). Every yield difference above 20 % was traced to one of the causes
above.

## Retired cases

No case is waiting on a fix any more: the defects that held them back are
fixed on `fix/7.4_fixes_testsuite` (see *Fixed on the fix branch* below) and
the cases are back, listed under *Revived cases*. F54a–F54e (legacy `.SOL`
probes, BUG-1) were not brought back: F54f and F54g cover the fix, and the `-`
spacer column they test is not a real file format. Their rows stay in `RETIRED`
in `assets/gen_cases_F.py`.

## Revived cases

Brought back on `fix/7.4_fixes_testsuite`, 2026-09-17. A case where AquaCrop
must refuse the input carries `expect_error:` with the message it must print,
and passes when the run stops with that message.

| Case | Input | Fixed defect | Now |
|---|---|---|---|
| F07 | `GEOM_0p05m.SOL` | BUG-4 | runs, one output column per compartment |
| F44 | `PEN_in_evap_layer.SOL` | BUG-5 | runs, the crop no longer dies |
| F54f, F54g | `V45_1L_nospacer.SOL`, `YoloClayLoam6.SOL` | BUG-1 | run |
| F56 | `LAYERS_6.SOL` | BUG-2 | expected error (warning, then the run stops) |
| D02, D08 | `OttawaDec.Tnx` | BUG-6 | run |
| D18 | `Agnostic1y.CLI` + 2016 dates | BUG-10 | expected error |
| A07 | a project list with an empty line | BUG-11 | expected error |
| A09, A10 | project naming an absent `.CRO`/`.SOL` | BUG-12, BUG-13 | run: the project is skipped with a warning |
| G13 | `GWT_var_late.GWT` | BUG-14 | runs |
| C13 | `Tbase` = `Tupper` | BUG-15 | expected error |
| N22 | default air below `Tbase` | BUG-15 | expected error |
| D20 | simulation period past the record end | BUG-16 | expected error |
| D23 | `OneDay.CLI` | BUG-6, BUG-16 | expected error |
| N02 | `Truncated.PPn` | BUG-21 | expected error (warning, then the run stops) |

New cases: O36 (evaluation in a single-run `.PRM`, BUG-9) and D21 (a
simulation period entirely after the record: expected error, BUG-16).

## Fixed on the fix branch

Fixed on `fix/7.4_fixes_testsuite` (from the 7.3 release), 2026-09-16/17.
Warnings and errors are written to the terminal and to
`OUTP/ListProjectsLoaded.OUT`. Still open: BUG-18 (the capillary-rise part is
fixed in PR #384; C29, P10 and F37 remain).

| Defect | What changed |
|---|---|
| BUG-1 | legacy `.SOL` layer lines are read as text; the description is the rest of the line |
| BUG-2 | more than five horizons gives a warning before the run stops |
| BUG-4 | one output column per compartment, also with a single compartment |
| BUG-5 | root-zone salinity is computed for any rooting depth, so a shallow root zone no longer kills the crop |
| BUG-6 | the monthly reference climate of a 10-daily temperature record is right; climate lookups stop with an error instead of hanging |
| BUG-7 | `C2Max = C1Max` in the 10-daily reader |
| BUG-8 | salt solubility is a 32-bit integer |
| BUG-9 | evaluation of a single-run `.PRM` reads the right data file |
| BUG-10 | a year-agnostic record with real project dates stops with an error |
| BUG-11 | an empty line in the project list gives a warning |
| BUG-12 | a missing input file gives a warning (a missing calendar file is only a warning, the run continues) |
| BUG-13 | a project that cannot be loaded is skipped |
| BUG-14 | the year-undefined water-table read has an `iostat` |
| BUG-15 | a crop that can never accumulate growing degrees stops with an error |
| BUG-16 | a simulation or cropping period past the end of a climate file stops with an error while the project is loaded |
| BUG-17 | off-season irrigation events are read (results change for K06–K14) |
| BUG-19 | salt drainage corrected for gravel, and the drain loop stops at the first cell |
| BUG-22 | the evaporation reduction coefficient is computed at a 15 cm evaporation layer too (N16a, S07 re-frozen) |
| BUG-18 (part) | no 0/0 depletion before the crop has roots (C29, P10), no division by a zero rooting depth (F37); the capillary-rise part is PR #384 |
| BUG-23 | the daily fluxes and the evaporation-layer state are set before they are read |
| BUG-21 | a `.PPn` with too few values gives a warning |

Fixed elsewhere:

| Defect | What changed |
|---|---|
| BUG-25 | the default CO2 record is resolved per run, not read once at start-up (PR #379, merged 2026-10-13; D30 re-frozen, D31 live) |

## Branches unreachable from inputs

**These are not rows in the suite any more.** 63 of them were removed
from the group tables on 2026-09-10, because a row that no input file can reach
is not a test waiting to be written — it is a note about the code. Leaving them
in made the coverage tables read as though there were far more outstanding work
than there is. Case numbering now has gaps, which is fine: the ids that remain
still mean what they always did.

They fall into four kinds:

**Fixed in the source.** The value exists in a file but nothing can move it, or
it never reaches a file at all: `IniAbstract` is read and then overwritten with 5
on the next line; `WithBeta` is assigned in code at three call sites; the `.CRO`
perennial block is parsed and never consumed (O7).

**Internal clamps.** `Runoff >= Shower`, `EffecRain < 0`, `excess < 0`,
`theta_nul > SAT` and their like guard values the model computes for itself
part-way through a routine. An input can make them *more likely*, but nothing in
a project file selects them, and a case claiming to test one would be asserting
something it does not actually drive.

**Within-timestep transitions.** "Ponded water exhausted mid-day", "surface
water insufficient for Tpot": real branches, but they open and close inside a
single daily step and leave no separate trace in daily output.

**Quantities never reported.** `UL`, `Dx`, `Macro`, `SinkMajor`, `SinkMinor` and
the salt-cell indices are internal geometry. Their *effects* show up in water and
salt content, which the built cases do cover, but the values themselves are not
observable.

Reaching any of them needs instrumentation — a debug build writing internal
state, or unit tests calling the routines directly — which is a different tool
from this suite. The full list is kept here so the analysis is not lost:

| Was | Branch | Why no input reaches it |
|---|---|---|
| A11 | Path with a trailing slash vs without | path concatenation — paths are concatenated verbatim (`//`, startunit.F90:208), so dropping the slash names a file that does not exist; blocked by BUG-12 until the loaders check |
| A12 | Relative (`./DATA/`) vs absolute paths | `GetPathName*` — an absolute path would put this machine into a frozen reference |
| B17 | Perennial: onset generated, TMeanPeriod (code 12) | `AirTCriterion_TMeanPeriod` |
| B18 | Perennial: onset generated, GDDPeriod (code 13) | `AirTCriterion_GDDPeriod` |
| B19 | Perennial: end generated by air-T criterion | `PerennialPeriod_GenerateEnd` |
| B20 | Perennial: onset occurrence 1 vs 2 vs 3 | `OnsetOccurrence` |
| B21 | Perennial: onset occurrence > 3 (clamped) | clamp to 3 |
| B22 | Perennial: extra years beyond the record | `OnsetExtraYears` |
| C22 | Temperature file `(External)` | LIS coupling branch — needs the coupled temperature arrays, not an input file |
| F33 | `Soil_RootMax` (single precision) vs `Crop_RootMax` (double) | sp/dp comparison at 1000× — the .CRO writes rooting depth as f9.2, too coarse to separate sp from dp at 1000x |
| N27 | `IniAbstract` (forced to 5 in v5.0+) | the overwritten read — no .PPn record exists; the reader just calls SetSimulParam_IniAbstract(5) |
| P17 | Cycle longer than 365 days | multi-year annual cycle |
| Q03 | `theta_x <= SAT` with theta above `theta_x` | first sub-branch |
| Q04 | `theta_x <= SAT` with theta between FC and `theta_x` | second sub-branch |
| Q08 | Excess reaching compartment 1 | `pre_nr == 1` loop exit |
| Q09 | Excess exactly zero | `abs(excess) < epsilon` exit |
| Q14 | `TauFromKsat` at its breakpoints | the tau lookup |
| R02 | Decadal rain record, shower-based runoff | the non-daily branch |
| R03 | Runoff ≥ shower depth | `Runoff >= Shower` clamp |
| R04 | Runoff computed greater than rain | `Runoff > Rain` clamp |
| R05 | Initial abstraction exceeding rain | `term <= epsilon` → zero runoff |
| R07 | Weighted wetness sum < 0 | `SUM < 0` clamp |
| R19 | `delta_theta_nul < delta_theta_SAT` | the first infiltration sub-branch |
| R20 | `theta_nul <= FCadj` | clamping to adjusted FC |
| R21 | `theta_nul > SAT` | clamping to saturation |
| R22 | `fluxout + drain_max` exceeded | flux limiting at a compartment |
| R23 | Infiltration excess pushed back up | `excess > 0`, `pre_comp` walk |
| R24 | Excess reaching compartment 1 → runoff | `Runoff > RunoffIni` |
| R25 | Excess computed negative | the `excess < 0` guard |
| R27 | Exactly enough storage capacity | `amount_still_to_store <= epsilon` exit |
| R34 | Rain on the last day of the simulation | end-of-record handling |
| R35 | Effective rainfall, percentage method, with `SubDrain` | `CalculateEffectiveRainfall` sub-drain path |
| R37 | Effective rain with `Zr <= 0` (no roots yet) | the zero-root-depth guard |
| R38 | Effective rain with `RestTheta <= 0` | the saturated-root-zone guard |
| R41 | `EffecRain` computed negative | the `EffecRain < 0` clamp |
| R42 | `EffecRain` exceeding rain − runoff | the upper clamp |
| S09 | `EvapZmax` == `EvapZmin` | the guard in stage II |
| S11 | Evaporation layer thinner than compartment 1 | sub-compartment extraction |
| S13 | Salt cell walk ending below the previous cell | `SCellEnd < SCellIniEvap(compi-1)` |
| S14 | Salt cell walk reaching `SCP1` | `SCellEnd == SCP1` |
| S15 | Evaporation walk running past the last compartment | the `compi <= NrCompartments` guard |
| S23 | Ponded water exhausted mid-day | transition from surface to soil evaporation |
| S28 | Evaporation with a restrictive layer at 0.10 m | S × F44 interaction |
| T07 | Root zone shallower than compartment 1 | partial-compartment uptake |
| T08 | Root zone exactly on a compartment boundary | boundary of the weighting walk |
| T09 | Root zone deeper than the compartments | uptake beyond the profile |
| T12 | Uptake limited by `SinkMajor` | maximum extraction with stress |
| T13 | Uptake limited by `SinkMinor` | required extraction without stress |
| T17 | Surface water insufficient for Tpot | `Tact < KsReduction*Part*Tpot` |
| T18 | KcTr,x 0.8 vs 1.15 vs 1.30 | crop coefficient sensitivity |
| U06 | `DTheta >= DThetaMax` | the per-compartment cap |
| U09 | CRa/CRb from the file (v ≥ 4.0) | file-supplied capillary parameters |
| U10 | CRa/CRb derived by `DetermineParametersCR` (v < 4.0) | the version-gated default path |
| U15 | Horizontal inflow into a drier compartment | the theta comparison branch |
| U17 | Horizontal inflow with matching EC | the no-salt-flow branch |
| V12 | Salt entering from a saline water table | `SaltIn` via horizontal inflow |
| V18 | `Macro` (= round(FC)) governing macropore flow | macropore branch |
| V19 | `UL` and `Dx` from `SAT` and `SC` | the cell geometry |
| V21 | Salt across a `KeepSWC` run boundary | salt carried between runs |
| V23 | Salt balance closure over a full season | `SaltIn − SaltOut` vs storage change |
| V25 | `DetermineSaltContent` from an ECe input | `.SW0` ECe → cell salt |
| Y15 | `AdjustpSenescenceToETo` with `WithBeta` true | senescence p with the beta term — WithBeta is set in code (simul.f90:3417/3610/3964), not from any input |
| Y16 | `AdjustpSenescenceToETo` with `WithBeta` false | senescence p without beta — as Y15 |


---

## O4 — the soil and the surface pond must be balanced together, not separately

*Found by R40/T16 (bunded fields), 2026-09-03. A correction to the suite, not a
model defect.*

The suite originally carried two separate daily identities:

    dWC   == Infilt + CR - Drain - E - Tr
    dSurf == Rain + Irri - Infilt - RO

Both hold on an unbunded field and both **fail on a bunded one**, by equal and
opposite amounts. On R40 day 26: 100 mm of rain, `Infilt` capped at 5.0 by a
Ksat of 5, `dSurf` +92.1 where the identity predicts +95.0 — and `dWC` too high
by exactly the same 2.90 mm.

The missing term is **evaporation from ponded water**. It is inside the reported
total `E`, but that water never entered the soil, so charging all of `E` against
the profile over-counts there and under-counts on the surface. There is no
column splitting `E` into its soil and surface parts.

Balancing the combined store removes the need to:

    d(WC + Surf) == Rain + Irri + CR - RO - Drain - E - Tr

Verified across all 201 cases that report a full daily balance, worst residual
0.20 mm — the print precision of seven one-decimal terms — including the bunded
cases that broke the split version. The single identity replaces both.

The season-level check has the same blind spot and cannot be fixed the same way,
because the season output has no surface-storage column: whatever is still
ponded on the last day is simply missing from `Infilt + Runoff`. It is therefore
skipped for any case whose `.MAN` declares non-zero bunds, which the runner
detects by reading the file.

**Also corrected here:** `-9` is `undef_int`, which the model writes for a ratio
that is undefined — `Tr/Trx` where nothing transpired, `Cycle` where no cycle
ran. Twelve drought and wilting-point cases reported it legitimately and the
percentage-range check was flagging them. It now treats -9 as the sentinel it is.


---

## O5 — the reported rooting depth can exceed the root zone actually used

*Found by F39/F40/F41 while building the Z07 invariant, 2026-09-03. Reporting
inconsistency; the water balance itself is correct.*

On a profile with a restrictive horizon the daily `Z` column reports a rooting
depth deeper than the root zone the model accounts water over:

| case | soil | `Soil_RootMax` | `Wr(...)` header | max `Z` reported |
|---|---|---|---|---|
| F41 | `PEN_25` | 0.975 | **0.98** | **2.50** |
| F39 | `PEN_50` | 1.650 | **1.65** | **3.00** |
| F40 | `PEN_graded` | 0.900 | **0.90** | **2.10** |

The `Wr(x)` header names the depth used for the root-zone water balance and it
matches `ZrAdjustedToRestrictiveLayers` exactly in all three, so the physics
respects the restriction. It is `Z` that carries the unrestricted depth the crop
would have reached had the profile allowed it.

Reading the `Z` column on a restrictive soil therefore overstates rooting by up
to 2.5x. The Z07/Z08 invariant is written against the `Wr` header rather than
the `Z` column for that reason, and holds across all 53 cases with a predicted
geometry. Whether `Z` should be clamped, or documented as unrestricted, is a
question for whoever owns the output specification.

`Z = -9` also occurs, on cases where no crop established (P10) -- `undef_int`
again, as in O4.


---

## O6 — a daily balance must not be taken across a run boundary

*Found by A04/A23/A24, 2026-09-04. A correction to the suite, not a model defect.*

The daily output of a multi-run project concatenates its runs into one file.
Unless the project carries the profile forward with `KeepSWC`, soil water
restarts at the first day of each run, so the difference taken across that
boundary is a reset, not a flux, and no balance identity applies to it.

Three cases showed it, each failing on exactly the boundary day:

| case | runs | reported at | segment lengths |
|---|---|---|---|
| A24 | two, with a gap | day 73 | 72 + 47 |
| A23 | two, out of order | day 165 | 164 + 164 |
| A04 | ten | day 493 | 10 x 164 |

`daily_runs` now splits the series wherever the date is not the next calendar
day, and the balance is evaluated within each segment. That leaves a `KeepSWC`
project as a single segment, which is correct — A01 and A21 are date-contiguous
and their profile genuinely does carry across, so the identity holds through the
boundary and is checked there.

The failure was well aimed: A04, A23 and A24 are the only cases in the suite
with a discontinuous multi-run structure, and they are three days old. Every
earlier multi-run case used `KeepSWC`.


---

## O7 — two crop-file blocks are read but never used

*Raised by the user, 2026-09-04, and confirmed in the source.*

`LoadCrop` parses the `.CRO` internal calendar block — the dormancy onset and
end criteria, their search windows, thresholds, successive-day counts and
occurrence counts — into `PerennialPeriod_*`. Nothing reads it back:
`GetPerennialPeriod_*` appears only in `global.f90`, as the accessors
themselves. `run.f90`, `simul.f90` and `tempprocessing.f90` never mention it.

This is the same situation as `LoadCropCalendar` and the `.CAL` file: the GUI
resolves these criteria into dates and writes those into the `.PRM`, which is
what the standalone actually reads. Three cases (B17, B19, B20) were built
against that block, froze green, and asserted nothing; they have been removed.

**The lesson for the suite:** a passing case is not evidence of coverage. Both
of these were found by a domain reader noticing the claim, not by anything the
harness does. Where a group's rows describe an input that may be GUI-only, the
cheap check is `grep` for the getter outside `global.f90` before building.

### A related correction

The same review corrected a claim in this plan that
`DetermineLengthGrowthStages` converts between calendar and GDD stage lengths.
It does not — it takes only calendar arguments and derives stage lengths from
the canopy parameters. The two stage sets are independent as *inputs*.

There is a conversion, in the other direction: `AdjustCalendarCrop` ->
`AdjustCalendarDays` (`tempprocessing.f90:1602`, `:1461`) derives the calendar
stages *from* the GDD stages through `SumCalendarDays`, called at `run.f90:8070`
under `if (GDDAvailable >= GetCrop_GDDaysToHarvest())` and unconditionally at
`tempprocessing.f90:2264`. So in GDD mode the GDD set is authoritative and the
calendar set is overwritten at run time. C24 and C25 are kept and re-described
to pin exactly that difference — it is one of the things the GDD refactor
changes.


---

## Group Z: what is checked, and how

Most of group Z is not made of cases. Four kinds of check now exist:

**Invariants, on every case.** The balance identities, flux signs, percentage
ranges, NaN, compartment bounds, profile reconstruction, root-zone depth, and
that a staged `.PPn` was actually read. These run inside `run_tests.py` and cost
no reference storage.

**Determinism (Z13).** `run_tests.py --repeat N` runs each case N times in
separate working trees and requires byte-identical output every time, comparing
a digest taken with the timestamp header removed. Worth running after any change
that touches allocation or ordering.

    python3 tests/runner/run_tests.py --repeat 3 -j 36

**Order independence (Z14).** `run_tests.py --shuffle` randomises case order.
Each case already runs in its own working tree, so this mostly guards against a
harness mistake rather than a model one -- but that is exactly the sort of
mistake this session kept making.

**Equivalence between cases (Z15).** Some properties are relationships, not
values, and no single frozen reference can express them.
`runner/check_equivalence.py` compares the per-run seasonal totals of a
multi-run project against the same runs done as separate projects:

    python3 tests/runner/check_equivalence.py

`Z15a` is two seasons as one two-run `.PRM`; `Z15b` and `Z15c` are the same two
seasons as standalone `.PRO` projects. With no `KeepSWC` to couple them the runs
are independent, so every seasonal column should agree.

**Z16 was dropped (2026-09-17).** Restarting mid-season from a `.SW0` and
matching a continuous run needs the profile state at the split point, which only
a run can produce -- a two-stage harness step (run, extract the profile into a
`.SW0`, re-run) rather than a case.

**Z18 and Z19 (build variants).** `runner/check_builds.sh` does both. It saves
the production binary, rebuilds twice, runs the suite against each, and restores
what was there — on an error path too, via a trap:

    ./tests/runner/check_builds.sh          # both
    ./tests/runner/check_builds.sh fpe      # Z18 only
    ./tests/runner/check_builds.sh o0       # Z19 only

**Z18** builds with `DEBUG=1` plus `-ffpe-trap=invalid,zero,overflow`, which the
Makefile accepts through `CPPFLAGS` (line 7 is `FCFLAGS = $(CPPFLAGS)`, so it is
prepended rather than replacing the flag set). Any case that dies under it is
doing arithmetic the production build absorbs silently. Given that BUG-15 was an
infinite loop on a division that can only be reached when the divisor is zero,
this is the variant most likely to find something.

**Z19** builds with `DEBUG=1` alone, which carries no `-O` flag and so compiles
at `-O0`, and runs it against references frozen at `-O2`. Within-tolerance is
the expected result; a genuine difference means an optimisation-sensitive
calculation. Note this variant also enables `-fcheck=all`, so a bounds violation
surfaces here too — which is a bonus rather than a confound, since the
production build would not report it either way.

Both need the compiler, so they are the one part of the suite that has to be run
rather than assembled.

---

## O9 — precipitated salt is reported by nothing

*Found by Z03, 2026-09-10.*

AquaCrop keeps salt in two stores per compartment: `Compartment%Salt`, in
solution, and `Compartment%Depo`, precipitated. Only the first reaches the
output. `Depo` appears 50 times in `simul.f90` and 24 in `global.f90`, and
**not once in `run.f90`**, which writes every output file.

So when salt crosses from solution into deposit it leaves the reported
`Salt(x)` state without passing through `SaltOut`, and a salt balance computed
from the output columns cannot close. Over 49 run segments the Z03 identity

    d(Salt) == SaltIn + SaltUp - SaltOut

closes to 0.007 ton/ha -- the print precision -- for 45 of them. The four that
do not are all cases where salt arrives from a water table and precipitates:

| case | deviation |
|---|---|
| U13 salt carried upward with capillary rise | +9.675 ton/ha |
| L18 a saline profile over a saline water table | -3.148 ton/ha |
| U16 horizontal inflow carrying salt from the water table | +1.976 ton/ha |
| V11 salt entering with capillary rise | +1.976 ton/ha |

U16 and V11 agreeing exactly is the expected sign that they are one
configuration reached two ways. These four carry
`skip_invariants: [salt_balance]`; their references are exact and their runs are
not in question. This is a gap in what the output reports, not evidence that the
model loses salt -- proving conservation would need `Depo` in an output column.

## O10 — 78 cases carry no daily water-balance assertion

*Found while auditing the extractor, 2026-09-10.*

The daily balance is the suite's strongest check, and it needs both the state
column (`WC(x)`) and the flux columns (`Rain`, `Irri`, `Infilt`, `RO`, `Drain`,
`CR`, `E`, `Tr`). Those live in different output blocks: the state in block 3 or
5, the fluxes in block 1. Of the 342 frozen daily references carrying a `WC()`
header, **264 run the balance and 78 do not**, because the case asked for block
3 or 5 without block 1. `invariants.py` skips them silently.

This is legitimate -- a balance cannot be checked without the fluxes -- but it
is invisible, and those 78 cases are asserting less than they appear to. Adding
`daily: [1]` to them would put the balance back, at the cost of re-freezing
their references, which is worth doing on the next freeze rather than on its
own.

## O11 — 100 % soil fertility stress leaves no crop on the GDD clock

*Found on J12 and SW05 in the GDD-native run, 2026-09-23. Decided: the new
behaviour is the right one.*

At 100 % soil fertility stress the crop's calibrated response is the extreme
value: `KsAny(1.0, ...)` returns 0, so `CropStressParametersSoilFertility` gives
`RedCGC = RedCCX = 100` and `CDecline = 1`. CCx is reduced to zero, and a canopy
cannot stand above a maximum of zero: CC goes to 0 on the day it is seeded, and
the canopy routine's "no crop as a result of stress" test ends the crop for the
season. `Cycle` is undefined, `Tr` and biomass are 0.

**v7.3 grew a crop there (J12: 4.8 t/ha) for an accidental reason.** The gate
that suppresses fertility stress before emergence used to compare calendar days
(`VirtualTimeCC < DaysToGermination`, 3 days here), while emergence on the GDD
clock falls a day earlier (22.8 GDD against a threshold of 20). The canopy
therefore got one stress-free day to establish, and during that day the adaptive
stress level was recomputed once and came down from 100 to 80, which maps to
`RedCGC = 12` and `RedCCX = 55` — heavy but survivable. The stress was never
applied at its stated strength.

The GDD-native code puts that gate on the crop's own clock, so the stress
arrives on the emergence day at its full value, and 100 % stress means what it
says. The affected cases are **J12** (`MAN_fert100.MAN`) and **SW05** (the same
fertility setting in a sweep); their references record the v7.3 behaviour and
must be re-frozen when the GDD branch lands.

## O8 — RETRACTED, then explained by BUG-22

*Raised 2026-09-04 from a partial Z18 run; withdrawn 2026-09-10 after the full
Z19 run; explained 2026-09-18 as [BUG-22](#BUG-22).*

**The effect was real, the cause was not optimisation.** `N16a` and `S07` are
the two cases with a 15 cm evaporation layer, where `Kr` was used before it
was set (BUG-22). Their numbers therefore moved when the build changed, which
is what the partial Z18 run showed. A plain `-O0` build happened to reproduce
the `-O2` values, which is why the full Z19 run found nothing. The original
reasoning below is kept as written.

O8 originally claimed that `N16a` and `S07` — the two cases that set the
evaporation depth to 0.15 m instead of the default 0.30 — produced different
numbers under `-O0` than under the production `-O2` build, by 0.8 % to 2.2 % on
season `E` and `Tr`.

**That does not reproduce.** The complete Z19 run (`check_builds.sh o0`, all 813
cases) reports:

    pass 797   within-tol 6   known-defect 0   FAIL 0   ERROR 10

`FAIL 0` means no case, `N16a` and `S07` included, differed from its frozen
reference beyond tolerance. The tolerance is the same in both runs — without
`--rtol`, each case uses its own `rtol` from `case.yml`, which is `1e-3` for
these two — so the earlier verdict cannot be explained by a looser comparison.

The Z18 run that produced the original claim was interrupted partway with
Ctrl-C, and the numbers above were read from its partial output. The most likely
explanation is that they were misread there; no evidence for the effect survives.

Six cases do land *within* tolerance rather than exactly equal, which is normal
for `-O0` vs `-O2` on the same source. Their names were not recoverable from
this run, because `run_tests` deletes the working tree of any case that is not a
failure and did not print which cases were close. It now prints a
`within tolerance:` line, so the next Z19 run identifies them; if `N16a` and
`S07` are among the six, the effect is real but three orders of magnitude
smaller than first reported.

