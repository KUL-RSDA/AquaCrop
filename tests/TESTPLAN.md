# AquaCrop test suite — cases

The test suite of branch `test/test_suite`: 856 small AquaCrop projects with
their expected output, which together run through most paths of the Fortran
code. How to run it is in `README_testsuite.md`; this file says what each case
is for, and what the suite has found.

The run explorer (`runner/build_explorer.py`) builds its case list, defect list
and observations from this file, so keep the rows and headings in the form they
have.

Each row is one case, or one thing checked another way. Column **St**: `[x]`
tested, by a case of its own or by the cases the row names · `[~]` partly
tested, by the cases the row names · `[i]` no case of its own, checked by a rule
on every run (group Z). Case ids are stable: a removed case leaves a gap, and a
number is never reused.

---

## Conventions

### Reference

The stored output (`cases/*/OUTP_REF/`) comes from one build, recorded in
`REFERENCE.txt`: branch, commit and checksum of the binary. Nothing regenerates
it automatically. A reference changes only when someone runs `runner/freeze.py`,
after checking that the change is intended (README, section 4).

### Tiers

| Tier | Meaning |
|---|---|
| T0 | Core regression, must never change |
| T1 | Core physics and feature coverage |
| T2 | Broad option coverage |
| T3 | Edge cases, error paths, robustness |

`run_tests.py --tier T0 T1` runs only the cases of those tiers.

### Output size

A case stores season output only, unless it is about something visible only day
by day (`daily:` in `case.yml`). It covers one season of at most 180 days,
unless the case is about several years. Output is stored as plain text, never
compressed, so a changed reference reads as a diff.

### Hard limits

From `global.f90`: `max_SoilLayers = 5`, `max_No_compartments = 12`, default
`CompDefThick = 0.10 m`. A profile deeper than 1.2 m therefore cannot be tiled
by default-thickness compartments, and `AdjustSizeCompartments` has to regrade
them. Much of group F is built around that arithmetic.

---

# Part I — Input and option coverage

### A. Project & run structure  (`startunit.F90`, `project_input.f90`)

| ID | Case | Exercises | Tier | St |
|---|---|---|---|---|
| A01 | Ottawa 3-run PRM, perennial alfalfa GDD | current `testcase/`, full reference | T0 | [x] |
| A02 | Season 1 only, as a `.PRO` single-run project | `typeproject_typepro` | T1 | [x] |
| A03 | PRM with 2 runs | multi-run loop | T1 | [x] |
| A04 | PRM with 10 runs over the 3 climate years | long multi-run stability | T2 | [x] |
| A05 | `ListProjects.txt` absent | auto-discovery via `grep -E ".*.PR[O,M]$"` | T2 | [x] |
| A06 | `ListProjects.txt` listing a PRO **and** a PRM | mixed project loop | T2 | [x] |
| A07 | `ListProjects.txt` empty | "does not contain ANY project file" branch | T3 | [x] |
| A08 | Listed project file missing on disk | error branch → `ListProjectsLoaded.OUT` | T3 | [x] |
| A09 | Project file referencing a missing `.CRO` | load failure reporting | T3 | [x] |
| A10 | Project file referencing a missing `.SOL` | load failure reporting | T3 | [x] |
| A13 | Sim period starts before the cropping period | `AdjustSimPeriod`, pre-season balance | T1 | [x] |
| A14 | Sim period ends after the cropping period | post-season evaporation/drainage — partly covered by R14: its simulation runs a month past the crop, but it is the only case that does | T1 | [~] |
| A15 | Sim period == cropping period exactly | `DetermineLinkedSimDay1` | T1 | [x] |
| A16 | Sim period one day longer at each end | off-by-one at both boundaries | T3 | [x] |
| A17 | Run crossing a calendar-year boundary | day-number arithmetic | T1 | [x] |
| A18 | Run inside a leap year (2016) | `DetermineDate`/`DetermineDayNr` Feb 29 | T1 | [x] |
| A19 | Run starting on 29 Feb 2016 | leap-day start | T3 | [x] |
| A20 | Run of exactly 1 day | degenerate period | T3 | [x] |
| A21 | Perennial, year 1 (seeding) vs year >1 | `AdjustYearPerennials`, `Sown1stYear` | T1 | [x] |
| A22 | Perennial run 2 with a *different* crop file | crop switch between runs | T2 | [x] |
| A23 | Runs in non-chronological order in the PRM | run ordering assumptions | T3 | [x] |
| A24 | Two runs with a gap between them | non-contiguous sim periods | T2 | [x] |

### B. Crop type, planting & phenology  (`LoadCrop`, `DetermineLengthGrowthStages`)

| ID | Case | Exercises | Tier | St |
|---|---|---|---|---|
| B01 | subkind = Vegetative | `subkind_Vegetative`, HI build-up from sowing | T1 | [x] |
| B02 | subkind = Grain | `subkind_Grain`, flowering block | T1 | [x] |
| B03 | subkind = Tuber | `subkind_Tuber` | T1 | [x] |
| B04 | subkind = Forage (alfalfa) | `subkind_Forage` — covered by A01 | T0 | [x] |
| B05 | Planting = sown from seed | `plant_Seed`, germination check — covered by A01, C02 | T1 | [x] |
| B06 | Planting = transplanted | `plant_Transplant`, CCo at transplant | T1 | [x] |
| B07 | Planting = regrowth | `plant_Regrowth`, CCini from re-growth — partly covered by A01: runs 2 and 3 of the Ottawa project are re-growth years | T1 | [~] |
| B08 | Transplanted crop with a long establishment lag | days to recovered transplant | T2 | [x] |
| B09 | Annual crop, not perennial | non-perennial branch throughout | T1 | [x] |
| B10 | Determinant crop (linked to flowering) | `Crop_DeterminancyLinked = true` | T2 | [x] |
| B11 | Indeterminant crop | `Crop_DeterminancyLinked = false` | T2 | [x] |
| B12 | Flowering length 0 (no flowering stage) | degenerate flowering window | T3 | [x] |
| B13 | Flowering starting on day 1 of the cycle | boundary of the flowering window | T3 | [x] |
| B14 | Crop with a premature-end date set | `DayNr Premature end` for annuals | T2 | [x] |
| B15 | Premature end before maturity | early termination | T2 | [x] |
| B16 | Perennial: fixed dormancy onset & end dates | `GenerateOnset = .false.` — partly covered by A01: the standalone takes these dates from the .PRM; the .CRO dormancy block is read but never used (O7) | T1 | [~] |
| B23 | Perennial self-thinning, 9-year CCx decline | years-to-90% CCx, shape factor | T2 | [x] |
| B25 | Crop with CGC so large CC closes in days | rapid canopy closure | T3 | [x] |
| B26 | Crop with CCx = CCo (no expansion) | degenerate canopy growth | T3 | [x] |

### C. Crop-cycle mode: GDD vs calendar days

`runner/gdd_check.py` recomputes the daily growing degrees of these cases in
Python, independently of the stored output. `OttawaConst.Tnx` holds a constant
12/28 °C, which with method 3, Tbase 5 and Tupper 30 gives exactly 15 GDD a day,
so the arithmetic can also be checked by hand.

| ID | Case | Exercises | Tier | St |
|---|---|---|---|---|
| C01 | Annual crop, `modeCycle_CalendarDays` | calendar-day path end to end | T1 | [x] |
| C02 | Annual crop, `modeCycle_GDDays` | GDD path end to end | T1 | [x] |
| C03 | Perennial, GDD (alfalfa) | current baseline | T0 | [x] |
| C04 | Perennial, calendar days | perennial × calendar-day combination | T1 | [x] |
| C05 | Same crop authored both ways, same season | GDD/calendar equivalence at the margin | T1 | [x] |
| C06 | GDD method 1 (no Tmax/Tmin adjustment) | `DegreesDay` case 1 | T1 | [x] |
| C07 | GDD method 2 (Tbase adjustment first) | `DegreesDay` case 2 | T1 | [x] |
| C08 | GDD method 3 (default) | `DegreesDay` default branch | T0 | [x] |
| C09 | GDD method sweep on an identical season | isolates the three formulations | T1 | [x] |
| C10 | Tmax < Tbase for a long stretch | zero-GDD days, stalled phenology | T1 | [x] |
| C11 | Tmin > Tupper for a stretch | capping at Tupper | T2 | [x] |
| C12 | Tmin < Tbase < Tmax (method 2 vs 3 diverge) | the case the methods disagree on | T1 | [x] |
| C13 | Tbase == Tupper | degenerate temperature window | T3 | [x] |
| C14 | Crop needing **more GDD than the record supplies** | truncation, no look-ahead | T1 | [x] |
| C15 | Crop needing exactly the GDD available | boundary of the record | T1 | [x] |
| C16 | Sim period ends mid-cycle | unfinished season output | T1 | [x] |
| C17 | Crop day 1 == last day of the temperature record | T-record boundary | T3 | [x] |
| C18 | Cropping period entirely outside the T record | `AdjustClimRecordTo` / defaults | T3 | [x] |
| C19 | Cropping period starting one day before the record | leading-edge clipping | T3 | [x] |
| C20 | No `.Tnx` — constant default Tmin/Tmax from PPn | `TemperatureFile == '(None)'` | T1 | [x] |
| C21 | No `.Tnx`, GDD crop | GDD from constant 12/28 °C defaults | T1 | [x] |
| C23 | GDD-based stage lengths (emergence → maturity) | `GDDaysTo*` conversions — covered by C02, A02 | T1 | [x] |
| C24 | Calendar crop | its stage lengths are used as written | T1 | [x] |
| C25 | GDD crop | its calendar stage lengths are derived at run time | T1 | [x] |
| C26 | GDD crop whose stages sum past `GDDaysToHarvest` | inconsistent crop file | T3 | [x] |
| C27 | Perennial GDD across a dormancy gap | GDD accumulation reset over winter — partly covered by A01: runs 2 and 3 start in November and pass the winter before growth restarts | T1 | [~] |
| C28 | Perennial `SumCalendarDays` walk over the T record | remaining T-record read in perennials — partly covered by A01: the only perennial in GDD mode over a multi-year record | T1 | [~] |
| C29 | GDD crop with `GDtranspLow` binding | minimum GDD for full transpiration | T2 | [x] |
| C30 | Cuttings scheduled by GDD interval | `TimeCuttings_IntGDD` × GDD mode — covered by J33: AlfOttawaGDD is GDD mode and the .MAN already generates on a GDD interval | T1 | [x] |

### D. Climate input  (`LoadClim`, `LoadClimate`, `climprocessing.f90`)

**The two 10-daily readers do different things.** `GetDecadeEToDataSet`
(`climprocessing.f90`) turns each decade into a piecewise-linear daily curve
through three points derived from the previous, current and next decade, so the
series is smooth across decade boundaries and its mean over the decade is the
decade value. `GetDecadeRainDataSet` spreads the decade total evenly over its
days. So a decadal ETo record holds means and a decadal rain record holds
totals. `runner/decade_check.py` ports both, and D04 and D06 are checked
against it.

| ID | Case | Exercises | Tier | St |
|---|---|---|---|---|
| D01 | Daily Tnx / ETo / PLU | `datatype_daily` — covered by A01 | T0 | [x] |
| D02 | 10-daily Tnx | `datatype_decadely` + interpolation | T1 | [x] |
| D03 | Monthly Tnx | `datatype_monthly` + interpolation | T1 | [x] |
| D04 | 10-daily ETo | decadal ETo splitting | T1 | [x] |
| D05 | Monthly ETo | monthly ETo splitting | T1 | [x] |
| D06 | 10-daily rain | decadal rain → daily | T1 | [x] |
| D07 | Monthly rain | monthly rain → daily | T1 | [x] |
| D08 | Mixed: daily T, monthly ETo, 10-daily rain | independent record types | T2 | [x] |
| D09 | Record starting on day 11 of a month | 10-daily records starting mid-month | T2 | [x] |
| D10 | Record starting on day 21 of a month | third decade start | T2 | [x] |
| D11 | Decadal record spanning a leap February | 29-day decade handling | T2 | [x] |
| D12 | Monthly record spanning a leap February | 29-day month | T2 | [x] |
| D13 | `.CLI` with `(None)` for rain | no-rain default | T2 | [x] |
| D14 | `.CLI` with `(None)` for ETo | default ETo path | T2 | [x] |
| D15 | `.CLI` with `(None)` for temperature | pairs with C20 | T2 | [x] |
| D16 | No `.CLI` at all | all-default climate | T2 | [x] |
| D17 | Record not linked to a year (first year 1901) | year-agnostic climate | T2 | [x] |
| D18 | Year-agnostic record used in a 2016 run | year mapping | T2 | [x] |
| D19 | Sim period starts before the climate record | must stop with a message (BUG-24) | T3 | [x] |
| D20 | Sim period ends after the climate record | record exhaustion | T3 | [x] |
| D21 | Sim period entirely outside the record | must stop with a message (BUG-16) | T3 | [x] |
| D22 | Crop year shifted onto the climate file | `AdjustCropYearToClimFile` | T2 | [x] |
| D23 | Single-day climate record | degenerate record — must stop with a message (BUG-6, BUG-16) | T3 | [x] |
| D24 | CO2 = MaunaLoa | interpolation in the CO2 record — covered by A01 | T0 | [x] |
| D25 | Constant CO2 file (single value) | flat CO2 | T2 | [x] |
| D26 | Simulation year before the CO2 record | leading extrapolation | T3 | [x] |
| D27 | Simulation year beyond the CO2 record | trailing extrapolation | T3 | [x] |
| D28 | CO2 at 369.41 ppm (the reference concentration) | no CO2 adjustment of WP | T1 | [x] |
| D29 | A run naming no CO2 file | `(None)` falls back to the default record | T2 | [x] |
| D30 | A run naming none after one that did | whether the default is resolved per run | T1 | [x] |
| D31 | The default CO2 record absent, the run naming its own | the start-up read of `SIMUL/MaunaLoa.CO2` (BUG-25) | T2 | [x] |

### E. Rainfall configuration & runoff settings

| ID | Case | Exercises | Tier | St |
|---|---|---|---|---|
| E01 | Effective rain method = full | `EffectiveRainMethod_full` | T2 | [x] |
| E02 | Effective rain method = USDA-SCS | `EffectiveRainMethod_usda` — covered by D06, D07 | T1 | [x] |
| E03 | Effective rain method = percentage | `EffectiveRainMethod_percentage` | T2 | [x] |
| E04 | % effective rainfall 50 / 70 / 100 | PPn `PercEffRain` | T2 | [x] |
| E05 | Showers per decade 1 / 2 / 5 / 10 | PPn `ShowersInDecade` | T2 | [x] |
| E06 | Daily-rainfall estimation from decadal, USDA on | `Method_usda` for record splitting | T2 | [x] |
| E07 | Daily-rainfall estimation from decadal, off | `Method_full` | T2 | [x] |
| E08 | Soil-evaporation reduction parameter for decadal rain | PPn `EvapZmax`-adjacent parameter | T2 | [x] |
| E09 | CN adjusted to antecedent moisture class, on | `DetermineCNIandIII` — covered by A01 | T1 | [x] |
| E10 | CN adjustment off | `SimulParam_CNcorrection = false` | T1 | [x] |
| E11 | CN 30 (low runoff) | CN sensitivity floor | T2 | [x] |
| E12 | CN 46 (Ottawa baseline) | covered by A01 | T0 | [x] |
| E13 | CN 90 (high runoff) | CN sensitivity ceiling | T1 | [x] |
| E14 | `Soil.PAR` runoff depth 0.10 vs 0.30 m | `SimulParam_RunoffDepth` | T2 | [x] |
| E15 | Zero rainfall over the whole season | dry extreme | T2 | [x] |
| E16 | `CN_default` derived from infiltration rate | `DetermineCN_default` | T2 | [x] |

### F. Soil profile, compartments & rooting geometry

`runner/soil_check.py` is an independent Python port of the compartment
arithmetic: each case checks the compartment count and thicknesses the source
implies, not only the stored output.

`DetermineNrandThicknessCompartments` tiles the profile with `CompDefThick`
(0.10 m) slices, at most `max_No_compartments = 12`, so a profile deeper than
1.20 m is at first represented only down to 1.20 m. `AdjustCompartments` then
takes one of six branches, and `AdjustSizeCompartments` either adds compartments
(step 3) or regrades all twelve with `fAdd = (CropZx/0.1 − 12)/78` (step 4).
Branches covered: **B2** F27 (two runs, `KeepSWC`), **B3** F01–F13, **B4**
F14–F26, **B5** F39/F43, **B6** F35–F44. B1, the `KeepSWC` branch that extends
the profile, has no case. `CompDefThick` is set in the source
(`initialsettings.f90`) and no input file changes it, so everything here
assumes 0.10 m.

**Step 4 runs in single precision, and F23 depends on it.** For a crop Zrmax of
3.15 m, `fAdd` is exactly 0.25 in single precision (0.24999999999999994 in
double), so `0.1·(1 + i·fAdd)·20` lands on exact halves of a 0.05 m step and
`roundc` rounds them to the even neighbour. The regrade is
`0.10 0.15 0.20 0.20 0.20 0.25 0.30 0.30 0.35 0.35 0.40 0.40` (3.20 m), trimmed
by one 0.05 m step to 3.15 m. `soil_check.py` follows step 4 in single
precision. The double-precision Pascal GUI gives a different layout at this
depth.

**Every group F case also checks its profile water.** No compartment may hold
more water than its layer's porosity, or less than none, on any day. And the
profile is reconstructed: `Out5CompWC` reports each compartment in vol% of the
soil matrix, so the profile holds

    sum_i  WC_i/100 * thickness_i * 1000 * (1 - GravelVol_i/100)

mm, with `GravelVol` from `FromGravelMassToGravelVolume`. This ties the reported
water content to the thicknesses `soil_check.py` predicts, so a geometry that
differs fails even when the compartment count matches. Without the gravel term,
the 60 %-gravel case F69 would be out by 168 mm.

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

### G. Groundwater table  (`LoadGroundWater`, `CalculateAdjustedFC`, `HorizontalInflowGWTable`)

| ID | Case | Exercises | Tier | St |
|---|---|---|---|---|
| G01 | No `.GWT` file | undefined water table — covered by A01 | T0 | [x] |
| G02 | `.GWT` present, mode 0 (no table) | mode-0 branch, `ConstGwt = true` | T2 | [x] |
| G03 | Constant GWT at 3.0 m (below the profile) | present but never reached | T2 | [x] |
| G04 | Constant GWT at 2.0 m | `ConstGwt = true`, FC adjustment deep | T1 | [x] |
| G05 | Constant GWT at 1.0 m | FC adjustment mid-profile | T1 | [x] |
| G06 | Constant GWT at 0.4 m (shallow) | strong FC adjustment, capillary rise | T1 | [x] |
| G07 | Constant GWT at 0.0 m (at the surface) | degenerate depth | T3 | [x] |
| G08 | GWT above the soil surface (negative depth) | `Zi ≥ DepthGWTmeter` for every compartment | T3 | [x] |
| G09 | Variable GWT, 2 observations | linear interpolation between them | T1 | [x] |
| G10 | Variable GWT, 12 monthly observations | repeated interpolation | T1 | [x] |
| G11 | Variable GWT, single observation | `iostat_end` → constant-table branch | T3 | [x] |
| G12 | Variable GWT rising then falling | non-monotonic table | T2 | [x] |
| G13 | Observations starting after the sim period | leading extrapolation | T3 | [x] |
| G14 | Observations ending before the sim period | trailing extrapolation | T3 | [x] |
| G15 | Observation year not linked (1901) | year-agnostic groundwater | T2 | [x] |
| G16 | Saline groundwater, EC 5 dS/m | salt entering from below | T1 | [x] |
| G17 | Saline groundwater, EC varying with the table | time-varying `ECdSm` | T2 | [x] |
| G18 | GWT with a restrictive layer above it | FC adjustment across a barrier | T2 | [x] |

### H. Initial conditions  (`LoadInitialConditions`, `AdjustThetaInitial`, `ResetSWCToFC`)

| ID | Case | Exercises | Tier | St |
|---|---|---|---|---|
| H01 | No `.SW0` — profile at FC | `DeclareInitialCondAtFCandNoSalt` — covered by A01 | T0 | [x] |
| H02 | `.SW0` specified at depths | `IniSWC_AtDepths = true` | T1 | [x] |
| H03 | `.SW0` specified per compartment | `IniSWC_AtDepths = false` | T1 | [x] |
| H04 | `.SW0` with 1 location | minimum location count | T2 | [x] |
| H05 | `.SW0` with 12 locations | `max_No_compartments` locations | T2 | [x] |
| H06 | `.SW0` locations not matching compartment edges | `AdjustThetaInitial` interpolation | T1 | [x] |
| H07 | `.SW0` shallower than the profile | undefined below the last location | T2 | [x] |
| H08 | `.SW0` deeper than the profile | truncation | T3 | [x] |
| H09 | `.SW0` with ECe per location | initial salinity profile | T1 | [x] |
| H10 | `.SW0` with an ECe gradient | per-cell salt initialisation | T1 | [x] |
| H11 | `.SW0` with initial surface storage | ponded start | T2 | [x] |
| H12 | `.SW0` with surface storage + ECStorage | saline ponded start | T2 | [x] |
| H13 | `.SW0` with CCini set | mid-season canopy restart | T1 | [x] |
| H14 | `.SW0` with Bini set | biomass carried in | T1 | [x] |
| H15 | `.SW0` with Zrini set | rooting depth carried in | T1 | [x] |
| H16 | `.SW0` with CCini + Bini + Zrini together | full mid-season restart | T1 | [x] |
| H17 | Profile initialised at wilting point | dry start, delayed germination | T1 | [x] |
| H18 | Profile initialised at saturation | waterlogged start | T2 | [x] |
| H19 | Profile initialised above saturation | clamping | T3 | [x] |
| H20 | `KeepSWC` between runs | Ottawa runs 2 and 3 | T0 | [x] |
| H21 | `KeepSWC` with a different soil in the next run | `AdjustThetaInitial` remapping | T2 | [x] |
| H22 | `KeepSWC` with a different Zrx in the next run | compartment regrade + remap (see F28) | T1 | [x] |
| H23 | `KeepSWC` with salt carried over | salt continuity between runs | T1 | [x] |

### I. Irrigation  (`LoadIrriScheduleInfo`, `Calculate_irrigation`, `.IRR`)

| ID | Case | Exercises | Tier | St |
|---|---|---|---|---|
| I01 | No `.IRR` — rainfed | `IrriMode_NoIrri` — covered by A01 | T0 | [x] |
| I02 | `.IRR` with mode 0 (rainfed, file present) | file read, mode 0 branch | T2 | [x] |
| I03 | Manual schedule, days + mm | `IrriMode_Manual` | T1 | [x] |
| I04 | Manual schedule, a single event | minimal schedule | T2 | [x] |
| I05 | Manual schedule, one event per day | dense schedule | T2 | [x] |
| I06 | Manual schedule with `IrriFirstDayNr` (v7.0 field) | version-gated read at `>= 70` | T2 | [x] |
| I07 | Manual schedule with `IrriInfoLastDay` (v7.3 field) | version-gated read at `>= 73` | T2 | [x] |
| I08 | Manual schedule with per-event ECw | irrigation water salinity | T1 | [x] |
| I09 | Manual events outside the cropping period | events before day 1 / after DayN | T3 | [x] |
| I10 | Generate, time = fixed interval 7 d | `GenerateTimeMode_FixInt` | T1 | [x] |
| I11 | Generate, time = fixed interval 1 d | daily irrigation | T2 | [x] |
| I12 | Generate, time = allowable depletion (mm) | `GenerateTimeMode_AllDepl` | T1 | [x] |
| I13 | Generate, `AllDepl` threshold never reached | no events generated | T2 | [x] |
| I14 | Generate, time = allowable depletion (% RAW) | `GenerateTimeMode_AllRAW` | T1 | [x] |
| I15 | Generate, `AllRAW` at 100 % | irrigation at RAW exhaustion | T2 | [x] |
| I16 | Generate, time = water between bunds (paddy) | `GenerateTimeMode_WaterBetweenBunds` | T2 | [x] |
| I17 | Generate, depth = back to FC | `GenerateDepthMode_ToFC` | T1 | [x] |
| I18 | Generate, depth = fixed application 30 mm | `GenerateDepthMode_FixDepth` | T1 | [x] |
| I19 | Generate, `ToFC` when the profile is already at FC | zero-depth event, `Irrigation < 0` clamp | T2 | [x] |
| I20 | Generate with `TargetTimeVal == 1` | the special-case branch in `Calculate_irrigation` — covered by I11 | T2 | [x] |
| I21 | Generate, schedule changing mid-season | multi-period irrigation parameters | T2 | [x] |
| I22 | Net irrigation requirement mode | `IrriMode_Inet` | T1 | [x] |
| I23 | Inet with `PercRAW` 50 % | partial refill | T2 | [x] |
| I24 | Inet with `PercRAW` 100 % | full refill to FC | T2 | [x] |
| I25 | Method = sprinkler | `IrriMethod_MSprinkler` | T1 | [x] |
| I26 | Method = basin | `IrriMethod_MBasin` | T2 | [x] |
| I27 | Method = border | `IrriMethod_MBorder` | T2 | [x] |
| I28 | Method = furrow | `IrriMethod_MFurrow` | T2 | [x] |
| I29 | Method = drip | `IrriMethod_MDrip` (default branch) | T1 | [x] |
| I30 | Fraction soil wetted 30 / 50 / 100 % | `SimulParam_IrriFwInSeason` | T2 | [x] |
| I31 | Fraction soil wetted 1 % | extreme localised wetting | T3 | [x] |
| I32 | Saline irrigation water, ECw 2 dS/m | mild salt build-up | T1 | [x] |
| I33 | Saline irrigation water, ECw 8 dS/m | strong salt build-up | T1 | [x] |
| I34 | Over-irrigation → leaching | salt removal via deep percolation | T2 | [x] |
| I35 | Irrigation exceeding layer-1 Ksat in a day | `calculate_Extra_runoff` from irrigation | T1 | [x] |
| I36 | Irrigation + rain together exceeding Ksat | combined extra-runoff branch | T1 | [x] |
| I37 | Irrigation onto a bunded field | routed through surface storage | T1 | [x] |
| I38 | Generate 6 mm/day, none in the last 10 days; GDD crop, constant 12/28 degC, no rain | `InLastDaysOfCropCycle`, GDD: 12 GDD/day, maturity 2014-08-29, last event 2014-08-19 | T1 | [x] |
| I39 | Same, Ottawa temperatures | GDD with the Tnx reference: last event about 10 days before maturity | T1 | [x] |
| I40 | Same, calendar crop (160 days) | calendar rule `Crop_DayN - 10 + 1`: last event 2014-10-17 | T1 | [x] |

### J. Field management  (`LoadManagement`, `.MAN`)

| ID | Case | Exercises | Tier | St |
|---|---|---|---|---|
| J01 | No `.MAN` file | `NoManagement` defaults | T1 | [x] |
| J02 | Mulches 0 % | baseline — covered by A01 | T0 | [x] |
| J03 | Mulches 50 %, effect 50 % | `Management_Mulch`, `EffectMulchInS` | T1 | [x] |
| J04 | Mulches 100 %, effect 50 % | full cover | T1 | [x] |
| J05 | Mulches 50 %, effect 100 % | maximum evaporation suppression | T2 | [x] |
| J06 | Mulches 100 %, effect 0 % | cover with no effect | T3 | [x] |
| J07 | Mulch combined with irrigation wetting | `AdjustEpotMulchWettedSurface` both terms | T1 | [x] |
| J08 | Soil fertility stress 0 % | no-stress branch | T1 | [x] |
| J09 | Soil fertility stress 25 % | below the calibration point | T1 | [x] |
| J10 | Soil fertility stress 50 % | at the crop's calibration point | T0 | [x] |
| J11 | Soil fertility stress 75 % | above the calibration point | T1 | [x] |
| J12 | Soil fertility stress 100 % | maximum stress | T2 | [x] |
| J13 | Fertility shape factors from a different crop | `CropStressParametersSoilFertility` sensitivity | T2 | [x] |
| J14 | Soil bunds 0.10 m | surface storage | T1 | [x] |
| J15 | Soil bunds 0.30 m + heavy rain | ponding to the bund height, then overflow | T1 | [x] |
| J16 | Soil bunds 0.00 m with `RunoffOn` false | runoff prevented without bunds | T1 | [x] |
| J17 | Bund height below the 0.001 m test | `Bundheight < 0.001` branch in effective rain | T3 | [x] |
| J18 | Runoff prevented by surface practices | `Management_RunoffOn = false` | T1 | [x] |
| J19 | CN correction +10 by field practices | `Management_CNcorrection` positive | T2 | [x] |
| J20 | CN correction −10 | `Management_CNcorrection` negative | T2 | [x] |
| J21 | Weeds: RC 25 % at canopy closure | `Management_WeedRC` | T1 | [x] |
| J22 | Weeds: RC 75 % | heavy infestation | T2 | [x] |
| J23 | Weeds: ΔRC +50 % in mid-season | `WeedDeltaRC` positive | T2 | [x] |
| J24 | Weeds: ΔRC −50 % | `WeedDeltaRC` negative | T2 | [x] |
| J25 | Weeds: shape factor 100 vs 50 vs −0.01 | `WeedShape` including the default | T2 | [x] |
| J26 | Weeds: `WeedAdj` 0 / 50 / 100 % for perennials | self-thinning replacement | T2 | [x] |
| J27 | Weeds combined with fertility stress | interacting canopy reductions | T2 | [x] |
| J28 | Multiple cuttings NOT considered | forage without cuttings | T1 | [x] |
| J29 | Cuttings: fixed harvest-day list | current Ottawa `.MAN` | T0 | [x] |
| J30 | Cuttings: a single cut | minimal schedule | T2 | [x] |
| J31 | Cuttings: a cut on the first day of the cycle | boundary | T3 | [x] |
| J32 | Cuttings generated, interval in days | `TimeCuttings_IntDay` — covered by J38, W16 | T1 | [x] |
| J33 | Cuttings generated, interval in GDD | `TimeCuttings_IntGDD` | T1 | [x] |
| J34 | Cuttings generated on dry biomass | `TimeCuttings_DryB` | T2 | [x] |
| J35 | Cuttings generated on dry yield | `TimeCuttings_DryY` | T2 | [x] |
| J36 | Cuttings generated on fresh yield | `TimeCuttings_FreshY` | T2 | [x] |
| J37 | Cutting criterion never met | no cuts generated | T3 | [x] |
| J38 | Cutting window `Day1` > 1 | window start clipping | T2 | [x] |
| J39 | Cutting window `NrDays` restricted | window end clipping | T2 | [x] |
| J40 | Cutting window `NrDays = -9` (whole cycle) | the default window — covered by A01 | T0 | [x] |
| J41 | Final harvest at maturity ON | `Cuttings_HarvestEnd` | T2 | [x] |
| J42 | CC after cut 10 / 25 / 50 % | regrowth starting canopy | T2 | [x] |
| J43 | CGC increase after cut 0 / 20 / 50 % | regrowth vigour | T2 | [x] |
| J44 | Cuttings across a multi-run project | `FirstDayNr` continuity between runs — covered by A01 | T1 | [x] |

**A generated cutting schedule needs a data block, and there is no threshold
record.** `LoadManagement` (`global.f90`) reads nine cuttings records
(considered, CC after cut, CGC increase, window first day, window length,
generate, time criterion, final harvest, day number of list day 1), and none of
them holds the criterion threshold. That lives in data rows at the end of the
`.MAN`, which `OpenHarvestInfo` (`run.f90`) reaches by skipping 2 + 10 + 12
records: description and version, ten management records, then the nine
cuttings records plus the blank, title and rule lines after them. Each row is
`FromDay  value`. An extra "threshold" record shifts every later read by one,
and the run stops with `Bad integer for item 1 in list input`.

### K. Off-season  (`LoadOffSeason`, `.OFF`)

| ID | Case | Exercises | Tier | St |
|---|---|---|---|---|
| K01 | No `.OFF` file | baseline — covered by A01 | T0 | [x] |
| K02 | Soil cover before the season | `SoilCoverBefore` | T1 | [x] |
| K03 | Soil cover after the season | `SoilCoverAfter` | T1 | [x] |
| K04 | Soil cover both sides, 100 % | full off-season mulch | T2 | [x] |
| K05 | Mulch effect off-season 25 / 50 / 100 % | `EffectMulchOffS` | T2 | [x] |
| K06 | 1 irrigation event BEFORE the growing period | `IrriBeforeSeason` | T1 | [x] |
| K07 | 5 irrigation events before (the array maximum) | `IrriBeforeSeason` full | T2 | [x] |
| K08 | 1 irrigation event AFTER the growing period | `IrriAfterSeason` | T1 | [x] |
| K09 | 5 irrigation events after | `IrriAfterSeason` full | T2 | [x] |
| K10 | Events both before and after | both title-skip branches | T2 | [x] |
| K11 | Pre-season irrigation water quality | `IrriECw_PreSeason` | T2 | [x] |
| K12 | Post-season irrigation water quality | `IrriECw_PostSeason` | T2 | [x] |
| K13 | Off-season fraction soil wetted | `IrriFwOffSeason` | T2 | [x] |
| K14 | Off-season with a sim period == cropping period | no off-season days to apply it to | T3 | [x] |
| K15 | Multi-run perennial with off-season between runs | interaction with `KeepSWC` | T2 | [x] |

### L. Salinity configuration

| ID | Case | Exercises | Tier | St |
|---|---|---|---|---|
| L01 | Non-saline baseline | — | T0 | [x] |
| L02 | Initial ECe 2 dS/m uniform | just at the crop's lower threshold | T1 | [x] |
| L03 | Initial ECe 4 dS/m uniform | moderate salt stress | T1 | [x] |
| L04 | Initial ECe 8 dS/m uniform | strong salt stress | T1 | [x] |
| L05 | Initial ECe 16 dS/m (== the crop's ECmax) | upper salinity bound, no growth | T2 | [x] |
| L06 | Initial ECe 25 dS/m (above ECmax) | beyond the tolerance range | T3 | [x] |
| L07 | ECe increasing with depth | salt profile gradient | T1 | [x] |
| L08 | ECe decreasing with depth | leached-surface profile | T2 | [x] |
| L09 | Crop ECn == ECx (step response) | degenerate tolerance range | T3 | [x] |
| L10 | Salt diffusion factor 0 % | no diffusion between cells | T2 | [x] |
| L11 | Salt diffusion factor 20 % | Ottawa default — covered by L03 | T0 | [x] |
| L12 | Salt diffusion factor 100 % | full mixing | T2 | [x] |
| L13 | Salt solubility 50 g/l | early precipitation | T2 | [x] |
| L14 | Salt solubility 100 g/l | Ottawa default — covered by L03 | T0 | [x] |
| L15 | Salt solubility 500 g/l | no precipitation | T2 | [x] |
| L16 | Calibrated CC distortion 0 / 25 / 100 % | `CalibratedCCdistortion` | T2 | [x] |
| L17 | Stomatal response to ECsw 0 / 100 / 200 % | `ResponseECsw` | T2 | [x] |
| L18 | Salinity with a shallow saline water table | G16 × L03 interaction | T1 | [x] |
| L19 | Salinity with saline irrigation and leaching | I33 × I34 interaction | T1 | [x] |
| L20 | Salt outputs enabled (`Out4Salt`, `Out6CompEC`) | salinity report writers | T1 | [x] |

### M. Growing-season calendar  (`.CAL`)

`LoadCropCalendar` is never called by the standalone program. The GUI uses the
`.CAL` file to generate the onset date and writes the result into the `.PRM` as
a fixed date, which is all the standalone reads. So the onset criteria of a
`.CAL` are not tested. The perennial dormancy criteria in the `.CRO` are read
but likewise never used (O7).

| ID | Case | Exercises | Tier | St |
|---|---|---|---|---|
| M01 | `.CAL` referenced in the `.PRM` | stored in `CalendarFile`, never opened | T2 | [x] |
| M02 | `(None)` for the calendar file | the no-calendar project | T2 | [x] |
| M03 | Referenced `.CAL` missing from disk | should be harmless — nothing opens it | T3 | [x] |

### N. Program parameters  (`.PPn`, `LoadProgramParametersProject`, `*.PAR`)

**The `.PPn` must be named after the project.** `ComposeFileForProgramParameters`
(`startunit.F90`) looks for `PARAM/<project name>.PPn` (`.PP1` for a `.PRO`). A
parameter file with any other name is ignored without a warning, and the
built-in defaults from `initialsettings.f90` are used. The harness renames the
`.PPn` a case stages to match its project, and checks that it was read.
`Ottawa.PPn` holds the same values as the built-in defaults.

| ID | Case | Exercises | Tier | St |
|---|---|---|---|---|
| N01 | No project `.PPn` | built-in defaults | T1 | [x] |
| N02 | `.PPn` present but short (truncated file) | partial read — must stop with a message (BUG-21) | T3 | [x] |
| N03 | Evaporation decline factor 1 / 4 / 8 | stage-II evaporation decline | T2 | [x] |
| N04 | Kex 1.00 / 1.10 / 1.20 | maximum soil evaporation coefficient | T2 | [x] |
| N05 | CC threshold for HI 0 / 5 / 20 % | HI cut-off on a senescing canopy | T2 | [x] |
| N06 | Root expansion start depth 50 / 70 / 100 % of Zmin | `rootunit.f90` curve start | T2 | [x] |
| N07 | Max root-zone expansion 1 / 5 cm/day | daily expansion cap | T2 | [x] |
| N08 | Shape factor for water stress on root expansion | −6 vs 0 vs +6 | T2 | [x] |
| N09 | Germination TAW threshold 5 / 20 / 50 % | `CheckGermination` | T1 | [x] |
| N10 | Germination threshold never met | germination never triggered | T3 | [x] |
| N11 | p-adjustment by ETo factor 0.5 / 1.0 / 2.0 | `AdjustpLeafToETo` and friends | T2 | [x] |
| N12 | Days to full aeration effect 1 / 3 / 7 | `DelayLowOxygen` | T2 | [x] |
| N13 | Senescence exponent 0.5 / 1.0 / 2.0 | drop in photosynthetic activity | T2 | [x] |
| N14 | p(sen) decrease 0 / 12 / 30 % | early-senescence trigger | T2 | [x] |
| N15 | Top-soil thickness 5 / 10 / 20 cm | top-soil depletion reporting | T2 | [x] |
| N16 | Evaporation depth `EvapZmax` 15 / 30 cm | evaporation-layer extent | T1 | [x] |
| N17 | `EvapZmax` == `EvapZmin` | the `EvapZmax > EvapZmin` guard — covered by N16a: EvapZmin is the parameter 15, so N16a IS the equal case | T3 | [x] |
| N18 | Depth for CN adjustment 0.10 / 0.30 m | AMC determination depth | T2 | [x] |
| N19 | Salt diffusion factor via `Soil.PAR` | `ReadSoilSettings` | T2 | [x] |
| N20 | Capillary-rise shape factor 8 / 16 / 32 | `RootNrDF` | T2 | [x] |
| N21 | Default Tmin/Tmax 12/28 °C (no T file) | pairs with C20 | T1 | [x] |
| N22 | Default Tmin/Tmax set below Tbase | no GDD accumulation at all | T3 | [x] |
| N23 | GDD method parameter 1 / 2 / 3 | `ReadTemperatureSettingsParameters` | T1 | [x] |
| N25 | `pMethod_NoCorrection` (crop-file flag 0) | p not adjusted by ETo | T1 | [x] |
| N26 | `pMethod_FAOCorrection` (flag 1) | Ottawa default — covered by A01 | T0 | [x] |
| N28 | Project `.PPn` overriding a global default | precedence of project over global — covered by A01 and every other case staging a .PPn: the lookup has one level, not two | T1 | [x] |

### O. Output & reporting  (`run.f90`, `inforesults.f90`)

| ID | Case | Exercises | Tier | St |
|---|---|---|---|---|
| O01 | No `DailyResults.SIM` | no daily output at all | T1 | [x] |
| O02 | Daily output 1 only (water balance) | `Out1Wabal` | T1 | [x] |
| O03 | Daily output 2 only (crop) | `Out2Crop` | T1 | [x] |
| O04 | Daily output 3 only (profile / root-zone WC) | `Out3Prof` | T1 | [x] |
| O05 | Daily output 4 only (salinity) | `Out4Salt` | T1 | [x] |
| O06 | Daily output 5 only (WC at depths) | `Out5CompWC` | T1 | [x] |
| O07 | Daily output 6 only (EC at depths) | `Out6CompEC` | T1 | [x] |
| O08 | Daily output 7 only (climate) | `Out7Clim` | T1 | [x] |
| O09 | Daily output 8 only (irrigation events) | `Out8Irri` — needs an `.IRR` case | T1 | [x] |
| O10 | All 8 daily outputs together | full `*day.OUT` | T1 | [x] |
| O11 | Daily outputs 5 and 6 with 12 compartments | widest possible daily record | T2 | [x] |
| O12 | Daily outputs 5 and 6 with 3 compartments | narrowest record | T2 | [x] |
| O13 | `DailyResults.SIM` listing a line twice | duplicate selection | T3 | [x] |
| O14 | `DailyResults.SIM` listing an out-of-range number | invalid selection | T3 | [x] |
| O15 | Aggregation 0 (none) | `OutputAggregate = 0` — covered by A01 | T0 | [x] |
| O16 | Aggregation 1 (daily) | `OutputAggregate = 1` | T1 | [x] |
| O17 | Aggregation 2 (10-daily) | `OutputAggregate = 2` | T1 | [x] |
| O18 | Aggregation 3 (monthly) | `OutputAggregate = 3` | T1 | [x] |
| O19 | Aggregation 2 on a season shorter than a decade | partial aggregation period | T3 | [x] |
| O20 | Aggregation 3 across a year boundary | monthly bins crossing years | T2 | [x] |
| O21 | No `ParticularResults.SIM` | neither harvests nor evaluation | T2 | [x] |
| O22 | Particular 1 only (multiple cuttings) | `Part1Mult` → `*harvests.OUT` — covered by J33 | T1 | [x] |
| O23 | Particular 2 only (evaluation) | `Part2Eval` → `*evaluation.OUT` — covered by O25 | T1 | [x] |
| O24 | Particular 1 on a crop with no cuttings | empty harvests file | T3 | [x] |
| O25 | `.OBS` with canopy-cover observations | `typeObsSim_ObsSimCC` | T1 | [x] |
| O26 | `.OBS` with biomass observations | `typeObsSim_ObsSimB` — covered by A01 | T1 | [x] |
| O27 | `.OBS` with soil-water-content observations | `typeObsSim_ObsSimSWC` | T1 | [x] |
| O28 | `.OBS` with all three, several seasons | per-run evaluation files | T1 | [x] |
| O29 | `.OBS` with observations outside the sim period | out-of-range observations | T3 | [x] |
| O30 | `.OBS` with a single observation | degenerate statistics (R², RMSE) | T3 | [x] |
| O31 | `.OBS` with all values −9 (missing) | all-missing statistics | T3 | [x] |
| O32 | No `.OBS` but `Part2Eval` requested | graceful skip | T3 | [x] |
| O33 | Season output for a multi-run project | `*season.OUT` totals — covered by A01 | T0 | [x] |
| O34 | `AllDone.OUT` and `ListProjectsLoaded.OUT` content | run-completion reporting — covered by A01 | T0 | [x] |
| O35 | Output file names > 8 characters | project-name truncation in output names | T3 | [x] |
| O36 | Evaluation in a single-run `.PRM` | `Part2Eval` with one run (BUG-9) | T1 | [x] |

### P. Stress & failure paths

| ID | Case | Exercises | Tier | St |
|---|---|---|---|---|
| P01 | Mild water stress on canopy expansion | `p-exp` upper/lower thresholds | T1 | [x] |
| P02 | Water stress closing stomata | `p-sto` | T1 | [x] |
| P03 | Water stress triggering early senescence | `p-sen` | T1 | [x] |
| P04 | Water stress at pollination | `p-pol` | T2 | [x] |
| P05 | Full crop failure from drought | zero yield | T2 | [x] |
| P06 | Perennial permanently wilted in dormancy | `SumETo` dormancy threshold (600) | T2 | [x] |
| P07 | Cold stress on pollination (Tmin < 8 °C) | pollination failure | T1 | [x] |
| P08 | Heat stress on pollination (Tmax > 40 °C) | pollination failure | T1 | [x] |
| P09 | Cold and heat stress in the same season | both branches in one run | T2 | [x] |
| P10 | Insufficient GDD for full transpiration | `GDtranspLow` | T2 | [x] |
| P11 | Deficient aeration (waterlogging) | `DayAnaero`, `AnaeroPoint` | T1 | [x] |
| P12 | Aeration stress lasting past `DelayLowOxygen` | full aeration effect | T1 | [x] |
| P13 | Water + fertility stress together | stress combination | T2 | [x] |
| P14 | Water + salinity stress together | stress combination | T2 | [x] |
| P15 | Water + fertility + salinity + temperature | all four at once | T2 | [x] |
| P16 | Very short cycle (~20 days) | short-season arithmetic | T3 | [x] |
| P18 | CCx tiny (0.05) | degenerate canopy | T3 | [x] |
| P19 | Zero plant density | degenerate CCo | T3 | [x] |
| P20 | Crop never emerging | germination never satisfied | T3 | [x] |

---

# Part II — Kernel coverage

Each group takes one family of `simul.f90` routines and drives its branches with
the smallest input that reaches them. Most cases are one season, with season
output only.

### Q. Drainage & internal redistribution  (`calculate_drainage`, `calculate_delta_theta`, `calculate_theta`)

| ID | Case | Exercises | Tier | St |
|---|---|---|---|---|
| Q01 | Uniform profile draining from saturation | the ordinary `drainability` path | T1 | [x] |
| Q02 | Drainage capped by `drainmax` | `drainsum <= drainmax` false | T1 | [x] |
| Q05 | `theta_x > SAT` (low-tau layer) | the whole `theta_x > SAT` block | T1 | [x] |
| Q06 | Excess generated, pushed up one compartment | `excess > 0`, `pre_nr < compi` | T1 | [x] |
| Q07 | Excess propagating up several compartments | the `pre_nr` walk | T1 | [x] |
| Q10 | Fine layer under a coarse layer | drainage limited at a boundary | T1 | [x] |
| Q11 | Coarse layer under a fine layer | free drainage below a slow layer | T1 | [x] |
| Q12 | Ksat 5 mm/day throughout (tau near zero) | slowest drainage | T1 | [x] |
| Q13 | Ksat 1500 mm/day (tau at its ceiling) | fastest drainage | T2 | [x] |
| Q15 | Drainage with a groundwater table raising FCadj | `FCadj` in place of FC | T1 | [x] |
| Q16 | Drainage with gravel reducing pore volume | gravel × drainage | T2 | [x] |
| Q17 | Profile starting below FC (no drainage) | the no-op path | T2 | [x] |
| Q18 | Repeated saturation over consecutive days | day-to-day drainage continuity | T2 | [x] |
| Q19 | Drainage with 3 compartments | minimal compartment count | T2 | [x] |
| Q20 | Drainage with 12 graded compartments | thickness-weighted redistribution | T1 | [x] |

### R. Runoff, surface storage & infiltration  (`calculate_runoff`, `calculate_surfacestorage`, `calculate_Extra_runoff`, `calculate_infiltration`)

`calculate_infiltration` is the largest kernel in the model, so it has the most
cases.

| ID | Case | Exercises | Tier | St |
|---|---|---|---|---|
| R01 | Daily rain record, CN runoff | `datatype_daily` branch of `calculate_runoff` — covered by A01 | T0 | [x] |
| R06 | Profile drier than WP in the runoff depth | the `theta < WP` weighting branch | T2 | [x] |
| R08 | Weighted wetness sum > 1 | `SUM > 1` clamp | T3 | [x] |
| R09 | `MaxDepth` reached before the last compartment | the `CumDepth >= MaxDepth` exit | T2 | [x] |
| R10 | `MaxDepth` deeper than the profile | the `compi == NrCompartments` exit | T2 | [x] |
| R11 | Rain onto bunds, below the bund height | surface storage accumulating | T1 | [x] |
| R12 | Rain onto bunds, overtopping | `SurfaceStorage > BundHeight*1000` | T1 | [x] |
| R13 | Surface storage before the cropping period | `dayi < Crop_Day1` | T2 | [x] |
| R14 | Surface storage after the cropping period | `dayi > Crop_DayN` | T2 | [x] |
| R15 | Rain exceeding layer-1 Ksat | `Sum > InfRate` branch | T1 | [x] |
| R16 | Irrigation alone exceeding layer-1 Ksat | `calculate_Extra_runoff` irrigation branch | T1 | [x] |
| R17 | Rain + irrigation exceeding Ksat together | the combined branch | T1 | [x] |
| R18 | Infiltration into an unsaturated profile | the ordinary store path — covered by A01 | T0 | [x] |
| R26 | Water still to store after the whole profile | `amount_still_to_store > 0` at the end | T1 | [x] |
| R28 | Infiltration into a profile already at SAT | zero storage capacity | T2 | [x] |
| R29 | Infiltration through a sand-over-clay boundary | perching at a layer interface | T1 | [x] |
| R30 | Infiltration with a shallow groundwater table | FCadj reducing capacity | T1 | [x] |
| R31 | 100 mm in one day onto a dry deep profile | full-profile wetting front | T1 | [x] |
| R32 | 100 mm in one day onto a shallow profile | immediate deep percolation | T1 | [x] |
| R33 | Rain on the first day of the simulation | day-1 initialisation of the balance | T2 | [x] |
| R36 | Effective rain with surface storage present | `SurfaceStorage > 0` branch | T2 | [x] |
| R39 | Effective rain, `SubDrain > DrainMax`, no bunds | the `Bundheight < 0.001` branch | T2 | [x] |
| R40 | Effective rain, `SubDrain > DrainMax`, with bunds | the bunded branch | T2 | [x] |
| R43 | USDA effective rain with `RainMonth <= 0.1` | the dry-month guard | T3 | [x] |

### S. Soil evaporation  (`PrepareStage1/2`, `CalculateSoilEvaporationStage1/2`, `ExtractWaterFromEvapLayer`, `WCEvapLayer`, `AdjustEpotMulchWettedSurface`, `CalculateEvaporationSurfaceWater`, `ConcentrateSalts`)

| ID | Case | Exercises | Tier | St |
|---|---|---|---|---|
| S01 | Bare soil, wet, stage-I evaporation | `CalculateSoilEvaporationStage1` | T1 | [x] |
| S02 | Drying soil crossing into stage II | `PrepareStage2`, the REW threshold | T1 | [x] |
| S03 | Long dry-down, deep into stage II | `CalculateSoilEvaporationStage2` | T1 | [x] |
| S04 | Rewetting resetting to stage I | `PrepareStage1` | T1 | [x] |
| S05 | ETo exactly 5 mm/day | the `abs(ETo - 5) > 0.01` reference branch | T2 | [x] |
| S06 | ETo 1 vs 5 vs 12 mm/day | the ETo scaling of stage II | T1 | [x] |
| S07 | `EvapZmax` 0.15 m | shallow evaporation layer | T1 | [x] |
| S08 | `EvapZmax` 0.30 m (Ottawa) | covered by A01 | T0 | [x] |
| S10 | Evaporation layer spanning two soil layers | `WCEvapLayer` across a boundary | T1 | [x] |
| S12 | Top compartment reaching WP/2 | the `theta <= WP/200` floor | T2 | [x] |
| S16 | REW 3 mm (short stage I) | early transition | T1 | [x] |
| S17 | REW 15 mm (long stage I) | late transition | T1 | [x] |
| S18 | Mulch 50 % reducing Epot | `AdjustEpotMulchWettedSurface` mulch term | T1 | [x] |
| S19 | Drip irrigation wetting 30 % of the surface | the wetted-surface term | T1 | [x] |
| S20 | Mulch and partial wetting together | both terms in one call | T1 | [x] |
| S21 | Off-season mulch (`SoilCoverBefore`) | the off-season mulch branch | T2 | [x] |
| S22 | Evaporation from ponded water on bunds | `CalculateEvaporationSurfaceWater` | T1 | [x] |
| S24 | Salts concentrating as water evaporates | `ConcentrateSalts` | T1 | [x] |
| S25 | Concentration past the solubility limit | precipitation into `Depo` | T1 | [x] |
| S26 | Evaporation under full canopy | `EffectCanopyCoverInReducingSoilEvap` at 60 % | T1 | [x] |
| S27 | Evaporation under a senescing canopy | late-season evaporation increase | T2 | [x] |

### T. Transpiration & root water uptake  (`calculate_transpiration`, `surface_transpiration`, `calculate_weighting_factors`, `DetermineRootZoneWC`)

| ID | Case | Exercises | Tier | St |
|---|---|---|---|---|
| T01 | Unstressed transpiration at full canopy | the reference path | T1 | [x] |
| T02 | Transpiration with water stress (Ks < 1) | the stomatal Ks factor | T1 | [x] |
| T03 | Transpiration with salinity stress | `Coeffb0Salt`/`b1`/`b2` | T1 | [x] |
| T04 | Transpiration with both water and salt stress | combined coefficients | T1 | [x] |
| T05 | Uptake distributed over 3 compartments | `calculate_weighting_factors` | T1 | [x] |
| T06 | Uptake distributed over 12 graded compartments | thickness-weighted uptake | T1 | [x] |
| T10 | `Smax` top vs bottom quarter (0.020 / 0.010) | the extraction-rate gradient | T1 | [x] |
| T11 | `Smax` equal top and bottom | uniform extraction | T2 | [x] |
| T14 | Anaerobic compartments past `DelayLowOxygen` | `surface_transpiration` aeration branch | T1 | [x] |
| T15 | `AnaeroPoint = 0` (no aeration stress) | the `AnaeroPoint > 0` guard | T2 | [x] |
| T16 | Transpiration from surface water (paddy) | `SurfaceStorage > KsReduction*Part*Tpot` | T1 | [x] |
| T19 | Kc ageing decline 0 / 11 / 30 % | the ageing term | T2 | [x] |
| T20 | `DetermineRootZoneWC` with `ZtopSWCconsidered` | top-soil water content reporting | T1 | [x] |
| T21 | Root zone water content at FC | the no-depletion reference | T1 | [x] |
| T22 | Root zone water content at WP | full depletion | T1 | [x] |
| T23 | Root zone spanning two soil layers | layer-weighted TAW/RAW | T1 | [x] |
| T24 | Transpiration with a groundwater table in the root zone | uptake from a saturated zone | T1 | [x] |
| T25 | Transpiration under weed competition | `WeedRC` reducing crop Tr | T2 | [x] |
| T26 | Transpiration with a partially wetted surface | drip irrigation × Tr | T2 | [x] |

### U. Capillary rise & groundwater inflow  (`calculate_CapillaryRise`, `HorizontalInflowGWTable`, `CalculateAdjustedFC`)

| ID | Case | Exercises | Tier | St |
|---|---|---|---|---|
| U01 | No water table — no capillary rise | the disabled path — covered by A01 | T0 | [x] |
| U02 | Table at 2.0 m, dry profile above | ordinary capillary rise | T1 | [x] |
| U03 | Table at 0.6 m | strong rise into the root zone | T1 | [x] |
| U04 | Table at 0.3 m | rise into the evaporation layer | T1 | [x] |
| U05 | Rise limited by `MaxMM > LimitMM` | the upper limit branch (both occurrences) | T1 | [x] |
| U07 | Compartment already above `ThetaThreshold` | no rise into that compartment | T2 | [x] |
| U08 | Rise walking to compartment 1 | the `compi < 1` loop exit | T1 | [x] |
| U11 | Capillary rise per soil class (12 textures) | `NumberSoilClass` → CR parameters | T2 | [x] |
| U12 | Shape factor `RootNrDF` 8 / 16 / 32 | the water-content gradient effect | T2 | [x] |
| U13 | Saline capillary rise (`CRsalt`) | salt carried up with the water | T1 | [x] |
| U14 | Horizontal inflow, compartment below the table | `Zi >= DepthGWTmeter` | T1 | [x] |
| U16 | Horizontal inflow with an EC difference | `ECeComp != ECiAqua` | T1 | [x] |
| U18 | `CalculateAdjustedFC` with the table below the profile | no FC adjustment | T2 | [x] |
| U19 | `CalculateAdjustedFC` with the table inside the profile | graded FC adjustment | T1 | [x] |
| U20 | Table rising during the season | FC adjustment changing daily | T1 | [x] |

### V. Salt transport  (`calculate_saltcontent`, salt cells, `Calculate_SaltMobility`)

| ID | Case | Exercises | Tier | St |
|---|---|---|---|---|
| V01 | Non-saline profile, salt balance closes at zero | the trivial path — covered by A01, L01 | T0 | [x] |
| V02 | 2 salt cells (Ksat 1200) | `SCP1 = 2`, coarsest discretisation | T0 | [x] |
| V03 | 4 salt cells (Ksat 500) | `SCP1 = 4` | T1 | [x] |
| V04 | 7 salt cells (Ksat 200) | `SCP1 = 7` | T1 | [x] |
| V05 | 11 salt cells (Ksat ≤ 112) | `SCP1 = 11`, finest discretisation | T1 | [x] |
| V06 | `SCP1` clamped to its minimum of 2 | very high Ksat | T3 | [x] |
| V07 | Different `SCP1` per layer | cell-count change across a boundary | T1 | [x] |
| V08 | Salt moving down with infiltrating rain | downward advection | T1 | [x] |
| V09 | Salt moving down with irrigation | advection from irrigation water | T1 | [x] |
| V10 | Salt leaving with deep percolation | `SaltOut` | T1 | [x] |
| V11 | Salt entering with capillary rise | `CRSalt` | T1 | [x] |
| V13 | Salt diffusion between cells, 0 % | `Calculate_SaltMobility` at zero | T2 | [x] |
| V14 | Salt diffusion between cells, 20 % | Ottawa default — covered by V02 | T0 | [x] |
| V15 | Salt diffusion between cells, 100 % | full mobility | T2 | [x] |
| V16 | Salt precipitating at the solubility limit | `Depo` filling | T1 | [x] |
| V17 | Precipitated salt redissolving | `Depo` emptying | T1 | [x] |
| V20 | Salt with surface storage present | salt in ponded water | T2 | [x] |
| V22 | Full leaching to a non-saline profile | complete salt removal | T1 | [x] |
| V24 | ECe → ECsw conversion in the root zone | `DetermineRootZoneSaltContent` | T1 | [x] |

### W. Canopy development  (`DetermineCCi`, `DetermineCCiGDD`, `FeedbackCC`, `GetCDCadjustedNoStressNew`)

| ID | Case | Exercises | Tier | St |
|---|---|---|---|---|
| W01 | Unstressed canopy, calendar days | `DetermineCCi` reference path | T1 | [x] |
| W02 | Unstressed canopy, GDD | `DetermineCCiGDD` reference path — covered by T01, I22 | T1 | [x] |
| W03 | Same canopy both ways | calendar/GDD canopy equivalence | T1 | [x] |
| W04 | Emergence delayed by dry soil | `CheckGermination` gating CC start | T1 | [x] |
| W05 | Canopy expansion limited by water stress | `StressLeaf` | T1 | [x] |
| W06 | Canopy expansion limited by fertility stress | `CCxAdjusted`, `CCoAdjusted` | T1 | [x] |
| W07 | Canopy expansion limited by salinity | salt effect on CC | T1 | [x] |
| W08 | Canopy never reaching CCx | truncated expansion | T1 | [x] |
| W09 | Canopy reaching CCx exactly at senescence | boundary | T2 | [x] |
| W10 | Early senescence triggered by water stress | `TimeSenescence`, `WithBeta` | T1 | [x] |
| W11 | Senescence with an adjusted CDC | `GetCDCadjustedNoStressNew` | T1 | [x] |
| W12 | Senescence recovery after rewetting | the un-senescence path | T1 | [x] |
| W13 | `FeedbackCC` correcting CC | the feedback branch | T2 | [x] |
| W14 | CC decline to zero before maturity | full canopy loss | T2 | [x] |
| W15 | Canopy after a cut (forage regrowth) | `CCcut`, CGC increase — covered by A01 | T1 | [x] |
| W16 | Two cuts in quick succession | repeated regrowth | T2 | [x] |
| W17 | Cut before canopy closure | regrowth from a partial canopy | T2 | [x] |
| W18 | Weed-infested canopy, `WeedRC` at closure | total vs crop CC | T1 | [x] |
| W19 | Weeds with `WeedDeltaRC` in mid-season | time-varying weed cover | T2 | [x] |
| W20 | Weeds replacing self-thinned perennial CC | `WeedAdj` | T2 | [x] |
| W21 | Perennial self-thinning across years | CCx decline with age | T2 | [x] |
| W22 | CCo from plant density | `SizeSeedling`, plants/ha | T1 | [x] |
| W23 | CCo from re-growth canopy size | `SizePlant` for perennials — covered by A01 | T1 | [x] |
| W24 | Plant density 10× the crop default | dense stand | T2 | [x] |
| W25 | Plant density 1/10 the crop default | sparse stand | T2 | [x] |
| W26 | CGC 0.05 vs 0.18 vs 0.30 | expansion rate sensitivity | T2 | [x] |
| W27 | CDC 0.01 vs 0.036 vs 0.10 | decline rate sensitivity | T2 | [x] |
| W28 | Transplanted crop starting above CCo | transplant canopy | T1 | [x] |
| W29 | Canopy under aeration stress | waterlogging reducing CC | T2 | [x] |
| W30 | Canopy with `CCini` from `.SW0` | mid-season canopy restart | T1 | [x] |

### X. Biomass, harvest index & yield  (`DeterminePotentialBiomass`, `DetermineBiomassAndYield`)

| ID | Case | Exercises | Tier | St |
|---|---|---|---|---|
| X01 | Unstressed biomass accumulation | the WP × Tr/ETo reference path | T1 | [x] |
| X02 | Biomass under water stress | reduced Tr → reduced B | T1 | [x] |
| X03 | Biomass under fertility stress | `FracBiomassPotSF` — covered by A01 | T1 | [x] |
| X04 | Biomass under salinity stress | salt-limited B | T1 | [x] |
| X05 | WP adjusted for CO2 below 369.41 ppm | downward CO2 adjustment | T1 | [x] |
| X06 | WP at exactly 369.41 ppm | no adjustment — covered by D28 | T1 | [x] |
| X07 | WP adjusted for CO2 above 369.41 ppm | upward CO2 adjustment — covered by A01 | T1 | [x] |
| X08 | CO2 at 800 ppm (far future) | strong CO2 effect | T2 | [x] |
| X09 | WP* for a C3 vs a C4 crop | the WP parameter itself | T1 | [x] |
| X10 | WP change during yield formation | the post-anthesis WP factor | T1 | [x] |
| X11 | HI reference, grain crop | `subkind_Grain` HI build-up | T1 | [x] |
| X12 | HI reference, tuber crop | `subkind_Tuber` | T1 | [x] |
| X13 | HI reference, vegetative crop | `subkind_Vegetative` | T1 | [x] |
| X14 | HI for forage at each cut | `subkind_Forage` yield per cut — covered by A01 | T1 | [x] |
| X15 | HI increased by water stress before flowering | positive HI adjustment | T2 | [x] |
| X16 | HI reduced by stress during yield formation | negative HI adjustment | T1 | [x] |
| X17 | HI reduced by pre-anthesis canopy loss | the `HIadj` vegetative term | T2 | [x] |
| X18 | HI blocked by low green CC | the PPn CC threshold for HI | T1 | [x] |
| X19 | HI with pollination failure (cold) | `KsPolC` | T1 | [x] |
| X20 | HI with pollination failure (heat) | `KsPolH` | T1 | [x] |
| X21 | HI with both pollination stresses | combined pollination failure | T2 | [x] |
| X22 | Yield with a fresh/dry mass conversion | forage fresh yield reporting | T2 | [x] |
| X23 | Biomass with a premature end | truncated accumulation | T2 | [x] |
| X24 | Zero biomass (crop failure) | the degenerate yield path | T2 | [x] |
| X25 | `Bini` from `.SW0` carried in | biomass restart | T1 | [x] |
| X26 | Potential vs actual vs unlimited biomass | `BiomassPot`, `BiomassUnlim`, `BiomassTot` — covered by A01 | T1 | [x] |

### Y. Stress engine  (`EffectSoilFertilitySalinityStress`, `CropStressParametersSoilFertility`, `Adjustp*ToETo`)

| ID | Case | Exercises | Tier | St |
|---|---|---|---|---|
| Y01 | Fertility stress 0 % | the no-stress shortcut | T1 | [x] |
| Y02 | Fertility stress at the crop's calibration point | the calibrated response — covered by A01 | T0 | [x] |
| Y03 | Fertility stress below the calibration point | interpolation downward | T1 | [x] |
| Y04 | Fertility stress above the calibration point | interpolation upward | T1 | [x] |
| Y05 | Fertility shape factor for canopy expansion | `ShapeCGC` | T1 | [x] |
| Y06 | Fertility shape factor for CCx | `ShapeCCX` | T1 | [x] |
| Y07 | Fertility shape factor for WP | `ShapeWP` (negative in the alfalfa file) | T1 | [x] |
| Y08 | Fertility shape factor for CC decline | `ShapeCDecline` | T1 | [x] |
| Y09 | `StressSFadjNEW` recomputed mid-season | the dynamic adjustment path | T1 | [x] |
| Y10 | Salinity stress alone driving the stress engine | `Coeffb0Salt` from salinity only | T1 | [x] |
| Y11 | Fertility and salinity stress together | the combined `EffectStress` | T1 | [x] |
| Y12 | `AdjustpLeafToETo` at ETo 1 mm/day | low-ETo p adjustment | T1 | [x] |
| Y13 | `AdjustpLeafToETo` at ETo 12 mm/day | high-ETo p adjustment | T1 | [x] |
| Y14 | `AdjustpStomatalToETo` across the ETo range | stomatal p adjustment | T1 | [x] |
| Y17 | `pMethod_NoCorrection` — no ETo adjustment at all | the disabled path | T1 | [x] |
| Y18 | p-exp upper == lower threshold | degenerate stress range | T3 | [x] |
| Y19 | Stress shape factor 0.0 (straight line) | the linear Ks branch | T1 | [x] |
| Y20 | Stress shape factor 3.0 (Ottawa) | the curved Ks branch — covered by A01 | T0 | [x] |
| Y21 | Stress shape factor 6.0 | strongly curved Ks | T2 | [x] |
| Y22 | Temperature stress on biomass (`KsTr`) | `GDtranspLow` on cool days | T2 | [x] |

### Z. Balance integrity & suite invariants  (`CheckWaterSaltBalance`)

Most of group Z is not cases but rules that `run_tests.py` applies to every
case, so they need no stored output. What each check does is described after
the table.

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
| Z18 | No floating-point exception under `-ffpe-trap` | build-level, not a case: `runner/check_builds.sh fpe` | T2 | [x] |
| Z19 | Same results at `-O0` and `-O2` within tolerance | build-level, not a case: `runner/check_builds.sh o0` | T2 | [x] |
| Z20 | Every `.OUT` file the run should produce exists | output completeness | T1 | [i] |

### What group Z checks

**Rules, on every case** (`runner/invariants.py`). The water balance, day by day,
over the soil and the surface pond together:

    d(WC + Surf) == Rain + Irri + CR - RO - Drain - E - Tr

The two are balanced together because evaporation from ponded water is part of
`E` but never entered the soil. The check needs the flux columns of daily block 1
(O10), and it is taken within each run: unless the project uses `KeepSWC`, soil
water restarts on the first day of a run, so a difference across that boundary
is a reset, not a flux. Over the season, `Rain + Irri + stored == Infilt +
Runoff`, skipped where the `.MAN` declares bunds, since water still ponded on
the last day is in no season column. Further: the salt balance (Z03), the signs
of the fluxes, percentages between 0 and 100 (`-9` is `undef_int`, the model's
value for an undefined ratio, and is accepted), no NaN or Inf, every compartment
between empty and saturated, the rooting depth within the root zone, and that a
staged `.PPn` was read. A case can switch off a rule that does not apply to it
with `skip_invariants:` in its `case.yml`; O2 and O9 say which cases do and why.

**Determinism (Z13).** `run_tests.py --repeat 3` runs each case three times in
separate folders and requires byte-identical output.

**Order independence (Z14).** `run_tests.py --shuffle` runs the cases in random
order.

**Equivalence (Z15).** `runner/check_equivalence.py` compares a two-run `.PRM`
(Z15a) with the same two seasons as separate `.PRO` projects (Z15b, Z15c).
Without `KeepSWC` the runs are independent, so every seasonal column must agree.

**Build variants (Z18, Z19).** `runner/check_builds.sh` rebuilds AquaCrop, runs
the suite against that build, and restores the production binary afterwards,
also when something fails:

    ./tests/runner/check_builds.sh fpe    # Z18: trap invalid, zero and overflow
    ./tests/runner/check_builds.sh o0     # Z19: -O0 with -fcheck=all, against the -O2 references
    ./tests/runner/check_builds.sh snan   # uninitialised reals set to a signalling NaN

These compare one build with another, so they run with `--ulp 1`. Run `snan` on
the cases you are investigating rather than on all of them: AquaCrop leaves some
variables unset on purpose.

---

# Part III — Input-file version compatibility

### VC. Input-file versions

Every loader reads the version number in line 2 of its file. The suite uses
current-format files, so for most files only the current format is covered, by
the baseline. Older soil files are covered by F54f and F54g.

| ID | Case | Exercises | Tier | St |
|---|---|---|---|---|
| VC01 | `.CRO` at v7.3 | current format — covered by A01 | T0 | [x] |
| VC10 | `.MAN` at v7.3 | current format — covered by A01 | T0 | [x] |
| VC15 | `.SOL` at v7.3 | current format — covered by A01 | T0 | [x] |

---

# Part IV — Generated sweep families

The groups above are hand-designed: each case drives one specific branch. The
families below are grids over axes that interact, generated by
`assets/gen_sweeps.py`. Generated case ids are `SWnn_<factor>_<factor>`.

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

251 cases, season output only, so the whole part costs under a megabyte of
reference. Every one still runs the rules of group Z, which is most of their
value. Cells that would not be a distinct code path are left out of the grids:
a GDD method has no meaning for a calendar-day crop, and an irrigation method
none in net-requirement mode. SW07 has no 10-daily temperature.

---

# Branches no input file reaches

These branches cannot be driven from a project's input files, so the suite does
not test them. They fall into four kinds:

**Fixed in the source.** The value exists in a file but nothing can change it,
or it never reaches a file at all: `IniAbstract` is read and then set to 5 on
the next line; `WithBeta` is assigned in code; the `.CRO` perennial block is
read and never used (O7).

**Internal clamps.** `Runoff >= Shower`, `EffecRain < 0`, `excess < 0`,
`theta_nul > SAT` and the like guard values the model computes for itself
part-way through a routine. An input can make them more likely, but nothing in a
project file selects them.

**Within-timestep transitions.** "Ponded water exhausted mid-day", "surface
water insufficient for Tpot": real branches, but they open and close inside one
daily step and leave no separate trace in the daily output.

**Quantities never reported.** `UL`, `Dx`, `Macro`, `SinkMajor`, `SinkMinor` and
the salt-cell indices are internal geometry. Their effects show up in water and
salt content, which the cases do cover, but the values themselves are not
observable.

Reaching them needs a debug build that writes internal state, or unit tests
that call the routines directly. The ids are those the rows had before they
were taken out of the groups.

| Id | Branch | Why no input reaches it |
|---|---|---|
| A11 | Path with a trailing slash vs without | path concatenation — paths are concatenated verbatim (`//`, startunit.F90:208), so dropping the slash names a file that does not exist |
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

# Defects found by the suite

**The suite does not carry cases that are known to fail.** When a case finds a
defect in AquaCrop, the defect is written up here under its own
`### BUG-n — <what goes wrong>` heading, followed by an italic line saying what
found it and how bad it is, then the source lines and the proposed fix. The case
is removed until AquaCrop is fixed, so a failing run always means something
new. When the fix lands, the case comes back and the defect is added to the
table *Fixed defects*.

No defect is open. The 23 below were found by this suite and are fixed in the
code the references were frozen from. The cases named guard against their
return.

### BUG-1 — legacy `.SOL` files fail on a token count, not a layer count

*Found by F54. Guarded by F54f and F54g.*

### BUG-2 — a `.SOL` declaring more than five horizons segfaults

*Found by F56. Guarded by F56.*

### BUG-4 — a single-compartment profile writes a duplicate output column

*Found by F07. Guarded by F07.*

### BUG-5 — a thin first horizon terminates the crop, and `SaltStr` reads 1000 %

*Found by F44. Guarded by F44.*

### BUG-6 — an unbounded search over the climate dataset runs off the array or hangs

*Found by D02 and D08. Guarded by D02, D08 and D23.*

### BUG-7 — `C2Max = C2Max` in the decadal temperature reader

*Found while reading the code for BUG-6. Reachable only with a record of a single observation.*

### BUG-8 — salt solubility above 127 g/l aborts the run

*Found by L15. Guarded by L15.*

### BUG-9 — evaluation against field data crashes on a single-run `.PRM`

*Found by O25 to O31. Guarded by O36.*

### BUG-10 — a year-agnostic climate record segfaults when the project names real years

*Found by D17 and D18. Guarded by D18.*

### BUG-11 — a project list containing a blank line crashes

*Found by A07. Guarded by A07.*

### BUG-12 — four of six input loaders open their file without checking it exists

*Found by A09 and A10. Guarded by A09 and A10.*

### BUG-13 — a listed project that does not exist still produces output files

*Found by A08. Guarded by A08, A09 and A10.*

### BUG-14 — the year-undefined groundwater branch reads past end of file

*Found by G13. Guarded by G13.*

### BUG-15 — a crop that can never accumulate growing degrees hangs

*Found by C13 and N22. Guarded by C13 and N22.*

### BUG-16 — running past the end of the climate record crashes

*Found by D20. Guarded by D20, D21 and D23.*

### BUG-17 — `LoadOffSeason` reads into an unallocated deferred-length string

*Found by the Z18 build. Guarded by K06 to K14.*

### BUG-18 — floating-point exceptions in the daily loop

*Found by the Z18 build (check_builds.sh fpe), on U02 to U16, V11, C29, P10 and F37.*

### BUG-19 — the salt-cell loop walks off the bottom of the array

*Found by the Z19 build (check_builds.sh o0) on Q16.*

### BUG-21 — the program-parameter loader reads 25 values without `iostat`

*Found by N02. Guarded by N02.*

### BUG-22 — soil evaporation uses an unset reduction coefficient at a 15 cm evaporation layer

*Found by N16a and S07 on the snan build. Guarded by N16a and S07.*

### BUG-23 — values read before they are set

*Found by the snan build (check_builds.sh snan).*

### BUG-24 — a period starting before the climate record runs on shifted weather

*Found by D19. Guarded by D19.*

### BUG-25 — the default CO2 record is read at start-up, and is stale across runs

*Found while reviewing PR #379. Guarded by D30 and D31.*

## Fixed defects

| Defect | What changed |
|---|---|
| BUG-1 | legacy `.SOL` layer lines are read as text; the description is the rest of the line |
| BUG-2 | more than five horizons gives a warning, then the run stops |
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
| BUG-17 | off-season irrigation events are read |
| BUG-18 | no `log(0)` in capillary rise for a compartment exactly at wilting point (U02 to U16, V11; PR #384); no 0/0 depletion before the crop has roots (C29, P10); no division by a zero rooting depth (F37). A soil with FC equal to WP would still divide by zero in capillary rise; no case builds one |
| BUG-19 | salt drainage corrected for gravel, and the drain loop stops at the first cell |
| BUG-21 | a `.PPn` with too few values gives a warning, then the run stops |
| BUG-22 | the evaporation reduction coefficient is computed at a 15 cm evaporation layer too |
| BUG-23 | the daily fluxes and the evaporation-layer state are set before they are read |
| BUG-24 | a simulation or cropping period starting before the climate record stops with an error |
| BUG-25 | the default CO2 record is resolved per run, not read once at start-up (PR #379) |

---

# Observations

Behaviour the suite found that is not a defect to fix, but matters when reading
the output or the checks.

## O2 — under `IrriMode_Inet` the reported `Irri` never crosses the soil surface

*Found by I22 and I23. Behaviour of the model, not a defect, but a trap when reading output.*

In net-irrigation-requirement mode the `Irri` column is a computed requirement,
not water that entered the profile: `Infilt` and `Runoff` account for rainfall
alone, and the water reaches the root zone without appearing in the daily
soil-water balance. Anyone adding up `Rain + Irri` over runs over-counts
wherever a run used Inet mode.

The three Inet cases, I22 to I24, carry `skip_invariants: [surface_balance,
daily_soil_balance, daily_surface_balance]`. I24 would balance only because its
requirement happens to be zero. Every other irrigation mode closes to print
precision.

## O3 — initial surface storage is discarded unless the field has bunds

*Found by S22. Behaviour of the model, not a defect.*

The `.SW0` record "water layer (mm) stored between soil bunds (if present)" is
used only when the management file declares bunds. S22 (bunds 0.30 m) keeps the
20 mm its `.SW0` sets; H11 and H12 (no bunds) stage the same file, and the 20 mm
is dropped without anything in the output saying so.

So H11 and H12 test the discarded path, not ponded initial conditions. The
season surface-balance rule takes the stored water as a third input term
(`surface_storage_in` in `case.yml`).

## O5 — the reported rooting depth can exceed the root zone actually used

*Found by F39, F40 and F41. A reporting inconsistency; the water balance is correct.*

On a profile with a restrictive horizon, the daily `Z` column reports the
rooting depth the crop would have reached without the restriction, deeper than
the root zone the model accounts water over. The `Wr(x)` header names the depth
actually used, and it matches `ZrAdjustedToRestrictiveLayers`. So the Z07 and
Z08 rules check the `Wr` header, not `Z`. Whether `Z` should be clamped is a
question for the output specification.

| case | soil | `Soil_RootMax` | `Wr(...)` header | max `Z` reported |
|---|---|---|---|---|
| F41 | `PEN_25` | 0.975 | **0.98** | **2.50** |
| F39 | `PEN_50` | 1.650 | **1.65** | **3.00** |
| F40 | `PEN_graded` | 0.900 | **0.90** | **2.10** |

`Z` is `-9` (`undef_int`) where no crop established, as in P10.

## O7 — the perennial calendar block of the crop file is read but never used

*Confirmed in the source. The model reads input it does not use.*

`LoadCrop` reads the perennial calendar block of the `.CRO` (dormancy onset and
end criteria, search windows, thresholds, occurrences) into `PerennialPeriod_*`,
and nothing reads it back: the `GetPerennialPeriod_*` accessors are used only in
`global.f90` itself. As with the `.CAL` file (group M), the GUI turns these
criteria into dates and writes them into the `.PRM`, which is what the
standalone reads.

So B16 is only partly tested, and a case built on that block would pass without
testing anything. Where an input may be used by the GUI only, `grep` for its
getter outside `global.f90` before building a case on it.

## O9 — precipitated salt is reported by nothing

*Found by Z03. A gap in the output, not a loss of salt.*

AquaCrop keeps salt in two stores per compartment: `Salt`, in solution, and
`Depo`, precipitated. Only `Salt` reaches the output (`run.f90` never mentions
`Depo`). Salt that precipitates leaves the reported `Salt(x)` without passing
through `SaltOut`, so the salt balance computed from the output closes only
where nothing precipitates.

The four cases where salt from a water table precipitates, U13, L18, U16 and
V11, carry `skip_invariants: [salt_balance]`; their references are exact.
Proving conservation would need `Depo` in an output column.

## O10 — cases without daily block 1 have no daily water-balance check

*A limit of the daily balance rule.*

The daily water balance needs the water content (daily block 3 or 5) and the
fluxes (block 1). A case that asks for block 3 or 5 without block 1 has no daily
balance check, and `invariants.py` skips it without saying so. Adding `1` to its
`daily:` list brings the check back, at the cost of re-freezing its reference.

## O11 — at 100 % soil fertility stress a GDD crop still grows, by accident

*Found by J12 and SW05. Changes with the GDD-native refactor.*

At 100 % soil fertility stress, `CropStressParametersSoilFertility` gives
`RedCGC = RedCCX = 100`: CCx is zero, and no crop should grow. In J12
(`MAN_fert100.MAN`, a GDD crop) one does, because the gate that keeps fertility
stress off before emergence counts calendar days (`VirtualTimeCC <
DaysToGermination`), while emergence on the GDD clock comes a day earlier. The
canopy gets one day without stress, during which the adaptive stress level is
recomputed once, from 100 to 80 (`RedCGC = 12`, `RedCCX = 55`): heavy, but
survivable.

The GDD-native refactor puts that gate on the crop's own clock, so that 100 %
stress leaves no crop, which was decided to be the right behaviour. J12 and SW05
move when it lands.
