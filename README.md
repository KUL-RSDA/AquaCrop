# FAO AquaCrop

<img src="https://encrypted-tbn1.gstatic.com/images?q=tbn:ANd9GcSEmcLG0zbWXIaLClg09v77ZccbFH_zuDVRbH-eBLxAdmcZ4nZ7" align="right" width="200px">

AquaCrop v7.0 is released as open-source Fortran code, 
developed at KU Leuven and the Food and Agriculture Organization (FAO) 
of the United Nations (FAO and KU Leuven copyright),
and based on the original AquaCrop v6.0 (FAO copyright). 
Compared to AquaCrop v6.0, the AquaCrop v7.0 features bug fixes,
performance improvements and internal restructuring,
a translation from Pascal to Fortran,
and a range of new and/or updated scientific features.

The following applications are publicly distributed:
* AquaCrop v7.0 version-controlled open **source code** (this GitHub page, and zip-file under [Releases](
  https://github.com/KUL-RSDA/AquaCrop/releases))
* AquaCrop v7.0 standard Windows **graphical user interface** (zip-file under [Releases](
  https://github.com/KUL-RSDA/AquaCrop/releases))
* AquaCrop v7.0 **standalone executable** (zip-file under [Releases](
  https://github.com/KUL-RSDA/AquaCrop/releases)) for
  * Windows
  * Linux
  * MacOS

From v7.0 onwards, it is also possible to use AquaCrop
as a crop model within [NASA’s Land Information System (LIS)](
https://github.com/NASA-LIS/LISF). More information can be found in
the LIS section below.

## Documentation

Online documentation and contact information is available at the [FAO website](https://www.fao.org/aquacrop/en/). The AquaCrop core team is small and answers will be found fastest in the release notes, training handbooks and youtube videos provided by FAO.

Please also visit our [Discussions](https://github.com/KUL-RSDA/AquaCrop/discussions) forum for FAQ, or to contribute.

## Running the executable

Download the ZIP file with the AquaCrop v7.0 executable for
Windows, Linux or MacOS from the release page. 
Consult the reference manual (FAO website) for details about the AquaCrop stand-alone program.

Optionally, it can be verified if the executable produces the expected output on the user's system, by running a self-contained testcase for which reference output is provided (zip-file under [Releases](
  https://github.com/KUL-RSDA/AquaCrop/releases)).

## Building the executable

Either unzip the ZIP file with the source code from the release page, 
or if you wish to contribute to the code, then fork the repository and locally clone your fork.

The source code is under the `src` directory. Building the Aquacrop executable on a Linux system requires:

* GNU Make (>= v3.82)
* a GNU or Intel Fortran compiler (GNU Fortran >= v6.4.0 and ifort >= v18.0.1).
  MinGW can be used to (cross)compile for Windows. In that case `make` needs
  to be called with an additional `CPPFLAGS=-D_WINDOWS` option.
* (optional) a Free Pascal compiler (>= v3.2.0).

```bash
cd AquaCrop/src
make
```

The main `make` targets are `bin` (producing an `aquacrop` executable),
`lib` (producing a `libaquacrop.so` library). The default target is
`all`, which combines the `bin` and `lib` targets.

## Optional build instructions

If the `FORTRAN_EXE` option is set to `0` (instead of the default `1`),
the `aquacrop` binary is built from the original Pascal code (instead of the Fortran version), linked to
the (Fortran-compiled) `libaquacrop.so` library. This approach can
currently only be used together with a GNU Fortran compiler.

> Note: The coupling is rather trivial since the Pascal program consists
  of a single call to `StartTheProgram()`, but it still provides a basic
  check of the Pascal interface.

For example:
```bash
cd AquaCrop/src
make
make clean          # cleans all build artifacts (binaries, object files, ...)
make DEBUG=1        # uses debug options for compiling
make FORTRAN_EXE=0  # builds a Pascal-based executable (instead of Fortran-based)
make STATIC=1       # builds a statically-linked binary for distribution
                    # as a standalone executable (only for GNU Fortran and
                    # with DEBUG=0 and FORTRAN_EXE=1)
make FC=ifort       # use the Intel Fortran compiler instead of GNU Fortran
```

## Optional contributing and support

Please follow good practices. New features, enhancements or suggestions will only be considered and reviewed once a year by the core AquaCrop developers.

We encourage scientific (only) exchanges via our [Discussions](https://github.com/KUL-RSDA/AquaCrop/discussions) forum. Only if the wealth of [documentation](https://www.fao.org/aquacrop/en/) or the FAO Contact (aquacrop@fao.org) did not provide sufficient help, or if you have a good suggestion, then start a new "Discussion" with the information you already gathered from the FAO Contact or documentation. Please do not open an "Issue" to ask your question and do not offer a "Pull Request" without any prior "Discussion" with the AquaCrop core team.

## Optional code development and testing of source code changes against reference output

A test suite for code development is available under the `tests` directory.
Running the test suite requires:

* Python3 with the `pytest` and `NumPy` packages
* An `$AQUACROP_TEST_ROOT` environment variable pointing to the location
  where the test suite input and reference output files are stored.
* An `$AQUACROP_COMMAND` environment variable point to the AquaCrop
  executable that the user wants to use.

> Note: when using executables built with `FORTRAN_EXE=0`, the user needs to add
  the directory with the `libaquacrop.so` library to the `$LD_LIBRARY_PATH`,
  because the AquaCrop executable is dynamically linked to it.

For example:
```bash
export AQUACROP_TEST_ROOT=/path/to/your/aquacrop/test/root/dir
export AQUACROP_COMMAND=$PWD/AquaCrop/src/aquacrop
export LD_LIBRARY_PATH=$PWD/AquaCrop/src:$LD_LIBRARY_PATH

cd AquaCrop/tests

# Run the whole test suite (will take about one hour):
pytest

# Run only 'Perennial' test case, with output shown:
pytest -s test_perennial.py

# Show all the so-called "parametrizations" of the "Europe" test case:
pytest test_europe.py --collect-only

# Run just two subcases of the "Europe" test case:
pytest test_europe.py -k 'False-1-16 or True-19-0'
```

> Note: by default, the tests will make use of temporary working directories
  inside $TMPDIR. The location of these directories can be changed by setting
  the `AQUACROP_TMPDIR` variable accordingly.

## LIS integration

The distribution of AquaCrop v.7.0 within NASA's LIS is currently being reviewed
and will be accessible after approval of an upcoming pull request to NASA via the [LIS GitHub page](https://github.com/NASA-LIS/LISF).

## Citation

A wide range of publications is available to refer to AquaCrop in the GUI or standalone version.
The users can refer to any publication of their choice, when using these AquaCrop assets. 

In case the open source code of AquaCrop v7.0 is used, please refer to one of the following papers, covering precursory work in preparation for the open source release. Publications using AquaCrop v7.0 in NASA's LIS will be added for reference as soon as they become available.
* de Roos, S., De Lannoy, G.J.M., Raes, D. (2021). Performance analysis of regional AquaCrop (v6.1) biomass and surface soil moisture simulations using satellite and in situ observations. Geoscientific Model Development, 14(12), 7309-7328, 10.5194/gmd-14-7309-2021.
* Busschaert, L., de Roos, S., Thiery, W., Raes, D., De Lannoy, G.J.M. (2022). Net irrigation requirement under different climate scenarios using AquaCrop over Europe. Hydrology and Earth System Sciences, 26, 3731–3752, 10.5194/hess-26-3731-2022.

