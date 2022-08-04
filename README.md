# AquaCrop

AquaCrop v7.0 (July 2022) is the first open-source Fortran-based version
of AquaCrop, developed at KU Leuven, and officially distributed by FAO.
Compared to AquaCrop v6.0, the AquaCrop v7.0 features bug fixes,
performance improvements and internal restructuring,
a translation from Pascal to Fortran,
and a range of new and/or updated scientific features.

The following applications are publicly distributed along with the
AquaCrop v7.0 source code:
* AquaCrop standard Windows graphical user interface (under [Releases](
  https://github.com/KUL-RSDA/AquaCrop/releases))
* AquaCrop Windows standalone executable (under [Releases](
  https://github.com/KUL-RSDA/AquaCrop/releases)) for
  * Windows
  * Linux
  * MacOS

From v7.0 onwards, it will furthermore be possible to use AquaCrop
as a crop model within [NASA’s Land Information System (LIS)](
https://github.com/NASA-LIS/LISF). More information can be found in
the LIS section below.

## Running the executable

Download the ZIP file with the AquaCrop v7.0 executable for
Windows, Linux or MacOS from the release page.
A simple simulation example is provided along with the executable:
follow the instructions in README.md to run a testcase.

## Building the executable

Either unzip the ZIP file from the release page, or if you wish to contribute to
the code, then fork the repository and locally clone your fork.

Building the Aquacrop executable on a Linux system requires:

* GNU Make (>= v3.82)
* a GNU or Intel Fortran compiler (GNU Fortran >= v6.4.0 and ifort >= v18.0.1).
  MinGW can be used to (cross)compile for Windows.
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
the `aquacrop` binary is built from the Pascal code instead, linked to
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

## Optional testing of source code conversions against reference output

Running the test suite requires:

* Python3 with the `pytest` and `NumPy` packages
* An `$AQUACROP_TEST_ROOT` environment variable pointing to the location
  where the test suite input and reference output files are stored.
* An `$AQUACROP_COMMAND` environment variable point to the AquaCrop
  executable that you want to use.

> Note: when using executables built with `FORTRAN_EXE=0`, you will need to add
  the directory with the `libaquacrop.so` library to your `$LD_LIBRARY_PATH`,
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

The distribution of AquaCrop v.7.0 within LIS is currently being tested
and reviewed. To use AquaCrop v7.0 together with NASA's
[Land Information System (LIS)](https://github.com/NASA-LIS/LISF),
you currently need to:
* build the AquaCrop shared library,
* clone/download and compile the following development branch:
  [https://github.com/mbechtold/LISF/tree/ac.7.0_integration](
   https://github.com/mbechtold/LISF/tree/ac.7.0_integration)
