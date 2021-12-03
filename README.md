# AquaCrop


## Installation

Building the Aquacrop executable requires:

* GNU Make
* the Free Pascal compiler
* the GNU Fortran compiler

Running `make` with the default options will build the executable by
(1) compiling the Fortran code into a shared library,
(2) compiling the Pascal code into an executable, linked to this shared library.

For example:
```bash
cd AquaCrop/src
make
make clean      # cleans both the Fortran and Pascal build artifacts
make DEBUG=1    # adds debug options to both compilers
```


## Testing

Running the test suite requires:

* Python3 with the `pytest` and `NumPy` packages
* An `$AQUACROP_TEST_ROOT` environment variable pointing to the location
  where the test suite input and reference output files are stored.
* An `$AQUACROP_COMMAND` environment variable point to the AquaCrop
  executable that you want to use.

> Note: you will need to add the directory with `libaquacrop.so` library
  to your `$LD_LIBRARY_PATH`, because the AquaCrop executable is dynamically
  linked to it.


For example:
```bash
export AQUACROP_TEST_ROOT=/path/to/your/aquacrop/test/root/dir
export AQUACROP_COMMAND=$PWD/AquaCrop/src/PlugInBareV70
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
