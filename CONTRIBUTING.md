# Contributing

A few things to keep in mind before committing and pushing changes:

- Do not work directly on the `main` branch (except for e.g. correcting typos).
  Instead, create a separate branch and create a pull request for merging it
  into the `main` branch.

- Make sure that the code compiles in both `DEBUG=1` and `DEBUG=0` mode.

- Make sure that all the tests pass with the executable compiled in `DEBUG=0`
  and `FORTRAN_EXE=1` mode.

- Tips on writing clear commit messages can be found here:
  https://wiki.fysik.dtu.dk/ase/development/contribute.html#writing-the-commit-message

- Do not insert trailing spaces (you can spot them with `git diff`).

- Consider following FORD-style docstring formats:
  https://github.com/Fortran-FOSS-Programmers/ford
  https://github.com/Fortran-FOSS-Programmers/ford/wiki/Writing-Documentation
  This will allow to automatically generate e.g. HTML documentation pages.
