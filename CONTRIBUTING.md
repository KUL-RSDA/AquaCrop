# Contributing

A few things to keep in mind before committing changes:

- make sure that the code compiles in both `DEBUG=1` and `DEBUG=0` mode

- make sure that the `perennial` test and selected `europe` tests pass
  (with the executable compiled in `DEBUG=0` mode)

- use commit messages like `ENH: convert <name> to Fortran` when commiting
  a Pascal-to-Fortran conversion

- more tips on writing clear commit messages can be found here:
  https://wiki.fysik.dtu.dk/ase/development/contribute.html#writing-the-commit-message

- remove trailing spaces (you can spot them with `git diff`)

- consider following FORD-style docstring formats:
  https://github.com/Fortran-FOSS-Programmers/ford
  https://github.com/Fortran-FOSS-Programmers/ford/wiki/Writing-Documentation
  This will allow to automatically generate e.g. HTML documentation pages.
