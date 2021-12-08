# Contributing

A few things to keep in mind before committing and pushing changes:

- Do not work directly on the `main` branch (except for e.g. correcting typos).
  Instead, create a separate branch (e.g. "convert_global_TimeToMaxCanopySF")
  and create a pull request for merging it into the `main` branch.

- Make sure that the code compiles in both `DEBUG=1` and `DEBUG=0` mode.

- Make sure that the `perennial` test and selected `europe` tests pass
  (with the executable compiled in `DEBUG=0` mode).

- Use commit messages like `ENH: convert <name> to Fortran` when commiting
  a Pascal-to-Fortran conversion.

- More tips on writing clear commit messages can be found here:
  https://wiki.fysik.dtu.dk/ase/development/contribute.html#writing-the-commit-message

- Remove trailing spaces (you can spot them with `git diff`).

- Consider following FORD-style docstring formats:
  https://github.com/Fortran-FOSS-Programmers/ford
  https://github.com/Fortran-FOSS-Programmers/ford/wiki/Writing-Documentation
  This will allow to automatically generate e.g. HTML documentation pages.
