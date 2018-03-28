# Contributing Guidelines

## Repository structure

This repository is structured as a standard R package
following the conventions outlined in the [Writing R
extensions](http://cran.r-project.org/doc/manuals/R-exts.html) manual.

## Code

All code for this package is found in `R/`. All functions should be thoroughly
documented with `roxygen2` notation; see Documentation. Code should
conform to the rOpenSci [Style guide](https://github.com/ropensci/onboarding/blob/master/packaging_guide.md).

## Testing

NOTE: We are still in the process of formulating our tests. Testing is a high priority of this project and will be added soon! Below are the guidelines for testing that we will be implementing.

Any new feature or bug-fix should include a unit-test demonstrating the
change.  Unit tests follow the `testthat` framework with files in
`tests/testthat`. Please make sure that the testing suite passes
before issuing a pull request. This can be done by running `check()`
from the `devtools` package, which will also check for consistent
documentation, etc.

This package uses the [travis](https://github.com/craigcitro/r-travis)
continuous testing mechanism for R to ensure that the test suite is run
on each push to Github.  An icon at the top of the README.md indicates
whether or not the tests are currently passing. 

This package also uses
codecov.io to
measure test coverage. While not all code can be covered by automated 
tests (in particular, functions involving user prompts), try to avoid
decreasing coverage by writing unit tests for any contributed code. 
Codecov.io will flag PRs that decrease coverage. 

## Documentation

All of the function documentation is generated automatically.
Please do not edit any of the documentation files in `man/`
or the `NAMESPACE`. Instead, construct the appropriate
[roxygen2](https://github.com/klutometis/roxygen) documentation in the
function files in `R/` themselves. The documentation is then generated
by running the `document()` function from the `devtools` package. Please
consult the [Advanced R programming](http://adv-r.had.co.nz/) guide if
this workflow is unfamiliar to you. Note that functions should include
examples in the documentation. Please use `\dontrun` for examples that
take more than a few seconds to execute or require an internet connection.

## General Development Goals & Guidelines

1. High-level functions requiring little to no knowledge of EML or R.
2. Functions to auto-detect metadata from data entities.
3. Check user inputs for valid entries.
4. Errors and warnings should guide users to solutions.

## Attribution

These contributing guidelines are based on those of the [rOpenSci EML project](https://github.com/ropensci/EML/blob/master/CONTRIBUTING.md).
