# lifecontingencies 1.4.4

# lifecontingencies 1.4.3

# lifecontingencies 1.4.2

<!-- NEWS.md is maintained by https://cynkra.github.io/fledge, do not edit -->

# lifecontingencies 1.4.1 (2025-09-16)

- Same as previous version.


# lifecontingencies 1.4.0 (2025-09-16)

- Same as previous version.


---
title: "News"
output: pdf_document
---

## lifecontingencies 1.3.12

- Updated R version dependency to R >= 4.2.0 due to `markovchain` package requirements.
- Bug fix: Ensured the default value for `n` in `axn()` and `Axn()` is non-negative.

## lifecontingencies 1.3.11

- Removed metatable from lifecontingencies.md.

## lifecontingencies 1.3.10

- Lowered R version requirements to 4.1.
- Added a `NEWS.md` file for tracking package changes.
- Fixed class calls.
- Transitioned to bibentry and resolved missing links.

## Version 1.3.8

- Resolved issues in bibliography.
- Removed vignette introduction due to LaTeX incompatibility.

## Version 1.3.7

- Added mortality projections with `StMoMo`.
- Introduced a new vignette on Pension Evaluation by Ivan Williams.
- Added DOI.

## Version 1.3.5

- Implemented vectorization of `pxt()`, `qxt()`, `axn()`, `Axn()`.
- Began vectorization of `axyzn()` (development version).
- Started transitioning documentation to Roxygen.

## Version 1.3.4

- Fixed an issue in Macaulay duration.
- Recreated old data in data-raw.

## Version 1.3.3

- Added clarifications on geometric discounting.

## Version 1.3.2

- Changes made in the Pension Funding Vignette.

## Version 1.3.1

- Removed .sty files as requested by CRAN.
- Added unit tests for mdt functions.
- Began deprecating `pxyt`, `qxyt`, `exyt`.

## Version 1.3

- Introduced `qxt.fromQxprime` to derive decrement from ASDT decrements.
- Updated the vignette on pension funding.

## Version 1.2.5

- Added a new vignette on pension funding.
- Included a section on associated single decrement tables.
- Added function to compute associated single decrement rate from multiple decrement tables.
- Made various fixes.

## Version 1.2.2

- Added registration of Dynamic libraries.
- Revised vignettes.

## Version 1.2.1

- Made small fixes.

## Version 1.2

- Added an initialize method for `actuarialtable` and `lifetable` classes.

## Version 1.1.14

- Fixed a small issue on removing NA from data.frames.
- Addressed medium bugs affecting `Axyn` and `Ax` (refer to GitHub issues ##1 and ##2).

## Version 1.1.12

- Made cosmetic amendments.

## Version 1.1.10

- Fixed `AExn=1` when `n=0`.
- Corrected `pxt` with multiple decrements when `t=0`.
- Implemented small changes to vignettes.

## Version 1.1.6

- Added GitHub repository.
- First use of Rcpp (hidden code).
- Introduced a new vignette for a quick introduction to the package.

## Version 1.1.5

- Replaced the Numerical Tests vignette with Unit Root testing.
- Resolved inconsistencies in the manual regarding `lifetable` and `actuarialtable` methods.
- Added `rmdt` to sample from multiple contingencies object.
- Updated description and namespace.

## Version 1.1

- Officially introduced multiple decrements in the package (added various methods and a vignette).
- Added an introductory vignette for multiple decrements.

## Version 1.0.6

- Modified vignettes to comply with CRAN policies (removed parallel function).
- Added SoA illustrative service table in data.
- Introduced multiple decrements through the `mdt` class.

## Version 1.0.5

- Resolved an issue with fractional probabilities (Kevin J. Owens).
- Fixed an issue with multiple lives random generation functions (Kevin J. Owens).
- Enhanced mortality vignettes.
- Enabled export of life tables as `markovchainList` objects.

## Version 1.0

- Fixed an issue with the print method on `actuarialtable`.
- Added reference to JSS Published Paper.
- Significantly enhanced the mortality vignette (thanks to Gian Paolo Clemente, co-author).

## Version 0.9.8.6

- Added annuities paid in advance/deferred options.
- Introduced functions `getLifecontingencyPV` and `getLifecontingencyXyzPV` for obtaining Present Value given the age of death.

## Version 0.9.8.5

- Fixed the `axn` function.
- Finalized the main vignette.

## Version 0.9.8

- Improved documentation.
- Added `as.numeric` method for `lifetable` and `actuarialtable` objects.
- Introduced a `power` argument to the APV function for evaluating higher moments.

## Version 0.9.7

- Minor changes to documentation.

## Version 0.9.6

- Added a summary method for `lifetable` and `actuarialtable` objects.
- Improvements and fixes in the simulation of life contingent insurances.
- Added `rLifeContingenciesXyz` function.
- Enhancements to the vignette.

## Version 0.9.5

- Added ALM section to the main vignette.
- Improved `rLife`.
- Added functions for switching between interest and discount rates.
- Ensured proper functioning of multiple life insurance functions.
- Introduced a function to generate variates from two life contingencies.
- Added French and UK life tables.

## Version 0.9.3

- Revised lifecontingencies vignettes to align with the Journal of Statistical Software style.
- Minor changes and bug fixes.

## Version 0.9.2

- Parallelized `rLifeContingencies`.
- Updated `demoIta` dataset.
- Added an n-year term insurance function `AExn`.
- Final revision of package vignettes.

## Version 0.9.1

- Standardized conventions on parameters.
- Minor changes in vignettes and function documentation.

## Version 0.9

- Added `rLifeContingencies` function for simulating random variates from life contingencies distribution.
- Deep revision of vignettes.
- Various bug fixes.

## Version 0.1.1

- Minor fixes.
- Set R required version to 2.14.

## Version 0.1

- Deep revision of vignettes.
- Added increasing annuity function `Iaxn`.
- Minor fixes.

## Version 0.0.7

- Added `rLife` function for simulating random times until death.
- Improved financial mathematics functions.
- Enhanced documentation and vignettes.

## Version 0.0.6

- Enhanced documentation.
- Improved financial functions.
- Added function for obtaining lifetables from raw survival or

