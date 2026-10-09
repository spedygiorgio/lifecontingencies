# Changelog

## lifecontingencies 1.6.3

- Minimum supported R raised from 4.1.0 to 4.4.0 (`Depends`); the CI
  full matrix now also checks R 4.4.

- Portability of floating-point comparisons (found by the macOS/aarch64
  CI job, R 4.6.1, Apple clang 21): the `mdt` validity check and
  `.tableSanitizer()` compared the total of the decrement columns with
  `lx` for *exact* equality, so a table whose decrements come from
  floating-point rates was accepted or rejected depending on how the
  platform happened to round.
  [`buildMdtFromIndependentRates()`](https://spedygiorgio.github.io/lifecontingencies/reference/buildMdtFromIndependentRates.md)
  balanced to the last bit on x86 but was rejected with
  `invalid class "mdt" object: Check the lx` on aarch64, where the
  compiler contracts `a * b + c` into a fused multiply-add. Both
  comparisons now allow a relative tolerance of 1e-8
  (`.MDT_BALANCE_TOL`); genuinely unbalanced tables are still rejected,
  and integer-valued published tables are unaffected.

- `test-pxt-lifetable-native.R` no longer requires the native
  [`pxt()`](https://spedygiorgio.github.io/lifecontingencies/reference/pxt.md)
  kernel to be *bit for bit* identical to the former R code, which is
  not a portable property for the “linear” and “hyperbolic” assumptions
  (same fused multiply-add: a few ulps of difference). It now asserts
  that the degenerate cases agree exactly (which entries are `NaN`,
  which are zero) and that the finite values agree to a relative
  tolerance of 1e-12. The kernel’s own values are unchanged.

- `_pkgdown.yml` added to `.Rbuildignore`: it was shipped in the tarball
  and raised a “Non-standard file/directory found at top level” NOTE
  under `R CMD check --as-cran`.

- [`dxt()`](https://spedygiorgio.github.io/lifecontingencies/reference/pxt.md)
  on `lifetable`/`actuarialtable` objects is now vectorised over `x` and
  `t` (recycled to a common length) and looks ages up with
  [`match()`](https://rdrr.io/r/base/match.html) instead of
  [`which()`](https://rdrr.io/r/base/which.html) per call;
  [`Tx()`](https://spedygiorgio.github.io/lifecontingencies/reference/other-demographic-functions.md)
  uses it instead of an [`sapply()`](https://rdrr.io/r/base/lapply.html)
  loop. Results are unchanged on every integer and fractional case
  previously computed (checked bit for bit on SOA 2008 and two other
  tables); fractional `t` reaching beyond the last tabulated age used to
  return `numeric(0)` and now correctly counts no further deaths.

- New function
  [`independentRatesFromMdt()`](https://spedygiorgio.github.io/lifecontingencies/reference/independentRatesFromMdt.md):
  extracts the full Associated Single Decrement Table (ASDT) as a matrix
  of independent rates $q\prime_{x}^{(j)}$ for every combination of age
  and decrement in an `mdt` object, under the UDD assumption. This is
  the vectorized convenience wrapper around
  [`qxt.prime.fromMdt()`](https://spedygiorgio.github.io/lifecontingencies/reference/qxt.prime.fromMdt.md).

- New function
  [`buildMdtFromIndependentRates()`](https://spedygiorgio.github.io/lifecontingencies/reference/buildMdtFromIndependentRates.md):
  constructs an `mdt` object from a matrix of independent (ASDT) rates,
  the inverse of
  [`independentRatesFromMdt()`](https://spedygiorgio.github.io/lifecontingencies/reference/independentRatesFromMdt.md).
  Uses the UDD integration formula via
  [`qxt.fromQxprime()`](https://spedygiorgio.github.io/lifecontingencies/reference/qxt.prime.fromMdt.md)
  to convert independent rates to absolute rates, then builds the
  survivorship column recursively.

- New [`plot()`](https://rdrr.io/r/graphics/plot.default.html) S4 method
  for `mdt` objects: produces a `ggplot2` visualisation with three views
  — stacked area chart of decrement counts (default), stacked bar chart,
  or line chart of decrement-specific probabilities. Requires `ggplot2`
  (already in `Imports`).

- The multiple-decrement vignette (`multiple_decrement_tables.Rmd`) now
  documents the three new functions with worked examples from
  Finan (2014) and a round-trip verification (mdt → ASDT → mdt).

- [`dxt()`](https://spedygiorgio.github.io/lifecontingencies/reference/pxt.md),
  [`pxt()`](https://spedygiorgio.github.io/lifecontingencies/reference/pxt.md)
  and
  [`qxt()`](https://spedygiorgio.github.io/lifecontingencies/reference/pxt.md)
  are now S4 generics with methods for `lifetable` (hence
  `actuarialtable`) and `mdt`, replacing the hand-written dispatch on
  `class(object)`. Signatures and results are unchanged (checked bit for
  bit on lifetables, actuarial tables and mdt objects across fractional
  assumptions and ages); unsupported objects keep the historical error
  message, and the per-call overhead is lower than before. New table
  classes can now register their own methods.

- Fixed decrement-specific probabilities on `mdt` objects:
  [`pxt()`](https://spedygiorgio.github.io/lifecontingencies/reference/pxt.md)/`qxt(..., decrement = )`
  used an optimised branch that rebuilt a pseudo-`lx` from the
  cause-specific counts alone whenever the first row of the table
  contained no zero cell (e.g. a table supplied from age 0), returning
  `NaN` or values outside \[0, 1\]
  ([`qxt()`](https://spedygiorgio.github.io/lifecontingencies/reference/pxt.md)
  gave -2 on a simple 3-age table). They now compute
  `tq_x^(j) = td_x^(j) / l_x` directly, vectorised over `x` and `t`,
  with linear (UDD) interpolation for fractional `t`;
  [`dxt()`](https://spedygiorgio.github.io/lifecontingencies/reference/pxt.md)
  on an `mdt` also interpolates fractional `t` instead of truncating it.

- [`dxt()`](https://spedygiorgio.github.io/lifecontingencies/reference/pxt.md),
  [`pxt()`](https://spedygiorgio.github.io/lifecontingencies/reference/pxt.md)
  and
  [`qxt()`](https://spedygiorgio.github.io/lifecontingencies/reference/pxt.md)
  on `mdt` objects accept several decrements at once
  (`decrement = c("death", "disability")`), returning the
  probability/number of leaving the table because of any of them.

- [`pxt()`](https://spedygiorgio.github.io/lifecontingencies/reference/pxt.md)
  and
  [`qxt()`](https://spedygiorgio.github.io/lifecontingencies/reference/pxt.md)
  now really default to `t = 1` as documented (previously a missing `t`
  raised an error).

- [`Axn.mdt()`](https://spedygiorgio.github.io/lifecontingencies/reference/multidecrins.md)
  gains cause-dependent benefits (`benefits`, one per covered
  decrement), several decrements at once, a deferment `m`, cover on the
  total decrement when `decrement` is missing, and vectorisation over
  `x`/`n`/`m`. Its default term is now `n = omega + 1 - x - m`,
  consistent with
  [`Axn()`](https://spedygiorgio.github.io/lifecontingencies/reference/endowment_trio.md):
  the previous default `omega - x - 1` silently dropped the last
  tabulated age. The corresponding regression test, which asserted the
  2-year value 0.1364 for the 3-year term of Finan (2014) Problem 68.1,
  now checks the correct 3-year value (APV 4515.40).

- New
  [`axn.mdt()`](https://spedygiorgio.github.io/lifecontingencies/reference/multidecrins.md):
  APV of an annuity payable while no decrement has occurred, with
  deferment, `k` payments per year and payments in advance or in
  arrears. Together with
  [`Axn.mdt()`](https://spedygiorgio.github.io/lifecontingencies/reference/multidecrins.md)
  it reproduces the worked examples of Finan (2014) Sections 68-69
  (Examples 68.1, 69.1, 69.2, 69.3), now in
  `test-mdt-s4-actuarial-long.R`.

- New
  [`mdtToLong()`](https://spedygiorgio.github.io/lifecontingencies/reference/mdtToLong.md):
  converts an `mdt` into an aggregated long (time, status, count) data
  set for competing-risks analysis with
  [`survival::Surv`](https://rdrr.io/pkg/survival/man/Surv.html); the
  Aalen-Johansen cumulative incidence computed by **survival** on it
  coincides with `qxt(..., decrement = )`. `survival` added to
  `Suggests`.

- The multiple-decrement vignette documents
  [`Axn.mdt()`](https://spedygiorgio.github.io/lifecontingencies/reference/multidecrins.md)
  with cause-dependent benefits,
  [`axn.mdt()`](https://spedygiorgio.github.io/lifecontingencies/reference/multidecrins.md)
  (premiums and reserves from Finan’s examples) and
  [`mdtToLong()`](https://spedygiorgio.github.io/lifecontingencies/reference/mdtToLong.md).

- Internal performance:
  [`rLifeContingencies()`](https://spedygiorgio.github.io/lifecontingencies/reference/rLifeContingencies.md)
  and
  [`rLifeContingenciesXyz()`](https://spedygiorgio.github.io/lifecontingencies/reference/rLifeContingencies.md)
  now compute the per-sample payoff with vectorized C++ kernels
  (`.fAxnCppVec()`, `.faxnCppVec()`, `.fAxyznCppVec()` and relatives)
  instead of calling the scalar kernel element by element via
  [`sapply()`](https://rdrr.io/r/base/lapply.html)/[`apply()`](https://rdrr.io/r/base/apply.html).
  The whole `deathsTimeX` vector/matrix is processed in a single
  `.Call`, which removes the per-element interop overhead that dominated
  runtime for large `n`. The `parallel = TRUE` argument now drives
  OpenMP inside the C++ loop rather than a
  [`parallel::makeCluster()`](https://rdrr.io/r/parallel/makeCluster.html),
  and is strictly opt-in: it has an effect only if the user also sets
  `options(lifecontingencies.openmp = TRUE)`; the number of threads
  comes from the new `nthreads` argument or
  `options(lifecontingencies.nthreads)` (default 2, never
  auto-detected). Without the option, or when the shared library was
  built without OpenMP support, the sequential vectorized kernel is
  used. Return values are numerically equivalent to the previous
  implementation (checked on 116 configurations of
  [`rLifeContingencies()`](https://spedygiorgio.github.io/lifecontingencies/reference/rLifeContingencies.md)/[`rLifeContingenciesXyz()`](https://spedygiorgio.github.io/lifecontingencies/reference/rLifeContingencies.md)
  against the previous release, max abs. difference 9e-15;
  `test-sim-vector-kernels.R` pins each kernel to its scalar reference),
  and [`set.seed()`](https://rdrr.io/r/base/Random.html) +
  `parallel = TRUE` produces the same realized sample as
  `parallel = FALSE`; the only intended change is the multi-life fix
  below. `Makevars`/`Makevars.win` now pass `$(SHLIB_OPENMP_CXXFLAGS)`.

- Fixed
  [`rLifeContingenciesXyz()`](https://spedygiorgio.github.io/lifecontingencies/reference/rLifeContingencies.md)
  when the lives have different issue ages: the vector `x` was added to
  the matrix of simulated lifetimes recycled down the column-major
  storage, so ages were swapped across lives (simulated `Axyz` for ages
  40 and 60 deviated from
  [`Axyzn()`](https://spedygiorgio.github.io/lifecontingencies/reference/Multiple-life-insurances.md)
  by more than 10 standard errors; equal ages were unaffected). Ages are
  now added column-wise; covered by a new test.

- Memory-safety hardening of the native code, found with an
  AddressSanitizer/UBSan build: the multi-life kernels reject a matrix
  with no columns (was a null dereference) and use 64-bit indices; the
  annuity kernel no longer converts an unbounded number of payment dates
  to an integer (undefined behaviour for `n = Inf` or very large `k`)
  and uses an `expm1` formulation that stays accurate for tiny payment
  steps; `.pxtCpp()` no longer reads out of bounds when
  `omega >= length(lx)` and no longer casts huge/NaN ages to `int`, and
  uses `R_xlen_t` sizes. New tests in `test-native-memory-safety.R`.

- New demographic functions on life tables:
  [`varxn()`](https://spedygiorgio.github.io/lifecontingencies/reference/varxn.md)
  and
  [`sdxn()`](https://spedygiorgio.github.io/lifecontingencies/reference/varxn.md)
  (variance and standard deviation of the curtate/complete future
  lifetime, the second-moment companions of
  [`exn()`](https://spedygiorgio.github.io/lifecontingencies/reference/exn.md)),
  [`modalAge()`](https://spedygiorgio.github.io/lifecontingencies/reference/modalAge.md)
  (Lexis modal age at death, with optional parabolic interpolation), and
  [`median()`](https://spedygiorgio.github.io/lifecontingencies/reference/median.md)/[`quantile()`](https://spedygiorgio.github.io/lifecontingencies/reference/quantile.md)
  methods for `lifetable`/`actuarialtable` objects (age-at-death
  distribution, optionally conditional on survival to a given `age`).
  [`Tx()`](https://spedygiorgio.github.io/lifecontingencies/reference/other-demographic-functions.md)
  and
  [`exn()`](https://spedygiorgio.github.io/lifecontingencies/reference/exn.md)
  gain `fxt` and `axOmega` arguments controlling the within-year
  distribution of deaths and the closure of the last, open age interval
  (`L_omega = axOmega * l_omega`), so published open-interval tables
  (e.g. NCHS, `L_omega = l_omega / m_omega`) can be reproduced; the
  defaults leave existing results unchanged.
  [`print()`](https://rdrr.io/r/base/print.html) for a `lifetable`
  forwards `exType` (“curtate”/“complete”), `fxt` and `axOmega` and
  returns the tabulated `data.frame` invisibly. New vignette
  *Demographic analysis with the lifecontingencies package* and tests in
  `test-demographic-extras.R`.

- Internal performance:
  [`pxt()`](https://spedygiorgio.github.io/lifecontingencies/reference/pxt.md)
  (and hence
  [`qxt()`](https://spedygiorgio.github.io/lifecontingencies/reference/pxt.md),
  [`axn()`](https://spedygiorgio.github.io/lifecontingencies/reference/endowment_trio.md),
  [`Axn()`](https://spedygiorgio.github.io/lifecontingencies/reference/endowment_trio.md),
  [`AExn()`](https://spedygiorgio.github.io/lifecontingencies/reference/endowment_trio.md),
  [`IAxn()`](https://spedygiorgio.github.io/lifecontingencies/reference/arithmetic_variation_insurances.md),
  [`Iaxn()`](https://spedygiorgio.github.io/lifecontingencies/reference/Iaxn_1.md),
  [`DAxn()`](https://spedygiorgio.github.io/lifecontingencies/reference/arithmetic_variation_insurances.md),
  [`axyzn()`](https://spedygiorgio.github.io/lifecontingencies/reference/Multiple-life-insurances.md),
  [`Axyzn()`](https://spedygiorgio.github.io/lifecontingencies/reference/Multiple-life-insurances.md)
  and the other functions built on it) now computes survival
  probabilities for `lifetable`/`actuarialtable` objects with a native
  kernel, `.pxtLifetableCpp()`, an exact port of the former R code
  (name-based lookup, NA-to-zero replacement of the one-year ratios,
  `R_pow` for `^`). Return values are bit-for-bit identical, degenerate
  NaN cases included, as checked against the previous implementation on
  577 outputs of the public functions and 2.4 million random
  [`pxt()`](https://spedygiorgio.github.io/lifecontingencies/reference/pxt.md)
  evaluations; `test-pxt-lifetable-native.R` keeps the former R code as
  reference. Typical speed-ups:
  [`pxt()`](https://spedygiorgio.github.io/lifecontingencies/reference/pxt.md)
  x12-x28,
  [`axn()`](https://spedygiorgio.github.io/lifecontingencies/reference/endowment_trio.md)/[`Axn()`](https://spedygiorgio.github.io/lifecontingencies/reference/endowment_trio.md)
  x5-x8 (x20-x26 with monthly payments), multiple-life annuities x3.5.
  The `mdt` branch of
  [`pxt()`](https://spedygiorgio.github.io/lifecontingencies/reference/pxt.md)
  is unchanged.

- Fixed `exyzt(..., type = "Tx")` for a finite term `t`: the complete
  expectation under UDD is `sum(kpxyz) + 0.5 * (1 - tpxyz)`; the
  previous code added a fixed 0.5, overstating the result unless `t`
  covered the whole lifespan (default `t = Inf` is unchanged).

- Documentation of
  [`exn()`](https://spedygiorgio.github.io/lifecontingencies/reference/exn.md),
  [`exyzt()`](https://spedygiorgio.github.io/lifecontingencies/reference/multiple-life-probabilities.md)
  and
  [`rLife()`](https://spedygiorgio.github.io/lifecontingencies/reference/rLifes.md):
  precise definitions of the curtate and complete (temporary)
  expectation of life, their UDD relation, the meaning of `n`, and the
  closed-interval treatment of omega; fixed the `Pr[K_x = t]` formula
  (denominator is `l_x`) and the description of the `ex` column of
  `print(lifetable)` (curtate) in the introductory vignette. New tests
  in `test-exn-definitions.R` and `test-published-life-expectancy.R`
  (worked examples from Finan’s MLC manual, SOA Exam MLC samples and
  Dickson-Hardy-Waters Exercise 2.1; note that Finan Ex. 23.24 prints
  e\_{80:3} = 1.64 while the correct value is 1.94).

- New regression tests against published national tables and worked
  examples: Spanish GRM-80 commutation table at 6%, German Destatis
  2002/2004 commutation numbers and annuity values (men, women, joint
  life), Australian Life Tables 1932-34 monetary table at 3%, Irish Life
  Table No. 14 personal injury multipliers (Society of Actuaries in
  Ireland).

- Removed fragile/dead links: the Travis-CI badge (`travis-ci.org`,
  discontinued free-tier domain, redundant with the existing GitHub
  Actions `R-CMD-check` badge) and the Depsy badge (`depsy.org`, defunct
  project, broken TLS certificate) in `README.md`; dropped the
  now-inaccessible `url` field (redirects to a Google sign-in page) from
  the Tim Riffe `LifeTable` bibliography entry in
  `vignettes/lifecontingenciesBiblio.bib`, keeping the citation itself.
  Also dropped the Google Books `url` fields from the
  `de2016assicurazioni` and `willekens2014multistate` bibliography
  entries in the same file, keeping the citations (ISBN retained).

- Added `tests/_snaps` and `tests/testthat/_snaps` to `.Rbuildignore` so
  a stray local empty snapshot directory no longer triggers the “Removed
  empty directory” note during `R CMD build`.

## lifecontingencies 1.6.2

- Internal performance:
  [`Axn.mdt()`](https://spedygiorgio.github.io/lifecontingencies/reference/multidecrins.md),
  `setAs("lifetable","numeric",...)`,
  `setAs("actuarialtable","numeric",...)` and
  `.lifetable_to_markovchain_list()` now call the already-vectorized
  [`pxt()`](https://spedygiorgio.github.io/lifecontingencies/reference/pxt.md)/[`qxt()`](https://spedygiorgio.github.io/lifecontingencies/reference/pxt.md)/[`Axn()`](https://spedygiorgio.github.io/lifecontingencies/reference/endowment_trio.md)
  once instead of looping over scalar calls. No change in return values
  (verified against the previous implementation on multiple test tables,
  including `mdt` and Markov-chain conversions).
- Removed the redundant `.github/workflows/ci.yml` CI workflow, which
  duplicated `R-CMD-check.yaml` but lacked TinyTeX setup and could fail
  the vignette build.

## lifecontingencies 1.6.1

- [`Exn()`](https://spedygiorgio.github.io/lifecontingencies/reference/endowment_trio.md)
  and
  [`AExn()`](https://spedygiorgio.github.io/lifecontingencies/reference/endowment_trio.md)
  are now vectorized: `x`, `n` and `i` are recycled to a common length
  and one value per element is returned (previously a vector age raised
  an error).
- New regression tests against published tables and worked examples: SOA
  Standard Ultimate Life Table, Bowers’ Illustrative Life Table and
  Finan’s Exam MLC examples.
- New dedicated vignette `multiple_decrement_tables.Rmd`: the full
  multiple-decrement workflow (the `mdt` class, decrement probabilities,
  Associated Single Decrement Tables, actuarial applications, and a
  worked, fully reproducible example building a cause-specific `mdt`
  object from NCHS’s “United States Life Tables Eliminating Certain
  Causes of Death, 1999-2001”, Table 8, including curation of a mutually
  exclusive cause-of-death partition and graduation from 5-year bands to
  single years of age via a monotone Hermite (Fritsch-Carlson) spline on
  `lx`, with an explicit note that this is not a substitute for
  PCLM-based ungrouping). The main vignette’s “Multiple Decrement
  Models” section is now a short introduction pointing to it.

## lifecontingencies 1.5.2

CRAN release: 2026-07-30

## lifecontingencies 1.5.0

- Revised creations of the object
