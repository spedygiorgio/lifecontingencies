# lifecontingencies 1.6.3

* New regression tests against published national tables and worked examples: Spanish GRM-80 commutation table at 6%, German Destatis 2002/2004 commutation numbers and annuity values (men, women, joint life), Australian Life Tables 1932-34 monetary table at 3%, Irish Life Table No. 14 personal injury multipliers (Society of Actuaries in Ireland).
* Removed fragile/dead links: the Travis-CI badge (`travis-ci.org`, discontinued free-tier domain, redundant with the existing GitHub Actions `R-CMD-check` badge) and the Depsy badge (`depsy.org`, defunct project, broken TLS certificate) in `README.md`; dropped the now-inaccessible `url` field (redirects to a Google sign-in page) from the Tim Riffe `LifeTable` bibliography entry in `vignettes/lifecontingenciesBiblio.bib`, keeping the citation itself. Also dropped the Google Books `url` fields from the `de2016assicurazioni` and `willekens2014multistate` bibliography entries in the same file, keeping the citations (ISBN retained).
* Added `tests/_snaps` and `tests/testthat/_snaps` to `.Rbuildignore` so a stray local empty snapshot directory no longer triggers the "Removed empty directory" note during `R CMD build`.

# lifecontingencies 1.6.2

* Internal performance: `Axn.mdt()`, `setAs("lifetable","numeric",...)`, `setAs("actuarialtable","numeric",...)` and `.lifetable_to_markovchain_list()` now call the already-vectorized `pxt()`/`qxt()`/`Axn()` once instead of looping over scalar calls. No change in return values (verified against the previous implementation on multiple test tables, including `mdt` and Markov-chain conversions).
* Removed the redundant `.github/workflows/ci.yml` CI workflow, which duplicated `R-CMD-check.yaml` but lacked TinyTeX setup and could fail the vignette build.

# lifecontingencies 1.6.1

* `Exn()` and `AExn()` are now vectorized: `x`, `n` and `i` are recycled to a common length and one value per element is returned (previously a vector age raised an error).
* New regression tests against published tables and worked examples: SOA Standard Ultimate Life Table, Bowers' Illustrative Life Table and Finan's Exam MLC examples.
* New dedicated vignette `multiple_decrement_tables.Rmd`: the full multiple-decrement workflow (the `mdt` class, decrement probabilities, Associated Single Decrement Tables, actuarial applications, and a worked, fully reproducible example building a cause-specific `mdt` object from NCHS's "United States Life Tables Eliminating Certain Causes of Death, 1999-2001", Table 8, including curation of a mutually exclusive cause-of-death partition and graduation from 5-year bands to single years of age via a monotone Hermite (Fritsch-Carlson) spline on `lx`, with an explicit note that this is not a substitute for PCLM-based ungrouping). The main vignette's "Multiple Decrement Models" section is now a short introduction pointing to it.

# lifecontingencies 1.5.2

# lifecontingencies 1.5.0

* Revised creations of the object
