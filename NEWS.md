# lifecontingencies 1.6.1

* `Exn()` and `AExn()` are now vectorized: `x`, `n` and `i` are recycled to a common length and one value per element is returned (previously a vector age raised an error).
* New regression tests against published tables and worked examples: SOA Standard Ultimate Life Table, Bowers' Illustrative Life Table and Finan's Exam MLC examples.
* Added a worked, fully reproducible example (vignette section P4.2) building a cause-specific `mdt` object from NCHS's "United States Life Tables Eliminating Certain Causes of Death, 1999-2001" (Table 8), including curation of a mutually exclusive cause-of-death partition and graduation from 5-year bands to single years of age via a monotone Hermite (Fritsch-Carlson) spline on `lx`, with an explicit note that this is not a substitute for PCLM-based ungrouping.

# lifecontingencies 1.5.2

# lifecontingencies 1.5.0

* Revised creations of the object
