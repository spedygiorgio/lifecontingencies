# Convert an mdt to long (time, status) format for survival analysis

`mdtToLong` reshapes a multiple decrement table into an aggregated long
data set with one row per (exit time, cause) and a `count` column, ready
for competing-risks tools such as
`survival::survfit(Surv(time, status) ~ 1, weights = count)`
(Aalen-Johansen cumulative incidence) or for any analysis based on
[`survival::Surv`](https://rdrr.io/pkg/survival/man/Surv.html).

## Usage

``` r
mdtToLong(object, x, t, exitTime = c("end", "mid"), dropZero = TRUE)
```

## Arguments

- object:

  an `mdt` object.

- x:

  entry age of the cohort (default: the lowest tabulated age, 0).

- t:

  length of the follow-up in years (default: to the end of the table).
  Lives still in the table at `x + t` are right censored.

- exitTime:

  where in the year of age decrements are placed: `"end"` (default, time
  \\k+1\\ for exits in year \\k\\) or `"mid"` (time \\k + 1/2\\,
  consistent with UDD).

- dropZero:

  logical: drop rows with zero count (default `TRUE`).

## Value

A `data.frame` with columns `time` (years since age `x`), `age` (age at
exit or censoring), `status` (a factor whose first level, `"censored"`,
is followed by the decrement names, as expected by
[`survival::Surv`](https://rdrr.io/pkg/survival/man/Surv.html) for
multi-state data) and `count` (number of lives, possibly non-integer).

## Details

With `exitTime = "end"` the Aalen-Johansen estimate of the cumulative
incidence of cause \\j\\ at time \\k\\ computed on the weighted long
data coincides with `qxt(object, x, k, decrement = j)`; see the multiple
decrement vignette.

## Examples

``` r
valdezDf <- data.frame(
  x = 50:54,
  lx = c(4832555, 4821937, 4810206, 4797185, 4782737),
  heart = c(5168, 5363, 5618, 5929, 6277),
  accidents = c(1157, 1206, 1443, 1679, 2152),
  other = c(4293, 5162, 5960, 6840, 7631))
valdezMdt <- new("mdt", name = "ValdezExample", table = valdezDf)
#> Added fictional decrement below last x and completed x and lx until zero.... 
#> Completed the table at top, all decrements on first cause 
long <- mdtToLong(valdezMdt, x = 50, t = 5)
head(long)
#>   time age    status count
#> 1    1  51     heart  5168
#> 2    1  51 accidents  1157
#> 3    1  51     other  4293
#> 4    2  52     heart  5363
#> 5    2  52 accidents  1206
#> 6    2  52     other  5162
if (requireNamespace("survival", quietly = TRUE)) {
  fit <- survival::survfit(survival::Surv(time, status) ~ 1,
                           data = long, weights = count)
  summary(fit, times = 1:5)$pstate
}
#>           (s0)       heart    accidents       other
#> [1,] 0.9978028 0.001069414 0.0002394179 0.000888350
#> [2,] 0.9953753 0.002179179 0.0004889753 0.001956522
#> [3,] 0.9926809 0.003341711 0.0007875751 0.003189824
#> [4,] 0.9896912 0.004568598 0.0011350104 0.004605224
#> [5,] 0.9863679 0.005867497 0.0015803235 0.006184306
```
