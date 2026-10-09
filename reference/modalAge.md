# Modal age at death of a life table

Returns the age at which the number of deaths \\d_x\\ is largest, i.e.
the Lexis modal age at death. (R's base
[`mode`](https://rdrr.io/r/base/mode.html) returns the storage type of
an object and cannot be used for this, so a dedicated function is
provided.)

## Usage

``` r
modalAge(object, startAge = NULL, interpolate = FALSE)
```

## Arguments

- object:

  A `lifetable` or `actuarialtable` object.

- startAge:

  Optional lower age bound for the search. Use it (e.g. `startAge = 10`)
  to obtain the adult modal age at death, ignoring the infant-mortality
  peak.

- interpolate:

  Logical. If `TRUE`, a continuous estimate is returned by fitting a
  parabola through the \\d_x\\ values at the modal age and its two
  neighbours (requires unit age spacing and an interior mode). Default
  `FALSE` (the integer age of maximum \\d_x\\).

## Value

The modal age at death (a numeric value).

## Details

The death counts are \\d_x = l_x - l\_{x+1}\\; the artificial mass at
the last, open age interval (all remaining survivors) is excluded from
the search.

## See also

[`median`](https://spedygiorgio.github.io/lifecontingencies/reference/median.md),
[`quantile`](https://spedygiorgio.github.io/lifecontingencies/reference/quantile.md)

## Examples

``` r
data(soa08Act)
modalAge(soa08Act)
#> [1] 81
modalAge(soa08Act, startAge = 10, interpolate = TRUE)
#> [1] 80.94644
```
