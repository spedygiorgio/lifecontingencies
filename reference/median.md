# Median age at death of a life table

S4 method for the [`median`](https://rdrr.io/r/stats/median.html)
generic: the median age at death implied by a `lifetable`, i.e. the age
at which the survivorship column \\l_x\\ has fallen to half of its value
at `age` (linear interpolation on \\l_x\\).

## Usage

``` r
# S4 method for class 'lifetable'
median(x, na.rm = FALSE, ...)
```

## Arguments

- x:

  A `lifetable` or `actuarialtable` object.

- na.rm:

  Unused, kept for compatibility with the generic.

- ...:

  Optional `age` (default: the youngest tabulated age): the median is
  computed for the age at death conditional on being alive at `age`.

## Value

The median age at death (a numeric value).

## See also

[`quantile`](https://spedygiorgio.github.io/lifecontingencies/reference/quantile.md),
[`modalAge`](https://spedygiorgio.github.io/lifecontingencies/reference/modalAge.md),
[`exn`](https://spedygiorgio.github.io/lifecontingencies/reference/exn.md)

## Examples

``` r
data(soa08Act)
median(soa08Act)              # median age at death from birth
#> [1] 76.40541
median(soa08Act, age = 65)    # median age at death given survival to 65
#> [1] 80.46888
```
