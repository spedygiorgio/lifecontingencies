# Quantiles of the age-at-death distribution of a life table

S4 method for the [`quantile`](https://rdrr.io/r/stats/quantile.html)
generic: quantiles of the age at death implied by a `lifetable`,
obtained by linear interpolation on the survivorship column \\l_x\\.

## Usage

``` r
# S4 method for class 'lifetable'
quantile(x, probs = seq(0, 1, 0.25), age = min(x@x), names = TRUE, ...)
```

## Arguments

- x:

  A `lifetable` or `actuarialtable` object.

- ...:

  Further arguments (currently unused).

- probs:

  Numeric vector of probabilities in \\\[0, 1\]\\.

- age:

  Youngest age to condition on (default: the youngest tabulated age).
  Quantiles refer to the age at death given survival to `age`.

- names:

  Logical: if `TRUE` (default) the result is named with the
  probabilities, as [`quantile`](https://rdrr.io/r/stats/quantile.html)
  does.

## Value

A numeric vector of age-at-death quantiles.

## See also

[`median`](https://spedygiorgio.github.io/lifecontingencies/reference/median.md),
[`modalAge`](https://spedygiorgio.github.io/lifecontingencies/reference/modalAge.md)

## Examples

``` r
data(soa08Act)
quantile(soa08Act)
#>        0%       25%       50%       75%      100% 
#>   0.00000  65.21144  76.40541  84.53137 140.00000 
quantile(soa08Act, probs = c(0.1, 0.9), age = 65)
#>      10%      90% 
#> 69.20655 91.59828 
```
