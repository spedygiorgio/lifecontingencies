# Life table from a vector of probabilities

Create a
[`lifetable`](https://spedygiorgio.github.io/lifecontingencies/reference/lifetable-class.md)
object from a vector of one-year survival or death probabilities,
starting at age 0. Useful for feeding mortality projections produced by
other software (e.g. StMoMo) into lifecontingencies.

## Usage

``` r
probs2lifetable(probs, radix = 10000, type = "px", name = "ungiven")
```

## Arguments

- probs:

  Numeric vector of one-year survival or death probabilities, starting
  at age 0. Its last element must be `0` when `type` is `"px"` (nobody
  survives beyond \\\omega\\) or `1` when `type` is `"qx"` (everyone
  alive at \\\omega\\ dies within the last year); otherwise the function
  appends the missing terminal value and warns via
  [`cat()`](https://rdrr.io/r/base/cat.html).

- radix:

  Initial cohort size \\l_0\\. Default `10000`.

- type:

  Either `"px"` (survival) or `"qx"` (death) probabilities.

- name:

  Character string stored in the `@name` slot of the returned object.

## Value

A
[`lifetable`](https://spedygiorgio.github.io/lifecontingencies/reference/lifetable-class.md)
object.

## Details

The limiting age \\\omega\\ is the length of `probs`. The survival
factor at age \\k\\ is `probs[k]` (if `type = "px"`) or `1 - probs[k]`
(if `type = "qx"`), and \\l_x\\ is built as the cumulative product of
these factors times `radix`.

## References

Actuarial Mathematics (Second Edition), 1997, by Bowers, N.L., Gerber,
H.U., Hickman, J.C., Jones, D.A. and Nesbitt, C.J.

## See also

[`lifetable`](https://spedygiorgio.github.io/lifecontingencies/reference/lifetable-class.md),
[`actuarialtable`](https://spedygiorgio.github.io/lifecontingencies/reference/actuarialtable-class.md)

## Author

Giorgio A. Spedicato

## Examples

``` r
fakeSurvivalProbs <- seq(0.9, 0, by = -0.1)
newTable <- probs2lifetable(fakeSurvivalProbs, type = "px", name = "fake")
head(newTable)
#>   x    lx
#> 1 0 10000
#> 2 1  9000
#> 3 2  7200
#> 4 3  5040
#> 5 4  3024
#> 6 5  1512
tail(newTable)
#>    x        lx
#> 5  4 3024.0000
#> 6  5 1512.0000
#> 7  6  604.8000
#> 8  7  181.4400
#> 9  8   36.2880
#> 10 9    3.6288
```
