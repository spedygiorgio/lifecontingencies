# Increasing annuity-certain

Present value of an increasing annuity with payment stream \\1, 2,
\ldots, n\\. Payments occur at times \\1, 2, \ldots, n\\ for an
annuity-immediate, or at \\0, 1, \ldots, n-1\\ for an annuity-due.

## Usage

``` r
increasingAnnuity(i, n, type = "immediate")
```

## Arguments

- i:

  Effective interest rate.

- n:

  Number of periods.

- type:

  Payment timing: `"immediate"` (default) or `"due"`; `"arrears"` and
  `"advance"` are the respective aliases.

## Value

A numeric scalar: the present value of the increasing cash flow.

## Note

Internally calls
[`presentValue`](https://spedygiorgio.github.io/lifecontingencies/reference/presentValue.md).

## References

Broverman, S.A. (2008). *Mathematics of Investment and Credit* (Fourth
Edition), ACTEX Publications.

## See also

[`annuity`](https://spedygiorgio.github.io/lifecontingencies/reference/annuity.md),
[`decreasingAnnuity`](https://spedygiorgio.github.io/lifecontingencies/reference/decreasingAnnuity.md),
[`IAxn`](https://spedygiorgio.github.io/lifecontingencies/reference/arithmetic_variation_insurances.md)

## Author

Giorgio A. Spedicato

## Examples

``` r
# Present value of 1, 2, 3, ..., 10 payable at the end of each year
# for 10 years at 3%:
increasingAnnuity(i = 0.03, n = 10)
#> [1] 44.83899
```
