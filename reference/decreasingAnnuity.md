# Decreasing annuity-certain

Present value of a decreasing annuity with payment stream \\n, n-1,
\ldots, 1\\. Payments occur at times \\1, 2, \ldots, n\\ for an
annuity-immediate, or at \\0, 1, \ldots, n-1\\ for an annuity-due.

## Usage

``` r
decreasingAnnuity(i, n, type = "immediate")
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

A numeric scalar: the present value of the decreasing cash flow.

## Note

Internally calls
[`presentValue`](https://spedygiorgio.github.io/lifecontingencies/reference/presentValue.md).

## References

Broverman, S.A. (2008). *Mathematics of Investment and Credit* (Fourth
Edition), ACTEX Publications.

## See also

[`annuity`](https://spedygiorgio.github.io/lifecontingencies/reference/annuity.md),
[`increasingAnnuity`](https://spedygiorgio.github.io/lifecontingencies/reference/increasingAnnuity.md),
[`DAxn`](https://spedygiorgio.github.io/lifecontingencies/reference/arithmetic_variation_insurances.md)

## Author

Giorgio A. Spedicato

## Examples

``` r
# Present value of 10, 9, 8, ..., 1 payable at the end of each year
# for 10 years at 3%:
decreasingAnnuity(i = 0.03, n = 10)
#> [1] 48.99324
# Check against the direct sum:
sum((10:1) / (1 + 0.03)^(1:10))
#> [1] 48.99324
```
