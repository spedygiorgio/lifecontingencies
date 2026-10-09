# Function to evaluate decreasing annuities.

This function returns present values for decreasing annuities-certain.

## Usage

``` r
decreasingAnnuity(i, n, type = "immediate")
```

## Arguments

- i:

  A numeric value representing the interest rate.

- n:

  The number of periods.

- type:

  The payment type. Use `"immediate"` (default) for an
  annuity-immediate, where payments are made at the end of each period,
  or `"due"` for an annuity-due, where payments are made at the
  beginning of each period. For compatibility, `"arrears"` is an alias
  for `"immediate"` and `"advance"` is an alias for `"due"` (can be
  abbreviated).

## Details

A decreasing annuity has the following flows of payments: n, n-1, n-2,
..., 1, 0. For an annuity-immediate these payments occur at times
\\1,2,\ldots,n\\; for an annuity-due they occur at times
\\0,1,\ldots,n-1\\.

## Value

A numeric value reporting the present value of the decreasing cash
flows.

## References

Broverman, S.A., Mathematics of Investment and Credit (Fourth Edition),
2008, ACTEX Publications.

## Author

Giorgio A. Spedicato

## Note

This function calls `presentValue` function internally.

## See also

[`annuity`](https://spedygiorgio.github.io/lifecontingencies/reference/annuity.md),
[`increasingAnnuity`](https://spedygiorgio.github.io/lifecontingencies/reference/increasingAnnuity.md),
[`DAxn`](https://spedygiorgio.github.io/lifecontingencies/reference/arithmetic_variation_insurances.md)

## Warning

The function is provided as is, without any guarantee regarding the
accuracy of calculation. The author disclaims any liability for eventual
losses arising from direct or indirect use of this software.

## Examples

``` r
# The present value of 10, 9, 8, ..., 0 payable at the end of the period for 10 years is
decreasingAnnuity(i = 0.03, n = 10)
#> [1] 48.99324
# Assuming a 3% interest rate
sum((10:1)/(1 + .03)^(1:10))
#> [1] 48.99324
```
