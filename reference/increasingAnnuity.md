# Increasing annuity.

This function evaluates non-stochastic increasing annuities.

## Usage

``` r
increasingAnnuity(i, n, type = "immediate")
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

An increasing annuity shows the following flow of payments:
\\1,2,\ldots,n-1,n\\. For an annuity-immediate these payments occur at
times \\1,2,\ldots,n\\; for an annuity-due they occur at times
\\0,1,\ldots,n-1\\.

## Value

The value of the annuity.

## References

Broverman, S.A., Mathematics of Investment and Credit (Fourth Edition),
2008, ACTEX Publications.

## Author

Giorgio A. Spedicato

## Note

This function calls internally `presentValue` function.

## See also

[`decreasingAnnuity`](https://spedygiorgio.github.io/lifecontingencies/reference/decreasingAnnuity.md),
[`IAxn`](https://spedygiorgio.github.io/lifecontingencies/reference/arithmetic_variation_insurances.md)

## Warning

The function is provided as is, without any guarantee regarding the
accuracy of calculation. We disclaim any liability for eventual losses
arising from direct or indirect use of this software.

## Examples

``` r
# The present value of an increasing sequence of payments payable at the end
# of each period for 10 periods is
increasingAnnuity(i = 0.03, n = 10)
#> [1] 44.83899
# Assuming a 3% interest rate
```
