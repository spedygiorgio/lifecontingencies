# Present value of an annuity-certain

Compute the present value of a stream of fixed payments separated by
equal intervals of time. Returns the perpetuity value when `n = Inf`.

## Usage

``` r
annuity(i, n, m = 0, k = 1, type = "immediate")
```

## Arguments

- i:

  Effective interest rate expressed in decimal form (e.g. `0.03` for
  3%). May also be a vector of per-period rates with the same length as
  the implied payment schedule.

- n:

  Number of payment periods. `Inf` returns the value of a perpetuity
  (either immediate or due, depending on `type`).

- m:

  Deferment period. Default `0`.

- k:

  Number of payments per year. A payment of \\1/k\\ is assumed in each
  sub-period, so that the total yearly outflow stays equal to 1.

- type:

  Payment timing. `"immediate"` (default) pays at the end of each
  period; `"due"` pays at the beginning. `"arrears"` and `"advance"` are
  the respective aliases (can be abbreviated).

## Value

A numeric scalar: the present value of the annuity.

## Details

For an annuity-immediate the first payment occurs at time \\1/k\\; for
an annuity-due the first payment occurs at time \\0\\. The present value
is obtained by delegating to
[`presentValue`](https://spedygiorgio.github.io/lifecontingencies/reference/presentValue.md)
with the appropriate payment schedule rather than closed-form formulas,
which keeps the output consistent across other APV functions that reuse
the same kernel.

## Note

The value returned derives from direct discounting of the cash flow
schedule, not from closed-form formulas such as
\\a^{(m)}\_{\overline{n}\|} = (1 - v^n)/i^{(m)}\\. When `m > 1`, the
per-period payment is \\1/m\\.

## References

Broverman, S.A. (2008). *Mathematics of Investment and Credit* (Fourth
Edition), ACTEX Publications.

## See also

[`accumulatedValue`](https://spedygiorgio.github.io/lifecontingencies/reference/accumulatedValue.md),
[`increasingAnnuity`](https://spedygiorgio.github.io/lifecontingencies/reference/increasingAnnuity.md),
[`decreasingAnnuity`](https://spedygiorgio.github.io/lifecontingencies/reference/decreasingAnnuity.md)

## Author

Giorgio A. Spedicato

## Examples

``` r
# Present value of 1 per year, paid in arrears, for 10 years at 3%:
annuity(i = 0.03, n = 10)
#> [1] 8.530203
# Same but paid monthly (1/12 at the end of each month):
annuity(i = 0.03, n = 10, k = 12)
#> [1] 8.646867
# Perpetuity due:
annuity(i = 0.05, n = Inf, type = "due")
#> [1] 21
```
