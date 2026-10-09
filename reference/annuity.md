# Annuity

This function calculates the present value of a stream of fixed payments
separated by equal intervals of time.

## Usage

``` r
annuity(i, n, m = 0, k = 1, type = "immediate")
```

## Arguments

- i:

  Effective interest rate expressed in decimal form. E.g. 0.03 means 3%.
  It can be a vector of interest rates of the same length of periods.

- n:

  Periods for payments. If n = `infinity` then `annuity` returns the
  value of a perpetuity (either immediate or due).

- m:

  Deferring period, whose default value is zero.

- k:

  Yearly payments frequency. A payment of \\k^-1\\ is supposed to be
  performed at the end of each year.

- type:

  The payment type. Use `"immediate"` (default) for an
  annuity-immediate, where payments are made at the end of each period,
  or `"due"` for an annuity-due, where payments are made at the
  beginning of each period. For compatibility, `"arrears"` is an alias
  for `"immediate"` and `"advance"` is an alias for `"due"` (can be
  abbreviated).

## Details

For an annuity-immediate the first payment occurs at time \\1/k\\; for
an annuity-due the first payment occurs at time \\0\\. Thus, for annual
payments, an annuity-immediate has payment times \\1,2,\ldots,n\\,
whereas an annuity-due has payment times \\0,1,\ldots,n-1\\.

## Value

A numeric value representing the present value of the annuity.

## References

Broverman, S.A., Mathematics of Investment and Credit (Fourth Edition),
2008, ACTEX Publications.

## Author

Giorgio A. Spedicato

## Note

The value returned by annuity function derives from direct calculation
of the discounted cash flow and not from formulas, like \\{a^{\left( m
\right)}}\_{\left. {\overline {\\ n \\}}\\ \right\| } = \frac{{1 -
{v^n}}}{{{i^{\left( m \right)}}}}\\. When m is greater than 1, the
payment per period is assumed to be \\\frac{1}{m}\\.
