# Function to evaluate the accumulated value.

This function returns the value at time n of a series of equally spaced
payments of 1.

## Usage

``` r
accumulatedValue(i, n, m = 0, k = 1, type = "immediate")
```

## Arguments

- i:

  Effective interest rate expressed in decimal form. E.g. 0.03 means 3%.

- n:

  Number of terms of payment.

- m:

  Deferring period, whose default value is zero.

- k:

  Frequency of payment.

- type:

  The payment type. Use `"immediate"` (default) for an
  annuity-immediate, where payments are made at the end of each period,
  or `"due"` for an annuity-due, where payments are made at the
  beginning of each period. For compatibility, `"arrears"` is an alias
  for `"immediate"` and `"advance"` is an alias for `"due"` (can be
  abbreviated).

## Details

The accumulated value is the future value of the terms of an annuity.
Its mathematical expression is \\s\_{\left. {\overline {\\ n \\}}\\
\right\| } = {\left( {1 + i} \right)^n} a\_{\left. {\overline {\\ n
\\}}\\ \right\| }\\. The payment timing follows the `type` argument: an
annuity-immediate has payments at the end of each period, while an
annuity-due has payments at the beginning of each period.

## Value

A numeric value representing the calculated accumulated value.

## References

Broverman, S.A., Mathematics of Investment and Credit (Fourth Edition),
2008, ACTEX Publications.

## Author

Giorgio A. Spedicato

## Note

Accumulated values are derived from annuities by the following basic
equation \\{s\_{\left. {\overline {\\ n \\}}\\ \right\| }} = {\left(
{1 + i} \right)^n} = a\_{\left. {\overline {\\ n \\}}\\ \right\| }\\.

## Warning

The function is provided as is, without any guarantee regarding the
accuracy of calculation. We disclaim any liability for eventual losses
arising from direct or indirect use of this software.

## See also

[`annuity`](https://spedygiorgio.github.io/lifecontingencies/reference/annuity.md)
