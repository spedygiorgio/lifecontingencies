# Accumulated value of an increasing annuity

Future (accumulated) value at time `n` of an increasing annuity with
payment stream \\1, 2, \ldots, n\\. Equivalent to capitalising
[`increasingAnnuity`](https://spedygiorgio.github.io/lifecontingencies/reference/increasingAnnuity.md)
by \\(1+i)^n\\.

## Usage

``` r
Isn(i, n, type = "immediate")
```

## Arguments

- i:

  Effective interest rate.

- n:

  Number of periods.

- type:

  `"immediate"` (default) or `"due"`.

## Value

A numeric scalar.

## References

Broverman, S.A. (2008). *Mathematics of Investment and Credit* (Fourth
Edition), ACTEX Publications.

## See also

[`accumulatedValue`](https://spedygiorgio.github.io/lifecontingencies/reference/accumulatedValue.md),
[`increasingAnnuity`](https://spedygiorgio.github.io/lifecontingencies/reference/increasingAnnuity.md)

## Author

Giorgio A. Spedicato

## Examples

``` r
Isn(n = 10, i = 0.03)
#> [1] 60.25986
```
