# Accumulated value of an annuity-certain

Future value at time `n` of a stream of equally spaced unit payments.
With the standard identity \\s\_{\overline{n}\|} = (1 + i)^n\\
a\_{\overline{n}\|}\\, this is a thin wrapper around
[`annuity`](https://spedygiorgio.github.io/lifecontingencies/reference/annuity.md).

## Usage

``` r
accumulatedValue(i, n, m = 0, k = 1, type = "immediate")
```

## Arguments

- i:

  Effective interest rate.

- n:

  Number of payment periods.

- m:

  Deferment period. Default `0`.

- k:

  Number of payments per year. Default `1`.

- type:

  Payment timing: `"immediate"` (default) or `"due"`; `"arrears"` and
  `"advance"` are the respective aliases.

## Value

A numeric scalar: the accumulated value.

## References

Broverman, S.A. (2008). *Mathematics of Investment and Credit* (Fourth
Edition), ACTEX Publications.

## See also

[`annuity`](https://spedygiorgio.github.io/lifecontingencies/reference/annuity.md)

## Author

Giorgio A. Spedicato

## Examples

``` r
# Future value of 1 per year, paid in arrears, for 10 years at 3%:
accumulatedValue(i = 0.03, n = 10)
#> [1] 11.46388
```
