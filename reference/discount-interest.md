# Convert between interest and discount rates

Switch between an effective interest rate \\i\\ and the corresponding
effective discount rate \\d\\ using the standard identity \$\$d =
\frac{i}{1 + i}, \qquad i = \frac{d}{1 - d}.\$\$

## Usage

``` r
interest2Discount(i)

discount2Interest(d)
```

## Arguments

- i:

  Effective interest rate.

- d:

  Effective discount rate.

## Value

A numeric scalar.

## References

Broverman, S.A. (2008). *Mathematics of Investment and Credit* (Fourth
Edition), ACTEX Publications.

## See also

[`intensity2Interest`](https://spedygiorgio.github.io/lifecontingencies/reference/intensity-interest.md),
[`nominal2Real`](https://spedygiorgio.github.io/lifecontingencies/reference/nominal-real-convertible.md)

## Author

Giorgio Alfredo Spedicato

## Examples

``` r
# An effective interest rate of 4% corresponds to a discount rate of
interest2Discount(i = 0.04)
#> [1] 0.03846154
# The inverse direction:
discount2Interest(d = 0.04)
#> [1] 0.04166667
```
