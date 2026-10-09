# Functions to switch from interest to discount rates

These functions switch from interest to discount rates and vice - versa

## Usage

``` r
interest2Discount(i)

discount2Interest(d)
```

## Arguments

- i:

  Interest rate

- d:

  Discount rate

## Details

The following formula (and its inverse) rules the relationships:
\$\$\frac{i}{{1 + i}} = d\$\$

## Value

A numeric value

## References

Broverman, S.A., Mathematics of Investment and Credit (Fourth Edition),
2008, ACTEX Publications.

## Author

Giorgio Alfredo Spedicato

## See also

[`intensity2Interest`](https://spedygiorgio.github.io/lifecontingencies/reference/intensity-interest.md),[`nominal2Real`](https://spedygiorgio.github.io/lifecontingencies/reference/nominal-real-convertible.md)

## Examples

``` r
discount2Interest(d=0.04)
#> [1] 0.04166667
```
