# Functions to switch from interest to intensity and vice versa.

There functions switch from interest to intensity and vice - versa.

## Usage

``` r
intensity2Interest(intensity)

interest2Intensity(i)
```

## Arguments

- intensity:

  Intensity rate

- i:

  interest rate

## Value

A numeric value.

## Details

Simple financial mathematics formulas are applied.

## References

Broverman, S.A., Mathematics of Investment and Credit (Fourth Edition),
2008, ACTEX Publications.

## See also

[`real2Nominal`](https://spedygiorgio.github.io/lifecontingencies/reference/nominal-real-convertible.md),
[`nominal2Real`](https://spedygiorgio.github.io/lifecontingencies/reference/nominal-real-convertible.md)

## Author

Giorgio A. Spedicato

## Examples

``` r
# a force of interest of 0.02 corresponds to an APR of 
intensity2Interest(intensity=0.02)
#> [1] 0.02020134
#an interest rate equal to 0.02 corresponds to a force of interest of of 
interest2Intensity(i=0.02)
#> [1] 0.01980263
```
