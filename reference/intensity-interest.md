# Convert between interest rate and force of interest

These functions switch between an effective interest rate \\i\\ and the
corresponding force of interest (intensity) \\\delta\\, using the
standard identities \\\delta = \log(1 + i)\\ and \\i = e^{\delta} - 1\\.

## Usage

``` r
intensity2Interest(intensity)

interest2Intensity(i)
```

## Arguments

- intensity:

  Force of interest (continuous-time intensity).

- i:

  interest rate

## Value

A numeric scalar.

## Details

Simple financial-mathematics formulas: \$\$i = e^{\delta} - 1, \qquad
\delta = \log(1 + i).\$\$

## References

Broverman, S.A. (2008). *Mathematics of Investment and Credit* (Fourth
Edition), ACTEX Publications.

## See also

[`real2Nominal`](https://spedygiorgio.github.io/lifecontingencies/reference/nominal-real-convertible.md),
[`nominal2Real`](https://spedygiorgio.github.io/lifecontingencies/reference/nominal-real-convertible.md)

## Author

Giorgio A. Spedicato

## Examples

``` r
# A force of interest of 0.02 corresponds to an effective rate of
intensity2Interest(intensity = 0.02)
#> [1] 0.02020134
#an interest rate equal to 0.02 corresponds to a force of interest of of 
interest2Intensity(i=0.02)
#> [1] 0.01980263
```
