# Convert between effective, nominal, convertible and discount rates

Convert between an effective annual rate (also called "real" rate here)
and the corresponding nominal rate convertible \\k\\-thly (also called
"convertible" rate), for either interest or discount. Convertible rates
are synonyms of nominal rates, so `convertible2Effective` and
`effective2Convertible` are thin wrappers of `nominal2Real` and
`real2Nominal` respectively, provided for naming consistency with
different textbook traditions.

## Usage

``` r
nominal2Real(i, k = 1, type = "interest")

convertible2Effective(i, k = 1, type = "interest")

real2Nominal(i, k = 1, type = "interest")

effective2Convertible(i, k = 1, type = "interest")
```

## Arguments

- i:

  The rate to be converted.

- k:

  The original / target compounding frequency (periods per year).

- type:

  `"interest"` (default) for interest rates or `"nominal"` for discount
  rates. (Historical naming retained for backward compatibility.)

## Value

A numeric scalar.

## Details

With `type = "interest"`, `nominal2Real(i, k)` returns \\(1 + i/k)^k -
1\\; with `type = "nominal"` it returns \\1 - (1 - i/k)^k\\ (the
discount analogue). `real2Nominal` inverts the respective expression.

## Note

Convertible rates are synonyms of nominal rates.

## References

Broverman, S.A. (2008). *Mathematics of Investment and Credit* (Fourth
Edition), ACTEX Publications.

## See also

`real2Nominal`

## Examples

``` r
# A nominal annual rate of 0.12 convertible monthly is equivalent to an
# effective annual rate of
nominal2Real(i = 0.12, k = 12, "interest")
#> [1] 0.126825
```
