# Functions to switch from nominal / effective / convertible rates

Functions to switch from nominal / effective / convertible rates

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

  The original / target compounding frequency.

- type:

  Either "interest" (default) or "nominal".

## Value

A numeric value.

## Details

`effective2Convertible` and `convertible2Effective` wrap the other two
functions.

## Note

Convertible rates are synonims of nominal rates

## References

Broverman, S.A., Mathematics of Investment and Credit (Fourth Edition),
2008, ACTEX Publications.

## See also

`real2Nominal`

## Examples

``` r
#a nominal rate of 0.12 equates an APR of
nominal2Real(i=0.12, k = 12, "interest")
#> [1] 0.126825
```
