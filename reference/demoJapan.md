# Japan Mortality Rates for life table construction

Two yearly mortality rates for each age

## Usage

``` r
data(demoJapan)
```

## Format

A data frame with 110 observations on the following 3 variables.

- `JP8587M`:

  Male life table

- `JP8587F`:

  Female life table

- `age`:

  Attained age

## Source

SOA mortality web site

## Details

Dowloaded in 2012 from Society of Actuaries (SOA) mortality table web
site

## Examples

``` r
data(demoJapan)
head(demoJapan)
#>   age JP8587M JP8587F
#> 1   0 0.00137 0.00126
#> 2   1 0.00098 0.00094
#> 3   2 0.00067 0.00065
#> 4   3 0.00048 0.00044
#> 5   4 0.00039 0.00030
#> 6   5 0.00036 0.00023
```
