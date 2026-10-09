# German population life tables

Dataset containing mortality rates for German population, male and
females.

## Usage

``` r
data(demoGermany)
```

## Format

A data frame with 113 observations on the following 5 variables.

- `x`:

  Attained age

- `qxMale`:

  Male mortality rate

- `qxFemale`:

  Female mortality rate

## Details

Sterbetafel DAV 1994

## Source

Private communicatiom

## Examples

``` r
data(demoGermany)
head(demoGermany)
#>   x   qxMale qxFemale
#> 1 0 0.000113  5.9e-05
#> 2 1 0.000113  5.9e-05
#> 3 2 0.000113  5.9e-05
#> 4 3 0.000113  5.9e-05
#> 5 4 0.000113  5.9e-05
#> 6 5 0.000113  5.9e-05
```
