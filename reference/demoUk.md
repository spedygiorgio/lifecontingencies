# UK life tables

AM and AF one year mortality rate. Series of 1992

## Usage

``` r
data(demoUk)
```

## Format

A data frame with 74 observations on the following 3 variables:

- `Age`:

  Annuitant age

- `AM92`:

  One year mortality rate (males)

- `AF92`:

  One year mortality rate (males)

## Source

Institute of Actuaries

## Details

This data set shows the one year survival rates for males and females of
the 1992 series. It has been taken from the Institute of Actuaries. The
series cannot be directly used to create a life table since neither
rates are not provided for ages below 16 nor for ages over 90. Various
approach can be used to complete the series.

## References

<https://www.actuaries.org.uk/learn-and-develop/continuous-mortality-investigation/cmi-mortality-and-morbidity-tables/92-series-tables>

## Examples

``` r
data(demoUk)
head(demoUk)
#>   Age     AM92     AF92
#> 1  17 0.000427 0.000113
#> 2  18 0.000426 0.000117
#> 3  19 0.000425 0.000121
#> 4  20 0.000425 0.000125
#> 5  21 0.000425 0.000130
#> 6  22 0.000427 0.000135
```
