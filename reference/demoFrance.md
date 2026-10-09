# French population life tables

Illustrative life tables from French population.

## Usage

``` r
data(demoFrance)
```

## Format

A data frame with 113 observations on the following 5 variables.

- `age`:

  Attained age

- `TH00_02`:

  Male 2000 life table

- `TF00_02`:

  Female 2000 life table

- `TD88_90`:

  1988 1990 life table

- `TV88_90`:

  1988 1990 life table

## Details

These tables are real French population life tables. They regard 88 - 90
and 00 - 02 experience.

## Source

Actuaris - Winter Associes

## Examples

``` r
data(demoFrance)
head(demoFrance)
#>   age TH00_02 TF00_02 TD88_90 TV88_90
#> 1   0  100000  100000  100000  100000
#> 2   1   99511   99616   99129   99352
#> 3   2   99473   99583   99057   99294
#> 4   3   99446   99562   99010   99261
#> 5   4   99424   99545   98977   99236
#> 6   5   99406   99531   98948   99214
```
