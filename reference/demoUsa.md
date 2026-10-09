# United States Social Security life tables

This data set contains period life tables for years 1990, 2000 and 2007.
Both males and females life tables are reported.

## Usage

``` r
demoUsa
```

## Format

A `data.frame` containing people surviving at the beginning of "age" at
2007, 2000, and 1990 split by gender

## Source

See <https://www.ssa.gov/oact/NOTES/as120/LifeTables_Body.html>

## Details

Reported age is truncated at the last age with lx\>0.

## Examples

``` r
data(demoUsa)
head(demoUsa)
#>   age USSS2007M USSS2007F USSS2000M USSS2000F USSS1990M USSS1990F
#> 1   0    100000    100000    100000    100000    100000    100000
#> 2   1     99262     99390     99241     99377     98972     99185
#> 3   2     99213     99347     99187     99332     98896     99120
#> 4   3     99182     99322     99150     99302     98844     99083
#> 5   4     99158     99303     99122     99283     98804     99053
#> 6   5     99138     99288     99100     99264     98771     99028
```
