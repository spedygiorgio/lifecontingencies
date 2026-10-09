# Canada Mortality Rates for UP94 Series

UP94 life tables underlying mortality rates

## Usage

``` r
data(demoCanada)
```

## Format

A data frame with 120 observations on the following 7 variables.

- `x`:

  age

- `up94M`:

  UP 94, males

- `up94F`:

  UP 94, females

- `up942015M`:

  UP 94 projected to 2015, males

- `up942015f`:

  UP 94 projected to 2015, females

- `up942020M`:

  UP 94 projected to 2020, males

- `up942020F`:

  UP 94 projected to 2020, females

## Details

Mortality rates are provided.

## Source

Courtesy of Andrew Botros

## References

Courtesy of Andrew Botros

## Examples

``` r
data(demoCanada)
head(demoCanada)
#>   x    up94M    up94F up942015M up942015f up942020M up942020F
#> 1 0 0.000637 0.000571  0.000417  0.000374  0.000377  0.000338
#> 2 1 0.000430 0.000372  0.000281  0.000243  0.000254  0.000220
#> 3 2 0.000357 0.000278  0.000234  0.000182  0.000211  0.000164
#> 4 3 0.000278 0.000208  0.000182  0.000136  0.000164  0.000123
#> 5 4 0.000255 0.000188  0.000167  0.000123  0.000151  0.000111
#> 6 5 0.000244 0.000176  0.000160  0.000115  0.000144  0.000104
#create the up94M life table
up94MLt<-probs2lifetable(probs=demoCanada$up94M,radix=100000,"qx",name="UP94")
#create the up94M actuarial table table
up94MAct<-new("actuarialtable", lx=up94MLt@lx, x=up94MLt@x,interest=0.02)
```
