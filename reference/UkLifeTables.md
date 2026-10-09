# Uk AM AF 92 life tables

Uk AM AF life tables

## Usage

``` r
data(AF92Lt)
```

## Format

The format is: Formal class 'lifetable' \[package ".GlobalEnv"\] with 3
slots ..@ x : int \[1:111\] 0 1 2 3 4 5 6 7 8 9 ... ..@ lx : num
\[1:111\] 100000 99924 99847 99770 99692 ... ..@ name: chr "AF92"

## Details

Probabilities for earliest (under 16) and lastest ages (over 92) have
been derived using a Brass - Logit model fit on Society of Actuaries
life table.

## Source

See Uk life table.

## References

<https://www.actuaries.org.uk/learn-and-develop/continuous-mortality-investigation/cmi-mortality-and-morbidity-tables/92-series-tables>

## Examples

``` r
data(AF92Lt)
exn(AF92Lt)
#> [1] 90.10887
data(AM92Lt)
exn(AM92Lt)
#> [1] 82.06002
```
