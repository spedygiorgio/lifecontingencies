# Society of Actuaries life table

This table has been used by the classical book Actuarial Mathematics and
by the Society of Actuaries for US professional examinations.

## Usage

``` r
data(soaLt)
```

## Format

A `data.frame` with 111 obs on the following 2 variables:

- `x`:

  a numeric vector

- `Ix`:

  a numeric vector

## Details

Early ages have been found elsewere since miss in the original data
sources; SOA did not provide population at risk data for certain spans
of age (e.g. 1-5, 6-9, 11-14 and 16-19)

## References

Actuarial Mathematics (Second Edition), 1997, by Bowers, N.L., Gerber,
H.U., Hickman, J.C., Jones, D.A. and Nesbitt, C.J.

## Examples

``` r
data(soaLt)
head(soaLt)
#>   x       Ix
#> 1 0 10000000
#> 2 1  9949901
#> 3 2  9899801
#> 4 3  9849702
#> 5 4  9799602
#> 6 5  9749503
```
