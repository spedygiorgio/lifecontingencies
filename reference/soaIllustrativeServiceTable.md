# SoA illustrative service table

Bowers' book Illustrative Service Table

## Usage

``` r
data(SoAISTdata)
```

## Format

A data frame with 41 observations on the following 6 variables.

- `x`:

  Attained age

- `lx`:

  Surviving subjects ate the beginning of each age

- `death`:

  Drop outs for death cause

- `withdrawal`:

  Drop outs for withdrawal cause

- `inability`:

  Drop outs for inability cause

- `retirement`:

  Drop outs for retirement cause

## Details

It is a data frame that can be used to create a multiple decrement table

## Source

Optical recognized characters from below source with some few
adjustments

## References

Actuarial Mathematics (Second Edition), 1997, by Bowers, N.L., Gerber,
H.U., Hickman, J.C., Jones, D.A. and Nesbitt, C.J.

## Examples

``` r
data(SoAISTdata)
head(SoAISTdata)
#>    x     lx death withdrawal inability retirement
#> 1 30 100000   100      19900         0          0
#> 2 31  80000    80      14466         0          0
#> 3 32  65454    72       9858         0          0
#> 4 33  55524    61       5702         0          0
#> 5 34  49761    60       3971         0          0
#> 6 35  45730    64       2693        46          0
```
