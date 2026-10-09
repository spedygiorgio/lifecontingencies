# Various demographic functions

Various demographic functions

## Usage

``` r
Lxt(object, x, t = 1, fxt = 0.5)

Tx(object, x, fxt = 0.5, axOmega = 1 - fxt)
```

## Arguments

- object:

  a `lifetable` or `actuarialtable` object

- x:

  age of the subject

- t:

  duration of the calculation

- fxt:

  fraction of the year of age lived by those who die within the year (so
  that \\L_x = l_x - fxt\\ d_x\\, matching `Lxt`). Defaults to `0.5`
  (uniform distribution of deaths).

- axOmega:

  mean number of years lived in the last, open-ended age interval by
  those still alive at the last tabulated age \\\omega\\ (so that
  \\L\_\omega = axOmega \cdot l\_\omega\\). The default, `1 - fxt`,
  reproduces the historical behaviour of the function (the last interval
  is closed assuming survivors live on average half a year longer when
  `fxt = 0.5`). Published life tables that leave the last interval open
  set \\L\_\omega = l\_\omega / m\_\omega\\: pass `axOmega = 1 / mOmega`
  to reproduce them. Only relevant when \\l\_\omega \> 0\\.

## Value

A numeric value

## Details

`Tx` il the sum of years lived since age `x` by the population of the
life table, it is the sum of `Lx`. The function is provided as is,
without any warranty regarding the accuracy of calculations. Use at own
risk.

## References

Actuarial Mathematics (Second Edition), 1997, by Bowers, N.L., Gerber,
H.U., Hickman, J.C., Jones, D.A. and Nesbitt, C.J.

## Author

Giorgio Alfredo Spedicato.

## Examples

``` r
data(soaLt)
soa08Act=with(soaLt, new("actuarialtable",interest=0.06,
x=x,lx=Ix,name="SOA2008"))
Lxt(soa08Act, 67,10)
#> [1] 61131812
#assumes SOA example life table to be load
data(soaLt)
soa08Act=with(soaLt, new("actuarialtable",interest=0.06,x=x,lx=Ix,name="SOA2008"))
Tx(soa08Act, 67)
#> [1] 102198948
```
