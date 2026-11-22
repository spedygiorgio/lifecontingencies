# Various demographic functions on a life table

`Lxt` computes the person-years lived between ages `x` and `x + t`; `Tx`
returns the total person-years lived from age `x` to the end of the
table. They are the standard building blocks of life-table analysis and
feed into the complete expectation of life
([`exn`](https://spedygiorgio.github.io/lifecontingencies/reference/exn.md)).

## Usage

``` r
Lxt(object, x, t = 1, fxt = 0.5)

Tx(object, x, fxt = 0.5, axOmega = 1 - fxt)
```

## Arguments

- object:

  a `lifetable` or `actuarialtable` object.

- x:

  age of the subject.

- t:

  duration of the calculation (default 1).

- fxt:

  fraction of the year of age lived by those who die within the year, so
  that \\L_x = l_x - fxt\\ d_x\\. Defaults to `0.5` (uniform
  distribution of deaths).

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

A numeric scalar.

## Details

With \\d_x = l_x - l\_{x+1}\\ and \\{}\_tL_x = \sum\_{k=0}^{t-1}
(l\_{x+k} - fxt\\ d\_{x+k})\\, the function sums person-years over the
requested age window.

`Tx` is the total number of years lived from age `x` by the life-table
cohort, i.e. the sum of `Lx` from `x` to the last tabulated age
\\\omega\\.

## References

Actuarial Mathematics (Second Edition), 1997, by Bowers, N.L., Gerber,
H.U., Hickman, J.C., Jones, D.A. and Nesbitt, C.J.

## Author

Giorgio Alfredo Spedicato

## Examples

``` r
data(soaLt)
soa08Act <- with(soaLt, new("actuarialtable", interest = 0.06,
                            x = x, lx = Ix, name = "SOA2008"))
Lxt(soa08Act, 67, 10)
#> [1] 61131812
Tx(soa08Act, 67)
#> [1] 102198948
```
