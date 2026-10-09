# Functions to evaluate survival, death probabilities and deaths.

These functions evaluate raw survival and death probabilities between
age x and x+t

## Usage

``` r
dxt(object, x, t, decrement)
pxt(object, x, t, fractional = "linear", decrement)
qxt(object, x, t, fractional = "linear", decrement)
```

## Arguments

- object:

  A `lifetable`, `actuarialtable` or `mdt` object.

- x:

  Age of life `x`. (can be a vector for `pxt, qxt`).

- t:

  Period until which the age shall be evaluated. Default value is 1.
  (can be a vector for `pxt, qxt`).

- fractional:

  Assumptions for fractional age. One of `"linear"`, `"hyperbolic"`,
  `"constant force"` (can be abbreviated).

- decrement:

  The reason of decrement (only for `mdt` class objects). Can be either
  ordinal numbers or the names of decrements; when several are given the
  probability (or number) of leaving the table because of any of them is
  returned.

## Details

Fractional assumptions are:

- linear: linear interpolation between consecutive ages, i.e. assume
  uniform distribution.

- constant force of mortality : constant force of mortality, also known
  as exponential interpolation.

- hyperbolic: Balducci assumption, also known as harmonic interpolation.

Note that `fractional="uniform"`, `"exponential"`, `"harmonic"` or
`"Balducci"` is also authorized. See references for details.

`dxt`, `pxt` and `qxt` are S4 generics with methods for `lifetable` (and
hence `actuarialtable`) and `mdt` objects; other packages can add
methods for new table classes.

For `mdt` objects with a `decrement`, \\{}\_tq_x^{(j)} = {}\_td_x^{(j)}
/ l_x^{(\tau)}\\ and `pxt` returns its complement \\1 -
{}\_tq_x^{(j)}\\; ages must be tabulated integers and fractional `t` is
interpolated linearly (UDD) within the year.

## Value

A numeric value representing requested probability.

## References

Actuarial Mathematics (Second Edition), 1997, by Bowers, N.L., Gerber,
H.U., Hickman, J.C., Jones, D.A. and Nesbitt, C.J.

## Author

Giorgio A. Spedicato

## Note

Function `dxt` accepts also fractional value of t. Linear interpolation
is used in such case. These functions are called by many other
functions.

## Warning

The function is provided as is, without any warranty regarding the
accuracy of calculations. The author disclaims any liability for
eventual losses arising from direct or indirect use of this software.

## See also

[`exn`](https://spedygiorgio.github.io/lifecontingencies/reference/exn.md),
[`lifetable`](https://spedygiorgio.github.io/lifecontingencies/reference/lifetable-class.md)

## Examples

``` r
  #dxt example
  data(soa08Act)
  dxt(object=soa08Act, x=90, t=2)
#> [1] 3757.835
  #qxt example
  qxt(object=soa08Act, x=90, t=2)
#> [1] 0.3550183
  #pxt example
  pxt(object=soa08Act, x=90, t=2, "constant force" )
#> [1] 0.6449817
  #MDT example (Valdez)
  valdezMdt <- new("mdt", name = "ValdezExample", table = data.frame(
    x = 50:54, lx = c(4832555, 4821937, 4810206, 4797185, 4782737),
    heart = c(5168, 5363, 5618, 5929, 6277),
    accidents = c(1157, 1206, 1443, 1679, 2152),
    other = c(4293, 5162, 5960, 6840, 7631)))
#> Added fictional decrement below last x and completed x and lx until zero.... 
#> Completed the table at top, all decrements on first cause 
  qxt(valdezMdt, x = 50, t = 3, decrement = "heart")
#> [1] 0.003341711
  qxt(valdezMdt, x = 50, t = 3, decrement = c("heart", "accidents"))
#> [1] 0.004129286
  pxt(valdezMdt, x = 50, t = 3)
#> [1] 0.9926809
```
