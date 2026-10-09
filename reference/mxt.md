# Central mortality rate

This function returns the central mortality rate demographic function.

## Usage

``` r
mxt(object, x, t)
```

## Arguments

- object:

  a `lifetable` or `actuarialtable` object

- x:

  subject's age

- t:

  period on which the rate is evaluated

## Value

A numeric value representing the central mortality rate between age
\\x\\ and \\x+t\\.

## References

Actuarial Mathematics (Second Edition), 1997, by Bowers, N.L., Gerber,
H.U., Hickman, J.C., Jones, D.A. and Nesbitt, C.J.

## Examples

``` r
#assumes SOA example life table to be load
data(soaLt)
soa08Act=with(soaLt, new("actuarialtable",interest=0.06,x=x,lx=Ix,name="SOA2008"))
#compare mx and qx 
mxt(soa08Act, 60,10)
#> [1] 0.02098962
qxt(soa08Act, 60,10)
#> [1] 0.1919766
```
