# Class `"lifetable"`

`lifetable` objects allow to define and use life tables with the aim to
evaluate survival probabilities and mortality rates easily. Such values
represent the building blocks used to estimate life insurances actuarial
mathematics.

## Objects from the Class

Objects can be created by calls of the form `new("lifetable", ...)`. Two
vectors are needed. The age vector and the population at risk vector.

## Slots

- `x`::

  Object of class `"numeric"`, representing the sequence 0,1,\\\ldots,
  \omega\\

- `lx`::

  Object of class `"numeric"`, representing the number of lives at the
  beginning of age \\x\\. It is a non increasing sequence. The last
  element of vector x is supposed to be \> 0.

- `name`::

  Object of class `"character"`, reporting the name of the table

## Methods

- coerce:

  `signature(from = "lifetable", to = "data.frame")`: method to create a
  data - frame from a lifetable object

- coerce:

  `signature(from = "lifetable", to = "markovchainList")`: coerce method
  from `lifetable` to `markovchainList`

- coerce:

  `signature(from = "lifetable", to = "numeric")`: brings to numeric

- coerce:

  `signature(from = "data.frame", to = "lifetable")`: brings to life
  table

- getOmega:

  `signature(object = "lifetable")`: returns the maximum attainable life
  age

- plot:

  `signature(x = "lifetable")`: plot method

- head:

  `signature(x = "lifetable")`: head method

- print:

  `signature(x = "lifetable")`: method to print the survival probability
  implied in the table

- show:

  `signature(object = "lifetable")`: identical to `plot` method

- summary:

  `signature(object = "lifetable")`: it returns summary information
  about the object

## References

Actuarial Mathematics (Second Edition), 1997, by Bowers, N.L., Gerber,
H.U., Hickman, J.C., Jones, D.A. and Nesbitt, C.J.

## Author

Giorgio A. Spedicato

## Note

`t` may be missing in `pxt`, `qxt`, `ext`. It assumes value equal to 1
in such case.

## Warning

The function is provided as is, without any warranty regarding the
accuracy of calculations. The author disclaims any liability for
eventual losses arising from direct or indirect use of this software.

## See also

[`actuarialtable`](https://spedygiorgio.github.io/lifecontingencies/reference/actuarialtable-class.md)

## Examples

``` r
showClass("lifetable")
#> Class "lifetable" [package "lifecontingencies"]
#> 
#> Slots:
#>                                     
#> Name:          x        lx      name
#> Class:   numeric   numeric character
#> 
#> Known Subclasses: "actuarialtable"
data(soa08)
summary(soa08)
#> This is lifetable:  SOA Illustrative Life Table 
#>  Omega age is:  140 
#>  Expected curtated lifetime at birth is:  71.30789
#the last attainable age under SOA life table is
getOmega(soa08) 
#> [1] 140
#head and tail
data(soaLt)
tail(soaLt)
#>       x   Ix
#> 106 105 1668
#> 107 106  727
#> 108 107  292
#> 109 108  108
#> 110 109   36
#> 111 110   11
head(soaLt)
#>   x       Ix
#> 1 0 10000000
#> 2 1  9949901
#> 3 2  9899801
#> 4 3  9849702
#> 5 4  9799602
#> 6 5  9749503
```
