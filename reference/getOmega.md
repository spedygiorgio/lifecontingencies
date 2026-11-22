# Last attainable age of a life or multiple-decrement table

Return the \\\omega\\ value of a life table or multiple-decrement table,
i.e. the largest age tabulated in the object. Methods are provided for
[`lifetable`](https://spedygiorgio.github.io/lifecontingencies/reference/lifetable-class.md)
(inherited by
[`actuarialtable`](https://spedygiorgio.github.io/lifecontingencies/reference/actuarialtable-class.md))
and
[`mdt`](https://spedygiorgio.github.io/lifecontingencies/reference/mdt-class.md).

## Usage

``` r
getOmega(object)

# S4 method for class 'mdt'
getOmega(object)

# S4 method for class 'lifetable'
getOmega(object)
```

## Arguments

- object:

  A life-table-like object
  ([`lifetable`](https://spedygiorgio.github.io/lifecontingencies/reference/lifetable-class.md),
  [`actuarialtable`](https://spedygiorgio.github.io/lifecontingencies/reference/actuarialtable-class.md)
  or
  [`mdt`](https://spedygiorgio.github.io/lifecontingencies/reference/mdt-class.md)).

## Value

A numeric scalar: the largest tabulated age.

## References

Actuarial Mathematics (Second Edition), 1997, by Bowers, N.L., Gerber,
H.U., Hickman, J.C., Jones, D.A. and Nesbitt, C.J.

## See also

[`lifetable`](https://spedygiorgio.github.io/lifecontingencies/reference/lifetable-class.md),
[`actuarialtable`](https://spedygiorgio.github.io/lifecontingencies/reference/actuarialtable-class.md)

## Author

Giorgio A. Spedicato

## Examples

``` r
# Assumes the SOA example life table to be loaded
data(soaLt)
soa08 <- with(soaLt, new("lifetable", x = x, lx = Ix, name = "SOA2008"))
getOmega(soa08)
#> [1] 110
```
