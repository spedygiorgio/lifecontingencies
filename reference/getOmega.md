# Function to return the terminal age of a life table.

This function returns the \\\omega\\ value of a life table object, that
is, the last attainable age within a life table.

## Usage

``` r
getOmega(object)
```

## Arguments

- object:

  A life table object.

## Value

A numeric value representing the \\\omega\\ value of a life table object

## References

Actuarial Mathematics (Second Edition), 1997, by Bowers, N.L., Gerber,
H.U., Hickman, J.C., Jones, D.A. and Nesbitt, C.J.

## Author

Giorgio A. Spedicato

## Warning

The function is provided as is, without any guarantee regarding the
accuracy of calculation. We disclaim any liability for eventual losses
arising from direct or indirect use of this software.

## See also

[`actuarialtable`](https://spedygiorgio.github.io/lifecontingencies/reference/actuarialtable-class.md)

## Examples

``` r
    #assumes SOA example life table to be load
    data(soaLt)
    soa08=with(soaLt, new("lifetable",
    x=x,lx=Ix,name="SOA2008"))
    #the last attainable age under SOA life table is
    getOmega(soa08) 
#> [1] 110
```
