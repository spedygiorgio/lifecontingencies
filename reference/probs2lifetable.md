# Life table from probabilities

This function returns a newly created lifetable object given either
survival or death (one year) probabilities)

## Usage

``` r
probs2lifetable(probs, radix = 10000, type = "px", name = "ungiven")
```

## Arguments

- probs:

  A real valued vector representing either one year survival or death
  probabilities. The last value in the vector must be either 1 or 0,
  depending if it represents death or survival probabilities
  respectively.

- radix:

  The radix of the life table.

- type:

  Character value either "px" or "qx" indicating how probabilities must
  be interpreted.

- name:

  The character value to be put in the corresponding slot of returned
  object.

## Details

The \\\omega\\ value is the length of the probs vector.

## Value

A
[`lifetable`](https://spedygiorgio.github.io/lifecontingencies/reference/lifetable-class.md)
object.

## References

Actuarial Mathematics (Second Edition), 1997, by Bowers, N.L., Gerber,
H.U., Hickman, J.C., Jones, D.A. and Nesbitt, C.J.

## Author

Giorgio A. Spedicato

## Note

This function allows to use mortality projection given by other
softwares with the lifecontingencies package.

## Warning

The function is provided as is, without any guarantee regarding the
accuracy of calculation. We disclaim any liability for eventual losses
arising from direct or indirect use of this software.

## See also

[`actuarialtable`](https://spedygiorgio.github.io/lifecontingencies/reference/actuarialtable-class.md)

## Examples

``` r
fakeSurvivalProbs=seq(0.9,0,by=-0.1)
newTable=probs2lifetable(fakeSurvivalProbs,type="px",name="fake")
head(newTable)
#>   x    lx
#> 1 0 10000
#> 2 1  9000
#> 3 2  7200
#> 4 3  5040
#> 5 4  3024
#> 6 5  1512
tail(newTable)
#>    x        lx
#> 5  4 3024.0000
#> 6  5 1512.0000
#> 7  6  604.8000
#> 8  7  181.4400
#> 9  8   36.2880
#> 10 9    3.6288
```
