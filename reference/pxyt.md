# Functions to evaluate joint survival probabilities.

These functions evaluate survival and death probabilities for two heads.

## Usage

``` r
exyt(objectx, objecty, x, y, t, status = "joint")

pxyt(objectx, objecty, x, y, t, status = "joint")

qxyt(objectx, objecty, x, y, t,  status = "joint")
```

## Arguments

- objectx:

  `lifetable` for life X.

- objecty:

  `lifetable` for life Y.

- x:

  Age of life X.

- y:

  Age of life Y.

- t:

  Time until survival has to be evaluated.

- status:

  Either `"joint"` for the joint-life status model or `"last"` for the
  last-survivor status model (can be abbreviated).

## Value

A numeric value representing joint survival probability.

## References

Actuarial Mathematics (Second Edition), 1997, by Bowers, N.L., Gerber,
H.U., Hickman, J.C., Jones, D.A. and Nesbitt, C.J.

## Author

Giorgio A. Spedicato, Kevin J. Owens.

## Note

These functions are used to evaluate two or more life contingencies.

## Warning

The function is provided as is, without any warranty regarding the
accuracy of calculations. The author disclaims any liability for
eventual losses arising from direct or indirect use of this software.
Also it is being Deprecated and asap removed from the package.

## See also

`exyt`

## Examples

``` r
if (FALSE) { # \dontrun{
data(soa08Act)
pxyt(soa08Act, soa08Act, 65, 70,10)
pxyt(soa08Act, soa08Act, 65, 70,10, "last")
} # }
```
