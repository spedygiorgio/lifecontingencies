# Function to calculated accumulated increasing annuity future value.

This function evaluates non - stochastic increasing annuities future
values.

## Usage

``` r
Isn(i, n, type = "immediate")
```

## Arguments

- i:

  Interest rate.

- n:

  Terms.

- type:

  Either "due" for annuity due or "immediate" for annuity immediate.

## Details

It calls
[`increasingAnnuity`](https://spedygiorgio.github.io/lifecontingencies/reference/increasingAnnuity.md)
after having capitalized by \\\left( 1 + i \right)^n\\

## Value

A numeric value

## References

Broverman, S.A., Mathematics of Investment and Credit (Fourth Edition),
2008, ACTEX Publications.

## Author

Giorgio A. Spedicato

## Note

This function calls internally `increasingAnnuity` function.

## Warning

The function is provided as is, without any guarantee regarding the
accuracy of calculation. We disclaim any liability for eventual losses
arising from direct or indirect use of this software.

## See also

[`accumulatedValue`](https://spedygiorgio.github.io/lifecontingencies/reference/accumulatedValue.md)

## Examples

``` r
Isn(n=10,i=0.03)
#> [1] 60.25986
```
