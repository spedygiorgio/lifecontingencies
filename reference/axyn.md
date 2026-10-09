# Functions to evaluate life insurance and annuities on two heads.

These functions evaluates life insurances and annuities on two heads.

## Usage

``` r
axyn(tablex, tabley, x, y, n, i, m, k = 1, status = "joint", type = "EV", 
payment="advance")
Axyn(tablex, x, tabley, y, n, i, m, k = 1, status = "joint", type = "EV")
```

## Arguments

- tablex:

  Life X `lifetable` object.

- tabley:

  Life Y `lifetable` object.

- x:

  Age of life X.

- y:

  Age of life Y.

- n:

  Insured duration. Infinity if missing.

- i:

  Interest rate. Default value is those implied in `actuarialtable`.

- m:

  Deferring period. Default value is zero.

- k:

  Fractional payments or periods where insurance is payable.

- status:

  Either `"joint"` for the joint-life status model or `"last"` for the
  last-survivor status model (can be abbreviated).

- type:

  A string, either `"EV"` for expected value of the actuarial present
  value (default) or `"ST"` for one stochastic realization of the
  underlying present value of benefits. Alternatively, one can use
  `"expected"` or `"stochastic"` respectively (can be abbreviated).

- payment:

  The Payment type, either `"advance"` for the annuity due (default) or
  `"arrears"` for the annuity immediate. Alternatively, one can use
  `"due"` or `"immediate"` respectively (can be abbreviated).

## Details

Actuarial mathematics book formulas has been implemented.

## Value

A numeric value returning APV of chosen insurance form.

## References

Actuarial Mathematics (Second Edition), 1997, by Bowers, N.L., Gerber,
H.U., Hickman, J.C., Jones, D.A. and Nesbitt, C.J.

## Author

Giorgio A. Spedicato

## Note

Deprecated functions. Use `Axyzn` and `axyzn` instead.

## Warning

The function is provided as is, without any warranty regarding the
accuracy of calculations. The author disclaims any liability for
eventual losses arising from direct or indirect use of this software.

## See also

[`pxyt`](https://spedygiorgio.github.io/lifecontingencies/reference/pxyt.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  data(soa08Act)
  #last survival status annuity
  axyn(tablex=soa08Act, tabley=soa08Act, x=65, y=70, 
    n=5,  status = "last",type = "EV")
    #first survival status annuity
  Axyn(tablex=soa08Act, tabley=soa08Act, x=65, y=70,
  status = "last",type = "EV")
  } # }
```
