# Class `"actuarialtable"`

Objects of class `"actuarialtable"` inherit the structure of class
`"lifetable"` adding just the slot for interest rate, `interest`.

## Objects from the Class

Objects can be created by calls of the form
`new("actuarialtable", ...)`. Creation is the same as lifetable objects
creation, the slot for interest must be added too.

## Slots

- `interest`::

  Object of class `"numeric"` slot for interest rate, e.g. 0.03

- `x`::

  Object of class `"numeric"` age slot

- `lx`::

  Object of class `"numeric"` subjects at risk at age x

- `name`::

  Object of class `"character"` name of the actuarial table

## Extends

Class
`"`[`lifetable`](https://spedygiorgio.github.io/lifecontingencies/reference/lifetable-class.md)`"`,
directly.

## Methods

- coerce:

  `signature(from = "actuarialtable", to = "data.frame")`: moves from
  `actuarialtable` to `data.frame`

- coerce:

  `signature(from = "actuarialtable", to = "numeric")`: coerce from
  `actuarialtable` to a numeric

- getOmega:

  `signature(object = "actuarialtable")`: as for `lifetable`

- print:

  `signature(x = "actuarialtable")`: tabulates the actuarial commutation
  functions

- show:

  `signature(object = "actuarialtable")`: show method

- summary:

  `signature(object = "actuarialtable")`: prints brief summary

## References

Actuarial Mathematics (Second Edition), 1997, by Bowers, N.L., Gerber,
H.U., Hickman, J.C., Jones, D.A. and Nesbitt, C.J.

## Author

Giorgio A. Spedicato

## Note

The interest slot will handle time-varying interest rates in the future.

## Warning

The function is provided as is, without any warranty regarding the
accuracy of calculations. The author disclaims any liability for
eventual losses arising from direct or indirect use of this software.

## See also

[`axn`](https://spedygiorgio.github.io/lifecontingencies/reference/endowment_trio.md),[`lifetable`](https://spedygiorgio.github.io/lifecontingencies/reference/lifetable-class.md)

## Examples

``` r
showClass("actuarialtable")
#> Class "actuarialtable" [package "lifecontingencies"]
#> 
#> Slots:
#>                                               
#> Name:   interest         x        lx      name
#> Class:   numeric   numeric   numeric character
#> 
#> Extends: "lifetable"
```
