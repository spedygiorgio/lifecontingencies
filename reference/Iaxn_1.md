# Increasing annuity life contingencies

This function evaluates increasing annuities

## Usage

``` r
Iaxn(actuarialtable, x, n, i, m = 0, type = "EV", power=1)
```

## Arguments

- actuarialtable:

  An `actuarialtable` object.

- x:

  The age of the insured head.

- n:

  The duration of the insurance

- i:

  The interest rate that overrides the one in the `actuarialtable`
  object.

- m:

  The deferring period.

- type:

  Yet only "EV" is implemented.

- power:

  The power of the APV. Default is 1 (mean)

## Details

This actuarial mathematics is generally exoteric. I have seen no valid
example of it.

## Value

The APV of the insurance

## References

Actuarial Mathematics (Second Edition), 1997, by Bowers, N.L., Gerber,
H.U., Hickman, J.C., Jones, D.A. and Nesbitt, C.J.

## Author

Giorgio A. Spedicato

## Note

The function is provided as is, without any guarantee regarding the
accuracy of calculation. We disclaim any liability for eventual losses
arising from direct or indirect use of this software.

## Warning

The function is provided as is, without any guarantee regarding the
accuracy of calculation. We disclaim any liability for eventual losses
arising from direct or indirect use of this software.

## See also

[`axn`](https://spedygiorgio.github.io/lifecontingencies/reference/endowment_trio.md),[`IAxn`](https://spedygiorgio.github.io/lifecontingencies/reference/arithmetic_variation_insurances.md)

## Examples

``` r
    #using SOA illustrative life tables
    data(soaLt)
    soa08Act=with(soaLt, new("actuarialtable",interest=0.06,
    x=x,lx=Ix,name="SOA2008"))
    #evaluate the value of a lifetime increasing annuity for a subject aged 80
    Iaxn(actuarialtable=soa08Act, x=80, n=10)
#> [1] 22.08823
```
