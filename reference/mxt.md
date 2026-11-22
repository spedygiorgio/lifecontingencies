# Central mortality rate

Compute the central mortality rate \\{}\_tm_x\\, i.e. deaths between
ages `x` and `x+t` divided by the person-years lived over that window.
It is the demographic rate counterpart of the (per-life) probability of
death
[`qxt`](https://spedygiorgio.github.io/lifecontingencies/reference/pxt.md).

## Usage

``` r
mxt(object, x, t)
```

## Arguments

- object:

  A `lifetable` or `actuarialtable` object.

- x:

  Subject's attained age.

- t:

  Length of the age interval over which the rate is evaluated (default
  1).

## Value

A numeric scalar: the central mortality rate between \\x\\ and \\x+t\\.

## Details

With
[`dxt`](https://spedygiorgio.github.io/lifecontingencies/reference/pxt.md)
and
[`Lxt`](https://spedygiorgio.github.io/lifecontingencies/reference/other-demographic-functions.md):
\$\${}\_tm_x = \frac{{}\_td_x}{{}\_tL_x}.\$\$ Under the uniform
distribution of deaths within each year of age, \\{}\_1m_x \approx
{}\_1q_x / (1 - 0.5 \cdot {}\_1q_x)\\.

## References

Actuarial Mathematics (Second Edition), 1997, by Bowers, N.L., Gerber,
H.U., Hickman, J.C., Jones, D.A. and Nesbitt, C.J.

## Examples

``` r
# Assumes the SOA example life table to be loaded
data(soaLt)
soa08Act <- with(soaLt,
  new("actuarialtable", interest = 0.06, x = x, lx = Ix, name = "SOA2008"))
# Compare mx and qx
mxt(soa08Act, 60, 10)
#> [1] 0.02098962
qxt(soa08Act, 60, 10)
#> [1] 0.1919766
```
