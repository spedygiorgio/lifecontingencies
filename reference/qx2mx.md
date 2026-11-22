# Convert death probabilities to mortality rates

Inverse of
[`mx2qx`](https://spedygiorgio.github.io/lifecontingencies/reference/mx2qx.md):
convert one-year death probabilities \\{}\_1q_x\\ into central mortality
rates \\{}\_1m_x\\, \$\${}\_1m_x = \frac{{}\_1q_x}{1 + a_x {}\_1q_x -
{}\_1q_x} .\$\$

## Usage

``` r
qx2mx(qx, ax = 0.5)
```

## Arguments

- qx:

  Numeric vector of one-year death probabilities.

- ax:

  Average fraction of a year lived by those who die in it. Default `0.5`
  (UDD).

## Value

A numeric vector of central mortality rates.

## See also

[`mxt`](https://spedygiorgio.github.io/lifecontingencies/reference/mxt.md),
[`qxt`](https://spedygiorgio.github.io/lifecontingencies/reference/pxt.md),
[`mx2qx`](https://spedygiorgio.github.io/lifecontingencies/reference/mx2qx.md)

## Examples

``` r
data(soa08Act)
soa08qx <- as(soa08Act, "numeric")
#> Warning: m argument has been recycled to match the maximum length of x, m and n
soa08mx <- qx2mx(qx = soa08qx)
soa08qx2 <- mx2qx(soa08mx)
```
