# Convert mortality rates to death probabilities

Convert a vector of central mortality rates \\{}\_1m_x\\ into one-year
death probabilities \\{}\_1q_x\\ using the standard identity
\$\${}\_1q_x = \frac{{}\_1m_x}{1 + (1 - a_x) {}\_1m_x} ,\$\$ where
\\a_x\\ is the average fraction of year lived by those who die between
ages \\x\\ and \\x+1\\. The default `ax = 0.5` corresponds to the
uniform distribution of deaths within the year.

## Usage

``` r
mx2qx(mx, ax = 0.5)
```

## Arguments

- mx:

  Numeric vector of central mortality rates.

- ax:

  Average fraction of a year lived by those who die in it. Default `0.5`
  (UDD).

## Value

A numeric vector of one-year death probabilities.

## See also

[`mxt`](https://spedygiorgio.github.io/lifecontingencies/reference/mxt.md),
[`qxt`](https://spedygiorgio.github.io/lifecontingencies/reference/pxt.md),
[`qx2mx`](https://spedygiorgio.github.io/lifecontingencies/reference/qx2mx.md)

## Examples

``` r
# Round-trip: qx2mx should invert mx2qx
qx2mx(mx2qx(0.2))
#> [1] 0.2
```
