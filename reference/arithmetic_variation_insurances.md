# Life insurance with arithmetic-variation benefit

Two term life insurances whose benefit varies arithmetically over the
coverage period, both with optional deferment `m` and `k` fractional
claim times (claims settled at the end of each sub-period):

- **IAxn** — *increasing* arithmetic term insurance (benefit grows
  linearly with time);

- **DAxn** — *decreasing* arithmetic term insurance (benefit declines
  linearly with time).

## Usage

``` r
IAxn(
  actuarialtable,
  x,
  n,
  i = actuarialtable@interest,
  m = 0,
  k = 1,
  type = "EV",
  power = 1
)

DAxn(
  actuarialtable,
  x,
  n,
  i = actuarialtable@interest,
  m = 0,
  k = 1,
  type = "EV",
  power = 1
)
```

## Arguments

- actuarialtable:

  A `lifetable` or `actuarialtable` object.

- x:

  Attained age at inception.

- n:

  Coverage length in years. If missing, defaults to
  `getOmega(actuarialtable) - x - m`.

- i:

  Annual effective interest rate. Defaults to `actuarialtable@interest`.

- m:

  Deferment (years). Default 0.

- k:

  Fractional periods per year (\\k \ge 1\\). Default 1.

- type:

  Output type: `"EV"` (expected value, default) or `"ST"` (one
  stochastic realization drawn with
  [`rLifeContingencies`](https://spedygiorgio.github.io/lifecontingencies/reference/rLifeContingencies.md)).

- power:

  Power applied to the discounted payoff before expectation (default 1).
  Used to compute higher moments.

## Value

A numeric scalar: the APV, or one simulated realization when
`type = "ST"`.

## Details

Let \\t_j = m + (j-1)/k\\ for \\j=1,\dots,nk\\ and \\v = (1+i)^{-1}\\.
With fractional claims at end-of-sub-period:

**IAxn** — increasing arithmetic term, benefit \\j/k\\ at \\t_j\\:
\$\$\mathrm{IA}\_{\overline{n}\|}^{(k)} = \sum\_{j=1}^{nk} \frac{j}{k}\\
v^{t_j + 1/k}\\ {}\_{t_j}p_x\\ q\_{x+t_j}^{(1/k)}.\$\$

**DAxn** — decreasing arithmetic term, benefit \\n - (j-1)/k\\ at
\\t_j\\: \$\$\mathrm{DA}\_{\overline{n}\|}^{(k)} = \sum\_{j=1}^{nk}
\Big(n - \frac{j-1}{k}\Big)\\ v^{t_j + 1/k}\\ {}\_{t_j}p_x\\
q\_{x+t_j}^{(1/k)}.\$\$

Both expressions call
[`pxt`](https://spedygiorgio.github.io/lifecontingencies/reference/pxt.md)
and
[`qxt`](https://spedygiorgio.github.io/lifecontingencies/reference/pxt.md)`(..., t = 1/k)`
just as
[`Axn`](https://spedygiorgio.github.io/lifecontingencies/reference/endowment_trio.md)
does for the level benefit case.

## Fractional timing conventions

For insurance benefits in this package, fractional claims are placed at
the *end* of each sub-period (at \\t_j + 1/k\\). This matches the
implementation that multiplies survival to \\t_j\\ by the fractional
death probability over the next sub-period: \$\$v^{t_j + 1/k}\\
{}\_{t_j}p_x\\ q\_{x+t_j}^{(1/k)}.\$\$ Annuities, by contrast, carry a
payment-timing flag (`"immediate"` vs `"due"`) that changes the
evaluation times; insurance here always uses end-of-sub-period timing.

## References

Bowers, N. L., Gerber, H. U., Hickman, J. C., Jones, D. A., Nesbitt, C.
J. (1997). *Actuarial Mathematics*, 2nd ed., SOA.

## See also

[`Axn`](https://spedygiorgio.github.io/lifecontingencies/reference/endowment_trio.md)
(level benefit),
[`AExn`](https://spedygiorgio.github.io/lifecontingencies/reference/endowment_trio.md),
[`Exn`](https://spedygiorgio.github.io/lifecontingencies/reference/endowment_trio.md),
[`axn`](https://spedygiorgio.github.io/lifecontingencies/reference/endowment_trio.md)

Other life-contingency APVs:
[`endowment_trio`](https://spedygiorgio.github.io/lifecontingencies/reference/endowment_trio.md)

## Examples

``` r
## Setup (legacy examples)
data(soaLt)
soa08Act <- with(soaLt,
  new("actuarialtable", interest = 0.06, x = x, lx = Ix, name = "SOA2008"))

## IAxn: increasing arithmetic term, 10 years, age 25 (legacy)
IAxn(actuarialtable = soa08Act, x = 25, n = 10)
#> [1] 0.05873735

## More examples (k > 1 and deferment)
IAxn(actuarialtable = soa08Act, x = 40, n = 20, k = 12)       # monthly
#> [1] 0.6524548
IAxn(actuarialtable = soa08Act, x = 40, n = 15, m = 5, k = 4) # deferred 5y, quarterly
#> [1] 0.379349

## DAxn: decreasing arithmetic term, 10 years, age 25 (legacy)
DAxn(actuarialtable = soa08Act, x = 25, n = 10)
#> [1] 0.06070191
## More examples (k > 1 and deferment)
DAxn(actuarialtable = soa08Act, x = 45, n = 10, k = 2)         # semiannual
#> [1] 0.2105033
DAxn(actuarialtable = soa08Act, x = 45, n = 12, m = 3, k = 12) # deferred 3y, monthly
#> [1] 0.3125503
```
