# Life insurance with arithmetic-variation benefit (increasing/decreasing, fractional claims)

This help page groups two term life insurances with \*\*arithmetic
variation\*\* of the benefit, both with optional deferment `m` and `k`
fractional claim times (claims at end of subperiods):

- **IAxn**: *increasing* arithmetic term insurance (benefit grows
  linearly with time);

- **DAxn**: *decreasing* arithmetic term insurance (benefit declines
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

  Coverage length in years. If missing, it is set to
  `getOmega(actuarialtable) - x - m`.

- i:

  Annual effective interest rate. Defaults to `actuarialtable@interest`.

- m:

  Deferment (years). Default 0.

- k:

  Fractional periods per year (\\k \ge 1\\). Default 1.

- type:

  Output type: `"EV"` (expected value, default) or `"ST"` (one
  stochastic realization via `rLifeContingencies`).

- power:

  Power applied to discounted cash flows before expectation (default 1).

## Value

A numeric value: the APV (or one simulated realization if `type="ST"`).

## Details

Let \\t_j = m + (j-1)/k\\, \\j=1,\dots,nk\\. With \*\*fractional claims
at end of subperiods\*\*, the EV implementations follow the pattern
already used in
[`Axn`](https://spedygiorgio.github.io/lifecontingencies/reference/endowment_trio.md):

**IAxn** (increasing): \$\$ \mathrm{IA}\_{\overline{n}\|}^{(k)} =
\sum\_{j=1}^{nk} \Big(\frac{j}{k}\Big)\\ v^{t_j + 1/k}\\ {}\_{t_j}p_x\\
q\_{x+t_j}^{(1/k)}, \$\$ where \\v=(1+i)^{-1}\\, computed via `pxt(...)`
and `qxt(..., t=1/k)`.

**DAxn** (decreasing) is analogous with benefit \\(n - (j-1)/k)\\; see
its subsection below.

**DAxn** (decreasing): \$\$ \mathrm{DA}\_{\overline{n}\|}^{(k)} =
\sum\_{j=1}^{nk} \Big(n - \frac{j-1}{k}\Big)\\ v^{t_j + 1/k}\\
{}\_{t_j}p_x\\ q\_{x+t_j}^{(1/k)}, \qquad t_j = m + \frac{j-1}{k}. \$\$
See “Fractional timing conventions” above for claim timing assumptions.

## `IAxn` — Increasing arithmetic term

Computes the APV of an n-year \*\*increasing\*\* term insurance on a
life aged `x`, with `k` fractional claim times and optional deferment
`m`. The benefit at the \\j\\-th subperiod equals \\j/k\\.

## Fractional timing conventions

For \*\*insurance\*\* benefits in this package, fractional claims are
assumed to occur at the \*\*end\*\* of each subperiod (i.e., at \\t_j +
1/k\\). This matches the implementation that multiplies survival to
\\t_j\\ and a fractional death probability over the next subperiod: \$\$
v^{t_j + 1/k}\\ {}\_{t_j}p_x\\ q\_{x+t_j}^{(1/k)}. \$\$ By contrast,
\*\*annuities\*\* use a payment-timing flag (`"immediate"` vs `"due"`)
which changes the evaluation times; insurance here has a fixed claim
timing at end-subperiod.

## `DAxn` — Decreasing arithmetic term

Computes the APV of an n-year \*\*decreasing\*\* term insurance on a
life aged `x`, with `k` fractional claim times and optional deferment
`m`. The benefit at the \\j\\-th subperiod equals \\n - (j-1)/k\\.

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
soa08Act <- with(soaLt, new("actuarialtable", interest=0.06, x=x, lx=Ix, name="SOA2008"))

## IAxn: increasing arithmetic term, 10 years, age 25 (legacy)
IAxn(actuarialtable = soa08Act, x = 25, n = 10)
#> [1] 0.05873735

## More examples (k>1 and deferment)
IAxn(actuarialtable = soa08Act, x = 40, n = 20, k = 12)     # monthly claims
#> [1] 0.6524548
IAxn(actuarialtable = soa08Act, x = 40, n = 15, m = 5, k = 4) # deferred 5y, quarterly
#> [1] 0.379349

## DAxn: decreasing arithmetic term, 10 years, age 25 (legacy)
DAxn(actuarialtable = soa08Act, x = 25, n = 10)
#> [1] 0.06070191
## More examples (k>1 and deferment)
DAxn(actuarialtable = soa08Act, x = 45, n = 10, k = 2)       # semiannual
#> [1] 0.2105033
DAxn(actuarialtable = soa08Act, x = 45, n = 12, m = 3, k = 12) # deferred 3y, monthly
#> [1] 0.3125503
```
