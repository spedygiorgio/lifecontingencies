# Endowment, insurance, pure endowment, and survival annuity APVs (shared topic)

This help page groups four classical life-contingency present values:

- **Exn**: pure endowment, pays 1 at time `n` if alive.

- **Axn**: term/whole life insurance, pays 1 at death within `n` years
  (or up to limiting age if `n` is missing), with fractional claim
  timing.

- **AExn**: n-year endowment insurance, i.e. `Axn + Exn`.

- **axn**: survival annuity (immediate/due), with deferment `m` and `k`
  payments per year.

## Usage

``` r
Exn(actuarialtable, x, n, i = actuarialtable@interest, type = "EV", power = 1)

axn(
  actuarialtable,
  x,
  n,
  i = actuarialtable@interest,
  m,
  k = 1,
  type = "EV",
  power = 1,
  payment = "advance",
  ...
)

Axn(
  actuarialtable,
  x,
  n,
  i = actuarialtable@interest,
  m,
  k = 1,
  type = "EV",
  power = 1,
  ...
)

AExn(
  actuarialtable,
  x,
  n,
  i = actuarialtable@interest,
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

  Contract length in years. If missing, for `Exn` and `Axn` it is set to
  `pmax(ceiling((getOmega(actuarialtable)+1 - x - m)*k)/k, 0)`; for
  `AExn` it defaults to `getOmega(actuarialtable) - x - 1`. (See
  function-specific details below.)

- i:

  Annual effective interest rate. Defaults to `actuarialtable@interest`.

- type:

  Output type: `"EV"` (expected value, default) or `"ST"` (one
  stochastic realization via `rLifeContingencies`).

- power:

  Power of the discounted payoff before expectation (default 1).

- m:

  Deferment (years). Default 0. Vector accepted. (Axn/axn)

- k:

  Fractional periods per year (\\k \ge 1\\). Default 1. Must be scalar.
  (Axn/axn/AExn insurance leg)

- payment:

  Payment timing for annuities: `"advance"` (aka due) or `"immediate"`
  (aka arrears). (axn)

- ...:

  Extra args forwarded to mortality helpers (`pxt`, `qxt`), e.g.
  fractional assumptions. (Axn)

## Value

A numeric value (or vector for vectorized inputs): the APV in expected
value, or one simulated realization when `type="ST"`.

## Details

**Exn**: \\E_x^n = v^n \\ {}\_np_x\\ with \\v=(1+i)^{-1}\\.

**Axn**: With fractional claims, \$\$A\_{\overline{n}\|}^{(k)} =
\sum\_{j=1}^{nk} v^{t_j + 1/k}\\ {}\_{t_j}p_x\\ q\_{x+t_j}^{(1/k)},\$\$
where \\t_j = m + (j-1)/k\\, computed via `pxt(...)` and
`qxt(..., t=1/k)`.

**AExn**: returns `Axn(...) + Exn(...)` with aligned arguments.

**Vectorization**: `Exn` and `AExn` accept vectors for `x`, `n` and `i`;
the arguments are recycled to a common length and one value per element
is returned.

**axn**: Survival annuity with payment timing `"immediate"` (arrears) or
`"due"` (advance), deferment `m` and `k` payments per year (see
function-specific parameters).

**axn** — Survival annuity (immediate/due), with deferment `m` and `k`
fractional payments. For `type="EV"` the annuity is computed as
\$\$a\_{\overline{n}\|}^{(k)} = \sum\_{j=1}^{nk} \frac{1}{k}\\ v^{t_j}\\
{}\_{t_j}p_x,\$\$ where \\t_j\\ are the payment times depending on
`payment` and `m`.

**Axn** — Life insurance (term / whole life), fractional claim times.
Vectorized in `x`, `n`, `m`. `k` must be scalar.

**AExn** — n-year endowment insurance, computed as `Axn + Exn`.

## `Exn` — Pure endowment

Computes the actuarial present value (APV) of a pure endowment that pays
1 at time `n` provided survival to `x+n`.

## References

Bowers, N. L., Gerber, H. U., Hickman, J. C., Jones, D. A., Nesbitt, C.
J. (1997). *Actuarial Mathematics*, 2nd ed., SOA.

## See also

`Axn`, `AExn`, `axn`, `Exn`

Other life-contingency APVs:
[`arithmetic_variation_insurances`](https://spedygiorgio.github.io/lifecontingencies/reference/arithmetic_variation_insurances.md)

## Examples

``` r
## Common setup used in legacy docs
data(soaLt)
soa08Act <- with(soaLt, new("actuarialtable", interest=0.06, x=x, lx=Ix, name="SOA2008"))

## Exn (pure endowment)
Exn(soa08Act, x=30, n=35)
#> [1] 0.1031648

## Axn (term / whole life insurance)
# 10-year term, semiannual claims:
Axn(soa08Act, x=50, n=10, k=2)
#> [1] 0.06138714
# Whole life (n inferred), monthly:
Axn(soa08Act, x=30, k=12)
#> [1] 0.1052722

# Several ages and terms at once:
Exn(soa08Act, x=c(30,40,50), n=c(35,25,15))
#> [1] 0.1031648 0.1884862 0.3512116

## AExn = Axn + Exn  (legacy book-check)
AExn(soa08Act, x=35, n=30, i=0.06)
#> [1] 0.2067226
Exn(soa08Act, x=35, n=30, i=0.06) + Axn(soa08Act, x=35, n=30, i=0.06)
#> [1] 0.2067226
AExn(soa08Act, x=c(35,45), n=c(30,20))
#> [1] 0.2067226 0.3448057

## axn (survival annuity, legacy example)
# Life-long annuity for age 65:
axn(soa08Act, x=65)
#> [1] 9.896928

## axn specific legacy examples
# Immediate (arrears) vs due (advance), quarterly, 15-year term deferred 5 years:
axn(soa08Act, x=60, n=15, m=5, k=4, payment="immediate")
#> [1] 5.584587
axn(soa08Act, x=60, n=15, m=5, k=4, payment="due")
#> [1] 5.719212
# Vectorization over x/n:
axn(soa08Act, x=c(60,65), n=c(10,20), k=12, payment="due")
#> [1] 7.024080 9.018623
```
