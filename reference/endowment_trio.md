# Endowment, insurance, pure endowment and survival-annuity APVs

Four classical single-life life-contingency actuarial present values,
sharing a single documentation page because they take the same core
arguments and are typically used together:

- **Exn** — pure endowment: pays 1 at time `n` if alive.

- **Axn** — term / whole-life insurance: pays 1 at the end of the
  subperiod of death within `n` years (or up to the limiting age if `n`
  is missing), with fractional claim timing.

- **AExn** — `n`-year endowment insurance: `Axn + Exn`.

- **axn** — survival annuity (immediate or due), deferred by `m` years,
  with `k` payments per year.

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

  Contract length in years. If missing, `Exn` and `Axn` use
  `pmax(ceiling((getOmega(actuarialtable)+1 - x - m)*k)/k, 0)` (coverage
  up to the limiting age), while `AExn` uses
  `getOmega(actuarialtable) - x - 1`.

- i:

  Annual effective interest rate. Defaults to `actuarialtable@interest`.

- type:

  Output type: `"EV"` (expected value, default) or `"ST"` (one
  stochastic realization drawn with
  [`rLifeContingencies`](https://spedygiorgio.github.io/lifecontingencies/reference/rLifeContingencies.md)).

- power:

  Power applied to the discounted payoff before expectation (default 1).
  Used to compute higher moments of the present value.

- m:

  Deferment (years). Default 0. Vector accepted (for `Axn` and `axn`).

- k:

  Fractional periods per year (\\k \ge 1\\); must be scalar. Default 1.

- payment:

  Payment timing for annuities: `"advance"` (alias `"due"`, the default
  for `axn`) or `"arrears"` (alias `"immediate"`).

- ...:

  Extra arguments forwarded to
  [`pxt`](https://spedygiorgio.github.io/lifecontingencies/reference/pxt.md)
  /
  [`qxt`](https://spedygiorgio.github.io/lifecontingencies/reference/pxt.md)
  (e.g. a different fractional-age assumption).

## Value

A numeric value for scalar inputs, or a numeric vector for vectorized
inputs: the APV when `type = "EV"`, or one simulated realization when
`type = "ST"`.

## Details

With \\v = (1 + i)^{-1}\\:

**Exn**: pure endowment, \\E_x^n = v^n \\ {}\_n p_x\\.

**Axn**: fractional-claim term/whole-life insurance,
\$\$A\_{\overline{n}\|}^{(k)} = \sum\_{j=1}^{nk} v^{t_j + 1/k}\\
{}\_{t_j}p_x\\ q\_{x+t_j}^{(1/k)},\$\$ where \\t_j = m + (j-1)/k\\,
computed via `pxt(...)` and `qxt(..., t = 1/k)`.

**AExn**: equals `Axn(...) + Exn(...)` with aligned arguments.

**Vectorization**: `Exn` and `AExn` accept vectors for `x`, `n` and `i`;
the arguments are recycled to a common length and one value per element
is returned.

**axn** — Survival annuity (immediate/due) with deferment `m` and `k`
fractional payments. For `type = "EV"` the annuity is
\$\$a\_{\overline{n}\|}^{(k)} = \sum\_{j=1}^{nk} \frac{1}{k}\\ v^{t_j}\\
{}\_{t_j}p_x,\$\$ with payment times \\t_j\\ determined by `payment` and
`m`.

**Axn** — Life insurance (term / whole life) with fractional claim
times, vectorized in `x`, `n` and `m` (recycled to a common length); `k`
must be scalar.

**AExn** — `n`-year endowment insurance, computed as
`Axn(..., n, k) + Exn(..., n)`. Vectorised over `x`, `n` and `i`; a
zero-term endowment pays 1 immediately. When all contracts share the
same interest rate, `Axn`/`Exn` are called once with the full vector of
ages/terms; otherwise the loop runs contract by contract (`Axn` takes a
single scalar `i`).

## References

Bowers, N. L., Gerber, H. U., Hickman, J. C., Jones, D. A., Nesbitt, C.
J. (1997). *Actuarial Mathematics*, 2nd ed., SOA.

## See also

`Axn`, `AExn`, `axn`, `Exn`,
[`rLifeContingencies`](https://spedygiorgio.github.io/lifecontingencies/reference/rLifeContingencies.md)

Other life-contingency APVs:
[`arithmetic_variation_insurances`](https://spedygiorgio.github.io/lifecontingencies/reference/arithmetic_variation_insurances.md)

## Examples

``` r
## Common setup used in legacy docs
data(soaLt)
soa08Act <- with(soaLt,
  new("actuarialtable", interest = 0.06, x = x, lx = Ix, name = "SOA2008"))

## Exn (pure endowment)
Exn(soa08Act, x = 30, n = 35)
#> [1] 0.1031648

## Axn (term / whole life insurance)
# 10-year term, semiannual claims:
Axn(soa08Act, x = 50, n = 10, k = 2)
#> [1] 0.06138714
# Whole life (n inferred), monthly:
Axn(soa08Act, x = 30, k = 12)
#> [1] 0.1052722

# Several ages and terms at once:
Exn(soa08Act, x = c(30, 40, 50), n = c(35, 25, 15))
#> [1] 0.1031648 0.1884862 0.3512116

## AExn = Axn + Exn (legacy book-check)
AExn(soa08Act, x = 35, n = 30, i = 0.06)
#> [1] 0.2067226
Exn(soa08Act, x = 35, n = 30, i = 0.06) +
  Axn(soa08Act, x = 35, n = 30, i = 0.06)
#> [1] 0.2067226
AExn(soa08Act, x = c(35, 45), n = c(30, 20))
#> Warning: m argument has been recycled to match the maximum length of x, m and n
#> [1] 0.2067226 0.3448057

## axn (survival annuity, legacy example)
# Life-long annuity for age 65:
axn(soa08Act, x = 65)
#> [1] 9.896928

## axn specific legacy examples
# Immediate (arrears) vs due (advance), quarterly, 15-year term deferred 5 years:
axn(soa08Act, x = 60, n = 15, m = 5, k = 4, payment = "immediate")
#> [1] 5.584587
axn(soa08Act, x = 60, n = 15, m = 5, k = 4, payment = "due")
#> [1] 5.719212
# Vectorization over x and n:
axn(soa08Act, x = c(60, 65), n = c(10, 20), k = 12, payment = "due")
#> Warning: m argument has been recycled to match the maximum length of x, m and n
#> [1] 7.024080 9.018623
```
