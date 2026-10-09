# Multiple decrement insurances and annuities

`Axn.mdt` gives the actuarial present value (APV) of a term insurance on
a multiple decrement table, paying at the end of the year of decrement a
benefit that may depend on the cause of decrement. `axn.mdt` gives the
APV of an annuity payable while the insured is still in the active (no
decrement yet) state.

## Usage

``` r
Axn.mdt(object, x, n, i, decrement, benefits = 1, m = 0)

axn.mdt(object, x, n, i, m = 0, k = 1, payment = "advance")
```

## Arguments

- object:

  an `mdt` object

- x:

  policyholder's age (an integer age tabulated in `object`)

- n:

  contract duration in years. If missing, the cover runs to the end of
  the table, i.e. \\n = \omega + 1 - x - m\\.

- i:

  interest rate

- decrement:

  decrement(s) covered: one or more names (or column indices) among
  `getDecrements(object)`. If missing, every decrement is covered
  (insurance on the total decrement \\(\tau)\\).

- benefits:

  benefit amounts, one per element of `decrement` (recycled). Default 1,
  i.e. a unit benefit for every covered cause.

- m:

  deferment period in years (default 0).

- k:

  number of annuity payments per year (default 1). Survival
  probabilities at fractional durations are interpolated linearly in
  \\l^{(\tau)}\_x\\.

- payment:

  `"advance"` (or `"due"`, default) or `"arrears"` (or `"immediate"`).

## Value

A numeric vector of APVs (one per element of `x`).

## Details

With \\v = (1+i)^{-1}\\ the insurance APV is \$\$\sum\_{j \in J} b_j
\sum\_{h=0}^{n-1} v^{m+h+1}\\{}\_{m+h}p^{(\tau)}\_x\\
q^{(j)}\_{x+m+h},\$\$ which for a single cause reduces to the historical
`Axn.mdt`. The annuity APV is \\\frac1k \sum\_{h} v^{t_h}\\
{}\_{t_h}p^{(\tau)}\_x\\, with payment times \\t_h = m, m+1/k, \ldots,
m+n-1/k\\ (in advance) or \\m+1/k, \ldots, m+n\\ (in arrears). Benefit
premiums and reserves follow from the equivalence principle as
ratios/differences of the two.

## References

Finan, M. B. (2014). *A Reading of the Theory of Life Contingency
Models: A Preparation for Exam MLC/3L*, Sections 68-69.

## Examples

``` r
# Finan (2014), Example 69.1: 3-year term on (16), i = 10%
myTable <- data.frame(x = 16:18, lx = c(20000, 17600, 14520),
                      da = c(1300, 1870, 2380), doc = c(1100, 1210, 1331))
myMdt <- new("mdt", table = myTable, name = "Finan 69.1")
#> Added fictional decrement below last x and completed x and lx until zero.... 
#> Completed the table at top, all decrements on first cause 
A <- Axn.mdt(myMdt, x = 16, n = 3, i = 0.10, decrement = "doc")
a <- axn.mdt(myMdt, x = 16, n = 3, i = 0.10)
20000 * A / a   # level annual premium: 1250
#> [1] 1250

# Finan (2014), Example 68.1: benefit 1 for cause 1, 2 for cause 2
t681 <- data.frame(x = 50:51, lx = c(1200, 800),
                   d1 = c(100, 200), d2 = c(300, 300))
m681 <- new("mdt", table = t681)
#> Added fictional decrement below last x and completed x and lx until zero.... 
#> Completed the table at top, all decrements on first cause 
Axn.mdt(m681, x = 50, n = 2, i = 0.5, decrement = c("d1", "d2"),
        benefits = c(1, 2))  # 0.6852
#> [1] 0.6851852
```
