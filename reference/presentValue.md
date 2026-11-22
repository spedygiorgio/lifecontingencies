# Present value of a series of cash flows

Evaluate the present value, or the actuarial present value when
occurrence `probabilities` are supplied, of a vector of cash flows
discounted at a given set of interest rates. This is the computational
kernel on top of which the actuarial APV machinery
([`Axn`](https://spedygiorgio.github.io/lifecontingencies/reference/endowment_trio.md),
[`axn`](https://spedygiorgio.github.io/lifecontingencies/reference/endowment_trio.md),
[`Exn`](https://spedygiorgio.github.io/lifecontingencies/reference/endowment_trio.md),
[`IAxn`](https://spedygiorgio.github.io/lifecontingencies/reference/arithmetic_variation_insurances.md),
[`DAxn`](https://spedygiorgio.github.io/lifecontingencies/reference/arithmetic_variation_insurances.md),
...) is built.

## Usage

``` r
presentValue(cashFlows, timeIds, interestRates, probabilities, power = 1)
```

## Arguments

- cashFlows:

  Numeric vector of cash flows, coherent in length with `timeIds`.

- timeIds:

  Numeric vector of time points at which the `cashFlows` are due.

- interestRates:

  Either a scalar effective interest rate or a numeric vector of the
  same length as `timeIds`: entry `j` discounts the `j`-th cash flow
  back to time 0.

- probabilities:

  Optional numeric vector of occurrence probabilities, one per element
  of `cashFlows`. If missing a vector of 1's (certainty) is assumed,
  which turns the output into a plain financial present value.

- power:

  Numeric exponent applied to each discounted (and probability-weighted)
  cash flow before summation. The default `1` returns a plain APV;
  higher integer powers are used by the package to compute higher
  moments of the present value, hence the name.

## Value

A numeric scalar: the (actuarial) present value of the cash-flow series.

## Details

With `power = 1`, the function implements \$\$\mathrm{APV} = \sum_j (1 +
i_j)^{-t_j}\\ CF_j\\ p_j,\$\$ where `(t_j, CF_j, i_j, p_j)` are the
aligned entries of `timeIds`, `cashFlows`, `interestRates` and
`probabilities`. A missing `timeIds` is interpreted as the single time
\\t = 1\\ (a warning is emitted so that silent misalignments can be
caught). The numerical work is delegated to the C++ kernel
`.presentValueC()` for performance.

## Note

This simple function is the kernel on which the package's actuarial and
financial mathematics rest.

## References

Broverman, S.A. (2008). *Mathematics of Investment and Credit* (Fourth
Edition), ACTEX Publications.

## See also

[`annuity`](https://spedygiorgio.github.io/lifecontingencies/reference/annuity.md),
[`Axn`](https://spedygiorgio.github.io/lifecontingencies/reference/endowment_trio.md),
[`axn`](https://spedygiorgio.github.io/lifecontingencies/reference/endowment_trio.md)

## Author

Giorgio A. Spedicato

## Examples

``` r
# Three $10 payments at the end of years 1-3, certainty, flat 3% rate:
cf <- c(10, 10, 10)
t  <- c(1, 2, 3)
p  <- c(1, 1, 1)
presentValue(cashFlows = cf, timeIds = t, interestRates = 0.03,
             probabilities = p)
#> [1] 28.28611
```
