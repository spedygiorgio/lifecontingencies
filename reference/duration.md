# Duration and convexity of a series of cash flows

`duration` returns the Macaulay (or modified) duration of a stream of
cash flows; `convexity` returns its convexity. Both are the classical
first- and second-order sensitivity measures of present value to a small
parallel shift of the interest rate.

## Usage

``` r
duration(cashFlows, timeIds, i, k = 1, macaulay = TRUE)

convexity(cashFlows, timeIds, i, k = 1)
```

## Arguments

- cashFlows:

  A numeric vector of cash-flow amounts.

- timeIds:

  Numeric vector of payment times, same length as `cashFlows`.

- i:

  Nominal (APR) interest rate convertible `k`-thly. Pass the effective
  annual rate when `k = 1`.

- k:

  Compounding frequency for the nominal interest rate (periods per
  year); default `1`.

- macaulay:

  Logical. `TRUE` (default) returns the Macaulay duration; `FALSE`
  returns the modified duration \\D^{mod} = D / (1 + i/k)\\.

## Value

A numeric scalar: the (Macaulay or modified) duration, or the convexity,
of the cash-flow series.

## Details

With \\v = (1 + i/k)^{-1}\\ and \\t'\_j = t_j k\\, the Macaulay duration
is \$\$D = \frac{\sum_j t_j\\ CF_j\\ v^{t'\_j}}{\sum_j CF_j\\ v^{t'\_j}}
,\$\$ and the convexity is \$\$C = \frac{\sum_j t_j (t_j + 1/k)\\ CF_j\\
v^{t'\_j}} {\sum_j CF_j\\ v^{t'\_j}} (1 + i/k)^{-2} .\$\$

## References

Broverman, S.A. (2008). *Mathematics of Investment and Credit* (Fourth
Edition), ACTEX Publications.

## Examples

``` r
# Duration and convexity of a coupon bond (10 per year, 100 at maturity)
cf <- c(10, 10, 10, 10, 10, 110)
t  <- c(1, 2, 3, 4, 5, 6)
duration(cf, t, i = 0.03)
#> [1] 4.984214
convexity(cf, t, i = 0.03)
#> [1] 30.69613
```
