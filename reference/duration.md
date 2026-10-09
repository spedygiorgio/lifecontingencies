# Compute the duration or the convexity of a series of CF

Compute the duration or the convexity of a series of CF

## Usage

``` r
duration(cashFlows, timeIds, i, k = 1, macaulay = TRUE)

convexity(cashFlows, timeIds, i, k = 1)
```

## Arguments

- cashFlows:

  A vector representing the cash flows amounts.

- timeIds:

  Cash flows times

- i:

  APR interest, i.e. nominal interest rate compounded m-thly.

- k:

  Compounding frequency for the nominal interest rate.

- macaulay:

  Use the Macaulay formula

## Value

A numeric value representing either the duration or the convexity of the
cash flow series

## Details

The Macaulay duration is defined as \\\sum\limits_t^{T}
\frac{t\*CF\_{t}\left( 1 + \frac{i}{k} \right)^{ - t\*k}}{P}\\, while
\\\sum\limits\_{t}^{T} t\*\left( t + \frac{1}{k} \right) \* CF_t
\left(1 + \frac{y}{k} \right)^{ - k\*t - 2}\\

## References

Broverman, S.A., Mathematics of Investment and Credit (Fourth Edition),
2008, ACTEX Publications.

## Examples

``` r
#evaluate the duration/convexity of a coupon payment
cf=c(10,10,10,10,10,110)
t=c(1,2,3,4,5,6)
duration(cf, t, i=0.03)
#> [1] 4.984214
convexity(cf, t, i=0.03)
#> [1] 30.69613
```
