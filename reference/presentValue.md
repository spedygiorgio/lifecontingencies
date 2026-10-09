# Present value of a series of cash flows

This function evaluates the present values of a series of cash flows,
given occurrence time. Probabilities of occurrence can also be taken
into account.

## Usage

``` r
presentValue(cashFlows, timeIds, interestRates, probabilities, power = 1)
```

## Arguments

- cashFlows:

  Numeric vector of cash flows. Must be coherent with `timeIds`.

- timeIds:

  Numeric vector of time points where `cashFlows` are due.

- interestRates:

  A single numeric interest rate or a numeric vector of the same length
  as `timeIds` used to discount cash flows.

- probabilities:

  Optional numeric vector of occurrence probabilities. If missing, a
  vector of ones (certainty) is assumed.

- power:

  Numeric power applied to discount and cash flows. Defaults to 1.

## Value

A numeric scalar representing the present value of the cash flow vector,
or the actuarial present value if `probabilities` are provided.

## Details

Present value of a series of cash flows.

Evaluate the present value (or actuarial present value when
probabilities are provided) of a vector of cash flows occurring at given
time instants and discounted by provided interest rates.

`probabilities` is optional; when omitted a sequence of 1's with the
same length as `timeIds` is assumed. Interest rate may be a fixed number
or a vector of the same size as `timeIds`. The `power` parameter is
normally unused except in special actuarial evaluations.

## Note

This simple function is the kernel working core of the package.
Actuarial and financial mathematics ground on it.

## References

Broverman, S.A., Mathematics of Investment and Credit (Fourth Edition),
2008, ACTEX Publications.

## Author

Giorgio A. Spedicato

## Examples

``` r
# simple example
cf <- c(10,10,10)    # $10 payments one per year for three years
t  <- c(1,2,3)       # years
p  <- c(1,1,1)       # payments certainty
presentValue(cashFlows = cf, timeIds = t, interestRates = 0.03, probabilities = p)
#> [1] 28.28611
```
