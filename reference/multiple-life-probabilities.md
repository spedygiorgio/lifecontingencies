# Functions to deals with multiple life models

These functions evaluate multiple life survival probabilities, either
for joint or last life status. Arbitrary life probabilities can be
generated as well as random samples of lifes.

## Usage

``` r
exyzt(tablesList, x, t = Inf, status = "joint",  type = "Kx", ...)

pxyzt(tablesList, x, t, status = "joint", 
fractional=rep("linear", length(tablesList)), ...)

qxyzt(tablesList, x, t, status = "joint",  
fractional=rep("linear",length(tablesList)), ...)
```

## Arguments

- tablesList:

  A list whose elements are either `lifetable` or `actuarialtable` class
  objects.

- x:

  A vector of the same size of tableList that contains the initial ages.

- t:

  The duration.

- status:

  Either `"joint"` for the joint-life status model or `"last"` for the
  last-survivor status model (can be abbreviated).

- type:

  Either `"Tx"` for continuous future lifetime, `"Kx"` for curtate
  future lifetime (can be abbreviated). For `"Tx"` the complete
  expectation is evaluated under UDD, i.e. \\\sum\_{k=1}^{t} {}\_k
  p\_{xyz\ldots} + 0.5 (1 - {}\_t p\_{xyz\ldots})\\.

- fractional:

  Assumptions for fractional age. One of `"linear"`, `"hyperbolic"`,
  `"constant force"` (can be abbreviated).

- ...:

  Options to be passed to
  [`pxt`](https://spedygiorgio.github.io/lifecontingencies/reference/pxt.md).

## Details

These functions extends
[`pxyt`](https://spedygiorgio.github.io/lifecontingencies/reference/pxyt.md)
family to an arbitrary number of life contingencies.

## Value

An estimate of survival / death probability or expected lifetime, or a
matrix of ages.

## References

Broverman, S.A., Mathematics of Investment and Credit (Fourth Edition),
2008, ACTEX Publications.

## Author

Giorgio Alfredo, Spedicato

## Note

The procedure is experimental.

## See also

[`pxt`](https://spedygiorgio.github.io/lifecontingencies/reference/pxt.md),[`exn`](https://spedygiorgio.github.io/lifecontingencies/reference/exn.md)

## Examples

``` r
#assessment of curtate expectation of future lifetime of the joint-life status
#generate a sample of lifes
data(soaLt)
soa08Act=with(soaLt, new("actuarialtable",interest=0.06,x=x,lx=Ix,name="SOA2008"))
tables=list(males=soa08Act, females=soa08Act)
xVec=c(60,65)
test=rLifexyz(n=50000, tablesList = tables,x=xVec,type="Kx")
#check first survival status
t.test(x=apply(test,1,"min"),mu=exyzt(tablesList=tables, x=xVec,status="joint"))
#> 
#>  One Sample t-test
#> 
#> data:  apply(test, 1, "min")
#> t = 0.60546, df = 49999, p-value = 0.5449
#> alternative hypothesis: true mean is not equal to 11.55512
#> 95 percent confidence interval:
#>  11.51222 11.63638
#> sample estimates:
#> mean of x 
#>   11.5743 
#> 
#check last survival status
t.test(x=apply(test,1,"max"),mu=exyzt(tablesList=tables, x=xVec,status="last"))
#> 
#>  One Sample t-test
#> 
#> data:  apply(test, 1, "max")
#> t = 0.67462, df = 49999, p-value = 0.4999
#> alternative hypothesis: true mean is not equal to 22.06004
#> 95 percent confidence interval:
#>  22.01725 22.14775
#> sample estimates:
#> mean of x 
#>   22.0825 
#> 
```
