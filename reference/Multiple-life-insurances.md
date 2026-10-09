# Multiple lifes insurances and annuities

Function to evalate the multiple lives insurances and annuities

## Usage

``` r
Axyzn(tablesList, x, n, i, m, k = 1, status = "joint", type = "EV", 
power=1)
axyzn(tablesList, x, n, i, m, k = 1, status = "joint", type = "EV", 
power=1, payment="advance")
```

## Arguments

- tablesList:

  A list whose elements are either lifetable or actuarialtable class
  objects.

- x:

  A vector of the same size of tableList that contains the initial ages.

- n:

  Lenght of the insurance.

- i:

  Interest rate

- m:

  Deferring period.

- k:

  Fractional payment frequency.

- status:

  Either `"joint"` for the joint-life status model or `"last"` for the
  last-survivor status model (can be abbreviated).

- type:

  A string, either `"EV"` for expected value of the actuarial present
  value (default) or `"ST"` for one stochastic realization of the
  underlying present value of benefits. Alternatively, one can use
  `"expected"` or `"stochastic"` respectively (can be abbreviated).

- power:

  The power of the APV. Default is 1 (mean).

- payment:

  The Payment type, either `"advance"` for the annuity due (default) or
  `"arrears"` for the annuity immediate. Alternatively, one can use
  `"due"` or `"immediate"` respectively (can be abbreviated).

## Details

In theory, these functions apply the same concept of life insurances on
one head on multiple heads.

## Value

The insurance value is returned.

## References

Broverman, S.A., Mathematics of Investment and Credit (Fourth Edition),
2008, ACTEX Publications.

## Author

Giorgio Alfredo Spedicato, Kevin J. Owens.

## Note

These functions are the more general version of
[`axyn`](https://spedygiorgio.github.io/lifecontingencies/reference/axyn.md)
and
[`Axyn`](https://spedygiorgio.github.io/lifecontingencies/reference/axyn.md).

## See also

[`axyn`](https://spedygiorgio.github.io/lifecontingencies/reference/axyn.md),[`Axyn`](https://spedygiorgio.github.io/lifecontingencies/reference/axyn.md).

## Examples

``` r
  data(soaLt)
  soa08Act=with(soaLt, new("actuarialtable",interest=0.06,
  x=x,lx=Ix,name="SOA2008"))
  #evaluate and life-long annuity for an aged 65
  listOfTables=list(soa08Act, soa08Act) 
  #Check actuarial equality
  axyzn(listOfTables,x=c(60,70),status="last")
#> [1] 12.15827
  axn(listOfTables[[1]],60)+axn(listOfTables[[2]],70)-
  axyzn(listOfTables,x=c(60,70),status="joint")  
#> [1] 12.15827
```
