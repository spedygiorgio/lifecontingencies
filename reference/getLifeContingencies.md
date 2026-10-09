# Functions to obtain the present value of a life contingency given the time to death

It returns the present value of a life contingency, specified by its APV
symbol, known the time to death ob the sibjects

## Usage

``` r
getLifecontingencyPv(deathsTimeX, lifecontingency, object, x, t, i = object@interest, 
m = 0, k = 1, payment = "advance")
getLifecontingencyPvXyz(deathsTimeXyz, lifecontingency, tablesList, x, t, i, m = 0, 
k = 1, status = "joint", payment = "advance")
```

## Arguments

- deathsTimeX:

  Time to death

- lifecontingency:

  lifecontingency symbol

- object:

  life table(s)

- x:

  age(s) of the policyholder(s)

- t:

  term of the contract

- i:

  interest rate

- m:

  deferrement

- k:

  fractional payments

- payment:

  The Payment type, either `"advance"` for the annuity due (default) or
  `"arrears"` for the annuity immediate. Alternatively, one can use
  `"due"` or `"immediate"` respectively (can be abbreviated).

- deathsTimeXyz:

  matrix of death times from birth

- tablesList:

  list of table of the same size of num column of deathTimeXyz.

- status:

  Either `"joint"` for the joint-life status model or `"last"` for the
  last-survivor status model (can be abbreviated).

## Details

This function is a wrapper to the many internal functions that give the
PV known the age of death.

## Value

A vector or matrix of size number of rows of deathTimeXyz / deathTimeXy

## References

Actuarial Mathematics (Second Edition), 1997, by Bowers, N.L., Gerber,
H.U., Hickman, J.C., Jones, D.A. and Nesbitt, C.J.

## Author

Spedicato Giorgio

## Note

Multiple life function needs to be tested

## Warning

The function is provided as is, without any warranty regarding the
accuracy of calculations. The author disclaims any liability for
eventual losses arising from direct or indirect use of this software.

## See also

[`rLifeContingenciesXyz`](https://spedygiorgio.github.io/lifecontingencies/reference/rLifeContingencies.md),
[`rLifeContingencies`](https://spedygiorgio.github.io/lifecontingencies/reference/rLifeContingencies.md)

## Examples

``` r
#simulate the PV values for some life contingencies given some death times
data(soa08Act)
testgetLifecontingencyPvXyzAxyz<-getLifecontingencyPvXyz(deathsTimeXyz=
matrix(c(50,50,51,43,44,22,12,56,20,24,53,12),
ncol=2),
lifecontingency = "Axyz",tablesList = list(soa08Act, soa08Act), i = 0.03, t=30,x=c(40,50),
m=0, k=1,status="last")
testgetLifecontingencyPvAxn<-getLifecontingencyPv(deathsTimeX = seq(0, 110, by=1), 
lifecontingency = "Axn", object=soa08Act, 
    x=40,t=20, m=0, k=1)
```
