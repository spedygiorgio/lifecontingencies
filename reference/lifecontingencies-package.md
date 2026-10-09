# Package to perform actuarial mathematics on life contingencies and classical financial mathematics calculations.

The lifecontingencies package performs standard financial, demographic
and actuarial mathematics calculation. The main purpose of the package
is to provide a comprehensive set of tools to perform risk assessment of
life contingent insurances.

## Details

Some functions have been powered by Rcpp code.

## Author

Giorgio Alfredo Spedicato with contributions from Reinhold Kainhofer and
Kevin J. Owens Maintainer: \<spedicato_giorgio@yahoo.it\>

## References

The lifecontingencies Package: Performing Financial and Actuarial
Mathematics Calculations in R, Giorgio Alfredo Spedicato, Journal of
Statistical Software, 2013,55 , 10, 1-36

## Note

Work in progress.

## See also

[`accumulatedValue`](https://spedygiorgio.github.io/lifecontingencies/reference/accumulatedValue.md),
[`annuity`](https://spedygiorgio.github.io/lifecontingencies/reference/annuity.md)

## Warning

This package and functions herein are provided as is, without any
guarantee regarding the accuracy of calculations. The author disclaims
any liability arising by any losses due to direct or indirect use of
this package.

## Examples

``` r

##financial mathematics example

#calculates monthly installment of a loan of 100,000, 
#interest rate 0.05

i=0.05
monthlyInt=(1+i)^(1/12)-1
Capital=100000
#Montly installment

R=1/12*Capital/annuity(i=i, n=10,k=12, type = "immediate")
R
#> [1] 1055.235
balance=numeric(10*12+1)
capitals=numeric(10*12+1)
interests=numeric(10*12+1)
balance[1]=Capital
interests[1]=0
capitals[1]=0

for(i in (2:121))  {
      balance[i]=balance[i-1]*(1+monthlyInt)-R
      interests[i]=balance[i-1]*monthlyInt
      capitals[i]=R-interests[i]
      }
loanSummary=data.frame(rate=c(0, rep(R,10*12)), 
  balance, interests, capitals)

head(loanSummary)
#>       rate   balance interests capitals
#> 1    0.000 100000.00    0.0000   0.0000
#> 2 1055.235  99352.18  407.4124 647.8230
#> 3 1055.235  98701.71  404.7731 650.4623
#> 4 1055.235  98048.60  402.1230 653.1123
#> 5 1055.235  97392.83  399.4621 655.7732
#> 6 1055.235  96734.38  396.7904 658.4449

tail(loanSummary)
#>         rate      balance interests capitals
#> 116 1055.235 5.212297e+03 25.431095 1029.804
#> 117 1055.235 4.178298e+03 21.235545 1034.000
#> 118 1055.235 3.140085e+03 17.022902 1038.212
#> 119 1055.235 2.097643e+03 12.793096 1042.442
#> 120 1055.235 1.050954e+03  8.546057 1046.689
#> 121 1055.235 4.949925e-10  4.281715 1050.954

##actuarial mathematics example

#APV of an annuity

    data(soaLt)
    soa08Act=with(soaLt, new("actuarialtable",interest=0.06,
    x=x,lx=Ix,name="SOA2008"))
    #evaluate and life-long annuity for an aged 65
    axn(soa08Act, x=65) 
#> [1] 9.896928
```
