# Function to generate random future lifetimes

Function to generate random future lifetimes

## Usage

``` r
rLife(n, object, x = 0, k = 1, type = "Tx")

rLifexyz(n, tablesList, x, k = 1, type = "Tx")
```

## Arguments

- n:

  Number of variates to generate

- object:

  An object of class lifetable

- x:

  The attained age of subject x, default value is 0

- k:

  Number of periods within the year when it is possible death to happen,
  default value is 1

- type:

  Either `"Tx"` for continuous future lifetime, `"Kx"` for curtate
  future lifetime (can be abbreviated).

- tablesList:

  An list of lifetables

## Value

A numeric vector of n elements.

## Details

Under the uniform distribution of deaths assumption the complete future
lifetime is the curtate one plus an independent uniform fraction of
year, \\T_x=K_x+U\\ with \\U\sim Unif(0,1)\\, hence
\\E\[T_x\]=E\[K_x\]+0.5\\. For `type="Tx"` the function returns
\\K_x+0.5/k\\, i.e. a deterministic mid-period fraction, so only the
expected value (not the full distribution) of \\T_x\\ is reproduced.

## Note

The function is provided as is, without any warranty regarding the
accuracy of calculations. The author disclaims any liability for
eventual losses arising from direct or indirect use of this software.

## References

Actuarial Mathematics (Second Edition), 1997, by Bowers, N.L., Gerber,
H.U., Hickman, J.C., Jones, D.A. and Nesbitt, C.J.

## See also

[`lifetable`](https://spedygiorgio.github.io/lifecontingencies/reference/lifetable-class.md),
[`exn`](https://spedygiorgio.github.io/lifecontingencies/reference/exn.md)

## Examples

``` r
if (FALSE) { # \dontrun{
##get 20000 random future lifetimes for the Soa life table at birth
data(soa08Act)
lifes=rLife(n=20000,object=soa08Act, x=0, type="Tx")
# check if the expected life at birth derived from the life table is statistically equal
# to the expected value of the sample
t.test(x=lifes, mu=exn(soa08Act, x=0, type="continuous"))
} # }
if (FALSE) { # \dontrun{
#assessment of curtate expectation of future lifetime of the joint-life status
#generate a sample of lifes
data(soaLt)
soa08Act=with(soaLt, new("actuarialtable",interest=0.06,x=x,lx=Ix,name="SOA2008"))
tables=list(males=soa08Act, females=soa08Act)
xVec=c(60,65)
test=rLifexyz(n=50000, tablesList = tables,x=xVec,type="Kx")
#check first survival status
t.test(x=apply(test,1,"min"),mu=exyzt(tablesList=tables, x=xVec,status="joint"))
#check last survival status
t.test(x=apply(test,1,"max"),mu=exyzt(tablesList=tables, x=xVec,status="last"))
} # }
```
