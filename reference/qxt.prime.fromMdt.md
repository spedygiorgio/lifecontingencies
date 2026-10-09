# Return Associated single decrement from absolute rate of decrement

Return Associated single decrement from absolute rate of decrement

## Usage

``` r
qxt.prime.fromMdt(object, x, t = 1, decrement)

qxt.fromQxprime(qx.prime, other.qx.prime, t = 1)
```

## Arguments

- object:

  a mdj object

- x:

  age

- t:

  period (default 1)

- decrement:

  type (necessary)

- qx.prime:

  single ASDT decrement of which corresponding decrement is desired

- other.qx.prime:

  ASDT decrements other than `qx.prime`

## Value

a single value (AST)

## Functions

- `qxt.fromQxprime()`: Obtain decrement from single decrements

## Examples

``` r
#Creating the valdez mdf

valdezDf<-data.frame(
x=c(50:54),
lx=c(4832555,4821937,4810206,4797185,4782737),
hearth=c(5168, 5363, 5618, 5929, 6277),
accidents=c(1157, 1206, 1443, 1679,2152),
other=c(4293,5162,5960,6840,7631))
valdezMdt<-new("mdt",name="ValdezExample",table=valdezDf) 
#> Added fictional decrement below last x and completed x and lx until zero.... 
#> Completed the table at top, all decrements on first cause 

qxt.prime.fromMdt(object=valdezMdt,x=53,decrement="other")
#> [1] 0.001426969

#Finan example 67.2

qxt.fromQxprime(qx.prime = 0.01,other.qx.prime = c(0.03,0.06))
#> [1] 0.009556
```
