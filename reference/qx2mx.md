# Death Probabilities to Mortality Rates

Function to convert death probabilities to mortality rates

## Usage

``` r
qx2mx(qx, ax = 0.5)
```

## Arguments

- qx:

  death probabilities

- ax:

  the average number of years lived between ages x and x +1 by
  individuals who die in that interval

## Value

A vector of mortality rates

## Details

Function to convert death probabilities to mortality rates

## See also

`mxt`, `qxt`, `mx2qx`

## Examples

``` r
data(soa08Act)
soa08qx<-as(soa08Act,"numeric")
soa08mx<-qx2mx(qx=soa08qx)
soa08qx2<-mx2qx(soa08mx)
```
