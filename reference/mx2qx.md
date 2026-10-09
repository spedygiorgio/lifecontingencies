# Mortality rates to Death probabilities

Function to convert mortality rates to probabilities of death

## Usage

``` r
mx2qx(mx, ax = 0.5)
```

## Arguments

- mx:

  mortality rates vector

- ax:

  the average number of years lived between ages x and x +1 by
  individuals who die in that interval

## Value

A vector of death probabilities

## Details

Function to convert mortality rates to probabilities of death

## See also

`mxt`, `qxt`, `qx2mx`

## Examples

``` r
#using some recursion
qx2mx(mx2qx(.2))
#> [1] 0.2
```
