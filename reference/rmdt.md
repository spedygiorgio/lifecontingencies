# Simulate from a multiple decrement table

Simulate from a multiple decrement table

## Usage

``` r
rmdt(n = 1, object, x = 0, t = 1, t0 = "alive", include.t0 = TRUE)
```

## Arguments

- n:

  Number of simulations.

- object:

  The `mdt` object to simulate from.

- x:

  the period to simulate from.

- t:

  the period until to simulate.

- t0:

  initial status (default is "alive").

- include.t0:

  should initial status to be included (default is TRUE)?

## Value

A matrix with n columns (the length of simulation) and either t (if
initial status is not included) or t+1 rows.

## Details

The function uses `rmarkovchain` from the optional markovchain package
to simulate the chain: it stops with an informative error if that
package is not installed.

## See also

[`rLifeContingenciesXyz`](https://spedygiorgio.github.io/lifecontingencies/reference/rLifeContingencies.md),[`rLifeContingencies`](https://spedygiorgio.github.io/lifecontingencies/reference/rLifeContingencies.md)

## Author

Giorgio Alfredo Spedicato

## Examples

``` r
mdtDf<-data.frame(x=c(0,1,2,3),death=c(100,50,30,10),lapse=c(150,20,2,0))
myMdt<-new("mdt",name="example Mdt",table=mdtDf)
#> Added lx 
# rmdt() needs the optional markovchain package
if (requireNamespace("markovchain", quietly = TRUE)) {
  ciao<-rmdt(n=5,object = myMdt,x = 0,t = 4,include.t0=FALSE,t0="alive")
}
```
