# Function to generate samples from the life contingencies stochastic variables

Function to generate samples from the life contingencies stochastic
variables

## Usage

``` r
rLifeContingencies(
  n,
  lifecontingency,
  object,
  x,
  t,
  i = object@interest,
  m = 0,
  k = 1,
  parallel = FALSE,
  nthreads = NULL,
  payment = "advance"
)

rLifeContingenciesXyz(
  n,
  lifecontingency,
  tablesList,
  x,
  t,
  i,
  m = 0,
  k = 1,
  status = "joint",
  parallel = FALSE,
  nthreads = NULL,
  payment = "advance"
)
```

## Arguments

- n:

  Size of sample

- lifecontingency:

  A character string, either `"Exn"`, `"Axn"`, `"axn"`, `"IAxn"` or
  `"DAxn"`

- object:

  An `actuarialtable` object.

- x:

  Policyholder's age at issue time; for `rLifeContingenciesXyz` a
  numeric vector of the same length of `object`, containing the
  policyholders' ages

- t:

  The lenght of the insurance. Must be specified according to the
  present value of benefits definition.

- i:

  The interest rate, whose default value is the `actuarialtable`
  interest rate slot value.

- m:

  Deferring period, default value is zero.

- k:

  Fractional payment, default value is 1.

- parallel:

  Request multi-threaded (OpenMP) computation of the per-sample payoff.
  This is opt-in twice: it has an effect only if `parallel = TRUE` *and*
  the option `options(lifecontingencies.openmp = TRUE)` has been set by
  the user; otherwise the (already vectorised) sequential kernel is
  used. It is also a no-op when the installed binary was built without
  OpenMP support (see `lifecontingencies:::.hasOpenMP()`). Simulated
  lifetimes are drawn in R before the parallel region, so results are
  identical with and without parallelisation for a given seed.

- nthreads:

  Number of OpenMP threads used when parallelisation is active. Defaults
  to `NULL`, which falls back to
  `getOption("lifecontingencies.nthreads", 2L)`; it is never set
  automatically to the number of available cores.

- payment:

  The Payment type, either `"advance"` for the annuity due (default) or
  `"arrears"` for the annuity immediate. Alternatively, one can use
  `"due"` or `"immediate"` respectively (can be abbreviated).

- tablesList:

  A list of `actuarialtable` objects

- status:

  Either `"joint"` for the joint-life status model or `"last"` for the
  last-survivor status model (can be abbreviated).

## Value

A numeric vector

## Examples

``` r
if (FALSE) { # \dontrun{
  #assumes SOA example life table to be load
  data(soaLt)
  soa08Act=with(soaLt, new("actuarialtable",interest=0.06, x=x,lx=Ix,name="SOA2008"))
  out<-rLifeContingencies(n=1000, lifecontingency="Axn",object=soa08Act, x=40,
  t=getOmega(soa08Act)-40, m=0)
  APV=Axn(soa08Act,x=40)
  #check if out distribution is unbiased
  t.test(x=out, mu=APV)$p.value>0.05
} # }
if (FALSE) { # \dontrun{
data(soa08Act)
n=10000
lifecontingency="Axyz"
tablesList=list(soa08Act,soa08Act)
x=c(60,60); i=0.06; m=0; status="joint"; t=30; k=1
APV=Axyzn(tablesList=tablesList,x=x,n=t,m=m,k=k,status=status,type="EV")
samples<-rLifeContingenciesXyz(n=n,lifecontingency = lifecontingency,tablesList = tablesList,
x=x,t=t,m=m,k=k,status=status, parallel=FALSE)
APV
mean(samples)
} # } 
```
