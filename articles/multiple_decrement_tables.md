# Multiple decrement tables in lifecontingencies

    ## Package:  lifecontingencies
    ## Authors:  Giorgio Alfredo Spedicato [aut, cre] (ORCID:
    ##     <https://orcid.org/0000-0002-0315-8888>),
    ##   Christophe Dutang [ctb] (ORCID:
    ##     <https://orcid.org/0000-0001-6732-1501>),
    ##   Reinhold Kainhofer [ctb] (ORCID:
    ##     <https://orcid.org/0000-0002-7895-1311>),
    ##   Kevin J Owens [ctb],
    ##   Ernesto Schirmacher [ctb],
    ##   Gian Paolo Clemente [ctb] (ORCID:
    ##     <https://orcid.org/0000-0001-6795-4595>),
    ##   Ivan Williams [ctb]
    ## Version:  1.6.3
    ## Date:     
    ## BugReport: https://github.com/spedygiorgio/lifecontingencies/issues

## Introduction

Until now no R package provided a good tool to manage multiple decrement
tables, even if Deshmukh (2012) gives an R-based treatment of multiple
decrement tables with applications. The topic is deeply related to
multistate analysis of life histories, on which Willekens (2014) provide
a very good introduction. The `mdt` class of **lifecontingencies** has
been specifically engineered to manage multiple decrement models in R.

This vignette walks through the whole workflow the package supports
around multiple decrements: creating and validating a combined `mdt`
table, computing decrement probabilities from it, deriving the
Associated Single Decrement Table (ASDT) implied by each cause under the
uniform distribution of decrements (UDD) assumption, using a `mdt`
object in actuarial insurance and annuity calculations, and finally
building a real, cause-specific `mdt` object end-to-end from an official
government mortality report. For a shorter, self-contained example see
the “Multiple Decrement Models” section of the main package vignette
(`vignette("an_introduction_to_lifecontingencies_package", package = "lifecontingencies")`);
this vignette develops the same material in full.

Following the notation in Finan (2014), let
$l_{x}^{(\tau)} = \sum_{j = 1\ldots m}l_{x}^{(j)}$ be the survivors to
age $x$ that will, at future ages, be fully depleted by $m$ causes of
decrement. $d_{x}^{(j)} = l_{x}^{(j)} - l_{x + 1}^{(j)}$ is the expected
number of lives exiting the population between ages $x$ and $x + 1$ due
to decrement $j$, so that
${}_{n}d_{x}^{(j)} = \sum_{t = 0\ldots n - 1}d_{x + t}^{(j)}$. The
probability that a life aged $x$ will leave the group within one year as
a result of decrement $j$ is
${}_{n}q_{x}^{(j)} = \frac{{}_{n}d_{x}^{(j)}}{l_{x}^{(\tau)}}$, so that
$q_{x}^{(\tau)} = \sum_{j = 1}^{m}q_{x}^{(j)}$ and
${}_{t}q_{x}^{(\tau)} = 1 - {}_{t}p_{x}^{(\tau)} = \sum_{j}{}_{t}q_{x}^{(j)}$.

## The mdt class

Examples in this section are worked on slides provided by Valdez (2011)
(p. 4). We create a `mdt` class object:

``` r
valdezDf <- data.frame(
  x = c(50:54),
  lx = c(4832555, 4821937, 4810206, 4797185, 4782737),
  heart = c(5168, 5363, 5618, 5929, 6277),
  accidents = c(1157, 1206, 1443, 1679, 2152),
  other = c(4293, 5162, 5960, 6840, 7631)
)
valdezMdt <- new("mdt", name = "ValdezExample", table = valdezDf)
## Added fictional decrement below last x and completed x and lx until zero.... 
## Completed the table at top, all decrements on first cause
```

The `mdt` class is an S4 class object (Chambers 2008) comprised of a
character slot `name` and a `data.frame` slot `table`, made of:

1.  `x`: the age, from 0 to $\omega$.
2.  `lx`: the subjects living (at risk) at the beginning of age $x$.
3.  one or more columns for the different causes of decrement.

Values within `table` represent the absolute number of subjects at risk
at the beginning of age $x$ dying of cause $j$ during the period $x$ to
$x + 1$.

`setValidity` performs consistency checks when a `mdt` object is
created. In particular it verifies that:

1.  `x` and `lx` exist and are consistent: `x` starts from 0 and
    increases by one, and
    $l_{x} = l_{x - 1} - \left( d_{x - 1,1} + d_{x - 1,2} + \ldots + d_{x - 1,k} \right)$
    for every $x$.
2.  If the decrements (or `x` and `lx`) are supplied only for a partial
    age range, the table is completed below assuming a decrement rate of
    `0.01` for the first cause of death. This rate can be changed via
    the `bottomCompletionSurvival` argument of `new("mdt", ...)`
    (default `0.99`, i.e. a 0.01 decrement rate, unchanged from previous
    releases); it only affects the synthetically reconstructed ages
    below the lowest age actually supplied and leaves all originally
    supplied rows untouched.
3.  If the decrements at the last supplied age $\omega$ do not sum to
    $l_{\omega}$, the table is extended by one row such that
    $l_{\omega + 1} = l_{\omega} - \left( d_{\omega,1} + d_{\omega,2} + \ldots + d_{\omega,j} \right)$.

The internal function `.tableSanitizer` implements this completion logic
and reports on the console what it did. The table can be viewed with
`print` and `show` (output omitted here for brevity), and coerced to a
`data.frame` or, if the **markovchain** package is available, to a
`markovchainList` object:

``` r
print(valdezMdt)
```

``` r
valdezDf2 <- as(valdezMdt, "data.frame")
if (requireNamespace("markovchain", quietly = TRUE) &&
    methods::canCoerce(valdezMdt, "markovchainList")) {
  valdezMarkovChainList <- as(valdezMdt, "markovchainList")
} else {
  message("'markovchain' package or S4 coercion method unavailable: skipping mdt -> markovchainList conversion.")
}
```

Two specific methods are defined for `mdt` objects: `getOmega`,
returning the maximum attainable age (as for the `lifetable` class), and
`getDecrements`, returning the decrement names (the columns of `table`
other than `x` and `lx`). A `summary` method is available as well.

``` r
getOmega(valdezMdt)
## [1] 55
getDecrements(valdezMdt)
## [1] "heart"     "accidents" "other"
summary(valdezMdt)
## This is Multiple Decrements Table:  ValdezExample 
##  Omega age is:  55 
##  Stored decrements are:  heart accidents other
```

## Decrement probabilities calculation

**lifecontingencies** makes it easy to compute $d_{x}^{(j)}$,
${}_{n}d_{x}^{(j)}$ and ${}_{n}d_{x}^{(\tau)}$ via the `dxt` function,
and the corresponding probabilities via `qxt`/`pxt`:

``` r
dxt(valdezMdt, x = 51, decrement = "other")
dxt(valdezMdt, x = 51, t = 2, decrement = "other")
dxt(valdezMdt, x = 51)
pxt(valdezMdt, x = 50, t = 3)
## [1] 0.9926809
qxt(valdezMdt, x = 53, t = 2, decrement = 1)
## [1] 0.002544409
```

It is also possible to generate random trajectories of a life subject to
multiple causes of decrement:

``` r
rmdt(n = 2, object = valdezMdt, x = 50, t = 2)
##    1       2      
## 50 "alive" "alive"
## 51 "alive" "alive"
## 52 "alive" "alive"
```

## Associated Single Decrement Table calculation

For each force of decrement $\mu_{j}(x + t)$, the Associated Single
Decrement Table (ASDT) is a decrement model that assumes survivorship
depends only on $j$. Within the ASDT the following identities hold:

$${}_{t}p_{x}^{\prime{(j)}} = \exp\!\left( \int_{a}^{b}\mu_{j}(x + s)\, ds \right),\qquad{}_{t}q_{x}^{\prime{(j)}} = 1 - {}_{t}p_{x}^{\prime{(j)}},\qquad{}_{t}p_{x}^{(\tau)} = \prod\limits_{j = 1}^{m}{}_{t}p_{x}^{\prime{(j)}}.$$

Assuming a uniform distribution of decrements (UDD), i.e.
${}_{t}q_{x}^{(j)} = t \cdot q_{x}^{(j)}$ for
$t \in \lbrack 0,1\rbrack$, then:

$$p_{x}^{\prime{(j)}} = \left( 1 - t \cdot q_{x}^{(\tau)} \right)^{\frac{q_{x}^{(j)}}{q_{x}^{(\tau)}}} = 1 - q_{x}^{\prime{(j)}}.$$

This is implemented in `qxt.prime.fromMdt`:

``` r
qxt.prime.fromMdt(object = valdezMdt, x = 53, decrement = "accidents")
## [1] 0.0003504636
```

If UDD also holds for each associated single decrement, then:

$${}_{t}q_{x}^{(j)} = {}_{t}q_{x}^{\prime{(j)}} \cdot \int_{0}^{t}\prod\limits_{i \neq j}\left( 1 - s \cdot q_{x}^{\prime{(i)}} \right)ds,$$

of which the case $m = 2$, $t = 1$ is a particular case:

$$q_{x}^{(2)} = q_{x}^{\prime{(2)}}\left( 1 - 0.5\, q_{x}^{\prime{(1)}} \right).$$

`qxt.fromQxprime` computes this; the example below replicates Finan
(2014, Example 67.2):

``` r
qxt.fromQxprime(qx.prime = 0.01, other.qx.prime = c(0.03, 0.06))
## [1] 0.009556
```

### Extracting the full ASDT matrix: `independentRatesFromMdt()`

While `qxt.prime.fromMdt` returns a single scalar for one (age,
decrement) pair, `independentRatesFromMdt` computes the full matrix of
independent rates $q\prime_{x}^{(j)}$ for every age and every decrement
in the table. This is convenient for inspecting and comparing rates
across ages, and for the round-trip reconstruction described below.

``` r
qprime <- independentRatesFromMdt(valdezMdt, x = 50:54)
qprime
##          heart    accidents       other
## 50 0.001070017 0.0002396526 0.000888932
## 51 0.001112944 0.0002503803 0.001071254
## 52 0.001168833 0.0003003488 0.001239943
## 53 0.001237032 0.0003504636 0.001426969
## 54 0.001313773 0.0004506072 0.001596938
```

Each cell equals the value that `qxt.prime.fromMdt` would return for the
same (age, decrement) pair:

``` r
qxt.prime.fromMdt(valdezMdt, x = 53, decrement = "accidents")
## [1] 0.0003504636
qprime["53", "accidents"]
## [1] 0.0003504636
```

A subset of ages may be specified:

``` r
independentRatesFromMdt(valdezMdt, x = 51:52)
##          heart    accidents       other
## 51 0.001112944 0.0002503803 0.001071254
## 52 0.001168833 0.0003003488 0.001239943
```

### Building an mdt from independent rates: `buildMdtFromIndependentRates()`

The inverse operation — constructing a combined multiple-decrement table
from a matrix of ASDT independent rates — is provided by
`buildMdtFromIndependentRates`. This is useful when independent rates
are estimated separately (e.g. from cause-specific mortality studies or
from different data sources) and need to be combined into a single `mdt`
object.

For each age, the absolute (combined) rate of decrement $j$ is obtained
by the UDD integration formula:
$$q_{x}^{(j)} = q\prime_{x}^{(j)}\int_{0}^{1}\prod\limits_{i \neq j}(1 - s\, q\prime_{x}^{(i)})\, ds,$$
and the survivorship column is built recursively from
$p_{x}^{(\tau)} = \prod_{j}\left( 1 - q\prime_{x}^{(j)} \right)$.

The following example replicates Finan (2014, Example 67.4): three
decrements (death, disability, retirement) over two ages, with 1000
initial lives:

``` r
qp <- matrix(c(0.010, 0.030, 0.100,
                0.013, 0.050, 0.200),
             nrow = 2, byrow = TRUE,
             dimnames = list(NULL, c("death", "disability", "retirement")))
qp
##      death disability retirement
## [1,] 0.010       0.03        0.1
## [2,] 0.013       0.05        0.2

mdt674 <- buildMdtFromIndependentRates(
  x = 60:61, qx.primes = qp,
  radix = 1000, name = "Finan 67.4"
)
## Added fictional decrement below last x and completed x and lx until zero.... 
## Completed the table at top, all decrements on first cause
print(mdt674)
## Multiple decrements table Finan 67.4 
##         death disability retirement
## 0  0.01000000 0.00000000  0.0000000
## 1  0.01000000 0.00000000  0.0000000
## 2  0.01000000 0.00000000  0.0000000
## 3  0.01000000 0.00000000  0.0000000
## 4  0.01000000 0.00000000  0.0000000
## 5  0.01000000 0.00000000  0.0000000
## 6  0.01000000 0.00000000  0.0000000
## 7  0.01000000 0.00000000  0.0000000
## 8  0.01000000 0.00000000  0.0000000
## 9  0.01000000 0.00000000  0.0000000
## 10 0.01000000 0.00000000  0.0000000
## 11 0.01000000 0.00000000  0.0000000
## 12 0.01000000 0.00000000  0.0000000
## 13 0.01000000 0.00000000  0.0000000
## 14 0.01000000 0.00000000  0.0000000
## 15 0.01000000 0.00000000  0.0000000
## 16 0.01000000 0.00000000  0.0000000
## 17 0.01000000 0.00000000  0.0000000
## 18 0.01000000 0.00000000  0.0000000
## 19 0.01000000 0.00000000  0.0000000
## 20 0.01000000 0.00000000  0.0000000
## 21 0.01000000 0.00000000  0.0000000
## 22 0.01000000 0.00000000  0.0000000
## 23 0.01000000 0.00000000  0.0000000
## 24 0.01000000 0.00000000  0.0000000
## 25 0.01000000 0.00000000  0.0000000
## 26 0.01000000 0.00000000  0.0000000
## 27 0.01000000 0.00000000  0.0000000
## 28 0.01000000 0.00000000  0.0000000
## 29 0.01000000 0.00000000  0.0000000
## 30 0.01000000 0.00000000  0.0000000
## 31 0.01000000 0.00000000  0.0000000
## 32 0.01000000 0.00000000  0.0000000
## 33 0.01000000 0.00000000  0.0000000
## 34 0.01000000 0.00000000  0.0000000
## 35 0.01000000 0.00000000  0.0000000
## 36 0.01000000 0.00000000  0.0000000
## 37 0.01000000 0.00000000  0.0000000
## 38 0.01000000 0.00000000  0.0000000
## 39 0.01000000 0.00000000  0.0000000
## 40 0.01000000 0.00000000  0.0000000
## 41 0.01000000 0.00000000  0.0000000
## 42 0.01000000 0.00000000  0.0000000
## 43 0.01000000 0.00000000  0.0000000
## 44 0.01000000 0.00000000  0.0000000
## 45 0.01000000 0.00000000  0.0000000
## 46 0.01000000 0.00000000  0.0000000
## 47 0.01000000 0.00000000  0.0000000
## 48 0.01000000 0.00000000  0.0000000
## 49 0.01000000 0.00000000  0.0000000
## 50 0.01000000 0.00000000  0.0000000
## 51 0.01000000 0.00000000  0.0000000
## 52 0.01000000 0.00000000  0.0000000
## 53 0.01000000 0.00000000  0.0000000
## 54 0.01000000 0.00000000  0.0000000
## 55 0.01000000 0.00000000  0.0000000
## 56 0.01000000 0.00000000  0.0000000
## 57 0.01000000 0.00000000  0.0000000
## 58 0.01000000 0.00000000  0.0000000
## 59 0.01000000 0.00000000  0.0000000
## 60 0.00936000 0.02836000  0.0980100
## 61 0.01141833 0.04471833  0.1937433
## 62 1.00000000 0.00000000  0.0000000
```

We can verify the key numerical results from the example. The combined
survival probability at age 60 is
$p_{60}^{(\tau)} = (1 - 0.010)(1 - 0.030)(1 - 0.100) = 0.86427$, giving
$l_{61}^{(\tau)} = 864.27$:

``` r
ptau60 <- prod(1 - qp[1, ])
round(ptau60, 5)
## [1] 0.86427
tbl674 <- mdt674@table
round(tbl674$lx[tbl674$x == 61], 2)
## [1] 864.27
```

### Round-trip: mdt $\rightarrow$ ASDT $\rightarrow$ mdt

The two functions compose naturally: extracting independent rates from
an `mdt` and then rebuilding should recover the original table (up to
floating-point rounding). We verify this on the Valdez example:

``` r
qprime <- independentRatesFromMdt(valdezMdt, x = 50:54)
rebuilt <- buildMdtFromIndependentRates(
  x = 50:54, qx.primes = qprime,
  radix = valdezMdt@table$lx[valdezMdt@table$x == 50],
  name = "Roundtrip"
)
## Added fictional decrement below last x and completed x and lx until zero.... 
## Completed the table at top, all decrements on first cause

## Compare lx
data.frame(
  age = 50:54,
  original = valdezMdt@table$lx[valdezMdt@table$x %in% 50:54],
  rebuilt  = round(rebuilt@table$lx[rebuilt@table$x %in% 50:54], 0)
)
##   age original rebuilt
## 1  50  4832555 4832555
## 2  51  4821937 4821937
## 3  52  4810206 4810206
## 4  53  4797185 4797185
## 5  54  4782737 4782737
```

## Visualising mdt objects: `plot()`

The `plot` S4 method for `mdt` objects produces a `ggplot2`
visualisation of the decrement structure. Three views are available:

- `"area"` (default): stacked-area chart of decrement counts
  $d_{x}^{(j)}$.
- `"bar"`: stacked bar chart of decrement counts.
- `"probability"`: line chart of decrement-specific probabilities
  $q_{x}^{(j)} = d_{x}^{(j)}/l_{x}^{(\tau)}$.

``` r
plot(valdezMdt)
```

``` r
plot(valdezMdt, type = "bar")
```

``` r
plot(valdezMdt, type = "probability")
```

The plot method also works on larger tables; here is the CDC-based `mdt`
object built later in this vignette, aggregated across its 18 causes:

``` r
plot(cdcMdt, type = "probability")
```

## Actuarial applications

Two functions value contracts on a `mdt` object. `Axn.mdt` gives the
actuarial present value (APV) of a term insurance paying, at the end of
the year of decrement, a benefit $b_{j}$ that may depend on the cause
$j$:
$$\sum\limits_{j \in J}b_{j}\sum\limits_{h = 0}^{n - 1}v^{h + 1}\,{}_{h}p_{x}^{(\tau)}\, q_{x + h}^{(j)},$$
where $J$ is the set of covered decrements (all of them if `decrement`
is omitted). `axn.mdt` gives the APV of an annuity payable while the
insured has not yet left the table, with optional deferment `m`, `k`
payments per year and payments in advance or in arrears. Benefit
premiums and reserves follow from the equivalence principle.

The example below, from Finan (2014, Problem 68.1), considers a 3-year
term issued to (16) paying 20,000 at the end of the year of death if
death results from an accident:

``` r
myTable <- data.frame(
  x = c(16, 17, 18),
  lx = c(20000, 17600, 14520),
  da = c(1300, 1870, 2380),
  doc = c(1100, 1210, 1331)
)
myMdt <- new("mdt", table = myTable, name = "Sample")
## Added fictional decrement below last x and completed x and lx until zero.... 
## Completed the table at top, all decrements on first cause
```

The value of $A_{16:\overline{3}|}^{1}$, restricted to decrement `da`
(accidental death), is computed by `Axn.mdt`:

``` r
20000 * Axn.mdt(object = myMdt, x = 16, n = 3, i = .1, decrement = "da")
## [1] 4515.402
```

If `n` is omitted the cover runs to the end of the table, which here
would include the synthetic closing row (age 19) added by
`new("mdt", ...)`: for term covers it is safer to always pass `n`.

Finan (2014, Example 69.1) uses the same table to price a level annual
premium. Finan’s printed solution uses the column of non-accidental
deaths (1100, 1210, 1331), giving $A = 3000/20000$,
${\ddot{a}}_{16:\overline{3}|}^{(\tau)} = 2.4$ and a premium of 1250:

``` r
A691 <- Axn.mdt(myMdt, x = 16, n = 3, i = 0.10, decrement = "doc")
a691 <- axn.mdt(myMdt, x = 16, n = 3, i = 0.10)
c(APV = 20000 * A691, annuity = a691, premium = 20000 * A691 / a691)
##     APV annuity premium 
##  3000.0     2.4  1250.0
```

Benefits that depend on the cause are passed through `benefits`. Finan
(2014, Example 68.1) pays 1 for cause 1 and 2 for cause 2 over two years
at $i = 50\%$ (printed answer 0.6852), and \[Example 69.2\] computes the
benefit reserve ${}_{2}V = 11.091$ of a 4-year term paying 2000 for
decrement 1 and 1000 for decrement 2, with annual premium 34 and
$v = 0.95$:

``` r
m681 <- new("mdt", table = data.frame(x = 50:51, lx = c(1200, 800),
                                      d1 = c(100, 200), d2 = c(300, 300)))
## Added fictional decrement below last x and completed x and lx until zero.... 
## Completed the table at top, all decrements on first cause
Axn.mdt(m681, x = 50, n = 2, i = 0.5, decrement = c("d1", "d2"),
        benefits = c(1, 2))
## [1] 0.6851852

m692 <- new("mdt", table = data.frame(x = 41:43, lx = c(800, 776, 752),
                                      d1 = 8, d2 = 16))
## Added fictional decrement below last x and completed x and lx until zero.... 
## Completed the table at top, all decrements on first cause
i692 <- 1 / 0.95 - 1
Axn.mdt(m692, x = 42, n = 2, i = i692, decrement = c("d1", "d2"),
        benefits = c(2000, 1000)) - 34 * axn.mdt(m692, x = 42, n = 2, i = i692)
## [1] 11.09072
```

Another example is inspired by data in De Angelis, Paolo and Di Falco,
L. (2016). We use life tables from that source and a small helper
function (still based on single decrement tables) to compute the APV of
a constant or variable annuity with multiple decrement, where benefits
are payable while the annuitant is in the current state and cease upon
transition to another state:

``` r
axnmdt.firsttype <- function(object, x, n, i, payment = "advance", delta = 0) {
  # delta is the annuity indexing
  out <- numeric(1)
  if (!(class(object) %in% c("lifetable", "actuarialtable", "mdt")))
    stop("Error! Only lifetable, actuarialtable or mdt classes are accepted")
  if (missing(object))
    stop("Error! Need a Multiple decrement table")
  if (missing(x))
    stop("Error! Need age!")
  if (x > getOmega(object)) {
    stop("Age greater than Omega")
  }
  if (class(object) == "mdt") {
    if (x < min(object@table$x)) {
      stop("Age lower than minimum age")
    }
  }
  if (class(object) == "actuarialtable") {
    if (x < min(object@x)) {
      stop("Age lower than minimum age")
    }
  }
  if (!(missing(i))) {
    interest <- i
  } else {
    if (class(object) == "actuarialtable") {
      interest <- object@interest
    } else {
      stop("Needed Interest Rate ")
    }
  }
  if (missing(n))
    n <- (getOmega(object) - x)
  if (n == 0) {
    stop("Contract duration equal to zero")
  }
  probs <- numeric(n)
  times <- seq(from = 0, to = n - 1, by = 1)
  if (payment == "arrears") times <- times + 1
  for (j in 1:length(times)) probs[j] <- pxt(object, x, times[j])
  out <- sum(apply(cbind(probs, ((1 + interest) / (1 + delta))^-times), 1, prod))
  return(out)
}
```

Data and examples are taken from De Angelis, Paolo and Di Falco, L.
(2016):

``` r
data("de_angelis_di_falco")
HealthyMaleTable2013 <- de_angelis_di_falco$HealthyMaleTable2013
DAT <- new("actuarialtable",
  x = de_angelis_di_falco$DisabledMaleLifeTable$age,
  lx = de_angelis_di_falco$DisabledMaleLifeTable$'2013',
  name = "DisabledTable", i = 0.03
)
axnmdt.firsttype(DAT, x = 65, n = 10, i = 0.03, payment = "arrears", delta = 0.02)
## [1] 3.73169
axnmdt.firsttype(DAT, 65, 10, payment = "arrears", delta = 0.02)
## [1] 3.73169
axnmdt.firsttype(DAT, 65, 10, payment = "arrears", i = 0.03, delta = 0.02)
## [1] 3.73169
# Last case equal to axn
axnmdt.firsttype(DAT, 65, 10, payment = "arrears", delta = 0)
## [1] 3.461472
axn(DAT, 65, 10, payment = "arrears")
## [1] 3.461472
```

## A worked example on real cause-of-death data, with a note on graduation

The examples above use small, illustrative tables. This section works
through the full pipeline — from an official, publicly available
cause-specific mortality report to a validated `mdt` object — using real
data: Table 8 of Arias et al. (2013), “United States Life Tables
Eliminating Certain Causes of Death, 1999-2001”, published by the U.S.
National Center for Health Statistics. Table 8 tabulates, for 22
five-year (and one open-ended) age bands from birth to 100+, the number
of decrements attributable to each of 33 causes of death out of a radix
of $l_{0} = 10,000,000$.

### From an official report to a mutually exclusive partition

A `mdt` object requires its non-`x`/`lx` columns to be a partition of
total mortality: at every age the columns must sum to exactly the
decrement implied by `lx`. The 33 causes tabulated by Arias et al.
(2013), however, are *not* mutually exclusive: several rows are parent
categories that contain other rows as subtypes (e.g. “Malignant
neoplasms” contains site-specific cancers such as colon, lung, breast
and prostate), and others are cross-cutting aggregates that overlap
several disease groups (e.g. “Alcohol-induced causes”). Summing all 33
columns as if they were disjoint would double count decrements.

We therefore curate a mutually exclusive partition made of the 17
top-level (non-nested, non-aggregate) causes of the original 33, plus a
residual `other` category that closes the table to total mortality at
every age. The columns kept are `septicemia`, `hiv`, `cancer`,
`diabetes`, `alzheimer`, `heart_disease`, `hypertension`,
`cerebrovascular`, `influenza_pneumonia`, `copd`, `pneumonitis`,
`liver_disease`, `nephritis`, `congenital`, `accidents`, `suicide` and
`homicide`; `other` is obtained, for each age band, as the total
decrements minus the sum of these 17 columns, and is non-negative at
every age band, confirming no double counting remains.

We embed the resulting curated, mutually exclusive 5-year table below
(`partition_sum` is the sum of the 17 named causes, kept only as a
diagnostic column to show that `partition_sum + other == total_dx` at
every age; it is dropped before building the `mdt`):

``` r
bandStart <- c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65,
               70, 75, 80, 85, 90, 95, 100)
cdc5yr <- data.frame(
  age_group = c("0-1", "1-5", "5-10", "10-15", "15-20", "20-25", "25-30",
    "30-35", "35-40", "40-45", "45-50", "50-55", "55-60", "60-65",
    "65-70", "70-75", "75-80", "80-85", "85-90", "90-95", "95-100",
    "100 and over"),
  lx = c(10000000.0, 9930523.0, 9917600.0, 9909689.0, 9899801.0, 9866403.0,
    9820252.0, 9775095.0, 9720086.0, 9642151.0, 9527362.0, 9360142.0,
    9123237.0, 8764226.0, 8233035.0, 7489085.0, 6464402.0, 5088466.0,
    3451511.0, 1849572.0, 687944.7, 147917.1),
  total_dx = c(69477.0, 12923.0, 7911.0, 9888.0, 33398.0, 46151.0, 45157.0,
    55009.0, 77935.0, 114789.0, 167220.0, 236905.0, 359011.0, 531191.0,
    743950.0, 1024683.0, 1375936.0, 1636955.0, 1601939.0, 1161627.3,
    540027.6, 147917.1),
  partition_sum = c(20749.0, 9384.9, 6159.0, 7847.6, 29571.0, 40140.9,
    38078.3, 45424.2, 63928.2, 94335.2, 138926.7, 202948.7, 315395.2,
    470524.7, 658241.3, 900367.8, 1191618.0, 1395849.7, 1344478.8,
    958568.3, 436904.4, 116411.8),
  other = c(48728.0, 3538.1, 1752.0, 2040.4, 3827.0, 6010.1, 7078.7,
    9584.8, 14006.8, 20453.8, 28293.3, 33956.3, 43615.8, 60666.3,
    85708.7, 124315.2, 184318.0, 241105.3, 257460.2, 203059.0, 103123.2,
    31505.3),
  septicemia = c(722.6, 247.7, 91.8, 76.5, 105.3, 194.7, 257.8, 418.8,
    671.8, 1120.7, 1847.0, 2783.5, 4280.8, 6452.9, 9611.9, 13677.9,
    19662.0, 23350.0, 23250.0, 15851.9, 6872.4, 1626.4),
  hiv = c(28.3, 43.0, 58.0, 62.8, 88.2, 427.6, 1680.0, 4167.0, 6114.4,
    6706.2, 5899.3, 4113.8, 2677.1, 1727.4, 1046.5, 539.9, 257.5, 81.0,
    25.5, 19.5, 10.6, 2.7),
  cancer = c(187.9, 1060.0, 1201.0, 1242.0, 1814.4, 2526.2, 3518.9,
    6114.5, 11915.0, 23559.7, 44358.6, 77442.7, 131212.1, 198969.2,
    267824.2, 332678.3, 368548.5, 334200.1, 233736.2, 113426.7, 35785.0,
    6122.5),
  diabetes = c(6.7, 16.9, 19.3, 58.8, 120.0, 277.8, 513.9, 940.3, 1524.6,
    2623.1, 4656.5, 8029.6, 13462.8, 20843.6, 29206.0, 37986.0, 46899.8,
    48482.2, 38608.7, 21835.1, 8005.9, 1545.5),
  alzheimer = c(0.0, 0.0, 0.0, 0.0, 0.0, 0.9, 0.8, 2.4, 2.8, 8.6, 42.5,
    126.0, 482.8, 1366.0, 3477.9, 9876.6, 25418.6, 49458.4, 63722.0,
    54057.0, 25433.7, 5666.6),
  heart_disease = c(1244.1, 496.3, 257.8, 402.5, 990.5, 1632.3, 2575.5,
    4841.8, 9546.1, 19013.9, 33655.2, 56478.6, 92602.3, 141365.9,
    202096.5, 288960.8, 415183.0, 537740.9, 577672.7, 457905.7, 227285.0,
    66431.5),
  hypertension = c(3.3, 4.2, 1.6, 3.2, 11.4, 33.8, 64.9, 138.3, 220.6,
    485.8, 838.2, 1359.1, 1972.7, 3237.0, 4635.8, 6594.0, 10279.8,
    14091.1, 15639.9, 12697.0, 6581.1, 1933.8),
  cerebrovascular = c(278.6, 123.9, 71.7, 106.2, 164.9, 324.5, 513.9,
    926.9, 1870.1, 3586.8, 5965.7, 8723.2, 13848.4, 22185.8, 34745.3,
    58994.0, 100084.4, 145206.2, 161264.4, 122436.7, 54220.7, 12630.6),
  influenza_pneumonia = c(755.1, 290.7, 112.0, 107.1, 166.6, 298.6, 337.8,
    506.5, 893.9, 1322.6, 1880.1, 2539.4, 3857.3, 6272.5, 9849.1,
    17726.2, 31608.2, 51735.7, 64665.0, 58841.1, 32133.0, 10956.1),
  copd = c(94.0, 124.7, 112.0, 195.6, 218.0, 262.2, 313.4, 429.9, 686.0,
    1299.8, 2657.7, 5557.3, 13792.5, 27507.5, 49137.5, 76948.9, 100455.3,
    105576.7, 81889.6, 41622.6, 14325.5, 2877.8),
  pneumonitis = c(31.6, 31.2, 13.7, 21.7, 25.3, 62.3, 72.4, 105.1, 131.2,
    243.3, 336.5, 528.8, 866.2, 1408.2, 2466.9, 4832.3, 8971.9, 14562.1,
    17864.6, 14771.2, 7068.0, 1928.4),
  liver_disease = c(9.1, 4.2, 1.6, 4.0, 18.0, 57.1, 198.0, 769.7, 2337.2,
    4790.8, 7989.0, 8922.6, 9780.0, 11092.9, 11344.1, 10895.8, 9658.5,
    6664.8, 3434.6, 1153.5, 225.4, 24.3),
  nephritis = c(383.3, 37.9, 26.6, 33.0, 62.9, 151.5, 218.2, 365.1, 599.5,
    977.3, 1607.2, 2564.0, 4302.1, 7118.9, 11608.2, 16664.5, 24123.1,
    29663.4, 29579.4, 21013.8, 9269.0, 2227.8),
  congenital = c(13912.0, 1349.1, 472.9, 495.9, 573.3, 586.8, 543.4,
    596.6, 578.2, 636.4, 732.9, 848.9, 1003.8, 1066.8, 970.3, 879.6,
    1001.5, 1063.5, 870.6, 504.0, 236.0, 56.6),
  accidents = c(2249.1, 4586.7, 3332.5, 3845.8, 16420.4, 19138.2,
    14938.4, 14067.2, 15926.3, 17254.8, 16556.9, 14316.4, 13565.0,
    13626.6, 14401.4, 17356.4, 23649.9, 29109.6, 29211.3, 21255.5,
    9191.2, 2357.0),
  suicide = c(0.0, 0.0, 12.9, 655.5, 3959.4, 6077.0, 6007.4, 6197.6,
    6794.9, 7200.1, 7130.0, 6521.5, 6005.7, 4995.4, 4726.3, 4904.1,
    5078.7, 4297.4, 2719.2, 1042.0, 214.9, 18.9),
  homicide = c(843.2, 968.4, 373.7, 537.2, 4832.4, 8089.4, 6323.6,
    4836.4, 4115.3, 3505.5, 2773.4, 2093.2, 1683.3, 1288.2, 1093.5,
    852.3, 737.2, 566.6, 325.3, 135.1, 46.9, 5.4)
)
## sanity check: the curated partition closes exactly to total mortality
stopifnot(all(abs(cdc5yr$partition_sum + cdc5yr$other - cdc5yr$total_dx) < 1e-6))
```

### Graduating from 5-year bands to single years of age

The `mdt` class requires one row per single year of age, while Table 8
is tabulated in 5-year bands. A modern, purpose-built way to “ungroup”
banded counts into single-year counts is Eilers’ penalized composite
link model (PCLM, Eilers (2007)), implemented in the **ungroup** and
**DemoTools** packages; it lets the within-band shape — including how
the cause-of-death mix evolves within a band — be estimated smoothly,
and is generally the preferred approach for this task.

**This vignette does not use PCLM.** **lifecontingencies** does not
depend on **ungroup** or **DemoTools**, and adding either as a
`Suggests` purely to compile one example was judged not worth the extra
dependency surface. Instead, we graduate `lx` with a monotone cubic
Hermite interpolant (the Fritsch-Carlson method, Fritsch and Carlson
(1980)), available in base R as
`stats::splinefun(..., method = "monoH.FC")`, and then split each
single-year total decrement across causes using each band’s own, fixed
cause mix (i.e. the cause shares are constant within a band and only the
total number of decrements is graduated). This is a materially weaker
assumption than PCLM: real cause-of-death mixes drift gradually within a
5-year band (e.g. the balance between accidents and degenerative disease
shifts within the 15-20 age band), and a constant-share assumption
cannot reproduce that drift. The single-year figures produced below
should therefore be read as a reasonable, fully reproducible,
dependency-free approximation, **not as a substitute for a proper PCLM
ungrouping**. Because the split is proportional to a graduated total
that itself sums exactly across each band, the construction below
reaggregates exactly back to the original 5-year, cause-specific totals
(checked below), which a naive graduation might not guarantee.

``` r
causeCols <- setdiff(names(cdc5yr),
                      c("age_group", "lx", "total_dx", "partition_sum", "other"))
causeCols <- c(causeCols, "other")   # 17 named causes + residual

## monotone Hermite (Fritsch-Carlson) graduation of lx, ages 0..100
lxFun <- splinefun(x = bandStart, y = cdc5yr$lx, method = "monoH.FC")
ages <- 0:100
lxSingle <- c(lxFun(ages), 0)          # append the closing lx=0 at age 101
dxTotalSingle <- -diff(lxSingle)       # single-year total decrements, ages 0..100
band <- findInterval(ages, bandStart)  # which 5-year band each single age falls in

cdcSingle <- data.frame(x = ages, lx = lxSingle[1:101])
for (cs in causeCols) {
  shareBand <- cdc5yr[[cs]] / cdc5yr$total_dx
  cdcSingle[[cs]] <- dxTotalSingle * shareBand[band]
}

## verify exact reaggregation back to the original 5-year cause totals
recheck <- aggregate(cdcSingle[causeCols],
                      by = list(age_group = cdc5yr$age_group[band]), FUN = sum)
recheck <- recheck[match(cdc5yr$age_group, recheck$age_group), ]
max(abs(as.matrix(recheck[causeCols]) - as.matrix(cdc5yr[causeCols])))
## [1] 2.910383e-11
```

### Building and validating the mdt object

The single-year table `cdcSingle` (columns `x`, `lx` and the 18 mutually
exclusive causes) is now a valid input for `new("mdt", ...)`; because it
already spans exactly $x = 0,\ldots,100$ and closes exactly to `lx[0]`,
no meaningful synthetic bottom- or top-completion is needed from
`.tableSanitizer` (described earlier in this vignette). At this scale,
floating-point arithmetic can still make the very last row’s decrements
sum to a value that differs from `lx` by a fraction of a unit, in which
case `.tableSanitizer` adds one further, economically negligible
completion row at $x = 101$ — a harmless artifact of exact
floating-point comparison, not a data or modelling error.

``` r
cdcMdt <- new("mdt",
              name = "NCHS US Life Table 1999-2001, cause-specific (Table 8)",
              table = cdcSingle)
## Completed the table at top, all decrements on first cause
getOmega(cdcMdt)
## [1] 101
getDecrements(cdcMdt)
##  [1] "septicemia"          "hiv"                 "cancer"             
##  [4] "diabetes"            "alzheimer"           "heart_disease"      
##  [7] "hypertension"        "cerebrovascular"     "influenza_pneumonia"
## [10] "copd"                "pneumonitis"         "liver_disease"      
## [13] "nephritis"           "congenital"          "accidents"          
## [16] "suicide"             "homicide"            "other"
isTRUE(validObject(cdcMdt))
## [1] TRUE
```

We can now use the same `dxt`/`qxt`/`pxt` functions used throughout this
vignette on a real, cause-specific national life table. For instance, at
age 70:

``` r
qxt(cdcMdt, x = 70, t = 1)                          # total decrement rate
## [1] 0.02421519
qxt(cdcMdt, x = 70, t = 1, decrement = "cancer")     # cancer only
## [1] 0.007861816
qxt(cdcMdt, x = 70, t = 1, decrement = "heart_disease")
## [1] 0.006828689
qxt(cdcMdt, x = 70, t = 1, decrement = "cancer") /
  qxt(cdcMdt, x = 70, t = 1)                          # cancer's share of q70
## [1] 0.3246646
```

At age 70 in this table, cancer alone accounts for roughly a third of
all decrements, illustrating how a `mdt` object built this way lets
standard **lifecontingencies** functions (`dxt`, `qxt`, `pxt`,
`Axn.mdt`, …) be applied directly to official, cause-specific national
mortality data, once the table has been reduced to a mutually exclusive
partition and graduated to single years of age.

## Competing-risks analysis: `mdtToLong()`

A `mdt` object is a life-table summary of a competing-risks process. For
analyses with standard survival tools, `mdtToLong` turns it into an
aggregated long data set: one row per (exit time, cause), a `status`
factor whose first level is `"censored"` (the layout expected by
[`survival::Surv`](https://rdrr.io/pkg/survival/man/Surv.html) for
multi-state data) and a `count` column to be used as case weights. Lives
still in the table at the end of the follow-up are censored there.

``` r
valdezLong <- mdtToLong(valdezMdt, x = 50, t = 5)
head(valdezLong)
##   time age    status count
## 1    1  51     heart  5168
## 2    1  51 accidents  1157
## 3    1  51     other  4293
## 4    2  52     heart  5363
## 5    2  52 accidents  1206
## 6    2  52     other  5162
```

With exits placed at the end of the year of age (the default), the
Aalen-Johansen estimate of the cumulative incidence of each cause
computed by **survival** coincides with the decrement probabilities of
the table:

``` r
if (requireNamespace("survival", quietly = TRUE)) {
  fit <- survival::survfit(survival::Surv(time, status) ~ 1,
                           data = valdezLong, weights = count)
  aj <- summary(fit, times = 1:5)$pstate[, -1]
  colnames(aj) <- getDecrements(valdezMdt)
  byTable <- sapply(getDecrements(valdezMdt), function(d)
    qxt(valdezMdt, x = 50, t = 1:5, decrement = d))
  max(abs(aj - byTable))
}
## [1] 5.290907e-17
```

The same conversion applies to the cause-specific national table built
above, e.g. to obtain the probability that a life aged 70 eventually
dies of cancer or of heart disease:

``` r
cdcLong <- mdtToLong(cdcMdt, x = 70)
tapply(cdcLong$count, cdcLong$status, sum)[c("cancer", "heart_disease")] /
  cdcMdt@table$lx[cdcMdt@table$x == 70]
##        cancer heart_disease 
##     0.1902098     0.3433236
```

## References

Arias, Elizabeth, Lester R. Curtin, Rong Wei, and Robert N. Anderson.
2013. “United States Life Tables Eliminating Certain Causes of Death,
1999–2001.” *National Vital Statistics Reports* 61 (9): 1–128.

Chambers, J. M. 2008. *Software for Data Analysis: Programming with* .
Statistics and Computing. Springer-Verlag.

De Angelis, Paolo, and Di Falco, L. 2016. *Assicurazioni sulla salute:
caratteristiche, modelli attuariali e basi tecniche*. Il Mulino. Il
Mulino.

Deshmukh, S. R. 2012. *Multiple Decrement Models in Insurance: An
Introduction Using* . SpringerLink : Bücher. Springer.

Eilers, Paul H. C. 2007. “Ill-Posed Problems with Counts, the Composite
Link Model and Penalized Likelihood.” *Statistical Modelling* 7 (3):
239–54.

Finan, Marcel A. 2014. “A Reading of the Theory of Life Contingency
Models: A Preparation for Exam MLC.” Lecture notes.

Fritsch, F. N., and R. E. Carlson. 1980. “Monotone Piecewise Cubic
Interpolation.” *SIAM Journal on Numerical Analysis* 17 (2): 238–46.

Valdez, Eduardo. 2011. “Multiple Decrement Models: Math 3631 Actuarial
Mathematics II.” Lecture notes.

Willekens, F. 2014. *Multistate Analysis of Life Histories with r*. Use
r! Springer International Publishing.
