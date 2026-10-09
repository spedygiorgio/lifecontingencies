# Extract the full Associated Single Decrement Table (ASDT) from an mdt object

`independentRatesFromMdt` returns a matrix of ASDT independent rates
\\q'^{(j)}\_x\\ for every combination of age and decrement in the
supplied multiple-decrement table.

## Usage

``` r
independentRatesFromMdt(object, x, t = 1)
```

## Arguments

- object:

  An `mdt` object.

- x:

  Optional numeric vector of ages to include. Defaults to all ages
  tabulated in `object` except the last. Note that this includes the
  synthetic ages that `new("mdt", ...)` adds below the lowest age
  supplied (e.g. ages 0-49 for a table given from age 50), whose
  decrements are all attributed to the first cause: pass `x` explicitly
  to restrict the result to the ages actually supplied.

- t:

  Period (default 1).

## Value

A numeric matrix with one row per age and one column per decrement. Row
names are the ages, column names the decrement identifiers.

## Details

The independent rate for decrement \\j\\ at age \\x\\ is obtained under
the Uniform Distribution of Deaths (UDD) assumption as \$\$q'^{(j)}\_x =
1 - \bigl(1 - q^{(\tau)}\_x\bigr)^{q^{(j)}\_x / q^{(\tau)}\_x},\$\$
which is the same formula used by
[`qxt.prime.fromMdt`](https://spedygiorgio.github.io/lifecontingencies/reference/qxt.prime.fromMdt.md)
for a single age/decrement pair. `independentRatesFromMdt` is a
convenience wrapper that applies this extraction to all ages and all
decrements at once, returning the result as a tidy matrix.

## See also

[`qxt.prime.fromMdt`](https://spedygiorgio.github.io/lifecontingencies/reference/qxt.prime.fromMdt.md)
for a single age/decrement pair,
[`buildMdtFromIndependentRates`](https://spedygiorgio.github.io/lifecontingencies/reference/buildMdtFromIndependentRates.md)
for the inverse operation.

## Examples

``` r
valdezDf <- data.frame(
  x = 50:54,
  lx = c(4832555, 4821937, 4810206, 4797185, 4782737),
  heart = c(5168, 5363, 5618, 5929, 6277),
  accidents = c(1157, 1206, 1443, 1679, 2152),
  other = c(4293, 5162, 5960, 6840, 7631))
valdezMdt <- new("mdt", name = "ValdezExample", table = valdezDf)
#> Added fictional decrement below last x and completed x and lx until zero.... 
#> Completed the table at top, all decrements on first cause 

# ASDT matrix on the ages actually supplied
independentRatesFromMdt(valdezMdt, x = 50:54)
#>          heart    accidents       other
#> 50 0.001070017 0.0002396526 0.000888932
#> 51 0.001112944 0.0002503803 0.001071254
#> 52 0.001168833 0.0003003488 0.001239943
#> 53 0.001237032 0.0003504636 0.001426969
#> 54 0.001313773 0.0004506072 0.001596938

# Subset of ages
independentRatesFromMdt(valdezMdt, x = 50:52)
#>          heart    accidents       other
#> 50 0.001070017 0.0002396526 0.000888932
#> 51 0.001112944 0.0002503803 0.001071254
#> 52 0.001168833 0.0003003488 0.001239943
```
