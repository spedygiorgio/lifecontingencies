# Build an mdt object from a matrix of independent (ASDT) rates

`buildMdtFromIndependentRates` constructs a multiple-decrement table
([`mdt`](https://spedygiorgio.github.io/lifecontingencies/reference/mdt-class.md))
from a matrix of independent single-decrement rates \\q'^{(j)}\_x\\, the
inverse of
[`independentRatesFromMdt`](https://spedygiorgio.github.io/lifecontingencies/reference/independentRatesFromMdt.md).

## Usage

``` r
buildMdtFromIndependentRates(
  x,
  qx.primes,
  radix = 1e+05,
  name = "ASDT-built mdt"
)
```

## Arguments

- x:

  Integer vector of ages (must be consecutive and start at 0 after
  internal completion by `new("mdt", ...)`). If missing, ages
  `0:(nrow(qx.primes)-1)` are used.

- qx.primes:

  Numeric matrix of independent rates. Rows correspond to ages in `x`,
  columns to decrement causes. Column names, if any, become the
  decrement identifiers in the resulting table; otherwise generic names
  `d1, d2, ...` are used.

- radix:

  Radix (initial cohort size). Default 100 000.

- name:

  Character string for the table name. Default `"ASDT-built mdt"`.

## Value

An `mdt` object.

## Details

For each age the combined (absolute) rate of decrement \\j\\ is obtained
by the UDD-based integration formula \$\$q^{(j)}\_x = q'^{(j)}\_x
\int_0^1 \prod\_{i \ne j} \bigl(1 - s\\q'^{(i)}\_x\bigr)\\ds,\$\$ which
is the same formula used by
[`qxt.fromQxprime`](https://spedygiorgio.github.io/lifecontingencies/reference/qxt.prime.fromMdt.md)
for a single age. The resulting absolute rates are multiplied by
\\l^{(\tau)}\_x\\ to obtain the decrement counts, and the survivorship
column is computed recursively from \\p^{(\tau)}\_x = \prod_j (1 -
q'^{(j)}\_x)\\.

## See also

[`independentRatesFromMdt`](https://spedygiorgio.github.io/lifecontingencies/reference/independentRatesFromMdt.md)
for the reverse extraction,
[`qxt.fromQxprime`](https://spedygiorgio.github.io/lifecontingencies/reference/qxt.prime.fromMdt.md)
for the single-age formula.

## Examples

``` r
# Finan (2014) Example 67.4:
# Three decrements (death, disability, retirement) at ages 60-61.
qp <- matrix(c(0.010, 0.030, 0.100,
                0.013, 0.050, 0.200), nrow = 2, byrow = TRUE,
              dimnames = list(NULL, c("death", "disability", "retirement")))
mdt674 <- buildMdtFromIndependentRates(x = 60:61, qx.primes = qp,
                                        radix = 1000, name = "Finan 67.4")
#> Added fictional decrement below last x and completed x and lx until zero.... 
#> Completed the table at top, all decrements on first cause 
print(mdt674)
#> Multiple decrements table Finan 67.4 
#>         death disability retirement
#> 0  0.01000000 0.00000000  0.0000000
#> 1  0.01000000 0.00000000  0.0000000
#> 2  0.01000000 0.00000000  0.0000000
#> 3  0.01000000 0.00000000  0.0000000
#> 4  0.01000000 0.00000000  0.0000000
#> 5  0.01000000 0.00000000  0.0000000
#> 6  0.01000000 0.00000000  0.0000000
#> 7  0.01000000 0.00000000  0.0000000
#> 8  0.01000000 0.00000000  0.0000000
#> 9  0.01000000 0.00000000  0.0000000
#> 10 0.01000000 0.00000000  0.0000000
#> 11 0.01000000 0.00000000  0.0000000
#> 12 0.01000000 0.00000000  0.0000000
#> 13 0.01000000 0.00000000  0.0000000
#> 14 0.01000000 0.00000000  0.0000000
#> 15 0.01000000 0.00000000  0.0000000
#> 16 0.01000000 0.00000000  0.0000000
#> 17 0.01000000 0.00000000  0.0000000
#> 18 0.01000000 0.00000000  0.0000000
#> 19 0.01000000 0.00000000  0.0000000
#> 20 0.01000000 0.00000000  0.0000000
#> 21 0.01000000 0.00000000  0.0000000
#> 22 0.01000000 0.00000000  0.0000000
#> 23 0.01000000 0.00000000  0.0000000
#> 24 0.01000000 0.00000000  0.0000000
#> 25 0.01000000 0.00000000  0.0000000
#> 26 0.01000000 0.00000000  0.0000000
#> 27 0.01000000 0.00000000  0.0000000
#> 28 0.01000000 0.00000000  0.0000000
#> 29 0.01000000 0.00000000  0.0000000
#> 30 0.01000000 0.00000000  0.0000000
#> 31 0.01000000 0.00000000  0.0000000
#> 32 0.01000000 0.00000000  0.0000000
#> 33 0.01000000 0.00000000  0.0000000
#> 34 0.01000000 0.00000000  0.0000000
#> 35 0.01000000 0.00000000  0.0000000
#> 36 0.01000000 0.00000000  0.0000000
#> 37 0.01000000 0.00000000  0.0000000
#> 38 0.01000000 0.00000000  0.0000000
#> 39 0.01000000 0.00000000  0.0000000
#> 40 0.01000000 0.00000000  0.0000000
#> 41 0.01000000 0.00000000  0.0000000
#> 42 0.01000000 0.00000000  0.0000000
#> 43 0.01000000 0.00000000  0.0000000
#> 44 0.01000000 0.00000000  0.0000000
#> 45 0.01000000 0.00000000  0.0000000
#> 46 0.01000000 0.00000000  0.0000000
#> 47 0.01000000 0.00000000  0.0000000
#> 48 0.01000000 0.00000000  0.0000000
#> 49 0.01000000 0.00000000  0.0000000
#> 50 0.01000000 0.00000000  0.0000000
#> 51 0.01000000 0.00000000  0.0000000
#> 52 0.01000000 0.00000000  0.0000000
#> 53 0.01000000 0.00000000  0.0000000
#> 54 0.01000000 0.00000000  0.0000000
#> 55 0.01000000 0.00000000  0.0000000
#> 56 0.01000000 0.00000000  0.0000000
#> 57 0.01000000 0.00000000  0.0000000
#> 58 0.01000000 0.00000000  0.0000000
#> 59 0.01000000 0.00000000  0.0000000
#> 60 0.00936000 0.02836000  0.0980100
#> 61 0.01141833 0.04471833  0.1937433
#> 62 1.00000000 0.00000000  0.0000000
```
