# Decrement names of an `mdt` object

Return the names of the decrement (cause-of-exit) columns stored in a
[`mdt`](https://spedygiorgio.github.io/lifecontingencies/reference/mdt-class.md)
multiple-decrement table, i.e. every column in the underlying data frame
other than the structural `x` (age) and `lx` (survivors) columns.

## Usage

``` r
getDecrements(object)

# S4 method for class 'mdt'
getDecrements(object)
```

## Arguments

- object:

  A
  [`mdt`](https://spedygiorgio.github.io/lifecontingencies/reference/mdt-class.md)
  object.

## Value

A character vector of decrement identifiers.

## References

Finan, M.B. *A Reading of the Theory of Life Contingency Models: A
Preparation for Exam MLC/3L*.

## See also

[`getOmega`](https://spedygiorgio.github.io/lifecontingencies/reference/getOmega.md),
[`mdt`](https://spedygiorgio.github.io/lifecontingencies/reference/mdt-class.md)

## Author

Giorgio Alfredo Spedicato

## Examples

``` r
tableDecr <- data.frame(d1 = c(150, 160, 160), d2 = c(50, 75, 85))
newMdt <- new("mdt", name = "testMDT", table = tableDecr)
#> Added lx 
#> Added x to the table... 
getDecrements(newMdt)
#> [1] "d1" "d2"
```
